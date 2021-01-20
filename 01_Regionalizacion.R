

# ---------------------------------------------------------------------------- #
# Paso 1: Borrar espacio de trabajo ----
# ---------------------------------------------------------------------------- #

# Borrar espacio de trabajo
rm(list = objects()); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Leer archivos YML de configuracion y parametros ----

# i. Leer YML de configuracion
args <- base::commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  archivo.config <- args[1]
} else {
  # No vino el archivo de configuracion por linea de comandos.
  # Utilizo un archivo default
  archivo.config <- paste0(getwd(), "/database_configuration.yml")
}

if (! file.exists(archivo.config)) {
  stop(paste0("El archivo de configuracion de ", archivo.config, " no existe\n"))
} else {
  cat(paste0("Leyendo archivo de configuracion ", archivo.config, "...\n"))
  config <- yaml::yaml.load_file(archivo.config)
}

# ii. YML de parametros para los controles de calidad
if (length(args) > 1) {
  archivo.params <- args[2]
} else {
  # No vino el archivo de parametros por linea de comandos.
  # Utilizo un archivo default
  archivo.params <- paste0(getwd(), "/database_parameters.yml")
}
if (! file.exists(archivo.params)) {
  stop(paste0("El archivo de parametros de ", archivo.params, " no existe\n"))
} else {
  cat(paste0("Leyendo archivo de parametros ", archivo.params, "...\n"))
  config$params <- yaml::yaml.load_file(archivo.params)
}

rm(archivo.config, archivo.params, args); gc()

# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso 3: Cargar e instalar paquetes necesarios ----
# ---------------------------------------------------------------------------- #
if (!require("pacman")) install.packages("pacman", repos = 'http://cran.us.r-project.org')

# Instalar o cargar los paquetes necesarios
pacman::p_load("dplyr", "tidyr", "raster", "sf", "maptools", "ggplot2",
  "spdep", "ade4", "Guerry", "spdep", "e1071", "magrittr", "qwraps2", "missMDA")
# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso 4: Cargar datos necesarios ----
# ---------------------------------------------------------------------------- #
estaciones <- readxl::read_xlsx(glue::glue('{config$dir$datos}/{config$params$datos}'),
  sheet = 2) %>%
  dplyr::rename(nombre = Estacion, station_id = cod, 
    latitud = lat, longitud = lon, elevacion = alt) %>%
  dplyr::select(station_id, nombre, longitud, latitud, elevacion) %>%
  sf::st_as_sf(., coords = c('longitud', 'latitud'), crs = config$params$projections$latlon) 
  
  
  
  

# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Rasters -------
# ------------------------------------------------------------------------#

list.files.prcp.clip <- purrr::map_chr(
  .x = seq(1, 12, 1),
  .f = function(mes) {
    
    glue::glue("{config$dir$raster$worldclim$prcp}/prcp_{mes}.tif")
  }
)

precipitation.dataset <- stars::read_stars(list.files.prcp.clip) %>%
  sf::st_as_sf(.) %>%
  dplyr::rename_at(.vars = vars(ends_with(".tif")),
    .funs = funs(sub("[.]tif$", "", .))) 

list.files.bio.clip <- purrr::map_chr(
  .x = seq(1, 19, 1),
  .f = function(mes) {
    
    glue::glue("{config$dir$raster$worldclim$bio}/bio_{mes}.tif")
  }
)

bioclimate.dataset <- stars::read_stars(list.files.bio.clip) %>%
  sf::st_as_sf(.) %>%
  dplyr::rename_at(.vars = vars(ends_with(".tif")),
    .funs = funs(sub("[.]tif$", "", .))) 

elevacion.dataset <- stars::read_stars(glue::glue('{config$dir$raster$dem}/dem_Argentina.tif'))  %>%
  sf::st_as_sf(.) %>%
  dplyr::rename(elevacion = dem_Argentina.tif)


matriz.pca <- precipitation.dataset %>%
  sf::st_join(bioclimate.dataset) %>%
  sf::st_join(elevacion.dataset) 

# Crear vector para dividir entre pixeles a regionalizar y validación
indice <- seq(0, nrow(matriz.pca), by = 1)

# Regionalizacion
set.seed(123)
pixeles.regionalizar <- sample(indice, length(indice)*0.6)

# validacion 
pixeles.validacion <- indice[!pixeles.regionalizar]

# Unir ambos objetos espacialmente
matriz.pca.train <- matriz.pca[pixeles.regionalizar,] %>%
  na.omit(.) %>%
  as(., "Spatial")

# -----------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Paso 6. Creacion de data frame con variables a agrupar ----
# ----------------------------------------------------------------------------- #
# convierte el objeto espacial en un data frame, solo con las variables de interés
variables.agrupar  <- data.frame(matriz.pca.train@data) %>%
  base::scale(.) %>%
  as.data.frame(.)

summary(variables.agrupar)

# extrae las coordenadas del objeto espatial
xy <- coordinates(matriz.pca.train)

variables.agrupar.exploratorio <- variables.agrupar %>%
  tidyr::gather(., variable, valor)

# Grafico exploratorio de variobles bioclimáticas
ggplot(data = variables.agrupar.exploratorio, aes(x = variable, y = valor)) +
  ggplot2::geom_boxplot(coef = 2000) +
  facet_wrap(.~variable, scales = 'free', ncol = 5)

# ------------------------------------------------------------------------------

# ----------------------------------------------------------------------------- #
# Paso 7. Análisis de componentes principales tradicional ----
# ----------------------------------------------------------------------------- #
pca <- ade4::dudi.pca(variables.agrupar, scannf = F)

eigen.pca <- data.frame(id = seq(1, length(pca$eig), by = 1),
  eigen = pca$eig/sum(pca$eig)*100)

# Scree plor para determinar el numero de componentes
factoextra::fviz_eig(pca, main = 'Scree plot', xlab = 'Componentes', 
  ylab = "Porecentaje de la variabilidad explicada")

# ------------------------------------------------------------------------------

# ----------------------------------------------------------------------------- #
# Paso 8. Análisis de componentes principales espacial ----
# ----------------------------------------------------------------------------- #

# Creación de la matriz de pesos espaciales

# poly2nb es una funciÛn que construye una lista de vecinos basada en regiones
# que estan contiguas por compartir m·s de un punto en sus fronteras
box.vecinos <-  rgeos::gUnarySTRtreeQuery(matriz.pca.train) # creación del bounding box

lista.vecinos <- spdep::poly2nb(matriz.pca.train,  foundInBox = box.vecinos)

# Esta función complementa la lista de vecinos con pesos espaciales
matriz.pesos.espaciales <- spdep::nb2listw(lista.vecinos, style = "W")

# Coeficiente de Moran
library(spdep)
#i.moran <- parallel::mclapply(variables.agrupar, moran.mc, matriz.pesos.espaciales, 999, mc.cores = 4)

#i.moran <- lapply(variables.agrupar, moran.mc, matriz.pesos.espaciales, 500)

#i.moran

#save(i.moran, file = "indice.moran.rda")

# An·lisis de componentes Principales espaciales (ACPe)
ms <- adespatial::multispati(pca, matriz.pesos.espaciales, scannf = F)

eigen.ms <- data.frame(id = seq(1, length(ms$eig), by = 1),
  eigen = ms$eig/sum(ms$eig)*100)

ggplot(data = eigen.ms, aes(y = eigen, x = id)) +
  geom_bar(stat = 'identity', fill = 'white', color = 'black') +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Componentes") + ylab("% de variancia")
  ggsave(paste0(dir.graficos, "scree.plot.multispati.png"), device = "png",
    dpi = 600)

# para extraer las componentes espaciales
sPC.var.bio <-ms$li

# para combinar las componentes espaciales con los nombres
sPC.var.bio <-cbind(ms$li,xy)

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Paso n ---- Análisis de cluster ----
# -----------------------------------------------------------------------------#

# Cantidad de grupos 
cantidad.grupos <- NbClust::NbClust(sample_n(as.data.frame(sPC.var.bio)[,1:2], 10000),
  distance = "euclidean", min.nc=2, max.nc = 8, 
  method = 'kmeans', index = "all")


# Guardar resultados del clustering 
if (!fs::dir_exists(config$dir$resultados)) {
  fs::dir_create(config$dir$resultados)
}
write.csv(cantidad.grupos$All.index, glue::glue('{config$dir$resultados}/indice.csv'))

save(cantidad.grupos, file = glue::glue('{config$dir$resultados}/cantidad.grupos.rda'))

# Análisis de fuzzy k-means
fuzzy.variables.bioclimaticas <- e1071::cmeans(as.data.frame(sPC.var.bio)[,1:2], 3, 
  iter.max = 100, verbose = FALSE, dist = "euclidean", method = "cmeans", m = 2,
  rate.par = NULL, weights = 1, control = list())

# Zonas homogeneas
cluster.resultado <- as.data.frame(cbind(xy, fuzzy.variables.bioclimaticas$cluster))

# Creación de poligonos con las zonas homogéneas 
fuzzy.variables.bioclimaticas_sf <- as.data.frame(cbind(xy, fuzzy.variables.bioclimaticas$cluster)) %>%
  sf::st_as_sf(., coords = 1:2, crs = config$params$projections$gk) %>%
  #sf::st_join(x = st_make_grid(., cellsize = 10000) %>% sf::st_sf(.), y = .) %>%
  dplyr::rename(., 'cluster' = 'V3') %>%
  sf2raster(., 'cluster') %>%
  raster::rasterToPolygons(.) 

# Guardar resultados del clustering 
if (!fs::dir_exists(glue::glue('{config$dir$resultados}/shapefiles') )) {
  fs::dir_create(glue::glue('{config$dir$resultados}/shapefiles') )
}

rgdal::writeOGR(obj = fuzzy.variables.bioclimaticas_sf,  
  dsn = glue::glue('{config$dir$resultados}'),
  layer="clusters", 
  driver="ESRI Shapefile")

# ------------------------------------------------------------------------------

# ----------------------------------------------------------------------------- #
# --- Paso n. Validacion del agrupamiento ----
fuzzy.variables.bioclimaticas.sf <- sf::st_read(dsn = "./db/shapefiles",
  layer = 'zonas_validacion') %>%
  sf::st_transform(., st_crs(var.bio.poligono)) %>%
  dplyr::select(., V3, V3_1, geometry) %>%
  dplyr::rename(., 'zona.validacion' = V3, 'zona.regionalizacion' = V3_1)


zona.validacion <- fuzzy.variables.bioclimaticas.sf %>%
  dplyr::filter(., zona.validacion == 0) %>%
  sf::st_join(., var.bio.poligono) %>%
  sf::st_set_geometry(NULL)


zona.regionalizacion <- fuzzy.variables.bioclimaticas.sf %>%
  dplyr::filter(., zona.validacion != 0) %>%
  sf::st_join(., var.bio.poligono) %>%
  sf::st_set_geometry(NULL)


zona <- 1L

Compositional::hotel2T2(as.matrix(zona.regionalizacion %>% dplyr::filter(., zona.regionalizacion %in% zona) %>%
    dplyr::select(., -c(zona.regionalizacion, zona.validacion))),
  as.matrix(zona.validacion %>% dplyr::filter(., zona.regionalizacion %in% zona) %>%
      dplyr::select(., -c(zona.regionalizacion, zona.validacion))))



# Guardar archivo


