

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
# --- Paso 3: Cargar e instalar paquetes necesarios ----
# ---------------------------------------------------------------------------- #
if (!require("pacman")) install.packages("pacman", repos = 'http://cran.us.r-project.org')

# Instalar o cargar los paquetes necesarios
pacman::p_load("dplyr", "tidyr", "raster", "sf", "maptools", "ggplot2",
  "spdep", "ade4", "Guerry", "spdep", "e1071", "magrittr", "qwraps2", "missMDA",
  "readxl", "ggplot2", "scales", "rlang", "climatol")

source('./src/funciones_auxiliares.R')

# ------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# --- Paso 4. Lectura de datos ----
#------------------------------------------------------------------------------#
# Cargar estaciones 
estaciones.todas   <- readxl::read_xlsx(glue::glue("{config$dir$datos}/datos.xlsx"), sheet = 2, col_names = T,
  na = 'NA') %>%
  dplyr::mutate(., longitud = lon, latitud = lat) %>%
  sf::st_as_sf(x = ., coords = c("lon", "lat"), crs = config$params$projections$latlon) %>%
  sf::st_transform(., crs = config$params$projections$gk)

# Cargar shape de zonas homogéneas
zonas.homogeneas <- sf::st_read(dsn = "./db/resultados/shapefiles",
  layer = 'clusters') %>%
  dplyr::rename('zona.homogenea' = 'z')
#-------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# --- Paso 3. Creación de las estaciones de cada zona homogéneas ----
#------------------------------------------------------------------------------#
purrr::map(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    if (!fs::dir_exists(glue::glue("{config$dir$resultados}/homogenea/datos/{zona.homogenea}"))) {
      fs::dir_create(glue::glue("{config$dir$resultados}/homogenea/datos/{zona.homogenea}"))
    }
    
    # Filtrar estaciones
    estaciones.todas.zonas <- sf::st_join(estaciones.todas, zonas.homogeneas, left = T) %>%
      dplyr::rename(., 'zona' = 'zona.homogenea') %>%
      dplyr::filter(., zona %in% zona.homogenea)
    
    metadatos.corregido <- estaciones.todas.zonas %>%
      dplyr::rename(., "lon" = longitud, "lat" = latitud) %>%
      dplyr::select(., c('lon', 'lat', 'alt', 'cod', 'Estacion'))  %>%
      sf::st_set_geometry(NULL)
    
    # Controlar que no haya estaciones repetidas
    if (length(unique(metadatos.corregido$Estacion)) == nrow(metadatos.corregido)) {
      cat('No hay estaciones repetidas') 
    } else {
      cat('Hay estaciones repetidas')
    }
    
    write.table(metadatos.corregido, 
      glue::glue("{config$dir$resultados}/homogenea/datos/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.est"),
      row.names = F, col.names = F)  
    
    # Datos de lluvia
    data <- readRDS(glue::glue("{config$dir$datos}/datos_mensuales_completos.rds"))
    
    # Crear data set en formato largo
    data.largo <- data %>%
      dplyr::select(-mes) %>%
      #tidyr::gather(., station_id, valor, -fecha ) %>%
      #dplyr::mutate(., valor = as.numeric(valor)) %>%
      dplyr::filter(., station_id %in% estaciones.todas.zonas$cod) %>%
      dplyr::select(., -c('fecha', 'station_id'))
    
    write.table(data.largo, 
      glue::glue("{config$dir$resultados}/homogenea/datos/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.dat"),
      row.names = F, col.names = F)
    
    
  }

)
#-------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# --- Paso 4. Control de calidad y homogeneidad exploratorio  ----
#------------------------------------------------------------------------------#
# Analisis exploratorio

purrr::map(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    if (!fs::dir_exists(glue::glue("{config$dir$resultados}/homogenea/exploratorio/{zona.homogenea}"))) {
      fs::dir_create(glue::glue("{config$dir$resultados}/homogenea/exploratorio/{zona.homogenea}"))
    }
    
    # Copiar metadatos estaciones
    base::file.copy(from = glue::glue("{config$dir$resultados}/homogenea/datos/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.est"),
        to = glue::glue("{config$dir$resultados}/homogenea/exploratorio/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.est"),
      overwrite = TRUE)
    
    # Copiar datos observados de estaciones
    base::file.copy(from = glue::glue("{config$dir$resultados}/homogenea/datos/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.dat"),
      to = glue::glue("{config$dir$resultados}/homogenea/exploratorio/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.dat"),
      overwrite = TRUE)
    
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados}/homogenea/exploratorio/{zona.homogenea}"))
    
    # Correr analisis exploratorio
    climatol::homogen(glue::glue("Prcp-{zona.homogenea}"), 1941, 2015, expl = T, std=2, 
      wd=c(100,100,50), gp = 4, nref=c(10, 10, 3))
    
    # Renombrar salidas
    #climatol::outrename(glue::glue("Prcp-{zona.homogenea}"), 1941, 2015, "-exploratorio")
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
  }

)



#-------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# --- Paso 5. Control de calidad y homogeneidad definitivo  ----
#------------------------------------------------------------------------------#
# Cargar estaciones 
estaciones.todas   <- readxl::read_xlsx("datos_claris.xlsx", sheet = 2, col_names = T,
  na = 'NA') %>%
  dplyr::mutate(., longitud = lon, latitud = lat) %>%
  sf::st_as_sf(x = ., coords = c("lon", "lat"), crs = sf::st_crs(zona.extendida.geo)) %>%
  sf::st_transform(., crs = sf::st_crs(zona.extendida.planar))

# Zona 1
zona.homogenea = 1L

estaciones.todas.zonas <- sf::st_join(estaciones.todas, zonas.homogeneas, left = T,
  largest = F) %>%
  dplyr::distinct(.) %>%
  dplyr::rename(., 'zona' = V3) %>%
  dplyr::filter(., zona %in% zona.homogenea)

metadatos.corregido <- estaciones.todas.zonas %>%
  dplyr::rename(., "lon" = longitud, "lat" = latitud) %>%
  dplyr::select(., c('lon', 'lat', 'alt', 'cod', 'Estacion'))  %>%
  sf::st_set_geometry(NULL)

write.table(metadatos.corregido, paste0("Prcp-", zona.homogenea,"-definitivo", "_1941-2015.est"), row.names = F,
  col.names = F)  

# Datos de lluvia
data <- readxl::read_xlsx("datos_claris.xlsx", sheet = 1, col_names = T,
  na = 'NA')

# Crear data set en formato largo
data.largo <- data %>%
  tidyr::gather(., estacion, valor, -Fecha ) %>%
  dplyr::mutate(., valor = as.numeric(valor)) %>%
  dplyr::filter(., estacion %in% estaciones.todas.zonas$cod) %>%
  dplyr::select(., -c('Fecha', 'estacion'))

write.table(data.largo, paste0("Prcp-", zona.homogenea, "-definitivo", "_1941-2015.dat"), row.names = F,
  col.names = F)

# Homogenizacion
climatol::homogen(paste0('Prcp-', zona.homogenea,"-definitivo"), 1941, 2015, expl = F,  std=2, 
  wd=c(75,75,50), gp = 4, snht1 = 20, snht2 = 25, dz.max = 6, dz.min = 6,
  ndec = 1, nref=c(10, 10, 4))

# climatol::outrename(paste0("Prcp-", zona.homogenea,"-definitivo"), 1941, 2015, 'prueba.dz.7')

# Zona 2
zona.homogenea = 2L

estaciones.todas.zonas <- sf::st_join(estaciones.todas, zonas.homogeneas, left = T,
  largest = F) %>%
  dplyr::distinct(.) %>%
  dplyr::rename(., 'zona' = V3) %>%
  dplyr::filter(., zona %in% zona.homogenea)

metadatos.corregido <- estaciones.todas.zonas %>%
  dplyr::rename(., "lon" = longitud, "lat" = latitud) %>%
  dplyr::select(., c('lon', 'lat', 'alt', 'cod', 'Estacion'))  %>%
  sf::st_set_geometry(NULL)

write.table(metadatos.corregido, paste0("Prcp-", zona.homogenea,"-definitivo", "_1941-2015.est"), row.names = F,
  col.names = F)  

# Datos de lluvia
data <- readxl::read_xlsx("datos_claris.xlsx", sheet = 1, col_names = T,
  na = 'NA')

# Crear data set en formato largo
data.largo <- data %>%
  tidyr::gather(., estacion, valor, -Fecha ) %>%
  dplyr::mutate(., valor = as.numeric(valor)) %>%
  dplyr::filter(., estacion %in% estaciones.todas.zonas$cod) %>%
  dplyr::select(., -c('Fecha', 'estacion'))

write.table(data.largo, paste0("Prcp-", zona.homogenea, "-definitivo", "_1941-2015.dat"), row.names = F,
  col.names = F)

# Homogenizacion
climatol::homogen(paste0('Prcp-', zona.homogenea,"-definitivo"), 1941, 2015, expl = F,  std=2, 
  wd=c(75,75,50), gp = 4, snht1 = 20, snht2 = 25, dz.max = 6, dz.min = 6,
  ndec = 1, nref=c(10, 10, 3))

# climatol::outrename(paste0("Prcp-", zona.homogenea,"-definitivo"), 1941, 2015, 'prueba.dz.7')

# Zona 3
zona.homogenea = 3L

estaciones.todas.zonas <- sf::st_join(estaciones.todas, zonas.homogeneas, left = T,
  largest = F) %>%
  dplyr::distinct(.) %>%
  dplyr::rename(., 'zona' = V3) %>%
  dplyr::filter(., zona %in% zona.homogenea)

metadatos.corregido <- estaciones.todas.zonas %>%
  dplyr::rename(., "lon" = longitud, "lat" = latitud) %>%
  dplyr::select(., c('lon', 'lat', 'alt', 'cod', 'Estacion'))  %>%
  sf::st_set_geometry(NULL)

write.table(metadatos.corregido, paste0("Prcp-", zona.homogenea,"-definitivo", "_1941-2015.est"), row.names = F,
  col.names = F)  

# Datos de lluvia
data <- readxl::read_xlsx("datos_claris.xlsx", sheet = 1, col_names = T,
  na = 'NA')

# Crear data set en formato largo
data.largo <- data %>%
  tidyr::gather(., estacion, valor, -Fecha ) %>%
  dplyr::mutate(., valor = as.numeric(valor)) %>%
  dplyr::filter(., estacion %in% estaciones.todas.zonas$cod) %>%
  dplyr::select(., -c('Fecha', 'estacion'))

write.table(data.largo, paste0("Prcp-", zona.homogenea, "-definitivo", "_1941-2015.dat"), row.names = F,
  col.names = F)

# Homogenizacion
climatol::homogen(paste0('Prcp-', zona.homogenea,"-definitivo"), 1941, 2015, expl = F,  std=2, 
  wd=c(75,75,50), gp = 4, snht1 = 20, snht2 = 25, dz.max = 6, dz.min = 6,
  ndec = 1, nref=c(10, 10, 3))

# climatol::outrename(paste0("Prcp-", zona.homogenea,"-definitivo"), 1941, 2015, 'prueba.dz.7')


# trace('homogen', edit = T)
#-------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# --- Paso 6. Gráfico diagnósticos  ----
#------------------------------------------------------------------------------#
# Datos disponibles ----
datos <- readxl::read_xlsx("datos.xlsx", sheet = 1, col_names = T,
    na = 'NA') %>%
  tidyr::gather(., estacion, valor, -Fecha) %>%
  dplyr::mutate(., valor = as.numeric(valor)) %>%
  tidyr::spread(., key = estacion, value = valor)

# Grafico 
datos.x <- datos %>%
  mutate(meses.validos = rowSums(!is.na(select(., -Fecha)))) %>%
  dplyr::select(., Fecha, meses.validos)

ggplot(data = datos.x, aes(x = as.Date(Fecha), y = meses.validos)) +
  geom_line() +
  scale_x_date(labels = date_format("%Y"), breaks='5 years') +
  ylab('Cantidad de estaciones') + xlab('Años') +
  theme_bw() +
  ggsave(paste0(dir.graficos, 'cantidad_estaciones.png'), device = 'png', dpi = 600)

# Histograma de datos mensuales ----
datos <- readxl::read_xlsx("datos.xlsx", sheet = 1, col_names = T,
  na = 'NA') %>%
  tidyr::gather(., estacion, valor, -Fecha) %>%
  dplyr::mutate(., valor = as.numeric(valor)) %>%
  tidyr::spread(., key = estacion, value = valor)

# Grafico 
datos.x <- datos %>%
  tidyr::gather(., estacion, valor, -Fecha) %>%
  dplyr::mutate(., Fecha = as.Date(Fecha), 
    mes = fecha2mes(Fecha)) %>%
  dplyr::mutate(., mes = factor(mes, levels = c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo',
    'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre'), 
    labels = c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo',
      'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')))

ggplot(data = datos.x, aes(x = valor)) +
  geom_histogram(color = 'Steelblue') +
  facet_wrap(.~mes, scales = 'free') +
  xlab('Precipitación [mm]') + ylab('Cantidad de meses') +
  theme_bw() +
  ggsave(paste0(dir.graficos, 'frecuencia_mensual.png'), device = 'png', dpi = 600, height = 8, width = 8)


# Outliers ----

estaciones.zonas <- sf::st_join(estaciones.todas, zonas.homogeneas, left = T,
  largest = F) %>%
  dplyr::distinct(.) %>%
  dplyr::rename(., 'zona' = V3) %>%
  sf::st_set_geometry(NULL)

outliers <- read.csv("Outliers.csv", stringsAsFactors = F) %>%
  dplyr::mutate(., Date = as.Date(Date, format = '%d/%m/%y')) %>%
  dplyr::mutate(., estacion.astronomica = fecha2estacion(Date),
    mes = fecha2mes(Date), 
    ano = lubridate::year(Date),
    signo = factor(if_else(Anomaly..std.devs.. > 0, "Derecha", "Izquierda"),
      levels = c("Izquierda", "Derecha"))) %>%
  dplyr::left_join(., y = estaciones.zonas, by = c('Code' = 'cod')) %>%
  dplyr::select(., Code, Date, Suggested, estacion.astronomica, mes, ano, signo, zona) %>%
  dplyr::filter(., Code %in% estaciones.usar$cod)



# Estaciones astronomicas
ggplot2::ggplot(data = outliers, aes(x = factor(estacion.astronomica), fill = factor(zona))) +
  ggplot2::geom_bar() +
  facet_wrap(.~signo) +
  scale_fill_brewer(palette = "Set1", name = "Zonas") +
  theme_bw() +
  xlab("Estación del año") + ylab("Cantidad") +
  ggsave(paste0(dir.graficos, 'outliers_.png'), device = 'png', dpi = 600)

outliers.estacion <- outliers %>%
  dplyr::right_join(., y = estaciones.todas, by = c('Code' = 'cod')) %>%
  dplyr::select(., Code, longitud, latitud, Suggested) %>%
  dplyr::filter(., !stringr::str_detect(Code, "\\*")) %>%
  dplyr::mutate(., cantidad = if_else(is.na(Suggested), 0, 1)) %>%
  dplyr::select(., -Suggested) %>%
  dplyr::group_by(., Code, latitud, longitud) %>%
  dplyr::summarise(., cantidad.outliers = sum(cantidad)) 

write.csv(outliers.estacion, 'outliers.estacion.csv')

# Quiebres ----
anos <- data.frame(ano = seq(1941, 2015, 1))


quiebres_año <- read.csv("Quiebres.csv", stringsAsFactors = F) %>%
  dplyr::mutate(., Date = as.Date(Date, format = '%d/%m/%Y')) %>%
  dplyr::mutate(., estacion.astronomica = fecha2estacion(Date),
    mes = fecha2mes(Date), 
    ano = lubridate::year(Date)) %>%
  dplyr::filter(., Code %in% estaciones.usar$cod) %>%
  dplyr::left_join(., y = estaciones.zonas, by = c('Code' = 'cod')) %>%
  dplyr::select(., Code, Date, SNHT, estacion.astronomica, mes, ano, zona) %>%
  dplyr::group_by(., ano, zona) %>%
  dplyr::summarise(., cantidad = n()) %>%
  dplyr::right_join(., anos, by = 'ano') %>%
  dplyr::mutate(., cantidad = if_else(is.na(cantidad), 0 , as.double(cantidad)))

# Estaciones astronomicas
ggplot2::ggplot(data = quiebres_año, aes(x = (ano), y = cantidad, fill = factor(zona))) +
  ggplot2::geom_bar(stat = 'identity') +
  theme_bw() +
  ylab("Cantidad de quiebres") + 
  scale_x_continuous(name="Años", limits=c(1941, 2015),
    breaks=seq(1941,2015,5)) +
  scale_fill_brewer(palette = "Set1",
    name="Zonas",
    breaks=c("1", "2", "3"),
    labels=c("1", "2 ", "3")) +
  ggsave(paste0(dir.graficos, 'cantidad de quiebres_.png'), device = 'png', dpi = 600)

# Quiebres estacion
quiebres_estacion <- read.csv("Quiebres.csv", stringsAsFactors = F) %>%
  dplyr::mutate(., Date = as.Date(Date, format = '%d/%m/%Y')) %>%
  dplyr::mutate(., estacion.astronomica = fecha2estacion(Date),
    mes = fecha2mes(Date), 
    ano = lubridate::year(Date)) %>%
  dplyr::filter(., Code %in% estaciones.usar$cod) %>%
  dplyr::left_join(., y = estaciones.zonas, by = c('Code' = 'cod')) %>%
  dplyr::select(., Code, Date, SNHT, estacion.astronomica, mes, ano, zona) %>%
  dplyr::group_by(., Code, zona) %>%
  dplyr::summarise(., cantidad = n())


# Estaciones astronomicas
ggplot2::ggplot(data = quiebres_estacion, aes(x = cantidad, fill = factor(zona))) +
  ggplot2::geom_bar(stat = 'count', width = 0.5) +
  theme_bw() +
  ylab("Cantidad de estaciones") +
  scale_fill_brewer(palette = "Set1",
    name="Zonas",
    breaks=c("1", "2", "3"),
    labels=c("1", "2 ", "3")) +
  scale_x_continuous(breaks=seq(1,4), name = 'Cantidad de quiebres') +
  ggsave(paste0(dir.graficos, 'cantidad de quiebres.estaciones_.png'), device = 'png', dpi = 600)



dahstat('Prcp-2-definitivo',1941,2015,stat='tnd', anyip = 1941, anyfp = 2015)

# Extraccion de estaciones a usar ----
estaciones.usar <- readxl::read_xlsx("estaciones_cod.xlsx") %>%
  dplyr::inner_join(., y = estaciones.todas, by = c('cod' = 'cod')) %>%
  dplyr::select(., cod, mostrar.quiebre, mejor.estacion, longitud, latitud) %>%
  sf::st_as_sf(., coords = 4:5, crs = crs.geo) %>%
  sf::st_transform(., crs.planar) %>%
  sf::st_join(., cordoba.extendida, join = st_intersects) %>%
  dplyr::filter(., NAME_1 == 'Córdoba')

# Evaluación de tendencias 
tendencias <- read.csv("tendencias.boxplot.csv") %>%
  dplyr::select(., -c('X', "Y")) %>%
  dplyr::filter(., if_else(Tipo == 'Definitivo', Code %in% estaciones.usar$mejor.estacion,
    Code %in% estaciones.usar$cod)) %>%
  tidyr::gather(., escala, valor, -c('Code', "Tipo")) %>%
  dplyr::mutate(., escala = factor(escala, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec", "Annual"),
    labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Anual")))

ggplot(tendencias %>% filter(., escala != 'Anual'), aes(x = Tipo, y = valor, fill = Tipo)) +
  facet_wrap(.~escala, scales = 'free', ncol = 4) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1",
    breaks = c('Definitivo', 'Exploratorio'),
    labels = c("Homogeizados", "Originales")) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab("") + ylab("Tendencia [mm/100 años]") +
  ggsave(paste0(dir.graficos, 'tendencias.png'), device = 'png', dpi = 600, height = 8, width = 8)


# Evaluación de desvíos ----
desvios <- read.csv("Prcp-1-desvio.estandar.csv") %>%
  dplyr::select(., -c('X', "Y"))  %>%
  dplyr::filter(., if_else(Tipo == 'Definitivo', Code %in% estaciones.usar$mejor.estacion,
    Code %in% estaciones.usar$cod)) %>%
  tidyr::gather(., escala, valor, -c('Code', "Tipo")) %>%
  dplyr::mutate(., escala = factor(escala, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec", "Annual"),
    labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Anual")))


ggplot(desvios %>% filter(., escala != 'Anual'), aes(x = Tipo, y = valor, fill = Tipo)) +
  facet_wrap(.~escala, scales = 'free', ncol = 4) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1",
    breaks = c('Definitivo', 'Exploratorio'),
    labels = c("Homogeizados", "Originales")) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab("") + ylab("Desviaciones estándar") +
  ggsave(paste0(dir.graficos, 'desvios.png'), device = 'png', dpi = 600)



  


dahstat('Prcp-3-definitivo',1941,2015,stat='series', anyip = 1941, anyfp = 2015)

# Evaluación de la precipitación ----
precipitacion.normal <- readxl::read_excel("precipitacion.normal.1941.1970.xlsx") %>%
  dplyr::filter(., if_else(Tipo == 'Definitivo', Code %in% estaciones.usar$mejor.estacion,
    Code %in% estaciones.usar$cod)) %>%
  dplyr::select(., -c("X", "Y")) %>%
  tidyr::gather(., escala, valor, -c('Code', "Tipo")) %>%
  dplyr::mutate(., escala = factor(escala, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec", "Annual"),
    labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Anual")))

ggplot(precipitacion.normal %>% filter(., escala != 'Anual'), aes(x = Tipo, y = valor, fill = Tipo)) +
  facet_wrap(.~escala, scales = 'free', ncol = 4) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1",
    breaks = c('Definitivo', 'Exploratorio'),
    labels = c("Homogeizados", "Originales")) +
  theme_bw() +
  theme(legend.position = 'bottom', strip.text = element_blank()) +
  xlab("") + ylab("Precipitacion acumulada [mm]") 
  ggsave(paste0(dir.graficos, 'precipitación.png'), device = 'png', dpi = 600)


# Evaluacion ----

codigos.estacion <- est.c$Code

anomalias <- anom %>%
  as.data.frame(.) %>%
  dplyr::rename_(.dots = purrr::set_names(names(.), codigos.estacion)) %>%
  dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
  tidyr::gather(., estacion, valor, -fecha)

anomalias.estandarizadas <- sanom %>%
  as.data.frame(.) %>%
  dplyr::rename_(.dots = set_names(names(.), codigos.estacion)) %>%
  dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
  tidyr::gather(., estacion, valor, -fecha)

datos.homogeneizados <- dah %>%
  matrix(., nrow = 12 * 75, byrow=F) %>%
  as.data.frame(.) %>%
  dplyr::rename_(.dots = set_names(names(.), codigos.estacion)) %>%
  dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
  tidyr::gather(., estacion, valor, -fecha)


datos.observados.estandarizados <- dat.z %>%
  as.data.frame(.) %>%
  dplyr::rename_(.dots = set_names(names(.), codigos.estacion)) %>%
  dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
  tidyr::gather(., estacion, valor, -fecha)

datos.estimados.estandarizados <- dat.e %>%
  as.data.frame(.) %>%
  dplyr::rename_(.dots = set_names(names(.), codigos.estacion)) %>%
  dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
  tidyr::gather(., estacion, valor, -fecha)

datos.estimados <- dat.c %>%
  as.data.frame(.) %>%
  dplyr::rename_(.dots = set_names(names(.), codigos.estacion)) %>%
  dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
  tidyr::gather(., estacion, valor, -fecha)

# Comparacion con estacion de referencia ----
comparacion.referencia <- datos.estimados %>%
  dplyr::filter(., estacion == 'S300') %>%
  dplyr::left_join(., datos.homogeneizados, by = c('estacion', 'fecha')) %>%
  tidyr::gather(., tipo, valor, -c('fecha', 'estacion')) %>%
  dplyr::mutate(., tipo = if_else(tipo == 'valor.x', 'Referencia', 'Observado'),
    ano = lubridate::year(fecha)) %>%
  dplyr::group_by(ano, tipo) %>%
  dplyr::summarise(., valor = sum(valor))

ggplot(comparacion.referencia, aes(x = ano, y = valor, color = tipo)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = 'Set1',
    name = 'Serie') +
  theme(legend.position="bottom") +
  xlab('Años') + ylab("Precipitación acumulada [mm]") +
  ggsave(paste0(dir.graficos, 'comparacion.estacion.referencia.png'), device = 'png', dpi = 600)
  
# Series con puntos de quiebre -----
precipitacion <- readxl::read_excel("precipitacion.xlsx") %>%
  tidyr::gather(., estacion, valor, -Date) %>%
  dplyr::left_join(., readxl::read_excel("flags.xlsx") %>%
      tidyr::gather(estacion, valor, -Date), by = c('Date', "estacion")) %>%
  dplyr::rename(., precipitacion = valor.x, flag = valor.y) %>%
  dplyr::filter(., estacion %in% estaciones.usar$mejor.estacion) %>%
  dplyr::mutate(., ano = lubridate::year(Date)) %>%
  dplyr::group_by(., ano, estacion) %>%
  dplyr::summarise(., precipitacion = sum(precipitacion),
    flag = round(mean(flag), 0)) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(., ano = lubridate::ymd(ano, truncated = 2L)) %>%
  dplyr::mutate(., flag = factor(flag, levels = c("0", "1", "2"))) %>%
  dplyr::filter(., estacion == 'S352-2')


quiebres <- read.csv("Quiebres.csv", stringsAsFactors = F) %>%
  dplyr::filter(., Code %in% estaciones.usar$cod) %>%
  dplyr::filter(., Code == "S352") %>%
  dplyr::mutate(., Date = as.Date(Date, format = '%d/%m/%Y'),
    ano = lubridate::year(Date))

ggplot(precipitacion, aes(ano, precipitacion, color = flag))+
  geom_path(aes(group = 1)) +
  geom_point() +
  scale_color_brewer(palette = 'Set1',
    name = 'Tipo',
    breaks = c("0", "1", "2"),
    labels = c("Observado", "Rellenado", "Corregido")) +
  geom_vline(data = quiebres, aes(xintercept = Date), linetype = "dashed") +
  scale_y_continuous(breaks = seq(300, 1600, by = 100)) +
  theme_bw() +
  xlab("Años") + ylab("Precipitación anual [mm]") +
  ggsave(paste0(dir.graficos, 'precipitacion_anual_S352-2.png'), device = 'png', dpi = 600)

flags <- readxl::read_excel("flags.xlsx") %>%
  tidyr::gather(estacion, valor, -Date)

# Sumas acumulativas ----
suma.cumulativa <- datos.homogeneizados %>% # datos homogeneizados de analisis exploratorio
  dplyr::filter(., estacion %in% c('S277','S547', 'S402', 'S365')) %>%
  dplyr::mutate(., ano = lubridate::year(fecha)) %>%
  dplyr::group_by(ano, estacion) %>%
  dplyr::summarise(., valor = sum(valor)) %>%
  dplyr::mutate(., normal.1941.1970 = dplyr::filter(., ano >= '1941' & ano <= '1970') %>%
      dplyr::group_by(estacion) %>%
      dplyr::summarise(normal = mean(valor)) %>%
      dplyr::pull(normal)) %>%
  dplyr::mutate(., normal.1971.2000 = dplyr::filter(., ano >= '1971' & ano <= '2000') %>%
      dplyr::group_by(estacion) %>%
      dplyr::summarise(normal = mean(valor)) %>%
      dplyr::pull(normal)) %>%
  dplyr::mutate(., normal = dplyr::filter(., ano >= '1941' & ano <= '2015') %>%
      dplyr::group_by(estacion) %>%
      dplyr::summarise(normal = mean(valor)) %>%
      dplyr::pull(normal)) %>%
  dplyr::mutate(., anomalia.1941.1970 = valor - normal.1941.1970,
    anomalia.1971.2000 = valor - normal.1971.2000,
    anomalia = valor - normal) %>%
  dplyr::group_by(., estacion) %>%
  dplyr::arrange(ano) %>%
  dplyr::mutate(., cumsum = cumsum(anomalia))

precipitacion.cumsum <- readxl::read_excel("precipitacion.xlsx") %>%
  tidyr::gather(., estacion, valor, -Date) %>%
  dplyr::filter(., estacion %in% c('S277-2','S547-2', 'S402-2', 'S365-2')) %>%
  dplyr::mutate(., ano = lubridate::year(Date)) %>%
  dplyr::group_by(ano, estacion) %>%
  dplyr::summarise(., valor = sum(valor)) %>%
  dplyr::mutate(., normal.1941.1970 = dplyr::filter(., ano >= '1941' & ano <= '1970') %>%
      dplyr::group_by(estacion) %>%
      dplyr::summarise(normal = mean(valor)) %>%
      dplyr::pull(normal)) %>%
  dplyr::mutate(., normal.1971.2000 = dplyr::filter(., ano >= '1971' & ano <= '2000') %>%
      dplyr::group_by(estacion) %>%
      dplyr::summarise(normal = mean(valor)) %>%
      dplyr::pull(normal)) %>%
  dplyr::mutate(., normal = dplyr::filter(., ano >= '1941' & ano <= '2015') %>%
      dplyr::group_by(estacion) %>%
      dplyr::summarise(normal = mean(valor)) %>%
      dplyr::pull(normal)) %>%
  dplyr::mutate(., anomalia.1941.1970 = valor - normal.1941.1970,
    anomalia.1971.2000 = valor - normal.1971.2000,
    anomalia = valor - normal) %>%
  dplyr::group_by(., estacion) %>%
  dplyr::arrange(ano) %>%
  dplyr::mutate(., cumsum = cumsum(anomalia)) 


gg <- ggplot(precipitacion.cumsum, aes(x = ano, y = cumsum, color = estacion)) +
  geom_line() +
  scale_color_brewer(palette = 'Set1',
    name = 'Estación',
    breaks = c('S277-2','S547-2', 'S402-2', 'S365-2'),
    labels = c('Las Acequias','Arata', 'San Javier Yacanto', 'Ascochinga')) +
  xlab('Año') + ylab('Suma acumulativa de precipitación [mm]') +
  theme_bw() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-6000, 2000)) +
  theme(legend.position = 'bottom')

pp <- ggplot(suma.cumulativa, aes(x = ano, y = cumsum, color = estacion)) +
  geom_line() +
  scale_color_brewer(palette = 'Set1',
    name = 'Estación',
    breaks = c('S277', 'S547', 'S402', 'S365'),
    labels = c('Las Acequias', 'Arata', 'San Javier Yacanto', 'Ascochinga')) +
  xlab('Año') + ylab('Suma acumulativa de precipitación [mm]') +
  theme_bw() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-6000, 2000)) +
  theme(legend.position = 'bottom')

ggpubr::ggarrange(pp, gg, nrow = 2,  common.legend = TRUE, legend="bottom") %>%
  ggpubr::ggexport(filename = paste0(dir.graficos, 'suma.acumulativa.png'), res = 300,
    width = 2000, height = 2000)
