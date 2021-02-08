# -----------------------------------------------------------------------------#
# --- PASO 1. Cargar paquetes necesarios ----
rm(list = ls()); gc()
list.of.packages <- c("cluster", "dplyr", "ggbiplot", "ggplot2", "ggrepel", 
                      "leaflet", "leafpop", "lubridate", "MASS", "purrr", "readr", 
                      "RPostgreSQL", "stats", "stringr", "utils", "tidyr", "yaml", 
                      "zoo", "spdep")
for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}
options(bitmapType = "cairo")
rm(list.of.packages, pack); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Leer archivo de configuracion ----
# -----------------------------------------------------------------------------#
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

# -----------------------------------------------------------------------------#
# --- PASO 3. Cargar librerías propias ----
# -----------------------------------------------------------------------------#

source(glue::glue('{config$dir$base}/src/funciones_auxiliares.R'), echo = TRUE)
# ----------------------------------------------------------------------------- 

# -----------------------------------------------------------------------------#
# --- PASO 4. Buscar datos observados de precipitaciones mensuales ----
# -----------------------------------------------------------------------------#

# a) Buscar estaciones y geolocalizarlas
estaciones <- readxl::read_xlsx(glue::glue('{config$dir$datos}/datos.xlsx'),
                                sheet = 2) %>%
  dplyr::rename(nombre = Estacion, station_id = cod, 
                latitud = lat, longitud = lon, elevacion = alt) %>%
  dplyr::select(station_id, nombre, longitud, latitud, elevacion) %>%
  sf::st_as_sf(., coords = c('longitud', 'latitud'), crs = config$params$projections$latlon) 


# Identificar vecinos
# convert points to sf objects
spbb <- st_transform(estaciones, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
pts <- st_as_sf(spbb)

# creates a matrix of distances between the points
dist_matrix <- st_distance(spbb)

# replaces 0s with NA
diag(dist_matrix) <- NA

# convert matrix to data frame and set column and row names
dist_matrix <- data.frame(dist_matrix)
names(dist_matrix) <- pts$station_id
rownames(dist_matrix) <- pts$station_id

# find the 5 nearest stations and create new data frame
near <- dist_matrix %>% 
  tibble::as_tibble(.) %>%
  dplyr::mutate(station_id = names(.)) %>% 
  tidyr::gather(closest, dist,-station_id) %>% 
  dplyr::filter(!is.na(dist)) %>% 
  dplyr::group_by(station_id) %>% 
  dplyr::arrange(dist) %>% 
  dplyr::slice(1:15) %>% 
  dplyr::mutate(dist_rank = 1:15) %>%
  dplyr::ungroup(.) 

# b) Cargar estadisticas mensuales
#estadisticas.mensuales <- base::readRDS(glue::glue('{config$dir$datos}/datos_mensuales_completos.rds'))
fecha.desde  <- as.Date(config$intervalo.procesable$fecha.desde)
fecha.hasta  <- as.Date(config$intervalo.procesable$fecha.hasta)

estadisticas.mensuales <- readxl::read_xlsx(glue::glue('{config$dir$datos}/datos.xlsx'),
                                            sheet = 1) %>%
  dplyr::filter(Fecha >= as.Date('1971-01-01') & Fecha < as.Date('2010-12-31')) %>%
  tidyr::gather(station_id, valor, -Fecha) %>%
  dplyr::rename(fecha = Fecha) %>%
  dplyr::mutate(valor = as.numeric(valor),
                fecha = as.Date(fecha))

# # Completar faltantes
# estadisticas.mensuales <- purrr::map_dfr(
#   .x = unique(estaciones$station_id),
#   .f = function(station.id) {
#     
#     # Datos de la estacion a imputar
#     estadisticas.estacion <- estadisticas.mensuales %>%
#       dplyr::filter(station_id == station.id) 
#     
#     # Obtener los vecinos más cercano de la estacion
#     closest <- near %>%
#       dplyr::filter(station_id == station.id) %>%
#       dplyr::pull(closest)
#     # Extraer estadísticas de vecinos más cercanos
#     estadisticas.vecinos <- estadisticas.mensuales %>%
#       dplyr::filter(station_id %in% closest) 
#     
#     # Indices de faltantes 
#     indexesToWrite <- c()
#     
#     missingIndexes <- which(is.na(estadisticas.estacion[, 3]))
#     
#     writeLines(paste0("> Station: ", station.id, ". Missing: ", length(missingIndexes)))
#     
#     if (length(missingIndexes) < 0.3 * nrow(estadisticas.estacion)) {
#       
#       # Check if there are missing values for this station and variable, otherwise, skip it.
#       if(length(missingIndexes) > 0) {
#         
#         indexesToWrite <- c(indexesToWrite, missingIndexes)
#         
#         # Imputar datos faltantes
#         datosEstacion <- impute_mf(estacion_id = station.id, 
#                                    datosEstacion = estadisticas.estacion, 
#                                    missingIndexes = missingIndexes, 
#                                    registrosVecinos = estadisticas.vecinos, 
#                                    max_parallelism = 5)
#         
#       } else {
#         writeLines(paste0("> Station: ", station.id, ". Missing: ", length(missingIndexes)))
#       }
#     }
#     
#     estadisticas.mensuales.imputados <- estadisticas.mensuales %>%
#       dplyr::left_join(., datosEstacion %>% dplyr::rename(valor.imputado = valor), by = c('fecha', 'station_id')) %>%
#       dplyr::mutate(valor = if_else(is.na(valor), valor.imputado, valor)) %>%
#       dplyr::select(-valor.imputado)
#   }
# )


# c) Buscar datos de elevacion de las estacion meteorologicas
estaciones.elevacion <- estaciones %>%
  dplyr::mutate(elev = raster::raster(glue::glue('{config$dir$raster$dem}/Argentina_1000m.tif')) %>%
                  raster::extract(., estaciones %>% sf::st_transform(config$params$projections$posgar))) %>%
  dplyr::select(station_id, elev)

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 5. Creacion de matriz de precipitaciones mensuales por estacion ----
# -----------------------------------------------------------------------------#
# Calcular mediana de precipitaciones por mes.
# Me quedo con aquellas estaciones que tengan al menos 50% de los datos no faltantes.
# Luego, agrupo por station_id y me quedo con las estaciones que tienen todos los meses completos.
# Luego, pasamos los formato ancho para poder calcular distancias.
set.seed(0)
cantidad.anos           <- length(unique(estadisticas.mensuales$fecha)) / 12

estadisticas.clustering.mensuales <- estadisticas.mensuales  %>%
  # Calculo mes
  dplyr::mutate(ano = lubridate::year(fecha),
                mes = lubridate::month(fecha)) %>%
  # Agrupar por estacion y mes
  dplyr::group_by(station_id) %>%
  # Calcular mediana y cantidad de meses con datos disponibles
  dplyr::mutate(disponibles = sum(! is.na(valor))) %>%
  # Determinar si hay datos suficientes para la estacion y mes
  dplyr::mutate(suficientes = ! is.na(valor) & (disponibles >= 0.5 * 480)) %>%
  # Contar la cantidad de meses completos por estacion
  dplyr::mutate(anos_completos = sum(suficientes)) %>%
  dplyr::ungroup(.) %>%
  # Dejar solamente las estaciones que tienen los 12 meses completos
  dplyr::filter(anos_completos >= 300) %>%
  # Seleccionar estacion, mes y mediana de precipitacion acumulada mensual
  dplyr::select(station_id, fecha, valor) %>%
  # Pasar a formato ancho
  tidyr::spread(fecha, valor) 

library(missMDA)
# Imputar valores faltantes para asegurar un buen resultado en 
# el PCA. Los valores imputados no serán utilizados en la 
# etapa siguiente
estadisticas.clustering.mensual.impute <- missMDA::imputeCA(estadisticas.clustering.mensuales %>%
                                                               dplyr::select(-station_id))

estadisticas.clustering.mensuales <- estadisticas.clustering.mensuales %>%
  dplyr::select(station_id) %>%
  cbind(estadisticas.clustering.mensual.impute)


# Agregamos las coordenadas de cada estacion meteorologica
estadisticas.clustering.espacial <- estadisticas.clustering.mensuales %>%
  #dplyr::left_join(., estadisticas.clustering.estacionales, by = 'station_id') %>%
  dplyr::left_join(., estaciones.elevacion %>% sf::st_set_geometry(NULL) %>% dplyr::select(station_id, elev), by = 'station_id') %>%
  dplyr::ungroup(.) %>%
  dplyr::left_join(estaciones %>% dplyr::select(station_id, geometry), by = 'station_id') %>%
  sf::st_as_sf(.) %>%
  dplyr::select(-station_id) %>%
  #dplyr::select(starts_with('mediana_mensual')) %>%
  as(., "Spatial")

# extrae las coordenadas del objeto espatial
coordenadas <- sp::coordinates(estadisticas.clustering.espacial)

# Pasamos el data frame a matriz.
# Filas = estaciones. Columnas = meses
matriz.estadisticas.clustering <- estadisticas.clustering.espacial %>%
  sf::st_as_sf(.) %>%
  sf::st_set_geometry(NULL) %>%
  #dplyr::as_tibble() %>%
  as.matrix()
rownames(matriz.estadisticas.clustering) <- dplyr::pull(estadisticas.clustering.mensuales, station_id)

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 5. Metricas (PCA Robusto + K-Means) ----
# -----------------------------------------------------------------------------#
 
# Se extraen las variables sin las coordenadas espaciales
variables.agrupar  <- data.frame(estadisticas.clustering.espacial@data) %>%
  as.data.frame(.)

# Creación de la matriz de pesos espaciales
# Se detectan los k vecinos más cercanos a cada punto
col.knn <- spdep::knearneigh(coordenadas, k = 5)
# Los vecinos son convertidos a una lista
lista.vecinos <- spdep::knn2nb(col.knn)
# Se calcula la matrix de pesos espcaciales a partir de los vecinos
matriz.pesos.espaciales <- spdep::nb2listw(lista.vecinos, style = "W")

# Se calcula el coeficiente de Moran para 
# estimar la autocorrelación espacial

# Calculo del índice de Moran con remuestro 
indice.moran <- parallel::mclapply(variables.agrupar, moran.mc, matriz.pesos.espaciales, 999, mc.cores = 4)

resultados.indice.moran <- purrr::map_dfr(
  .x = names(variables.agrupar),
  .f = function(variable) {
    
    resultados <- data.frame(mes = NA, statistic = NA, p_value = NA)
    # Extraer resultados para el mes i
    indice.mora.mes.i <- indice.moran[[variable]]
    # Guardar resultados
    resultados$mes <- variable
    resultados$statistic <- indice.mora.mes.i$statistic
    resultados$p_value <- indice.mora.mes.i$p.value
    
    return(resultados)
  }
)


# Explorar cluster para distintos valores de K
resultados.clustering <- purrr::map(
  .x = seq(from = 2, to = 20),
  .f = function(k) {
    # Realizar PCA
    set.seed(0)
    
    # PCA sin componente espacial
    pca <- ade4::dudi.pca(variables.agrupar, scannf = F)
    # PCA con componente espacial
    ms <- adespatial::multispati(pca, matriz.pesos.espaciales, scannf = F)
    
    # Scores del PCA
    scores.pca <- ms$li
    # Variancia explicada por los componentes
    variance.pca <- ms$eig/sum(ms$eig)*100
    
    # Realizamos el clustering para el K dado
    cluster.pca.kmeans <- stats::kmeans(x = scores.pca, centers = k, iter.max = 100)
    
    # Calculamos metricas
    matriz.distancias.pca <- stats::dist(x = scores.pca, method = "euclidean")
    metricas <- list(
      WSS = cluster.pca.kmeans$tot.withinss,
      BSS = cluster.pca.kmeans$betweenss,
      SI = mean(cluster::silhouette(cluster.pca.kmeans$cluster, matriz.distancias.pca)[,3])
    )
    
    # Integramos los datos de los clusters con los IDS de las estaciones
    grupos.pca.kmeans <- purrr::map_dfr(
      .x = sort(unique(cluster.pca.kmeans$cluster)),
      .f = function(group_index) {
        estaciones %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::filter(station_id %in% as.integer(names(which(cluster.pca.kmeans$cluster == group_index)))) %>%
          dplyr::mutate(grupo = group_index)
      }
    ) %>% dplyr::select(station_id, grupo)
    
    # Geolozalizamos
    estaciones.regionalizadas.pca.kmeans <- estaciones %>%
      dplyr::left_join(grupos.pca.kmeans, by = c("station_id"))
    
    return (list(prop.pca.var = variance.pca, scores.pca = scores.pca,
      k = k, estaciones = estaciones.regionalizadas.pca.kmeans, metricas = metricas))
  }
)

# Elegimos k = 4
k.pca <- 4

# Graficar metricas
metricas.pca.kmeans <- purrr::map_dfr(
  .x = resultados.clustering,
  .f = function(resultado) {
    return (data.frame(k = resultado$k, WSS = resultado$metricas$WSS, BSS = resultado$metricas$BSS, SI = 5000*resultado$metricas$SI))
  }
) %>% tidyr::gather(key = metrica, value = valor, -k)
grafico.metricas <- ggplot2::ggplot(data = metricas.pca.kmeans) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = k, y = valor, col = metrica)) +
  ggrepel::geom_label_repel(data = data.frame(x = k.pca, y = 3000, label = paste0("k = ", k.pca)), 
                            mapping = ggplot2::aes(x = x, y = y, label = label),
                            nudge_x = 10, nudge_y = 0) +
  ggplot2::geom_vline(xintercept = k.pca, linetype = "dotted") +
  ggplot2::scale_y_continuous(name = "WSS/BSS", sec.axis = sec_axis(~./5000, name = "Silhouette Index")) +
  ggplot2::labs(x = "Cantidad de clusters", col = "Métrica") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'bottom')
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 6. PCA Robusto +  Fuzzy K-means ----
# -----------------------------------------------------------------------------#
# Hacer PCA robusto
set.seed(0)
# PCA sin componente espacial
pca <- ade4::dudi.pca(variables.agrupar, scannf = F, nf = 6)

prc <- prcomp(variables.agrupar, center=TRUE, scale=TRUE)
varimax7 <- varimax(prc$rotation[,1:7])
newData <- scale(x) %*% varimax7$loadings
# PCA con componente espacial
ms <- adespatial::multispati(pca, matriz.pesos.espaciales,
                             nfposi = 3,
                             nfnega = 3,
                             scannf = F)

# Scores del PCA
scores.pca <- ms$li
# Variancia explicada por los componentes
variance.pca <- (ms$eig/sum(ms$eig)*100)[1:10]
# Agregar nombres a las estaciones
rownames(scores.pca) <- dplyr::pull(estadisticas.clustering.mensuales, station_id)

cantidad.grupos <- NbClust::NbClust(scores.pca,
                                    distance = "euclidean", min.nc = 2, max.nc = 8, 
                                    method = 'kmeans', index = "all")


# Agrupar Scores del sPCA por K-means difuso
fuzzy.kmeans.estaciones <- e1071::cmeans(scores.pca, centers = 7, 
                              iter.max = 1000, verbose = TRUE,
                              dist = "euclidean", 
                              method = "cmeans", m = 10,
                              rate.par = NULL, 
                              #weights = 1, 
                              control = list())



cantidad.grupos$All.CriticalValues

aux <- cantidad.grupos$Best.nc %>%
  t(.) %>%
  tibble::as_tibble(.) %>%
  dplyr::mutate(indice = colnames(cantidad.grupos$Best.nc))

cantidad.grupos$All.index

grupos.estaciones <- data.frame(station_id = estadisticas.clustering.mensuales %>% dplyr::pull(station_id),
  grupo = fuzzy.kmeans.estaciones$cluster) %>%
  dplyr::left_join(., estaciones, by = 'station_id') %>%
  sf::st_as_sf()

plot(grupos.estaciones['grupo'])

# Nos quedamos con PC1 y PC2 ya que explican mas del 90% de la variabilidad.
# Obtengo los scores para PC1 y PC2 y hago clustering Jerarquico
scores.rob                 <- scores.pca
matriz.distancias.pca.rob  <- stats::dist(x = scores.rob, method = "euclidean")
cluster.jerarquico.pca.rob <- stats::hclust(d = matriz.distancias.pca.rob, method = "ward.D2")
plot(cluster.jerarquico.pca.rob)
cluster.pca.rob.station.id     <- stats::rect.hclust(cluster.jerarquico.pca.rob, k = 5, border = "red")

# Asignar grupos a estaciones que no participaron del clustering
cluster.estaciones.vecinas <-  dist_matrix %>% 
  tibble::as_tibble(.) %>%
  dplyr::mutate(station_id = names(.)) %>% 
  tidyr::gather(closest, dist,-station_id) %>% 
  dplyr::filter(!is.na(dist)) %>% 
  dplyr::group_by(station_id) %>% 
  dplyr::arrange(dist) %>% 
  dplyr::slice(1:5) %>% 
  dplyr::mutate(dist_rank = 1:5) %>%
  dplyr::ungroup(.) %>%
  dplyr::left_join(grupos.estaciones, by = c("closest" = "station_id")) %>%
  dplyr::rename(grupo.vecino = grupo)

# Agrupo las estaciones
grupos.estaciones.pca <- grupos.estaciones %>%
  dplyr::select(station_id, grupo)  %>%
  #sf::st_set_geometry(NULL) %>%
  dplyr::full_join(cluster.estaciones.vecinas, by = 'station_id') %>%
  dplyr::group_by(station_id) %>%
  dplyr::summarise(grupo = as.integer(unique(grupo)),
                   grupo.vecino = as.integer(statip::mfv(grupo.vecino, na_rm = TRUE))) %>%
  dplyr::mutate(grupo = if_else(is.na(grupo), grupo.vecino, grupo)) %>%
  dplyr::select(station_id, grupo) %>%
  dplyr::distinct()

# Integramos los datos para geolocalizarlos
cluster.estaciones.pca <- estaciones %>%
  dplyr::left_join(grupos.estaciones.pca, by = c("station_id"))
grupos.pca             <- cluster.estaciones.pca %>%
  dplyr::mutate(latitud = sf::st_coordinates(geometry)[,2],
                longitud = sf::st_coordinates(geometry)[,1]) %>%
  sf::st_set_geometry(NULL)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 7. Graficos ----
# -----------------------------------------------------------------------------#
aux <- resultados.clustering[[5]]

# i. Graficamos las proporciones de varianza explicadas
grafico.porcentaje.varianzas <- ggplot(data = data.frame(proporcion = aux$prop.pca.var[1:10], componente = 1:length(aux$prop.pca.var[1:10])), 
                                       mapping = ggplot2::aes(x = componente, y = proporcion, fill = componente)) + 
  ggplot2::scale_x_continuous(breaks = 1:length(aux$prop.pca.var)) +
  ggplot2::geom_bar(stat = 'identity') + 
  ggplot2::geom_text(mapping = ggplot2::aes(y = proporcion + 1, label = sprintf("%0.2f", proporcion))) + 
  ggplot2::labs(x = "Componente", y = "Porcentaje de varianza explicada") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = 'none')

# ii. Biplot para PC1 y PC2 y mostrar en colores los grupos
clases <- data.frame(station_id = as.character(rownames(matriz.estadisticas.clustering))) %>%
  dplyr::inner_join(grupos.estaciones.pca) %>%
  dplyr::pull(grupo) %>%
  as.factor()
grafico.biplot <- ggbiplot::ggbiplot(pcobj = pca, labels = NULL, groups = clases,
                                     ellipse = TRUE) +
  ggplot2::scale_colour_brewer(name = 'Región', type = "qual", palette = "Set1") +
  ggplot2::labs(x = sprintf("Componente 1 (%0.2f%%)", 100*prop.var.rob[1]), 
                y = sprintf("Componente 2 (%0.2f%%)", 100*prop.var.rob[2])) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'right') +
  ggplot2::coord_fixed(ratio = 0.5)

# iii. Mapa con graficos en popup
graficos.mapa <- purrr::map(
  .x = dplyr::pull(grupos.pca, station_id),
  .f = function(station_id) {
    tryCatch({
      estacion      <- dplyr::filter(grupos.pca, station_id == !! station_id)
      prcp.estacion <- matriz.estadisticas.clustering[as.character(station_id),]
      df.prcp.est   <- data.frame(x = as.character(names(prcp.estacion)), 
                                  y = as.double(prcp.estacion)) %>%
        dplyr::filter(stringr::str_detect(x, 'mediana_mensual_')) %>%
        dplyr::mutate(x = as.numeric(stringr::str_remove(x, 'mediana_mensual_')))
      grafico.est   <- ggplot2::ggplot(data = df.prcp.est) +
        ggplot2::geom_bar(mapping = ggplot2::aes(x = x, y = y, fill = as.factor(x)), stat = 'identity') +
        ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y + 10, label = sprintf("%.2f", y))) +
        ggplot2::scale_x_continuous(breaks = seq(from = 1, to = 12)) +
        ggplot2::labs(x = "Mes", y = "Mediana de prcp. acumulada (mm)",
                      title = paste0(estacion$nombre, " (", estacion$station_id, ")")) +
        ggplot2::theme_bw() + ggplot2::theme(legend.position = 'none')
      return (grafico.est)  
    }, error = function(e) {
      return (NA)
    })
  }
)

dom.grupo   <- dplyr::pull(grupos.pca, grupo)
pal.grupo   <- leaflet::colorFactor(palette = "Set1", domain = dom.grupo)
mapa.grupos <- leaflet::leaflet(data = grupos.pca) %>%
  leaflet::addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}") %>%
  leaflet::fitBounds(lng1 = ~min(longitud), lng2 = ~max(longitud), lat1 = ~min(latitud), lat2 = ~max(latitud)) %>%
  leaflet::addCircleMarkers(lng = ~longitud, lat = ~latitud, color = ~pal.grupo(grupo), radius = 7, stroke = FALSE, 
                            fillOpacity = 0.8, popup = leafpop::popupGraph(graficos.mapa, "svg")) %>%
  leaflet::addLegend(position = "bottomright", pal = pal.grupo, 
                     values = ~grupo, title = "Región", opacity = 1)

# iv. Graficos de scores vs. significado de las componentes
# a. PC1 vs totales anuales
pc1.data <- data.frame(station_id = as.character(rownames(scores.rob)), x = scores.rob[, 1], 
                       y = rowSums(matriz.estadisticas.clustering)) %>%
  dplyr::inner_join(grupos.estaciones.pca.rob, by = c("station_id")) %>%
  dplyr::select(station_id, x, y, grupo)
pc1.plot <- ggplot2::ggplot(data = pc1.data) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y, col = as.factor(grupo))) +
  ggplot2::scale_colour_brewer(name = 'Región', type = "qual", palette = "Set1") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "PC1", y = "Totales anuales")

# b. PC2 vs Sum(Mayo-Sep) - Sum(Nov-Mar)
pesos    <- c(-1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1)
pc2.data <- data.frame(station_id = as.character(rownames(scores.rob)), x = scores.rob[, 2], 
                       y = apply(matriz.estadisticas.clustering, MARGIN=1, 
                                 FUN=function(x){return(sum(x*pesos))})) %>%
  dplyr::inner_join(grupos.estaciones.pca.rob, by = c("station_id")) %>%
  dplyr::select(station_id, x, y, grupo)
pc2.plot <- ggplot2::ggplot(data = pc2.data) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y, col = as.factor(grupo))) +
  ggplot2::scale_colour_brewer(name = 'Región', type = "qual", palette = "Set1") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "PC2", y = "Sumas ponderadas")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 8. Generar informe ----
# -----------------------------------------------------------------------------#
# Crear HTML
if (! is.null(config$pandoc.custom.location)) {
  Sys.setenv(RSTUDIO_PANDOC = config$pandoc.custom.location)
}
output.dir <- paste0(config$dir$output, "/regionalizacion")
if (! dir.exists(output.dir)) {
  dir.create(output.dir)
}
rmarkdown::render(
  input = paste0(config$dir$base, "/InformeRegionalizacion.Rmd"),
  output_file = paste0(output.dir, "/InformeRegionalizacion.html"),
  output_dir = output.dir
)
# ------------------------------------------------------------------------------