
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
  "readxl", "ggplot2", "scales", "rlang")

source('./src/funciones_auxiliares.R')
#source('./src/depurdat.R')
#source('/Users/alessiobocco/Downloads/depurdat.R')

# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# --- Paso 4: Cargar datos necesarios ----
# ---------------------------------------------------------------------------- #

# Cargar estaciones 
estaciones.todas   <- readxl::read_xlsx(glue::glue("{config$dir$datos}/datos.xlsx"), sheet = 2, col_names = T,
  na = 'NA') %>%
  dplyr::mutate(., longitud = lon, latitud = lat) %>%
  sf::st_as_sf(x = ., coords = c("lon", "lat"), crs = config$params$projections$latlon) %>%
  sf::st_transform(., crs = config$params$projections$gk)


# Cargar shape de zonas homogéneas
zonas.homogeneas <- sf::st_read(dsn = glue::glue("{config$dir$resultados$shapefiles}/clusters.shp"),
  layer = 'clusters') %>%
  dplyr::rename('zona.homogenea' = 'z')

# ------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# --- Paso 5. Gráfico diagnósticos  ----
#------------------------------------------------------------------------------#
# Datos disponibles ----
datos <- readxl::read_xlsx(glue::glue("{config$dir$datos}/datos.xlsx"), sheet = 1, col_names = T,
  na = 'NA') %>%
  dplyr::mutate(fecha = as.Date(Fecha)) %>%
  dplyr::select(fecha, everything(), -Fecha) %>%
  tidyr::gather(., estacion, valor, -fecha) %>%
  dplyr::mutate(., valor = as.numeric(valor)) %>%
  tidyr::spread(., key = estacion, value = valor)

# Gráficos descriptivos de los datos 
# --------------------------------------------------------------------------- #
descriptivos <-   glue::glue("{config$dir$resultados$graficos}/descriptivos")

if (!fs::dir_exists(descriptivos)) {
  fs::dir_create(descriptivos)
}

# Meses validos ----
meses.validos <- datos %>%
  dplyr::mutate(meses.validos = rowSums(!is.na(.)) - 1) %>%
  dplyr::select(., fecha, meses.validos)

ggplot(data = meses.validos, aes(x = as.Date(fecha), y = meses.validos)) +
  geom_line() +
  scale_x_date(labels = date_format("%Y"), breaks='5 years') +
  ylab('Cantidad de estaciones') + xlab('Años') +
  theme_bw() +
  ggsave(glue::glue("{descriptivos}/cantidad_estaciones.png"), 
    device = 'png', dpi = 600, height = 3)
# -----------------------------------------------------------------------------

# Histograma de datos mensuales ----


# Grafico 
datos.x <- datos %>%
  tidyr::gather(., estacion, valor, -fecha) %>%
  dplyr::mutate(., fecha = as.Date(fecha), 
    mes = fecha2mes(fecha)) %>%
  dplyr::mutate(., mes = factor(mes, levels = c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo',
    'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre'), 
    labels = c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo',
      'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre'))) %>%
  tidyr::drop_na(.)

ggplot(data = datos.x, aes(x = valor)) +
  geom_histogram(fill = 'Steelblue', binwidth = 25) +
  facet_wrap(.~mes, scales = "free_y") +
  xlab('Precipitación [mm]') + ylab('Cantidad de meses') +
  theme_bw() +
  ggsave(glue::glue("{descriptivos}/frecuencia_mensual.png"), 
    device = 'png', dpi = 600, height = 8, width = 8)
# -----------------------------------------------------------------------------

# Leer parametros para cada zona
parametros_homogen <- purrr::pmap_dfr(
  .l = utils::tail(config$params$parametros.homogen, -1) %>% purrr::transpose(), 
  .f = function(..., nombres_columnas) { 
    tibble::tibble(..., .name_repair = "minimal") %>% setNames(nombres_columnas) },
  nombres_columnas = config$params$parametros.homogen[[1]]
) 

# Outliers ----
# Leer parametros para cada zona
outliers <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    outliers <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_out.csv"),
      stringsAsFactors = F) %>%
      dplyr::mutate(zona = zona.homogenea,
        Date = as.Date(Date)) %>%
      dplyr::mutate(., estacion.astronomica = fecha2estacion(Date),
        mes = fecha2mes(Date), 
        ano = lubridate::year(Date),
        signo = factor(if_else(Anomaly..std.devs.. > 0, "Derecha", "Izquierda"),
          levels = c("Izquierda", "Derecha")))
    
    
  }
)

# Estaciones astronomicas
ggplot2::ggplot(data = outliers, aes(x = factor(estacion.astronomica), fill = factor(zona))) +
  ggplot2::geom_bar() +
  facet_wrap(.~signo) +
  scale_fill_brewer(palette = "Set1", name = "Zonas") +
  theme_bw() +
  xlab("Estación del año") + ylab("Cantidad") +
  ggsave(glue::glue("{descriptivos}/outliers.png"), device = 'png', dpi = 600,
    height = 4)

# ------------------------------------------------------------------------------

# Quiebres ----
quiebres <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    quiebres <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_brk.csv"),
      stringsAsFactors = F) %>%
      dplyr::mutate(zona = zona.homogenea,
        Date = as.Date(Date)) %>%
      dplyr::mutate(., estacion.astronomica = fecha2estacion(Date),
        mes = fecha2mes(Date), 
        ano = lubridate::year(Date))
    
  }
)

# Quiebres por año
quiebres_año <- quiebres %>%
  dplyr::group_by(., ano, zona) %>%
  dplyr::summarise(., cantidad = n()) %>%
  dplyr::right_join(., data.frame(ano = seq(1941, 2015, 1)), by = 'ano') %>%
  dplyr::mutate(., cantidad = if_else(is.na(cantidad), 0 , as.double(cantidad)))

# Estaciones astronomicas
ggplot2::ggplot(data = quiebres_año, aes(x = (ano), y = cantidad, fill = factor(zona))) +
  ggplot2::geom_bar(stat = 'identity') +
  theme_bw() +
  ylab("Cantidad de quiebres") + 
  scale_x_continuous(name="Años", limits=c(1941, 2015),
    breaks=seq(1941, 2015, 5)) +
  scale_fill_brewer(palette = "Set1",
    name="Zonas",
    breaks=c("1", "2", "3"),
    labels=c("1", "2 ", "3")) +
  ggsave(glue::glue("{descriptivos}/cantidad_quiebres_ano.png"), 
    device = 'png', dpi = 600, height = 4)

# Quiebres estacion
quiebres_estacion <- quiebres %>%
  dplyr::select(., Code, Date, SNHT, estacion.astronomica, mes, ano, zona) %>%
  dplyr::group_by(., Code, zona) %>%
  dplyr::summarise(., cantidad = n())

# Estaciones astronomicas
ggplot2::ggplot(data = quiebres_estacion, aes(x = cantidad, fill = factor(zona))) +
  ggplot2::geom_bar(stat = 'count', width = 0.5) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1",
    name="Zonas",
    breaks=c("1", "2", "3"),
    labels=c("1", "2 ", "3")) +
  scale_x_continuous(breaks=seq(1,5), name = 'Cantidad de quiebres') +
  scale_y_continuous(breaks = seq(0, 50, 10), name = 'Cantidad de estaciones') +
  ggsave(glue::glue("{descriptivos}/cantidad_quiebres_estaciones.png"),
    device = 'png', dpi = 600, height = 4)

# ------------------------------------------------------------------------------

# Tendencias ----
# Leer parametros para cada zona
tendencias <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Tendencias del análisis definitivo
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}"))
    
    climatol::dahstat(varcli = glue::glue("Prcp-{zona.homogenea}"),
      anyi = parametros_homogen_zona$anyi,
      anyf = parametros_homogen_zona$anyf, 
      stat = 'tnd', 
      anyip = 1941, 
      anyfp = 2015)
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    # Evaluación de tendencias 
    tendencias_definitiva <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_tnd.csv")) %>%
      dplyr::select(., -c('X', "Y")) %>%
      tidyr::gather(., escala, valor, -c('Code')) %>%
      dplyr::mutate(., escala = factor(escala, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec", "Annual"),
        labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Anual")))%>%
      dplyr::mutate(tipo = 'definitivo')
    # --------------------------------------------------------------------------
    
    # Tendencias del análisis exploratorio
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados$exploratorio}/{zona.homogenea}"))
    
    climatol::dahstat(varcli = glue::glue("Prcp-{zona.homogenea}"),
      anyi = parametros_homogen_zona$anyi,
      anyf = parametros_homogen_zona$anyf, 
      stat = 'tnd', 
      anyip = 1941, 
      anyfp = 2015)
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    # Evaluación de tendencias 
    tendencias_exploratoria <- read.csv(glue::glue("{config$dir$resultados$exploratorio}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_tnd.csv")) %>%
      dplyr::select(., -c('X', "Y")) %>%
      tidyr::gather(., escala, valor, -c('Code')) %>%
      dplyr::mutate(., escala = factor(escala, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec", "Annual"),
        labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Anual"))) %>%
      dplyr::mutate(tipo = 'exploratorio')
    # --------------------------------------------------------------------------
    
    tendencias <- tendencias_definitiva %>%
      rbind(tendencias_exploratoria)
    
  }
)

ggplot(tendencias %>% filter(., escala != 'Anual'), aes(x = tipo, y = valor, fill = tipo)) +
  facet_wrap(.~escala, scales = 'free', ncol = 4) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Set1",
    breaks = c('definitivo', 'exploratorio'),
    labels = c("Homogeizados", "Originales")) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab("") + ylab("Tendencia [mm/100 años]") 
  ggsave(paste0(dir.graficos, 'tendencias.png'), device = 'png', dpi = 600, height = 8, width = 8)

# ------------------------------------------------------------------------------

# Desvios ----
# Leer parametros para cada zona
desvios <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Desvios del análisis definitivo
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}"))
    
    climatol::dahstat(varcli = glue::glue("Prcp-{zona.homogenea}"),
      anyi = parametros_homogen_zona$anyi,
      anyf = parametros_homogen_zona$anyf, 
      stat = 'std', 
      anyip = 1941, 
      anyfp = 2015)
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    # Evaluación de tendencias 
    desvios_definitivos <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_std.csv")) %>%
      dplyr::select(., -c('X', "Y")) %>%
      tidyr::gather(., escala, valor, -c('Code')) %>%
      dplyr::mutate(., escala = factor(escala, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec", "Annual"),
        labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Anual")))%>%
      dplyr::mutate(tipo = 'definitivo')
    # --------------------------------------------------------------------------
    
    # Desvíos del análisis exploratorio
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados$exploratorio}/{zona.homogenea}"))
    
    climatol::dahstat(varcli = glue::glue("Prcp-{zona.homogenea}"),
      anyi = parametros_homogen_zona$anyi,
      anyf = parametros_homogen_zona$anyf, 
      stat = 'std', 
      anyip = 1941, 
      anyfp = 2015)
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    # Evaluación de tendencias 
    desvios_exploratorios <- read.csv(glue::glue("{config$dir$resultados$exploratorio}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_std.csv")) %>%
      dplyr::select(., -c('X', "Y")) %>%
      tidyr::gather(., escala, valor, -c('Code')) %>%
      dplyr::mutate(., escala = factor(escala, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec", "Annual"),
        labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Anual"))) %>%
      dplyr::mutate(tipo = 'exploratorio')
    # --------------------------------------------------------------------------
    
    desvios <- desvios_definitivos %>%
      rbind(desvios_exploratorios)
    
  }
)
  
ggplot(desvios %>% filter(., escala != 'Anual'), aes(x = tipo, y = valor, fill = tipo)) +
  facet_wrap(.~escala, scales = 'free', ncol = 4) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Set1",
    breaks = c('definitivo', 'exploratorio'),
    labels = c("Homogeizados", "Originales")) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab("") + ylab("Tendencia [mm/100 años]") 
  ggsave(paste0(dir.graficos, 'tendencias.png'), device = 'png', dpi = 600, height = 8, width = 8)
  
# ------------------------------------------------------------------------------

# Normales ----
# Leer parametros para cada zona
series_normales <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Desvios del análisis definitivo
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}"))
    
    climatol::dahstat(varcli = glue::glue("Prcp-{zona.homogenea}"),
      anyi = parametros_homogen_zona$anyi,
      anyf = parametros_homogen_zona$anyf, 
      stat = 'series', 
      anyip = 1941, 
      anyfp = 2015)
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    # Normales
    series_definitiva <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_series.csv"),
      stringsAsFactors = F) %>%
      dplyr::rename(fecha = Date) %>%
      dplyr::mutate(fecha = as.Date(fecha)) %>%
      tidyr::gather(station_id, valor, -fecha)
    
    normales_definitiva_1941_1970 <- series_definitiva %>%
      dplyr::filter(lubridate::year(fecha) >= 1941, lubridate::year(fecha) <= 1970) %>%
      dplyr::group_by(ano = lubridate::year(fecha), station_id) %>%
      dplyr::summarise(valor = sum(valor)) %>%
      dplyr::mutate(periodo = '1941-1970',
        tipo = 'definitivo')
    
    normales_definitiva_1971_2000 <- series_definitiva %>%
      dplyr::filter(lubridate::year(fecha) >= 1971, lubridate::year(fecha) <= 2000) %>%
      dplyr::group_by(ano = lubridate::year(fecha), station_id) %>%
      dplyr::summarise(valor = sum(valor)) %>%
      dplyr::mutate(periodo = '1941-1970',
        tipo = 'definitivo')

    # --------------------------------------------------------------------------
    
    # Normales del análisis exploratorio
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados$exploratorio}/{zona.homogenea}"))
    
    climatol::dahstat(varcli = glue::glue("Prcp-{zona.homogenea}"),
      anyi = parametros_homogen_zona$anyi,
      anyf = parametros_homogen_zona$anyf, 
      stat = 'series', 
      anyip = 1941, 
      anyfp = 2015)
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    # Normales
    series_exploratoria <- read.csv(glue::glue("{config$dir$resultados$exploratorios}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_series.csv"),
      stringsAsFactors = F) %>%
      dplyr::rename(fecha = Date) %>%
      dplyr::mutate(fecha = as.Date(fecha)) %>%
      tidyr::gather(station_id, valor, -fecha)
    
    normales_exploratoria_1941_1970 <- series_exploratoria %>%
      dplyr::filter(lubridate::year(fecha) >= 1941, lubridate::year(fecha) <= 1970) %>%
      dplyr::group_by(ano = lubridate::year(fecha), station_id) %>%
      dplyr::summarise(valor = sum(valor)) %>%
      dplyr::mutate(periodo = '1971-2000',
        tipo = 'exploratorio')
    
    normales_exploratoria_1971_2000 <- series_exploratoria %>%
      dplyr::filter(lubridate::year(fecha) >= 1971, lubridate::year(fecha) <= 2000) %>%
      dplyr::group_by(ano = lubridate::year(fecha), station_id) %>%
      dplyr::summarise(valor = sum(valor)) %>%
      dplyr::mutate(periodo = '1971-2000',
        tipo = 'exploratorio')
    
    # --------------------------------------------------------------------------
    
    normales <- normales_definitiva_1941_1970 %>%
      rbind(normales_definitiva_1971_2000, normales_exploratoria_1941_1970, normales_exploratoria_1971_2000)
    
    return(normales)
  }
)

ggplot(desvios %>% filter(., escala != 'Anual'), aes(x = tipo, y = valor, fill = tipo)) +
  facet_wrap(.~escala, scales = 'free', ncol = 4) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Set1",
    breaks = c('definitivo', 'exploratorio'),
    labels = c("Homogeizados", "Originales")) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab("") + ylab("Tendencia [mm/100 años]") 
ggsave(paste0(dir.graficos, 'tendencias.png'), device = 'png', dpi = 600, height = 8, width = 8)

# ------------------------------------------------------------------------------

# Datos complementarios ----
# Leer parametros para cada zona
anomalias <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Tendencias del análisis definitivo
    # Definir directorio de datos
    #setwd(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}"))
    
    # Leer datos de entrada
    load(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}.rda"))
    
    # Codigos de las estaciones 
    codigos.estacion <- est.c$Code
    
    # Anomalias absolutas
    anomalias <- anom %>%
      as.data.frame(.) %>%
      dplyr::rename_all(., ~codigos.estacion) %>%
      dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
      tidyr::gather(., estacion, valor, -fecha) %>%
      dplyr::mutate(zona = zona.homogenea)
    
    
    # --------------------------------------------------------------------------
    
    return(anomalias)
    
  }
)

anomalias.estandarizadas <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Leer datos de entrada
    load(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}.rda"))
    
    # Codigos de las estaciones 
    codigos.estacion <- est.c$Code
    
    # Anomalias estandarizadas
    anomalias.estandarizadas <- sanom %>%
      as.data.frame(.) %>%
      dplyr::rename_all(., ~codigos.estacion) %>%
      dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
      tidyr::gather(., estacion, valor, -fecha) %>%
      dplyr::mutate(zona = zona.homogenea)
    
    # --------------------------------------------------------------------------
    
    return(anomalias.estandarizadas)
    
  }
)

datos.homogeneizados <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
   
    # Leer datos de entrada
    load(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}.rda"))
    
    # Codigos de las estaciones 
    codigos.estacion <- est.c$Code
    
    # Datos homogenizados
    datos.homogeneizados <- dah %>%
      matrix(., nrow = 12 * 75, byrow=F) %>%
      as.data.frame(.) %>%
      dplyr::rename_all(., ~codigos.estacion) %>%
      dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
      tidyr::gather(., estacion, valor, -fecha) %>%
      dplyr::mutate(zona = zona.homogenea)
    
    
    # --------------------------------------------------------------------------
    
    return(datos.homogeneizados)
    
  }
)

datos.estandarizados <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Leer datos de entrada
    load(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}.rda"))
    
    # Codigos de las estaciones 
    codigos.estacion <- est.c$Code
    
    # Datos observados estandarizados
    datos.observados.estandarizados <- dat.z %>%
      as.data.frame(.) %>%
      dplyr::rename_(.dots = set_names(names(.), codigos.estacion)) %>%
      dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
      tidyr::gather(., estacion, valor, -fecha) %>%
      dplyr::mutate(zona = zona.homogenea)
    
    
    # --------------------------------------------------------------------------
    
    return(datos.observados.estandarizados)
    
  }
)

datos.observados.estandarizados <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Leer datos de entrada
    load(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}.rda"))
    
    # Codigos de las estaciones 
    codigos.estacion <- est.c$Code
    
    
    # Datos estimados estandarizados
    datos.estimados.estandarizados <- dat.e %>%
      as.data.frame(.) %>%
      dplyr::rename_all(., ~codigos.estacion) %>%
      dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
      tidyr::gather(., estacion, valor, -fecha) %>%
      dplyr::mutate(zona = zona.homogenea)
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    
    # --------------------------------------------------------------------------
    
    return(datos.estimados.estandarizados)
    
  }
)

datos.estimados <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Leer datos de entrada
    load(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}.rda"))
    
    # Codigos de las estaciones 
    codigos.estacion <- est.c$Code
    
    # Datos estimados
    datos.estimados <- dat.c %>%
      as.data.frame(.) %>%
      dplyr::rename_all(., ~codigos.estacion) %>%
      dplyr::mutate(., fecha = seq(as.Date('1941-01-01'), as.Date('2015-12-31'), by = 'months')) %>%
      tidyr::gather(., estacion, valor, -fecha) %>%
      dplyr::mutate(zona = zona.homogenea)
    
    
    # --------------------------------------------------------------------------
    
    return(datos.estimados)
    
  }
)

# Comparacion con estacion de referencia 
comparacion.referencia <- datos.estimados %>%
  dplyr::filter(., estacion == 'S120') %>%
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
  xlab('Años') + ylab("Precipitación acumulada anual [mm]")+
  ggsave(glue::glue("{descriptivos}/comparacion_serie_referencia.png"), 
    device = 'png', dpi = 600, height = 3)

# ------------------------------------------------------------------------------

# Quiebres en series y tipo de datos ----
series_anuales_flags <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    
    # Precipitacion
    series_precipitacion <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_series.csv"),
      stringsAsFactors = F) %>%
      dplyr::rename(fecha = Date) %>%
      dplyr::mutate(fecha = as.Date(fecha)) %>%
      tidyr::gather(station_id, valor, -fecha)
    
    # Flags
    flags_precipitacion <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_flags.csv"),
      stringsAsFactors = F) %>%
      dplyr::rename(fecha = Date) %>%
      dplyr::mutate(fecha = as.Date(fecha)) %>%
      tidyr::gather(station_id, flag, -fecha)
    
    # Combinar datos con los flags de cada mes
    precipitacion <- series_precipitacion %>%
      dplyr::left_join(flags_precipitacion, by = c('station_id', 'fecha'))  %>%
      dplyr::mutate(., ano = lubridate::year(fecha)) %>%
      dplyr::group_by(., ano, station_id) %>%
      dplyr::summarise(., valor = sum(valor),
        flag = round(mean(flag), 0)) %>%
      dplyr::ungroup(.) %>%
      dplyr::mutate(., ano = lubridate::ymd(ano, truncated = 2L)) %>%
      dplyr::mutate(., flag = factor(flag, levels = c("0", "1", "2"))) %>%
      dplyr::mutate(zona = zona.homogenea)
    
    # --------------------------------------------------------------------------
    
    return(precipitacion)
  }
)

ggplot2::ggplot(series_anuales_flags %>%
    dplyr::filter(station_id == 'S415'), ggplot2::aes(ano, valor, color = flag))+
  geom_path(aes(group = 1)) +
  geom_point() +
  scale_color_brewer(palette = 'Set1',
    name = 'Tipo',
    breaks = c("0", "1", "2"),
    labels = c("Observado", "Rellenado", "Corregido")) +
  geom_vline(data = quiebres  %>%
      dplyr::filter(Code == 'S415'), aes(xintercept = Date), linetype = "dashed") +
  scale_y_continuous(breaks = seq(300, 1600, by = 100)) +
  theme_bw() +
  xlab("Años") + ylab("Precipitación anual [mm]") 
  #ggsave(paste0(dir.graficos, 'precipitacion_anual_S352-2.png'), device = 'png', dpi = 600)

# ----------------------------------------------------------------------------- 

# Sumas acumulativas ----
# Sumas de datos homogenea definitiva
sumas_precipitacion_homogenea <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    
    # Precipitacion
    series_precipitacion <- read.csv(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_series.csv"),
      stringsAsFactors = F) %>%
      dplyr::rename(fecha = Date) %>%
      dplyr::mutate(fecha = as.Date(fecha)) %>%
      tidyr::gather(station_id, valor, -fecha) 
    
    
    # Combinar datos con los flags de cada mes
    precipitacion <- series_precipitacion  %>%
      dplyr::mutate(ano = lubridate::year(fecha)) %>%
      dplyr::group_by(ano, station_id) %>%
      dplyr::summarise(., valor = sum(valor)) %>%
      dplyr::mutate(., normal.1941.1970 = dplyr::filter(., ano >= '1941' & ano <= '1970') %>%
          dplyr::group_by(station_id) %>%
          dplyr::summarise(normal = mean(valor)) %>%
          dplyr::pull(normal)) %>%
      dplyr::mutate(., normal.1971.2000 = dplyr::filter(., ano >= '1971' & ano <= '2000') %>%
          dplyr::group_by(station_id) %>%
          dplyr::summarise(normal = mean(valor)) %>%
          dplyr::pull(normal)) %>%
      dplyr::mutate(., normal = dplyr::filter(., ano >= '1941' & ano <= '2015') %>%
          dplyr::group_by(station_id) %>%
          dplyr::summarise(normal = mean(valor)) %>%
          dplyr::pull(normal)) %>%
      dplyr::mutate(., anomalia.1941.1970 = valor - normal.1941.1970,
        anomalia.1971.2000 = valor - normal.1971.2000,
        anomalia = valor - normal) %>%
      dplyr::group_by(., station_id) %>%
      dplyr::arrange(ano) %>%
      dplyr::mutate(., cumsum = cumsum(anomalia)) %>%
      dplyr::mutate(zona = zona.homogenea) %>%
      dplyr::ungroup(.)
    
    # --------------------------------------------------------------------------
    
    return(precipitacion)
  }
)

# Sumas de datos original
sumas_precipitacion_original <- purrr::map_dfr(
  .x = unique(zonas.homogeneas$zona.homogenea),
  .f = function(zona.homogenea) {
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    
    # Precipitacion
    series_precipitacion <- read.csv(glue::glue("{config$dir$resultados$exploratorio}/{zona.homogenea}/Prcp-{zona.homogenea}_{parametros_homogen_zona$anyi}-{parametros_homogen_zona$anyf}_series.csv"),
      stringsAsFactors = F) %>%
      dplyr::rename(fecha = Date) %>%
      dplyr::mutate(fecha = as.Date(fecha)) %>%
      tidyr::gather(station_id, valor, -fecha) 
    
    
    # Combinar datos con los flags de cada mes
    precipitacion <- series_precipitacion  %>%
      dplyr::mutate(ano = lubridate::year(fecha)) %>%
      dplyr::group_by(ano, station_id) %>%
      dplyr::summarise(., valor = sum(valor)) %>%
      dplyr::mutate(., normal.1941.1970 = dplyr::filter(., ano >= '1941' & ano <= '1970') %>%
          dplyr::group_by(station_id) %>%
          dplyr::summarise(normal = mean(valor)) %>%
          dplyr::pull(normal)) %>%
      dplyr::mutate(., normal.1971.2000 = dplyr::filter(., ano >= '1971' & ano <= '2000') %>%
          dplyr::group_by(station_id) %>%
          dplyr::summarise(normal = mean(valor)) %>%
          dplyr::pull(normal)) %>%
      dplyr::mutate(., normal = dplyr::filter(., ano >= '1941' & ano <= '2015') %>%
          dplyr::group_by(station_id) %>%
          dplyr::summarise(normal = mean(valor)) %>%
          dplyr::pull(normal)) %>%
      dplyr::mutate(., anomalia.1941.1970 = valor - normal.1941.1970,
        anomalia.1971.2000 = valor - normal.1971.2000,
        anomalia = valor - normal) %>%
      dplyr::group_by(., station_id) %>%
      dplyr::arrange(ano) %>%
      dplyr::mutate(., cumsum = cumsum(anomalia)) %>%
      dplyr::mutate(zona = zona.homogenea) %>%
      dplyr::ungroup(.)
    
    # --------------------------------------------------------------------------
    
    return(precipitacion)
  }
)


gg <- ggplot(sumas_precipitacion_homogenea %>%
    dplyr::filter(., station_id %in% c('S122 ','S120', 'S121', 'S123')), aes(x = ano, y = cumsum, color = station_id)) +
  geom_line() +
  scale_color_brewer(palette = 'Set1',
    name = 'Estación',
    breaks = c('S122 ','S120', 'S121', 'S123'),
    labels = c('Las Acequias','Arata', 'San Javier Yacanto', 'Ascochinga')) +
  xlab('Año') + ylab('Suma acumulativa de precipitación [mm]') +
  theme_bw() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-6000, 2000)) +
  theme(legend.position = 'bottom')

pp <- ggplot(sumas_precipitacion_original%>%
    dplyr::filter(., station_id %in% c('S122 ','S120', 'S121', 'S123')), aes(x = ano, y = cumsum, color = station_id)) +
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

# ----------------------------------------------------------------------------- 
