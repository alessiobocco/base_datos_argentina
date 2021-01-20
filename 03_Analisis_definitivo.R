# No te olvides de editar la funcion pegando esto
#save(dat,dah,est.c,nd,ne,nei,nm,x,ndec,std,ini,zy, tVx, snhx, sanom, anom,
#     dat.z, dat.e, dat.c, file=sprintf('%s.rda',fbas))
library(climatol)
trace('homogen', edit = T)
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
# --- Paso 4: Correr proceso definitivo ----
# ---------------------------------------------------------------------------- #

# Leer parametros para cada zona
parametros_homogen <- purrr::pmap_dfr(
  .l = utils::tail(config$params$parametros.homogen, -1) %>% purrr::transpose(), 
  .f = function(..., nombres_columnas) { 
    tibble::tibble(..., .name_repair = "minimal") %>% setNames(nombres_columnas) },
  nombres_columnas = config$params$parametros.homogen[[1]]
) 

parametros_homogen$wd <- list(as.integer(stringr::str_split(parametros_homogen$wd,",")[[1]]))
parametros_homogen$nref <- list(as.integer(stringr::str_split(parametros_homogen$nref,",")[[1]]))

# Correr homogenizacion
purrr::map(
  .x = unique(parametros_homogen$zona),
  .f = function(zona.homogenea) {
    
    config$dir$resultados$definitivos
    if (!fs::dir_exists(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}"))) {
      fs::dir_create(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}"))
    }
    
    
    # Copiar metadatos estaciones
    base::file.copy(from = glue::glue("{config$dir$resultados$datos}/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.est"),
      to = glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.est"),
      overwrite = TRUE)
    
    # Copiar datos observados de estaciones
    base::file.copy(from = glue::glue("{config$dir$resultados$datos}/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.dat"),
      to = glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}/Prcp-{zona.homogenea}_1941-2015.dat"),
      overwrite = TRUE)
    
    # Definir directorio de datos
    setwd(glue::glue("{config$dir$resultados$definitivos}/{zona.homogenea}"))
    
    parametros_homogen_zona <- parametros_homogen %>%
      dplyr::filter(zona == zona.homogenea)
    
    # Correr analisis exploratorio
    climatol::homogen(varcli = glue::glue("Prcp-{zona.homogenea}"),
      anyi = parametros_homogen_zona$anyi,
      anyf = parametros_homogen_zona$anyf, 
      expl = parametros_homogen_zona$exp, 
      std = parametros_homogen_zona$std, 
      snht1 = parametros_homogen_zona$snht1,
      snht2 = parametros_homogen_zona$snht2,
      dz.max = parametros_homogen_zona$dz.max,
      dz.min = parametros_homogen_zona$dz.min,
      wd = parametros_homogen_zona$wd[[1]], 
      gp = parametros_homogen_zona$gp, 
      nref = parametros_homogen_zona$nref[[1]],
      ndec = 1)
    
    # Renombrar salidas
    #climatol::outrename(glue::glue("Prcp-{zona.homogenea}"), 1941, 2015, "-exploratorio")
    
    # Definir nuevamente el directorio base de trabajo
    setwd(glue::glue("{config$dir$base}"))
    
    
  }
)

#trace('homogen', edit = T)

# ------------------------------------------------------------------------------

