# ---------------------------------------------------------------------------- #
# --- Paso 1: Borrar espacio de trabajo ----
# ---------------------------------------------------------------------------- #

# Borrar espacio de trabajo
rm(list = objects()); gc()
# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# --- Paso 2: Cargar e instalar paquetes necesarios ----
# ---------------------------------------------------------------------------- #
if (!require("pacman")) install.packages("pacman", repos = 'http://cran.us.r-project.org')

# Instalar o cargar los paquetes necesarios
pacman::p_load("dplyr", "tidyr", "raster", "sf", "maptools",
  "spdep", "ade4", "Guerry", "spdep", "e1071", "magrittr", "qwraps2", "missMDA")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Leer archivos YML de configuracion y parametros ----

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
# --- PASO 4: Descargar los datos necesarios ----
# -----------------------------------------------------------------------------#
# Funcion para descargar archivo
DownloadFile <- function(base.url, file, output.dir, max.retries) {
  # a. Creacion de carpeta destino si no existe
  if (! dir.exists(output.dir)) {
    dir.create(output.dir)
  }
  
  # b. Generar path para archivo de salida
  destfile <- paste0(output.dir, "/", file)
  
  # c. Intentar descargar con una cierta cantidad de retries
  retries <- 0
  success <- FALSE
  while (! success && (retries < max.retries)) {
    full.url  <- paste0(base.url, file)
    exit.code <- utils::download.file(full.url, destfile, method="wget", mode="wb", quiet = TRUE)
    if (exit.code == 0) {
      success <- TRUE
    } else {
      retries <- retries + 1
      if (retries < max.retries) {
        Log.warn(paste0("Error al descargar el archivo ", file, " en el intento ", retries))
      }
    }
  }
  
  return (success)
}

# Verificar que los datos ya hayan sido descargados
list.files.prcp.original <- purrr::map_chr(
  .x = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'),
  .f = function(mes) {
    
    glue::glue("{config$dir$raster$worldclim$prcp}/{config$download$prcp$file}_{mes}.tif")
  }
)

list.files.prcp.clip <- purrr::map_chr(
  .x = seq(1, 12, 1),
  .f = function(mes) {
    
    glue::glue("{config$dir$raster$worldclim$prcp}/prcp_{mes}.tif")
  }
)

# Descargar archivos de precipitacion
if (!any(fs::file_exists(glue::glue("{config$dir$raster$worldclim$prcp}/{config$download$prcp$file}")) ,
    any(fs::file_exists(list.files.prcp.original)),
    any(fs::file_exists(list.files.prcp.clip)))) {
  file.success <- DownloadFile(base.url = config$download$prcp$url,
    file = paste0(config$download$prcp$file, '.zip'),
    output.dir = config$dir$raster$worldclim$prcp,
    max.retries = config$download$max.retries)
  if (! file.success) {
    file.remove(paste0(config$dir$output, config$download$prcp$file))
    cat(paste0("Se intento descargar el archivo ",  config$download$prcp$file, " ", config$download$retries, " veces sin exito. Borrando y abortando."))
    downloader$stop()
    q()
  } else {
    cat(paste0("Archivo ", config$download$prcp$file, " descargado exitosamente"))
    unzip(exdir = config$dir$raster$worldclim$prcp,
      paste0(config$dir$raster$worldclim$prcp, "/", config$download$prcp$file, ".zip"))
    file.remove(paste0(config$dir$raster$worldclim$prcp, "/", config$download$prcp$file))
  }
}

# Descargar archivos de variables biocliomáticas

# Verificar que los datos ya hayan sido descargados
list.files.bio.original <- purrr::map_chr(
  .x = seq(1, 19, 1),
  .f = function(variable) {
    
    glue::glue("{config$dir$raster$worldclim$bio}/{config$download$bio$file}_{variable}.tif")
  }
)

list.files.bio.clip <- purrr::map_chr(
  .x = seq(1, 19, 1),
  .f = function(mes) {
    
    glue::glue("{config$dir$raster$worldclim$bio}/bio_{mes}.tif")
  }
)

if (!any(fs::file_exists(glue::glue("{config$dir$raster$worldclim$bio}/{config$download$bio$file}")) ,
  any(fs::file_exists(list.files.prcp.original)),
  any(fs::file_exists(list.files.prcp.clip)))) {
  file.success <- DownloadFile(base.url = config$download$bio$url,
    file = config$download$bio$file,
    output.dir = config$dir$raster$worldclim$bio,
    max.retries = config$download$max.retries)
  if (! file.success) {
    file.remove(paste0(config$dir$output, config$download$bio$file))
    cat(paste0("Se intento descargar el archivo ",  config$download$bio$file, " ", config$download$retries, " veces sin exito. Borrando y abortando."))
    downloader$stop()
    q()
  } else {
    cat(paste0("Archivo ", config$download$bio$file, " descargado exitosamente"))
    unzip(exdir = config$dir$raster$worldclim$bio,
      paste0(config$dir$raster$worldclim$bio, "/", config$download$bio$file, ".zip"))
    file.remove(paste0(config$dir$raster$worldclim$bio, "/", config$download$bio$file))
  }
}

# ----------------------------------------------------------------------------- 

# -----------------------------------------------------------------------------#
# --- Paso 5: Recortar rasters al área en estudio ----
# -----------------------------------------------------------------------------#
# Shapefiles
# Cargar shapefile con la zona en estudio
zona.estudio       <- sf::st_read(dsn = glue::glue("{config$dir$shapefiles}"), 
  layer = glue::glue("{config$params$shapefile$area_estudio}"))
# Crear shapefile con area de estudio más zona de influencia
zona.extendida.planar     <- sf::st_transform(x = zona.estudio, crs = config$params$projections$gk) #%>%
#sf::st_buffer(dist = buffer * 1000)  # La distancia debe ser especificada en metros
zona.extendida.geo     <- sf::st_transform(x = zona.extendida.planar, crs = config$params$projections$latlon) 

e <- raster::extent(zona.extendida.planar) 
e <- raster::raster(e,nrows=1,ncols=1,crs=config$params$projections$gk)
raster::res(e) <- c(10000, 10000)
raster::values(e) <- 0

# Recorte de la precipitacion al área de estudio
if (any(fs::file_exists(list.files.prcp.original))) {
  
  # Recorte de la precipitacion al área de estudio
  purrr::map2(
    .x = list.files.prcp.original,
    .y = seq(1, 12, 1),
    .f = function(archivo, mes) {
      
      raster::raster(archivo) %>%
        raster::projectRaster(., crs = config$params$projections$gk) %>%
        raster::crop(., y = zona.extendida.planar) %>%
        raster::resample(., e) %>%
        raster::writeRaster(., filename = glue::glue("{config$dir$raster$worldclim$prcp}/prcp_{mes}.tif"))
      
      file.remove(archivo)
      
    }
  )
} else {
  cat('Las capas de precipitación ya han sido descargadas y cortadas')
}

# Recorte de la precipitacion al área de estudio
if (any(fs::file_exists(list.files.bio.original))) {
  
  # Recorte de la precipitacion al área de estudio
  purrr::map2(
    .x = list.files.bio.original,
    .y = seq(1, 19, 1),
    .f = function(archivo, variable) {
      
      tryCatch({
      
      raster::raster(archivo) %>%
        raster::projectRaster(., crs = config$params$projections$gk) %>%
        raster::crop(., y = zona.extendida.planar) %>%
        raster::resample(., e) %>%
        raster::writeRaster(., filename = glue::glue("{config$dir$raster$worldclim$bio}/bio{variable}.tif"))
      
      cat('El archivo', glue::glue("{config$dir$raster$worldclim$bio}/bio_{variable}.tif"), 'está listo')
      
      file.remove(archivo)
      
      }, error = function(e) {
        return (NA)
      })
      
    }
  )
} else {
  cat('Las capas de precipitación ya han sido descargadas y cortadas \n')
}

# DEM
if (fs::file_exists(config$dir$raster$dem)) {
      
      tryCatch({
        
        modelo.elevacion <- raster::raster(glue::glue('{config$dir$raster$dem}/Argentina_1000m.tif')) %>%
          raster::projectRaster(., crs = config$params$projections$gk) %>%
          raster::crop(., y = zona.extendida.planar) %>%
          raster::resample(., e)  %>%
          raster::writeRaster(., filename = glue::glue("{config$dir$raster$dem}/dem_Argentina.tif"))
        
        cat('El archivo', glue::glue("{config$dir$raster$dem}_Argentina.tif"), 'está listo')
        
        file.remove(glue::glue('{config$dir$raster$dem}/Argentina_1000m.tif'))
        
      }, error = function(e) {
        return (NA)
      })

} else {
  cat('Las capa ya ha sido cortada y su resolucion corregida \n')
}

# ----------------------------------------------------------------------------- 

