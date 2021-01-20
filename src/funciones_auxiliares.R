fecha2mes <- function(x) {
  
  # Checking that 'class(x)==Date'
  if ( ( !( class(x) %in% c("Date", "POSIXct", "POSIXt") ) ) && TRUE )
    stop("Invalid argument: 'x' must be of class 'Date'")
  
  ####################
  months <- format(x, "%m")
  
  Enero <- which(months == '01')
  Febrero <- which(months == '02')
  Marzo <- which(months == '03')
  Abril <- which(months == '04')
  Mayo <- which(months == '05')
  Junio <- which(months == '06')
  Julio <- which(months == '07')
  Agosto <- which(months == '08')
  Septiembre <- which(months == '09')
  Octubre <- which(months == '10')
  Noviembre <- which(months == '11')
  Diciembre <- which(months == '12')
  
  
  # Creation of the output, with the same length of the 'x' input
  mes <- rep(NA, length(x))
  
  
  mes[Enero] <- "Enero"
  mes[Febrero] <- "Febrero"
  mes[Marzo] <- "Marzo"
  mes[Abril] <- "Abril"
  mes[Mayo] <- "Mayo"
  mes[Junio] <- "Junio"
  mes[Julio] <- "Julio"
  mes[Agosto] <- "Agosto"
  mes[Septiembre] <- "Septiembre"
  mes[Octubre] <- "Octubre"
  mes[Noviembre] <- "Noviembre"
  mes[Diciembre] <- "Diciembre"
  
  return(mes)
  
} 



fecha2estacion <- function(x, out.fmt="months") {
  
  # Checking that 'class(x)==Date'
  if ( ( !( class(x) %in% c("Date", "POSIXct", "POSIXt") ) ) && TRUE )
    stop("Invalid argument: 'x' must be of class 'Date'")
  
  # Checking the class of out.fmt
  if (is.na(match(out.fmt, c("seasons", "months") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('seasons', 'months')")
  
  ####################
  months <- format(x, "%m")
  
  summer <- which( months %in% c("12", "01", "02") )
  fall <- which( months %in% c("03", "04", "05") )
  winter <- which( months %in% c("06", "07", "08") )
  spring <- which( months %in% c("09", "10", "11") ) 
  
  # Creation of the output, with the same length of the 'x' input
  seasons <- rep(NA, length(x))
  
  if (out.fmt == "seasons") {
    
    seasons[winter] <- "Invierno"
    seasons[spring] <- "Primavera"
    seasons[summer] <- "Verano"
    seasons[fall] <- "Otoño"
    
  } else { # out.fmt == "months"
    
    seasons[summer] <- "DEF"
    seasons[fall] <- "MAM"
    seasons[winter] <- "JJA"
    seasons[spring] <- "SON"
    
    
  } # IF end
  
  return(seasons)
  
} # 'time2season' END

