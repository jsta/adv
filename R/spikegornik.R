#'@name spikegornik
#'@title Despike ADV data using the GoringNikora phase-space procedure
#'@description Despike ADV data
#'@author Joseph Stachelek
#'@param fpath file.path to ADV output where the column of interest is named "Speed"
#'@param outpath file.path to save results. optional.
#'@param maxcounter numeric number of smoothing passes
#'@param plotting logical output progress to plots?
#'@param plotwin numeric vector of length 2 specifying the plotting window
#'@details This function is nearly a direct translation of the MacVicar et al. (2014) SpikeGoringNikora.m script from MATLAB to R. Single value spikes are not removed if they are the same as a linear interpolation between adjacent points. Remove measurements where the SNR > 5 per the ADV manual?
#'@references \url{https://github.com/macvicab/MITT} 
#'@return data.frame datetime of class POSIXct and speed of class numeric
#'@export
#'@examples \dontrun{
#'fpath <- "inst/extdata/2014-10-14_R322_001_Z5-3.dat"
#'outpath <- "inst/extdata/2014-10-14_R322_001_Z5-3.csv"
#'res <- spikegornik(fpath)
#'res <- spikegornik(fpath, outpath)
#' plot(res, type = "l")
#'}

spikegornik <- function(fpath, outpath = NULL, maxcounter = 5, plotting = TRUE, plotwin = c(1, nrow(dt))){
  
	dt <- read.table(fpath, header = TRUE)
  speed <- data.frame(dt[, "Speed"])
  names(speed) <- "speed"
  univmult <- sqrt(log(nrow(speed)))
  univthresh <- univmult * (2 * log(nrow(speed)))^0.5       
  sig <- seq(from = 0, to = (2 * pi), by=(2 * pi/144))
  
  not_spike <- matrix(0, nrow = nrow(speed), ncol = 3)
  
  spike <- 1
  counter <- 0
    
  while(spike > 0){
  
  #step 1===================================================#
    speed$deriv.1 <- c(0, (speed[(3:(nrow(speed))), 1] - speed[1:(nrow(speed) - 2), 1]) / 2, 0)
    speed$deriv.2 <- c(0, 0, (speed[(4:(nrow(speed) - 1)), 2] - speed[2:(nrow(speed) - 3), 2]) / 2, 0, 0)
    
  #step 2===================================================#
  #modified stdev calculation following: Wahl, T. L. (2003), Discussion of Despiking Acoustic Doppler Velocimeter Data by Derek G. Goring and Vladimir I. Nikora, Journal of Hydraulic Engineering, 129(6), 484-487.
    std_of_speed <- sapply(1:3, function(x) 1.483 * median(abs(speed[,x] - median(speed[, x]))))
  
  #step 2b==================================================#
    speedmax <- univthresh * std_of_speed
  
  #step 3===================================================#
    theta <- c(0, 0, atan(sum(speed[,1] * speed[,3]) / sum(speed[,1]^2)))
  
  #step 4===================================================#
    xa <- c(1, 2, 1)
    ya <- c(2, 3, 3)
  
    pol2cart <- function(theta, rho){
      x <- rho * cos(theta)
      y <- rho * sin(theta)
      c(x, y)
    }
    
    for(fg in 1:3){
      a <- xa[fg]
      b <- ya[fg]
    
      r <- ((speedmax[a]^2 * speedmax[b]^2)/(speedmax[a]^2 * (sin(sig))^2+speedmax[b]^2 * (cos(sig))^2))^0.5
    
      xy <- t(apply(cbind(sig, r), 1, function(x) pol2cart(x[1], x[2])))
      x <- xy[,1] * cos(theta[fg]) - xy[,2] * sin(theta[fg])
      y <- xy[,2] * cos(theta[fg]) + xy[,1] * sin(theta[fg])
      
      not_spike[,fg] <- inellipse(speed[,a], speed[,b], x, y)
    }
  
    min_of_not_spike <- apply(not_spike, 1, min)
    plot(speed[plotwin[1]:plotwin[2],1], t = "l")
    
    speed[,1] <- spikereplace(as.numeric(min_of_not_spike == 0), speed[,1])
    
    plot(speed[plotwin[1]:plotwin[2],1] , t = "l")
  
  spike <- min(sum(as.numeric(min_of_not_spike == 0)), (maxcounter - counter))
  print(paste("spikes:", sum(as.numeric(min_of_not_spike == 0))))
  print(paste("counter at:", counter))
  counter <- counter + 1
  }
  
  datetime <- as.POSIXct(paste(dt[,"Year"], "-", dt[,"Month"], "-", dt[,"Day"], " ", dt[,"Hour"], ":", dt[,"Minute"], ":", dt[,"Second"], sep = ""))
  
  res <- cbind(datetime,data.frame(speed[,1]), dt$Speed, rowMeans(cbind( dt$SNR1, dt$SNR2, dt$SNR3), na.rm = TRUE))
  names(res) <- c("datetime", "speed", "speed_raw", "snr_mean")
  
  if(length(outpath) > 0){
  	write.csv(res, outpath, row.names = FALSE)
  }
  
  res
}

#'@name inellipse
#'@title Determine whether coordinates are enclosed by ellipse
#'@description Determine whether coordinates are enclose by ellipse
#'@param xdat numeric?
#'@param ydat numeric?
#'@param xellipse numeric?
#'@param yellipse numeric?
#'@export
#'@examples \dontrun{
#'xellipse<-x
#'yellipse<-y
#'xdat<-speed[,a]
#'ydat<-speed[,b]
#'}
inellipse <- function(xdat, ydat, xellipse, yellipse){
  is_yes <- matrix(0, length(xdat))

  cart2pol <- function(x, y){
    rho <- sqrt(((x^2) + (y^2)))
    theta <- atan2(y, x)
    c(theta, rho)
  }
  
  rangellipse <-t(apply(cbind(xellipse, yellipse), 1, function(x) cart2pol(x[1], x[2])))
  
  rellipse <- rangellipse[1:(nrow(rangellipse) - 1), 2]
  angellipse <- rangellipse[1:(nrow(rangellipse) - 1), 1]
  order_of_angellipse <- order(angellipse)
  angellipse <- angellipse[order_of_angellipse]
  rellipse <- rellipse[order_of_angellipse]
  
  angellipse <- c((angellipse[length(angellipse)]) - (2 * pi), angellipse)
  rellipse <- c(rellipse[length(rellipse)], rellipse)
  ravgellipse <- rellipse[1:(length(rellipse) - 1)] + (diff(rellipse) / 2)
  
  rangdat <- t(apply(cbind(xdat, ydat), 1, function(x) cart2pol(x[1], x[2])))
  angdat <- rangdat[,1]
  rdat <- rangdat[,2]
  
  n <- hist(angdat, breaks = angellipse, plot = FALSE)$counts
  angidx <- findInterval(angdat, angellipse)
  nogoo <- which(angidx == length(angellipse))
  angidx[nogoo] <- 1
  angmem <- unique(angidx)[order(unique(angidx))]
  
  natot <- length(angmem)
  for(na in 1:natot){
    goo <- which(angidx == angmem[na])
      is_yes[goo] <- as.numeric(rdat[goo] <= ravgellipse[angmem[na]])
  }
  return(is_yes)
}

#'@name spikereplace
#'@title Replace spikes
#'@description Replaces spikes
#'@param is_spike logical
#'@param sp numeric?
#'@import zoo
spikereplace <- function(is_spike, sp){
  res <- sp
  spikes <- which(diff(is_spike) > 0)
  spikee <- which(diff(is_spike) < 0)
  
  if(length(spikes) == 0){
    spikee <- NA
  }
  if(length(spikee) == 0){
    spikes <- NA
  }
  
  if(length(spikes) != 0 || length(spikee) != 0){
    if(spikee[1] < spikes[1]){
      spikee <- spikee[-1]
    }
    if(length(spikee) != 0){
      if(spikes[length(spikes)] > spikee[length(spikee)]){
        spikes <- spikes[-length(spikes)]
      }
    }else{
      spikes <- NA
    }
  }
  
  #replace spikes
  spiketot <- length(spikes)
  for(snum in 1:spiketot){
    #snum<-1
    res[(spikes[snum] + 1):spikee[snum]] <- NA
    res <- zoo::na.approx(res)
  }
  res
}
