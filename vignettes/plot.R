## ---- eval=FALSE, echo=FALSE, fig.align='center',fig.width=7, fig.height=4----
#  library(adv)
#  
#  flist <- list.files("../inst/extdata", include.dirs = TRUE, full.names = TRUE, "*.csv")[3:6]
#  #flist <- list.files("inst/extdata", include.dirs = TRUE, full.names = TRUE, "*.csv")[3:6]
#  
#  dt <- lapply(flist, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE))
#  
#  dtnames <- lapply(flist, function(x) rep(strsplit(strsplit(basename(x), "\\.")[[1]][1], "_")[[1]][4]
#   , nrow(read.csv(x))))
#  
#  dtnames <- do.call(c, dtnames)
#  dt <- do.call(rbind, dt)
#  dt$site <- dtnames
#  dt$datetime <- as.POSIXct(dt$datetime)
#  
#  library(ggplot2)
#  theme_opts <- list(ggplot2::theme(
#  	panel.grid.minor = ggplot2::element_blank(),
#  	panel.grid.major = ggplot2::element_blank(),
#  	panel.background = ggplot2::element_blank(),
#  	plot.background = ggplot2::element_rect(fill="white"),
#  	panel.border = ggplot2::element_blank(),
#  	axis.line = ggplot2::element_line(),
#  	axis.text.x = ggplot2::element_text(angle=90),
#  	axis.text.y = ggplot2::element_text(size=12),
#  	axis.ticks = ggplot2::element_line(),
#  	axis.title.x = ggplot2::element_blank(),
#  	axis.title.y = ggplot2::element_text(size=14),
#  	plot.title = ggplot2::element_text(size=22),
#  	legend.position = "none",
#  	strip.background = ggplot2::element_rect(fill = 'white')))
#  
#  gg <- ggplot(dt, aes(x = datetime, y = speed, colour = site))
#  gg <- gg + geom_line(alpha = 0.8)
#  gg <- gg + theme_opts + ggplot2::ylab("Speed") + viridis::scale_color_viridis(discrete = TRUE) + scale_x_datetime(date_breaks = "1 week", date_labels = "%m-%d")
#  gg + facet_grid(site~.)
#  
#  gg <- ggplot(dt, aes(x = datetime, y = speed_raw, colour = site))
#  gg <- gg + geom_line(alpha = 0.8)
#  gg <- gg + theme_opts + ggplot2::ylab("Speed Raw") + viridis::scale_color_viridis(discrete = TRUE) + scale_x_datetime(date_breaks = "1 week", date_labels = "%m-%d")
#  gg + facet_grid(site~.)
#  
#  gg <- ggplot(dt, aes(x = datetime, y = snr_mean, colour = site))
#  gg <- gg + geom_line(alpha = 0.8)
#  gg <- gg + theme_opts + ggplot2::ylab("3-probe average SNR") + viridis::scale_color_viridis(discrete = TRUE) + scale_x_datetime(date_breaks = "1 week", date_labels = "%m-%d")
#  gg + facet_grid(site~.)
#  

