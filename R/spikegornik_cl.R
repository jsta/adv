#'@name spikegornik_cl
#'@title Command-line interface to the spikegornik function
#'@description Command-line interface to the spikegornik function
#'@param fpath file.path to ADV output where the column of interest is named "Speed"
#'@param outpath file.path to save results. optional.
spikegornik_cl <- function(fpath, outpath){
	adv::spikegornik(fpath, outpath)
}
#args <- commandArgs(trailingOnly = TRUE)
#spikegornik_cl(args[1], args[2])