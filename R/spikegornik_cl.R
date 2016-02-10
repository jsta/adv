#'@name spikegornik_cl
#'@title Command-line interface to the spikegornik function
spikegornik_cl <- function(fpath, outpath){
	adv::spikegornik(fpath, outpath)
}
#args <- commandArgs(trailingOnly = TRUE)
#spikegornik_cl(args[1], args[2])