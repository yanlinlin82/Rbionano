#' Load .cmap file
#'
#' Load BioNano consensus map.
#'
load_cmap <- function(file, sort = FALSE) {

	a <- tbl_df(read.table(file, stringsAsFactors = FALSE))

	colnames(a) <- c("cmapID", "contigLength", "numSites", "siteID",
			"labelChannel", "position", "stddev", "coverage",
			"occurance", "gmeanSNR", "lnSNRsd", "SNRcount")[1:ncol(a)]
	if (sort) {
		a <- a %>% arrange(cmapID, position)
	}

	return(a)
}
