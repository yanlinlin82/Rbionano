#' Load .bed file
#'
#' BioNano structural variants are usually exported as BED files for
#' visualization in IrysView application.
#'
#' @param filename filename to load
#' @param sort sort data by genomic coordinates
#'
#' @return a 'tbl' data frame
#'
#' @seealso load_smap
#'
load_bed <- function(filename, sort = TRUE) {

	a <- tbl_df(read.table(filename, stringsAsFactors = FALSE))

	colnames(a) <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand",
			"thickStart", "thickEnd", "itemRgb",
			"blockCount", "blockSizes", "blockStarts")[1:ncol(a)]
	if (sort) {
		a <- a %>% arrange(chrom, chromStart, chromEnd)
	}
	
	return(a)
}
