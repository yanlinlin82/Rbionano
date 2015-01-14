#' Read .bed file
#'
#' BioNano structural variants are usually exported as BED files for
#' visualization in IrysView application.
#'
#' @param file  .bed file to read
#' @param sort  logical. If "TRUE", the results will be sorted by
#'              genomic coordinates, otherwise to keep order as in file
#'
#' @return 'read_bed()' returns a data frame
#'
#' @seealso read_smap
#'
read_bed <- function(filename, sort = FALSE) {

	a <- tbl_df(read.table(filename, stringsAsFactors = FALSE))

	colnames(a) <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand",
			"thickStart", "thickEnd", "itemRgb",
			"blockCount", "blockSizes", "blockStarts")[1:ncol(a)]
	if (sort) {
		a <- a %>% arrange(chrom, chromStart, chromEnd)
	}
	
	return(a)
}
