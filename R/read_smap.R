#' Read .smap file
#'
#' Read structural variants in .smap file.
#'
read_smap <- function(file, sort = FALSE) {

	a <- tbl_df(read.table(file, stringsAsFactors = FALSE))

	if (ncol(a) >= 13) {
		colnames(a) <- c("smapID", "qryID", "refID1", "refID2", "qryStart", "qryEnd",
				"refStart", "refEnd", "orientation", "confidence",
				"type", "xmapID1", "xmapID2")
	} else {
		colnames(a) <- c("smapID", "qryID", "refID1", "refID2", "qryStart", "qryEnd",
				"refStart", "refEnd", "confidence",
				"type", "xmapID1", "xmapID2")[1:ncol(a)]
	}

	if (sort) {
		a <- a %>% arrange(refID1, refID2, refStart, refEnd)
	}
	return(a)
}
