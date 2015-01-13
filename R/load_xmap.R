#' Load .xmap file
#'
#' Load alignment info from .xmap file.
#'
load_xmap <- function(file, sort = FALSE, expandHitEnum = FALSE) {

	a <- tbl_df(read.table(file, stringsAsFactors = FALSE))

	colnames(a) <- c("xmapID", "qryID", "refID", "qryStart", "qryEnd",
			"refStart", "refEnd", "orientation", "confidence", "hitEnum")
	if (sort) {
		a <- a %>% arrange(qryID, qryStart, qryEnd)
	}

	stopifnot(all(grepl("^([0-9]+[MDI]+)+$", a$hitEnum)))
	if (expandHitEnum) {
		a$hitEnum <- expand_hit_enum(a$hitEnum)
	}

	return(a)
}
