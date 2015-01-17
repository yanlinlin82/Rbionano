# Check if input file gzipped
check_if_gzipped <- function(filename) {
    if (!file.exists(filename)) {
        filename <- paste0(filename, ".gz")
        stopifnot(file.exists(filename))
    }
    return(filename)
}

# Expand hit-enum in xmap data
expand_hit_enum <- function(s) {

	s1 <- lapply(strsplit(s, "[MDI]"), as.integer)
	s2 <- lapply(strsplit(s, "[0-9]+"), "[", -1)

	return(split(tbl_df(data.frame(
			type = factor(unlist(s2), c("M", "D", "I")),
			labelCount = unlist(s1))),
			rep(seq_along(s), sapply(s1, length))))
}
