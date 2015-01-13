#' Load .smap file
#'
#' Load structural variants in .smap file.
#'
load_smap <- function(filename, sort = TRUE, checkBedFiles = FALSE) {

	a <- tbl_df(read.table(filename, stringsAsFactors = FALSE))

	if (ncol(a) >= 13) {
		colnames(a) <- c("smapID", "qryID", "refID1", "refID2", "qryStart", "qryEnd",
				"refStart", "refEnd", "orientation", "confidence",
				"type", "xmapID1", "xmapID2")
	} else {
		colnames(a) <- c("smapID", "qryID", "refID1", "refID2", "qryStart", "qryEnd",
				"refStart", "refEnd", "confidence",
				"type", "xmapID1", "xmapID2")[1:ncol(a)]
	}

    if (checkBedFiles) {
        filename_bed_r <- gsub("\\.smap(\\.gz)?$", ".bed", filename) %>% check_if_gzipped
        filename_bed_q <- gsub("\\.smap(\\.gz)?$", "_query.bed", filename) %>% check_if_gzipped

        bed_r <- load_bed(filename_bed_r, sort = FALSE)
        bed_q <- load_bed(filename_bed_q, sort = FALSE)
        indel <- a %>% select(type == "insertion" | type == "deletion")

        stopifnot(all(indel$refID1 == indel$refID2))
        stopifnot(all(indel$refID1 == bed_r$chrom))
        stopifnot(all(indel$refStart == bed_r$chromStart))
        stopifnot(all(indel$refEnd == bed_r$chromEnd))
        stopifnot(all(bed_r$name == "insertion" | bed_r$name == "deletion"))
        stopifnot(all(bed_r$score == -1))
        stopifnot(all(bed_r$strand == "+"))
        stopifnot(all(bed_r$thickStart == 0))
        stopifnot(all(bed_r$thickEnd == 0))
        stopifnot(all(bed_r$itemRgb[bed_r$name == "insertion"] == "0,128,0"))
        stopifnot(all(bed_r$itemRgb[bed_r$name == "deletion"] == "255,0,0"))

        stopifnot(all(indel$qryID == bed_q$chrom))
        stopifnot(all(indel$qryStart == bed_q$chromStart))
        stopifnot(all(indel$qryEnd == bed_q$chromEnd))
        stopifnot(all(bed_q$name == "insertion" | bed_q$name == "deletion"))
        stopifnot(all(bed_q$score == -1))
        stopifnot(all(bed_q$strand == "+"))
        stopifnot(all(bed_q$thickStart == 0))
        stopifnot(all(bed_q$thickEnd == 0))
        stopifnot(all(bed_q$itemRgb[bed_q$name == "insertion"] == "0,128,0"))
        stopifnot(all(bed_q$itemRgb[bed_q$name == "deletion"] == "255,0,0"))
    }

	if (sort) {
		a <- a %>% arrange(refID1, refID2, refStart, refEnd)
	}
	return(a)
}
