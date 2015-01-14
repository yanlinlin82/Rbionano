#' Load indels
#'
#' Load indels from .smap file.
#'
load_indel <- function(file, sort = FALSE, checkBedFiles = FALSE) {

    a <- read_smap(file, sort = FALSE) %>%
        filter(type == "insertion" | type == "deletion") %>%
        mutate(retStart = round(refStart),
               refEnd = round(refEnd),
               qryStart = round(qryStart),
               qryEnd = round(qryEnd),
               refSize = abs(refEnd - refStart),
               qrySize = abs(qryEnd - qryStart),
               size = abs(refSize - qrySize),
               type = ifelse(type == "insertion", "INS", "DEL"))

    if (checkBedFiles) {
        ref_bed_file <- gsub("\\.smap(\\.gz)?$", ".bed", file) %>% check_if_gzipped
        qry_bed_file <- gsub("\\.smap(\\.gz)?$", "_query.bed", file) %>% check_if_gzipped

        bed_r <- read_bed(ref_bed_file, sort = FALSE)
        bed_q <- read_bed(qry_bed_file, sort = FALSE)
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

    stopifnot(a$refID1 == a$refID2)
    colnames(a)[which(colnames(a) == "refID1")] <- "refID"
    a$refID2 <- NULL

	if (sort) {
		a <- a %>% arrange(refID, refStart, refEnd)
	}
    return(a)
}
