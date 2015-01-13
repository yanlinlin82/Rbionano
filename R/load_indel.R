#' Load indels
#'
#' Load indels from .smap file.
#'
load_indel <- function(file, sort = FALSE, checkBedFiles = FALSE) {

    a <- load_smap(file, sort, checkBedFiles) %>%
        filter(type == "insertion" | type == "deletion") %>%
        mutate(retStart = round(refStart),
               refEnd = round(refEnd),
               qryStart = round(qryStart),
               qryEnd = round(qryEnd),
               refSize = abs(refEnd - refStart),
               qrySize = abs(qryEnd - qryStart),
               size = abs(refSize - qrySize),
               type = ifelse(type == "insertion", "INS", "DEL"))

    stopifnot(a$refID1 == a$refID2)
    colnames(a)[which(colnames(a) == "refID1")] <- "refID"
    a$refID2 <- NULL

    return(a)
}
