#' Load inversions
#'
#' Load inversions from .inv file.
#'
load_inv <- function(filename) {

    a <- tbl_df(read.table(filename, skip = 1, stringsAsFactors = FALSE))
    colnames(a) <- c("ref",
                     "iContig", "iMatchId1", "iMatchId2", "iRefInvBkptStart", "iRefInvBkptEnd",
                     "jContig", "jMatchId1", "jMatchId2", "jRefInvBkptStart", "jRefInvBkptEnd",
                     "validPair")
    return(a)
}
