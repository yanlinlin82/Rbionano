#' Read inversions
#'
#' Read inversions from .inv file.
#'
read_inv <- function(file) {

    a <- tbl_df(read.table(file, skip = 1, stringsAsFactors = FALSE))
    colnames(a) <- c("ref",
                     "iContig", "iMatchId1", "iMatchId2", "iRefInvBkptStart", "iRefInvBkptEnd",
                     "jContig", "jMatchId1", "jMatchId2", "jRefInvBkptStart", "jRefInvBkptEnd",
                     "validPair")
    return(a)
}
