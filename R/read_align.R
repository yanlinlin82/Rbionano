#' Read .align file
#'
#' In BioNano data, alignment details are saved in .align files. This function
#' is used to read such data.
#'
#' @param file  .align file to read
#'
#' @return 'read_align()' returns a data frame
#'
read_align <- function(file) {

    s <- scan(file, what = "character", sep = "\n", comment.char = "#", quiet = TRUE)
    stopifnot(length(s) %% 3 == 0)
    dim(s) <- c(3, length(s) / 3)

    a <- NULL
    if (length(s) > 0) {
        a <- tbl_df(read.table(text = s[1, ], stringsAsFactors = FALSE))
        colnames(a) <- c("token", "alignID", "molID1", "molID2", "score", "centerOffset",
                         "overlap", "orientation", "log10Pvalue", "trueOffset",
                         "trueOverlapFraction", "fileID1", "fileID2")
        stopifnot(all(a$token == ">0"))
        a$token <- NULL

        a$align <- apply(s[-1, , drop = FALSE], 2, function(x) {
            x %>% strsplit("\t", fixed = TRUE) %>% sapply(as.integer) %>% as.data.frame %>% tbl_df
        })
    }
    return(a)
}
