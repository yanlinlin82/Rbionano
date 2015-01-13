#' Load .bnx file
#'
#' Load BioNano raw molecule data.
#'
#' @param file  .bnx file to load
#'
load_bnx <- function(file) {
    a <- list()

    # load from .bnx file
    lines <- scan(file, what = "character", sep = "\n", comment.char = "#", quiet = TRUE)
    stopifnot(length(lines) %% 4 == 0)
    dim(lines) <- c(4, length(lines) / 4)

    # check format
    stopifnot(all(substr(lines[1, ], 1, 2) == "0\t"))
    stopifnot(all(substr(lines[2, ], 1, 2) == "1\t"))
    stopifnot(all(substr(lines[3, ], 1, 5) == "QX11\t"))
    stopifnot(all(substr(lines[4, ], 1, 5) == "QX12\t"))

    # form data structure 'info'
    a$info <- unlist(strsplit(lines[1, ], "\\s+"))
    stopifnot(length(a$info) %% 11 == 0)
    dim(a$info) <- c(11, length(a$info) / 11)
    stopifnot(all(a$info[9,] == -1)) # column 'ScanDirection'
    a$info <- tbl_df(data.frame(
       id        = as.integer(a$info[2, ]),  # column 'MoleculeID'
       size      = as.numeric(a$info[3, ]),  # column 'Length'
       intensity = as.numeric(a$info[4, ]),  # column 'AvgIntensity'
       snr       = as.numeric(a$info[5, ]),  # column 'SNR'
       labels    = as.integer(a$info[6, ]),  # column 'NumberofLabels'
       originId  = as.integer(a$info[7, ]),  # column 'OriginalMoleculeId'
       scan      = as.integer(a$info[8, ]),  # column 'ScanNumber'
       chipId    = factor    (a$info[10,]),  # column 'ChipId'
       flowcell  = as.integer(a$info[11,]))) # column 'Flowcell'

    # form data structure 'labels'
    a$labels <- lines[2,] %>%
        strsplit("\t", fixed = TRUE) %>%
        lapply(function(x) as.numeric(x[-1])) %>%
        lapply(function(x) diff(c(0, x)))

    stopifnot(all(sapply(a$labels, sum) - a$info$size < .01))

    # form data structure 'snr'
    a$snr <- lines[3,] %>%
        strsplit("\t", fixed = TRUE) %>%
        lapply(function(x) as.numeric(x[-1]))

    # form data structure 'intensity'
    a$intensity <- lines[4,] %>%
        strsplit("\t", fixed = TRUE) %>%
        lapply(function(x) as.numeric(x[-1]))

    rm(lines)
    return(a)
}
