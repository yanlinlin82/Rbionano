#' Load alignments between optical maps
#'
#' BioNano optical maps can be aligned to each other. Normally, the alignment
#' results are saved into three files, named as "xxx.xmap", "xxx_r.xmap" and
#' "xxx_q.xmap".
#'
#' @param file           xmap ('xxx.xmap') file to load
#' @param ref_file       reference cmap ('xxx_r.cmap') file to load
#' @param qry_file       query cmap ('xxx_q.cmap') file to load
#' @param sort           logical. If "TRUE", the results will be sorted by
#'                       genomic coordinates, otherwise to keep order as in file
#' @param expandHitEnum  logical. If "TRUE", the 'hitEnum' field will be parsed
#'
#' @return 'load_alignment()' returns a list, which contains three data frames,
#'     named as 'xmap', 'ref' and 'qry'.
#'
#' @seealso load_xmap load_cmap
#'
load_alignment <- function(file, ref_file, qry_file,
                           sort = FALSE, expandHitEnum = FALSE)
{
    if (is.null(ref_file) || is.null(qry_file)) {
        prefix <- gsub("\\.xmap(\\.gz)?$", "", file)
        if (is.null(ref_file)) {
            ref_file <- paste0(prefix, "_r.cmap") %>% check_if_gzipped
        }
        if (is.null(qry_file)) {
            qry_file <- paste0(prefix, "_q.cmap") %>% check_if_gzipped
        }
    }

    return(list(xmap = load_xmap(file, sort, expandHitEnum),
                ref = load_cmap(ref_file, sort),
                qry = load_cmap(qry_file, sort)))
}
