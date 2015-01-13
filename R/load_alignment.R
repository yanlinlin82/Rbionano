#' Load alignments between optical maps
#'
#' BioNano optical maps can be aligned to each other. Normally, the alignment
#' results are saved into three files, named as "xxx.xmap", "xxx_r.xmap" and
#' "xxx_q.xmap".
#'
#' @param filename (.xmap) filename to load
#' @param sort sort data by genomic coordinates
#' @param filename_cmap_r (_r.cmap) filename to load. It could be inferred from filename
#' @param filename_cmap_q (_q.cmap) filename to load. It could be inferred from filename
#' @param expandHitEnum whether to expand hitEnum field
#'
#' @return 'load_alignment()' returns a list, which contains three data frames,
#'     named as 'xmap', 'ref' and 'qry'.
#'
#' @seealso load_xmap load_cmap
#'
load_alignment <- function(filename, sort = TRUE, filename_cmap_r = NULL, filename_cmap_q = NULL, expandHitEnum = FALSE) {

    if (is.null(filename_cmap_r) || is.null(filename_cmap_q)) {
        prefix <- gsub("\\.xmap(\\.gz)?$", "", filename)
        if (is.null(filename_cmap_r)) {
            filename_cmap_r <- paste0(prefix, "_r.cmap") %>% check_if_gzipped
        }
        if (is.null(filename_cmap_q)) {
            filename_cmap_q <- paste0(prefix, "_q.cmap") %>% check_if_gzipped
        }
    }

    return(list(xmap = load_xmap(filename, sort, expandHitEnum),
                ref = load_cmap(filename_cmap_r, sort),
                qry = load_cmap(filename_cmap_q, sort)))
}
