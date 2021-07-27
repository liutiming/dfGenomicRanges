#' @export
tibble_makeGenomicRanges  <- function(tbl) {
  tbl  %>%
    GenomicRanges::makeGRangesFromDataFrame(start.field = "pos", end.field = "pos", keep.extra.columns = TRUE)
}

#' @export
tibble_subsetByOverlaps <- function(range_1, range_2) {
  IRanges::subsetByOverlaps(range_1, range_2)  %>%
    tibble::as_tibble()  %>%
    dplyr::rename("chr" = "seqnames", "pos" = "start")  %>%
    dplyr::select(chr, pos, end)
}

#' @export
mutate_chr  <- function(tbl) {
  result <- dplyr::mutate(tbl, chr = stringr::str_c("chr", chr))
  return(result)
}



