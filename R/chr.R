#' @export
mutate_chr  <- function(tbl) {
  if (check_chr_has_suffix(tbl)) {
    stop("chr is already in the chr# format")
  }
  result <- dplyr::mutate(tbl, chr = stringr::str_c("chr", chr))
  return(result)
}

#' @export
check_chr_has_suffix <- function(tbl) {
  chr_str <- tbl[1,"x"]
  if (stringr::str_detect(chr_str, "chr")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @export
chr_makeup_level <- function(tbl_to_split) {
  if(!check_chr_has_suffix(tbl_to_split)) {
    stop("tbl_to_split needs to have 'chr' suffix")
  }

  chr_model_tbl <- tibble::tibble(chr = stringr::str_c("chr", 1:22) %>% factor(., levels = (.)))
  split_list <- left_join(chr_model_tbl, tbl_to_split)  %>% group_split(chr)
  return(split_list)
}
