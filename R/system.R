#' write a table contains a chromosome column (assume unique) and use the chr as a part of file name
#' @export
write_chr_tbl <- function(chr_tbl, prefix, suffix, delim, col_names) {
  chr <- dplyr::distinct(chr_tbl, chr) %>% dplyr::pull() %>% as.character()
  if (length(chr) > 1) {
    stop("more than one chromosome in the column `chr`")
  }
  file_name <- glue::glue("{prefix}{chr}{suffix}")
  vroom::vroom_write(chr_tbl, path = file_name, delim = delim, col_names = col_names)
  return(file_name)
}
