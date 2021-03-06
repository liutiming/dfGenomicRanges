#' Select score allele from a data frame
#'
#' @export
select_score_allele <- function(variantTbl_proxy_selected) {
  expected_cols <- c("chr", "score","a1_major", "score_allele", "snp_proxy_selected", "af_proxy_selected", "maf_vcf_proxy_selected")
  variant_proxy_selected_col <- dplyr::select(variantTbl_proxy_selected, all_of(expected_cols))
  # NOTE: unnest list columns, instead of unlist individual cells
  variantTbl_proxy_selected_score_allele <- variant_proxy_selected_col  %>%
    # decide whether the original score allele is major, if so, use the proxy major allele as the score allele (effect allele)
    dplyr::mutate(score_allele_is_major = (a1_major == score_allele))  %>%
    # decide which is major and which minor
    # TODO should we remove this implicit assumption that snp_proxy_selected can be separated into four parts?
    tidyr::separate(snp_proxy_selected, into = stringr::str_c(c("chr", "pos", "ref", "alt"), "_proxy_selected"), remove = FALSE)  %>%
    dplyr::mutate(ref_is_major = (af_proxy_selected == maf_vcf_proxy_selected))  %>%
    # all ref_is_major on 20210429
    dplyr::mutate(major_proxy_selected = dplyr::if_else(ref_is_major, ref_proxy_selected, alt_proxy_selected), minor_proxy_selected = dplyr::if_else(!ref_is_major, ref_proxy_selected, alt_proxy_selected))  %>%
    dplyr::mutate(score_allele_proxy_selected = dplyr::if_else(score_allele_is_major, major_proxy_selected, minor_proxy_selected))  %>%
    dplyr::select(chr, snp_proxy_selected, score_allele_proxy_selected, score)  %>%
    dplyr::rename("score_allele" = "score_allele_proxy_selected", "topmed_id" = "snp_proxy_selected")

  return(variantTbl_proxy_selected_score_allele)
}
