test_that("select_score_allele detects maf af mismatch", {
  result <- data.frame(
    "chr" = c("chr1", "chr1","chr1","chr1","chr1"),
    "score" = c(1,1,1,1,1),
    "a1_major" = c("C", "C","C","A","C"),
    "snp_proxy_selected" = c("chr6:32408740:C:T", "chr6:32408740:C:T","chr6:32408740:C:T","chr6:32408740:C:T","chr6:32408740:T:C"),
    "score_allele" = c("C","C","A","C","C"),
    "af_proxy_selected" = c(0.1,0.9,0.1,0.1,0.1),
    "maf_vcf_proxy_selected" = c(0.1,0.1,0.1,0.1,0.1)
  ) %>%
    select_score_allele()


  expect_equal(result,
  data.frame(
    stringsAsFactors = FALSE,
    chr = c("chr1", "chr1", "chr1", "chr1", "chr1"),
    topmed_id = c(
      "chr6:32408740:C:T",
      "chr6:32408740:C:T",
      "chr6:32408740:C:T",
      "chr6:32408740:C:T",
      "chr6:32408740:T:C"
    ),
    score_allele = c("C", "T", "T", "T", "T"),
    score = c(1, 1, 1, 1, 1)
  ))
})
