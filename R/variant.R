str_complement <- function(string) {
  string %>% Biostrings::DNAStringSet() %>% Biostrings::complement()  %>% as.character()
}

df_match_variant <- function(tbl1, tbl2, suffix) {
  tbl1_rm <- remove_palindromic_snp(tbl1)
  tbl2_rm <- remove_palindromic_snp(tbl2)

  location <-
    inner_join(tbl1_rm,tbl2_rm, by = c("chr", "pos"), suffix = suffix)
  make_match_variant(location, major_indian, minor_indian, major_elghRef, minor_elghRef)
}

remove_palindromic_snp <- function(tbl){
  tbl %>%
    mutate(
      palindromic =
        if_else(
          (minor == "A" & major == "T") |
            (minor == "T" & major == "A") |
            (minor == "C" & major == "G") |
            (minor == "G" & major == "C"), TRUE, FALSE

        )
    )  %>%
    filter(!palindromic) %>%
    select(-palindromic)
}

mutate_major_minor <- function(tbl, major_column, minor_column) {
  mutate(tbl,
         major_minor = str_c({{major_column}}, {{minor_column}}),
         major_minor_switch = str_c({{minor_column}}, {{major_column}}),
         major_flip = str_complement({{major_column}}),
         minor_flip = str_complement({{minor_column}}),
         major_minor_flip = str_c(major_flip, minor_flip),
         major_minor_flip_switch = str_c(minor_flip, major_flip)
  )
}


make_match_variant <- function(location_tbl, major_test, minor_test, major_keep, minor_keep) {
  original_cols <- colnames(location_tbl)
  variantTbl_nonHLABim_location_variations <- mutate_major_minor(location_tbl, {{major_keep}}, {{minor_keep}})
  a <- mutate(variantTbl_nonHLABim_location_variations, join_major_minor = str_c({{major_test}}, {{minor_test}}))
  test_tbl <- mutate(variantTbl_nonHLABim_location_variations, join_major_minor = str_c({{major_test}}, {{minor_test}}))
  mutate(test_tbl,
         match = (
           join_major_minor == major_minor |
             join_major_minor == major_minor_switch |
             join_major_minor == major_minor_flip |
             join_major_minor == major_minor_flip_switch
         )
  ) %>%
    filter(match)  %>%
    mutate(match = str_c({{major_keep}}, ":", {{minor_keep}})) %>%
    select(all_of(original_cols), match)
}

 datapasta::vector_paste(plink_bim_col_names)

