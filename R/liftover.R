#' Obtain liftover chain
#' @export

get_liftOver_chain <- function() {
  ahub <- AnnotationHub::AnnotationHub()

  ahub.chain <- AnnotationHub::subset(ahub, rdataclass == "ChainFile" & species == "Homo sapiens")

  # AnnotationHub::query(ahub.chain, c("hg19", "hg38"))
  chain <- ahub.chain[ahub.chain$title == "hg19ToHg38.over.chain.gz"][[1]]
  return(chain)
}

#' lift over a dataframe
#' @export
df_liftOver <- function(variantTbl, chain) {
  variantTbl_GR <- variantTbl  %>%
    dplyr::mutate(pos = as.double(pos)) %>%
    GenomicRanges::makeGRangesFromDataFrame(start.field = "pos", end.field = "pos", keep.extra.columns = TRUE)

  variantTbl_GR_38 <- rtracklayer::liftOver(variantTbl_GR, chain)

  variantTbl_GR_38  %>%
    unlist() %>%
    tibble::as_tibble()  %>%
    dplyr::rename("chr" = "seqnames", "pos" = "start")  %>%
    dplyr::select(-c(end, width, strand))
}


