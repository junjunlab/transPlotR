#' @title loadBigWig
#' @name loadBigWig
#' @author JunZhang
#' @description read bigwig files.
#'
#' @param bwFile the path of bigwig files, default(NULL).
#'
#' @return a data.frame
#' @export

loadBigWig <- function(bwFile = NULL){
  # loop read bed
  purrr::map_df(1:length(bwFile),function(x){
    tmp <- rtracklayer::import.bw(bwFile[x]) %>%
      data.frame()

    # add name
    tmp$fileName <- strsplit(bwFile[x],split = ".bw|.bigwig") %>% unlist()

    return(tmp)
  }) -> bWData
  return(bWData)
}
