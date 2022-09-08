#' @title loadBigWig
#' @name loadBigWig
#' @author JunZhang
#' @description read bigwig files.
#'
#' @param bwFile the path of bigwig files, default(NULL). bigwig files should end with ".bw" or ".bigwig" and directory should not be named "bw" or"bigwig".
#'
#' @return a data.frame
#' @export

loadBigWig <- function(bwFile = NULL){
  # loop read bed
  purrr::map_df(1:length(bwFile),function(x){
    tmp <- rtracklayer::import.bw(bwFile[x]) %>%
      data.frame()

    # sampe name
    spt <- strsplit(bwFile[x],split = "/|.bw|.bigwig") %>% unlist()
    sname <- spt[length(spt)]
    # add name
    tmp$fileName <- sname

    return(tmp)
  }) -> bWData
  return(bWData)
}
