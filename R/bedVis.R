#' @title bedVis
#' @name bedVis
#' @author JunZhang
#' @description visualize peaks(bed files).
#'
#' @param bdFile the bed file path, default(NULL).
#' @param chr the chromesome of peak, default(NULL).
#' @param region.min the peak start coordinate, default(NULL).
#' @param region.max the peak end coordinate, default(NULL).
#' @param track.width track width, default(0.1).
#' @param collapse whether collapse the track, default(FALSE).
#' @param fill track fill colors, default(NULL).
#' @param show.legend whether show fill color legend, default(TRUE).
#' @param add.label whether add peak name, default(FALSE).
#' @param label.column the peak name column name, default(NULL).
#' @param label.vjsut the peak label vjust, default(0.1).
#'
#' @return a ggplot object.
#'
#' @export

globalVariables(c("sn","ymin"))

bedVis <- function(bdFile = NULL,
                   chr = NULL,
                   region.min = NULL,
                   region.max = NULL,
                   track.width = 0.1,
                   collapse = FALSE,
                   fill = NULL,
                   show.legend = TRUE,
                   add.label = FALSE,
                   label.column = NULL,
                   label.vjsut = 0.1){
  # loop read bed
  purrr::map_df(1:length(bdFile),function(x){
    tmp <- rtracklayer::import.bed(bdFile[x]) %>%
      data.frame()

    # add name
    tmp$fileName <- strsplit(bdFile[x],split = ".bed") %>% unlist()

    # add sn
    tmp$sn <- x
    return(tmp)
  }) -> bdData

  # filter region data
  if(!is.null(region.min) & !is.null(region.max)){
    regeion.bd <- bdData %>%
      dplyr::filter(seqnames == chr) %>%
      dplyr::filter(start >= region.min & end <= region.max)
  }else{
    regeion.bd <- bdData %>%
      dplyr::filter(seqnames == chr)
  }

  # whether collapse track
  if(collapse == TRUE){
    regeion.bd$sn <- 1
  }

  # add y region
  regeion.bd <- regeion.bd %>%
    dplyr::mutate(ymin = sn - track.width,
                  ymax = sn + track.width)

  # plot
  bed <-
    ggplot2::ggplot(regeion.bd) +
    ggplot2::geom_rect(ggplot2::aes_string(xmin = 'start',xmax = 'end',
                                           ymin = 'ymin',ymax = 'ymax',
                                           fill = 'fileName'),
                       show.legend = show.legend) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = ''))

  if(!is.null(fill)){
    bed.col <- bed +
      ggplot2::scale_fill_manual(values = fill)
  }else{
    bed.col <- bed
  }

  # ========================================
  # add peak label
  if(add.label == TRUE){
    bed.label <- bed.col +
      ggplot2::geom_text(ggplot2::aes(x = (start + end)/2,y = ymin - label.vjsut,
                                      label = get(label.column)),
                         check_overlap = TRUE)
  }else{
    bed.label <- bed.col
  }
  return(bed.label)
}
