#' @title trancriptVis
#' @name trancriptVis
#' @author Junjun Lao
#' @description This package is to visualize gene diffrent isoforms.
#' @param gtfFile GTF file.
#' @param gene Target gene to plot.
#' @param myTranscript Specify which transcripts to plot use transcipt id.
#' @param Chr Chromosome number.
#' @param posStart Region start position on genome.
#' @param posEnd Region end position on genome.
#' @param collapse Whether to collapse multiple transcripts into one, default(FALSE).
#' @param exonWidth Exon width to plot, default(0.3).
#' @param relTextDist Transcripts name or gene name relative to exon, default(0.3).
#' @param intronSize Intron line size, default(0.5).
#' @param arrowBreak How many gap distance to draw arrows, the smaller the more arrows, default(0.15).
#' @param exonColorByTrans Whether color group by transcripts, default(FALSE).
#' @param exonFill Exon fill color, default('#333399').
#' @param circle Whether make plot into a circle plot, default(FALSE).
#' @param cicStart Circle plot start position, default(pi).
#' @param circSegCol Circle sgement color, default('#333399').
#' @param text_only When circle plot labeled by gene name, whether remove the line connected with gene name, default(FALSE).
#' @param ylimLow The Y axis lower limitation of Circle plot, default(-10).
#' @param openAngle The gap of the circle plot, default(0.5).
#' @param arrowCol Normal arrow color, default('#333399').
#' @param arrowAngle Normal arrow angle, default(30).
#' @param arrowLength Normal arrow length, default(0.1).
#' @param arrowType Normal arrow type, default('open').
#' @param addNormalArrow Whether add normal arrow on plot, default(TRUE).
#' @param newStyleArrow Whether add new style arrow on plot, default(FALSE).
#' @param speArrowRelPos The relative position to the transcript on horizontal direction of new style arrow, default(0).
#' @param speArrowRelLen The relative length to the transcript length of new style arrow, default(0.05).
#' @param speArrowStart The new style arrow start position on the vertical direction, default(-0.15).
#' @param speArrowRelHigh The relative height of new style arrow to the vertical length, default(2).
#' @param speArrowLineSize The new style arrow line size, default(0.5).
#' @param speArrowCol The new style arrow line color, default('black').
#' @param speArrowAngle The new style arrow angle, default(30).
#' @param speArrowLen The new style arrow length, default(0.1).
#' @param speArrowType The new style arrow type, default('closed').
#' @param textLabel The text label aesthetic mappings, default('transcript_id').
#' @param textLabelSize The text label size, default(5).
#' @param textLabelColor The text label color, default('black').
#' @param base_size Theme basesize, default(14).
#' @param marginX Plot left and right margins, default(0.2).
#' @param marginY Plot top and bottomn margins, default(0.2).
#' @param aspect.ratio Plot ratio, default(NULL).
#'
#' @import tidyverse
#'
#' @return ggplot object.
#'
#' @export
#' @examples
#' ##############################################################
#' # test function
#'
#' ########################################################
#' # load data
#' data(gtf)
#'
#' # non-coding gene
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Xist')
#'
#' # coding gene
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Nanog')
#'
#' # change fill color
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Nanog',
#'              exonFill = '#CCFF00')
#'
#' # change inrton line size
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Nanog',
#'              intronSize = 1)
#'
#' # change label size,color and position
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Nanog',
#'              textLabelSize = 4,
#'              textLabelColor = 'red',
#'              relTextDist = 0)
#'
#' # aes by gene name
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Nanog',
#'              textLabel = 'gene_name')
#'
#' # color aes by transcript
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Tpx2',
#'              exonColorByTrans = TRUE)
#'
#' # change arrow color and type
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Nanog',
#'              arrowCol = 'orange',
#'              arrowType = 'closed')
#'
#' # no intron gene and add arrow color
#' # change arrow color and type
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Jun',
#'              textLabel = 'gene_name',
#'              arrowCol = 'white',
#'              arrowType = 'closed') +
#'   theme_void()
#'
#' # add arrow breaks
#' trancriptVis(gtfFile = gtf,
#'              gene = 'Nanog',
#'              arrowCol = 'orange',
#'              arrowType = 'closed',
#'              arrowBreak = 0.1)
#'
#' # draw specific transcript
#' p1 <- trancriptVis(gtfFile = gtf,
#'                    gene = 'Commd7')
#'
#' p2 <- trancriptVis(gtfFile = gtf,
#'                    gene = 'Commd7',
#'                    myTranscript = c('ENSMUST00000071852','ENSMUST00000109782'))
#'
#' # combine
#' cowplot::plot_grid(p1,p2,ncol = 2,align = 'hv')

# global variables
globalVariables(c('end', 'gene_biotype', 'gene_id', 'gene_name','seqnames',
                  'start', 'strand','transcript_id', 'type', 'vl_x1' ,'width', 'yPos'))

# use_package("tidyverse", type = "depends")

# define function
trancriptVis <- function(gtfFile = NULL,
                         gene = NULL,
                         myTranscript = NULL,
                         Chr = NULL,
                         posStart = NULL,
                         posEnd = NULL,
                         collapse = FALSE,
                         exonWidth = 0.3,
                         relTextDist = 0.3,
                         intronSize = 0.5,
                         arrowBreak = 0.15,
                         exonColorByTrans = FALSE,
                         exonFill = '#333399',
                         circle = FALSE,
                         cicStart = pi,
                         circSegCol = '#333399',
                         text_only = FALSE,
                         ylimLow = -10,
                         openAngle = 0.5,
                         arrowCol = '#333399',
                         arrowAngle = 30,
                         arrowLength = 0.1,
                         arrowType = 'open',
                         addNormalArrow = TRUE,
                         newStyleArrow = FALSE,
                         speArrowRelPos = 0,
                         speArrowRelLen = 0.05,
                         speArrowStart = -0.15,
                         speArrowRelHigh = 2,
                         speArrowLineSize = 0.5,
                         speArrowCol = 'black',
                         speArrowAngle = 30,
                         speArrowLen = 0.1,
                         speArrowType = "closed",
                         textLabel = 'transcript_id',
                         textLabelSize = 5,
                         textLabelColor = 'black',
                         base_size = 14,
                         marginX = 0.2,
                         marginY = 0.2,
                         aspect.ratio = NULL){
  ##############################################################################
  # test whether with a given specific gene or region
  if(is.null(gene)){
    # filter gene by region
    myGene <- gtfFile %>%
      dplyr::filter(seqnames == Chr & start >= posStart & end <= posEnd) %>%
      dplyr::filter(type != 'gene') %>%
      dplyr::select(seqnames,start,end,width,strand,type,gene_id,gene_name,gene_biotype,transcript_id)
  }else{
    # filter gene by gene name
    myGene <- gtfFile %>%
      dplyr::filter(gene_name %in% gene) %>%
      dplyr::filter(type != 'gene') %>%
      dplyr::select(seqnames,start,end,width,strand,type,gene_id,gene_name,gene_biotype,transcript_id)
  }

  ##############################################################################
  # whether plot specific transcript
  if(is.null(myTranscript)){
    myData <- myGene
  }else{
    myData <- myGene %>%
      dplyr::filter(transcript_id %in% myTranscript)
  }

  # get gene id
  gid <- unique(myData$gene_id)

  ##############################################################################
  # add y axis position
  if(collapse == FALSE){
    # expand gene
    purrr::map_df(1:length(gid),function(x){
      tmp <- myData %>%
        dplyr::filter(gene_id == gid[x])
      # loop for tid
      trans_tmp <- tmp %>% dplyr::filter(type == 'transcript') %>%
        dplyr::arrange(width)
      tid <- unique(trans_tmp$transcript_id)

      # assign y position
      purrr::map_df(1:length(tid),function(x){
        tmp1 <- myData %>%
          dplyr::filter(transcript_id == tid[x]) %>%
          dplyr::mutate(yPos = x)
        tmp1$ymin <- ifelse(tmp1$type == 'CDS',
                            tmp1$yPos - exonWidth/2,
                            tmp1$yPos - exonWidth/6)
        tmp1$ymax <- ifelse(tmp1$type == 'CDS',
                            tmp1$yPos + exonWidth/2,
                            tmp1$yPos + exonWidth/6)
        return(tmp1)
      }) -> exon_ypos

      return(exon_ypos)
    }) -> mul_exon_ypos
  }else{
    # collapse gene
    mul_exon_ypos <- myData %>% dplyr::mutate(yPos = 1)
    mul_exon_ypos$ymin <- ifelse(mul_exon_ypos$type == 'CDS',
                                 mul_exon_ypos$yPos - exonWidth/2,
                                 mul_exon_ypos$yPos - exonWidth/6)
    mul_exon_ypos$ymax <- ifelse(mul_exon_ypos$type == 'CDS',
                                 mul_exon_ypos$yPos + exonWidth/2,
                                 mul_exon_ypos$yPos + exonWidth/6)
  }

  ##############################################################################
  # extarct data
  exon <- mul_exon_ypos %>% dplyr::filter(type != 'transcript')
  trans <- mul_exon_ypos %>% dplyr::filter(type == 'transcript')

  # add text x/y pos
  trans$textX <- ifelse(trans$strand == '+',trans$start + trans$width/2,
                        trans$end - trans$width/2)
  trans$textY <- trans$yPos + relTextDist

  ##############################################################################
  # whether add specific arrow
  if(newStyleArrow == FALSE){
    arrow_trans <- trans
  }else{
    # add special arrow
    purrr::map_df(trans$transcript_id,function(x){
      tmp <- trans %>% dplyr::filter(transcript_id == x)
      # test strand
      if(tmp$strand == '+'){
        tmp <- tmp %>% dplyr::mutate(vl_x1 = start + speArrowRelPos*width,
                                     vl_x2 = vl_x1 + speArrowRelLen*width,
                                     vl_y1 = yPos + speArrowStart,
                                     vl_y2 = yPos + speArrowStart*speArrowRelHigh)
      }else if(tmp$strand == '-'){
        tmp <- tmp %>% dplyr::mutate(vl_x1 = end - speArrowRelPos*width,
                                     vl_x2 = vl_x1 - speArrowRelLen*width,
                                     vl_y1 = yPos + speArrowStart,
                                     vl_y2 = yPos + speArrowStart*speArrowRelHigh)
      }
      return(tmp)
    }) -> arrow_trans
  }

  # strand control arrow direction
  arrow_trans$ad <- ifelse(arrow_trans$strand == '+','last','first')

  # arrow breaks
  arrow_seq = c(0.05,seq(0,0.95,arrowBreak)[-1],1)

  ##############################################################################
  # first layer
  if(exonColorByTrans == FALSE){
    p1 <- ggplot2::ggplot(exon) +
      ggplot2::geom_rect(ggplot2::aes_(xmin = ~start,xmax = ~end,
                                       ymin = ~ymin,ymax = ~ymax),
                         fill = exonFill)
  }else{
    p1 <- ggplot2::ggplot(exon) +
      ggplot2::geom_rect(ggplot2::aes_(xmin = ~start,xmax = ~end,
                                       ymin = ~ymin,ymax = ~ymax,
                                       fill = ~transcript_id),
                         show.legend = F)
  }

  ##############################################################################
  if(newStyleArrow == FALSE){
    p2 <- p1
  }else{
    p2 <- p1 +
      # add vertical line
      ggplot2::geom_segment(data = arrow_trans,
                            ggplot2::aes_string(x = "vl_x1",xend = "vl_x1",
                                                y = "vl_y1",yend = "vl_y2"),
                            color = speArrowCol,
                            size = speArrowLineSize) +
      # add horizotal line and arrow
      ggplot2::geom_segment(data = arrow_trans,
                            ggplot2::aes_string(x = "vl_x1",xend = "vl_x2",
                                                y = "vl_y2",yend = "vl_y2"),
                            color = speArrowCol,
                            size = speArrowLineSize,
                            arrow = ggplot2::arrow(angle = speArrowAngle,
                                                   length = ggplot2::unit(speArrowLen, "inches"),
                                                   ends = "last",
                                                   type = speArrowType))
  }


  ##############################################################################
  # whether draw ploar plot
  if(circle == FALSE){
    # add arrow and segment line with geom_arrowsegment
    if(addNormalArrow == TRUE){
      p3 <- p2 +
        ggarchery::geom_arrowsegment(data = arrow_trans,
                                     ggplot2::aes_(x = ~start,xend = ~end,
                                                   y = ~yPos,yend = ~yPos),
                                     color = arrowCol,
                                     size = intronSize,
                                     arrow_positions = arrow_seq,
                                     arrow_fills = rep(arrowCol,length(arrow_seq)),
                                     arrows = list(ggplot2::arrow(angle = arrowAngle,
                                                                  length = ggplot2::unit(arrowLength, "inches"),
                                                                  ends = arrow_trans$ad,
                                                                  type = arrowType)))
    }else{
      p3 <- p2 +
        ggarchery::geom_arrowsegment(data = arrow_trans,
                                     ggplot2::aes_(x = ~start,xend = ~end,
                                                   y = ~yPos,yend = ~yPos),
                                     color = arrowCol,
                                     size = intronSize)
    }
  }else{
    # add arrow and segment line geom_segment
    if(addNormalArrow == TRUE){
      p3 <- p2 +
        ggplot2::geom_segment(data = arrow_trans,
                              ggplot2::aes_(x = ~start,xend = ~end,
                                            y = ~yPos,yend = ~yPos),
                              color = circSegCol,
                              size = intronSize,
                              arrow = ggplot2::arrow(angle = arrowAngle,
                                                     length = ggplot2::unit(arrowLength, "inches"),
                                                     ends = arrow_trans$ad,
                                                     type = arrowType))
    }else{
      p3 <- p2 +
        ggplot2::geom_segment(data = arrow_trans,
                              ggplot2::aes_(x = ~start,xend = ~end,
                                            y = ~yPos,yend = ~yPos),
                              color = circSegCol,
                              size = intronSize)
    }
  }

  ##############################################################################
  # add text label
  if(circle == FALSE){
    # geom_text
    p4 <- p3 +
      ggplot2::geom_text(data = arrow_trans,
                         ggplot2::aes_string(x = 'textX',y = 'textY',
                                             label = textLabel),
                         size = textLabelSize,
                         color = textLabelColor,
                         check_overlap = T) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(t = marginY,r = marginX,b = marginY ,l = marginX)) +
      ggplot2::xlab('Positions on genome')
  }else{
    # geom_textpath
    p4 <- p3 +
      geomtextpath::geom_textpath(data = arrow_trans,
                                  ggplot2::aes_string(x = "textX",y = "textY",label = textLabel),
                                  size = textLabelSize,
                                  color = textLabelColor,
                                  text_only = text_only) +
      ggplot2::coord_polar(theta = 'x',start = cicStart) +
      ggplot2::scale_y_continuous(limits = c(ylimLow,nrow(arrow_trans) + 1)) +
      ggplot2::scale_x_continuous(expand = c(0,openAngle*max(trans$width))) +
      ggplot2::theme_void()
  }

  ##############################################################################
  # add ratio
  if(is.null(aspect.ratio)){
    p5 <- p4
  }else{
    p5 <- p4 +
      ggplot2::theme(aspect.ratio = aspect.ratio)
  }
  return(p5)
}

###############################
#' This is a test data for this package
#' test data describtion
#'
#' @name gtf
#' @docType data
#' @author Junjun Lao
"gtf"
