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
#' @param exonColorBy Whether color group by "transcript_id" or "gene_name", default(NULL).
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
#' @param absSpecArrowLen Whether make new style arrow length to be relative to each transcript length or absolute length to the longest transcript, default(FALSE).
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
#' @param facetByGene Whether facet by gene to plot, this useful for your genes which are far away from each other or not located on the same chromosome, default(FALSE).
#' @param ncolGene The column numbers to plot, default(NULL).
#' @param scales Facet plot scales, same as "facet_wrap" function, default('free').
#' @param strip.position Facet plot strip.position, same as "facet_wrap" function, default('top').
#' @param forcePosRel Whether force the genome coordinate to relative position to transcript start/end position, default('FALSE').
#' @param panel.spacing Facet plot panel space, default(0.3).
#' @param revNegStrand Whether reverse the negtive strand when set "forcePosRel=TRUE", default('FALSE').
#'
#' @import tidyverse
#' @import cowplot
#' @import stats
#'
#' @return A ggplot object.
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
#'              exonColorBy = 'transcript_id')
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
globalVariables(c('end', 'gene_id', 'gene_name','seqnames',
                  'start', 'strand','transcript_id','transcript_name', 'type', 'vl_x1' ,'width', 'yPos'))

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
                         exonColorBy = NULL,
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
                         absSpecArrowLen = FALSE,
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
                         marginX = 10,
                         marginY = 10,
                         aspect.ratio = NULL,
                         facetByGene = FALSE,
                         ncolGene = NULL,
                         scales = 'free',
                         strip.position = 'top',
                         forcePosRel = FALSE,
                         panel.spacing = 0.3,
                         revNegStrand = FALSE){
  ##############################################################################
  # test whether with a given specific gene or region

  if(is.null(gene)){
    # filter gene by region
    myGene <- gtfFile %>%
      dplyr::filter(seqnames == Chr & start >= posStart & end <= posEnd) %>%
      dplyr::filter(type != 'gene') %>%
      dplyr::select(seqnames,start,end,width,strand,type,gene_id,gene_name,transcript_id,transcript_name)
  }else{
    # filter gene by gene name
    myGene <- gtfFile %>%
      dplyr::filter(gene_name %in% gene) %>%
      dplyr::filter(type != 'gene') %>%
      dplyr::select(seqnames,start,end,width,strand,type,gene_id,gene_name,transcript_id,transcript_name)
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
  # whether transform coordinate
  if(forcePosRel == TRUE){
    purrr::map_df(gene,function(x){
      tmp <- mul_exon_ypos %>%
        dplyr::filter(gene_name == x)
      purrr::map_df(unique(tmp$transcript_id),function(t){
        tmp1 <- tmp %>% dplyr::filter(transcript_id == t) %>%
          dplyr::arrange(start,end)

        # whether reverse negtive strand
        if(revNegStrand == FALSE){
          # start coord
          startPos <- min(tmp1$start)

          # add new pos
          tmp1 <- tmp1 %>% dplyr::mutate(start = start - startPos,
                                         end = end - startPos)
        }else{
          if(unique(tmp1$strand) == '-'){
            # end coord
            endPos <- max(tmp1$end)

            # add new pos
            tmp1 <- tmp1 %>% dplyr::mutate(start = endPos - start,
                                           end = endPos - end)
          }else{
            # start coord
            startPos <- min(tmp1$start)

            # add new pos
            tmp1 <- tmp1 %>% dplyr::mutate(start = start - startPos,
                                           end = end - startPos)
          }
        }
        return(tmp1)
      }) -> relPos_tmp
      return(relPos_tmp)
    }) -> exonNewPos
  }else{
    exonNewPos <- mul_exon_ypos
  }

  ##############################################################################
  # extarct data
  exon <- exonNewPos %>% dplyr::filter(type != 'transcript')
  trans <- exonNewPos %>% dplyr::filter(type == 'transcript')

  # add text x/y pos
  if(revNegStrand == FALSE){
    trans$textX <- ifelse(trans$strand == '+',trans$start + trans$width/2,
                          trans$end - trans$width/2)
    trans$textY <- trans$yPos + relTextDist
  }else{
    trans$textX <- (trans$start + trans$end)/2
    trans$textY <- trans$yPos + relTextDist
  }

  ##############################################################################
  # whether add specific arrow
  if(newStyleArrow == FALSE){
    arrow_trans <- trans
  }else{
    purrr::map_df(gene,function(gen){
      genTrans <- trans %>% dplyr::filter(gene_name == gen) %>%
        dplyr::arrange(dplyr::desc(width))

      # define longest transcript length
      longestWidth = genTrans$width[1]

      # add special arrow
      purrr::map_df(genTrans$transcript_id,function(x){
        tmp <- genTrans %>%
          dplyr::filter(transcript_id == x)

        # test strand
        if(absSpecArrowLen == TRUE){
          # add absolute specArrow
          if(tmp$strand == '+'){
            tmp <- tmp %>% dplyr::mutate(vl_x1 = start + speArrowRelPos*width,
                                         vl_x2 = vl_x1 + speArrowRelLen*longestWidth,
                                         vl_y1 = yPos + speArrowStart,
                                         vl_y2 = yPos + speArrowStart*speArrowRelHigh)
          }else if(tmp$strand == '-'){
            tmp <- tmp %>% dplyr::mutate(vl_x1 = end - speArrowRelPos*width,
                                         vl_x2 = vl_x1 - speArrowRelLen*longestWidth,
                                         vl_y1 = yPos + speArrowStart,
                                         vl_y2 = yPos + speArrowStart*speArrowRelHigh)
          }
        }else{
          # add relative specArrow
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
        }
        return(tmp)
      }) -> arrow_trans1
      return(arrow_trans1)
    }) -> arrow_trans
  }

  # change reversed specArrow direction
  if(revNegStrand == FALSE){
    arrow_trans <- arrow_trans
  }else{
    arrow_trans$vl_x2 <- abs(arrow_trans$vl_x2)
  }

  # strand control arrow direction
  arrow_trans$ad <- ifelse(arrow_trans$strand == '+','last','first')

  # arrow breaks
  arrow_seq = c(0.05,seq(0,0.95,arrowBreak)[-1],1)

  ##############################################################################
  # first layer
  if(is.null(exonColorBy)){
    p1 <- ggplot2::ggplot(exon) +
      ggplot2::geom_rect(ggplot2::aes_(xmin = ~start,xmax = ~end,
                                       ymin = ~ymin,ymax = ~ymax),
                         fill = exonFill)
  }else{
    p1 <- ggplot2::ggplot(exon) +
      ggplot2::geom_rect(ggplot2::aes_(xmin = ~start,xmax = ~end,
                                       ymin = ~ymin,ymax = ~ymax,
                                       fill = ~get(exonColorBy)),
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
  # whether facet by gene when gene far away each other or not on same chromosome
  if(facetByGene == FALSE){
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
          ggplot2::geom_segment(data = arrow_trans,
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
  }else{
    # define columns to facet
    if(is.null(ncolGene)){
      ncol = length(gene)
    }else{
      ncol = ncolGene
    }
    # facet plot

    # whether draw ploar plot
    if(circle == FALSE){
      # add arrow and segment line with geom_arrowsegment
      if(addNormalArrow == TRUE){
        # loop add arrow
        for (g in gene) {
          tmpTrans <- arrow_trans %>% dplyr::filter(gene_name == g)
          p2 <- p2 +
            ggarchery::geom_arrowsegment(data = tmpTrans,
                                         ggplot2::aes_(x = ~start,xend = ~end,
                                                       y = ~yPos,yend = ~yPos),
                                         color = arrowCol,
                                         size = intronSize,
                                         arrow_positions = arrow_seq,
                                         arrow_fills = rep(arrowCol,length(arrow_seq)),
                                         arrows = list(ggplot2::arrow(angle = arrowAngle,
                                                                      length = ggplot2::unit(arrowLength, "inches"),
                                                                      ends = tmpTrans$ad,
                                                                      type = arrowType)))
        }
        p3_tmp <- p2
      }else{
        p3_tmp <- p2 +
          ggplot2::geom_segment(data = arrow_trans,
                                ggplot2::aes_(x = ~start,xend = ~end,
                                              y = ~yPos,yend = ~yPos),
                                color = arrowCol,
                                size = intronSize)
      }
    }else{
      # add arrow and segment line geom_segment
      if(addNormalArrow == TRUE){
        # loop add arrow
        for (g in gene) {
          tmpTrans <- arrow_trans %>% dplyr::filter(gene_name == g)
          p2 <- p2 +
            ggplot2::geom_segment(data = tmpTrans,
                                  ggplot2::aes_(x = ~start,xend = ~end,
                                                y = ~yPos,yend = ~yPos),
                                  color = circSegCol,
                                  size = intronSize,
                                  arrow = ggplot2::arrow(angle = arrowAngle,
                                                         length = ggplot2::unit(arrowLength, "inches"),
                                                         ends = tmpTrans$ad,
                                                         type = arrowType))
        }
        p3_tmp <- p2
      }else{
        p3_tmp <- p2 +
          ggplot2::geom_segment(data = arrow_trans,
                                ggplot2::aes_(x = ~start,xend = ~end,
                                              y = ~yPos,yend = ~yPos),
                                color = circSegCol,
                                size = intronSize)
      }
    }

    # facet
    p3 <- p3_tmp +
      ggplot2::facet_wrap(facets = "gene_name",
                          scales = scales,
                          ncol = ncol,
                          strip.position = strip.position)
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

  ##############################################################################
  # facet background
  if(facetByGene == TRUE){
    p6 <- p5 +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = base_size + 2),
                     strip.background = ggplot2::element_rect(fill = 'grey90'),
                     panel.spacing = ggplot2::unit(panel.spacing,'cm'))
  }else{
    p6 <- p5
  }

  ##############################################################################
  # xlabel
  if(forcePosRel == TRUE){
    p7 <- p6 +
      ggplot2::xlab('Positions on genome')
  }else{
    p7 <- p6 +
      ggplot2::xlab('Relative position to start/end')
  }
  return(p7)
}

###############################
#' This is a test data for this package
#' test data describtion
#'
#' @name gtf
#' @docType data
#' @author Junjun Lao
"gtf"
