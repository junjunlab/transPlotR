#' @title trackVis
#' @name trackVis
#' @author JunZhang
#' @description visualize bigwig files.
#'
#' @param bWData the data.frame bigwig data, default(NULL).
#' @param gtf.file whether supply gtf annotation file, default(NULL).
#' @param gene.name the gene name to be choosed for visualization, default(NULL).
#' @param chr chr the chromesome of peak, default(NULL).
#' @param region.min the start coordinate, default(NULL).
#' @param region.max the end coordinate, default(NULL).
#' @param show.legend whether show color legend, default(FALSE).
#' @param legend.position the legend position, default("right").
#' @param color the track color, default(NULL).
#' @param extend.up extend for upstream of start site, default(3000).
#' @param extend.dn extend for downstream of start site, default(3000).
#' @param base_size theme base size, default(14).
#' @param label.angle the facet label angle, default(0).
#' @param label.face the facet label face, default("bold").
#' @param space.y the facet panel space, default(0.5).
#' @param sample.order the sample order to be plotted in graph, default(NULL).
#' @param sampleName.dist the facet label distance from Y axis, default(0.4).
#' @param sampleName.hjust the facet label hjust, default(1).
#' @param sampleName.vjust the facet label vjust, default(0.5).
#' @param xAxis.info whether retain X axis info, default(TRUE).
#' @param yAxis.info whether retain Y axis info, default(TRUE).
#' @param ticks.y.len the y axis ticks length, default(0.3).
#' @param theme plot theme, "bw" or "classic", default("classic").
#' @param scales the facet scales settings, default("fixed").
#' @param ncol the columns to be arranged, default(1).
#' @param mark.region whether highlight regions in plot, default(FALSE).
#' @param mark.col the colors of marked regions, default(NULL).
#' @param mark.alpha the color alpha of marked regions, default(0.5).
#' @param new.yaxis whether add new style Y axis, default(FALSE).
#' @param pos.ratio the new style Y axis relative position, default(c(0.05,0.8)).
#' @param yinfo.text.size the new style Y axis text size, default(5).
#'
#' @param back.color whether add panel background color, default(FALSE).
#' @param back.color.alpha panel background color alpha, default(0.15).
#' @param y.max the ylim, default(NULL).
#' @param new.label whether add label in plot, default(FALSE).
#' @param label.color the label color, default(NULL).
#' @param pos.label.ratio the new label relative position, default(c(0.95,0.8)).
#' @param label.text.size the new label text size, default(5).
#' @param label.hjust the new label text hjust, default(1).
#'
#' @return a ggplot object.
#' @export

globalVariables(c("fileName","group","label","score","x","y"))

trackVis <- function(bWData = NULL,
                     gtf.file = NULL,
                     gene.name = NULL,
                     chr = NULL,
                     region.min = NULL,
                     region.max = NULL,
                     show.legend = FALSE,
                     legend.position = "right",
                     color = NULL,
                     extend.up = 3000,
                     extend.dn = 3000,
                     base_size = 14,
                     label.angle = 0,
                     label.face = "bold",
                     space.y = 0.5,
                     sample.order = NULL,
                     sampleName.dist = 0,
                     sampleName.hjust = 1,
                     sampleName.vjust = 0.5,
                     y.max = NULL,
                     xAxis.info = TRUE,
                     yAxis.info = TRUE,
                     ticks.y.len = 0.3,
                     theme = "classic",
                     scales = "fixed",
                     ncol = 1,
                     mark.region = NULL,
                     mark.col = NULL,
                     mark.alpha = 0.5,
                     new.yaxis = FALSE,
                     pos.ratio = c(0.05,0.8),
                     yinfo.text.size = 5,
                     new.label = FALSE,
                     label.color = NULL,
                     pos.label.ratio = c(0.95,0.8),
                     label.text.size = 5,
                     label.hjust = 1,
                     back.color = FALSE,
                     back.color.alpha = 0.15){
  # whether supply gene name
  if(!is.null(gtf.file) & !is.null(gene.name)){
    gene <- gtf.file %>% dplyr::filter(gene_name == gene.name)
    chr <- unique(gene$seqnames) %>% as.character()
    region.min <- min(gene$start)
    region.max <- max(gene$start)
  }

  # filter specified region
  regeion.bw <- bWData %>%
    dplyr::filter(seqnames == chr) %>%
    dplyr::filter(start >= (region.min - extend.up) & end <= (region.max + extend.dn))

  # whether change order
  if(!is.null(sample.order)){
    regeion.bw$fileName <- factor(regeion.bw$fileName,levels = sample.order)
  }

  # panel background data
  dback <- data.frame(fileName = unique(regeion.bw$fileName))

  # whether add background
  if(back.color == TRUE){
    p0 <-
      ggplot2::ggplot(regeion.bw) +
      ggplot2::geom_rect(data = dback,
                         ggplot2::aes(xmin = -Inf,xmax = Inf,ymin = 0,ymax = Inf,
                                      fill = fileName),
                         show.legend = FALSE,
                         alpha = back.color.alpha)
  }else{
    p0 <-
      ggplot2::ggplot(regeion.bw)
  }

  # plot
  p1 <- p0 +
    ggplot2::geom_rect(ggplot2::aes(xmin = start,xmax = end,
                                    ymin = 0,ymax = score,
                                    fill = fileName,color = fileName),
                       show.legend = show.legend) +
    ggplot2::geom_segment(ggplot2::aes(x = min(start),xend = max(end),
                                       y = 0,yend = 0),
                          size = 1) +
    ggplot2::xlab('') + ggplot2::ylab('') +
    ggplot2::coord_cartesian(expand = 0) +
    ggplot2::facet_wrap(~fileName,ncol = ncol,
                        strip.position = 'left',scales = scales) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = '')) +
    ggplot2::guides(color = ggplot2::guide_legend(title = ''))

  # y labels
  if(is.null(y.max)){
    ylimit <- range(regeion.bw$score)[2]
  }else{
    ylimit <- y.max
  }

  if(scales == "fixed"){
    p1 <- p1 +
      ggplot2::scale_y_continuous(breaks = c(0,ylimit),
                                  limits = c(0,ylimit))
  }else{
    p1 <- p1
  }

  # choose theme
  if(theme == "bw"){
    p1.1 <- p1 +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = label.angle,
                                                               face = label.face,
                                                               size = base_size,
                                                               hjust = sampleName.hjust,
                                                               vjust = sampleName.vjust),
                     panel.grid = ggplot2::element_blank(),
                     legend.position = legend.position,
                     strip.placement = 'outside',
                     strip.background = ggplot2::element_rect(fill = NA,colour = NA),
                     strip.switch.pad.wrap = ggplot2::unit(sampleName.dist,'cm'),
                     panel.spacing = ggplot2::unit(space.y,'cm'))

  }else{
    p1.1 <- p1 +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = label.angle,
                                                               face = label.face,
                                                               size = base_size,
                                                               hjust = sampleName.hjust,
                                                               vjust = sampleName.vjust),
                     panel.grid = ggplot2::element_blank(),
                     legend.position = legend.position,
                     strip.placement = 'outside',
                     strip.background = ggplot2::element_rect(fill = NA,colour = NA),
                     strip.switch.pad.wrap = ggplot2::unit(sampleName.dist,'cm'),
                     panel.spacing = ggplot2::unit(space.y,'cm'),
                     axis.ticks.length.y = ggplot2::unit(ticks.y.len,"cm"))
  }

  # whether supply own colors
  if(!is.null(color)){
    p2 <- p1.1 +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::scale_color_manual(values = color)
  }else{
    p2 <- p1.1
  }

  # whether retain X axis info
  if(xAxis.info == FALSE){
    p3 <- p2 +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())
  }else{
    p3 <- p2
  }

  # whether retain Y axis info
  if(yAxis.info == FALSE){
    p4 <- p3 +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank())
  }else{
    p4 <- p3
  }

  # whether mark some regions
  if(!is.null(mark.region)){
    if(is.list(mark.region)){
      mark.df <- data.frame(start = unlist(mark.region[[1]]),
                            end = unlist(mark.region[[2]]),
                            group = as.character(1:length(unlist(mark.region[[1]]))))

      # add mark
      p5 <- p4 +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_rect(data = mark.df,
                           ggplot2::aes(xmin = start,xmax = end,
                                        ymin = 0,ymax = ylimit,
                                        fill = group),alpha = mark.alpha,
                           show.legend = FALSE)

      # change mark colors
      if(!is.null(mark.col)){
        p5 <- p5 +
          ggplot2::scale_fill_manual(values = mark.col)
      }else{
        p5 <- p5
      }
    }else{
      print("Please supply list object!")
    }
  }else{
    p5 <- p4
  }

  # whether add new yaxis
  if(new.yaxis == TRUE){
    yinfo <- data.frame(label = paste("[0-",ylimit,"]",sep = ''),
                        x = range(regeion.bw$start)[1] + pos.ratio[1]*(range(regeion.bw$start)[2] - range(regeion.bw$start)[1]),
                        y = pos.ratio[2]*ylimit)

    # add text label
    p6 <- p5 +
      ggplot2::geom_text(data = yinfo,
                         ggplot2::aes(x = x,y = y,label = label),size = yinfo.text.size)
  }else{
    p6 <- p5
  }

  # whether add new sample label
  if(new.label == TRUE){
    labelinfo <- data.frame(fileName = unique(regeion.bw$fileName),
                        x = range(regeion.bw$start)[1] + pos.label.ratio[1]*(range(regeion.bw$start)[2] - range(regeion.bw$start)[1]),
                        y = pos.label.ratio[2]*ylimit)

    # add text label
    if(is.null(label.color)){
      label.color <- rep('black',nrow(labelinfo))
    }else{
      label.color <- label.color
    }

    # plot
    p7 <- p6 +
      ggnewscale::new_scale_color() +
      ggplot2::geom_text(data = labelinfo,
                         ggplot2::aes(x = x,y = y,label = fileName,color = fileName),
                         show.legend = FALSE,
                         size = label.text.size,
                         hjust = label.hjust,
                         fontface = label.face) +
      ggplot2::scale_color_manual(values = label.color) +
      ggplot2::theme(strip.text.y.left = ggplot2::element_blank())
  }else{
    p7 <- p6
  }

  return(p7)
}
