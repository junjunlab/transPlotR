#' @title linkVis
#' @name linkVis
#' @author JunZhang
#' @description visualize the coordinate relation like chromtin accessbility or peak sites correlation.
#'
#' @param linkData the link data with data.frame format, default(NULL).
#' @param start link start position, default(NULL).
#' @param end link end position, default(NULL).
#' @param group facet group variable name, default(NULL).
#' @param link.aescolor link line color or mapping variable name, default(NULL).
#' @param link.color colors to change the link line colors when "link.aescolor" is a mapping variable, default(NULL).
#' @param line.size link line size, default(0.5).
#' @param curvature the link line curvature, default(0.5).
#' @param yshift the space upper the link line, default(0.1).
#' @param legend.title the legend title, default("").
#' @param facet whether show the plot with facet plot, default(TRUE).
#' @param facet.placement the facet label placement, default("outside").
#' @param facet.fill facet rectangle fill color, default("grey90").
#' @param facet.color facet rectangle border color, default("black").
#' @param facet.text.angle facet text angle, default(90).
#' @param facet.text.size facet text size, default(14).
#' @param xAixs.info whether remove X axis info, default(FASLE).
#'
#' @return a ggplot object.
#' @export

linkVis <- function(linkData = NULL,
                    start = NULL,
                    end = NULL,
                    group = NULL,
                    link.aescolor = NULL,
                    link.color = NULL,
                    line.size = 0.5,
                    curvature = 0.5,
                    yshift = 0.1,
                    legend.title = "",
                    facet = TRUE,
                    facet.placement = "outside",
                    facet.fill = "grey90",
                    facet.color = "black",
                    facet.text.angle = 90,
                    facet.text.size = 14,
                    xAixs.info = TRUE){
  # get input data
  data <- linkData
  colname <- colnames(data)

  # whether facet by groups
  if(facet == TRUE){
    if(link.aescolor %in% colname){
      p1 <-
        ggplot2::ggplot(linkData) +
        ggplot2::geom_curve(ggplot2::aes_string(x = start,xend = end,
                                                y = group,yend = group,
                                                color = link.aescolor),
                            curvature = curvature,
                            lwd = line.size)
    }else{
      p1 <-
        ggplot2::ggplot(linkData) +
        ggplot2::geom_curve(ggplot2::aes_string(x = start,xend = end,
                                                y = group,yend = group),
                            color = link.aescolor,
                            curvature = curvature,
                            lwd = line.size)
    }
  }else{
    linkData$yc <- as.character(1)

    if(link.aescolor %in% colname){
      p1 <-
        ggplot2::ggplot(linkData) +
        ggplot2::geom_curve(ggplot2::aes_string(x = start,xend = end,
                                                y = "yc",yend = "yc",
                                                color = link.aescolor),
                            curvature = curvature,
                            lwd = line.size)
    }else{
      p1 <-
        ggplot2::ggplot(linkData) +
        ggplot2::geom_curve(ggplot2::aes_string(x = start,xend = end,
                                                y = "yc",yend = "yc"),
                            color = link.aescolor,
                            curvature = curvature,
                            lwd = line.size)
    }
  }

  # ajust y
  p1 <- p1 +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0,-yshift))) +
    ggplot2::theme_bw() +
    ggplot2::xlab('') + ggplot2::ylab('')

  # whether mapping color
  if(link.aescolor %in% colname){
    if(is.null(link.color)){
      if(is.numeric(linkData[,link.aescolor])){
        p2 <- p1 +
          ggplot2::scale_color_gradient(low = "#339933",
                                        high = "#FF9900",
                                        name = legend.title)
      }else{
        p2 <- p1 +
          ggplot2::scale_color_discrete(name = legend.title)
      }
    }else{
      if(is.numeric(linkData[,link.aescolor])){
        p2 <- p1 +
          ggplot2::scale_color_gradient(low = link.color[1],
                                        high = link.color[2],
                                        name = legend.title)
      }else{
        p2 <- p1 +
          ggplot2::scale_color_manual(values = link.color,
                                      name = legend.title)
      }
    }
  }else{
    p2 <- p1
  }

  # facet
  if(facet == TRUE){
    p3 <- p2 +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     strip.placement = facet.placement,
                     strip.background = ggplot2::element_rect(fill = facet.fill,
                                                              color = facet.color),
                     strip.text.y.left = ggplot2::element_text(angle = facet.text.angle,
                                                               size = facet.text.size)) +
      ggplot2::facet_wrap(facets = group,
                          ncol = 1,scales = "free_y",
                          strip.position = "left")
  }else{
    p3 <- p2 +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank())
  }

  # whether remove X axis info
  if(xAixs.info == FALSE){
    p4 <- p3 +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
  }else{
    p4 <- p3
  }
  return(p4)
}
