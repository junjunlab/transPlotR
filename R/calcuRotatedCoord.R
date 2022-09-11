#' @title calcuRotatedCoord
#' @name calcuRotatedCoord
#' @author JunZhang
#' @description calculate the rotated rectangle coordinate with specified degree.
#'
#' @param data data.frame
#' @param theta rotate degree, default(45).
#' @param workers how many workers for parallel calculation, default(1).
#' @param rx x variable name, default(NULL).
#' @param ry y variable name, default(NULL).
#'
#' @return a data.frame
#' @export

globalVariables(c('.data','multisession','xr','yr'))

calcuRotatedCoord <- function(data = NULL,
                              rx = NULL,
                              ry = NULL,
                              theta = 45,
                              workers = 1){
  # Set a "plan" for how the code should run.
  future::plan(future::multisession, workers = workers)

  # get coord
  furrr::future_map_dfr(1:nrow(data),function(i){
    tmp <- data[i,]

    x <- rx
    y <- ry

    tmp <- tmp %>%
      dplyr::mutate(xr = .data[[x]]*cos(pi*(theta/180)) + .data[[y]]*sin(pi*(theta/180)),
                    yr = .data[[y]]*cos(pi*(theta/180)) - .data[[x]]*sin(pi*(theta/180)))

    tmp <- tmp %>%
      dplyr::mutate(xr = xr*cos(pi*(theta/180)),
                    yr = yr*sin(pi*(theta/180)))
    return(tmp)
  }) -> data

  return(data)
}
