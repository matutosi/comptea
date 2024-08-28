#'
#'
#'
#'
#'
#'
locate_items <- function(detectron_results, file_name, 
                        item = c("all", "table", "species", "layer", "comp", "cols", "rows", "sname")){
  class_table <- 0
  class_spec  <- 1
  class_layer <- 2
  class_cols  <- 3
  class_rows  <- 4
  class_sname <- 5
  class_comp  <- 6
  
  dir_img <-  unique(detectron_results$dir_img)
  model <- unique(detectron_results$model)

  spec_x_range  <- locate_x_range(detectron_results, file_name, obj_class = class_spec)
  sname_x_range <- locate_x_range(detectron_results, file_name, obj_class = class_sname)
  layer_x_range <- locate_x_range(detectron_results, file_name, obj_class = class_layer)
  comp_x_range  <- locate_x_range(detectron_results, file_name, obj_class = class_cols)
  comp_y_range  <- locate_y_range(detectron_results, file_name, obj_class = class_rows)

  col_mean_width <- mean_width(detectron_results, file_name, obj_class = class_cols)
  row_mean_height <- mean_height(detectron_results, file_name, obj_class = class_rows)

  n_cols <- ((comp_x_range[[2]] - comp_x_range[[1]]) / col_mean_width)  |> round(0)
  n_rows <- ((comp_y_range[[2]] - comp_y_range[[1]]) / row_mean_height) |>  round(0)
  spec  <- coord_item(spec_x_range , comp_y_range, n_rows = n_rows,                  obj_class = class_spec , file_name = file_name)
  sname <- coord_item(sname_x_range, comp_y_range, n_rows = n_rows,                  obj_class = class_sname, file_name = file_name)
  layer <- coord_item(layer_x_range, comp_y_range, n_rows = n_rows,                  obj_class = class_layer, file_name = file_name)
  comp  <- coord_item(comp_x_range , comp_y_range, n_rows = n_rows, n_cols = n_cols, obj_class = class_comp , file_name = file_name)
  res <- 
    dplyr::bind_rows(spec, sname, layer, comp) |>
    dplyr::mutate(`:=`(dir_img  , dir_img)) |>
    dplyr::mutate(`:=`(file_name, file_name)) |>
    dplyr::mutate(`:=`(model    , model))
  return(res)
}

#' 
#' file_name: 画像のファイル名
#' obj_class: colsのクラス
#' 
locate_x_range <- function(detectron_results, file_name, obj_class = 3){
  results <- filter_detectron_results(detectron_results, file_name, obj_class = obj_class)
  if(nrow(results) != 0){
    x_range <- dplyr::summarise(results, xmin = min(xmin), xmax = max(xmax))
  }else{
    x_range <- tibble::tibble(xmin = 0, xmax = 0)
  }
  return(x_range)
}

#' 
#' file_name: 画像のファイル名
#' obj_class: rowsのクラス
#' 
locate_y_range <- function(detectron_results, file_name, obj_class = 4){
  y_range <- 
    detectron_results |>
    filter_detectron_results(file_name, obj_class = obj_class) |>
    dplyr::summarise(ymin = min(ymin), ymax = max(ymax))
  return(y_range)
}


#' file_name: 画像のファイル名
#' obj_class: colsのクラス
#' 
#' Difference of each colmuns width (xmax - xmin) is smaller than expected, 
#' because it does not contain space between colmuns
#' 
#' 
mean_width <- function(detectron_results, file_name, obj_class = 3){
  df <- 
    filter_detectron_results(detectron_results, file_name, obj_class = obj_class) |>
    dplyr::arrange(xmin)
  mw <- 
    c(dplyr::lead(df$xmin) - df$xmin,     # diff of xmin
      dplyr::lead(df$xmax) - df$xmax) |>  # diff of xmax
    mean(na.rm = TRUE, trim = 0.1)
  return(mw)
}

#' file_name: 画像のファイル名
#' obj_class: colsのクラス
#' 
#' Difference of each colmuns width (xmax - xmin) is smaller than expected, 
#' because it does not contain space between colmuns
#' 
#' 
mean_height <- function(detectron_results, file_name, obj_class = 4){
  df <- 
    detectron_results |>
    filter_detectron_results(file_name, obj_class = obj_class) |>
    dplyr::arrange(ymin)
  mh <- 
    c(dplyr::lead(df$ymin) - df$ymin,     # diff of ymin
      dplyr::lead(df$ymax) - df$ymax) |>  # diff of ymax
    mean(na.rm = TRUE, trim = 0.4)
  return(mh)
}


#' file_name: 画像のファイル名
#' obj_class: rowsのクラス
#' 
mean_height_old <- function(detectron_results, file_name, obj_class = 4){
  mh <- 
    detectron_results |>
    filter_detectron_results(file_name, obj_class = obj_class) |>
    dplyr::mutate(height = ymax - ymin) |>
    `$`(_, "height") |>
    mean()
  return(mh)
}

#' 
#' rangeとnから
#' 端と区切りの位置を求める
#' 両端を含むので，length.out は n ではなく，n + 1．
#' rangeがリストの場合は，range[1]ではエラーになるので，range[[1]]を使う
#' 
seq_range <- function(range, n){
  res <- seq(from = range[[1]], 
             to = range[[2]], 
             length.out = n + 1)
  return(res)
}

#'
#'
#'
#'
#'
coord_item <- function(x_range, y_range, 
                       n_rows = NULL, n_cols = NULL, 
                       obj_class, file_name = file_name){
  # check input
  if(is.null(n_rows) & is.null(n_cols)){
    stop('Need one of "n_rows" or "n_cols" !')
  }
  # x-axis
  if(is.null(n_cols)){
    xmin <- x_range[[1]]
    xmax <- x_range[[2]]
  }else{
    xmin <- seq_range(x_range, n_cols)
    xmax <- dplyr::lead(xmin) # include NA
  }
  # y-axis
  if(is.null(n_rows)){
    ymin <- y_range[[1]]
    ymax <- y_range[[2]]
  }else{
    ymin <- seq_range(y_range, n_rows)
    ymax <- dplyr::lead(ymin) # include NA
  }
  if(!is.null(n_rows) & !is.null(n_cols)){
    xmin <- rep(xmin, each = n_rows + 1)
    xmax <- rep(xmax, each = n_rows + 1)
    ymin <- rep(ymin, times = n_cols + 1)
    ymax <- rep(ymax, times = n_cols + 1)
  }
  df <- 
    tibble::tibble(xmin = xmin,
                   xmax = xmax, 
                   ymin = ymin, 
                   ymax = ymax,
                   obj_class = obj_class) |>
    na.omit()
  return(df)
}

