#' Create a Blank ggplot Object with Image Dimensions
#'
#' Creates a blank ggplot object with the same width and height as a specified image.
#'
#' @param path The path to the image file.
#' @return A ggplot object with a blank canvas.
#' @examples
#' image_path <- "path/to/your/image.jpg"
#' blank_plot <- gg_draw_blank(image_path)
#'
#' @export
gg_draw_blank <- function(path){
  img <- magick::image_read(path)
  width <- magick::image_info(img)$width
  height <- magick::image_info(img)$height
  dammy <- tibble::tibble(width, height)
  gg <- 
    ggplot(data = dammy, aes(width, height)) +
      ggplot2::scale_x_continuous(limits = c(0, width) , expand = c(0, 0)) + 
      ggplot2::scale_y_continuous(limits = c(0, height), expand = c(0, 0)) + 
      ggplot2::theme_void() +
      ggplot2::theme(panel.background = element_rect(fill = "white", colour = "white"))
  return(gg)
}

#' Add Text Annotations to a ggplot
#'
#' Adds text annotations to an existing ggplot object based on a data frame.
#'
#' @param gg The ggplot object to modify.
#' @param df A data frame containing the annotation data.
#' @param ... Additional arguments to be passed to `geom_text()`.
#' @return The modified ggplot object with the added text annotations.
#'
#' @export
gg_add_text <- function(gg, df, ...){
  # image width and height
  width <- gg$data$width
  height <- gg$data$height
  if(is.null(width)){
    width <- gg$coordinates$limits$x[2]
    height <- gg$coordinates$limits$y[2]
  }
  # y-axis: upside down (image -> ggplot)
  df <- dplyr::mutate(df, ymin = height - ymin)
  gg +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(x = xmin, y = ymin, label = label), 
      hjust = "left",
      ...)
}


#' @title Draw a Rectangle on an Image
#' 
#' @description Draws a rectangle on an image using Magick.
#' base version 
#' @param img The image to draw on.
#' @param xmin,ymin,xmax,ymax The xy-coordinate of the corners of the rectangle.
#' @param border The color of the rectangle border (default is "red").
#' @param lty The line type of the rectangle border (default is "dashed").
#' @param lwd The line width of the rectangle border (default is 2).
#' @return The modified image with the rectangle drawn on it.
#' @examples
#' # Load an image
#' image_path <- "path/to/your/image.jpg"
#' img <- magick::image_read(image_path)
#'
#' # Draw a rectangle
#' img_with_rect <- draw_rect(img, 100, 200, 300, 400)
#'
#' # Display or save the modified image
#' magick::image_show(img_with_rect)
#' @export
#' 
draw_rect <- function(img, xmin, ymin, xmax, ymax,
                      border = "red", lty = "dashed", lwd = 2){
  on.exit(dev.off())
  img <- magick::image_draw(img)
  xleft <- xmin
  xright <- xmax # right_bottom[1]
  ybottom <- ymax  # right_bottom[2]
  ytop <- ymin # left_top[2]
  rect(xleft, ybottom, xright, ytop, border = border, lty = lty, lwd = lwd)
  return(img)
}


gg_draw_image <- function(path){
  img <- magick::image_read(path)
  width <- magick::image_info(img)$width
  height <- magick::image_info(img)$height
  gg <- 
    magick::image_ggplot(img) +
      ggplot2::scale_x_continuous(limits = c(0, width) , expand = c(0, 0)) + 
      ggplot2::scale_y_continuous(limits = c(0, height), expand = c(0, 0)) + 
      ggplot2::theme_void()
  return(gg)
}

#' 
#' df: data.frame (cols: xmin, ymin, xmax, ymax, obj_class)
#' 
#' 
#' @export
#' 
gg_add_rect <- function(gg, df, alpha = 0.1, hide_legend = TRUE){
  # image width and height
  width <- gg$coordinates$limits$x[2]
  height <- gg$coordinates$limits$y[2]
  if(is.null(width)){
    width <- gg$data$width
    height <- gg$data$height
  }
  # y-axis: upside down (image -> ggplot)
  df <- dplyr::mutate(df, ymax = height - ymax, ymin = height - ymin)
  gg <- 
    gg + 
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                   fill = factor(obj_class)),
      data = df,
      alpha = alpha,
      inherit.aes = FALSE)
  if(hide_legend){
    gg <- gg + ggplot2::theme(legend.position="none")
  }
  return(gg)
}

#' 
#' df: data.frame (cols: xmin, ymin, xmax, ymax, obj_class)
#' 
gg_add_box <- function(gg, df, hide_legend = TRUE){
  # image width and height
  width <- gg$coordinates$limits$x[2]
  height <- gg$coordinates$limits$y[2]
  # y-axis: upside down (image -> ggplot)
  df <- dplyr::mutate(df, ymax = height - ymax, ymin = height - ymin)
  gg <- 
    gg + 
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                   colour = factor(obj_class)),
      data = df,
      fill = "transparent",
      inherit.aes = FALSE)
  if(hide_legend){
    gg <- gg + ggplot2::theme(legend.position="none")
  }
  return(gg)
}



#' detectron_results：detectronの結果のcsvを読み取ったdf
#' file_name,obj_class,min_score：列の値でのフィルタ用の設定
#' 
#' @export
#' 
show_detectron_results <- function(detectron_results, file_name = "", obj_class = "all", min_score = 0.8){
  detectron_results <- filter_detectron_results(detectron_results, file_name, obj_class, min_score)
  files <- 
    detectron_results |>
    file_names() |>
    unique()
  for(f in files){
    results <- 
      detectron_results |>
      filter_detectron_results(file_name = f) |>
      dplyr::select(xmin, ymin, xmax, ymax)
    img <- magick::image_read(f)
    img <- preduce(results, draw_rect, .init = img)
    magick::image_write(img, paste("res", obj_class, f, sep = "_"))
  }
}




#' detectron_results：detectronの結果のcsvを読み取ったdf
#' file_name,obj_class,min_score：列の値でのフィルタ用の設定
#' 
#' @export
#' 
filter_detectron_results <- function(detectron_results, file_name = "", obj_class = "all", min_score = 0.8){
  if(file_name != ""){
    detectron_results <- dplyr::filter(detectron_results, file_name %in% {{ file_name }})
  }
  if(obj_class != "all"){
    detectron_results <- dplyr::filter(detectron_results, obj_class %in% {{ obj_class }})
  }
  detectron_results <- dplyr::filter(detectron_results, score > {{ min_score }})
  return(detectron_results)
}
