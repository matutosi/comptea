
ocr_segments <- function(locations, trim = FALSE, ...){
  img <- 
    fs::path(locations$dir_img, locations$file_name) |>
    unique() |>
    magick::image_read()
  geometry <- pmap_chr(locations, ltrb2geo)
  texts <- purrr::map_chr(geometry, ocr_segment, img = img, trim = trim)
  res <- dplyr::mutate(locations, label = texts, .after = ymax)
  return(res)
}


ocr_segment <- function(geometry, img, trim = FALSE, ...){
  img <- magick::image_crop(img, geometry)
  if(trim){
    image_trim_ps <- purrr::possibly(magick::image_trim, otherwise = img)
    img <- image_trim_ps(img)
  }
  text <- magick::image_ocr(img)
  if(length(text) == 0) {
    text <- ""
  }
  text <- stringr::str_remove(text, "\n$")
  return(text)
}


#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' @export
#' 
crop_segments <- function(img_path, locations, out_dir = fs::path_temp(), trim = FALSE){
  img <- magick::image_read(img_path)
  file <- fs::path_file(img_path)
  geometry <- pmap_chr(locations, ltrb2geo)
  out_path <- pmap_chr(locations, create_out_path, file = file, out_dir = out_dir)
  purrr::walk2(geometry, out_path, 
               crop_segment, 
               img = img, trim = trim)
  return(out_path)
}

#' Wrapper function for crop_segments()
#' img will be cropped and saved in the out_path.
#' 
#' @param geometry  Geometry for image_crop().
#' @param out_path  A string for output path
#' @param img       {magick} image object.
#' 
#' @return           A character string representing the output file path.
#' 
#' @export
#' 
crop_segment <- function(geometry, out_path, img, trim = FALSE){
  img <- magick::image_crop(img, geometry)
  if(trim){
    image_trim_ps <- purrr::possibly(magick::image_trim, otherwise = img)
    img <- image_trim_ps(img)
  }
  magick::image_write(img, out_path)
  return(out_path)
}

#' Create a output file path based on the image file name, location and object class
#'
#' This function generates a new file path by combining the image file name, 
#' additional information (specified by `obj_class`, `xmin`, and `ymin`), 
#' and the original file extension. 
#'
#' @param obj_class,xmin,ymin  
#' @param file       The original image file path.
#' @param out_dir    The output directory.
#' @param ...        Additional arguments (will be omitted).
#' @return           A character string representing the output file path.
#'
#' @examples
#' create_out_path(locations$spec, "image.png", fs::path_temp())
#' 
#' @export
#' 
create_out_path <- function(obj_class, xmin, ymin, file, out_dir, ...){
  xmin <- round(xmin, 0)
  ymin <- round(ymin, 0)
  body <- fs::path_ext_remove(file)
  ext <- fs::path_ext(file)
  file <- 
    paste(body, obj_class, xmin, ymin, sep = "_") |>
    fs::path_ext_set(ext)
  fs::path(out_dir, file)
}


#' Comvert coordinate x-y point into {magick} geometry
#' @params  xmin,ymin,xmax,ymax 
#' @params  ... will be omitted.
#' 
#' @return A string like: "WIDTHxHEITH+X_OFFSET+Y_OFFSET".
#'         see https://docs.ropensci.org/magick/reference/transform.html
#' 
#' @examples
#' 
#' @export
#' 
ltrb2geo <- function(xmin, ymin, xmax, ymax, ...){
  xmin <- round(xmin, 0)
  ymin <- round(ymin, 0)
  xmax <- round(xmax, 0)
  ymax <- round(ymax, 0)
  geometry <- 
    paste0(     xmax - xmin, 
           "x", ymax - ymin, 
           "+", xmin, 
           "+", ymin)
    return(geometry)
}
