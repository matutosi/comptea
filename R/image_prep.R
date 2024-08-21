#'
#'
#' 
#' @export
#' 
prep_img <- function(path, fuzz = 50, type = "otsu", ...){
  dsk <- automater::path_convert(path, pre = "dsk_")
  img <- 
    path |>
    magick::image_read() |>
    magick::image_convert(format = "pgm", colorspace = "Gray") |>
    image.binarization::image_binarization(type = type, ...) |>
    magick::image_fill("white", "+0+0", fuzz = fuzz) |>
    magick::image_deskew() |>
    magick::image_trim() |>
    magick::image_border("white", geometry = "50x50") |>
    magick::image_write(dsk)
  return(img)
}

binarization <- function(path, type = "otsu", ...){
  bnz <- automater::path_convert(path, pre = "bnz_")
  magick::image_convert(img, format = "pgm", colorspace = "Gray")
  img <- 
    path |>
    magick::image_read() |>
    image.binarization::image_binarization(type = type, ...)
    magick::image_trim() |>
    magick::image_border("white", geometry = "50x50") |>
    magick::image_write(bnz)
  return(img)
}

#'
#' @examples
#' # 入力ファイル
#' jpgs <- fs::dir_ls(path = "D:/matu/scan", regexp = "s01114_kinki")
#' tmp <- fs::path_temp()
#' jpgs <- fs::file_copy(jpgs, tmp, overwrite = TRUE)
#' 
#' # 作業ディレクトリ
#' # wd <- ""
#' setwd(tmp)
#' 
#' # 傾き補正
#' jpgs <- 
#'   jpgs |> 
#'  purrr::map_chr(deskew_trim_img, fuzz = 40)
#' 
#' @export
#' 
deskew_trim_img <- function(path, fuzz = 50){
  dsk <- automater::path_convert(path, pre = "dsk_")
  img <- 
    path |>
    magick::image_read() |>
    magick::image_fill("white", "+0+0", fuzz = fuzz) |>
    magick::image_deskew() |>
    magick::image_trim() |>
    magick::image_border("white", geometry = "50x50") |>
    magick::image_write(dsk)
  return(img)
}

