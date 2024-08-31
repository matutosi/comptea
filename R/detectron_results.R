read_detectron_results <- function(dir_results, model = "", dir_img){
  files <- fs::dir_ls(dir_results, regexp = "\\.csv")
  if(model != ""){
    files <- stringr::str_subset(files, model)
  }
  results <- 
    files |>
    purrr::map(readr::read_csv, show_col_types = FALSE) |>
    purrr::map2(files, 
      \(.x ,.y){ dplyr::mutate(.x, 
                               model = fs::path_file(.y),
                               .before = 1) }) |>
    dplyr::bind_rows() |>
    rename_cols() |>
    dplyr::mutate(`:=`(dir_img, {{ dir_img }}))
  return(results)
}

read_comp_tables <- function(dir_img){
  tables <-
    dir_img |>
    fs::dir_ls(regexp = "\\.jpg") |>
    magick::image_read()
  return(tables)
}

#' detectron_results
#' 列名をわかりやすく名前を変更するだけ
#' 
#' 
#' 
rename_cols <- function(detectron_results){
  cn <- 
              c("bb_0", "bb_1", "bb_2", "bb_3", "class") |>
    `names<-`(c("xmin", "ymin", "xmax", "ymax", "obj_class"))
  return(dplyr::rename(detectron_results, any_of(cn)))
}

#' Extract Unique File Names from Detectron2 Results
#' 
#' @param detectron_results A list or data frame containing Detectron2 results.
#' @param file_name The name of the column containing the file names (default is "file_name").
#' @return A vector of unique file names.
#' @export
file_names <- function(detectron_results, file_name = "file_name"){
  detectron_results[["file_name"]] |>
  unique()
}
