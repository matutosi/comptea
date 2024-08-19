#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 

labelme2coco <- function(input_dir, regexp = ""){
  path_jsons <- fs::dir_ls(input_dir, regexp = regexp)
  labelmes <- 
    path_jsons |>
    purrr::map(read_labelme) |>
    purrr::map(tibble::tibble) |>
    dplyr::bind_rows() |>
    dplyr::mutate(image_id    = image_path |> forcats::fct_inorder() |> as.integer()) |>
    dplyr::mutate(category_id = label      |> forcats::fct_inorder() |> as.integer())
  images <- 
    labelmes |>
    dplyr::distinct(image_id, image_path, image_height, image_width) |>
    dplyr::rename(id = image_id, file_name = image_path)
  categories <- 
    labelmes |>
    dplyr::distinct(category_id, label) |>
    dplyr::select(id = category_id, name = label, supercategory = label)
  annotations <- 
    labelmes |>
    dplyr::distinct(points, image_id, category_id) |>
    dplyr::transmute(image_id, category_id,
                     bbox = purrr::map(points, points2bbox),
                     area = purrr::map_dbl(bbox, bbox2area),
                     iscrowd = 0) |>
    tibble::rowid_to_column("id")
  return(
    list(annotations = annotations,
         images = images,
         categories = categories))
}

bbox2area <- function(bbox){
  bbox[3] * bbox[4]
}

points2bbox <- function(points){
  c(points[1, 1], 
    points[1, 2],
    points[2, 1] - points[1, 1],
    points[2, 2] - points[1, 2])
}

read_labelme <- function(path_json){
  json <- jsonlite::fromJSON(path_json)
  labelme <- json$shapes
  labelme$image_path <- json$imagePath
  labelme$image_height <- json$imageHeight
  labelme$image_width <- json$imageWidth
  return(labelme)
}

read_coco <- function(path_json){
  json <- jsonlite::fromJSON(path_json)
  annotations <- 
    tibble::tibble(json$annotations) 
  images <- tibble::tibble(json$images)
  categories <- tibble::tibble(json$categories)
  return(
    list(annotations = annotations,
         images = images,
         categories = categories))
}
