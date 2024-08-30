#' Convert LabelMe Annotations to COCO Format
#' 
#' @description 
#'   Reads LabelMe JSON files from an input directory and converts them to the COCO format.
#' @param input_dir  The path to the input directory containing LabelMe JSON files.
#' @param regexp     An optional regular expression pattern to filter files. Defaults to "".
#' @param labelmes   labelme data read by read_labelmes()
#' @return         A list containing three elements:
#'   * **annotations:** A data frame of COCO annotations.
#'   * **images:** A data frame of COCO images.
#'   * **categories:** A data frame of COCO categories.
#' @name labelme2coco
#' @examples
#' \dontrun{
#'   labelme_dir <- "path/to/your/labelme/annotations"
#'   coco_data <- labelme2coco(labelme_dir)
#' }
#' 
#' @export
labelme2coco <- function(input_dir, regexp = ""){
  labelmes <- read_labelmes(input_dir, regexp)
  annotations <- extract_annotations(labelmes)
  images      <- extract_images(labelmes)
  categories  <- extract_categories(labelmes)
  return(
    list(annotations = annotations,
         images = images,
         categories = categories))
}

#' Helper function for labelme2coco()
#' @rdname labelme2coco
#' @export
read_labelmes <- function(input_dir, regexp){
  labelmes <- 
    fs::dir_ls(input_dir, regexp = regexp) |>
    purrr::map(read_labelme) |>
    purrr::map(tibble::tibble) |>
    dplyr::bind_rows() |>
    dplyr::mutate(image_id    = image_path |> forcats::fct_inorder() |> as.integer()) |>
    dplyr::mutate(category_id = label      |> forcats::fct_inorder() |> as.integer())
  return(labelmes)
}

#' Helper function for labelme2coco()
#' @rdname labelme2coco
extract_images <- function(labelmes){
  images <- 
    labelmes |>
    dplyr::distinct(image_id, image_path, image_height, image_width) |>
    dplyr::rename(id = image_id, file_name = image_path)
  return(images)
}

#' Helper function for labelme2coco()
#' @rdname labelme2coco
extract_categories <- function(labelmes){
  categories <- 
    labelmes |>
    dplyr::distinct(category_id, label) |>
    dplyr::select(id = category_id, name = label, supercategory = label)
  return(categories)
}

#' Helper function for labelme2coco()
#' @rdname labelme2coco
extract_annotations <- function(labelmes){
  annotations <- 
    labelmes |>
    dplyr::distinct(points, image_id, category_id) |>
    dplyr::transmute(image_id, category_id,
                     bbox = purrr::map(points, points2bbox),
                     area = purrr::map_dbl(bbox, bbox2area),
                     iscrowd = 0) |>
    tibble::rowid_to_column("id")
  return(annotations)
}


#' Helper function for labelme2coco() and extract_images()
#' 
#' @description Calculates the area of a bounding box given its coordinates.
#' @param bbox A numeric vector of length 4 representing the bounding box coordinates (x1, y1, width, height).
#' @return The area of the bounding box.
#' @rdname labelme2coco
bbox2area <- function(bbox){
  bbox[3] * bbox[4]
}

#' Helper function for labelme2coco() and extract_images()
#' 
#' @description Converts a matrix of points to a bounding box.
#' @param points A matrix of points.
#' @return A numeric vector of length 4 representing the bounding box coordinates (x1, y1, width, height).
#' @rdname labelme2coco
points2bbox <- function(points){
  c(points[1, 1], 
    points[1, 2],
    points[2, 1] - points[1, 1],
    points[2, 2] - points[1, 2])
}

#' Read a LabelMe JSON File
#' 
#' @description Reads a LabelMe JSON file and extracts relevant information.
#' @param path_json The path to the LabelMe JSON file.
#' @return A list containing the LabelMe data.
#' @export
read_labelme <- function(path_json){
  json <- jsonlite::fromJSON(path_json)
  labelme <- json$shapes
  labelme$image_path <- json$imagePath
  labelme$image_height <- json$imageHeight
  labelme$image_width <- json$imageWidth
  return(labelme)
}

#' Read a COCO JSON File
#' 
#' @description Reads a COCO JSON file and extracts relevant information.
#' @param path_json The path to the COCO JSON file.
#' @return A list containing the COCO data.
#' @export
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
