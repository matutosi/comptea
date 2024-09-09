#' Add group numbers to located text based on object classes
#'
#' This function adds group numbers to located text based on their 
#'      object classes and spatial arrangement. 
#'      It joins the located text data with object definitions, 
#'      removes unnecessary columns, arranges the data by x and y 
#'      coordinates, and calculates row and column numbers using 
#'      grouping functions.
#'
#' @param located_text A data frame containing located text data with 
#'                     columns `obj_class`, `xmin`, and `ymin`.
#' @param obj_df       A data frame containing object definitions with 
#'                     columns `obj_class`, `xmax`, and `ymax`.
#' @return             A data frame containing the original located 
#'                     text data with additional columns `row_no` and `col_no` representing the group numbers.
#'
#' @examples
#' # Example usage:
#' # Assuming you have located text data and object definitions
#' located_text <- data.frame(obj_class = c("text1", "text2", "text3"), xmin = c(10, 20, 30), ymin = c(10, 15, 10))
#' obj_df <- data.frame(obj_class = c("text1", "text2", "text3"), xmax = c(20, 30, 40), ymax = c(20, 20, 20))
#' add_group(located_text, obj_df)
#' 
#' memo
#' ojb_class-col_no: 説明
#' 5        -1     : scientific name
#' 1        -2     : japanese name
#' 2        -3     : layer
#' plot: 6-4
#'   6始まりはプロット，col_noは4から開始する(1-3は学名・和名・階層)
#' 
#' @export
add_group <- function(located_text, obj_df){
  df <- 
    located_text |>
    dplyr::left_join(obj_df, by = "obj_class") |>
    dplyr::select(-xmax, -ymax) |> 
    dplyr::arrange(xmin, ymin) |> 
    dplyr::mutate(row_no = group_no(ymin)) |> 
    dplyr::mutate(col_no = group_no(xmin))
  return(df)
}


#'
#'
#'
#'
#'
#'
#' ピボットで整形
comp_df <- function(df){
  df <- 
    df |>
  #     tidyr::pivot_wider(id_cols = row_no, 
    tidyr::pivot_wider(id_cols = c(row_no, ymin),
                       names_from = c(object, col_no, xmin), 
                       values_from = label,
                       names_sep = "-") |> 
    tidyr::pivot_longer(cols = starts_with("plot"), 
                        names_to = "plot", 
                        values_to = "value")
  return(df)
}


#'
#'
#'
#'
#'
#'
correct_layers <- function(located_text){
  class_layer <- 2
  not_layer <- dplyr::filter(located_text, obj_class != class_layer)
  layer <- 
    located_text |>
    dplyr::filter(obj_class == class_layer) |>
    dplyr::mutate(suggest = correct_layer(label)) |>
    check_modify()
  corrected <- dplyr::bind_rows(layer, not_layer)
  return(corrected)
}

#'
#'
#'
#'
#'
#'
correct_layer <- function(text){
  corrected <- 
    text |>
    stringr::str_replace_all("[^TSK]", ";") |>
    stringr::str_replace_all(";+", ";") |>
    stringr::str_remove("^;") |>
    stringr::str_remove(";$")
  return(corrected)
}

#'
#'
#'
#'
#'
#'
correct_values <- function(located_text){
  class_value <- 6
  not_value <- dplyr::filter(located_text, obj_class != class_value)
  value <- 
    located_text |>
    dplyr::filter(obj_class == class_value) |>
    dplyr::mutate(suggest = correct_value(label)) |>
    check_modify()
  corrected <- dplyr::bind_rows(value, not_value)
  return(corrected)
}

#'
#'
#'
#'
#'
#'
check_modify <- function(df){
  df <- 
    df |>
    dplyr::mutate(status = 
      dplyr::case_when(
        label != suggest ~ "modified",
        .default = ""
      )
    )
  return(df)
}

#'
#'
#'
#'
#'
#'
correct_value <- function(text){
  corrected <- 
    text |>
    stringr::str_remove_all(" ") |>  
    stringr::str_remove("[|)\\]]$") |>
    stringr::str_remove("^[|(\\[]") |>
    stringr::str_replace("[e0.°]", "-") |>
    stringr::str_replace("^([1-4+])\\+?([1-4])$", "\\1-\\2") |>
    stringr::str_replace_all("[lj]", "1") |>
    stringr::str_remove_all("[6-9A-z_,]") |>
    stringr::str_remove("^-$")
  return(corrected)
}


#'
#'
#'
#'
#'
#'
correct_snames <- function(located_text){
  class_sname <- 5
  not_sname <- dplyr::filter(located_text, obj_class != class_sname)
  sname <- 
    dplyr::filter(located_text, obj_class == class_sname) |>
    dplyr::select(-c(suggest, status))
  corrected <- correct_sname(sname$label)
  sname <- 
    dplyr::left_join(sname, corrected,
                     by = dplyr::join_by(label == species))
  corrected <- dplyr::bind_rows(sname, not_sname)
  return(corrected)
}

correct_sname <- function(species, reference = NULL, len = 1, min_dist = 3, n = 1){
  species <- species[species != ""]
  if(is.null(reference)){
    reference <- unique(wameicheckr::ref_sc$name_sc)
  }
  # species in reference
  as_is <- intersect(species, reference)
  as_is <- tibble::tibble(species = as_is, suggest = as_is, status = "ok") 
  # update species
  species <- setdiff(species, as_is$species)
  # species that match forward to reference
  match_fw <- match_forward(species, reference)
  # update species
  species <- setdiff(species, match_fw$species)
  # species suggested by reference
  suggest <- suggest_species(species, reference = reference, min_dist = min_dist, len = len, n = n)
  # update species
  species <- setdiff(species, suggest$species)
  # species without match
  not_match <- tibble::tibble(species = species, suggest = "", status = "should_check")
  # bind all results
  dplyr::bind_rows(as_is, match_fw, suggest, not_match)
}


#'
#'
#'
#'
#'
#'
match_forward <- function(species, reference = NULL){
  if(is.null(reference)){
    reference <- unique(wameicheckr::ref_sc$name_sc)
  }
  species_matched <- 
    species |>
    purrr::map_lgl(\(x) stringr::str_starts(reference, x) |> any()) |>
    `[`(x = species, i = _)
  reference_matched <-
    paste0("^", species_matched, ".+") |>
    purrr::map(\(x) stringr::str_subset(reference, x)) |>
    unlist()
  match_fw <- suggest_species(species_matched, reference_matched, min_dist = 20, len = 1, n = 1)
  return(match_fw)
}

#'
#'
#'
#'
#'
#'
suggest_species <- function(species, reference = NULL, min_dist = 3, len = 1, n = 1){
  if(is.null(reference)){
    reference <- unique(wameicheckr::ref_sc$name_sc)
  }
  suggest <- 
    species |>
    wameicheckr::editdist_multi(reference, len = len) |>
    dplyr::filter(editdist <= min_dist) |>
    dplyr::slice_min(editdist, n = n, by = s1) |>
    dplyr::summarise(s2 = purrr::reduce(s2, paste, sep = ", "), .by = s1) |>
    `colnames<-`(c("species", "suggest")) |>
    dplyr::mutate(status = "modified")
  return(suggest)
}
