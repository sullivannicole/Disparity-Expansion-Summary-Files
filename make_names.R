#' File naming
#'
#' Creates names for files saved in memory
#'
#' @param name_vector character vector of file names, usually produced by list.files()
#'
#' @return Character vector of length 1 by which to name the file
#' 
#' @examples
#' list2env(purrr::map(set_names(file_vector_from_list.files, make_names(file_vector_from_list.files), 
#'                     read_csv), 
#'          envir = .GlobalEnv))
#'
#' @export
make_names <- function(name_vector) {
  
  name_vector_df <- as.data.frame(name_vector)
  
  new_names <- name_vector_df %>%
    rename(df_name = 1) %>%
    separate(df_name, into = c("df_name", "drop"), sep = 7) %>%
    dplyr::select(-drop)
  
  new_names_list <- as.list(new_names)
  
  return(new_names_list$df_name)
}