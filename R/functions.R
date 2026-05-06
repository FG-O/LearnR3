#' Read in one nurses' stress data file
#'
#' @param a path to a data file
#' @param maximum number of rows to read
#'
#' @returns outputs a dataframe / tibble

read <- function(file.path, max_rows = 100) {
  data <- file.path |>
    readr::read_csv( # readr lokaliserer hvilken pakke der anvendes
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  base::return(data)
}


#' Read in all files in nurses stress
#'
#' @param filename
#'
#' @returns outputs a dataframe

read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/") %>%
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files %>%
    purrr::map(read) %>%
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}


#' Get IDs from file_path_id
#'
#' @param data
#'
#' @returns outputs a new column with ID

get_participant_id <- function(data) {
  data <- data %>%
    dplyr::mutate(
      id = str_extract(
        file_path_id,
        pattern = "/stress/[:alnum:]{2}/"
      ) %>%
        stringr::str_remove("/stress/") %>%
        stringr::str_remove("/"),
      .before = file_path_id
    ) %>%
    dplyr::select(-file_path_id)
  return(data)
}
