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

#' Summarise by datetime and add mean, median and SD of numeric values
#'
#' @param data
#'
#' @returns a dateframe

summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )

  return(summarised_data)
}

#' Tidy survey date
#'
#' @param data
#'
#' @returns data frame
tidy_survey_dates <- function(data){
  tidied <- data |>
    dplyr::mutate(date = lubridate::mdy(date),
                  start_datetime = lubridate::as_datetime(paste(date, start_time)),
                  end_datetime = lubridate::as_datetime(paste(date, end_time)),
                  datetime_id = start_datetime,
                  .before = start_time) |>
    dplyr::select(-c(date, start_time, end_time, duration)
                  )
  return(tidied)

}

#' Pivot to long
#'
#' @param data
#'
#' @returns data frame

survey_to_long <- function(data) {
  longer <- data |>
    dplyr::select(id, datetime_id, start_datetime, end_datetime) |>
    tidyr::pivot_longer(c(start_datetime, end_datetime),
                        names_to = NULL,
                        values_to = "collection_datetime"
    ) |>
    dplyr::group_by(pick(-collection_datetime)) |>
    tidyr::complete(collection_datetime = seq(min(collection_datetime), max(collection_datetime),
                                              by = 60
    )) |>
    dplyr::ungroup()
  return(longer)
}
