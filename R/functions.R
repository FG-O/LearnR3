
#' Read in one nurses' stress data file
#'
#' @param a path to a data file
#' @param maximum number of rows to read
#'
#' @returns outputs a dataframe / tibble

read <- function(file.path, max_rows = 100) {
  data <- file.path |>
    readr::read_csv( #readr lokaliserer hvilken pakke der anvendes
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  base::return(data)
}
