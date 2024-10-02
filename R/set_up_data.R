#' Set up data file path
#'
#' This function returns the path to the `geo_inputs.xlsx` file included in the package.
#'
#' @return A character string with the path to the `geo_inputs.xlsx` file.
#' @import devCEQ
#' @export
set_up_data <- function() {
  input_file <- system.file("extdata", "geo_inputs.xlsx", package = "GeoappPackage")
  devCEQ::load_input_xlsx(input_file)
}


#' Load Input Structure
#'
#' This function loads the input structure from the `geo_inputs.xlsx` file included in the package.
#'
#' @return A list containing the input structure.
#' @export
#' @importFrom devCEQ load_input_xlsx
#'
#' @examples
#' inputs_str <- load_input_structure()
load_input_structure <- function() {
  # Get the path to the 'geo_inputs.xlsx' file within the package
  inputs_path <- system.file("extdata", "geo_inputs.xlsx", package = "GeoappPackage")

  # Load the input structure using the devCEQ package
  devCEQ::load_input_xlsx(inputs_path)
}


#' Load Input Tabs Structure
#'
#' This function loads the input tabs structure from the `geo_inputs.xlsx` file included in the package.
#'
#' @return A list containing the input tabs structure.
#' @export
#' @importFrom devCEQ load_inputtabs_xlsx
#'
#' @examples
#' inputs_tab_str <- load_input_tabs_structure()
load_input_tabs_structure <- function() {
  # Get the path to the 'geo_inputs.xlsx' file within the package
  inputs_path <- system.file("extdata", "geo_inputs.xlsx", package = "GeoappPackage")

  # Load the input tabs structure using the devCEQ package
  devCEQ::load_inputtabs_xlsx(inputs_path)
}

#' Load Input Tables Structure
#'
#' This function loads the input tables structure from the `geo_inputs.xlsx` file included in the package.
#'
#' @return A list containing the input tables structure.
#' @export
#' @importFrom devCEQ load_inputtables_xlsx
#'
#' @examples
#' inputs_table_str <- load_input_tables_structure()
load_input_tables_structure <- function() {
  # Get the path to the 'geo_inputs.xlsx' file within the package
  inputs_path <- system.file("extdata", "geo_inputs.xlsx", package = "GeoappPackage")

  # Load the input tables structure using the devCEQ package
  devCEQ::load_inputtables_xlsx(inputs_path)
}
