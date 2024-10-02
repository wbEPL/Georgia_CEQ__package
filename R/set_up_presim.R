#' Set Up Presimulation Data as a Reactive Expression
#'
#' Reads presimulation and baseline data from specified file paths and returns a reactive expression containing the combined data.
#'
#' @param path_to_presim Character string. The file path to the presimulation `.rds` file.
#' @param path_to_baseline Character string. The file path to the baseline `.rds` file.
#'
#' @return A reactive expression that, when evaluated, returns a list containing the presimulation data with the baseline data included.
#' @export
#'
#' @examples
#' # Example usage within a Shiny app server function:
#' \dontrun{
#' presim_dta <- set_up_presim("data/presim.rds", "data/baseline.rds")
#' # Access the data by calling presim_dta()
#' data <- presim_dta()
#' }
set_up_presim <- function(path_to_presim, path_to_baseline) {

  # Check if the presim file exists
  if (!file.exists(path_to_presim)) {
    stop("The presimulation file does not exist at the specified path.")
  }

  # Check if the baseline file exists
  if (!file.exists(path_to_baseline)) {
    stop("The baseline file does not exist at the specified path.")
  }

  # Ensure that the shiny package is loaded
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required but is not installed.")
  }

  # Create and return a reactive expression
  shiny::reactive({
    # Read the presimulation data
    presim <- readr::read_rds(path_to_presim)

    # Read the baseline data and add it to the presim list
    presim$baseline <- readr::read_rds(path_to_baseline)

    # Return the combined presim data
    presim
  })
}
