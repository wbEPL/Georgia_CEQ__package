.onAttach <- function(libname, pkgname) {
  # Load devCEQ when mypackage is loaded
  packageStartupMessage("Loading devCEQ along with mypackage.")
  library("devCEQ", character.only = TRUE)
  packageStartupMessage("Loading dplyr along with mypackage.")
  library("dplyr", character.only = TRUE)
  packageStartupMessage("Loading shiny along with mypackage.")
  library("shiny", character.only = TRUE)
  packageStartupMessage("Loading plotly along with mypackage.")
  library("plotly", character.only = TRUE)
  packageStartupMessage("Loading DiagrammeR along with mypackage.")
  library("DiagrammeR", character.only = TRUE)
  packageStartupMessage("Loading DiagrammeRsvg along with mypackage.")
  library("DiagrammeRsvg", character.only = TRUE)
  packageStartupMessage("Loading rsvg along with mypackage.")
  library("rsvg", character.only = TRUE)
  packageStartupMessage("Loading purrr along with mypackage.")
  library("purrr", character.only = TRUE)
  packageStartupMessage("Loading stringr along with mypackage.")
  library("stringr", character.only = TRUE)
}



