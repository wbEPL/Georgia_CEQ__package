# mypackage/R/myscript.R

#' @import dplyr
NULL

my_function <- function(data) {
  data %>%
    mutate(new_col = 1) %>%
    select(new_col)
}
