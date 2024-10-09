#
library(devCEQ)
library(tidyverse)
library(haven)
library(diffdf)

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)


fldr_app <- "."    # Place where the app data is.

# Loading inputs
inp_xlsx <- devCEQ::load_input_xlsx(file.path(fldr_app, "./data-app/geo_inputs.xlsx"))
inps_app <- inp_xlsx %>% filter(!is.na(inputId)) %>% filter(include) %>% get_all_inps()
inps_all <- inp_xlsx %>% get_all_inps()
inps <- add_missing_inp_generic(inps_app, inps_all)

inp_str_test(inp_xlsx)

safe_inp <- devCEQ::make_get_inp_fn(actual_inps = inps)



test_gen_inp_front_simple(
  inp_raw_str = inp_xlsx, inp_tab_str = , inp_table_str = , n_choices = 2)

# devCEQ::test_gen_inp_front_tabs_file("./data-app/geo_inputs.xlsx")


# Testing tables


inputs_path <- "./data-app/geo_inputs.xlsx"
inputs_str <- load_input_xlsx(inputs_path)
inputs_tab_str <- load_inputtabs_xlsx(inputs_path )
inputs_table_str <- load_inputtables_xlsx(inputs_path)


# test_gen_inp_front_simple(
#    inp_raw_str = inp_xlsx, inp_tab_str = NULL, inp_table_str = inputs_table_str, n_choices = 2)


test_gen_inp_front_tabs(
  inp_raw_str = inp_xlsx, inp_tab_str = inputs_tab_str,
  inp_table_str = inputs_table_str, n_choices = 2)

