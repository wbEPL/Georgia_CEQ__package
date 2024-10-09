# This is a pre-simulation step, where we mimic stata code and load
#
library(devCEQ)
library(tidyverse)
library(haven)
library(diffdf)

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# Folders
fldr_app <- "."    # Place where the app data is.
fldr <- "..\\geoCEQ"

# Stata - related folders
#fldr_raw <- file.path(fldr, "01. Data\\1_raw\\") |> normalizePath()
fldr_presim <- file.path(fldr, "1.Data\\02_presim\\") |> normalizePath()
fldr_temp <- file.path(fldr, "1.Data\\03_dbmod\\") |> normalizePath()

# Loading inputs
inp_xlsx <- devCEQ::load_input_xlsx(file.path(fldr_app, "./data-app/geo_inputs.xlsx"))
inps_app <- inp_xlsx %>% filter(!is.na(inputId)) %>% filter(include) %>% get_all_inps()
inps_all <- inp_xlsx %>% get_all_inps()
inps <- add_missing_inp_generic(inps_app, inps_all)


get_inp <- devCEQ::make_get_inp_fn(actual_inps = inps)


# # Loading pre-sim data
presim <- read_rds("data-app/presim.rds")

dta_test <- full_ceq_test(inps = inps, presim = presim)
dta_test_2 <- full_ceq(inps = inps, presim = presim)

get_dta_pov(
  dta = dta_test,
  policy_name = "Policy",
  wt_var = get_wt_nm(),
  income_vars_tbl = get_inc_nm(),
  poverty_line_var = "pline_mod",
  # poverty_line_value = 100,
  para_names =
    tibble(
      parameter = c("rate", "headcount", "gap", "severity"),
      label = c(
        "Rate",
        "headcount",
        "gap",
        "severity"
      ) %>% factor(., levels = .)
    )
)

get_dta_pov(
  dta = dta_test_2,
  policy_name = "Policy2",
  wt_var = get_wt_nm(),
  income_vars_tbl = get_inc_nm(),
  poverty_line_var = "pline_mod",
  # poverty_line_value = 100,
  para_names =
    tibble(
      parameter = c("rate", "headcount", "gap", "severity"),
      label = c(
        "Rate",
        "headcount",
        "gap",
        "severity"
      ) %>% factor(., levels = .)
    )
)

