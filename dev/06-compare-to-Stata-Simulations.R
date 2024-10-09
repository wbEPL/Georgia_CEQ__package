library(devCEQ)
library(tidyverse)
library(haven)
library(diffdf)

Stata_data_path <- "C:\\Users\\laure\\OneDrive\\WB\\github_repos\\different-simulations-geoCEQ\\1.Data\\04_results\\GEO22_CEQ_Mar2024.dta"

Stata_final <- file.path(Stata_data_path) %>%
  normalizePath() %>%
  read_dta() %>%
  haven::zap_labels()

stripAttributes <- function(data) {
  attr <- names(attributes(data))
  good <- c("names", "row.names", "class")
  for (i in attr[!attr %in% good]) {
    attr(data, i) <- NULL
  }
  return(data)
}

# Function to apply the stripping to each column of a dataframe
applyStripAttributesToDataFrame <- function(df) {
  df[] <- lapply(df, stripAttributes)
  return(df)
}

Stata_fina_attributes_dropped <- applyStripAttributesToDataFrame(Stata_final)

source("R/georgia_ceq_sim.R")
source("R/fct_simulation.R")


# Folders
fldr_app <- "."    # Place where the app data is.
fldr <- "..\\geoCEQ"

# Loading inputs
inp_xlsx <- devCEQ::load_input_xlsx(file.path(fldr_app, "./data-app/geo_inputs.xlsx"))
inps_app <- inp_xlsx %>% filter(!is.na(inputId)) %>% filter(include) %>% get_all_inps()
inps_all <- inp_xlsx %>% get_all_inps()
inps <- add_missing_inp_generic(inps_app, inps_all)


# manually overide numbers to some other figure to test if simulation worked
inps$pitw_rate   =  0.4
inps$pits_rate   =  0.07
inps$pitk_rate   =  0.10
inps$sscw_rate   =  0.05
inps$ssce_rate   =  0.08
inps$sscs_rate   =  0.01
inps$sscg1_rate  =  0.025
inps$sscg2_rate  =  0.1
inps$sscg1_limit = 64000
inps$sscg2_limit = 90000
inps$ptax_thr1  =  10000
inps$ptax_thr2  = 200000
inps$ptax_r1l = 0.0015
inps$ptax_r1u = 0.0027
inps$ptax_r2l = 0.006
inps$ptax_r2u = 0.013
inps$trland_arab_dedoplistskaro	= 67
inps$trland_arab_telavi			= 89
inps$trland_arab_KvareliGori		= 80
inps$trland_arab_BolsiniGardabani	= 99
inps$trland_arab_rustavi			= 91
inps$trland_arab_lanchkhuti		= 65
inps$trland_arab_AbashSenakiKhobi  = (70+76)/2
inps$trland_arab_kutaisi			= 65
inps$trland_arab_ZestaponiVani	= 74
inps$trland_past_dedoplistskaro	= 20+9
inps$trland_past_TelaviKvareli	= 20+26
inps$trland_past_kutaisi			= 23+16
inps$trland_past_ZestaponiVani	= 20+15
inps$trland_past_adminterr1		= 28+16
inps$GFK  =  0.91
inps$vat_rate = (0.18/1.70)
inps$exc_rate_mobile = 0.09
inps$exc_rate_beer   = 0.64
inps$exc_rate_vodka  = 4
inps$exc_rate_wine   = 1
inps$exc_rate_brandy      = 9.9
inps$exc_rate_tobfil      = 0.91
inps$exc_rate_tobunf      = 0.76
inps$exc_rate_fuel_liquid = 0.76
inps$exc_rate_fuel_gas    = 0.40
inps$sub_rate_elec  = 0.18
inps$sub_price_elec = 0.85
inps$sub_rate_gas  = 10.55
inps$sub_price_gas = 55.66
inps$sub_upbound_wat = 30
inps$sub_rate_trans  = 0.45
inps$sub_price_trans = 0.1
inps$health_ben_2022_1 = 700
inps$health_ben_2022_2 =533
inps$health_ben_2022_3 =40.0
inps$health_ben_2022_4 =27
inps$health_ben_2022_5 =910
inps$health_ben_2022_6 =650
inps$health_ben_2022_7 =134
inps$health_ben_2022_8 =500
inps$health_ben_2022_9 =2



presim <- readr::read_rds("data-app/presim.rds")

R_simulation <- full_ceq(inps = inps, presim = presim)

R_simulation_attributes_dropped <- applyStripAttributesToDataFrame(R_simulation)


differences <- diffdf(
  base =
    R_simulation_attributes_dropped %>%
    as.data.frame(),
  compare = Stata_fina_attributes_dropped %>%
    as.data.frame(),
  tolerance = 0.0001
)
differences



# change globals in different filea to:
# global pitw_rate   =  0.4  // personal income tax rate wage earners
# global pits_rate   =  0.07 // personal income tax rate self-employed
# global pitk_rate   =  0.10 // personal income tax rate capital income
# global sscw_rate   =  0.05 // pension contributions employee
# global ssce_rate   =  0.08 // pension contributions employer
# global sscs_rate   =  0.01 // pension contributions self-employed
#
# global sscg1_rate  =  0.025 // pension contributions government threshold 1
# global sscg2_rate  =  0.1 // pension contributions government threshold 2
#
# global sscg1_limit = 64000 // pension contributions threshold 1
# global sscg2_limit = 90000 // pension contributions threshold 2
# // Property Tax.
# global ptax_thr1  =  10000 // Threshold 1
# global ptax_thr2  = 200000 // Threshold 2
#
# global ptax_r1l = 0.0015   // Rate threshold 1 lower bound
# global ptax_r1u = 0.0027   // Rate threshold 1 upper bound
# global ptax_r2l = 0.006   // Rate threshold 2 lower bound
# global ptax_r2u = 0.013   // Rate threshold 2 upper bound
# // Land Tax
#
# // Article 204 Property tax rate on land
# global trland_arab_dedoplistskaro	= 67
# global trland_arab_telavi			= 89
# global trland_arab_KvareliGori		= 80
# global trland_arab_BolsiniGardabani	= 99
# global trland_arab_rustavi			= 91
# global trland_arab_lanchkhuti		= 65
# global trland_arab_AbashSenakiKhobi  = (70+76)/2
# global trland_arab_kutaisi			= 65
# global trland_arab_ZestaponiVani	= 74
#
# global trland_past_dedoplistskaro	= 20+9
# global trland_past_TelaviKvareli	= 20+26
# global trland_past_kutaisi			= 23+16
# global trland_past_ZestaponiVani	= 20+15
# global trland_past_adminterr1		= 28+16
# scalar GFK  =  0.91 // gross up factor capital income
# global vat_rate = (0.18/1.70)  // not 0.18
# global exc_rate_mobile = 0.09
# global exc_rate_beer   = 0.64
# global exc_rate_vodka  = 4
# global exc_rate_wine   = 1
# global exc_rate_brandy      = 9.9
# global exc_rate_tobfil      = 0.91
# global exc_rate_tobunf      = 0.76
# global exc_rate_fuel_liquid = 0.76
# global exc_rate_fuel_gas    = 0.40
# global sub_rate_elec  = 0.18
# global sub_price_elec = 0.85
# global sub_rate_gas  = 10.55
# global sub_price_gas = 55.66
# global sub_upbound_wat = 30
# global sub_rate_trans  = 0.45
# global sub_price_trans = 0.1
# mat health_ben_2022  = (700,	533,	40.0,	27,	910, 650,	134,	500,	2)
differences
