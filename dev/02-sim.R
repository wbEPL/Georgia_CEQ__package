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
fldr_res <- file.path(fldr, "1.Data\\04_results\\") |> normalizePath()

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

# # Loading pre-sim data
presim <- read_rds("data-app/presim.rds")

# Starting the simulation -------------------------------------------------

sim <- list()


# 02_GEO22_DirectTaxesContrib.do------------------------------------------------------

sim <- func_1_direct_transfers(sim = list(), inps = inps, presim= presim)

# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_di_govtransfers.dta"))
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
capitalincome_final_attributes_dropped <- applyStripAttributesToDataFrame(sim$di_govtransfers )


differences <- diffdf(base =capitalincome_final_attributes_dropped %>% as.data.frame() ,
                     compare = from_stata_summarized_dropped%>% as.data.frame(),
                     tolerance = 0.00001)

differences
#differences |> get_diffdf_sum()

# profvis::profvis({func_1_direct_transfers(sim, inps, presim)})









# 02. Income Tax.do ------------------------------------------------------

sim <- func_2_direct_tax(sim, inps, presim)

# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_mi_laborincome.dta"))
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
laborincome_final_attributes_dropped <- applyStripAttributesToDataFrame(sim$laborincome)


differences <- diffdf(base =laborincome_final_attributes_dropped %>% as.data.frame() ,
                     compare = from_stata_summarized_dropped%>% as.data.frame(),
                     tolerance = 0.001)

differences #here there are stoll small differences due to rounding
#differences |> get_diffdf_sum()

# profvis::profvis({func_2_direct_tax(sim, inps, presim)})



# 03.  capital taxes------------------------------------------------------


sim <- func_3_capital_tax(sim, inps, presim)

# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_mi_capitalincome.dta"))
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
capitalincome_final_attributes_dropped <- applyStripAttributesToDataFrame(sim$mi_capitalincome)


differences <- diffdf(base =capitalincome_final_attributes_dropped %>% as.data.frame() ,
                     compare = from_stata_summarized_dropped%>% as.data.frame(),
                     tolerance = 0.00001)

differences
#differences |> get_diffdf_sum()

# profvis::profvis({func_1_direct_transfers(sim, inps, presim)})




# 04.  real estate taxes------------------------------------------------------


sim <- func_4_realestate(sim, inps, presim)

# # Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_realetax.dta"))
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
realestate_tax_attributes_dropped <- applyStripAttributesToDataFrame(sim$realestate_tax)


differences <- diffdf(base =realestate_tax_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped%>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

# profvis::profvis({func_4_realestate(sim, inps, presim)})

sim <- func_5_landtax(sim, inps, presim)



# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_landtax.dta")) %>%
  zap_labels()
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
land_tax_attributes_dropped <- applyStripAttributesToDataFrame(sim$landtax)


differences <- diffdf(base = land_tax_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped%>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

# profvis::profvis({func_5_landtax(sim, inps, presim)})



sim <- func_6_indtaxes(sim, inps, presim)


# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_ci_consumption.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
consumption_attributes_dropped <- applyStripAttributesToDataFrame(sim$ci_consumption %>%
                                                                    arrange(uid ))


differences <- diffdf(base = consumption_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped%>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_6_indtaxes(sim, inps, presim)})
#
#


sim <- func_7_subs_elec(sim, inps, presim)



# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_ci_01subs_elec.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
ci_01subs_elecattributes_dropped <- applyStripAttributesToDataFrame(sim$ci_01subs_elec %>%
                                                                    arrange(uid ))


differences <- diffdf(base = ci_01subs_elecattributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_7_subs_elec(sim, inps, presim)})


sim <- func_8_subs_gas(sim, inps, presim)


# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_ci_02subs_gas.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
ci_02subs_gas_attributes_dropped <- applyStripAttributesToDataFrame(sim$ci_02subs_gas %>%
                                                                    arrange(uid ))


differences <- diffdf(base = ci_02subs_gas_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_8_subs_gas(sim, inps, presim)})




sim <- func_9_conssub_water(sim, inps, presim)


# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_ci_03subs_water.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
conssub_water_attributes_dropped <- applyStripAttributesToDataFrame(sim$ci_03subs_water %>%
                                                                    arrange(uid ))


differences <- diffdf(base = conssub_water_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.000001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_9_conssub_water(sim, inps, presim)})


sim <- func_10_conssub_transport(sim, inps, presim)

# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_ci_04subs_transp.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
ci_04subs_transp_attributes_dropped <- applyStripAttributesToDataFrame(sim$ci_04subs_transp  %>%
                                                                    arrange(uid ))


differences <- diffdf(base = ci_04subs_transp_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_10_conssub_transport(sim, inps, presim)})


sim <- func_11_all_subsidies(sim = sim)

# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


from_stata_summarized <- read_dta(file.path(fldr_temp, "01_ci_indir_subs.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
ci_indir_subs_attributes_dropped <- applyStripAttributesToDataFrame(sim$ci_indir_subs  %>%
                                                                    arrange(uid ))


differences <- diffdf(base = ci_indir_subs_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_11_all_subsidies(sim)})

sim <- func_12_health(sim, inps, presim)

# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_inkind_health.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
inkind_health_attributes_dropped <- applyStripAttributesToDataFrame(sim$inkind_health  %>%
                                                                    arrange(uid ))


differences <- diffdf(base = inkind_health_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_12_health(sim, inps, presim)})

sim <- func_13_education(sim, inps, presim)


# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_temp, "01_inkind_educ.dta")) %>%
  zap_labels() %>%
  arrange(uid )
from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
inkind_educ_attributes_dropped <- applyStripAttributesToDataFrame(sim$inkind_educ  %>%
                                                                    arrange(uid ))


differences <- diffdf(base = inkind_educ_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.00001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_13_education(sim, inps, presim)})
#


presim <- read_rds("data-app/presim.rds")
sen_out <- full_ceq(inps, presim)

# Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

from_stata_summarized <- read_dta(file.path(fldr_res, "GEO22_CEQ_Mar2024.dta")) %>%
  zap_labels() %>%
  arrange(hhid )
 from_stata_summarized_dropped <- applyStripAttributesToDataFrame(from_stata_summarized)
 sen_out_attributes_dropped <- applyStripAttributesToDataFrame(sen_out %>%
                                                                    arrange(hhid ))



differences <- diffdf(base = sen_out_attributes_dropped %>% as.data.frame() ,
                      compare = from_stata_summarized_dropped %>% as.data.frame(),
                      tolerance = 0.001)

differences
differences |> get_diffdf_sum()

#profvis::profvis({func_13_education(sim, inps, presim)})
#




# Complete CEQ ------------------------------------------------------------

presim <- read_rds("data-app/presim.rds")
sen_out <- full_ceq(inps, presim)
sen_out %>% write_rds("data-app/baseline.rds", compress = "xz")
# # Check
# output_stata <-
#   file.path(fldr_temp, "..", "4_sim_output", "output_ref.dta") %>%
#   haven::read_dta() %>%
#   haven::zap_labels() %>% haven::zap_label() %>% haven::zap_formats()
#
# out_diff <-
#   sen_out %>%
#   mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
#   diffdf(output_stata, tolerance = 0.01, keys = c("hhid"))
#
# out_diff %>% get_diffdf_sum() %>% print(n = 50)
# out_diff$VarDiff_trimf

