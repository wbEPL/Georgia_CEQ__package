# This is a pre-simulation step, where we mimic stata code and load
library(devCEQ)
library(tidyverse)
library(haven)
library(diffdf)

# Folders
fldr_app <- "."    # Place where the app data is.
fldr <- "..\\geoCEQ"

# Stata - related folders
fldr_raw <- file.path(fldr, "1.Data\\01_raw\\") |> normalizePath()
fldr_presim <- file.path(fldr, "1.Data\\02_presim\\") |> normalizePath()
fldr_dbmod <- file.path(fldr, "1.Data\\03_dbmod\\") |> normalizePath()
fldr_res <- file.path(fldr, "1.Data\\04_results\\") |> normalizePath()

# Loading inputs
inp_xlsx <- devCEQ::load_input_xlsx(file.path(fldr_app, "./data-app/geo_inputs.xlsx"))
inps_app <- inp_xlsx %>% filter(!is.na(inputId)) %>% filter(include) %>% get_all_inps()
inps_all <- inp_xlsx %>% get_all_inps()
inps <- add_missing_inp_generic(inps_app, inps_all)

# Temp and presim data -----------------------------------------------------------
presim <- list()

#    02_GEO21_DirectTransfers.do


presim$dir_transfers <-
  file.path(fldr_presim, "dir_transfers.dta") %>%
  read_dta() %>%
  haven::zap_labels()


presim$marketincome <-
  file.path(fldr_presim, "marketincome.dta") %>%
  read_dta() %>%
  haven::zap_labels()

presim$sysschedule_mod <-
  file.path(fldr_presim, "00_sysschedule_mod.dta") %>%
  read_dta() %>%
  haven::zap_labels()


presim$familysize_mod <-
  file.path(fldr_presim, "00_familysize_mod.dta") %>%
  read_dta() %>%
  haven::zap_labels()


presim$dtx <-
  file.path(fldr_presim, "dtx.dta") %>%
  read_dta() %>%
  haven::zap_labels()

presim$aux_laborincome_for_health <-
  file.path(fldr_presim, "01_aux_laborincome_for_health.dta") %>%
  read_dta() %>%
  haven::zap_labels()

presim$mi_capitalincome <-
  file.path(fldr_presim, "00_mi_capitalincome.dta") %>%
  read_dta() %>%
  haven::zap_labels()

house_mktvalue <-
  file.path(fldr_presim, "house_mktvalue.dta")%>%
  read_dta() %>%
  haven::zap_labels()


incomes_raw <-
  file.path(fldr_raw, "tblincomes.dta") %>%
  read_dta() %>%
  haven::zap_labels() %>%
  rename_with(tolower)%>%
  arrange(uid)

anti_join(house_mktvalue,
          incomes_raw,
          by = join_by(uid)) %>%
  nrow() == 0


presim$house_mktvalue <- incomes_raw %>%
  left_join(house_mktvalue, by = join_by(uid))



presim$land <-
  file.path(fldr_presim, "land.dta") %>%
  read_dta() %>%
  haven::zap_labels()


presim$consumption<-
  file.path(fldr_presim, "consumption.dta") %>%
  read_dta() %>%
  haven::zap_labels()

presim$conssub_electricity <-
  file.path(fldr_presim, "conssub.dta") %>%
  read_dta() %>%
  haven::zap_labels()  %>%
  filter(tableno == 14 & itemno == 1) %>%  # Keep rows where item == 1 and tableno == 14
  rename(
    qconsumed = raodenoba,  # Rename raodenoba to qconsumed
    cons_elec = value,  # Rename value to cons_elec
    cons_elec_free = raodenobaufaso  # Rename raodenobaufaso to cons_elec_free
  ) %>%
  mutate(
    connect_elec = s_q7 > 0 & !is.na(s_q7)  # Generate new variable, s_q7>0 and not NA
  ) %>%
  select(-qconsumed)


presim$conssub_gas <-
  file.path(fldr_presim, "conssub.dta")%>%
  read_dta() %>%
  haven::zap_labels()  %>%
  filter(itemno == 2 & tableno == 14) %>%
  mutate(month = 0) %>%
  mutate(monthyear = as.factor(monthyear)) %>%
  arrange(monthyear, uid) %>%
  group_by(monthyear) %>%
  mutate(month = cur_group_id())%>%
  group_by(uid) %>%
  mutate(hhrepeat = row_number())%>%
  mutate(
    cons_dummy = as.integer(value > 0 & !is.na(value)),
    free_dummy = as.integer(raodenobaufaso > 0 & !is.na(raodenobaufaso))
  ) %>%
  mutate(cons_gas = value,
         cons_gas_free = raodenobaufaso) %>%
  ungroup()

presim$conssub_water <-
  file.path(fldr_presim, "conssub.dta")%>%
  read_dta() %>%
  haven::zap_labels()  %>%
  filter(itemno == 3 & tableno == 14) %>%
  filter(!is.na(value))  %>%
  group_by(uid) %>%
  summarise(
    cons_wat = sum(value),
    cons_wat_free = sum(raodenobaufaso),
    watersource = mean(watersource, na.rm = TRUE),
    weights = mean(weights, na.rm = TRUE),
    regno = mean(regno, na.rm = TRUE),
    urban = mean(urbanorrural, na.rm = TRUE),
    quartno = mean(quartno, na.rm = TRUE),
    familysize = mean(familysize, na.rm = TRUE),
    tsa_tra_dummy = mean(tsa_tra_dummy, na.rm = TRUE)
  ) %>%
  mutate(freewat_dummy = as.integer(cons_wat_free > 0)) %>%
  ungroup()



presim$conssub_transport <-
  file.path(fldr_presim, "subtransport.dta")%>%
  read_dta() %>%
  haven::zap_labels()



presim$hltgroups <-
  file.path(fldr_presim, "hltgroups.dta")%>%
  read_dta() %>%
  haven::zap_labels()



presim$outofpocket <-
  file.path(fldr_presim, "outofpocket.dta")%>%
  read_dta() %>%
  haven::zap_labels()

presim$education_hh <-
  file.path(fldr_presim, "education_hh.dta")%>%
  read_dta() %>%
  haven::zap_labels()

presim$poverty <-
  file.path(fldr_raw, "POVERTY.dta") %>%
  read_dta() %>%
  haven::zap_labels()  %>%
  rename_with(tolower)

presim$out <- file.path(fldr_res, "GEO22_CEQ_Mar2024.dta")  %>%
  read_dta() %>%
  haven::zap_labels()




# GEO22_CEQ_Mar2024.dta

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Re-saving pre-sim data --------------------------------------------------


#resim$out <- full_ceq(inps, presim)

presim %>% write_rds(file.path(fldr_app, "data-app", "presim.rds"), compress = "gz")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

