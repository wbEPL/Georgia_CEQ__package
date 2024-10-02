#' Key variables names and labels
#'
#' 01. 02_GEO22_DirectTaxesContrib.do replication
#' @export
#' @noRd
#' @import dplyr
func_1_direct_transfers<- function(sim = list(), inps, presim) {

  # Define local variables
  transf <- c("disabil_pen", "lossbread_pen", "scholar_tra", "tempdis_tra", "insuran_tra",
              "tsa_tra", "veteran_tra", "sochh_tra", "idp_tra", "other_tra")

  df <- presim$dir_transfers

  benef_q <- c(inps$benef_q_1,
               inps$benef_q_2,
               inps$benef_q_3,
               inps$benef_q_4,
               inps$benef_q_5)

  if (inps$simu_tsa_benef == 1) {
    df <- df %>%
      left_join(
        presim$marketincome %>%
          mutate(quintile_m = ntile(ym_pc, 5)) %>%
          select(uid, quintile_m),
        by = "uid"
      ) %>%
      mutate(tsa_tra_dummy = benef_q[quintile_m])
  }

  if (inps$simu_tsa_value == 1) {
    df <- df %>%
      mutate(tsa_tra_u16 = tsa_tra_dummy * under16 * inps$tsa_under_16 * 12,
             tsa_tra_o16 = tsa_tra_dummy * older16 * inps$tsa_over_16 * 12) %>%
      mutate(tsa_tra =  rowSums(across(c(tsa_tra_u16, tsa_tra_o16)), na.rm = TRUE))

  } else if (inps$simu_tsa_value == 2) {
    df <- df %>%
      mutate(tsa_tra = tsa_tra * inps$simu_tsa_mult)
  }

  df <-  df %>%
    group_by(uid) %>%
    summarise(across(all_of(transf), sum, .names = "{col}"),
              under16 = sum(under16, na.rm = TRUE),
              older16 = sum(older16, na.rm = TRUE),
              disable = sum(disable, na.rm = TRUE),
              across(ends_with("_dummy"),  \(x) max(x, na.rm = TRUE), .names = "{col}"),
              across(ends_with("_count"),\(x) max(x, na.rm = TRUE), .names = "{col}")) %>%
    ungroup()%>%
    left_join(presim$sysschedule_mod, by = join_by(uid))%>%
    left_join(presim$familysize_mod, by = join_by(uid)) %>%

    # Create new columns by multiplying existing columns with familysize %>%
    mutate(across(all_of(transf), ~ get(paste0(cur_column(), "_dummy")) * familysize,
                  .names = "{.col}_pop")) %>%

    # Aggregate public transfers
    mutate(pubtrans_inc = rowSums(across(all_of(transf)), na.rm = TRUE),
           pubtrans_dummy = pmax(disabil_pen_dummy, lossbread_pen_dummy,
                                 scholar_tra_dummy, tempdis_tra_dummy, insuran_tra_dummy ,tsa_tra_dummy,
                                 veteran_tra_dummy, sochh_tra_dummy, idp_tra_dummy, other_tra_dummy),
           pubtrans_count = rowSums(across(c(disabil_pen_count, lossbread_pen_count,
                                             scholar_tra_count ,tempdis_tra_count, insuran_tra_count, tsa_tra_count,
                                             veteran_tra_count, sochh_tra_count, idp_tra_count,
                                             other_tra_count)), na.rm = TRUE),
           pubtrans_pop = pubtrans_dummy * familysize,
           pubtrans_inc_alt =  rowSums(across(all_of(transf)), na.rm = TRUE),
           pubtrans_alt_dummy = pmax(disabil_pen_dummy, lossbread_pen_dummy,
                                     scholar_tra_dummy, tempdis_tra_dummy,
                                     insuran_tra_dummy, tsa_tra_dummy,
                                     veteran_tra_dummy, sochh_tra_dummy ,
                                     idp_tra_dummy, other_tra_dummy),
           pubtrans_alt_count = rowSums(across(c(disabil_pen_count, lossbread_pen_count,
                                                 scholar_tra_count, tempdis_tra_count,
                                                 insuran_tra_count, tsa_tra_count,
                                                 veteran_tra_count, sochh_tra_count,
                                                 idp_tra_count, other_tra_count)), na.rm = TRUE),
           pubtrans_alt_pop = pubtrans_alt_dummy * familysize)%>%
    select(uid, pubtrans_inc, pubtrans_inc_alt, all_of(transf),
           ends_with("_dummy"), ends_with("_count"), ends_with("_pop"))

  # Aggregate data
  sim$di_govtransfers <- df


  sim
}


#' 02. 03_GEO22_DirectTaxesContrib.do replication 2.1.1 Presim data  to 2.2.4 Save household file
#' @export
#' @noRd
#' @import dplyr purrr
func_2_direct_tax <- function(sim = list(), inps, presim) {


  get_inp <- devCEQ::make_get_inp_fn(actual_inps = inps)


  employee_rates <-
    make_tax_brackets(
      c(0, c("pit1", "pit2", "pit3", "pit4", "pit5", "pit6", "pit7", "pit8") %>%
          map_dbl(get_inp)) %>% `[`(!is.na(.)),
      c("pitw_rate_0", "pitw_rate_1", "pitw_rate_2", "pitw_rate_3", "pitw_rate_4", "pitw_rate_5", "pitw_rate_6", "pitw_rate_7", "pitw_rate_8")%>%  map_dbl(get_inp)%>% `[`(!is.na(.))
    )



  # Compute scalar values
  TOTCONG01 <- inps$sscg1_limit * inps$sscg1_rate
  TOTCONG02 <- ifelse(is.na(inps$sscg2_rate) | is.na(inps$sscg2_limit), 0, TOTCONG01 + (inps$sscg2_limit - inps$sscg1_limit) * inps$sscg2_rate)
  TOTCONG03 <- ifelse(is.na(inps$sscg3_rate) | is.na(inps$sscg3_limit), 0, TOTCONG02 + (inps$sscg3_limit - inps$sscg2_limit) * inps$sscg3_rate)
  TOTCONG04 <- ifelse(is.na(inps$sscg4_rate) | is.na(inps$sscg4_limit), 0, TOTCONG03 + (inps$sscg4_limit - inps$sscg3_limit) * inps$sscg4_rate)


  varsums <- c("lab_wage1", "lab_other1", "lab_earn", "lab_wage_prim", "lab_wage2",
               "lab_other2", "lab_other3", "lab_wage_sec", "abroad", "lab_wage_prim_yr", "lab_earn_yr",
               "lab_wage_sec_yr", "lab_other3_yr", "abroad_yr", "lab_inc_yr", "lab_wage_prim_tax",
               "lab_earn_tax", "lab_wage_sec_tax", "lab_inc_tax", "lab_wage_prim_gross", "lab_earn_gross",
               "lab_wage_sec_gross", "abroad_gross", "lab_inc_gross", "lab_wage_all_gross",
               "lab_wage_grossup", "lab_sec_grossup", "lab_earn_grossup", "taxinc_con", "pcon_indiv",
               "pcon_gov", "pcon_tot", "formal_dummy", "wage_formal_dummy", "earn_formal_dummy",
               "sec_formal_dummy", "lab_wage_sec_taxable", "lab_earn_taxable"
  )

  # can we not speed this up by creating a function that calculates the average tax someone has to pay
  # and multiply their income with that?  nesting is inherently slow.
  df <- presim$dtx %>%
    match_tax_brackets("lab_wage_prim_gross", employee_rates, "lab_wage_prim_tax") %>%
    match_tax_brackets("lab_wage_sec_taxable", employee_rates, "lab_wage_sec_tax") %>%
    mutate(lab_wage_prim_tax = replace_na(lab_wage_prim_tax, 0)) %>%
    mutate(lab_wage_sec_tax = replace_na(lab_wage_sec_tax, 0)) %>%

    mutate(
      lab_earn_tax = lab_earn_taxable * inps$pits_rate,
      lab_inc_tax =  rowSums(across(c(lab_wage_prim_tax,lab_wage_sec_tax, lab_earn_tax )), na.rm = TRUE),
      lab_earn_gross =  rowSums(across(c( lab_earn_yr , lab_earn_tax)), na.rm = TRUE),
      lab_wage_sec_gross =  rowSums(across(c( lab_wage_sec_yr,lab_wage_sec_tax)), na.rm = TRUE),
      abroad_gross = abroad_yr,
      lab_wage_all_gross =  rowSums(across(c(lab_wage_prim_gross , lab_wage_sec_gross , abroad_gross)), na.rm = TRUE),
      lab_inc_gross =  rowSums(across(c(lab_wage_prim_gross, lab_earn_gross, lab_wage_sec_gross, abroad_gross)), na.rm = TRUE),
      pcon_wage = if_else(pen_contributor == 1, lab_wage_prim_gross * (inps$sscw_rate + inps$ssce_rate), 0),
      pcon_sec = if_else(pen_contributor == 1, lab_wage_sec_gross * (inps$sscw_rate + inps$ssce_rate), 0),
      pcon_earn = if_else(pen_contributor == 1, lab_earn_gross * inps$sscs_rate, 0),
      pcon_indiv =  rowSums(across(c(pcon_wage, pcon_sec, pcon_earn)), na.rm = TRUE),
      lab_wage_grossup =  rowSums(across(c(lab_wage_prim_gross, pcon_wage)), na.rm = TRUE),
      lab_sec_grossup =  rowSums(across(c(lab_wage_sec_gross, pcon_sec)), na.rm = TRUE),
      lab_earn_grossup =  rowSums(across(c(lab_earn_gross, pcon_earn)), na.rm = TRUE),
      lab_tot_grossup =  rowSums(across(c(lab_wage_grossup, lab_sec_grossup, lab_earn_grossup)), na.rm = TRUE),
      #taxinc_con = pmin(lab_tot_grossup*pen_contributor, inps$sscg_max)) %>%
      taxinc_con = pmin(lab_tot_grossup*pen_contributor, max(inps$sscg1_limit,
                                                             inps$sscg2_limit,
                                                             inps$sscg3_limit,
                                                             inps$sscg4_limit,
                                                             inps$sscg5_limit,
                                                             na.rm = TRUE))) %>%
    mutate(pcon_gov = case_when(
      taxinc_con <= inps$sscg1_limit ~ taxinc_con * inps$sscg1_rate,
      taxinc_con > inps$sscg1_limit & taxinc_con <= inps$sscg2_limit ~ TOTCONG01 + (taxinc_con - inps$sscg1_limit) * inps$sscg2_rate,
      taxinc_con > inps$sscg2_limit & taxinc_con <= inps$sscg3_limit ~ TOTCONG02 + (taxinc_con - inps$sscg2_limit) * inps$sscg3_rate,
      taxinc_con > inps$sscg3_limit & taxinc_con <=inps$sscg4_limit ~ TOTCONG03 + (taxinc_con - inps$sscg3_limit) * inps$sscg4_rate,
      taxinc_con > inps$sscg4_limit ~ TOTCONG04 + (taxinc_con - inps$sscg4_limit) * inps$sscg5_rate,
      TRUE ~ 0
    )) %>%
    mutate(
      pcon_tot = if_else(pen_contributor == 1, pcon_indiv + pcon_gov, NA_real_)
    ) %>%
    group_by(uid) %>%
    summarise(across(all_of(varsums), \(x)  sum(x, na.rm = TRUE)),
              weights = weights[1],
              urbanorrural = urbanorrural[1]
              # weights = first(weights),
              # urbanorrural = first(urbanorrural)
    ) %>%
    ungroup()%>%
    mutate(neg_pcon_indiv = -pcon_indiv,
           neg_pcon_gov = -pcon_gov,
           neg_pcon_tot = -pcon_tot) %>%
    mutate(lab_inc_tax =floor(lab_inc_tax *100)/100)

  sim$laborincome <- df


  sim
}



#' 03. 03_GEO22_DirectTaxesContrib.do replication 2.3: Capital Tax
#' @export
#' @noRd
#' @import dplyr
func_3_capital_tax <- function(sim = list(), inps, presim) {

  df <- presim$mi_capitalincome %>%
    mutate(cap_inc_tax = cap_inc_gross * inps$pitk_rate)


  # Aggregate data
  sim$mi_capitalincome <- df


  sim
}


#' 03a. 03_GEO22_DirectTaxesContrib.do replication 2.3: Capital Tax
#' @export
#' @noRd
#' @import dplyr
func_4_realestate <- function(sim = list(), inps, presim) {
  df <- presim$house_mktvalue %>%
    mutate(
      annual_transfer = shemosavalidatransf  * 12,
      reale_tax_min = case_when(
        annual_transfer >= inps$ptax_thr1 & annual_transfer < inps$ptax_thr2 ~ p_price * inps$ptax_r1l,
        annual_transfer >= inps$ptax_thr2 ~ p_price * inps$ptax_r2l,
        TRUE ~ 0
      ),
      reale_tax_max = case_when(
        annual_transfer >= inps$ptax_thr1 & annual_transfer < inps$ptax_thr2 ~ p_price * inps$ptax_r1u,
        annual_transfer >= inps$ptax_thr2 ~ p_price * inps$ptax_r2u,
        TRUE ~ 0
      )
    ) %>%
    select(uid, reale_tax_min, reale_tax_max)

  # Aggregate data
  sim$realestate_tax <- df

  sim
}



#' 04. 02_GEO22_DirectTaxesContrib.do replication 2.6:  Agricultural land
#' @export
#' @noRd
#' @import dplyr

func_5_landtax <- function(sim = list(), inps, presim) {


  # Define matrices larab and lpast
  larab <- c(inps$larab_1,
             inps$larab_2,
             inps$larab_3,
             inps$larab_4,
             inps$larab_5,
             inps$larab_6,
             inps$larab_7,
             inps$larab_8,
             inps$larab_9,
             inps$larab_10,
             inps$larab_11,
             inps$larab_12,
             inps$larab_13,
             inps$larab_14
  )
  lpast <- c(inps$lpast_1,
             inps$lpast_2,
             inps$lpast_3,
             inps$lpast_4,
             inps$lpast_5,
             inps$lpast_6,
             inps$lpast_7,
             inps$lpast_8,
             inps$lpast_9,
             inps$lpast_10,
             inps$lpast_11,
             inps$lpast_12,
             inps$lpast_13,
             inps$lpast_14)


  df <- presim$land   %>%
    mutate(
      str_regno = as.character(regno),
      num_regno = regno + 1,
      tlarab = larab[num_regno],
      tlpast = lpast[num_regno],
      landtax1 = ifelse(is.na(tlarab), 0, wholeland * sh_arable * tlarab),
      landtax2 = ifelse(is.na(tlpast), 0, wholeland * sh_pasture * tlpast)
    ) %>%
    mutate(landtax = rowSums(across(c(landtax1, landtax2)), na.rm = TRUE))

  sim$landtax <- df

  sim

}

#' 06. 04_GEO22_IndTaxes.do replication of whole file
#' @export
#' @noRd
#' @import dplyr

func_6_indtaxes<- function(sim = list(), inps, presim) {
  df <- presim$consumption  %>%
    mutate(
      excise1  =  mobile * inps$exc_rate_mobile,
      excise_beer  =  q_alcohol_beer * inps$exc_rate_beer,
      excise_vodka  =  q_alcohol_vodka * inps$exc_rate_vodka,
      excise_wine  =  q_alcohol_wine * inps$exc_rate_wine,
      excise_brandy  =  q_alcohol_brandy * inps$exc_rate_brandy,
      excise_tobfil  =  q_tobfil * inps$exc_rate_tobfil,
      excise_tobunf  =  q_tobunf * inps$exc_rate_tobunf,
      excise_fuel_liquid_sh03  =  q_fuel_liquid * inps$exc_rate_fuel_liquid,
      excise_fuel_gas_sh03  =  q_fuel_gas * inps$exc_rate_fuel_gas
    ) %>%
    mutate(excise_fuel_sh03 =  rowSums(across(
      c(excise_fuel_liquid_sh03, excise_fuel_gas_sh03)
    ), na.rm = TRUE))  %>%
    mutate(exc_eff_sh03   = excise_fuel_sh03 / fuel_vehicle_sh03) %>%
    mutate(excise_fuel_sh04   = median(exc_eff_sh03, na.rm = TRUE) * fuel_vehicle_sh04) %>%
    mutate(
      excise_fuel =  rowSums(across(c(
        excise_fuel_sh03, excise_fuel_sh04
      )), na.rm = TRUE),
      exc_tax =  rowSums(across(
        c(
          excise1,
          excise_beer,
          excise_vodka,
          excise_brandy,
          excise_tobfil,
          excise_tobunf,
          excise_fuel
        )
      ), na.rm = TRUE)

    ) %>%
    mutate(
      exc_tobac_eff = excise_tobfil / tobacco,
      exc_fuel_eff = excise_fuel / fuel,
      exc_eff_rate = exc_tax / value_cons
    ) %>%
    mutate(
      value_grossup = value_cons + exc_tax,
      cons_only_vat = value_grossup - health - educ - books - financial -
        produce - babyfood,
      eff_vat_rate = (inps$vat_rate)/(1+inps$vat_rate),
      # vat           = (cons_only_vat) * (inps$vat_rate) * formal_share,

      vat = (cons_only_vat)*(eff_vat_rate)*formal_share,
      #vat_full      = value_grossup * (inps$vat_rate) * formal_share,
      vat_full      =value_grossup*(eff_vat_rate)*formal_share,
      # vat_exemp     = (health + educ + books + financial + produce +
      #                    babyfood) * (inps$vat_rate) * formal_share,

      vat_exemp     =  (health+educ+books+financial+produce+babyfood)*(eff_vat_rate)*formal_share,
      vat_exemp_neg = -vat_exemp
    ) %>%
    select(
      uid,
      value_cons,
      exc_tax,
      value_grossup,
      cons_only_vat,
      vat,
      vat_full,
      vat_exemp,
      vat_exemp_neg,
      exc_eff_rate
    )
  sim$ci_consumption <- df

  sim
}

#' 07. 05_GEO22_IndSubs.do replication of  electic subsidies
#' @export
#' @noRd
#' @import dplyr

func_7_subs_elec<- function(sim = list(), inps, presim) {

  df<- presim$conssub_electricity %>%  # Drop the old qconsumed column
    mutate(qconsumed = cons_elec / inps$sub_price_elec)%>%
    mutate(a = ifelse(connect_elec > 0, 1, 0))%>%
    arrange(a, cons_elec, uid)%>%
    mutate(
      b = cumsum(a * weights / 4),
      c = sum(a * weights / 4),
      d = b / c
    ) %>%
    mutate(selected = ifelse(d <= 0.95 & a == 1 & connect_elec > 0, 1, 0)) %>%
    mutate(subs_elec = 0) %>%
    # Replacing subs_elec under specific conditions
    mutate(subs_elec = ifelse(connect_elec & selected, cons_elec_free, subs_elec)) %>%
    # Create a dummy for whether subs_elec > 0 and not missing

    mutate(subs_dummy = ifelse(subs_elec > 0 & !is.na(subs_elec), 1, 0)) %>%
    # Creating subs_elec2 under specific conditions
    mutate(subs_elec12 = ifelse(qconsumed <= inps$sub_limit_elec &  subs_elec == 0, cons_elec * ( inps$sub_rate_elec /  inps$sub_price_elec), NA)) %>%
    # Replacing NA values in subs_elec2 with 0
    mutate(subs_elec2 = ifelse(is.na(subs_elec12), 0, subs_elec12)) %>%
    # Updating subs_elec by adding subs_elec2
    mutate(subs_elec = subs_elec + subs_elec2) %>%
    # Dropping the subs_dummy and recreating it
    # mutate(subs_dummy = ifelse(subs_elec > 0 & !is.na(subs_elec), 1, 0)) %>%
    # Annualizing quarterly values
    mutate(
      cons_elec = cons_elec * 4,
      # cons_elec_free = cons_elec_free * 4,
      subs_elec = subs_elec * 4
    ) %>%
    # Creating total annual electricity consumption
    # mutate(totcons_elec = cons_elec + cons_elec_free) %>%
    # Creating monthly frequency values
    # mutate(
    #   cons_elec_month = cons_elec / 12,
    #   cons_elec_month_free = cons_elec_free / 12,
    #   subs_elec_month = subs_elec / 12
    # ) %>%
    select(uid, cons_elec, subs_elec)

  sim$ci_01subs_elec <- df

  sim
}

#' 08. 05_GEO22_IndSubs.do replication of 3.2: Gas
#' @export
#' @noRd
#' @import dplyr

func_8_subs_gas<- function(sim = list(), inps, presim) {

  df <- presim$conssub_gas %>%
    mutate(subs_gas = 0,
           subs_gas = ifelse(cons_gas_free < inps$sub_free_gas, inps$sub_free_gas * gas_connection, cons_gas_free * gas_connection),
           subs_gas = if_else(s_q9 <= 2, cons_gas_free, subs_gas),
           subs_gas = if_else(cons_gas_free < inps$sub_free_gas & s_q9 <= 2, inps$sub_free_gas , subs_gas)) %>% ## should these be globals?
    mutate(subs_dummy = as.integer(subs_gas > 0 & !is.na(subs_gas))) %>%
    mutate(sub_gas2 = (value / inps$sub_price_gas)*inps$sub_rate_gas) %>%
    mutate(cons_gas = cons_gas*4,
           cons_gas_free=cons_gas_free*4,
           subs_gas=subs_gas*4) %>%
    select(uid,subs_gas,cons_gas )

  sim$ci_02subs_gas <- df

  sim
}


#' 09. 05_GEO22_IndSubs.do replication of 3.3: Water
#' @export
#' @noRd
#' @import dplyr

func_9_conssub_water <- function(sim = list(), inps, presim) {

  df <- presim$conssub_water %>%
    mutate(subs_wat = 0) %>%
    mutate(subs_wat = ifelse((regno %in% c(1, 11) & watersource <= 2) | (regno == 3 & urban == 1 & watersource <= 2), cons_wat_free, 0)) %>%
    mutate(subs_wat = ifelse(subs_wat > inps$sub_upbound_wat, inps$sub_upbound_wat, subs_wat)) %>%
    mutate(
      subs_wat = subs_wat * 4,
      cons_wat = cons_wat * 4,
      cons_wat_free = cons_wat_free * 4
    ) %>%
    select(uid, cons_wat, subs_wat)

  sim$ci_03subs_water <- df

  sim
}



#' 10. 05_GEO22_IndSubs.do replication of 3.4: Transport
#' @export
#' @noRd
#' @import dplyr

func_10_conssub_transport <- function(sim = list(), inps, presim) {

  df <- presim$conssub_transport %>%
    mutate(fortrans_base=pubtrans*fortrans ) %>%
    mutate(tbi_transp = (fortrans_base/inps$sub_price_trans)*inps$sub_rate_trans) %>%
    select(uid, tbi_transp)

  sim$ci_04subs_transp <- df

  sim
}

#' 11. 05_GEO22_IndSubs.do replication of 3. all subsidies
#' @export
#' @noRd
#' @import dplyr
#' @import tidyr
func_11_all_subsidies <- function(sim = list()) {


  df <- sim$ci_01subs_elec %>%
    full_join( sim$ci_02subs_gas, by = join_by(uid) )%>%
    full_join( sim$ci_03subs_water, by = join_by(uid) )%>%
    full_join( sim$ci_04subs_transp, by = join_by(uid) ) %>%
    mutate(ind_sub =  rowSums(across(c(subs_elec, subs_gas, subs_wat, tbi_transp)), na.rm = TRUE)) %>%
    select(	uid,ind_sub,cons_elec,subs_elec,cons_gas,subs_gas,subs_wat,tbi_transp )

  sim$ci_indir_subs <- df

  sim
}


#' 12. 06_GEO22_Health.do replication of health file
#' @export
#' @noRd
#' @import dplyr
#' @import tidyr
func_12_health <- function(sim = list(), inps, presim) {

  health_ben_2022 <- c(inps$health_ben_2022_1,
                       inps$health_ben_2022_2,
                       inps$health_ben_2022_3,
                       inps$health_ben_2022_4,
                       inps$health_ben_2022_5,
                       inps$health_ben_2022_6,
                       inps$health_ben_2022_7,
                       inps$health_ben_2022_8,
                       inps$health_ben_2022_9
  )


  health_ben_2022 =  health_ben_2022 * ( inps$total_health_benefits / 3688460)

  # Create uhcp_benefit variable
  df <- presim$hltgroups %>%
    mutate(uhcp_benefit = round(health_ben_2022[uhcp])) %>%
    # Aggregate benefits at the household level
    group_by(uid) %>%
    summarise(inkind_health = sum(uhcp_benefit, na.rm = TRUE),
              regno = regno[1],
              monthly_inc = monthly_inc[1],
              weights= weights[1]) %>%
    mutate(loghealth = log(inkind_health))%>%
    left_join(presim$outofpocket, by = join_by(uid)) %>%
    mutate(health_tot =rowSums(across(c(inkind_health, health_oop)), na.rm = TRUE)) %>%
    mutate(log_heatot=log(health_tot)) %>%
    select(	uid,inkind_health, health_oop, health_tot)


  sim$inkind_health <- df

  sim
}


#' 13. 07_GEO22_Education.do replication of health file
#' @export
#' @noRd
#' @import dplyr
#' @import tidyr
func_13_education <- function(sim = list(), inps, presim) {
  # Pre-school private
  df <- presim$education_hh %>%
    mutate(a = preschool_n_tutoring > 0) %>%
    arrange(a, desc(preschool_exp), uid) %>%
    mutate(
      b = cumsum(preschool_n_tutoring * weights / 4),
      c = sum(preschool_n_tutoring * weights / 4),
      d = b / c,
      preschool_priv = (d <= inps$sh_priv_preschool &
                          a == 1 & preschool_exp > 0)
    ) %>%
    select(-a,-b,-c,-d)

  # General private
  df <- df %>%
    mutate(a = general_n_tutoring > 0) %>%
    arrange(a, desc(general_exp), uid) %>%
    mutate(
      b = cumsum(general_n_tutoring * weights / 4),
      c = sum(general_n_tutoring * weights / 4),
      d = b / c,
      general_priv = (d <= inps$sh_priv_general &
                        a == 1 & general_exp > 0)
    ) %>%
    select(-a,-b,-c,-d)

  # Vocational private
  df <- df %>%
    mutate(a = vocational > 0) %>%
    arrange(a, desc(vocational_exp), uid) %>%
    mutate(
      b = cumsum(vocational * weights / 4),
      c = sum(vocational * weights / 4),
      d = b / c,
      vocational_priv = (d <= inps$sh_priv_vocational &
                           a == 1 & vocational_exp > 0)
    ) %>%
    select(-a,-b,-c,-d)

  # Higher private
  df <- df %>%
    mutate(a = higher_n_tutoring > 0) %>%
    arrange(a, desc(higher_exp), uid) %>%
    mutate(
      b = cumsum(higher_n_tutoring * weights / 4),
      c = sum(higher_n_tutoring * weights / 4),
      d = b / c,
      higher_priv = (d <= inps$sh_priv_higher &
                       a == 1 & higher_exp > 0)
    ) %>%
    select(-a,-b,-c,-d)

  df <- df %>%
    mutate(preschool = ifelse(preschool_priv == 1, 0, preschool),
           general = ifelse(general_priv == 1, 0, general),
           vocational = ifelse(vocational_priv == 1, 0, vocational),
           higher = ifelse(higher_priv == 1, 0, higher))

  # In-kind benefits calculations
  df <- df %>%
    mutate(
      inkind_preschool = preschool * inps$pc_preschool,
      inkind_general = general * inps$pc_general,
      inkind_vocational = vocational * inps$pc_vocational,
      inkind_higher = higher * inps$pc_higher
    )

  # Total in-kind education benefits
  df <- df %>%
    mutate(inkind_educ = rowSums(across(
      c(
        inkind_preschool,
        inkind_general,
        inkind_vocational,
        inkind_higher
      )
    ), na.rm = TRUE))

  # Rename columns
  df <- df %>%
    rename(
      edu_pesc_rh = preschool,
      edu_genr_rh = general,
      edu_voca_rh = vocational,
      edu_tert_rh = higher
    )

  # Select required columns
  df <- df %>%
    select(uid, starts_with("inkind_"), ends_with("_rh"))



  sim$inkind_educ <- df

  sim
}




#' 99. run simulation
#' @export
#' @noRd
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_replace
#' @importFrom purrr map_dbl
func_99_simulation <- function(sim = sim,
                               inps,
                               presim = list(),
                               ...) {



  nat_povline_monthly <- inps$nat_povline_monthly
  gppp_2017 <- inps$gppp_2017
  gcpi_2017 <-inps$gcpi_2017
  gcpi_2022 <- inps$gcpi_2022
  pline_mod <- nat_povline_monthly * 12


  ppp_2017 <- gppp_2017
  cpi_2017 <- gcpi_2017
  cpi_2022 <- gcpi_2022

  # Function to perform operations
  pa_pc <- function(var, data) {
    # Creating names for the new variables
    var_pc <- str_replace(var, "_hh$", "_pc")
    var_pa <- str_replace(var, "_hh$", "_pa")

    # Generating new variables
    data[[var_pc]] <- data[[var]] / (data$hsize * data$all_cpi * data$reg_def)
    data[[var_pa]] <- data[[var]] / (data$scale_adjust * data$all_cpi * data$reg_def)

    # Replacing NA values with 0
    data[[var_pc]][is.na(data[[var_pc]])] <- 0
    data[[var_pa]][is.na(data[[var_pa]])] <- 0

    # Creating labels
    labhh <- names(data)[names(data) == var]
    labpc <- str_replace(labhh, "(household)", "(per capita)")
    labpa <- str_replace(labhh, "(household)", "(adult equivalent)")

    # Optional: Assigning labels if the dataframe supports it, e.g., using Hmisc to add variable labels

    # Inverting values based on regex condition
    if (grepl("dtx|cpn|itx", var_pc)) {
      data[[var_pc]] <- data[[var_pc]] * -1
      data[[var_pa]] <- data[[var_pa]] * -1
    }

    return(data)
  }

  sim_results <- presim$marketincome %>%
    select(-c(lab_wage1, lab_other1, lab_earn, lab_wage_prim,
              lab_wage2, lab_other2, lab_other3, lab_wage_sec, abroad, lab_wage_prim_yr,
              lab_earn_yr, lab_wage_sec_yr, lab_other3_yr, abroad_yr, lab_inc_yr,
              lab_wage_prim_tax, lab_earn_tax, lab_wage_sec_tax, lab_wage_prim_gross,
              lab_earn_gross, lab_wage_sec_gross, abroad_gross, lab_inc_gross,
              lab_wage_all_gross, lab_wage_grossup, lab_sec_grossup, lab_earn_grossup,
              taxinc_con, pcon_indiv, pcon_gov, pcon_tot, formal_dummy, wage_formal_dummy,
              earn_formal_dummy, sec_formal_dummy, lab_wage_sec_taxable, lab_earn_taxable,
              urbanorrural, neg_pcon_indiv, neg_pcon_gov, neg_pcon_tot)) %>%
    full_join(sim$laborincome %>% select(-weights), by = join_by(uid)) %>%
    select(-c(cap_inc_yr, cap_inc_gross)) %>%
    full_join(sim$mi_capitalincome, by = join_by(uid)) %>%
    select(-c(disabil_pen, lossbread_pen, tsa_tra_dummy,
              tsa_tra_count, pubtrans_inc)) %>%
    full_join(sim$di_govtransfers, by = join_by(uid)) %>%
    select(-c(reale_tax_min)) %>%
    full_join(sim$realestate_tax, by = join_by(uid)) %>%
    select(-c(
      homebuildeddate, ownerofhome, walls, floor, ceiling, wholes, livings,
      howmanyrooms, howmanybedrooms, kitchen, cellar, bath, garage, watersource,
      typeoftoilet, sellingprice, rent, otherhouse, otherhouses, othervillage,
      othervillages, otherland, otherlands, usedlandyesno, numberofpices, wholeland,
      s_q1, s_q2, s_q2a, s_q2b, s_q2c, s_q3, s_q4, s_q5, s_q6, s_q7, s_q8, s_q9,
      arable, pasture, areaplot, sh_arable, sh_pasture, arableland, pastureland,
      num_regno, tlarab, tlpast
    )) %>%

    full_join(sim$landtax %>% select(-c(monthyear, regno, urbanorrural,quartno, diaryid, str_regno, weights, landtax1, landtax2))
              , by = join_by(uid))%>%
    full_join(sim$ci_consumption, by = join_by(uid)) %>%
    full_join(sim$ci_indir_subs, by = join_by(uid))%>%
    full_join(sim$inkind_health, by = join_by(uid))%>%
    full_join(sim$inkind_educ, by = join_by(uid)) %>%
    mutate(
      pen_inc = pen_inc_month, #### NOT SURE ABOUT THIS?
      #pen_inc = if_else(is.na(pen_inc), 0, pen_inc),
      pen_inc_month = if_else(is.na(pen_inc_month), 0, pen_inc_month),
      pension_dummy = if_else(is.na(pension_dummy), 0, pension_dummy),
      pit_tax =  rowSums(across(c(lab_inc_tax, cap_inc_tax)), na.rm = TRUE),
      prop_tax =  rowSums(across(c(reale_tax_max, landtax)), na.rm = TRUE),
      dir_tax =  rowSums(across(c( pit_tax, prop_tax)), na.rm = TRUE),
      socprot_inc =  rowSums(across(c(  pen_inc, pubtrans_inc_alt)), na.rm = TRUE),
      neg_prop_tax = -prop_tax,
      neg_pit_tax = -pit_tax,
      neg_socprot_inc = -socprot_inc
    ) %>%
    full_join(presim$poverty %>% select(-c( weights, monthyear, quartno, diaryid, childern, working_age_man, working_age_woman,
                                            pensioner_age_man, pensioner_age_woman, eq_adult, eq_skale_0_6, ed_skale_0_8, shemdaq, shemtvitdasaqm, shem_sof,
                                            qonebidan, pensstipdaxm, ucxoetidan, axloblebisagan, shemosavalidatransf, qonebisgayidvit, sesxandanazog,
                                            sxvafuladisaxsrebi, arafuladimoxmareba, shemosavalisul, fuladisaxsrebi, saxsrebi_sul, type, type1, weights_quar, popw,
                                            hhsize, adult, age1, age2, age3, age4, age5, age6, child15, hhhsex, hhhage, hhheduc, hhhmigr, hhhmigr_from, q11_1,
                                            q11_2, q11_3, q11_4, q11_5, q11_6, q11_7, q11_8, dur_n1, dur_n2, dur_n3, dur_n4, dur_n5, dur_n6, dur_n7, dur_n8,
                                            dur_n9, dur_n10, dur_n11, dur_n12, dur_n13, dur_n14, dur_n15, dur_n17, dur_n18, dur_n19, dur_n20, dur_n21, dur_n22,
                                            dur_n23, dur_n24, dur_n25, dur_n26, dur_n27, dur_n28, dur_n29, dur_q1, dur_q2, dur_q3, dur_q4, dur_q5, dur_q6, dur_q7,
                                            dur_q8, dur_q9, dur_q10, dur_q11, dur_q12, dur_q13, dur_q14, dur_q15, dur_q17, dur_q18, dur_q19, dur_q20, dur_q21,
                                            dur_q22, dur_q23, dur_q24, dur_q25, dur_q26, dur_q27, dur_q28, dur_q29, lvs_n1, lvs_n2, lvs_n3, lvs_n4, lvs_n5, lvs_n6,
                                            lvs_n7, lvs_n8, lvs_n9, lvs_n10, lvs_n11, lvs_n12, lvs_n13, lvs_q1, lvs_q2, lvs_q3, lvs_q4, lvs_q5, lvs_q6, lvs_q7,
                                            lvs_q8, lvs_q9, lvs_q10, lvs_q11, lvs_q12, lvs_q13, sursati__sasmeli__tambaqo, tansacmeli, saojaxo, samedicino,
                                            energia, transporti, ganatleba, sxva, samomxmxarjebi, sasoflo, transferti, dazogvaancasesxeba, qonebis_sezena,
                                            sxvagasavlebi, arafuladi_moxm_, fuladixarjebi, mtlianimoxmareba_, mtlianixarjebi_, mtlianimoxmareba_eqadscale,
                                            mtlianimoxmareba_eqadscale_08, totexp, totcons, aecons_old, totinc, aeinc, aggr, aecons, decilc, quintilc, pline, cpsc,
                                            pgc, p2c, homebuildeddate, ownerofhome, walls, floor, ceiling, wholes, livings, howmanyrooms, howmanybedrooms, kitchen,
                                            cellar, bath, garage, watersource, typeoftoilet, sellingprice, rent, otherhouse, otherhouses, othervillage,
                                            othervillages, otherland, otherlands, usedlandyesno, numberofpices, wholeland, s_q1, s_q2, s_q2a, s_q2b, s_q2c, s_q3,
                                            s_q4, s_q5, s_q6, s_q7, s_q8, s_q9, regno)), by = join_by(uid)) %>%
    mutate(laecons = log(aecons)) %>%
    select(-laecons) %>%
    mutate( weight = round(weights*familysize/4),
            pen_pens_hh = pen_inc_month*12,
            socprot_dummy = pmax(pension_dummy ,pubtrans_alt_dummy, na.rm = TRUE),
            socprot_count =  rowSums(across(c(pensioners, disabil_pen_count, lossbread_pen_count,
                                              tempdis_tra_count, tsa_tra_count, veteran_tra_count,
                                              idp_tra_count, other_tra_count)), na.rm = TRUE),
            socprot_pop	= socprot_dummy*familysize)%>%
    # mutate(yd_hh = pmax(cons_hh, dpe_totl_hh),
    #        yd_pc = yd_hh / (hsize		 *all_cpi*reg_def),
    #        yd_pa = yd_hh/  (scale_adjust*all_cpi*reg_def) ) %>%
    mutate(dtr_lbrd_hh = lossbread_pen,
           dtr_tsat_hh = tsa_tra,
           dtr_idpt_hh = idp_tra,
           dtr_othr_hh =  rowSums(across(c(other_tra, scholar_tra,  tempdis_tra,
                                           disabil_pen, veteran_tra)), na.rm = TRUE),
           dtr_lbrd_ri = lossbread_pen_count,
           dtr_tsat_ri = tsa_tra_count,
           dtr_idpt_ri = idp_tra_count,
           dtr_othr_ri = pmax(other_tra_count, scholar_tra_count, tempdis_tra_count,
                              veteran_tra_count, disabil_pen_count),
           pen_pens_ri = pensioners) %>%
    mutate(across(c(dtr_lbrd_ri, dtr_othr_ri, pen_pens_ri), ~replace(., is.na(.), 0))) %>%
    rename(dtr_lbrd_rh = lossbread_pen_dummy,
           dtr_tsat_rh = tsa_tra_dummy,
           dtr_idpt_rh = idp_tra_dummy) %>%
    mutate(dtr_othr_rh = pmax(other_tra_dummy, scholar_tra_dummy, tempdis_tra_dummy,
                              veteran_tra_dummy, disabil_pen_dummy)) %>%
    mutate(across(contains("_rh"), ~replace(., is.na(.), 0))) %>%
    mutate(across(contains("_ri"), ~replace(., is.na(.), 0))) %>%
    mutate(dtr_pcon_hh     = pcon_gov) %>%
    mutate(dtr_pcon_hh = ifelse(is.na(dtr_pcon_hh),0, dtr_pcon_hh)) %>%
    mutate(dtr_pcon_rh     = as.numeric(dtr_pcon_hh>0)) %>%
    select(-dtr_totl_hh)


  vars_hh_1 <- names(sim_results)[grepl("dtr_.*_hh$", names(sim_results))]

  # Apply the function to each 'hh' variable
  for (var in vars_hh_1) {
    sim_results <- pa_pc(var, sim_results)
  }

  sim_results <- sim_results%>%
    mutate(
      # Creating new variables by directly assigning existing ones
      dtx_pitx_hh = pit_tax,       # Direct taxes on income
      dtx_prop_hh = prop_tax,      # Direct taxes on property

      # Contributions (noted as not included in analysis, but still created)
      cpn_pens_hh = pcon_indiv,    # Contributions: Individual pensions
      cpn_cgov_hh = pcon_gov       # Contributions: Government pensions
    )


  vars_hh_2 <- names(sim_results)[grepl("cpn_.*_hh$|dtx_.*_hh$|pen_.*_hh$", names(sim_results))]

  # Apply the function to each 'hh' variable
  for (var in vars_hh_2) {
    sim_results <- pa_pc(var, sim_results)
  }

  sim_results <- sim_results %>%
    # mutate(
    #   # Rounding the calculated market incomes
    #   ym_pc = round(ym_pc, digits = 8),
    #   ym_pa = round(ym_pa, digits = 8)
    # ) %>%
    mutate(yp_pc =  ym_pc + (pen_pens_pc),
           yp_pa =  ym_pa + (pen_pens_pa),
           yn_pc = yp_pc + (dtx_pitx_pc + dtx_prop_pc),
           yn_pa = yp_pa + (dtx_pitx_pa + dtx_prop_pa) ,
           yg_pc = yp_pc + (dtr_lbrd_pc + dtr_tsat_pc + dtr_idpt_pc + dtr_othr_pc + dtr_pcon_pc),
           yg_pa = yp_pa + (dtr_lbrd_pa + dtr_tsat_pa + dtr_othr_pa + dtr_idpt_pa + dtr_pcon_pa) ,
           yd_pc = yn_pc + (dtr_lbrd_pc + dtr_tsat_pc + dtr_idpt_pc + dtr_othr_pc + dtr_pcon_pc) ,
           yd_pa = yn_pa + (dtr_lbrd_pa + dtr_tsat_pa + dtr_othr_pa + dtr_idpt_pa + dtr_pcon_pa)

           # yn_pc =  round(yp_pc + (dtx_pitx_pc + dtx_prop_pc), digits = 8),
           # yn_pa =  round(yp_pa + (dtx_pitx_pa + dtx_prop_pa), digits = 8),
           # yg_pc =  round(yp_pc + (dtr_lbrd_pc + dtr_tsat_pc + dtr_idpt_pc + dtr_othr_pc + dtr_pcon_pc) , digits = 8),
           # yd_pc =  round(yn_pc + (dtr_lbrd_pc + dtr_tsat_pc + dtr_idpt_pc + dtr_othr_pc + dtr_pcon_pc) , digits = 8),
           # yd_pa =  round(yn_pa + (dtr_lbrd_pa + dtr_tsat_pa + dtr_othr_pa + dtr_idpt_pa + dtr_pcon_pa), digits = 8)
    ) %>%
    mutate(
      # Creating new variables for indirect taxes
      itx_vatx_hh = vat,         # VAT
      itx_excs_hh = exc_tax,     # Excises

      # Creating new variables for indirect subsidies
      sub_elec_hh = subs_elec,   # Electricity subsidy
      sub_gass_hh = subs_gas,    # Gas subsidy
      sub_watr_hh = subs_wat,    # Water subsidy
      sub_trns_hh = tbi_transp   # Public transport subsidy
    )


  vars_hh_3 <- names(sim_results)[grepl("itx.*_hh$|sub.*hh$", names(sim_results))]

  # Apply the function to each 'hh' variable
  for (var in vars_hh_3) {
    sim_results <- pa_pc(var, sim_results)
  }


  sim_results <- sim_results %>%
    mutate(
      # Generating yc_pc by adding up the respective subsidies and taxes to yd_pc
      yc_pc = yd_pc + sub_gass_pc + sub_elec_pc + sub_watr_pc + sub_trns_pc +
        itx_vatx_pc + itx_excs_pc,

      # Generating yc_pa by adding up the respective subsidies and taxes to yd_pa
      yc_pa = yd_pa + sub_gass_pa + sub_elec_pa + sub_watr_pa + sub_trns_pa +
        itx_vatx_pa + itx_excs_pa
    )%>%
    mutate(
      # Health benefits
      hlt_publ_hh = inkind_health,    # Public health benefits
      hlt_totl_hh = inkind_health,    # Total health spending (public+oop), assuming oop is not separate here

      # Education benefits
      edu_pesc_hh = inkind_preschool, # Pre-school education
      edu_genr_hh = inkind_general,   # General education
      edu_voca_hh = inkind_vocational, # Vocational education
      edu_tert_hh = inkind_higher,    # Higher education
      edu_totl_hh = inkind_educ       # Total education
    )


  vars_hh_4 <- names(sim_results)[grepl("edu_.*_hh$|hlt.*hh$", names(sim_results))]

  # Apply the function to each 'hh' variable
  for (var in vars_hh_4) {
    sim_results <- pa_pc(var, sim_results)
  }



  sim_results <- sim_results %>%
    mutate(
      # # Gross Income Construction by subtracting direct taxes
      # yg_pc = yd_pc - (dtx_pitx_pc + dtx_prop_pc),
      # yg_pa = yd_pa - (dtx_pitx_pa + dtx_prop_pa),

      # Final Income Construction by adding health and education benefits to yc_* incomes
      yf_pc = yc_pc + hlt_totl_pc + edu_pesc_pc + edu_genr_pc + edu_voca_pc + edu_tert_pc,
      yf_pa = yc_pa + hlt_totl_pa + edu_pesc_pa + edu_genr_pa + edu_voca_pa + edu_tert_pa
    ) %>%
    mutate(
      pline_mod = nat_povline_monthly * 12,
      pline_loic = ppp_2017 * (cpi_2022 / cpi_2017) * 2.15 * 365,
      pline_lmic = ppp_2017 * (cpi_2022 / cpi_2017) * 3.65 * 365,
      pline_umic = ppp_2017 * (cpi_2022 / cpi_2017) * 6.85 * 365,
    ) %>%
    # add totals

    mutate(across(
      c(starts_with("dtx_"), starts_with("itx_")),
      ~ - .
    )) |>

    mutate(tot_pensions = rowSums(across(c(pen_pens_pc)), na.rm = TRUE),
           tot_dtransfer = rowSums(across(c(dtr_lbrd_pc,  dtr_tsat_pc,  dtr_idpt_pc,
                                            dtr_othr_pc,  dtr_pcon_pc)), na.rm = TRUE),
           tot_dtaxes = rowSums(across(c(dtx_pitx_pc,  dtx_prop_pc)), na.rm = TRUE),
           tot_itaxes = rowSums(across(c(itx_vatx_pc,  itx_excs_pc)), na.rm = TRUE),
           tot_subsidies = rowSums(across(c(sub_elec_pc,  sub_gass_pc,
                                            sub_watr_pc,  sub_trns_pc)), na.rm = TRUE),
           tot_dtaxes_negative = -tot_dtaxes, # this is needed because they were made negative above and otherwise it doesnt replicate the Stata Code
           tot_itaxes_negative = -tot_itaxes,

           tot_netcash = rowSums(across(c(tot_pensions,  tot_dtransfer,
                                          tot_dtaxes_negative,  tot_itaxes_negative,
                                          tot_subsidies)), na.rm = TRUE),
           tot_education = rowSums(across(c(edu_pesc_pc,  edu_genr_pc,
                                            edu_voca_pc,  edu_tert_pc)), na.rm = TRUE),
           tot_health = rowSums(across(c(hlt_publ_pc)), na.rm = TRUE),
           tot_inkind = rowSums(across(c(tot_education, tot_health)), na.rm = TRUE),
           tot_netfisc = rowSums(across(c(tot_netcash,  tot_education,
                                          tot_health)), na.rm = TRUE))
  # CONTINUE HERE
  #sim_results <- sim_results %>%

  # List of variables to process
  vars_to_add_i_to <- names(sim_results)[grepl("^tot_", names(sim_results))]

  # Process each variable
  for (var in vars_to_add_i_to) {
    new_var <- paste0("i", var)

    sim_results <- sim_results %>%
      mutate(!!new_var := .data[[var]] / ym_pc,
             !!new_var := ifelse(is.na(.data[[new_var]]), 0, .data[[new_var]]))

  }
  sim_results <- sim_results %>%

    mutate(itot_pensions = ifelse(is.infinite(itot_pensions), 0, itot_pensions),
           itot_dtransfer= ifelse(is.infinite(itot_dtransfer        ), 0, itot_dtransfer ),
           itot_dtaxes = ifelse(is.infinite(itot_dtaxes), 0, itot_dtaxes          ),
           itot_itaxes = ifelse(is.infinite(itot_itaxes         ), 0, itot_itaxes  ),
           itot_subsidies  = ifelse(is.infinite(itot_subsidies          ), 0, itot_subsidies   ),
           itot_netcash = ifelse(is.infinite(itot_netcash         ), 0, itot_netcash  ),
           itot_education  = ifelse(is.infinite(itot_education          ), 0, itot_education   ),
           itot_health  = ifelse(is.infinite(itot_health          ), 0, itot_health   ),
           itot_netfisc = ifelse(is.infinite(itot_netfisc         ), 0, itot_netfisc  ),

    ) %>%
    mutate(
      hhweight = round(weights / 4),
      hhid = uid
    ) %>%
    select("urbanorrural","monthyear","quartno","diaryid",
           "regno","familysize","childern","adolescents" ,
           "working_age_man","working_age_woman","pensioner_age_man","pensioner_age_woman",
           "eq_adult","eq_skale_0_6","ed_skale_0_8","memberno" ,
           "relations","gender","nationality","maritalstatus",
           "age","pen_pens_rh","dtr_lbrd_rh","dtr_tsat_rh",
           "dtr_idpt_rh","health_oop","edu_pesc_rh","edu_genr_rh",
           "edu_voca_rh","edu_tert_rh","type","type1",
           "hhsize","adult","age1","age2",
           "age3","age4","age5","age6",
           "child15","hhhsex","hhhage","hhheduc",
           "hhhmigr","hhhmigr_from","cpsc","scale_adjust",
           "aecons_yr","reg_def","cons_yr","hsize",
           "cons_pc","weight","yd_pc","yd_pa",
           "dtr_lbrd_ri","dtr_tsat_ri","dtr_idpt_ri","dtr_othr_ri",
           "pen_pens_ri","dtr_othr_rh","dtr_pcon_rh","dtr_lbrd_pc"  ,
           "dtr_lbrd_pa","dtr_tsat_pc","dtr_tsat_pa","dtr_idpt_pc",
           "dtr_idpt_pa","dtr_othr_pc","dtr_othr_pa","dtr_pcon_pc"  ,
           "dtr_pcon_pa","yn_pc","yn_pa","cpn_pens_pc"  ,
           "cpn_pens_pa","cpn_cgov_pc","cpn_cgov_pa","dtx_pitx_pc" ,
           "dtx_pitx_pa","dtx_prop_pc","dtx_prop_pa","pen_pens_pc" ,
           "pen_pens_pa","yp_pc","yp_pa","ym_pc",
           "ym_pa","itx_vatx_pc","itx_vatx_pa","itx_excs_pc"  ,
           "itx_excs_pa","sub_elec_pc","sub_elec_pa","sub_gass_pc"  ,
           "sub_gass_pa","sub_watr_pc","sub_watr_pa","sub_trns_pc" ,
           "sub_trns_pa","yc_pc","yc_pa","edu_pesc_pc",
           "edu_pesc_pa","edu_genr_pc","edu_genr_pa","edu_voca_pc",
           "edu_voca_pa","edu_tert_pc","edu_tert_pa","hlt_totl_pc" ,
           "hlt_totl_pa","yg_pc","yg_pa","yf_pc",
           "yf_pa","pline_mod","hhweight","hhid",
           "kitchen",  "cellar" ,
           "garage",
           "watersource" ,
           "typeoftoilet" ,
           "str_regno",
           "tlarab"    ,
           "tlpast",
           "popw",
           "cons_elec",
           "tbi_transp",
           "ind_sub",
           "tot_pensions",
           "tot_dtransfer",
           "tot_dtaxes",
           "tot_itaxes",
           "tot_subsidies",
           "tot_netcash",
           "tot_health",
           "tot_netfisc",
           "tot_education",
           "pline_loic",
           "pline_lmic",
           "pline_umic",
           "itot_pensions",  "itot_dtransfer", "itot_dtaxes","itot_itaxes",
           "itot_subsidies", "itot_netcash",   "itot_education" ,"itot_health" ,
           "itot_inkind","itot_netfisc"
    ) %>%
    mutate(
      tot_pensions_pc = tot_pensions,
      tot_dtransfer_pc = tot_dtransfer,
      tot_dtaxes_pc = tot_dtaxes,
      tot_itaxes_pc = tot_itaxes,
      tot_subsidies_pc = tot_subsidies,
      tot_netcash_pc = tot_netcash,
      tot_health_pc = tot_health,
      tot_netfisc_pc = tot_netfisc,
      tot_education_pc = tot_education
    ) |>
    mutate(across(ends_with(c("_rh", "_ri")), as.double),
           across(matches("type-cpsc|memberno-age"), as.integer)
    ) %>%
    arrange(hhid)
  sim_results


}


