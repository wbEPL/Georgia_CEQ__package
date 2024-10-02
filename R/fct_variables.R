
#' Key variables names and labels
#'
#' @returns data frame with variables names
#' @importFrom shiny isTruthy
#' @importFrom stringr str_c
#' @importFrom dplyr mutate select filter
get_var_nm <- function(vars = NULL, suffix = "_pc") {
  dta <-
    tribble(
      ~factor,  ~ var                , ~var_title,

      # Direct taxes
      -1, "tot_dtaxes", "Direct taxes",
      -1, "dtx_pitx" ,   "Personal Income Tax",
      -1, "dtx_prop" ,  "Property tax" ,


      # Indirect taxes
      -1,"tot_itaxes" ,   "Indirect taxes",
      -1,"itx_vatx"   ,   "Vat",
      -1,"itx_excs"  ,  "Excises" ,

      # Direct transfer

      1, "tot_dtransfer"     , "Direct transfers",
      1, "dtr_tsat"         , "Direct transfer to vulnerable families",
      1, "dtr_lbrd"         , "Direct transfer due to loss of breadwinner",
      1, "dtr_idpt"         , "Direct transfer to  internally displaced persons",
      1, "dtr_pcon"         , "Government contributions to pensions",
      1, "dtr_othr"         , "Any other direct transfer",


      # Social contribution
      1, "tot_pensions", "Total pension",
      1, "cpn_pens" , "Social Contributions to pensions: individual",
      1, "cpn_cgov" , "Social Contributions to pensions: government ",
      1, "pen_pens", "Pension benefits",


      # Subsidies
      1, "tot_subsidies",	"Total subsidy",
      1, "sub_elec",	"Electricity subsidy",
      1, "sub_gass",	"Gas subsidy",
      1, "sub_watr",	"Water subsidy",
      1, "sub_trns", "Transport subsidy",

      # In kind
      1, "tot_inkind", "Total benefits in-kind",
      1, "tot_education", "Education",
      1, "tot_health", "Health",
      1, "edu_pesc", "Primary education",
      1, "edu_genr", "Secondary education",
      1, "edu_tert", "Tertiary education",
      1, "edu_voca", "Vocational education",


      # Income concepts
      1,        "ym"             ,  "Market income",
      1,        "yp"             ,  "Market income plus pensions",
      1,        "yd"             ,  "Disposable income" ,
      1,        "yc"             ,  "Consumable income" ,
     # 1,        "yg"             ,  "Gross income" , asked to remove
      1,        "yf"             ,  "Final income" ,

      # Totals
      # 1, "tot_pensions", "Pensions",
      # 1, "tot_dtransfer", "Direct transfers",
      # 1, "tot_dtaxes", "Direct taxes",
      # 1, "tot_itaxes", "Indirect taxes",
      # 1, "tot_subsidie", "Subsidies",


      # Auxiliary variables
      1,        "hhweight"             ,  "Weight: households" ,
     1,        "weight"             ,  "Survey weight" ,

      1,        "hhid"             ,  "HH id"# ,
      # 1,        "hhsize"           ,  "HH size"
    ) %>%
    distinct() %>%
    mutate(
      var_title = factor(var_title, levels = .$var_title, labels = .$var_title)
    ) %>%
    select(var, var_title, factor)

  if (!is.null(suffix)) {
    dta <- dta %>% mutate(var = str_c(var, suffix))
  }

  if (shiny::isTruthy(vars)) {
    dta <-
      dta %>%
      filter(var %in% vars) %>%
      mutate(var_title = factor(var_title, levels = var_title, labels = var_title))
  }

  return(dta)
}




#' @describeIn get_var_nm Income Concepts variables IDs and labels returned in a data frame
#'
#' @returns a data frame with variables names
get_inc_nm <- function(suffix = "_pc") {
  c("ym", "yp", "yd", "yc", "yg", "yf") %>%
    str_c(., suffix) %>%
    get_var_nm(suffix = suffix)
}

#' @describeIn get_var_nm Weight variable IDs and labels in a data frame
#'
#' @returns a data frame with variables names
get_wt_nm <- function(suffix = NULL) {
  "weight" %>%
    str_c(., suffix) %>%
    get_var_nm(suffix = suffix) %>%
    pull(var)
}
