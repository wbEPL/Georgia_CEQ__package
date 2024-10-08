
local_get_dta_gini <- function(dta, policy_name, ...) {
  get_dta_gini_updated(
    dta = dta,
    policy_name = policy_name,
    wt_var = get_wt_nm(),
    income_vars_tbl =  get_inc_nm() %>%
      filter(!var %in%  c("yg_pc"#,
                          #"yf_pc"
                          )),
    para_names = tibble(
      parameter = c("Gini",
                    "Theil",
                    "P90_P10"),
      label = c("Gini index",
                "Theil index",
                "90/10 income ratio")
    ),
    ...
  )
}



##### to get the same GINI results as Stata use this code (maybe Stata code needs revision instead)





calc_top_bottom_percentiles <- function (x, w = rep(1, length(x)), na.rm = TRUE, bottom_percentile = 0.1, top_percentile = 0.9)
{
  if (!na.rm && any(is.na(x))) {
    return(NA_real_)
  }
  if (is.null(w)) {
    w <- rep(1, length(x))
  }


  x.bottom <- spatstat.univar::weighted.quantile(x, w, bottom_percentile) %>%  as.numeric()
  x.top    <- spatstat.univar::weighted.quantile(x, w, top_percentile)    %>%  as.numeric()

  x.top/x.bottom


}

get_dta_gini_updated <- function (dta, policy_name = NULL, income_vars_tbl = NULL, wt_var = "pcweight",
                                  para_names = tibble(parameter = c("Gini", "Theil", "P90_P10"),
                                                      label = c("Gini index",
                                                                                                           "Theil index",
                                                                                                           "90/10 income ratio")), ...)
{
  if (is.null(income_vars_tbl))
    income_vars_tbl <- get_inc_nm()
  if (is.null(wt_var)) {
    warning("`wt_var` was not specified. ", "Non-weighted statistics is computed.")
    dta <- mutate(dta, dummy_weighting_variable = 1)
    wt_var <- "dummy_weighting_variable"
  }
  if (!is.null(wt_var) && !wt_var %in% names(dta)) {
    warning("Weighting variable '", wt_var, "' is not present in the `dta`. ",
            "Non-weighted statistics is computed.")
    dta <- mutate(dta, dummy_weighting_variable = 1)
    wt_var <- "dummy_weighting_variable"
  }
  check_income_vars_tbl <- all(c("var", "var_title") %in% names(income_vars_tbl)) %>%
    isTRUE()
  if (!check_income_vars_tbl) {
    stop("In the argument `income_vars_tbl` some of the required comuns are\n         not present. Required coluns are: `var` and `var_title`")
  }
  wt_var_sym <- sym(wt_var)
  dta %>% dplyr::select(any_of(income_vars_tbl$var), any_of(wt_var)) %>%
    dplyr::summarise(dplyr::across(any_of(income_vars_tbl$var),
                                   ~calc_gini(., !!wt_var_sym,  drop_zero_and_less = FALSE) * 100, .names = "Gini_____{.col}"),
                     dplyr::across(any_of(income_vars_tbl$var), ~calc_theil(.,
                                                                            !!wt_var_sym) * 100, .names = "Theil_____{.col}"),
                     dplyr::across(any_of(income_vars_tbl$var), ~calc_top_bottom_percentiles(.,
                                                                                             !!wt_var_sym,
                                                                                             bottom_percentile = 0.1,
                                                                                             top_percentile = 0.9), .names = "P90_P10_____{.col}"),
    ) %>% tidyr::pivot_longer(cols = everything(), names_to = "var",
                              values_to = "Value") %>% tidyr::separate(var, into = c("parameter",
                                                                                     "var"), sep = "_____") %>% dplyr::left_join(income_vars_tbl,
                                                                                                                                 by = "var") %>% dplyr::left_join(para_names, by = "parameter") %>%
    dplyr::filter(!is.na(label)) %>% dplyr::mutate(Parameter = label %>%
                                                     factor(., levels = para_names$label) %>% forcats::fct_drop()) %>%
    {
      dta <- .
      if (!is.null(policy_name))
        dta <- dplyr::mutate(dta, Simulation = policy_name)
      dta
    } %>% dplyr::select(Income = var_title, tidyselect::any_of("Simulation"),
                        any_of(c("Parameter", "Value", "Gini", "Theil"))) %>%
    arrange(Income, Parameter)
}

#####

local_get_dta_pov <-
  function(dta,
           policy_name,
           poverty_line_var,
           poverty_line_value = 500000,
           ...) {
    get_dta_pov(
      dta = dta,
      policy_name = policy_name,
      wt_var = get_wt_nm(),
      income_vars_tbl = get_inc_nm() %>%
        filter(!var %in%  c( "yg_pc", "yf_pc")),
      poverty_line_var = poverty_line_var,
      poverty_line_value = poverty_line_value,
      para_names =
        tibble(
          parameter = c("rate", "headcount", "gap", "severity"),
          label = c(
            "Rate",
            "Number of Poor",
            "Gap",
            "Severity"
          ) %>% factor(., levels = .)
        ),
      ...
    )
  }


local_get_dta_pov_pa <-
  function(dta,
           policy_name,
           poverty_line_var,
           poverty_line_value = 500000,
           ...) {
    get_dta_pov(
      dta = dta,
      policy_name = policy_name,
      wt_var = get_wt_nm(),
      income_vars_tbl = get_inc_nm(suffix = "_pa") %>%
        filter(!var %in%  c( "yg_pa", "yf_pa")),
      poverty_line_var = poverty_line_var,
      poverty_line_value = poverty_line_value,
      para_names =
        tibble(
          parameter = c("rate", "headcount", "gap", "severity"),
          label = c(
            "Rate",
            "Number of Poor",
            "gap",
            "severity"
          ) %>% factor(., levels = .)
        ),
      ...
    )
  }

local_gini_server <-
  function(id, sim_res, ...) {
    mod_gini_pov_gen_server_updated(
      id = id,
      sim_res = sim_res,
      title = "Inequality",
      export_btn_title = "Save plot",
      pl_choices = NULL,
      pl_title = NULL,
      get_dta_fn = local_get_dta_gini,
      make_plot_fn = function(dta, ...) {
        make_gini_pov_updated(
          dta = dta,
          x = Income,
          color = Simulation,
          title = "Inequality",
          xlab = "Income",
         # colour_set = "Accent",
          ...
        )
      },
      ...
    )
  }



local_pov_server <-
  function(id, sim_res, ...) {
    mod_gini_pov_gen_server_updated(
      id = id,
      sim_res = sim_res,
      title = "Poverty",
      export_btn_title = "Save",
      pl_choices = c(
        "Poverty line lower income countries  (2.15 USD per day PPP)" = "pline_loic",
        "Poverty line lower middle income countries  (3.65 USD per day PPP)" = "pline_lmic",
        "Poverty line upper middle income countries (6.85 USD per day PPP)" = "pline_umic"),
      pl_title = "Poverty line",
      get_dta_fn = local_get_dta_pov,
      make_plot_fn = function(dta, ...) {
        make_gini_pov_updated(
          dta = dta,
          x = Income,
          color = Simulation,
          title = "Poverty rate",
          xlab = "Income concept",
         # colour_set = "Accent",
          ...
        )
      },
      ...
    )
  }
local_pov_server_pa <-
  function(id, sim_res, ...) {
    mod_gini_pov_gen_server_updated(
      id = id,
      sim_res = sim_res,
      title = "Poverty",
      export_btn_title = "Save",
      pl_choices = c(
        "National" = "pline_mod"
      ),
      pl_title = "Poverty line",
      get_dta_fn = local_get_dta_pov_pa,
      make_plot_fn = function(dta, ...) {
        make_gini_pov_updated(
          dta = dta,
          x = Income,
          color = Simulation,
          title = "Poverty rate",
          xlab = "Income conept",
          #colour_set = "Accent",
          ...
        )
      },
      ...
    )
  }


local_gini_pov_ui <- function(id, ...) {
  ns <- NS(id)
  fluidRow(
    column(6, mod_gini_ui(ns("gini-srv"))),
    column(6, mod_gini_ui(ns("pov-srv")))
  )
}


local_gini_pov_ui1 <- function(id, ...) {
  ns <- NS(id)
  # fluidRow(
  column(12, mod_gini_ui(ns("pov-srv")))#,
  # column(6, mod_gini_ui(ns("pov-srv")))
  # )
}


local_gini_pov_ui2 <- function(id, ...) {
  ns <- NS(id)
  # fluidRow(
    column(12, mod_gini_ui(ns("gini-srv")))#,
    # column(6, mod_gini_ui(ns("pov-srv")))
  # )
}

local_gini_pov_ui_pa <- function(id, ...) {
  ns <- NS(id)
  # fluidRow(
  column(12, mod_gini_ui(ns("pov-srv-pa")))#,
  # )
}

local_gini_pov_server <-
  function(id, sim_res, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      # local_gini_server("gini-srv", sim_res = sim_res, ...)
      local_pov_server("pov-srv", sim_res = sim_res, ...)

    })
  }


local_gini_pov_server_pa <-
  function(id, sim_res, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      local_pov_server_pa("pov-srv-pa", sim_res = sim_res, ...)

    })
  }


local_gini_pov_server2 <-
  function(id, sim_res, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
       local_gini_server("gini-srv", sim_res = sim_res, ...)
      #local_pov_server("pov-srv", sim_res = sim_res, ...)

    })
  }



