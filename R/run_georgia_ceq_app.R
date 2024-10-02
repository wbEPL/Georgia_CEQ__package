#' Run Georgia CEQ Microsimulation Tool
#'
#' This function launches the Georgia CEQ Microsimulation Tool Shiny application.
#'
#' @param path_to_presim Character string. The file path to the presimulation `.rds` file.
#' @param path_to_baseline Character string. The file path to the baseline `.rds` file.
#' @export
#' @import dplyr devCEQ shiny
#' @examples
#' \dontrun{
#' run_georgia_ceq_app()
#' }
run_georgia_ceq_app <- function(path_to_presim = NULL, path_to_baseline = NULL, name_app = "Georgia CEQ Microsimulation Tool") {


  # Set default paths if not provided
  if (is.null(path_to_presim)) {
    path_to_presim <- system.file("extdata", "presim.rds", package = "YourPackageName")
  }
  if (is.null(path_to_baseline)) {
    path_to_baseline <- system.file("extdata", "baseline.rds", package = "YourPackageName")
  }

  # Load presimulation data
  presim_dta <- set_up_presim(path_to_presim, path_to_baseline)

  # Load the input Excel data
  inp_xlsx <- set_up_data()

  # Load input structures
  inputs_str <- load_input_structure()
  inputs_tab_str <- load_input_tabs_structure()
  inputs_table_str <- load_input_tables_structure()

  # Process inputs
  inps_app <- inp_xlsx %>%
    filter(!is.na(inputId)) %>%
    filter(include) %>%
    get_all_inps()
  inps_all <- inp_xlsx %>% get_all_inps()
  inps <- add_missing_inp_generic(inps_app, inps_all)

  # Create input retrieval function
  get_inp <- devCEQ::make_get_inp_fn(actual_inps = inps)

  # Set options
  options(
    scipen = 16,
    ceq_dev = FALSE,
    ceq_run_dev = FALSE,
    ceq_results_dev = FALSE,
    golem.app.prod = TRUE,
    shiny.fullstacktrace = FALSE,
    shiny.error = NULL,
    current.app.name = name_app
  )

  # Generate UI functions
  local_inp_str_fn <- gen_inp_str_front(inp_table_str = inputs_table_str)
  local_tab_ui_fn <- gen_tabinp_ui_front(
    inp_tab_str = inputs_tab_str,
    inp_table_str = inputs_table_str
  )
  local_ceq_ui <- gen_ceq_ui_local(
    inp_nav_width = 3,
    fn_results_ui = mod_results_ui
  )

  # Server-side logic
  local_run_sim_server <- devCEQ::make_run_sim_server(fn_ceq_sim = full_ceq)

  # Run the application
  devCEQ::CEQ_run(
    # Key data
    inputs_str = inputs_str,
    presim = presim_dta,

    # Info page
    info_page_md = "inst/extdata/info-page.md",
    info_page_size = "m",

    # Key policy number options
    n_policy = c(1, 2, 2),
    n_policy_type = "dropdown", # Use devCEQ::get_n_policy_types() to see the options.

    # Key functions
    ui_fn = local_ceq_ui,         # User interface part
    inp_str_fn = local_inp_str_fn, # Inputs structuring
    ui_gen_fn = local_tab_ui_fn,   # Inputs UI

    # Server simulation function
    fn_sim_srvr = local_run_sim_server, # Server-side logic

    # Server-side results
    fn_res_disp_srvr = mod_results_server  # Results side logic
  )
}
