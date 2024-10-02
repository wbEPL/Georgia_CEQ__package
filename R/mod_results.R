#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @export
#' @importFrom shiny NS tagList
mod_results_ui <- function(id) {
  ns <- NS(id)
  res_content <-
    tagList(
      shiny::actionButton(ns("browse"), label = "Browse the visualisation"),
      shiny::actionButton(ns("save"), label = "Save simulation raw data"))

  left_col <-
    list(
      shinyWidgets::radioGroupButtons(
        inputId = ns("results_tab_choice"),
        label = NULL,
        choices = c(
          "Poverty (national poverty line)"     = "res_pov_tab_pa",

          "Poverty (international poverty line)"     = "res_pov_tab",

          "Income distribution"     = "res_poverty_density_tab",
          "Inequality"     = "res_ineq_tab",


          "Net cash position"          = "ncp_panel",

          "<hr class=\"hr-small-line\"/>" = "hr1",
          "Incidences:" = "ins",
          "Direct taxes"               = "direct_taxes_panel",
          "Indirect taxes" = "indirect_taxes_panel",
          "Direct transfers"           =  "direct_transfer_panel",
          "Subsidies"           =  "subsidies_panel",
          "Benefits in-kind"           =  "inkind_mod_panel",
          "Pensions benefits" = "contributions_pensions_panel",
          "<hr class=\"hr-small-line\"/>" = "hr1",
          "Semantic model"     = "diagram_panel",
          "Geospatial results"     = "map_panel"
        ),
        direction = "vertical",
        justified = TRUE,
        width = "100%"
      )#,
      # mod_results_btns_ui(id)
    ) %>%
    wellPanel() %>%
    column(width = 3)

  right_col <-
    column(
      9,
      tabsetPanel(
        tabPanelBody(value = "res_pov_tab", local_gini_pov_ui1(ns("gin-pov-srv"))),
        tabPanelBody(value = "res_pov_tab_pa", local_gini_pov_ui_pa(ns("gin-pov-srv-pa"))),

        tabPanelBody(value = "res_poverty_density_tab", density_poverty_ui(ns("povdens"))),
        tabPanelBody(value = "res_ineq_tab", local_gini_pov_ui2(ns("gin-pov-srv2"))),
        tabPanelBody(value = "ncp_panel", mod_incidences_ui(ns("ncp_dtx"))),
        tabPanelBody(value = "direct_taxes_panel", mod_incidences_ui(ns("direct_taxes"))),
        tabPanelBody(value = "indirect_taxes_panel", mod_incidences_ui(ns("indirect_taxes"))),
        tabPanelBody(value = "direct_transfer_panel", mod_incidences_ui(ns("direct_transfer"))), #am_tot1
        tabPanelBody(value = "contributions_pensions_panel", mod_incidences_ui(ns("contributions_pensions"))),
        tabPanelBody(value = "inkind_mod_panel", mod_incidences_ui(ns("inkind_mod"))),
        tabPanelBody(value = "subsidies_panel", mod_incidences_ui(ns("subsidies"))),
        tabPanelBody(value = "diagram_panel", mod_diagram_ui(ns("diagram"))),
        tabPanelBody(value = "map_panel", mod_map_ui(ns("map"))),


        #
        # tabPanelBody(value = "ncp_panel", mod_incidences_ui(ns("ncp"))),

        id = ns("results_tab"),
        selected = "results_panel1",
        type = c("hidden"),
        header = NULL,
        footer = NULL
      ))

  extra_css <-

    tags$head(
      tags$style(HTML("

.tooltip-disabled {
  position: relative;
  display: inline-block;
}

.tooltip-enabled {
  position: relative;
  display: inline-block;
}

.tooltiptext {visibility: hidden;}
.tooltip-disabled .tooltiptext {
  visibility: hidden;
  width: 120px;
  background-color: black;
  color: #fff;
  text-align: center;
  border-radius: 6px;
  padding: 5px 0;

  /* Position the tooltip */
  position: absolute;
  z-index: 1;
}

.tooltip-disabled:hover .tooltiptext {
  visibility: visible;
}

    "))
    )

  tabPanel(title = "Results",
           # res_content, # uncomment this to add the option to save raw results
           #if (golem::app_dev()) {res_content},
           extra_css,
           tags$head(tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js")),
           fluidRow(left_col, right_col))

}


#' results Server Functions
#'
#' @noRd
#' @export
mod_results_server <- function(id,
                                   sim_res = reactive(list()),
                                   postsim_res = reactive(list()),
                                   active_tab = reactive(NULL),
                                   ...) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns



    observeEvent(
      sim_res(),
      {
        # browser()
        # res_list <- reactiveValuesToList(sim_res)
        if (!isTruthy(sim_res())) {
          shinyjs::addClass(selector = "#main_sidebar li:nth-child(3)", class = "tooltip-disabled")
          shinyjs::html(selector = "#main_sidebar li:nth-child(3)",
                        html = '<span class="tooltiptext" id="ToolTipContent">Press Run to generate results.</span>',
                        add = TRUE,
                        asis = TRUE)
          shinyjs::disable(selector = "#main_sidebar li:nth-child(3)")

        } else {
          removeUI(selector = "#ToolTipContent")
          shinyjs::removeClass(selector = "#main_sidebar li:nth-child(3)", class = "tooltip-disabled")
          shinyjs::addClass(selector = "#main_sidebar li:nth-child(3)", class = "tooltip-enabled")
          shinyjs::enable(selector = "#main_sidebar li:nth-child(3)")
        }
      }, ignoreInit = FALSE, ignoreNULL = FALSE
    )

    # Switching tabs ---------------------------------------------------
    observe({
      req(active_tab())
      shinyWidgets::updateRadioGroupButtons(
        session,
        "results_tab_choice",
        disabledChoices = c("ins", "hr1", "hr2"))
    })

    ### TAB switch logic ----------------------------------------------
    observe({
      req(input$results_tab_choice)
      shiny::updateTabsetPanel(session, "results_tab",
                               selected = input$results_tab_choice)
    })

    # Error message -------------------------------------------
    sim_res_local <- reactive({
      validate(need(sim_res(),
                    "Click `Run` on the policy choices tab to see the results."))
      # browser()
      # readr::write_rds(sim_res(), "data-temp/sim_res_2.rds")
      sim_res()
    })

    # DEBUGGING - save policy results locally -------------------------------
    observeEvent(input$save, {
      req(sim_res())
      dir.create("data-temp", showWarnings = F)
      readr::write_rds(sim_res(), "data-temp/sim_res.rds")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input$browse, {
      req(sim_res())
      # golem::browser_dev({
      #   sim_res()
      #
      # })
      # browser()
      # dir.create("data-temp", showWarnings = F)
      # readr::write_rds(sim_res(), "data-temp/sim_res.rds")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Poverty and Inequality ------------------------------------------------
    local_gini_pov_server("gin-pov-srv", sim_res = sim_res_local, ...)
    local_gini_pov_server_pa("gin-pov-srv-pa", sim_res = sim_res_local, ...)

    local_gini_pov_server2("gin-pov-srv2", sim_res = sim_res_local, ...)


    # Net Cash Position -----------------------------------------------------
    # local_mod_ncp_ser(
    #   id = "ncp",
    #   sim_res = sim_res_local,
    #   dec_vars = c(
    #     "dtx_total",
    #     "itx_total",
    #     "pen"
    #     )
    #   )


    # Net Cash Position -----------------------------------------------------
    #mod_incidences_server(
    local_mod_ncp_ser(
      id = "ncp_dtx",
      sim_res = sim_res_local,
      dec_vars = c(
        "tot_dtaxes_pc", "tot_itaxes_pc", "tot_pensions_pc", "tot_dtransfer_pc",
        "tot_subsidies_pc", "tot_education_pc", "tot_health_pc") ,
      make_bar_fn = local_make_ncp_dta)

    # # incidences ------------------------------------------------------------
    local_mod_inc_ser(id = "direct_taxes", sim_res = sim_res_local,
                          dec_vars = c( "tot_dtaxes_pc",
                                        "dtx_pitx_pc",
                                       "dtx_prop_pc"),
                      tab_title = "Direct taxes",
                      title_for_page = "Relative and absolute incidence for direct taxes for different income concepts")


    local_mod_inc_ser(id = "indirect_taxes", sim_res = sim_res_local,

                      dec_vars = c("tot_itaxes_pc",
                                   "itx_vatx_pc",
                                   "itx_excs_pc"),
                      tab_title = "Indirect taxes",
                      title_for_page = "Relative and absolute incidence for indirect taxes for different income concepts")

    local_mod_inc_ser(id = "direct_transfer", sim_res = sim_res_local,
                      dec_vars = c("tot_dtransfer_pc",
                                   "dtr_tsat_pc",
                                   "dtr_lbrd_pc",
                                   "dtr_idpt_pc",
                                   "dtr_pcon_pc",
                                   "dtr_othr_pc"),
                      title_for_page = "Relative and absolute incidence for direct transfers for different income concepts")

    local_mod_inc_ser(id = "subsidies", sim_res = sim_res_local,
                      dec_vars = c("tot_subsidies_pc",
                                   "sub_elec_pc",
                                   "sub_gass_pc",
                                   "sub_watr_pc",
                                   "sub_trns_pc"),
                      tab_title = "Subsidies",
                      title_for_page = "Relative and absolute incidence for subsidies for different income concepts")


    local_mod_inc_ser(id = "contributions_pensions", sim_res = sim_res_local,
                      dec_vars = c("pen_pens_pc"),
                      tab_title = "Pension penefits",
                      title_for_page = "Relative and absolute incidence for pension benefits for different income concepts")


    local_mod_inc_ser(id = "inkind_mod", sim_res = sim_res_local,
                      dec_vars = c("tot_education_pc", "tot_health_pc", "edu_pesc_pc", "edu_genr_pc",
                                   "edu_tert_pc", "edu_voca_pc"),
                      tab_title = "Benefits in-kind",
                      title_for_page = "Relative and absolute incidence for in-kind benefits for different income concepts")


    # local_poverty_density_server_step1(id = "pov-density-server", sim_res = sim_res_local)
    mod_poverty_density_server(id = "povdens", sim_res = sim_res_local)


    mod_diagram_server(id = "diagram", sim_res = sim_res_local)



    # Observe changes in the selected tab
    observeEvent(input$results_tab_choice, {
      # Check if the selected tab is 'map_panel'
      if (input$results_tab_choice == "map_panel") {
        # Execute the map server module only when the map panel is active
        mod_maps_server(id = "map", sim_res = sim_res_local)
      }
    }, ignoreInit = TRUE)

  })
}

# mod_incidences_server
## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")

#' Tests a simple results page module providing it with the sim_res
#'
#' @export
test_results_mod <-
  function(sim_res = NULL,
           ui_side,
           server_side,
           id = "testid") {
    ui <- fluidPage(ui_side(id = id))
    srv <-
      function(input, output, session) {
        server_side(id = id, sim_res = reactive(sim_res))
      }
    shinyApp(ui, srv)

  }
