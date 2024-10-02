local_make_bar_dta <-
  function(dta, ...) {
    make_bar_dta_updated(
      dta = dta,
      plot_types = c("relative", "absolute", "level"),
      title_glue = "Relative incidence (% of {income_type}) and absolute (% of all deciles)",
      x_title = "deciles of {income_type}",
      y_titles =
        list(
          relative = "Relative incidence (% of {income_type})",
          absolute = "Absolute incidence (% of all deciles)",
          level = "Lari"
        ),
      digits = 2,
      ...
    )

  }


local_mod_inc_ser <- function(id, sim_res, dec_vars, tab_title = NULL, ...) {
 mod_incidences_server_updated(
 # mod_incidences_server(
    id = id,
    sim_res = sim_res,
    dec_vars = dec_vars,
    make_bar_fn = local_make_bar_dta,
    tab_title = tab_title,
    ...
  )
}

# NCP ---------------------------------------------------------------------
local_agg_ncp_total <- function(dta, ...) {


  # This unfortunatly does not work yet
  # if(input$by_income == "yp_pc"){
  #   dta <- dta %>%
  #     filter(!Source %in%
  #              get_var_nm(
  #                c(
  #                  "tot_pensions_pc"
  #                )
  #              )$var_title)
  #
  # }


  dta_1<- dta %>%
    dplyr::group_by(Decile, Income_value, Income, Simulation, name) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Source = "Net fiscal position")

  dta_2 <- dta %>%
    dplyr::filter( !Source %in% c("Education", "Health")) %>%
   #dplyr::filter(!Source %in% get_var_nm(c("tot_education_pc", "tot_health_pc"))$var_title) %>%
    dplyr::group_by(Decile, Income_value, Income, Simulation, name) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Source = "Net cash position")

 dplyr::bind_rows(dta_1, dta_2)
}
# get_var_nm(c("tot_education_pc", "tot_health_pc"))

local_make_ncp_dta <-
  function(dta, ...) {
    make_nct_dta_updated(
   # make_nct_dta(
      dta = dta,
      plot_types = c("relative", "absolute", "level"), #, "level"),
      title_glue =  "Net cash position by deciles of {income_type}",
      x_title = "Deciles of {income_type}",
      y_titles =
        list(relative = "Relative incidence (% of {income_type})",
             absolute = "Absolute incidence (% of all deciles)",
             level = "Lari"),
      ncp_agg_fn = local_agg_ncp_total,
      digits = 2,
      ...
    )
  }
# ## replace this to have access to the income concepts
# local_mod_ncp_ser <- function(id, sim_res, dec_vars, ...) {
#   mod_incidences_server(
#     id = id,
#     sim_res = sim_res,
#     dec_vars = dec_vars,
#     make_bar_fn = local_make_ncp_dta,
#     n_dec_label = "Number of deciles",
#     dec_by_label = "Déciles par:",
#     ...
#   )
# }



local_mod_ncp_ser <-
  function (id,
            sim_res,
            dec_vars = get_var_nm()$var,
            make_bar_fn = local_make_ncp_dta,
            n_dec_label = "Number of deciles",
            dec_by_label = "Deciles by:",
            ...)
  {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      output$incidence_ui <- renderUI({
        req(sim_res())
        tagList(
          uiOutput(ns("title")),
          fluidRow(
            column(
              2,
              numericInput(
                ns("n_dec"),
                label = n_dec_label,
                value = 5,
                min = 1,
                max = 100
              )
            ),
            column(
              3,
              selectInput(
                ns("by_income"),
                label = dec_by_label,
                choices = purrr::set_names(get_inc_nm() %>%
                                      filter(var %in% c("yp_pc",
                                                        "ym_pc")) %>%
                                      pull(var),
                                    get_inc_nm() %>%
                                      filter(var %in% c("yp_pc",
                                                        "ym_pc")) %>%
                                      pull(var_title) ),
                selected = NULL,
                width = "350px"
              )
            ),
            column(7,
                   uiOutput(ns(
                     "radioGroupButtons"
                   )))
          ),
          tags$div(
            tags$div(
              shinyWidgets::dropMenu(
                shinyWidgets::actionBttn(
                  inputId = ns("ddmenue"),
                  style = "simple",
                  size = "xs",
                  color = "primary",
                  icon = icon("bars")
                ),
                actionButton(
                  inputId = ns("more"),
                  label = "Save plot",
                  icon = icon("download"),
                  class = "btn-sm"
                )
              ),
              style = htmltools::css(
                position = "absolute",
                top = 0,
                left = "5px",
                zIndex = 30
              )
            ),
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns(
              "incidence_ly"
            ))),
            style = htmltools::css(
              position = "relative",
              width = htmltools::validateCssUnit("100%"),
              height = htmltools::validateCssUnit("400px")
            )
          ),
          tags$hr(),
          column(12, shinycssloaders::withSpinner(DT::DTOutput(
            ns("incidence_dt")
          ))),
          tags$hr()
        )
      })
      n_dec_debounced <- reactive(req(input$n_dec)) %>% debounce(1000)
      incidence_esimates <- reactive({
        req(sim_res())
        req(n_dec_debounced())
        req(input$by_income)

        if(input$by_income == "yp_pc") {
          dec_vars_local <- dec_vars[dec_vars != "tot_pensions_pc"]
        } else {
          dec_vars_local <- dec_vars
        }
        out <- try({
          sim_res() %>% agg_sims_by_deciles(
            dec_by = input$by_income,
            dec_vars = dec_vars_local,
            wt_var = get_wt_nm(),
            n_dec = n_dec_debounced(),
            get_var_fn = get_var_nm,
            ...
          ) %>%
            make_bar_fn()
        }, silent = T)
        out %>% validate_result(str_c("Error occured when estimating the incidencesin in ",
                                      ns("")))
      })
      output$title <- renderUI({
        req(incidence_esimates())
        incidence_esimates()$title %>% h3()
      })
      plt_indx_upd <- reactiveVal(NULL)
      radioGroupButtons_ui <- reactiveVal(NULL)
      observe({
        req(incidence_esimates()$plt_indx)
        isolate({
          if (!isTruthy(plt_indx_upd())) {
            shinyWidgets::radioGroupButtons(
              inputId = ns("plot_var"),
              label = NULL,
              choices = incidence_esimates()$plt_indx[c(1, 3)], # this is a simple way to drop the absolute indices. (bit of an inefficient solution, as they are still computed, just the ui is supressed)
              selected = incidence_esimates()$plt_indx[[1]],
              status = "primary",
              size = "sm",
              individual = TRUE,
              direction = "horizontal",

            ) %>% radioGroupButtons_ui()
            plt_indx_upd(incidence_esimates()$plt_indx)
          }
        })
      })
      output$radioGroupButtons <- renderUI({
        req(radioGroupButtons_ui())
      })
      cur_plot_var <- reactive({
        if (!isTruthy(input$plot_var)) {
          return(dec_vars[[1]])
        }
        else {
          return(input$plot_var)
        }
      })
      output$incidence_ly <- renderPlotly({
        incidence_esimates()$ly[[cur_plot_var()]] %>% validate_result() %>%
          plotly_config()
      })
      output$incidence_dt <- DT::renderDT({
        dta <- incidence_esimates()$tbl_dt
        dta %>% validate_result() %>% fct_config_export_dt_updated("Incidences")
      }, server = FALSE)
      rv <- reactiveValues(plot = NULL)
      observe({
        req(input$plot_var)
        req(incidence_esimates()$gg[[input$plot_var]])
        rv$plot <- incidence_esimates()$gg[[input$plot_var]]
      })
      observeEvent(input$more, {
        esquisse::save_ggplot_modal(id = session$ns("export"),
                                    title = "Export chart")
      })
      save_ggplot_server2("export",
                          plot_rv = rv,
                          dpi = 450,
                          scale = 2)
      incidence_esimates
    })
  }
