# this function simply finds the 99th percentile of all incomes in all simulalations
# only use this to make the charts a bit more readible
get_max_99_quantile_income <- function(sim,
                                       income_vars_tbl = get_inc_nm(suffix = "_pa")  %>%
                                         filter(!var %in%  c( "yg_pa", "yf_pa"))){


  sim %>% purrr::map(~{.x$policy_sim_raw}%>%
                       select(dplyr::any_of(income_vars_tbl$var)) %>%
                       pivot_longer(cols = everything()) %>%
                       pull(value) %>%
                       quantile( 0.99, na.rm = TRUE)) %>%
    bind_cols() %>%
    max()


}


# This function calculated the weighted density for the income
calculate_density_f <- function(x,
                                df = dta_long,
                                pl_var = "pl_var",
                                value_var = "value",
                                var_var = var,
                                weight_var = "weight"  ){
  density_data <-df %>%
    filter({{var_var}} == x)

  poverty_line <- df[[pl_var]][1]


  dens <- density(density_data[[value_var]] ,
                  weights = density_data[[weight_var]]/sum(density_data[[weight_var]]),
                  type ='l',
                  bty = 'n')
  dens_tibble <- tibble::tibble(x_dens = dens$x,
                                y_dens = dens$y) %>%
    mutate(below_poverty_line = x_dens < poverty_line) %>%
    mutate(var = x) %>%
    mutate(poverty_line = poverty_line)
  dens_tibble
}


# now the densities are calculated (per income and per simulation)#
# I also join the estimated poverty levels, as I want to colour those left of the povertly line
# a different shade
create_data_for_density_plots_f <- function(current_dta,
                                            log = FALSE,
                                            policy_name = NA_character_,
                                            income_vars_tbl = get_inc_nm(suffix = "_pa")  %>%
                                              filter(!var %in%  c( "yg_pa", "yf_pa")),
                                            poverty_line_var = "pline_mod",
                                            wt_var = get_wt_nm()){

  wt_var_sym <- sym(wt_var)
  poverty_line_var_2 <- sym(poverty_line_var)


  dta         <- current_dta
  dta         <- mutate(dta, pl_var = {{poverty_line_var_2}})

  dta_long <- dta %>% dplyr::select(dplyr::any_of(income_vars_tbl$var),
                                    dplyr::any_of("pl_var"),
                                    dplyr::any_of(wt_var)) %>%
    tidyr::pivot_longer(names_to = "var",
                        cols = any_of(income_vars_tbl$var))


  if(log){
    dta_long <- dta_long %>%
      mutate(value = value +1) %>%
      mutate(value = pmax(value, 1)) %>%
      mutate(value = log10(value)) %>%
      mutate(pl_var = log10(pl_var))
  }


  poverty_rates <- dta_long %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(dplyr::across(
      value,
      list(
        rate = ~ calc_pov_fgt(
          x = .,
          pl = pl_var,
          alpha = 0,
          w = !!wt_var_sym,
          na.rm = TRUE
        ) * 100
      ),
      .names = "{.fn}"
    ))


  suppressWarnings({result <- map(poverty_rates$var,
                                  calculate_density_f,
                                  df = dta_long) %>%
    bind_rows() %>%
    mutate(Simulation = policy_name %>% forcats::fct_drop())%>%
    # for now just adding it, so that I dont need to create yet another function
    left_join(poverty_rates, by = join_by(var))})

  result
}


# This is where I am lost. I need to add the computation to the shiny server
# local_density_poverty_ui <- function(id, ...) {
#   ns <- NS(id)
#   column(12, density_poverty_ui(ns("pov_density_server")))#,
# }


density_poverty_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(
      uiOutput(ns("pov_density_ui"))
    )
  ) |>
    shiny::column(width = 12)
}

# local_poverty_density_server_step1 <-
#   function(id, sim_res, ...) {
#     moduleServer(id, function(input, output, session) {
#       ns <- session$ns
#       # local_poverty_density_server_step2("dens-srv", sim_res = sim_res_local, ...)
#       mod_poverty_density_server("dens-srv", sim_res = sim_res_local, ...)
#
#     })
#   }


# local_poverty_density_server_step2 <-
#   function(id, sim_res, ...) {
#     mod_poverty_density_server(id,
#                                sim_res = sim_res,
#                                title = "Density Povery",
#                                pl_title = "Percent under Poverty line",
#                                ...)
#   }

#' @import dplyr purrr glue
mod_poverty_density_server <-
  function(id,
           sim_res = sim_res,
           title = "Density Distribution of People under Poverty Line (per adult equivalent)",
           pl_title = "Percent under Poverty line",

           ...) {

    moduleServer( id, function(input, output, session){
      ns <- session$ns

      output$pov_density_ui <- renderUI({ # change this
        validate(
          need(sim_res(), "Click `Run` on the policy choices tab to see the results.")
        )
        req(sim_res())


        tagList(
          tags$h4(title),
          fluidRow(
            column(
              12,
              checkboxInput( ns("log_scale"),   label = "transforms to log scale ", value = FALSE, width = NULL)

            )),

          tags$div(
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns("density_plot"),
                                                              height = "800px")),
            style = htmltools::css(
              position = "relative",
              width = htmltools::validateCssUnit("100%"),
              height = htmltools::validateCssUnit("800px")
            )
          ),

          tags$hr()
        )
      })

      stat_esimates <-
        reactive({
          req(sim_res())
          results <- sim_res() %>%
            purrr::map(~{
            create_data_for_density_plots_f(current_dta = .x$policy_sim_raw, policy_name = .x$policy_name)
          }) %>%
            bind_rows() %>%
            left_join(get_inc_nm(suffix = "_pa")  %>%
                        filter(!var %in%  c( "yg_pa", "yf_pa"))
                      %>% select(-factor),
                      by = join_by(var))


          results_log <- sim_res() %>%
            purrr::map(~{
              create_data_for_density_plots_f(current_dta = .x$policy_sim_raw, policy_name = .x$policy_name, log = TRUE)
            }) %>%
            bind_rows() %>%
            left_join(get_inc_nm(suffix = "_pa")  %>%
                        filter(!var %in%  c( "yg_pa", "yf_pa"))
                      %>% select(-factor),
                      by = join_by(var))






          if(input$log_scale){
            # calculate labels

            labels_plot <- results %>%
              group_by(var_title , Simulation) %>%
              summarise(poverty_line = round(poverty_line [1], 1),
                        percent = round(rate[1], 1)) %>%
              ungroup()

            labels_plot <- labels_plot  %>%
              mutate(x = 1,
                     y = max(results_log$y_dens) * 0.9) %>%
              mutate(label = glue::glue("{format(percent, nsmall = 1)}% below\nthe poverty line of\n{poverty_line}"))


            ggplot_density_poverty <-  results_log %>%
              ggplot(aes(x_dens,
                         y_dens )) +
              facet_grid(Simulation~var_title , scales = "free_x") +
              geom_area(aes(alpha = below_poverty_line,
                            fill = Simulation),
                        position = "dodge") +
              geom_vline( data = labels_plot,
                          aes(xintercept = log10(poverty_line)),
                          linetype = "dashed") +
              scale_alpha_manual(values = c(0.2, 0.8)) +
              theme_minimal() +
              ylab("Density")+
              xlab("Income") +
              theme(legend.position = "none",
                    axis.text.y = element_blank(),
                    strip.text = element_text(size = 12,
                                              face = "bold")
              ) +
              ggthemes::scale_fill_colorblind()+
              scale_x_continuous(breaks = c(log10(1), log10(100), log10(1000), log10(10000)),
                                 labels = c("0", "100", "1K","10K")
                                 #labels = scales::unit_format(
                                  # unit = "k",
                                  # scale = 1e-3,
                                   #accuracy = 1),
                                 )+
              coord_cartesian(xlim = c(0, NA))


              geom_text(data = labels_plot, aes(x = x,
                                                y = y,
                                                label = label),
                        vjust = 0,
                        hjust = 0,
                        size = 3)
          }else{

            labels_plot <- results %>%
              group_by(var_title , Simulation) %>%
              summarise(poverty_line = round(poverty_line [1], 1),
                        percent = round(rate[1], 1)) %>%
              ungroup() %>%
              mutate(x = get_max_99_quantile_income(sim_res()) *0.9,
                     y = max(results$y_dens) * 0.9) %>%
              mutate(label = glue::glue("{format(percent, nsmall = 1)}% below\nthe poverty line of\n{poverty_line}"))


            ggplot_density_poverty <-  results %>%
              ggplot(aes(x_dens,
                         y_dens ))+
              facet_grid(Simulation~var_title , scales = "free_x") +
              geom_area(aes(alpha = below_poverty_line,
                            fill = Simulation),
                        position = "dodge") +
              geom_vline( data = labels_plot,
                          aes(xintercept = poverty_line ),
                          linetype = "dashed") +
              scale_alpha_manual(values = c(0.2, 0.8)) +
              theme_minimal() +
              ylab("Density")+
              xlab("Income") +
              theme(legend.position = "none",
                    axis.text.y = element_blank(),
                    strip.text = element_text(size = 12,
                                              face = "bold")
              )+
              ggthemes::scale_fill_colorblind()+
              geom_text(data = labels_plot, aes(x = x,
                                                y = y,
                                                label = label),
                        size = 3)+

              coord_cartesian(xlim = c(NA, get_max_99_quantile_income(sim_res())*1.5))

          }





          plot_ly <- ggplotly(ggplot_density_poverty,  tooltip = "text")

          list(
            ly = plot_ly
          )
        })

      # Plotly plotting ---------------------------------------------------------
      output$density_plot <- plotly::renderPlotly({
        stat_esimates()$ly
      })

      # stat_esimates
    })
  }



#' Title create_diagram
#' @noRd
#' @import glue dplyr purrr
create_diagram <- function(number, sim_res){

  policy_choices_baseline <-  sim_res[[1]]$policy_choices
  policy_choices <-  sim_res[[number]]$policy_choices

  title <-  sim_res[[number]]$policy_name


  inp_xlsx_baseline <- as.data.frame(policy_choices_baseline) %>%
    pivot_longer(cols = everything(), names_to = "inputId_baseline", values_to = "value_baseline")

  inp_xlsx<- as.data.frame(policy_choices) %>%
    pivot_longer(cols = everything(), names_to = "inputId")


  policy_name = NA_character_
  income_vars_tbl = income_vars_tbl = get_inc_nm(suffix = "_pa")  %>%
    filter(!var %in%  c( "yg_pa", "yf_pa"))
  poverty_line_var = "pline_mod"
  wt_var = get_wt_nm()


  wt_var_sym <- sym(wt_var)
  poverty_line_var_2 <- sym(poverty_line_var)

  dta         <- sim_res[[number]]$policy_sim_raw
  dta         <- mutate(dta, pl_var = {{poverty_line_var_2}})


  dta_baseline         <- sim_res[[1]]$policy_sim_raw
  dta_baseline         <- mutate(dta_baseline, pl_var = {{poverty_line_var_2}})

  dta_long <- dta %>% dplyr::select(dplyr::any_of(income_vars_tbl$var),
                                    dplyr::any_of("pl_var"),
                                    dplyr::any_of(wt_var)) %>%
    tidyr::pivot_longer(names_to = "var",
                        cols = any_of(income_vars_tbl$var))


  dta_baseline_long <- dta_baseline %>% dplyr::select(dplyr::any_of(income_vars_tbl$var),
                                                      dplyr::any_of("pl_var"),
                                                      dplyr::any_of(wt_var)) %>%
    tidyr::pivot_longer(names_to = "var",
                        cols = any_of(income_vars_tbl$var))


  title_direct_transfers <- "direct transfers"
  title_direct_taxes <- "personal income tax brackets"
  title_capital_income <- "capital income rate"
  title_housemk_value <- 'housing value taxes'
  title_landtax <- 'land taxes'

  inputs_short_form <- readr::read_csv(
    system.file("extdata", "inputs_short_form.csv", package = "GeoappPackage")
  )

 # inputs_short_form <- readr::read_csv("./inst/extdata/inputs_short_form.csv")

  table_direct_tranfers_vector <- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId  %in% c("simu_tsa_value",
                           "tsa_under_16",
                           "tsa_over_16",
                           "simu_tsa_benef",
                           "benef_q_1",
                           "benef_q_2",
                           "benef_q_3",
                           "benef_q_4",
                           "benef_q_5")) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline  %in% c("simu_tsa_value",
                                                "tsa_under_16",
                                                "tsa_over_16",
                                                "simu_tsa_benef",
                                                "benef_q_1",
                                                "benef_q_2",
                                                "benef_q_3",
                                                "benef_q_4",
                                                "benef_q_5")),
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(value = ifelse(inputId %in% c( "benef_q_1",
                                          "benef_q_2",
                                          "benef_q_3",
                                          "benef_q_4",
                                          "benef_q_5"),
                          as.character(as.logical(value)),
                          as.character(value))) %>%
    mutate(value = case_when((inputId == "simu_tsa_value" & value == "0") ~ "use survey data",
                             (inputId == "simu_tsa_value" & value == "1") ~ "use simulated values",
                             (inputId == "simu_tsa_value" & value == "2") ~ "use multiplier",
                             TRUE ~ value)) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short} </TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")




  complete_table_direct_tranfers <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'><TR><TD ALIGN='CENTER' COLSPAN='2'><b>{title_direct_transfers}:</b></TD></TR>{table_direct_tranfers_vector}")


  # direct taxes
  table_direct_taxes_1 <- inp_xlsx %>%
    select(inputId, value) %>%
    filter( inputId %in% c("pitw_rate_0","pitw_rate_1",
                           "pit1","pitw_rate_2","pit2","pitw_rate_3"   ,
                           "pit3","pitw_rate_4","pit4","pitw_rate_5" ,
                           "pit5","pitw_rate_6","pit6","pitw_rate_7" ,
                           "pit7","pitw_rate_8","pit8" ))




  table_direct_taxes_1_wide <- table_direct_taxes_1 %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter( inputId_baseline %in% c("pitw_rate_0","pitw_rate_1",
                                                "pit1","pitw_rate_2","pit2","pitw_rate_3"   ,
                                                "pit3","pitw_rate_4","pit4","pitw_rate_5" ,
                                                "pit5","pitw_rate_6","pit6","pitw_rate_7" ,
                                                "pit7","pitw_rate_8","pit8" )),
              by = c("inputId" = "inputId_baseline")
    ) %>%
    add_row(inputId  = "pit0", value = NA,  value_baseline = NA  )  %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              (is.na(value_baseline ) &inputId  %in% c("pitw_rate_1",
                                                                       "pitw_rate_2",
                                                                       "pitw_rate_3",
                                                                       "pitw_rate_4",
                                                                       "pitw_rate_5"
                              ) & value > 0.2) ~ "'blue'",
                              (is.na(value_baseline ) &inputId  %in% c("pitw_rate_1",
                                                                       "pitw_rate_2",
                                                                       "pitw_rate_3",
                                                                       "pitw_rate_4",
                                                                       "pitw_rate_5"
                              ) & value < 0.2) ~ "'red'",
                              (is.na(value_baseline ) &inputId  %in% c("pitw_rate_1",
                                                                       "pitw_rate_2",
                                                                       "pitw_rate_3",
                                                                       "pitw_rate_4",
                                                                       "pitw_rate_5"
                              ) & value > 0.2) ~ "'black'",
                              (is.na(value_baseline ) &!inputId  %in% c("pitw_rate_1",
                                                                        "pitw_rate_2",
                                                                        "pitw_rate_3",
                                                                        "pitw_rate_4",
                                                                        "pitw_rate_5"
                              )) ~ "'black'",

                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     (is.na(value_baseline ) &inputId  %in% c("pitw_rate_1",
                                                                              "pitw_rate_2",
                                                                              "pitw_rate_3",
                                                                              "pitw_rate_4",
                                                                              "pitw_rate_5"
                                     ) & value < 0.2) ~ "↓",
                                     (is.na(value_baseline ) &inputId  %in% c("pitw_rate_1",
                                                                              "pitw_rate_2",
                                                                              "pitw_rate_3",
                                                                              "pitw_rate_4",
                                                                              "pitw_rate_5"
                                     ) & value > 0.2) ~ "↑",
                                     (is.na(value_baseline ) &inputId  %in% c("pitw_rate_1",
                                                                              "pitw_rate_2",
                                                                              "pitw_rate_3",
                                                                              "pitw_rate_4",
                                                                              "pitw_rate_5"
                                     ) & value > 0.2) ~ "(=)",
                                     (is.na(value_baseline ) &!inputId  %in% c("pitw_rate_1",
                                                                               "pitw_rate_2",
                                                                               "pitw_rate_3",
                                                                               "pitw_rate_4",
                                                                               "pitw_rate_5"
                                     )) ~ "(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    mutate(value = ifelse(grepl("rate", inputId) &!is.na(value), paste0(round(value *100, 2), "%"), scales::comma(value))) %>%
    mutate(value = glue("{value}_{colour}_{symbol_change}")) %>%
    select(inputId  , value) %>%
    mutate(inputId = str_replace_all(inputId, "pitw_rate_", "rate_")) %>%
    mutate(inputId = str_replace_all(inputId, "pit", "thresshold_")) %>%
    separate(inputId, into = c("type", "bracket"), sep = "_", extra = "merge", fill = "right")  %>%
    pivot_wider(names_from = type,
                values_from = value) %>%
    mutate(rate = ifelse(rate == "NA_'black'_(=)", "-_'black'_(=)", rate)) %>%
    mutate(thresshold = ifelse(thresshold == "NA_'black'_(=)", "-_'black'_(=)", thresshold)) %>%
    separate(rate, into = c("rate", "rate_colour", "rate_symbol_change" ), sep = "_") %>%
    separate(thresshold, into = c("thresshold", "thresshold_colour", "thresshold_symbol_change"), sep = "_") %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'> {bracket}</TD><TD><FONT COLOR={thresshold_colour}>{thresshold} {thresshold_symbol_change}    </FONT></TD><TD><FONT COLOR={rate_colour}>{rate} {rate_symbol_change}</FONT></TD> </TR>") )



  table_direct_taxes_1_wide_vector <- table_direct_taxes_1_wide %>%
    filter(!(rate == "-" &thresshold  == "-" )) %>%
    select(label) %>%
    add_row(label = "<TR><TD ALIGN='LEFT'><b> bracket</b> </TD><TD> <b> threshold</b> </TD><TD>  <b>tax(%)</b></TD></TR>",
            .before = 1) %>%
    pull(label) %>%
    paste(collapse = "\n")



  table_other_direct_taxes_vector <- inp_xlsx %>%
    select(inputId, value) %>%
    filter( inputId %in% c("pits_rate" )) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline  %in% c("pits_rate")),
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(value = glue("{round(value *100, 1)} %")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}: <FONT COLOR={colour}>{value} {symbol_change}  </FONT></TD> <TD></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_income_taxes <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
       <TR><TD ALIGN='CENTER' COLSPAN='3'><b>{title_direct_taxes}:</b></TD></TR>{table_direct_taxes_1_wide_vector}<TR><TD></TD><TD></TD><TD></TD></TR>{table_other_direct_taxes_vector}")





  complete_table_other_income_taxes <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'><TR><TD ALIGN='CENTER' COLSPAN='2'>Other income taxes</TD></TR>{table_other_direct_taxes_vector}")


  table_capital_income_vector <- inp_xlsx %>%
    select(inputId, value) %>%
    filter( inputId %in% c("pitk_rate" )) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline  %in% c("pitk_rate")),
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(value = glue("{round(value *100, 1)} %")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}: <FONT COLOR={colour}>{value} {symbol_change}  </FONT></TD> <TD></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")



  complete_table_capital_income <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>{title_capital_income}:</b></TD></TR>{table_capital_income_vector}")


  table_house_mktvalue_vector <- inp_xlsx %>%
    select(inputId, value) %>%
    filter( inputId %in% c("ptax_thr1",
                           "ptax_thr2",
                           "ptax_r1l",
                           "ptax_r1u",
                           "ptax_r2l",
                           "ptax_r2u"))%>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline  %in% c("ptax_thr1",
                                                "ptax_thr2",
                                                "ptax_r1l",
                                                "ptax_r1u",
                                                "ptax_r2l",
                                                "ptax_r2u")),
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑"))  %>%
    mutate(value = case_when(inputId %in% c("ptax_r1l",
                                            "ptax_r1u",
                                            "ptax_r2l",
                                            "ptax_r2u") ~ glue("{round(value*100, 2)} %"),
                             inputId %in% c("ptax_thr1",
                                            "ptax_thr2") ~  scales::comma(value, accuracy = 1, suffix = " GEL"),
                             TRUE ~NA))%>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'><FONT COLOR={colour}>{value}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")


  complete_table_housing_value <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>{title_housemk_value}:</b></TD></TR>{table_house_mktvalue_vector}")



  table_landtax_vector <- inp_xlsx %>%
    select(inputId, value) %>%
    filter( grepl("larab_", inputId)| grepl("lpast_", inputId)  ) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(grepl("larab_", inputId_baseline)| grepl("lpast_", inputId_baseline)),
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑"))  %>%
    mutate(value = ifelse(is.na(value), "-", glue("{value}%"))) %>%
    mutate(value = glue(" <TD> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD>")) %>%
    select(inputId, value) %>%
    separate(col = inputId  , into = c("type", "number"), sep = "_") %>%
    pivot_wider(names_from = type,
                values_from = value) %>%
    rename(region = number) %>%
    mutate(region = as.character(region)) %>%




    mutate( region = case_match(
      region,
      "1" ~ "Kakheti",
      "2" ~ "Tbilisi",
      "3" ~ "Shida Kartli",
      "4" ~ "Kvemo Kartli",
      "5" ~ "Abkhazia (no data available)",
      "6" ~ "Samtskhe-Javakheti",
      "7" ~ "Tskhinvali Region/South Ossetia (no data available)",
      "8" ~ "Adjara A.R.",
      "9" ~ "Guria",
      "10" ~ "Samegrelo-Zemo Svaneti",
      "11" ~ "Imereti",
      "12" ~ "Mtskheta-Mtianeti",
      "13" ~ "region 12",
      "14" ~ "Racha-Lechkhumi and Kvemo Svaneti",
      .default = as.character(region)
    )) %>%
    mutate(label = glue("<TR><TD ALIGN='LEFT'>{region}</TD>{larab} {lpast}</TR>"))  %>%
    select(label) %>%
    add_row(label = "<TR><TD ALIGN='LEFT'><b>       region       </b></TD><TD>  <b>    arable land      </b></TD><TD> <b>      pastures </b>  </TD></TR>",
            .before = 1) %>%
    pull(label ) %>%
    paste0(collapse = "")


  complete_table_landtax <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='3'><b>{title_landtax}:</b></TD></TR>{table_landtax_vector}")


  table_excise_vector<- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("vat_rate",
                          "exc_rate_mobile",
                          "exc_rate_beer",
                          "exc_rate_vodka",
                          "exc_rate_wine",
                          "exc_rate_brandy",
                          "exc_rate_tobfil",
                          "exc_rate_tobunf",
                          "exc_rate_fuel_liquid",
                          "exc_rate_fuel_gas"
    )) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("vat_rate",
                                               "exc_rate_mobile",
                                               "exc_rate_beer",
                                               "exc_rate_vodka",
                                               "exc_rate_wine",
                                               "exc_rate_brandy",
                                               "exc_rate_tobfil",
                                               "exc_rate_tobunf",
                                               "exc_rate_fuel_liquid",
                                               "exc_rate_fuel_gas"
                )) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(value = case_when(is.na(value) ~ "-",
                             inputId %in% c("vat_rate",  "exc_rate_mobile") ~ glue("{round(value *100, 2)}%"),
                             TRUE ~  glue("{round(value * 100, 2)} GEL"))) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_table_excise<- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>excises:</b></TD></TR>{table_excise_vector}")



  table_elec_vector<- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("sub_rate_elec",
                          "sub_price_elec",
                          "sub_limit_elec"

    )) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("sub_rate_elec",
                                               "sub_price_elec",
                                               "sub_limit_elec"
                )) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_table_elec <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>electicity subsidies:</b></TD></TR>{table_elec_vector}")




  table_gas_vector<- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("sub_rate_gas",
                          "sub_price_gas",
                          "sub_simul_gas",
                          "sub_free_gas"

    ))  %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("sub_rate_gas",
                                               "sub_price_gas",
                                               "sub_simul_gas",
                                               "sub_free_gas"
                )) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_gas <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>gas subsidies:</b></TD></TR>{table_gas_vector}")

  table_water_vector<- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("sub_upbound_wat")) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("sub_upbound_wat"
                )) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%

    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")


  complete_table_water <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>water subsidies:</b></TD></TR>{table_water_vector}")


  table_transport_vector<- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("sub_rate_trans",
                          "sub_price_trans")) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("sub_rate_trans",
                                               "sub_price_trans"
                )) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_transport <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>transport subsidies:</b></TD></TR>{table_transport_vector}")

  table_health_vector<- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("total_health_benefits",
                          "health_ben_2022_1",
                          "health_ben_2022_2",
                          "health_ben_2022_3",
                          "health_ben_2022_4",
                          "health_ben_2022_5",
                          "health_ben_2022_6",
                          "health_ben_2022_7",
                          "health_ben_2022_8",
                          "health_ben_2022_9"
    )) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("total_health_benefits",
                                               "health_ben_2022_1",
                                               "health_ben_2022_2",
                                               "health_ben_2022_3",
                                               "health_ben_2022_4",
                                               "health_ben_2022_5",
                                               "health_ben_2022_6",
                                               "health_ben_2022_7",
                                               "health_ben_2022_8",
                                               "health_ben_2022_9"
                )) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%

    mutate(value = ifelse(inputId == "total_health_benefits",
                          scales::comma(value, accuracy =1),
                          as.character(value))) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")


  complete_table_health <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>health subsidies:</b></TD></TR>{table_health_vector}")


  table_poverty_vector <- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("nat_povline_monthly"
    ))%>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("nat_povline_monthly")) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId"))%>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_poverty <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>poverty line:</b></TD></TR>{table_poverty_vector}")


  table_education_vector<- inp_xlsx %>%
    select(inputId, value) %>%
    filter(inputId %in% c("pc_preschool",
                          "pc_general",
                          "pc_vocational",
                          "pc_higher",
                          "sh_priv_preschool",
                          "sh_priv_general",
                          "sh_priv_vocational",
                          "sh_priv_higher"

    )) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline %in% c("pc_preschool",
                                               "pc_general",
                                               "pc_vocational",
                                               "pc_higher",
                                               "sh_priv_preschool",
                                               "sh_priv_general",
                                               "sh_priv_vocational",
                                               "sh_priv_higher"

                )) ,
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    mutate(value = case_when(inputId %in% c( "sh_priv_preschool",
                                             "sh_priv_general",
                                             "sh_priv_vocational",
                                             "sh_priv_higher") ~ glue("{round(value*100, 2)} %"),
                             inputId %in% c( "pc_preschool",
                                             "pc_general",
                                             "pc_vocational",
                                             "pc_higher") ~  scales::comma(value, accuracy = 1, suffix = " GEL"),
                             TRUE ~NA)) %>%

    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_education <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>education subsidies:</b></TD></TR>{table_education_vector}")



  table_pension_income_vector <- inp_xlsx %>%
    select(inputId, value) %>%
    filter( inputId %in% c("sscw_rate",
                           "ssce_rate",
                           "sscs_rate",
                           "sscg1_rate",
                           "sscg2_rate",
                           "sscg1_limit",
                           "sscg2_limit"
    )) %>%
    left_join(inp_xlsx_baseline %>%
                select(inputId_baseline, value_baseline) %>%
                filter(inputId_baseline  %in%  c("sscw_rate",
                                                 "ssce_rate",
                                                 "sscs_rate",
                                                 "sscg1_rate",
                                                 "sscg2_rate",
                                                 "sscg3_rate",
                                                 "sscg4_rate",
                                                 "sscg5_rate",
                                                 "sscg1_limit",
                                                 "sscg2_limit",
                                                 "sscg3_limit",
                                                 "sscg4_limit",
                                                 "sscg5_limit",
                                                 "sscg_max")),
              by = c("inputId" = "inputId_baseline")) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value) ~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value) ~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    mutate(value = case_when(is.na(value)~ NA,
                             grepl("rate",inputId)~ paste0(round(value*100, 1), "%"),
                             grepl("limit",inputId)~ scales::comma(value, accuracy =1),
                             grepl("max",inputId)~ scales::comma(value, accuracy =1),
                             TRUE~as.character(value))) %>%
    left_join(inputs_short_form, by = c("inputId" = "para__inputId")) %>%

    mutate(value = ifelse(is.na(value ), "-", value)) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{short}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    pull(label ) %>%
    paste0(collapse = "")

  complete_table_pensions <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>pension contributions:</b></TD></TR>{table_pension_income_vector}")






  poverty_rates_table <- dta_long %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(dplyr::across(
      value,
      list(
        value = ~ calc_pov_fgt(
          x = .,
          pl = pl_var,
          alpha = 0,
          w = !!wt_var_sym,
          na.rm = TRUE
        ) * 100
      ),
      .names = "{.fn}"
    ))%>%
    left_join(dta_baseline_long %>%
                dplyr::group_by(var) %>%
                dplyr::summarise(dplyr::across(
                  value,
                  list(
                    value_baseline = ~ calc_pov_fgt(
                      x = .,
                      pl = pl_var,
                      alpha = 0,
                      w = !!wt_var_sym,
                      na.rm = TRUE
                    ) * 100
                  ),
                  .names = "{.fn}"
                )))%>%
    left_join(get_inc_nm(suffix = "_pa")) %>%
    arrange(var_title) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    mutate(value = glue("{round(value, 1)} %")) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{var_title}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") ) %>%
    add_row(label = "<TR><TD ALIGN='LEFT'><B> Income   </B> </TD><TD> <B> rate   </B> </TD></TR>",
            .before = 1)%>%
    pull(label ) %>%

    paste0(collapse = "")


  complete_table_poverty_rates <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='2'><b>Poverty rates (pa):</b></TD></TR>{poverty_rates_table}")




  inequality_rates_table <- local_get_dta_gini(dta = dta,
                                               policy_name = "" ) %>%
    rename(value = Value) %>%
    left_join(local_get_dta_gini(dta = dta_baseline,
                                 policy_name = "" ) %>%
                rename(value_baseline = Value)) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    mutate(value = glue(" <TD> <FONT COLOR={colour}>{round(value, 1)} {symbol_change}</FONT></TD>")) %>%
    select(Income , value, Parameter   ) %>%
    pivot_wider(names_from = Parameter,
                values_from = value) %>%
    arrange(Income) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{Income }</TD>{`Gini index`} {`Theil index`}  {`90/10 income ratio`}</TR>") )  %>%
    select(label) %>%
    add_row(label = "<TR><TD ALIGN='LEFT'><B> Income    </B></TD><TD> <B>  Gini Index  </B>  </TD><TD> <B>Theil Index    </B></TD><TD> <B>90/10 income ratio  </B></TD></TR>",
            .before = 1)%>%
    pull(label ) %>%
    paste0(collapse = "")


  complete_table_inequality_rates <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='3'><b>Inequality rates (pc):</b></TD></TR>{inequality_rates_table}")

  # dec_vars = c("tot_dtaxes",
  #              "tot_itaxes",
  #              "tot_pensions",
  #              "tot_dtransfer",
  #              "tot_subsidies")



  dec_vars = c(
    "tot_dtaxes_pc", "tot_itaxes_pc", "tot_pensions_pc", "tot_dtransfer_pc",
    "tot_subsidies_pc")


  ncp_rates_table <- dta %>% agg_by_deciles(dec_by =  "ym_pc",
                                            dec_vars = dec_vars,
                                            wt_var = get_wt_nm(),
                                            n_dec = 10,
                                            get_var_fn = get_var_nm) %>%
    dplyr::group_by(Decile, Income_value, Income, Simulation) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    select(Decile, Income, value) %>%
    bind_rows(dta %>% agg_by_deciles(dec_by =  "yf_pc",
                                     dec_vars = dec_vars,
                                     wt_var = get_wt_nm(),
                                     n_dec = 10,
                                     get_var_fn = get_var_nm) %>%
                dplyr::group_by(Decile, Income_value, Income, Simulation) %>%
                dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
                dplyr::ungroup() %>%
                select(Decile, Income, value)) %>%
    left_join(dta_baseline %>% agg_by_deciles(dec_by =  "ym_pc",
                                              dec_vars = dec_vars,
                                              wt_var = get_wt_nm(),
                                              n_dec = 10,
                                              get_var_fn = get_var_nm) %>%
                dplyr::group_by(Decile, Income_value, Income, Simulation) %>%
                dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
                dplyr::ungroup() %>%
                select(Decile, Income, value) %>%
                bind_rows(dta_baseline %>% agg_by_deciles(dec_by =  "yf_pc",
                                                          dec_vars = dec_vars,
                                                          wt_var = get_wt_nm(),
                                                          n_dec = 10,
                                                          get_var_fn = get_var_nm) %>%
                            dplyr::group_by(Decile, Income_value, Income, Simulation) %>%
                            dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
                            dplyr::ungroup() %>%
                            select(Decile, Income, value)) %>%
                rename(value_baseline = value)) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    mutate(value = glue(" <TD> <FONT COLOR={colour}>{scales::label_number(accuracy=.01,scale_cut = scales::cut_short_scale())(value)} {symbol_change}</FONT></TD>")) %>%
    select(Decile  , value, Income) %>%
    pivot_wider(names_from = Income,
                values_from = value) %>%
    mutate(label = glue(" <TR><TD ALIGN='LEFT'>{Decile }</TD>{`Market income` }   { `Final income`}</TR>") )  %>%
    select(label) %>%
    add_row(label = "<TR><TD ALIGN='LEFT'><B>  Decile   </B></TD><TD>  <B>Market income  </B></TD><TD> <B>   Final income  </B></TD></TR>",
            .before = 1)%>%
    pull(label ) %>%
    paste0(collapse = "")



  complete_table_ncp <- glue("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='-0.1' CELLPADDING='-0.5'>
      <TR><TD ALIGN='CENTER' COLSPAN='3'><b>Net cash position Levels (GEL):</b></TD></TR>{ncp_rates_table}")

  # # Income concepts
  # 1,        "ym"             ,  "Market income",
  # 1,        "yp"             ,  "Market income plus pensions",
  # 1,        "yd"             ,  "Disposable income" ,
  # 1,        "yc"             ,  "Consumable income" ,
  # # 1,        "yg"             ,  "Gross income" , asked to remove
  # 1,        "yf"             ,  "Final income" ,

  income_summaries_baseline <- dta_baseline %>%
    summarise(`median market income (pc)` = round(spatstat.univar::weighted.median(x = ym_pc, w = weight), 2),
              `median disposable income (pc)` =  round(spatstat.univar::weighted.median(x = yd_pc, w = weight), 2),
              `median consumable income (pc)` =  round(spatstat.univar::weighted.median(x= yc_pc, w = weight), 2),
              `median final income (pc)` =  round(spatstat.univar::weighted.median(x = yf_pc, w = weight), 2)) %>%
    pivot_longer(everything(), values_to = "value_baseline")

  income_summaries <- dta %>%
    summarise(`median market income (pc)` = round(spatstat.univar::weighted.median(x = ym_pc, w = weight), 2),
              `median disposable income (pc)` =  round(spatstat.univar::weighted.median(x = yd_pc, w = weight), 2),
              `median consumable income (pc)` =  round(spatstat.univar::weighted.median(x= yc_pc, w = weight), 2),
              `median final income (pc)` =  round(spatstat.univar::weighted.median(x = yf_pc, w = weight), 2)) %>%
    pivot_longer(everything() )



  income_summaries_tables<- income_summaries %>%
    left_join(income_summaries_baseline) %>%
    mutate(colour = case_when(value ==value_baseline ~"'black'",
                              is.na(value )~"'black'",
                              value < value_baseline ~"'red'",
                              value > value_baseline ~"'blue'")) %>%
    mutate(symbol_change = case_when(value ==value_baseline ~"(=)",
                                     is.na(value )~"(=)",
                                     value < value_baseline ~"↓",
                                     value > value_baseline ~"↑")) %>%
    mutate(label = glue("{name}: <FONT COLOR={colour}> {value} {symbol_change}</FONT>>") )


  market_income_label <-
    paste0(
      "<<FONT > <B>Market income </B></FONT><BR/>",
      income_summaries_tables %>%
        filter(name  == "median market income (pc)") %>%
        pull(label)
    )%>%
    paste0(collapse = "")

  disposable_income_label <-
    paste0(
      "<<FONT > <B>Disposable income </B></FONT><BR/>",
      income_summaries_tables %>%
        filter(name  == "median disposable income (pc)") %>%
        pull(label)
    )%>%
    paste0(collapse = "")

  consumable_income_label <-
    paste0(
      "<<FONT > <B>Consumable income </B></FONT><BR/>",
      income_summaries_tables %>%
        filter(name  == "median consumable income (pc)") %>%
        pull(label)
    )%>%
    paste0(collapse = "")

  final_income_label <-
    paste0(
      "<<FONT > <B>Final income </B></FONT><BR/>",
      income_summaries_tables %>%
        filter(name  == "median final income (pc)") %>%
        pull(label)
    )%>%
    paste0(collapse = "")


  # mutate(label = glue(" <TR><TD ALIGN='LEFT'>{name}</TD><TD ALIGN='RIGHT'> <FONT COLOR={colour}>{value} {symbol_change}</FONT></TD></TR>") )

  GEO_CEQ_WORKFLOW <- grViz(paste0("
  digraph {

  graph[splines = ortho,
  ordering = 'in',
  rankdir='LR',
  concentrate=true,
  labeljust= 'c',
  layout = dot,
  overlap =true,
  outputorder = nodesfirst,
  fontsize = 60, labelloc = t, label = '",title,"', fontname = Helvetica]
  node [shape = rectangle, style = filled, fillcolor = 'blanchedalmond', fontsize = 15,  fontname = Helvetica]
  edge[color = black, fontsize = 12, arrowhead=vee, minlen = 1]


    #data_source_1a [label = <<FONT > <B>Market income </B></FONT>>, shape = cylinder, fillcolor = '#90CDC3',  fontsize = 20]
    data_source_1a [label = ",market_income_label, " , shape = cylinder, fillcolor = '#90CDC3',  fontsize = 20]

    data_source_1 [label = 'direct transfers data', shape = cylinder, fillcolor = '#E8D6CF']
    data_source_1b [label = 'family size data', shape = cylinder, fillcolor = '#E8D6CF']

    process_1 [label = ",complete_table_direct_tranfers, " </TABLE>>]


    product_1 [label = 'simulated direct transfers data', shape = cylinder, fillcolor = 'lightsalmon']

   # data_source_1a -> process_1
    data_source_1b -> process_1
    data_source_1 -> process_1

    process_1 -> product_1

    data_source_2 [label = 'direct taxes data', shape = cylinder, fillcolor = '#E8D6CF']



    process_2 [label = ",complete_table_income_taxes, " </TABLE>>]
    #process_2b [label = ",complete_table_other_income_taxes, " </TABLE>>]


    #complete_table_other_income_taxes

    product_2 [label = 'simulated direct taxes data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_2 -> process_2
    #data_source_2 -> process_2b

    process_2 -> product_2


    data_source_3 [label = 'capital income data', shape = cylinder, fillcolor = '#E8D6CF']

    process_3 [label = ",complete_table_capital_income, " </TABLE>>]
    product_3 [label = 'simulated capitcal income data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_3 -> process_3
    process_3 -> product_3

    data_source_4 [label = 'housing value data', shape = cylinder, fillcolor = '#E8D6CF']

    process_4 [label = ",complete_table_housing_value, " </TABLE>>]
    product_4 [label = 'simulated housing value data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_4 -> process_4
    process_4 -> product_4


    data_source_5 [label = 'land value data', shape = cylinder, fillcolor = '#E8D6CF']

    process_5 [label = ",complete_table_landtax, " </TABLE>>]

    product_5 [label = 'simulated land tax data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_5 -> process_5

    process_5 -> product_5




    data_source_pensions [label = 'pension data', shape = cylinder, fillcolor = '#E8D6CF']

    process_pensions [label = ",complete_table_pensions, " </TABLE>>]

    product_pensions [label = 'simulated pension data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_pensions -> process_pensions

    process_pensions -> product_pensions

    data_source_6 [label = 'consumption data', shape = cylinder, fillcolor = '#E8D6CF']

    process_6 [label = ",complete_table_table_excise, " </TABLE>>]
    #product_6 [label = 'simulated consumption data', shape = cylinder, fillcolor = 'lightsalmon']


data_source_6 -> process_6
    process_7 [label = ",complete_table_table_elec, " </TABLE>>]
   # product_7 [label = 'simulated electicity consumption data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_6 -> process_7

    process_8 [label = ",complete_table_gas, " </TABLE>>]
   # product_8 [label = 'simulated gas consumption data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_6 -> process_8

    process_9 [label = ",complete_table_water, " </TABLE>>]
    #product_9 [label = 'simulated water consumption data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_6 -> process_9

    process_10 [label = ",complete_table_transport," </TABLE>>]
   # product_10 [label = 'simulated transport consumption data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_6 -> process_10


    product_11 [label = 'simulated consumption data', shape = cylinder, fillcolor = 'lightsalmon']

process_6 -> product_11
process_7 -> product_11
process_8 -> product_11
process_9 -> product_11
process_10 -> product_11


    #product_8 -> product_11
    #product_9 -> product_11
    #product_10 -> product_11


    data_source_12 [label = 'health data', shape = cylinder, fillcolor = '#E8D6CF']
    data_source_12a [label = 'out of pocket expenditure health data', shape = cylinder, fillcolor = '#E8D6CF']

    process_12 [label = ",complete_table_health, " </TABLE>>]
    product_12 [label = 'simulated health expenditure data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_12 -> process_12
    data_source_12a -> process_12


product_1 -> disposable_income
product_2 -> disposable_income
product_3 -> disposable_income
product_4 -> disposable_income
product_5 -> disposable_income
product_pensions -> disposable_income


   combined_health_data [shape=point,width=0.01,height=0.01];

    process_12 -> product_12


    data_source_13 [label = 'education data', shape = cylinder, fillcolor = '#E8D6CF']

    process_13 [label = ",complete_table_education, " </TABLE>>]
    product_13 [label = 'simulated education expenditure data', shape = cylinder, fillcolor = 'lightsalmon']

    data_source_13 -> process_13
    process_13 -> product_13



    data_source_14 [label = 'poverty data', shape = cylinder, fillcolor = '#E8D6CF']


    process_poverty [label = ",complete_table_poverty, " </TABLE>>]



    final_process  [label = <<FONT > <B> <BR/>  CEQ computations   <BR/>  </B></FONT>> , fillcolor = '#d16d81',  fontsize = 20]


    disposable_income  [label =  ",disposable_income_label, ", fillcolor = '#90CDC3',  fontsize = 20]
    consumable_income  [label =  ",consumable_income_label, ", fillcolor = '#90CDC3',  fontsize = 20]
    final_income  [label = ",final_income_label, ", fillcolor = '#90CDC3',  fontsize = 20]





    #product_6 -> final_process
    product_11 -> consumable_income



    product_12 -> final_income
    product_13 -> final_income

     final_income ->final_process [weight=2, penwidth=5]
    data_source_1a ->  disposable_income [weight=2, penwidth=5]


    process_poverty -> final_process
    data_source_1a -> final_process

    disposable_income -> consumable_income [weight=2, penwidth=5]
    disposable_income-> final_process

    consumable_income -> final_income  [weight=2, penwidth=5]
    consumable_income -> final_process


    data_source_14 -> final_process


    poverty_rates[label =",complete_table_poverty_rates, " </TABLE>>, style='bold, diagonals, rounded, filled' shape=box, fillcolor = 'lightblue']
    inequality_rates[label =",complete_table_inequality_rates, " </TABLE>>, style='bold, diagonals, rounded, filled' shape=box, fillcolor = 'lightblue']
    ncp_rates[label =",complete_table_ncp, " </TABLE>>, style='bold, diagonals, rounded, filled' shape=box, fillcolor = 'lightblue']



    final_process -> poverty_rates
    final_process -> inequality_rates
    final_process -> ncp_rates
   subgraph cluster_0 {
        node [style=filled]
        'poverty_rates' 'inequality_rates' 'ncp_rates'

        label = 'Simulation Results';
        color='#d6ecf3';
                        style=filled;

   }

       subgraph cluster_1 {
        node [style=filled]
        'data_source_1a' 'consumable_income' 'disposable_income' 'final_income'

        label = 'Income Concepts';
        color='#d3ebe7';
                style=filled;

       }
           subgraph cluster_2 {
        node [style=filled]
        'data_source_1' data_source_1b 'data_source_2' 'data_source_3' 'data_source_4'  'data_source_5' 'data_source_6' 'data_source_12' 'data_source_12a' 'data_source_pensions' 'data_source_13' 'data_source_14'

        label = 'Survey Data';
        color='#f6eeec';
                style=filled


           }

               subgraph cluster_3 {
        node [style=filled]
        'process_1' 'process_2' 'process_3' 'process_4' 'process_5' 'process_6' 'process_7' 'process_8'  'process_9' 'process_10' 'process_12' 'process_pensions' 'process_13' 'process_poverty'

        label = 'Input Parameters';
        color='#fff5e6';
                style=filled;

               }

                   subgraph cluster_4 {
        node [style=filled]
        'product_1' 'product_2' 'product_3' 'product_4' 'product_5' 'product_pensions' 'product_11' 'product_13', 'product_12'
        label = 'Data Simulation';
        color='#ffd0bd';
        style=filled;
    {rank=same data_source_13 process_13}  // optional, keep A3 close to A2

    }





  }
"))



  GEO_CEQ_WORKFLOW

}


#' @import DiagrammeRsvg
mod_diagram_server <- function(id, sim_res = sim_res(), title = "Diagrams", ...) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$diagram_ui <- renderUI({
      validate(
        need(sim_res(), "Click `Run` on the policy choices tab to see the results.")
      )
      req(sim_res())

      fluidPage(
        #theme = bs_theme(),

        tags$head(
          tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js"),

          tags$style(HTML("
            .card-body {
              overflow: hidden;
              position: relative;
            }
          .tall-card .card-body { /* Apply min-height only to .tall-card */
          min-height: 600px;
        }
            .grViz {
              position: absolute;
              top: 0;
              left: 0;
            }
             .tall-card  {
              border: 2px solid darkgrey; /* Regular border */
              border-radius: 5px;
              background-color: white; /* Regular background */
            }
            .bslib-fullscreen .card {
              border: 4px solid darkgrey; /* Border in full screen */
              background-color: transparent; /* No background in full screen */
            }

            .bslib-full-screen-enter {
          bottom: var(--bslib-full-screen-enter-bottom);
        }
          "))
        ),

        if( length(sim_res()) == 2 ){

          list(

            fluidRow(

                 card(
                   full_screen = TRUE,
                   class = "tall-card",  # Apply custom class to this card
                   card_header(uiOutput(ns("baseline_title_ui"))),
                   card_body(
                     shinycssloaders::withSpinner(grVizOutput(ns("baseline"), height = "60vh")),
                     tags$script(HTML('panzoom($(".grViz").get(0))'))
                   ),
                   card_footer(
                     downloadButton(ns("download_baseline_pdf"), "Download as PDF"),
                     downloadButton(ns("download_baseline_png"), "Download as PNG")
                   )
                 )
          ),
          hr(),
          fluidRow(
                 card(
                   full_screen = TRUE,
                   class = "tall-card",  # Apply custom class to this card
                   card_header(uiOutput(ns("policy_1_title_ui"))),
                   card_body(
                     shinycssloaders::withSpinner(grVizOutput(ns("policy_1"), height = "60vh")),
                     tags$script(HTML('panzoom($(".grViz").get(1))'))
                   ),
                   card_footer(
                     downloadButton(ns("download_policy1_pdf"), "Download as PDF"),
                     downloadButton(ns("download_policy1_png"), "Download as PNG")
                   )
                 )
          ))

        } else{


        list(


          fluidRow(
                 card(
                   full_screen = TRUE,
                   class = "tall-card",  # Apply custom class to this card
                   card_header(uiOutput(ns("baseline_title_ui"))),
                   card_body(
                     shinycssloaders::withSpinner(grVizOutput(ns("baseline"), height = "60vh")),
                     tags$script(HTML('panzoom($(".grViz").get(0))'))
                   ),
                   card_footer(
                     downloadButton(ns("download_baseline_pdf"), "Download as PDF"),
                     downloadButton(ns("download_baseline_png"), "Download as PNG")
                   )
                 )
          ),
          hr(),

          fluidRow(
                 card(
                   full_screen = TRUE,
                   class = "tall-card",  # Apply custom class to this card
                   card_header(uiOutput(ns("policy_1_title_ui"))),
                   card_body(
                     shinycssloaders::withSpinner(grVizOutput(ns("policy_1"), height = "60vh")),
                     tags$script(HTML('panzoom($(".grViz").get(1))'))
                   ),
                   card_footer(
                     downloadButton(ns("download_policy1_pdf"), "Download as PDF"),
                     downloadButton(ns("download_policy1_png"), "Download as PNG")
                   )
                 )
          ),
          hr(),

          fluidRow(
                 card(
                   full_screen = TRUE,
                   class = "tall-card",  # Apply custom class to this card
                   card_header(uiOutput(ns("policy_2_title_ui"))),
                   card_body(
                     shinycssloaders::withSpinner(grVizOutput(ns("policy_2"), height = "60vh")),
                     tags$script(HTML('panzoom($(".grViz").get(2))'))
                   ),
                   card_footer(
                     downloadButton(ns("download_policy2_pdf"), "Download as PDF"),
                     downloadButton(ns("download_policy2_png"), "Download as PNG")
                   )
                 )
          )


        )
        }
      )
    })

    # Plotly plotting ---------------------------------------------------------
    output$baseline <- renderGrViz({
      create_diagram(1, sim_res())
    })

    output$policy_1 <- renderGrViz({
      create_diagram(2, sim_res())
    })

    output$policy_2 <- renderGrViz({
      create_diagram(3, sim_res())
    })

    # Dynamic title rendering
    output$baseline_title_ui <- renderUI({
      h3(paste0("Semantic model diagram: ", sim_res()[[1]]$policy_name))
    })

    output$policy_1_title_ui <- renderUI({
      h3(paste0("Semantic model diagram: ", sim_res()[[2]]$policy_name))
    })

    output$policy_2_title_ui <- renderUI({
     h3(paste0("Semantic model diagram: ", sim_res()[[3]]$policy_name))
    })


    # Export functions for baseline
    output$download_baseline_pdf <- downloadHandler(
      filename = function() {
        paste(sim_res()[[1]]$policy_name, "_diagram_",  Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        svg <- export_svg(create_diagram(1,  sim_res()))
        writeLines(svg, "temp.svg")
        rsvg_pdf("temp.svg", file)
        unlink("temp.svg")
      }
    )

    output$download_baseline_png <- downloadHandler(
      filename = function() {
        paste( sim_res()[[1]]$policy_name, "_diagram_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        svg <- export_svg(create_diagram(1,  sim_res()))
        writeLines(svg, "temp.svg")
        rsvg_png("temp.svg", file)
        unlink("temp.svg")
      }
    )


    # Export functions for policy 1
    output$download_policy1_pdf <- downloadHandler(
      filename = function() {
        paste( sim_res()[[2]]$policy_name, "_diagram_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        svg <- export_svg(create_diagram(2,  sim_res()))
        writeLines(svg, "temp.svg")
        rsvg_pdf("temp.svg", file)
        unlink("temp.svg")
      }
    )

    output$download_policy1_png <- downloadHandler(
      filename = function() {
        paste(sim_res()[[2]]$policy_name, "_diagram_",  Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        svg <- export_svg(create_diagram(2,  sim_res()))
        writeLines(svg, "temp.svg")
        rsvg_png("temp.svg", file)
        unlink("temp.svg")
      }
    )



    # Export functions for policy 1
    output$download_policy2_pdf <- downloadHandler(
      filename = function() {
        paste( sim_res()[[3]]$policy_name, "_diagram_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        svg <- export_svg(create_diagram(3,  sim_res()))
        writeLines(svg, "temp.svg")
        rsvg_pdf("temp.svg", file)
        unlink("temp.svg")
      }
    )

    output$download_policy2_png <- downloadHandler(
      filename = function() {
        paste(sim_res()[[3]]$policy_name, "_diagram_",  Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        svg <- export_svg(create_diagram(3,  sim_res()))
        writeLines(svg, "temp.svg")
        rsvg_png("temp.svg", file)
        unlink("temp.svg")
      }
    )

  })
}

# Define the module UI
mod_diagram_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("diagram_ui"))
}


# overriding  this to add  a different logo
CEQ_ui_local <- function (request,
                    theme_fn = function() {
                      bslib::bs_theme(
                        version = 4,
                        bootswatch = "flatly",
                        `enable-rounded` = TRUE
                      )
                    },
                    style_logo_position = NULL,
                    inp_nav_width = NULL,
                    fn_results_ui = fn_results_ui_dummy,
                    ...)
{
  spinner <-
    tagList(waiter::spin_circle(),
            br(),
            span(paste0(get_app_name(),
                        " loading..."), style = "color:white;"))
  pages <-
    navbarPage(
      id = "main_sidebar",
      title = div(
        div(
          id = "img-logo-navbar",
          style = style_logo_position,
          img(src = "georgia_logo.png",
              id = "logo-style")
        ),
        get_app_name()
      ),
      windowTitle = get_app_name(),
      collapsible = TRUE,
      theme = theme_fn(),

      selected = "Info",
      tabPanel("Info"),
      tabPanel(
        "Policy Choices",
        tags$head(
          tags$style(HTML("
[id*='sscw_rate'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='ssce_rate'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='sub_rate_gas'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='sub_price_gas'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='sub_simul_gas'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='sub_free_gas'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_1'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_2'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_3'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_4'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_5'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_6'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_7'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_8'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='health_ben_2022_9'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}


[id*='larab'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='lpast'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='ptax_thr1'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='ptax_thr2'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='ptax_r1l'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='ptax_r2l'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='ptax_r1u'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}

[id*='ptax_r2u'] {
  pointer-events: none;
  opacity: 0.5;
  background-color: #e9ecef;
}


        "))
        ),
        value = "pc2019",
        mod_inputs_ui_wrapper("generic_inputs", inp_nav_width = inp_nav_width)
      ),
      fn_results_ui(id = "ceqsim"),
      if (golem::app_dev()) {
        tabPanel("DEV-Results", devCEQ::mod_dev_res_ui("devres"))
      },
      tabPanel("How it works?", value = "howto")
    )
  tagList(golem_add_external_resources(), if (isTRUE(unlist(options("golem.app.prod")))) {
    waiter::waiter_show_on_load(spinner)
  }, if (isTRUE(unlist(options("golem.app.prod")))) {
    waiter::waiter_hide_on_render(NS("generic_inputs")("n_policy_ui"))
  }, pages)
}




#' @export
#' @noRd
gen_ceq_ui_local <-
  function (fn_results_ui = fn_results_ui_dummy,
            theme_fn = function()
              shinythemes::shinytheme("cerulean"),
            style_logo_position = NULL,
            inp_nav_width = NULL,
            ...)
  {
    function(request) {
      CEQ_ui_local(
        request,
        theme_fn = theme_fn,
        style_logo_position = style_logo_position,
        inp_nav_width = inp_nav_width,
        fn_results_ui = fn_results_ui,
        ...
      )
    }
  }



ceq_pre_postsim_generic <- function(x, ...) {
  list(x = x,
       dots = rlang::dots_list(...))
}

# needed to fix hard coded typos in code

mod_generic_run_sim_server_edited <- function (id, run = reactive(NULL), presim = reactive(NULL),
                                               inps = reactive(NULL), all_inps = reactive(NULL), ceq_progress = reactive(NULL),
                                               fn_add_missing_inp = add_missing_inp_generic, fn_ceq_sim = ceq_sim_generic,
                                               fn_ceq_pre_postsim = ceq_pre_postsim_generic, ...)
{


  observe({
    # Disable the inputs
    #shinyjs::disable("numInput1")
    shinyjs::disable("generic_inputs-policy1-pitw_rate_0")
    shinyjs::disable("generic_inputs-policy2-pitw_rate_0")
    shinyjs::disable("pitw_rate_0")

  })

  moduleServer(id, function(input, output, session) {

    observe({
      # Disable the inputs
      #shinyjs::disable("numInput1")
      shinyjs::disable("generic_inputs-policy1-pitw_rate_0")
      shinyjs::disable("generic_inputs-policy2-pitw_rate_0")
      shinyjs::disable("pitw_rate_0")

    })

    ns <- session$ns
    sim_res <- reactiveVal(NULL)
    last_res <- reactiveVal(list(NULL))
    inps_local <- reactive({
      inps() %>% purrr::map(~{
        .x$policy_choices <- fn_add_missing_inp(.x$policy_choices,
                                                all_inps())
        .x
      })
    }, label = "ceq-add-missing-inputs")
    observeEvent(run(), {
      req(run())
      req(inps_local())
      req(length(inps_local()) > 0)
      ceq_progress()$set(value = 0)
      fct_big_step_ceq_progress(ceq_progress(), message = "Step 1/3: Pre-simulation!",
                                detail = "Loading pre-simulation data")
      prog_upd <- function(detail) {
        fct_samll_step_ceq_progress(ceq_progress(), n_small = length(inps_local()) +
                                      1, message = "Step 2/3: CEQ simulation:",
                                    detail = detail)
      }
      req(presim())
      prog_upd("")
      inps_local() %>% purrr::imap(~{
        last <- last_res()[[.y]]
        .x$run <- TRUE
        if (isTruthy(last)) {
          same_policy <- all.equal(.x$policy_choices,
                                   last$policy_choices, check.attributes = FALSE) %>%
            isTRUE() %>% all()
          if (same_policy) {
            .x$run <- FALSE
            .x$policy_sim_raw <- last$policy_sim_raw
            .x$run_timestamp <- last$run_timestamp
            .x$policy_sim_agg <- last$policy_sim_agg
          }
        }
        .x
      }) %>% purrr::map(~{
        prog_upd(.x$policy_name)
        if (.x$run && .x$policy_as_base) {
          .x$policy_sim_raw <- presim()$baseline
          .x$policy_sim_agg <- .x$policy_sim_raw %>%
            fn_ceq_pre_postsim()
          .x$run_timestamp <- Sys.time()
        }
        if (.x$run && !.x$policy_as_base) {
          .x$policy_sim_raw <- fn_ceq_sim(inp = .x$policy_choices,
                                          presim = presim())
          .x$policy_sim_agg <- .x$policy_sim_raw %>%
            fn_ceq_pre_postsim()
          .x$run_timestamp <- Sys.time()
        }
        else {
        }
        .x
      }) %>% sim_res()
    }, ignoreNULL = TRUE, ignoreInit = TRUE, label = "ceq-runing the sim and postsim")
    observeEvent(sim_res(), {
      sim_res() %>% last_res()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    sim_res
  })
}
# # 4. Server side logic
local_run_sim_server_edited <- function (id, run, presim, inps, all_inps, ceq_progress, ...) {
  mod_generic_run_sim_server_edited(id, run = run, presim = presim, inps = inps,
                                    all_inps = all_inps, ceq_progress = ceq_progress, fn_add_missing_inp = add_missing_inp_generic,
                                    fn_ceq_sim = full_ceq, fn_ceq_pre_postsim = ceq_pre_postsim_generic)
}


# not needed in the end because I just use css
# CEQ_server_edited <- function(
#     input,
#     output,
#     session,
#     inputs_str = reactive(NULL),
#     presim_dta = reactive(list(NULL)),
#     inp_str_fn = gen_inp_str,
#     ui_gen_fn = gen_tabinp_ui,
#     n_policy = c(1, 2, 1),
#     n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
#     info_page_md = NULL,
#     info_page_size = "l",
#
#     fn_sim_srvr = make_run_sim_server(),
#
#     fn_add_missing_inp = NULL,
#     fn_ceq_sim = NULL,
#     fn_ceq_pre_postsim = NULL,
#
#     fn_postsim_srvr = mod_generic_run_postsim_server,
#     fn_ceq_postsim = function(x, ...) x,
#     fn_res_disp_srvr = fn_results_display_server_dummy,
#     ...) {
#
#   # # Loading underlining data for 2022 simulation
#   inputs_str_ui <-
#     inputs_str %>%
#     filter(if_any(any_of("include"), ~ .x)) %>%
#     select(-any_of("include"))
#
#   inps_all <- inputs_str %>% get_all_inps()
#
#   # Info-page, guides and blackouts ==========================================
#   active_tab <- reactive(NULL)
#   active_tab <- mod_info_page_server(first_tab = "pc2019",
#                                      how_to_tab = "howto",
#                                      info_page_md = info_page_md,
#                                      info_page_size = info_page_size,
#                                      ui_ns = NS("generic_inputs"))
#
#   # Inputs UI server =========================================================
#   ceq_inputs <- mod_inputs_server_edited(
#     'generic_inputs',
#     inp_raw_str = inputs_str_ui,
#     inp_str_fn = inp_str_fn,
#     ui_gen_fn = ui_gen_fn,
#     active_tab = active_tab,
#     target_tab = "pc2019",
#     source_tab = "howto",
#     n_policy = n_policy,
#     n_policy_type = n_policy_type
#   )
#
#
#   observe({
#     # Disable the inputs
#     #shinyjs::disable("numInput1")
#     shinyjs::disable("generic_inputs-policy1-pitw_rate_0")
#     shinyjs::disable("generic_inputs-policy2-pitw_rate_0")
#     shinyjs::disable("generic_inputs-policy1-pitw_rate_0-l")
#     shinyjs::disable("pitw_rate_0")
#
#   })
#
#
#   # observeEvent(ceq_inputs$run(), {browser()}, ignoreInit = TRUE)
#   # Simulation runner module ==================================================
#   ceq_progress <-
#     moduleServer(NULL, function(input, output, session) {
#       eventReactive(ceq_inputs$run(), {
#         fct_make_ceq_progress(session = session, 3)
#       })
#     })
#
#   sim_results <-
#     fn_sim_srvr(
#       id = 'ceqsim',
#       run = ceq_inputs$run,
#       inps = ceq_inputs$key,
#       presim = presim_dta,
#       all_inps = reactive(inps_all),
#       ceq_progress = ceq_progress,
#       fn_add_missing_inp = fn_add_missing_inp,
#       fn_ceq_sim = fn_ceq_sim,
#       fn_ceq_pre_postsim = fn_ceq_pre_postsim,
#       ...
#     )
#
#   # Post simulation ===========================================================
#   postsim_results <-
#     fn_postsim_srvr(
#       id = 'ceqsim',
#       sim_res = sim_results,
#       fn_ceq_postsim = fn_ceq_postsim,
#       ceq_progress = ceq_progress
#     )
#
#   # Results display ==================================================
#   fn_res_disp_srvr(
#     id = "ceqsim",
#     sim_res = sim_results,
#     postsim_res = postsim_results,
#     ceq_progress = ceq_progress,
#     active_tab = active_tab
#   )
#
#   # Dev results display  ==================================================
#   mod_dev_res_server("devres", sim_results)
#
#   # # Browser for the button ================================================
#   mod_browser_button_server(
#     NULL,
#     sim_results = sim_results,
#     postsim_results = postsim_results,
#     ceq_inputs = ceq_inputs
#   )
#
#   # observe({
#   #   req(input$browser)
#   #   browser()
#   # })
#
#   # Observer to make things run
#   reactive({
#     list(
#       ceq_inputs$run,
#       ceq_inputs$key,
#       sim_results(),
#       postsim_results
#     )
#   })
#
#
#
# }

#
# mod_inputs_server_edited <- function (id, inp_raw_str, inp_str_fn, ui_gen_fn, run_guide = function() NULL,
#                                       active_tab = function() NULL, id_result = NULL, target_tab = NULL,
#                                       source_tab = NULL, n_policy = c(1, 2, 1), n_policy_type = get_n_policy_types(),
#                                       ...)
# {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     cur_inps <- mod_dyn_inp_srv(id = NULL, inp_raw_str = inp_raw_str,
#                                 n_choices = reactive(inp_btns$n_choices), n_max_choices = reactive(inp_btns$n_max_choices),
#                                 upd_inp = reactive(inp_btns$upload_sim) %>% debounce(750),
#                                 reseter = reactive(if (!isTruthy(inp_btns$reset)) {
#                                   0
#                                 }
#                                 else {
#                                   inp_btns$reset
#                                 }), inp_str_fn = inp_str_fn, ui_gen_fn = ui_gen_fn)
#     cur_inps_export <- reactive({
#       req(cur_inps$inp())
#     })
#     inp_btns <- mod_inputs_btns_server(NULL, sim_export_dta = cur_inps$inp,
#                                        n_policy = n_policy, n_policy_type = n_policy_type)
#     cur_inps$run <- reactive({
#       list(value = inp_btns$run, timestamp = Sys.time())
#     })
#     output$inputs_out <- renderPrint({
#       list(guide_starter(), cur_inps$key(), cur_inps$run(),
#            inp_btns$run_guide) %>% str()
#     })
#     guide_starter <- reactiveVal(NULL)
#     observeEvent(inp_btns$run_guide, {
#       if (!isTruthy(guide_starter())) {
#         guide_starter(0)
#       }
#       else {
#         new_val <- guide_starter() + 1
#         guide_starter(new_val)
#       }
#     }, ignoreNULL = TRUE, ignoreInit = TRUE)
#     observeEvent(active_tab(), {
#       req(active_tab()$previous)
#       req(active_tab()$current)
#       req(source_tab)
#       req(target_tab)
#       if (active_tab()$previous == source_tab && active_tab()$current ==
#           target_tab) {
#         if (!isTruthy(guide_starter())) {
#           guide_starter(0)
#         }
#         else {
#           new_val <- guide_starter() + 1
#           guide_starter(new_val)
#         }
#       }
#     }, ignoreNULL = TRUE, ignoreInit = TRUE)
#     observeEvent(guide_starter(), {
#     }, ignoreNULL = TRUE, ignoreInit = TRUE)
#     cur_inps
#   })
# }
#




fct_config_export_dt_updated <- function (.data,
          file_title = "Table",
          digits_number = 3,
          pageLength = 10,
          scroll_y = TRUE)
{
  pageLength = min(nrow(.data), pageLength)
  if (scroll_y)
    scroll_y_height = round(pageLength * 250 / 6, 0) %>% str_c(.,
                                                               "px")
  else {
    scroll_y_height = FALSE
  }
  req(.data)
  .data %>% DT::datatable(
    .,
    rownames = FALSE,
    extensions = c("Buttons",
                   "Scroller"),
    options = list(
      pageLength = pageLength,
      dom = c("Bfrtip"),
      scrollX = TRUE,
      deferRender = TRUE,
      scrollY = scroll_y_height,
      scroller = TRUE,
      buttons = list(
        list(extend = "copy",
             text = "Copy"),
        list(
          extend = "excel",
          text = "Save in Excel format",
          title = file_title
        ),
        list(
          extend = "csv",
          text = "Save in csv format",
          title = file_title
        )
      )
    )
  )
}


mod_incidences_server_updated <- function (id, sim_res, dec_vars = get_var_nm()$var, make_bar_fn = make_bar_dta,
                                           n_dec_label = "Number of deciles", dec_by_label = "Deciles by:",
                                           title_for_page ="TITLE", ...){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$incidence_ui <- renderUI({
      req(sim_res())
      tagList(h3(title_for_page),
              fluidRow(column(2,
                                                     numericInput(ns("n_dec"), label = n_dec_label,
                                                                  value = 10, min = 5, max = 50)), column(3,
                                                                                                          selectInput(ns("by_income"), label = dec_by_label,
                                                                                                                      choices = purrr::set_names(get_inc_nm()$var, get_inc_nm()$var_title),
                                                                                                                      selected = NULL, width = "350px")), column(7,
                                                                                                                                                                 uiOutput(ns("radioGroupButtons")))), tags$div(tags$div(shinyWidgets::dropMenu(shinyWidgets::actionBttn(inputId = ns("ddmenue"),
                                                                                                                                                                                                                                                                        style = "simple", size = "xs", color = "primary",
                                                                                                                                                                                                                                                                        icon = icon("bars")), actionButton(inputId = ns("more"),
                                                                                                                                                                                                                                                                                                           label = "Save plot", icon = icon("download"),
                                                                                                                                                                                                                                                                                                           class = "btn-sm")), style = htmltools::css(position = "absolute",
                                                                                                                                                                                                                                                                                                                                                      top = 0, left = "5px", zIndex = 30)), shinycssloaders::withSpinner(plotly::plotlyOutput(ns("incidence_ly"))),
                                                                                                                                                                                                               style = htmltools::css(position = "relative",
                                                                                                                                                                                                                                      width = htmltools::validateCssUnit("100%"),
                                                                                                                                                                                                                                      height = htmltools::validateCssUnit("400px"))),
              tags$hr(), column(12, shinycssloaders::withSpinner(DT::DTOutput(ns("incidence_dt")))),
              tags$hr())
    })
    n_dec_debounced <- reactive(req(input$n_dec)) %>% debounce(1000)
    incidence_esimates <- reactive({
      req(sim_res())
      req(n_dec_debounced())
      req(input$by_income)
      out <- try({
        sim_res() %>% agg_sims_by_deciles(dec_by = input$by_income,
                                          dec_vars = dec_vars, wt_var = get_wt_nm(),
                                          n_dec = n_dec_debounced(), get_var_fn = get_var_nm,
                                          ...) %>% make_bar_fn()
      }, silent = T)
      out %>% validate_result(str_c("Erro occured when estimating the incidencesin in ",
                                    ns("")))
    })
    # output$title <- renderUI({
    #   req(incidence_esimates())
    #   incidence_esimates()$title %>% h3()
    # })
    plt_indx_upd <- reactiveVal(NULL)
    radioGroupButtons_ui <- reactiveVal(NULL)
    observe({
      req(incidence_esimates()$plt_indx)
      isolate({
        if (!isTruthy(plt_indx_upd())) {
          shinyWidgets::radioGroupButtons(inputId = ns("plot_var"),
                                          label = NULL, choices = incidence_esimates()$plt_indx,
                                          selected = incidence_esimates()$plt_indx[[1]],
                                          status = "primary", size = "sm", individual = TRUE,
                                          direction = "horizontal", ) %>% radioGroupButtons_ui()
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
    save_ggplot_server2("export", plot_rv = rv, dpi = 450,
                        scale = 2)
    incidence_esimates
  })

}
mod_gini_pov_gen_server_updated <- function (id, sim_res = reactive(NULL), title = "Inequality",
                                             export_btn_title = "Save plot", pl_choices = NULL, pl_title = "Poverty line",
                                             get_dta_fn = function(dta, policy_name, ...) {
                                               get_dta_gini(dta = dta, policy_name = policy_name, income_vars_tbl = get_inc_nm(),
                                                            wt_var = get_wt_nm(), para_names = tibble(parameter = c("Gini",
                                                                                                                    "Theil"), label = c("Gini index", "Theil index")),
                                                            ...)
                                             }, make_plot_fn = function(dta, ...) {
                                               make_gini_pov_updated(dta = dta, y = NULL, x = Income, color = Simulation,
                                                             title = "Inequality", ylab = NULL, xlab = "Income",
                                                             #colour_set = 'Accent',
                                                             ...)
                                             }, ...)
{
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$gini_ui <- renderUI({
      validate(need(sim_res(), "Click `Run` on the policy choices tab to see the results."))
      req(sim_res())
      if (isTruthy(pl_choices)) {
        btn_ui <- fluidRow(column(4, shiny::selectInput(inputId = ns("povLine"),
                                                        label = pl_title, choices = pl_choices)), column(8,
                                                                                                         uiOutput(ns("radioGroupButtons"))))
      }
      else {
        btn_ui <- fluidRow(column(12, uiOutput(ns("radioGroupButtons"))))
      }
      tagList(tags$h4(title), btn_ui, tags$div(tags$div(shinyWidgets::dropMenu(shinyWidgets::actionBttn(inputId = ns("ddmenue"),
                                                                                                        style = "simple", size = "xs", color = "primary",
                                                                                                        icon = icon("bars")), actionButton(inputId = ns("more"),
                                                                                                                                           label = "Save plot", icon = icon("download"),
                                                                                                                                           class = "btn-sm")), style = htmltools::css(position = "absolute",
                                                                                                                                                                                      top = 0, left = "5px", zIndex = 30)), shinycssloaders::withSpinner(plotly::plotlyOutput(ns("ineq_ly"))),
                                               style = htmltools::css(position = "relative",
                                                                      width = htmltools::validateCssUnit("100%"),
                                                                      height = htmltools::validateCssUnit("400px"))),
              tags$hr(), column(12, shinycssloaders::withSpinner(DT::DTOutput(ns("ineq_dt")))),
              tags$hr())
    })
    pl_choice <- reactive({
      if (isTruthy(pl_choices) & !isTruthy(input$povLine)) {
        return(pl_choices[[1]])
      }
      else if (isTruthy(pl_choices) & isTruthy(input$povLine)) {
        return(input$povLine)
      }
      else {
        return(NULL)
      }
    }) %>% debounce(500)
    stat_esimates <- reactive({
      req(sim_res())
      sim_res() %>% purrr::map(~{
        get_dta_fn(dta = .x$policy_sim_raw, policy_name = .x$policy_name,
                   poverty_line_var = pl_choice())
      }) %>% make_plot_fn()
    })
    plt_indx_upd <- reactiveVal(NULL)
    radioGroupButtons_ui <- reactiveVal(NULL)
    observe({
      req(stat_esimates()$plt_indx)
      isolate({
        if (!isTruthy(plt_indx_upd())) {
          shinyWidgets::radioGroupButtons(inputId = ns("plot_var"),
                                          label = NULL, choices = stat_esimates()$plt_indx,
                                          selected = stat_esimates()$plt_indx[[1]],
                                          size = "sm", status = "primary", individual = TRUE,
                                          direction = "horizontal", ) %>% radioGroupButtons_ui()
          plt_indx_upd(stat_esimates()$plt_indx)
        }
      })
    })
    output$radioGroupButtons <- renderUI({
      req(radioGroupButtons_ui())
    })
    cur_plot_var <- reactive({
      if (!isTruthy(input$plot_var)) {
        return(1)
      }
      else {
        return(input$plot_var)
      }
    })
    output$ineq_ly <- renderPlotly({
      stat_esimates()$ly[[cur_plot_var()]] %>% validate_result() %>%
        plotly_config()
    })
    output$ineq_dt <- DT::renderDT({
      dta <- stat_esimates()$tbl_dt
      dta %>% validate_result() %>% fct_config_export_dt_updated(file_title = title) %>%  # here I make the change
        DT::formatRound(columns = names(dta)[sapply(dta,
                                                    is.numeric)], digits = 4)
    })
    rv <- reactiveValues(plot = NULL)
    observe({
      req(cur_plot_var())
      req(stat_esimates()$gg[[cur_plot_var()]])
      rv$plot <- stat_esimates()$gg[[cur_plot_var()]]
    })
    observeEvent(input$more, {
      esquisse::save_ggplot_modal(id = session$ns("export"),
                                  title = export_btn_title)
    })
    save_ggplot_server2("export", plot_rv = rv, dpi = 450,
                        scale = 2)
    stat_esimates
  })
}



make_nct_dta_updated <- function (dta, title_glue = "Net cash position by deciles of {income_type}",
                                  x_title = "Deciles of {income_type}", y_titles = list(relative = "Relative incidence (% of {income_type})",
                                                                                        absolute = "Absolute incidence (% of all deciles)", level = "West African CFA franc"),
                                  ncp_agg_fn = agg_ncp_total, digits = 2,
                                  #colour_set = "Accent",
                                  ...)
{
  tbl_long <- dta %>% fct_ncp_long_dta()
  income_type <- tbl_long$Income %>% unique() %>% first()%>%
    stringr::str_to_lower()
  tbl_exp <- tbl_long %>% dplyr::select(-dplyr::any_of(c("Income_value",
                                                         "value", "Income"))) %>% dplyr::mutate(dplyr::across(dplyr::contains("incid"),
                                                                                                              ~scales::percent(., 1/10^digits))) %>% dplyr::mutate(dplyr::across(dplyr::contains("Level"),
                                                                                                                                                                                 ~scales::number(., 1/10^digits, big.mark = ""))) %>%
    tidyr::pivot_longer(c(dplyr::everything(), -Decile, -Simulation,
                          -Source)) %>% tidyr::pivot_wider(names_from = Source,
                                                           values_from = dplyr::contains("value"), names_glue = "{Source}") %>%
    dplyr::mutate(name = factor(name, levels = unique(name))) %>%
    dplyr::arrange(name, Simulation, Decile) %>% dplyr::select(`NCP Type` = name,
                                                               Simulation, Decile, dplyr::everything())
  first_source <- tbl_long$Source %>% unique() %>% sort() %>% first() %>% as.character()
  tbl_flex <- tbl_exp %>% flextable_config(title = glue::glue(title_glue),
                                           digits = digits)
  n_cols <- tbl_long$Source %>% forcats::fct_drop() %>% levels() %>%
    length()
  # colors_to_expand <- RColorBrewer::brewer.pal(min(max(3, n_cols -
  #                                                        1), 12), colour_set)

  colors_to_expand <-  ggthemes::colorblind_pal()(min(max(3, n_cols -  1), 8))
  plotly_pal <- colorRampPalette(colors_to_expand)
  all_plt_dta <- tbl_long %>% tidyr::pivot_longer(cols = c(contains("Relative"),
                                                           contains("Absolute"),
                                                           contains("Level"))) %>%
    dplyr::mutate(Source = Source %>%  forcats::fct_drop()) %>%
    dplyr::mutate(name2 = name) %>%
    dplyr::group_by(name2) %>%
    tidyr::nest() %>%
    purrr::pmap(~{
      list(plot_dta = .y, line_dta =  .y %>% ncp_agg_fn()# NA
           ) %>%
        list() %>% setNames(.x)
    }) %>%
    unlist(recursive = F)
  all_ggs <- all_plt_dta %>% purrr::imap(~{
    y_title <- y_titles[str_detect(.y, stringr::regex(names(y_titles),
                                                      ignore_case = T))][[1]]
    fct_make_ncp_gg(.x, type = .y, digits = 2, title = glue::glue(title_glue),
                    x_title = glue::glue(x_title), y_title = glue::glue(y_title),
                    plotly_pal = plotly_pal)
  })
  all_lys <- all_plt_dta %>% purrr::imap(~{
    y_title <- y_titles[str_detect(.y, regex(names(y_titles),
                                             ignore_case = T))][[1]]
    fct_make_ncp_ly(.x, type = .y, digits = 2, title = glue::glue(title_glue),
                    x_title = glue::glue(x_title), y_title = glue::glue(y_title),
                    plotly_pal = plotly_pal)
  })
  plt_indx <- purrr::set_names(x = names(all_lys), nm = names(all_lys))
  list(title = glue::glue(title_glue), tbl_exp = tbl_exp, tbl_long = tbl_long,
       tbl_flex = tbl_flex, tbl_dt = tbl_exp, gg = all_ggs,
       ly = all_lys, plt_indx = plt_indx)
}


make_bar_dta_updated <-
  function (dta, plot_types = c("relative", "absolute"), title_glue = "{first_source}: Relative (% of {income_type}) and absolute (% of all deciles) incidence",
          x_title = "Deciles of {income_type}", y_titles = list(relative = "Relative incidence (% of {income_type})",
                                                                absolute = "Absolute incidence (% of all deciles)", level = "West African CFA franc"),
          digits = 2,
         # colour_set = "Accent",
          ...)
{
  tbl_long <- dta %>% bind_rows() %>% group_by(Income, Simulation,
                                               Source) %>% mutate(tot = sum(value, na.rm = TRUE)) %>%
    ungroup() %>% mutate(`Relative Incidence` = value/Income_value,
                         `Absolute Incidence` = value/tot, Level = value, Simulation = as.factor(Simulation)) %>%
    select(-tot, -value)
  income_type <- tbl_long$Income %>% unique() %>% first() %>% stringr::str_to_lower()
  tbl_exp <- tbl_long %>% select(-any_of(c("Income_value",
                                           "value", "Income"))) %>% mutate(across(contains("incid"),
                                                                                  ~scales::percent(., 1/10^digits))) %>% mutate(across(contains("Level"),
                                                                                                                                       ~scales::number(., 1/10^digits, big.mark = ""))) %>%
    pivot_wider(names_from = Source, values_from = c(contains("incid"),
                                                     contains("Level")), names_glue = "{.value}: {Source}") %>%
    select(Simulation, Decile, everything())

  tbl_flex <- tbl_exp %>% flextable_config(title = glue::glue(title_glue),
                                           digits = digits)
  tbl_dt <- tbl_exp
  n_cols <- tbl_long$Simulation %>% forcats::fct_drop() %>%
    levels() %>% length()
  #colors_to_expand <- RColorBrewer::brewer.pal(min(max(3, n_cols - 1), 12), colour_set)
  colors_to_expand <-  ggthemes::colorblind_pal()(min(max(3, n_cols -  1), 8))

  plotly_pal <- colorRampPalette(colors_to_expand)
  all_plots <- tbl_long %>% group_by(Source) %>% nest() %>%
    mutate(ly_comb = map2(data, Source, ~{
      single_tbl <- .x
      plot_n <- length(plot_types)
      if (length(plot_types) == 1)
        title_positions <- c(0.5)
      if (length(plot_types) == 2)
        title_positions <- c(0.25, 0.75)
      if (length(plot_types) == 3)
        title_positions <- c(0.165, 0.495, 0.825)
      plot_titles <- plot_types %>% map2(title_positions,
                                         ~{
                                           list(x = .y, y = 1, text = glue::glue(y_titles[[.x]]),
                                                showarrow = F, font = list(size = 16), xanchor = "center",
                                                yanchor = "top", xref = "paper", yref = "paper")
                                         })
      all_ly <- plot_types %>% list(seq_along(.)) %>% pmap(~{
        single_tbl %>% make_bar_ly(type = ..1, showleg = ..2 ==
                                     1, digits = digits, y_titles = y_titles, x_title = x_title,
                                   income_type = income_type, plotly_pal = plotly_pal)
      })
      all_ly %>% plotly::subplot(., nrows = 1, titleX = T,
                                 titleY = T, margin = c(0.025, 0.025, 0.55, 0.05)) %>%
        layout(title = list(text = paste0("<b>", .y,
                                          "</b>"), y = 0.99, x = 0.5, xanchor = "center",
                            yanchor = "top", font = list(size = 20)), annotations = plot_titles)
    }), gg_comb = map2(data, Source, ~{
      single_tbl <- .x
      all_ggplts <- plot_types %>% list(seq_along(.)) %>%
        pmap(~{
          single_tbl %>% make_bar_gg(type = ..1, digits = digits,
                                     y_titles = y_titles, x_title = x_title, income_type = income_type,
                                     plotly_pal = plotly_pal)
        })
      all_ggplts %>% reduce(~.x | .y) + patchwork::plot_layout(guides = "collect") +
        patchwork::plot_annotation(title = as.character(.y)) &
        theme(legend.position = "bottom")
    }))
  plot_codes <- all_plots$Source %>% stringr::str_remove_all("[^[:alnum:]]") %>%
    stringr::str_c("gg", .) %>% stringr::str_to_lower()
  plt_indx <- purrr::set_names(x = plot_codes, nm = all_plots$Source)
  plot_gg <- purrr::set_names(all_plots$gg_comb, nm = all_plots$Source %>%
                         stringr::str_remove_all("[^[:alnum:]]") %>% stringr::str_c("gg",
                                                                                    .) %>% stringr::str_to_lower())
  plot_ly <- purrr::set_names(all_plots$ly_comb, nm = all_plots$Source %>%
                         stringr::str_remove_all("[^[:alnum:]]") %>% stringr::str_c("gg",
                                                                                    .) %>% stringr::str_to_lower())
  list(title = glue::glue(title_glue), tbl_exp = tbl_exp, tbl_long = tbl_long,
       tbl_flex = tbl_flex, tbl_dt = tbl_dt, gg = plot_gg, ly = plot_ly,
       plt_indx = plt_indx)
  }



make_gini_pov_updated <- function (dta, x, color, title = "", y = NULL, ylab = NULL, xlab = NULL,
                                   digits = 3,
                                   #colour_set = "Accent",
                                   ...)
{
  tbl_exp <- dta %>% bind_rows() %>% tidyr::pivot_wider(names_from = {
    {
      color
    }
  }, values_from = any_of(c("value", "Value")))
  tbl_long <- dta %>% bind_rows()
  tbl_flex <- tbl_exp %>% flextable_config(title = title, digits = digits)
  tbl_dt <- tbl_exp
  local_colours <- tbl_long$Simulation %>% unique() %>% length()

  #local_colours <- RColorBrewer::brewer.pal(local_colours, colour_set)
  local_colours <- ggthemes::colorblind_pal()(local_colours)



  #local_colours <- wb_pal(reverse = F)(local_colours)
  plot_dta <- tbl_long %>% dplyr::group_by(Parameter) %>% tidyr::nest()
  plot_dta <- setNames(plot_dta$data, plot_dta$Parameter)
  plot_ly <- plot_dta %>% purrr::imap(~{
    rlang::eval_tidy(rlang::quo_squash(quo({
      .x %>% plotly::plot_ly() %>% plotly::add_trace(x = ~{
        {
          x
        }
      }, y = ~Value, color = ~{
        {
          color
        }
      }, colors = local_colours, text = ~scales::number(Value,
                                                        1/10^digits), hoverinfo = "text+name", type = "scatter",
      mode = "markers+lines")
    }))) %>% plotly::layout(title = str_c(title, ". ", .y),
                            xaxis = list(title = xlab, rangemode = "normal",
                                         dtick = 1), yaxis = list(title = .y, rangemode = "normal"),
                            legend = list(x = 100, y = 0.5)) %>% plotly_config()
  })
  plot_gg <- plot_dta %>% purrr::imap(~{
    .x %>% ggplot2::ggplot() + ggplot2::aes(x = {
      {
        x
      }
    }, y = Value, color = {
      {
        color
      }
    }, group = {
      {
        color
      }
    }, text = scales::number(Value, 1/10^digits), label = scales::number(Value,
                                                                         1/10^digits)) + ggplot2::geom_point(size = 2.5, alpha = 0.75) +
      ggplot2::geom_path(size = 1) + ggplot2::scale_color_manual(values = local_colours) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x,
                                                                       width = 15)) + ggplot2::scale_y_continuous(labels = scales::label_number(1/10^digits)) +
      ggplot2::theme_minimal() + ggplot2::theme(legend.position = c(0.85,
                                                                    0.85)) + ggplot2::labs(title = str_c(title, ". ",
                                                                                                         .y)) + ggplot2::xlab(xlab) + ggplot2::ylab(.y)
  })
  plt_indx <- purrr::set_names(x = names(plot_ly), nm = names(plot_ly))
  list(title = glue::glue(title), tbl_exp = tbl_exp, tbl_long = tbl_long,
       tbl_flex = tbl_flex, tbl_dt = tbl_dt, gg = plot_gg, ly = plot_ly,
       plt_indx = plt_indx)
}


