# taken from https://github.com/chris31415926535/bivariatechoropleths/blob/master/R/bivariate_choropleth.R
addBivariateChoropleth <- function(map, map_data, var1_name, var2_name, ntiles = 3,
                                   var1_label = NA, var2_label = NA,
                                   label_arrows = TRUE,
                                   region_names = NA,
                                   add_legend = TRUE,
                                   labs_prefix = "",
                                   paletteFunction = pals::brewer.ylgn, ...){

  if (is.na(var1_label)) var1_label <- rlang::enexpr(var1_name)
  if(is.na(var2_label))  var2_label <- rlang::enexpr(var2_name)


  # separate labels for the palette and the map, if so desired
  var1_pal_label <- var1_label
  var2_pal_label <- var2_label

  # are we putting fun arrows on the labels?
  if (label_arrows){
    var1_pal_label <- paste0(var1_label, " \U2192")
    var2_pal_label <- paste0(var2_label, " \U2192")
  }

  # the pals package has many palettes for bivariate choropleths.
  # NOTE!! That all of them are 2x2 or 3x3. larger matrices will probably be
  # harder to interpret, and it looks like people don't use them
  bivar_pal <- function(x) paletteFunction(n=ntiles^2)[x]

  forplot <- map_data %>%
    dplyr::rename (var1 = {{var1_name}},
                   var2 = {{var2_name}}) %>%
    dplyr::mutate(var1_ntile = dplyr::ntile(var1, n = ntiles),
                  var2_ntile = dplyr::ntile(var2, n = ntiles),
                  pal_num = var1_ntile + (var2_ntile - 1)*ntiles,#(ntiles -var1_ntile)*3 + var2_ntile,
                  pal_colour = bivar_pal(pal_num))

  # FIXME: region names not working
  if (!is.na(region_names)) forplot <- dplyr::rename(forplot,
                                                     region_name_label = region_names)

  # set up some css for the html palette swatch
  palette_size_px <- 160
  swatch_size_px <- round(palette_size_px / ntiles)

  row_col_px <- rep(paste0(swatch_size_px,"px"), times = ntiles) %>%
    stringr::str_flatten(collapse = " ") %>%
    paste0(., ";")


  div_var1 <- paste0('<div class = "var1-label" style="grid-row-start:1; grid-row-end:',(ntiles+1),'; text-align: center; writing-mode: tb-rl;
        transform: rotate(-180deg);">',var1_pal_label,'</div>')
  div_var2 <- paste0('<div style="text-align:center; grid-column:2 / ',(ntiles+2),';">',var2_pal_label,'</div>')

  # set up the indices for the palette
  div_indices <- matrix((1:ntiles^2),
                        nrow=ntiles,
                        ncol = ntiles,
                        byrow = TRUE)

  div_indices <- div_indices[,c(ntiles:1)]

  # set up the divs for the palette squares
  divs <- paste0('<div style="background-color:',bivar_pal(div_indices),
                 '; color:',bivar_pal(div_indices),
                 ';">',div_indices,' </div>') %>%
    stringr::str_flatten()

  # combine the above bits with a css grid wrapper for the html palette
  palette_html <- paste0(
    '<style> .grid-container { display: grid;
    grid-template-columns: 70px ',row_col_px,
    'grid-auto-rows: ',row_col_px,' 90px;','}
    </style>
    <div class="grid-container">',
    div_var1,
    divs,
    div_var2,
    '</div>')

  labs <- paste0("<b>", labs_prefix,"</b><br>",
                 "<b>",var1_label,"</b>",
                 #"<br>Ntile: ", forplot$var1_ntile,
                 "<br>Value: ", round(forplot$var1, 2),
                 "<br><b>",var2_label,"</b> ",
                 # "<br>Ntile: ",forplot$var2_ntile,
                 "<br>Value: ",  round(forplot$var2, 2))

  # FIXME: region names not working
  if(!is.na(region_names)) labs <- paste0("<b>",forplot$region_name_label,"</b><br>",labs)

  labs <- purrr::map(labs, htmltools::HTML)

  # are we adding a legend? we may not want to, if e.g. we're being clever and
  # showing different bivariate choropleths (with the same palettes) at different
  # zoom levels
  if (add_legend){
    map <- map %>%
      leaflet::addControl(
        html = palette_html,
        position = "bottomleft",
      )
  }
  map %>%
    # leaflet::addControl(
    #   html = palette_html,
    #   position = "bottomleft",
    # )   %>%
    leaflet::addPolygons(data = forplot,
                         label = labs,
                         fillColor = ~pal_colour,
                         ...)

}




# function_for_deciles_per_region <- function(dta, n_dec = 5, region, income) {
#   dta %>% filter(regno == region) %>%
#     agg_by_deciles(
#     dec_by = income,
#     dec_vars = dec_vars,
#     wt_var = get_wt_nm(),
#     n_dec = n_dec,
#     get_var_fn = get_var_nm
#   ) %>%
#     group_by(Decile, Income_value, Income, Simulation) %>%
#     summarise(across(where(is.numeric), ~ sum(., na.rm = TRUE)), .groups = "drop") %>%
#     ungroup() %>%
#     select(Decile, Income, value) %>%
#     mutate(regno = region, Income = income) %>%
#     mutate(var = glue("{Income}_quintile_{Decile}")) %>%
#     select(regno, var, value)
# }

#' @noRd
#' @import dplyr purrr glue
calculate_summary_stats_for_map <- function(sim_res, number) {
  # Initializing variables
  policy_choices <- sim_res[[number]]$policy_choices
  title <- sim_res[[number]]$policy_name

  policy_name <- NA_character_
  income_vars_tbl_pa <- get_inc_nm(suffix = "_pa") %>%
    filter(!var %in% c("yg_pa", "yf_pa"))

  income_vars_tbl_pc <- get_inc_nm(suffix = "_pc") %>%
    filter(!var %in% c("yg_pc"))

  poverty_line_var_pa <- "pline_mod"
  poverty_line_var_pc <- "pline_lmic"

  wt_var <- get_wt_nm()

  wt_var_sym <- sym(wt_var)
  poverty_line_var_2_pa <- sym(poverty_line_var_pa)
  poverty_line_var_2_pc <- sym(poverty_line_var_pc)

  dta <- sim_res[[number]]$policy_sim_raw
  dta <- mutate(dta, pl_var_pa = !!poverty_line_var_2_pa, pl_var_pc = !!poverty_line_var_2_pc)

  dec_vars <- c("tot_dtaxes_pc", "tot_itaxes_pc", "tot_pensions_pc", "tot_dtransfer_pc",
                "tot_subsidies_pc")

  dta_long_pa <- dta %>%
    select(any_of(income_vars_tbl_pa$var), "pl_var_pa", all_of(wt_var), regno) %>%
    pivot_longer(names_to = "var", cols = any_of(income_vars_tbl_pa$var))

  dta_long_pc <- dta %>%
    select(any_of(income_vars_tbl_pc$var), "pl_var_pc", all_of(wt_var), regno) %>%
    pivot_longer(names_to = "var", cols = any_of(income_vars_tbl_pc$var))

  poverty_rates_table_regions_pa <- dta_long_pa %>%
    group_by(var, regno) %>%
    summarise(across(value, list(value = ~ calc_pov_fgt(x = ., pl = pl_var_pa, alpha = 0, w = !!wt_var_sym, na.rm = TRUE) * 100), .names = "{.fn}")) %>%
    mutate(var = glue("{var}_poverty_pa")) %>%
    pivot_wider(names_from = var, values_from = value)

  poverty_rates_table_regions_pc <- dta_long_pc %>%
    group_by(var, regno) %>%
    summarise(across(value, list(value = ~ calc_pov_fgt(x = ., pl = pl_var_pc, alpha = 0, w = !!wt_var_sym, na.rm = TRUE) * 100), .names = "{.fn}")) %>%
    mutate(var = glue("{var}_poverty_pc")) %>%
    pivot_wider(names_from = var, values_from = value)

  gini_rates_table_regions <- dta_long_pc %>%
    group_by(var, regno) %>%
    summarise(across(value, list(value = ~ calc_gini(x = ., w = !!wt_var_sym, na.rm = TRUE, drop_zero_and_less = FALSE) * 100), .names = "{.fn}")) %>%
    mutate(var = glue("{var}_gini_pc")) %>%
    pivot_wider(names_from = var, values_from = value)

  theil_rates_table_regions <- dta_long_pc %>%
    group_by(var, regno) %>%
    summarise(across(value, list(value = ~ calc_theil(x = ., w = !!wt_var_sym, na.rm = TRUE) * 100), .names = "{.fn}")) %>%
    mutate(var = glue("{var}_theil_pc")) %>%
    pivot_wider(names_from = var, values_from = value)

  p90_p10_rates_table_regions <- dta_long_pc %>%
    group_by(var, regno) %>%
    summarise(across(value, list(value = ~ calc_top_bottom_percentiles(x = ., w = !!wt_var_sym,bottom_percentile = 0.1,
                                                                       top_percentile = 0.9)), .names = "{.fn}")) %>%
    mutate(var = glue("{var}_p90_p10_pc")) %>%
    pivot_wider(names_from = var, values_from = value)
  #all_regions <- unique(dta$regno)
   all_incomes <- c("ym_pc", "yd_pc", "yc_pc", "yf_pc")
  #
  # combinations <- expand_grid(region = all_regions, income = all_incomes)
  #
  #
  # quintile_results <- combinations %>%
  #   mutate(data = map2(region, income, ~ function_for_deciles_per_region(dta, 5, .x, .y)))
  #
  #
  # quintile_results_table_regions_old <- quintile_results %>%
  #   unnest(data)  %>%
  #   select(regno, var, value) %>%
  #   pivot_wider(names_from = var, values_from = value)
  # dec_var <- c("tot_dtaxes_pc", "tot_itaxes_pc", "tot_pensions_pc", "tot_dtransfer_pc",
  #               "tot_subsidies_pc", "tot_education_pc", "tot_health_pc")
  #

  quintile_results_table_regions <- dta %>%
    group_by(regno) %>%
    mutate(across(
      any_of(all_incomes),
      ~ statar::xtile(.,  n = 5,
                      wt = !!wt_var_sym) %>%
        factor()
    )) %>%
    select(regno, any_of(all_incomes),  any_of(dec_vars), any_of(wt_var)) %>%
    ungroup() %>%
    pivot_longer(cols = all_of(all_incomes),
                 values_to = "Decile",
                 names_to = "Income") %>%
    group_by(regno, Income,  Decile) %>%
    summarise(across(any_of(dec_vars),    ~ sum(. * {
      {
        wt_var_sym
      }
    }, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(value = tot_pensions_pc + tot_dtransfer_pc + tot_subsidies_pc - (tot_dtaxes_pc + tot_itaxes_pc) ) %>%
    select( regno, Income, Decile, value) %>%
    mutate(var = glue("{Income}_quintile_{Decile}")) %>%
    select(regno, var, value) %>%
    pivot_wider(names_from = var, values_from = value)


  #diffdf::diffdf(quintile_results_table_regions, quintile_results_table_regions_old)

  summary_stats_baseline <- poverty_rates_table_regions_pa %>%
    left_join(poverty_rates_table_regions_pc) %>%
    left_join(gini_rates_table_regions) %>%
    left_join(theil_rates_table_regions) %>%
    left_join(p90_p10_rates_table_regions) %>%
    left_join(quintile_results_table_regions)

  return(summary_stats_baseline)
}

remove_item <- function(lst, item_name) {
  for (category in names(lst)) {
    if (item_name %in% lst[[category]]) {
      lst[[category]] <- lst[[category]][lst[[category]] != item_name]
    }
  }
  return(lst)
}


# copied from https://github.com/chris-prener/biscale/blob/main/R/bi_legend.R to add a string wrap


#' @import dplyr purrr glue
bi_legend_build_updated <- function(leg, dim, xlab, ylab, size, pad_width, pad_color, breaks,
                                    arrows, family = "sans", wrap_length ){

  # global bindings
  bi_fill = x = y = NULL
  wrap_length = wrap_length
  # nse
  xQN <- as.name(xlab)
  yQN <- as.name(ylab)

  # create tibble for plotting
  leg <- data.frame(
    bi_class = names(leg),
    bi_fill = leg
  )

  leg$x <- as.integer(substr(leg$bi_class, 1, 1))
  leg$y <- as.integer(substr(leg$bi_class, 3, 3))

  # create ggplot2 legend object
  ## initial build
  legend <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = leg, mapping = ggplot2::aes(x = str_wrap(x, wrap_length),
                                                          y = str_wrap(y,wrap_length),
                                                          fill = bi_fill), lwd = pad_width, col = pad_color) +
    ggplot2::scale_fill_identity()

  ## optionally add breaks
  if (is.null(breaks) == FALSE){

    breaks_include <- TRUE

    if (length(breaks$bi_x) == dim){

      breaks_seq <- seq(from = 1, to = dim, by = 1)

    } else if (length(breaks$bi_x) == dim+1){

      breaks_seq <- seq(from = 0.5, to = dim+0.5, by = 1)

    }

    legend <- legend +
      ggplot2::scale_x_continuous(
        breaks = breaks_seq,
        labels = breaks$bi_x,
        expand = c(.015, .015)) +
      ggplot2::scale_y_continuous(
        breaks = breaks_seq,
        labels = breaks$bi_y,
        expand = c(.015, .015))

  } else {

    breaks_include <- FALSE

  }

  ## add arrows
  if (arrows == TRUE) {


    yQN <- str_wrap(yQN, wrap_length)
    xQN <- str_wrap(xQN, wrap_length)

    # add labels
    legend <- legend +
      ggplot2::labs(x = substitute(paste(xQN, ""%->%"")), y = substitute(paste(yQN, ""%->%"")))

  } else if (arrows == FALSE){

    # add labels
    legend <- legend +
      ggplot2::labs(x =  str_wrap(xQN,wrap_length), y =  str_wrap(yQN,wrap_length))

  }

  ## final legend elements
  legend <- legend +
    ggplot2::theme(axis.title = ggplot2::element_text(size = size, vjust = 0, hjust = 0)) +
    ggplot2::coord_fixed()

  ## add theme
  if (breaks_include == TRUE){
    legend <- legend + biscale:::bi_theme_legend(base_size = size, base_family = family)+
      ggplot2::theme(axis.title = ggplot2::element_text(size = size, vjust = 0, hjust = 0))
  } else if (breaks_include == FALSE){
    legend <- legend + biscale:::bi_theme(base_size = size, base_family = family)+
      ggplot2::theme(axis.title = ggplot2::element_text(size = size, vjust = 0, hjust = 0))
  }

  # return output
  return(legend)

}

bi_legend_updated <- function (pal, dim = 3, xlab, ylab, size = 25, flip_axes = FALSE,
                               rotate_pal = FALSE,
                               pad_width = NA,
                               pad_color = "#ffffff",
                               wrap_length = 1000,
                               breaks = NULL,
                               arrows = TRUE)
{
  bi_class = bi_fill = x = y = NULL
  if (missing(pal) == TRUE) {
    stop("A palette name or a custom palette vector must be specified for the 'pal' argument. Please see bi_pal's help file for a list of included palettes.")
  }
  if (is.logical(arrows) == FALSE) {
    stop("A logical scalar must be supplied for 'arrows'. Please provide either 'TRUE' or 'FALSE'.")
  }
  if (missing(xlab) == TRUE) {
    xlab <- "x var "
  }
  if (is.character(xlab) == FALSE) {
    stop("The 'xlab' argument must be a character string.")
  }
  if (missing(ylab) == TRUE) {
    ylab <- "y var "
  }
  if (is.character(ylab) == FALSE) {
    stop("The 'ylab' argument must be a character string.")
  }
  if (is.numeric(size) == FALSE) {
    stop("The 'size' argument must be a numeric value.")
  }
  # biscale:::pal_validate(pal = pal, dim = dim, flip_axes = flip_axes,
  #              rotate_pal = rotate_pal)
  if (length(pal) == 1) {
    leg <- biscale:::bi_pal_pull(pal = pal, dim = dim, flip_axes = flip_axes,
                                 rotate_pal = rotate_pal)
  }
  else if (length(pal) > 1) {
    leg <- pal
  }
  out <- bi_legend_build_updated(leg = leg, dim = dim, xlab = xlab,
                                 ylab = ylab, size = size, pad_width = pad_width, pad_color = pad_color,
                                 wrap_length = wrap_length,
                                 breaks = breaks, arrows = arrows)
  return(out)
}






#' @import leaflet leaflet.extras2 biscale
stat_map_render_funct <- function(input = input, results_for_maps = results_for_maps(), number){

  # Define the mapping of variable names to labels Probably want to move this out of this function
  var_labels <- list(
    "ym_pa_poverty_pa" = "poverty rate market income (national pl)",
    "yp_pa_poverty_pa" = "poverty rate market income + pensions (national pl)",
    "yc_pa_poverty_pa" = "poverty rate consumable income (national pl)",
    "yd_pa_poverty_pa" = "poverty rate disposable income (national pl)",
    "ym_pc_poverty_pc" = "poverty rate market income (international pl)",
    "yp_pc_poverty_pc" = "poverty rate market income + pensions (international pl)",
    "yc_pc_poverty_pc" = "poverty rate consumable income (international pl)",
    "yd_pc_poverty_pc" = "poverty rate disposable income (international pl)",

    "ym_pc_p90_p10_pc"  = "90/10 income ratio (market income)",
    "yp_pc_p90_p10_pc"  = "90/10 income ratio (market + pensions income)" ,
    "yd_pc_p90_p10_pc"  =  "90/10 income ratio (disposable income)" ,
    "yc_pc_p90_p10_pc"  =  "90/10 income ratio (consumable income)",
    "yf_pc_p90_p10_pc" = "90/10 income ratio (final income)",


    "ym_pc_gini_pc" = "GINI index (market income)",
    "yp_pc_gini_pc" = "GINI index (market + pensions income)",
    "yd_pc_gini_pc" = "GINI index (disposable income)",
    "yc_pc_gini_pc" = "GINI index (consumable income)",
    "yf_pc_gini_pc" = "GINI index (final income)",

    "ym_pc_theil_pc" = "Theil index (market income)",
    "yp_pc_theil_pc" = "Theil index (market + pensions income)",
    "yd_pc_theil_pc" = "Theil index (disposable income)",
    "yc_pc_theil_pc" = "Theil index (consumable income)",
    "yf_pc_theil_pc" = "Theil index (final income)",

    "ym_pc_quintile_1" = "net cash position first quintile (market income)",
    "ym_pc_quintile_2" = "net cash position second quintile (market income)",
    "ym_pc_quintile_3" = "net cash position third quintile (market income)",
    "ym_pc_quintile_4" = "net cash position fourth quintile (market income)",
    "ym_pc_quintile_5" = "net cash position fifth quintile (market income)",
    "yd_pc_quintile_1" = "net cash position first quintile (disposable income)",
    "yd_pc_quintile_2" = "net cash position second quintile (disposable income)",
    "yd_pc_quintile_3" = "net cash position third quintile (disposable income)",
    "yd_pc_quintile_4" = "net cash position fourth quintile (disposable income)",
    "yd_pc_quintile_5" = "net cash position fifth quintile (disposable income)",
    "yc_pc_quintile_1" = "net cash position 1st quintile (market income + pensions)",
    "yc_pc_quintile_2" = "net cash position 2nd quintile (market income + pensions)",
    "yc_pc_quintile_3" = "net cash position 3rd quintile (market income + pensions)",
    "yc_pc_quintile_4" = "net cash position 4th quintile (market income + pensions)",
    "yc_pc_quintile_5" = "net cash position 5th quintile (market income + pensions)",
    "yf_pc_quintile_1" = "net cash position 1st quintile (market income + pensions)",
    "yf_pc_quintile_2" = "net cash position 2nd quintile (market income + pensions)",
    "yf_pc_quintile_3" = "net cash position 3rd quintile (market income + pensions)",
    "yf_pc_quintile_4" = "net cash position 4th quintile (market income + pensions)",
    "yf_pc_quintile_5" = "net cash position 5th quintile (market income + pensions)"
  )




  var1_name <- input$var1_name
  var2_name <- input$var2_name

  var1_label <- var_labels[[var1_name]]
  var2_label <- if (var2_name == "no_second_var") "" else var_labels[[var2_name]]

  var_1_sym <- sym(var1_name)
  var_2_sym <- sym(var2_name)


  data_for_ggmap <-  results_for_maps[[number]] %>%
    st_transform(crs = 32638)


  if(  var2_name == "no_second_var"){

    data_for_ggmap <- data_for_ggmap %>%
      mutate(label = paste0(ADM1_EN,
                            "\n",
                            var_labels[var1_name], ": ",
                            ifelse(is.na({
                              {
                                var_1_sym
                              }
                            }), "-", format(round({
                              {
                                var_1_sym
                              }
                            }, 1), nsmall = 1))))
  } else{

    data_for_ggmap <- data_for_ggmap %>%
      mutate(label =  paste0(
        ADM1_EN,
        "\n",
        as.vector(var_labels[var1_name]),
        ": ",
        ifelse(is.na({
          {
            var_1_sym
          }
        }), "-", format(round({
          {
            var_1_sym
          }
        }, 1), nsmall = 1)),
        "\n",
        var_labels[var2_name],
        ": ",
        ifelse(is.na({
          {
            var_2_sym
          }
        }), "-", format(round({
          {
            var_2_sym
          }
        }, 1), nsmall = 1))
      ))
  }

  if (var2_name == "no_second_var") {


    final_plot <-  ggplot(data = data_for_ggmap)+
      geom_sf(data =data_for_ggmap , aes(fill = {{var_1_sym}} ))+
      scale_fill_gradientn(colors = as.vector( pals::brewer.ylgn(100)),
                           na.value = "grey80") +
      labs(fill = str_wrap(var1_label, 15)) +
      cowplot::theme_map() +
      theme(plot.background = element_rect(fill = "white", color = "transparent")) +
      theme(legend.position = "left", # Move legend to the left
            legend.key.size = unit(2, "lines"), # Bigger legend keys
            legend.text = element_text(size = 12), # Bigger text, match Arial as an example
            legend.title = element_text(size = 14)) +
      # ggrepel::geom_text_repel(data = data_for_ggmap,
      #                          aes(label =  str_wrap(ADM1_EN, 15), geometry = geometry),
      #                          stat = "sf_coordinates",
      ggrepel::geom_label_repel(data = data_for_ggmap %>%
                                  filter(ADM1_EN  %in% c("Autonomous Republic of Abkhazia",
                                                         "Samegrelo-Zemo Svaneti",
                                                         "Guria",
                                                         "Autonomous Republic of Adjara")),
                                aes(label =  label, geometry = geometry,
                                    fill=  {{var_1_sym}}),
                                stat = "sf_coordinates",
                                alpha = .7,
                                xlim = c(- 200000, -50000),
                                #ylim = c(40, 50),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "y",
                                #hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0) +
      ggrepel::geom_label_repel(data = data_for_ggmap %>%
                                  filter(ADM1_EN  %in% c("Samtskhe-Javakheti",
                                                         "Kvemo Kartli",
                                                         "Shida Kartli" )),
                                aes(label =  label, geometry = geometry,
                                    fill=  {{var_1_sym}}),
                                stat = "sf_coordinates",
                                alpha = .7,
                                #xlim = c(50000, 60000),
                                ylim = c(4000000  , 4500000),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "x",
                                #hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0) +
      ggrepel::geom_label_repel(data = data_for_ggmap%>%
                                  filter(ADM1_EN  %in% c("Racha-Lechkhumi and Kvemo Svaneti",
                                                         "Imereti" )),
                                aes(label =  label, geometry = geometry,
                                    fill=  {{var_1_sym}}),
                                stat = "sf_coordinates",
                                alpha = .7,
                                xlim = c(0, 800000 ),
                                ylim = c(4850000, 5000000),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "x",
                                # hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0) +
      ggrepel::geom_label_repel(data = data_for_ggmap %>%
                                  filter(ADM1_EN  %in% c( "Kakheti",
                                                          "Tbilisi",
                                                          "Provisional Administration",
                                                          "Mtskheta-Mtianeti")),
                                aes(label =  label, geometry = geometry,
                                    fill=  {{var_1_sym}}),
                                stat = "sf_coordinates",
                                alpha = .7,
                                xlim = c(800000, 1000000),
                                ylim = c(4500000, 4900000),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "y",
                                hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0) +
      coord_sf(xlim = c(-300000, 1100000 ),
               ylim = c(4450000, 4900000),
               clip = "off"
      )
  } else{


    # Temporarily rename the columns in the data frame
    data_temp <- data_for_ggmap %>%
      rename(x_var = all_of(var1_name), y_var = all_of(var2_name))

    # Pass column names directly using !!
    biscale_data <- bi_class(
      data_temp,
      x = x_var,
      y = y_var,
      style = "quantile",
      dim = 3
    )


    custom_pal3 <- c(
      "1-1" = "#FFFFE5", # low x, low y (done)
      "2-1" = "#ADDD8E", # done
      "3-1" = "#238443", # high x, low y (done)
      "1-2" = "#F7FCB9", # x1, y2
      "2-2" = "#78C679", # medium x, medium y (done)
      "3-2" = "#006837", #x3, y2
      "1-3" = "#D9F0A3", # low x, high y (done)
      "2-3" = "#41AB5D", #x2 y3
      "3-3" = "#004529" # high x, high y (done)
    )

    map <- ggplot() +
      geom_sf(data = biscale_data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
      bi_scale_fill(pal = custom_pal3, dim = 3) +
      labs(
        # title = "Race and Income in St. Louis, MO",
        #  subtitle = "Gray Pink (GrPink) Palette"
      ) +
      ggrepel::geom_label_repel(data = biscale_data %>%
                                  filter(ADM1_EN  %in% c("Autonomous Republic of Abkhazia",
                                                         "Samegrelo-Zemo Svaneti",
                                                         "Guria",
                                                         "Autonomous Republic of Adjara")),
                                aes(label =  label, geometry = geometry, fill = bi_class),
                                stat = "sf_coordinates",
                                alpha = .7,
                                xlim = c(- 200000, -50000),
                                #ylim = c(40, 50),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "y",
                                #hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0) +
      ggrepel::geom_label_repel(data = biscale_data %>%
                                  filter(ADM1_EN  %in% c("Samtskhe-Javakheti",
                                                         "Kvemo Kartli",
                                                         "Shida Kartli" )),
                                aes(label =  label, geometry = geometry, fill = bi_class),
                                stat = "sf_coordinates",
                                alpha = .7,
                                #xlim = c(50000, 60000),
                                ylim = c(4000000  , 4500000),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "x",
                                #hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0) +
      ggrepel::geom_label_repel(data = biscale_data%>%
                                  filter(ADM1_EN  %in% c("Racha-Lechkhumi and Kvemo Svaneti",
                                                         "Imereti" )),
                                aes(label =  label, geometry = geometry, fill = bi_class),
                                stat = "sf_coordinates",
                                alpha = .7,
                                xlim = c(0, 800000 ),
                                ylim = c(4850000, 5000000),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "x",
                                # hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0) +
      ggrepel::geom_label_repel(data = biscale_data %>%
                                  filter(ADM1_EN  %in% c( "Kakheti",
                                                          "Tbilisi",
                                                          "Provisional Administration",
                                                          "Mtskheta-Mtianeti")),
                                aes(label =  label, geometry = geometry, fill = bi_class),
                                stat = "sf_coordinates",
                                alpha = .7,
                                xlim = c(800000, 1000000),
                                ylim = c(4500000, 4900000),
                                max.time	= 10,
                                max.iter	= 1000000,
                                direction = "y",
                                hjust         = 0,
                                max.overlaps = Inf,
                                segment.curvature = -1e-20,
                                segment.angle = 90,
                                segment.alpha = .5,
                                size = 3,
                                segment.linetype = "dotted",
                                box.padding = 1,
                                #segment.ncp = 3,
                                # family="century gothic bold",
                                min.segment.length = 0)+

      cowplot::theme_map()+
      theme(plot.background = element_rect(fill = "white",
                                           color = "transparent"),
            legend.position = "none") +
      coord_sf(xlim = c(-300000, 1100000 ),
               ylim = c(4450000, 4900000),
               clip = "off"
      )

    legend <- bi_legend_updated(pal = custom_pal3,
                                dim = 3,
                                wrap_length = 25,
                                xlab = var1_label,
                                ylab =  var2_label,
                                size = 16)



    # final_plot <-cowplot:: ggdraw() +
    #   cowplot::draw_plot(map, .2, 0, .8, 1) +
    #   cowplot::draw_plot(legend, 0, 0.5, 0.3, 0.3, hjust = 0, vjust = 0)

    final_plot <-  legend + map + patchwork::plot_layout(widths = c(0.15, 0.85))


  }
  final_plot
}


mod_maps_server <- function(id, sim_res = sim_res(),
                            title = "Results presented on interactive map", ...) {
#' Title
#'
#' @noRd
#' @import bslib sf
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    var1_choices <- list(
      "Poverty rate"= c(
        "poverty rate market income (national poverty line)" = "ym_pa_poverty_pa",
        "poverty rate market income + pensions (national poverty line)"= "yp_pa_poverty_pa",
        "poverty rate consumable income (national poverty line)"= "yc_pa_poverty_pa",
        "poverty rate disposable income (national poverty line)" = "yd_pa_poverty_pa",
        "poverty rate market income (international poverty line)" = "ym_pc_poverty_pc",
        "poverty rate market income + pensions (international poverty line)" = "yp_pc_poverty_pc",
        "poverty rate consumable income (international poverty line)"= "yc_pc_poverty_pc",
        "poverty rate disposable income (international poverty line)" = "yd_pc_poverty_pc"
      ),


      "Inequality" = c(
        "90/10 income ratio (market income)"  = "ym_pc_p90_p10_pc",
        "90/10 income ratio (market + pensions income)"  = "yp_pc_p90_p10_pc",
        "90/10 income ratio (disposable income)"  = "yd_pc_p90_p10_pc",
        "90/10 income ratio (consumable income)"  = "yc_pc_p90_p10_pc",
        "90/10 income ratio (final income)"  = "yf_pc_p90_p10_pc",

        "GINI index (market income)"  = "ym_pc_gini_pc",
        "GINI index (market + pensions income)"  = "yp_pc_gini_pc",
        "GINI index (disposable income)"  = "yd_pc_gini_pc",
        "GINI index (consumable income)"  = "yc_pc_gini_pc",
        "GINI index (final income)"  = "yf_pc_gini_pc",
        "Theil index (market income)"  = "ym_pc_theil_pc",
        "Theil index (market + pensions income)"  = "yp_pc_theil_pc",
        "Theil index (disposable income)"  = "yd_pc_theil_pc",
        "Theil index (consumable income)"  = "yc_pc_theil_pc",
        "Theil index (final income)"  = "yf_pc_theil_pc"
      ),
      "Net Cash Position" = c(
        "first quintile (market income)" = "ym_pc_quintile_1",
        "second quintile (market income)" = "ym_pc_quintile_2",
        "third quintile (market income)" = "ym_pc_quintile_3",
        "fourth quintile (market income)" = "ym_pc_quintile_4",
        "fifth quintile (market income)" = "ym_pc_quintile_5",
        "first quintile (disposable income)" = "yd_pc_quintile_1",
        "second quintile (disposable income)" = "yd_pc_quintile_2",
        "third quintile (disposable income)" = "yd_pc_quintile_3",
        "fourth quintile (disposable income)" = "yd_pc_quintile_4",
        "fifth quintile (disposable income)" = "yd_pc_quintile_5",
        "first quintile (market income + pensions)" = "yc_pc_quintile_1",
        "second quintile (market income + pensions)" = "yc_pc_quintile_2",
        "third quintile (market income + pensions)" = "yc_pc_quintile_3",
        "fourth quintile (market income + pensions)" = "yc_pc_quintile_4",
        "fifth quintile (market income + pensions)" = "yc_pc_quintile_5",
        "first quintile (market income + pensions)" = "yf_pc_quintile_1",
        "second quintile (market income + pensions)" = "yf_pc_quintile_2",
        "third quintile (market income + pensions)" = "yf_pc_quintile_3",
        "fourth quintile (market income + pensions)" = "yf_pc_quintile_4",
        "fifth quintile (market income + pensions)" = "yf_pc_quintile_5"
      )
    )

    var2_choices <- list(
      "no second variable " = "no_second_var",
      "Poverty rate"= c(
        "poverty rate market income (national poverty line)" = "ym_pa_poverty_pa",
        "poverty rate market income + pensions (national poverty line)"= "yp_pa_poverty_pa",
        "poverty rate consumable income (national poverty line)"= "yc_pa_poverty_pa",
        "poverty rate disposable income (national poverty line)" = "yd_pa_poverty_pa",
        "poverty rate market income (international poverty line)" = "ym_pc_poverty_pc",
        "poverty rate market income + pensions (international poverty line)" = "yp_pc_poverty_pc",
        "poverty rate consumable income (international poverty line)"= "yc_pc_poverty_pc",
        "poverty rate disposable income (international poverty line)" = "yd_pc_poverty_pc"
      ),
      "Inequality" = c(
        "90/10 income ratio (market income)"  = "ym_pc_p90_p10_pc",
        "90/10 income ratio (market + pensions income)"  = "yp_pc_p90_p10_pc",
        "90/10 income ratio (disposable income)"  = "yd_pc_p90_p10_pc",
        "90/10 income ratio (consumable income)"  = "yc_pc_p90_p10_pc",
        "90/10 income ratio (final income)"  = "yf_pc_p90_p10_pc",

        "GINI index (market income)"  = "ym_pc_gini_pc",
        "GINI index (market + pensions income)"  = "yp_pc_gini_pc",
        "GINI index (disposable income)"  = "yd_pc_gini_pc",
        "GINI index (consumable income)"  = "yc_pc_gini_pc",
        "GINI index (final income)"  = "yf_pc_gini_pc",
        "Theil index (market income)"  = "ym_pc_theil_pc",
        "Theil index (market + pensions income)"  = "yp_pc_theil_pc",
        "Theil index (disposable income)"  = "yd_pc_theil_pc",
        "Theil index (consumable income)"  = "yc_pc_theil_pc",
        "Theil index (final income)"  = "yf_pc_theil_pc"
      ),
      "Net Cash Position" = c(
        "first quintile (market income)" = "ym_pc_quintile_1",
        "second quintile (market income)" = "ym_pc_quintile_2",
        "third quintile (market income)" = "ym_pc_quintile_3",
        "fourth quintile (market income)" = "ym_pc_quintile_4",
        "fifth quintile (market income)" = "ym_pc_quintile_5",
        "first quintile (disposable income)" = "yd_pc_quintile_1",
        "second quintile (disposable income)" = "yd_pc_quintile_2",
        "third quintile (disposable income)" = "yd_pc_quintile_3",
        "fourth quintile (disposable income)" = "yd_pc_quintile_4",
        "fifth quintile (disposable income)" = "yd_pc_quintile_5",
        "first quintile (market income + pensions)" = "yc_pc_quintile_1",
        "second quintile (market income + pensions)" = "yc_pc_quintile_2",
        "third quintile (market income + pensions)" = "yc_pc_quintile_3",
        "fourth quintile (market income + pensions)" = "yc_pc_quintile_4",
        "fifth quintile (market income + pensions)" = "yc_pc_quintile_5",
        "first quintile (market income + pensions)" = "yf_pc_quintile_1",
        "second quintile (market income + pensions)" = "yf_pc_quintile_2",
        "third quintile (market income + pensions)" = "yf_pc_quintile_3",
        "fourth quintile (market income + pensions)" = "yf_pc_quintile_4",
        "fifth quintile (market income + pensions)" = "yf_pc_quintile_5"
      )
    )

    # Define the mapping of variable names to labels
    var_labels <- list(
      "ym_pa_poverty_pa" = "poverty rate market income (national pl)",
      "yp_pa_poverty_pa" = "poverty rate market income + pensions (national pl)",
      "yc_pa_poverty_pa" = "poverty rate consumable income (national pl)",
      "yd_pa_poverty_pa" = "poverty rate disposable income (national pl)",
      "ym_pc_poverty_pc" = "poverty rate market income (international pl)",
      "yp_pc_poverty_pc" = "poverty rate market income + pensions (international pl)",
      "yc_pc_poverty_pc" = "poverty rate consumable income (international pl)",
      "yd_pc_poverty_pc" = "poverty rate disposable income (international pl)",

      "ym_pc_p90_p10_pc"  = "90/10 income ratio (market income)",
      "yp_pc_p90_p10_pc"  = "90/10 income ratio (market + pensions income)" ,
      "yd_pc_p90_p10_pc"  =  "90/10 income ratio (disposable income)" ,
      "yc_pc_p90_p10_pc"  =  "90/10 income ratio (consumable income)",
      "yf_pc_p90_p10_pc" = "90/10 income ratio (final income)",


      "ym_pc_gini_pc" = "GINI index (market income)",
      "yp_pc_gini_pc" = "GINI index (market + pensions income)",
      "yd_pc_gini_pc" = "GINI index (disposable income)",
      "yc_pc_gini_pc" = "GINI index (consumable income)",
      "yf_pc_gini_pc" = "GINI index (final income)",

      "ym_pc_theil_pc" = "Theil index (market income)",
      "yp_pc_theil_pc" = "Theil index (market + pensions income)",
      "yd_pc_theil_pc" = "Theil index (disposable income)",
      "yc_pc_theil_pc" = "Theil index (consumable income)",
      "yf_pc_theil_pc" = "Theil index (final income)",

      "ym_pc_quintile_1" = "net cash position first quintile (market income)",
      "ym_pc_quintile_2" = "net cash position second quintile (market income)",
      "ym_pc_quintile_3" = "net cash position third quintile (market income)",
      "ym_pc_quintile_4" = "net cash position fourth quintile (market income)",
      "ym_pc_quintile_5" = "net cash position fifth quintile (market income)",
      "yd_pc_quintile_1" = "net cash position first quintile (disposable income)",
      "yd_pc_quintile_2" = "net cash position second quintile (disposable income)",
      "yd_pc_quintile_3" = "net cash position third quintile (disposable income)",
      "yd_pc_quintile_4" = "net cash position fourth quintile (disposable income)",
      "yd_pc_quintile_5" = "net cash position fifth quintile (disposable income)",
      "yc_pc_quintile_1" = "net cash position 1st quintile (market income + pensions)",
      "yc_pc_quintile_2" = "net cash position 2nd quintile (market income + pensions)",
      "yc_pc_quintile_3" = "net cash position 3rd quintile (market income + pensions)",
      "yc_pc_quintile_4" = "net cash position 4th quintile (market income + pensions)",
      "yc_pc_quintile_5" = "net cash position 5th quintile (market income + pensions)",
      "yf_pc_quintile_1" = "net cash position 1st quintile (market income + pensions)",
      "yf_pc_quintile_2" = "net cash position 2nd quintile (market income + pensions)",
      "yf_pc_quintile_3" = "net cash position 3rd quintile (market income + pensions)",
      "yf_pc_quintile_4" = "net cash position 4th quintile (market income + pensions)",
      "yf_pc_quintile_5" = "net cash position 5th quintile (market income + pensions)"
    )



    output$map_ui <- renderUI({
      validate(
        need(sim_res(), "Click `Run` on the policy choices tab to see the results.")
      )
      req(sim_res())

      fluidPage(
        #theme = bs_theme(),
        tags$head(
          tags$style(HTML("
  .leaflet-container {
    background: #FFF;
  }"))),

        #if( length(sim_res()) == 2 ){

        fluidRow(

          column(4,
                 shinyjs::useShinyjs(),  # Initialize shinyjs

                 h4("Options"),
                 p("Results take some time to compute"),
                 checkboxInput( ns("show_backgroud_map"),
                                label = "show backgroud maps",
                                value = TRUE, width = NULL),
                 checkboxInput(ns("compare_to_baseline"),
                               label ="allow for comparison to baseline",
                               value = FALSE, width = NULL),
                 shiny::sliderInput( ns("transparency_maps"),
                                     min = 0,
                                     max = 100,
                                     value = 70,
                                     step = 1,
                                label = "transparency of map overlay"),
                 checkboxInput(ns("show_static_map"),
                               label ="show static map",
                               value = FALSE, width = NULL)


          ),


          column(4,
                 h4("Primary variable to display on map"),
                 selectInput(
                   ns("var1_name"),
                   "Variable 1:",
                   choices = var1_choices
                 )

          ),
          column(4,
                 h4("Optional second variable"),
                   selectInput(
                     ns("var2_name"),
                     "Variable 2:",
                     choices = var2_choices
                   )

          )),
        uiOutput(ns("maps_display")),
        #tags$hr(),
        h3("Results in table"),
        column(12, shinycssloaders::withSpinner(DT::DTOutput(
          ns("geo_results_table")
        ))),
        tags$hr()

      )

    })

    observeEvent(input$compare_to_baseline, {
      if (input$compare_to_baseline) {
        shinyjs::disable("show_static_map")
        updateCheckboxInput(session, "show_static_map", value = FALSE)

      } else {
        shinyjs::enable("show_static_map")
      }
    })


    observeEvent(input$show_static_map, {
      if (input$show_static_map) {
        shinyjs::disable("transparency_maps")
      } else {
        shinyjs::enable("transparency_maps")
      }
    })

    # Define reactive expressions for data loading and processing
    shape_file_data <- reactive({
      req(sim_res())  # Ensure sim_res is available
      shape_file <- sf::st_read("./inst/extdata/geo_admbnda_adm1_geostat_20191018.json") %>%
        dplyr::select(ADM1_EN, ADM1_KA, geometry) %>%
        mutate(regno = case_when(
          ADM1_EN == "Kakheti" ~ 0,
          ADM1_EN == "Tbilisi" ~ 1,
          ADM1_EN == "Shida Kartli" ~ 2,
          ADM1_EN == "Kvemo Kartli" ~ 3,
          ADM1_EN == "Autonomous Republic of Abkhazia" ~ 4,
          ADM1_EN == "Samtskhe-Javakheti" ~ 5,
          ADM1_EN == "Provisional Administration" ~ 6,
          ADM1_EN == "Autonomous Republic of Adjara" ~ 7,
          ADM1_EN == "Guria" ~ 8,
          ADM1_EN == "Samegrelo-Zemo Svaneti" ~ 9,
          ADM1_EN == "Imereti" ~ 10,
          ADM1_EN == "Mtskheta-Mtianeti" ~ 11,
          ADM1_EN == "Racha-Lechkhumi and Kvemo Svaneti" ~ 13,
          TRUE ~ NA_integer_
        ))
      return(shape_file)
    })

    observe({
      req(sim_res())


      results_for_maps <- eventReactive(list(input$results_tab_choice == "map_panel"), {
        sim_res_val <- sim_res()
        shape_file <- shape_file_data()  # Get the pre-processed shape file data

        if(length(sim_res())==3){
          list(
            baseline = shape_file %>% left_join(calculate_summary_stats_for_map(sim_res = sim_res_val, number = 1)),
            policy_1 = shape_file %>% left_join(calculate_summary_stats_for_map(sim_res = sim_res_val, number = 2)),
            policy_2 = shape_file %>% left_join(calculate_summary_stats_for_map(sim_res = sim_res_val, number = 3))
          )
        }else{
          list(
            baseline = shape_file %>% left_join(calculate_summary_stats_for_map(sim_res = sim_res_val, number = 1)),
            policy_1 = shape_file %>% left_join(calculate_summary_stats_for_map(sim_res = sim_res_val, number = 2)))
        }


      }, ignoreNULL = FALSE)



      observeEvent( input$var1_name, {
        var1_selected <-  input$var1_name
        var2_selected <-  input$var2_name


        var2_choices <- remove_item(var2_choices, var1_selected)

        updateSelectInput(session, "var2_name", choices = var2_choices, selected = var2_selected)
      })


      observeEvent(input$var2_name, {
        var1_selected <-  input$var1_name
      var2_selected <-  input$var2_name

        var1_choices <- remove_item(var1_choices, var2_selected)

        updateSelectInput(session, "var1_name", choices = var1_choices, selected = var1_selected)
      })


      output$map_1 <-   leaflet::renderLeaflet({
        req(results_for_maps())  # Ensure results are available before rendering the map

        var1_name <- input$var1_name

        #var1_name <- "ym_pc_quintile_4"
        #var2_name <- "ym_pc_quintile_5"
        var2_name <- input$var2_name
        var1_label <- var_labels[[var1_name]]
        var2_label <- if (var2_name == "no_second_var") "" else var_labels[[var2_name]]



        if (var2_name == "no_second_var") {

          labs_baseline <- paste0("<b>", "Baseline results","</b><br>",
                                  "<b>",results_for_maps()[[1]][["ADM1_EN"]],"</b><br>",
                                  "<b>",var1_label,"</b>",
                                  #"<br>Ntile: ", forplot$var1_ntile,
                                  "<br>Value: ", round(results_for_maps()[[1]][[var1_name]], 2))

          labs_policy <- paste0("<b>", "Results  policy 1","</b><br>",
                                "<b>",results_for_maps()[[2]][["ADM1_EN"]],"</b><br>",
                                "<b>",var1_label,"</b>",
                                #"<br>Ntile: ", forplot$var1_ntile,
                                "<br>Value: ", round(results_for_maps()[[2]][[var1_name]], 2))

          labs_baseline <- purrr::map(labs_baseline, htmltools::HTML)
          labs_policy   <- purrr::map(labs_policy, htmltools::HTML)

          #palette <- colorNumeric(pals::brewer.ylgn(9), domain = results_for_maps()[[1]][[var1_name]])
          map <- leaflet() %>%
            addMapPane("left", zIndex = 0) %>%
            addMapPane("right", zIndex = 0)

          if(input$show_backgroud_map) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto", layerId = "cartoid", options = pathOptions(pane = "left")) %>%
              addProviderTiles(providers$CartoDB.Positron, group = "base", layerId = "baseid", options = pathOptions(pane = "right"))
          }

          map <- map %>%
            addPolygons(data = results_for_maps()[[1]],
                        fillColor = ~colorQuantile( pals::brewer.ylgn(9), results_for_maps()[[1]][[var1_name]])(results_for_maps()[[1]][[var1_name]]),
                        fillOpacity = input$transparency_maps/ 100,
                        color = "white",
                        weight = 2,
                        highlightOptions = leaflet::highlightOptions(color = "orange", weight = 2, opacity = 1),                  ,
                        label = labs_baseline,
                        options = pathOptions(pane = "right"))  %>%
            addPolygons(data = results_for_maps()[[2]],
                        fillColor = ~colorQuantile( pals::brewer.ylgn(9), results_for_maps()[[2]][[var1_name]])(results_for_maps()[[2]][[var1_name]]),
                        fillOpacity = input$transparency_maps/ 100,
                        color = "white",
                        weight = 2,
                        highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),                  ,
                        label = labs_policy,
                        options = pathOptions(pane = "left"))  %>%
            addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
            addLegend("bottomleft",
                      pal = colorQuantile( pals::brewer.ylgn(9), NULL),
                      values = unique(results_for_maps()[[1]][[var1_name]]),
                      title = var1_label,
                      opacity = 1)
        } else {


          map <- leaflet() %>%
            addMapPane("left", zIndex = 0) %>%
            addMapPane("right", zIndex = 0)

          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto", layerId = "cartoid", options = pathOptions(pane = "left")) %>%
              addProviderTiles(providers$CartoDB.Positron, group = "base", layerId = "baseid", options = pathOptions(pane = "right"))
          }

          map <- map %>%
            addBivariateChoropleth(
              map_data = results_for_maps()[[1]],
              labs_prefix = "Baseline results",
              var1_name = var1_name,
              var2_name = var2_name,
              ntiles = 3,
              var1_label = var1_label,
              var2_label = var2_label,
              region_name = "ADM1_EN",
              color = "white",
              weight = 2,
              fillOpacity = input$transparency_maps/ 100,
              highlightOptions = leaflet::highlightOptions(color = "orange", weight = 2, opacity = 1),
              options = pathOptions(pane = "right")
            ) %>%
            addBivariateChoropleth(
              add_legend = FALSE,
              labs_prefix = "Policy results",

              map_data = results_for_maps()[[2]],
              var1_name = var1_name,
              var2_name = var2_name,
              ntiles = 3,
              var1_label = var1_label,
              var2_label = var2_label,
              region_name = "ADM1_EN",
              fillOpacity = input$transparency_maps/ 100,
              color = "white",
              weight = 2,
              highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),
              options = pathOptions(pane = "left")
            ) %>%
            addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid")
        }

        map
      })


      output$map_2 <-   leaflet::renderLeaflet({
        var1_name <- input$var1_name

        #var1_name <- "ym_pc_quintile_4"
        var2_name <- input$var2_name
        var1_label <- var_labels[[var1_name]]
        var2_label <- if (var2_name == "no_second_var") "" else var_labels[[var2_name]]



        if (var2_name == "no_second_var") {

          labs_baseline <- paste0("<b>", "Baseline results","</b><br>",
                                  "<b>",results_for_maps()[[1]][["ADM1_EN"]],"</b><br>",
                                  "<b>",var1_label,"</b>",
                                  #"<br>Ntile: ", forplot$var1_ntile,
                                  "<br>Value: ", round(results_for_maps()[[1]][[var1_name]], 2))

          labs_policy <- paste0("<b>", "Results policy 2","</b><br>",
                                "<b>",results_for_maps()[[3]][["ADM1_EN"]],"</b><br>",
                                "<b>",var1_label,"</b>",
                                #"<br>Ntile: ", forplot$var1_ntile,
                                "<br>Value: ", round(results_for_maps()[[3]][[var1_name]], 2))

          labs_baseline <- purrr::map(labs_baseline, htmltools::HTML)
          labs_policy   <- purrr::map(labs_policy, htmltools::HTML)

          map <- leaflet() %>%
            addMapPane("left", zIndex = 0) %>%
            addMapPane("right", zIndex = 0)

          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto", layerId = "cartoid", options = pathOptions(pane = "left")) %>%
              addProviderTiles(providers$CartoDB.Positron, group = "base", layerId = "baseid", options = pathOptions(pane = "right"))
          }

          map <- map %>%
            addPolygons(data = results_for_maps()[[1]],
                        fillColor = ~colorQuantile( pals::brewer.ylgn(9), results_for_maps()[[1]][[var1_name]])(results_for_maps()[[1]][[var1_name]]),
                        fillOpacity = input$transparency_maps/ 100,
                        color = "white",
                        weight = 2,
                        highlightOptions = leaflet::highlightOptions(color = "orange", weight = 2, opacity = 1),                  ,
                        label = labs_baseline,
                        options = pathOptions(pane = "right"))  %>%
            addPolygons(data = results_for_maps()[[3]],
                        fillColor = ~colorQuantile( pals::brewer.ylgn(9), results_for_maps()[[3]][[var1_name]])(results_for_maps()[[3]][[var1_name]]),
                        fillOpacity = input$transparency_maps/ 100,
                        color = "white",
                        weight = 2,
                        highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),                  ,
                        label = labs_policy,
                        options = pathOptions(pane = "left"))  %>%
            addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
            addLegend("bottomleft",
                      pal = colorQuantile( pals::brewer.ylgn(9), NULL),
                      values = unique(results_for_maps()[[1]][[var1_name]]),
                      title = var1_label,
                      opacity = 1)
        } else {


          map <- leaflet() %>%
            addMapPane("left", zIndex = 0) %>%
            addMapPane("right", zIndex = 0)

          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto", layerId = "cartoid", options = pathOptions(pane = "left")) %>%
              addProviderTiles(providers$CartoDB.Positron, group = "base", layerId = "baseid", options = pathOptions(pane = "right"))
          }

          map <- map %>%
            addBivariateChoropleth(
              map_data = results_for_maps()[[1]],
              labs_prefix = "Baseline results",
              var1_name = var1_name,
              var2_name = var2_name,
              ntiles = 3,
              var1_label = var1_label,
              var2_label = var2_label,
              region_name = "ADM1_EN",
              fillOpacity = input$transparency_maps/ 100,
              color = "white",
              weight = 2,
              highlightOptions = leaflet::highlightOptions(color = "orange", weight = 2, opacity = 1),
              options = pathOptions(pane = "right")
            ) %>%
            addBivariateChoropleth(
              add_legend = FALSE,
              map_data = results_for_maps()[[3]],
              labs_prefix = "Policy results",

              var1_name = var1_name,
              var2_name = var2_name,
              ntiles = 3,
              var1_label = var1_label,
              var2_label = var2_label,
              region_name = "ADM1_EN",
              color = "white",
              weight = 2,
              fillOpacity = input$transparency_maps/ 100,
              highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),
              options = pathOptions(pane = "left")
            ) %>%
            addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid")
        }

        map
      })
      output$map_1_nocomp <-   leaflet::renderLeaflet({
        var1_name <- input$var1_name

        #var1_name <- "ym_pc_quintile_4"
        var2_name <- input$var2_name
        var1_label <- var_labels[[var1_name]]
        var2_label <- if (var2_name == "no_second_var") "" else var_labels[[var2_name]]



        if (var2_name == "no_second_var") {

          labs_baseline <- paste0("<b>", "Baseline results","</b><br>",
                                  "<b>",results_for_maps()[[1]][["ADM1_EN"]],"</b><br>",
                                  "<b>",var1_label,"</b>",
                                  #"<br>Ntile: ", forplot$var1_ntile,
                                  "<br>Value: ", round(results_for_maps()[[1]][[var1_name]], 2))


          labs_baseline   <- purrr::map(labs_baseline, htmltools::HTML)

          #palette <- colorNumeric(pals::brewer.ylgn(9), domain = results_for_maps()[[1]][[var1_name]])
          map <- leaflet()

          if(input$show_backgroud_map) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.Positron, group = "base")
          }

          map <- map %>%
            addPolygons(data = results_for_maps()[[1]],
                        fillColor = ~colorQuantile( pals::brewer.ylgn(9), results_for_maps()[[1]][[var1_name]])(results_for_maps()[[1]][[var1_name]]),
                        fillOpacity = input$transparency_maps/ 100,
                        color = "white",
                        weight = 2,
                        highlightOptions = leaflet::highlightOptions(color = "orange", weight = 2, opacity = 1),                  ,
                        label = labs_baseline)  %>%
            addLegend("bottomleft",
                      pal = colorQuantile( pals::brewer.ylgn(9), NULL),
                      values = unique(results_for_maps()[[1]][[var1_name]]),
                      title = var1_label,
                      opacity = 1)
        } else {


          map <- leaflet()

          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.Positron, group = "base")
          }

          map <- map %>%
            addBivariateChoropleth(
              map_data = results_for_maps()[[1]],
              labs_prefix = "Baseline results",
              var1_name = var1_name,
              var2_name = var2_name,
              ntiles = 3,
              var1_label = var1_label,
              var2_label = var2_label,
              region_name = "ADM1_EN",
              color = "white",
              weight = 2,
              fillOpacity = input$transparency_maps/ 100,
              highlightOptions = leaflet::highlightOptions(color = "orange", weight = 2, opacity = 1),
            )

        }

        map
      })


      output$map_2_nocomp <-   leaflet::renderLeaflet({
        var1_name <- input$var1_name

        #var1_name <- "ym_pc_quintile_4"
        var2_name <- input$var2_name
        var1_label <- var_labels[[var1_name]]
        var2_label <- if (var2_name == "no_second_var") "" else var_labels[[var2_name]]



        if (var2_name == "no_second_var") {


          labs_policy <- paste0("<b>", "Results policy 1","</b><br>",
                                "<b>",results_for_maps()[[2]][["ADM1_EN"]],"</b><br>",
                                "<b>",var1_label,"</b>",
                                #"<br>Ntile: ", forplot$var1_ntile,
                                "<br>Value: ", round(results_for_maps()[[2]][[var1_name]], 2))

          labs_policy   <- purrr::map(labs_policy, htmltools::HTML)

          #palette <- colorNumeric(pals::brewer.ylgn(9), domain = results_for_maps()[[1]][[var1_name]])

          map <- leaflet()

          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto")
          }

          map <- map %>%
            addPolygons(data = results_for_maps()[[2]],
                        fillColor = ~colorQuantile( pals::brewer.ylgn(9), results_for_maps()[[2]][[var1_name]])(results_for_maps()[[2]][[var1_name]]),
                        fillOpacity = input$transparency_maps/ 100,
                        color = "white",
                        weight = 2,
                        highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),                  ,
                        label = labs_policy,
            )  %>%
            addLegend("bottomleft",
                      pal = colorQuantile( pals::brewer.ylgn(9), NULL),
                      values = unique(results_for_maps()[[2]][[var1_name]]),
                      title = var1_label,
                      opacity = 1)
        } else {


          map <- leaflet()

          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto")
          }

          map <- map %>%
            addBivariateChoropleth(
              add_legend = FALSE,
              map_data = results_for_maps()[[2]],
              labs_prefix = "Policy results",

              var1_name = var1_name,
              var2_name = var2_name,
              ntiles = 3,
              var1_label = var1_label,
              var2_label = var2_label,
              region_name = "ADM1_EN",
              color = "white",
              weight = 2,
              fillOpacity = input$transparency_maps/ 100,
              highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),
            )
        }
        map
      })

      output$map_3_nocomp <-   leaflet::renderLeaflet({
        var1_name <- input$var1_name

        #var1_name <- "ym_pc_quintile_4"
        var2_name <- input$var2_name
        var1_label <- var_labels[[var1_name]]
        var2_label <- if (var2_name == "no_second_var") "" else var_labels[[var2_name]]



        if (var2_name == "no_second_var") {


          labs_policy <- paste0("<b>", "Results policy 2","</b><br>",
                                "<b>",results_for_maps()[[3]][["ADM1_EN"]],"</b><br>",
                                "<b>",var1_label,"</b>",
                                #"<br>Ntile: ", forplot$var1_ntile,
                                "<br>Value: ", round(results_for_maps()[[3]][[var1_name]], 2))

          labs_policy   <- purrr::map(labs_policy, htmltools::HTML)

          #palette <- colorNumeric(pals::brewer.ylgn(9), domain = results_for_maps()[[1]][[var1_name]])

          map <- leaflet()

          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto")
          }

          map <- map %>%
            addPolygons(data = results_for_maps()[[3]],
                        fillColor = ~colorQuantile( pals::brewer.ylgn(9), results_for_maps()[[3]][[var1_name]])(results_for_maps()[[3]][[var1_name]]),
                        fillOpacity = input$transparency_maps/ 100,
                        color = "white",
                        weight = 2,
                        highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),                  ,
                        label = labs_policy,
            )  %>%
            addLegend("bottomleft",
                      pal = colorQuantile( pals::brewer.ylgn(9), NULL),
                      values = unique(results_for_maps()[[3]][[var1_name]]),
                      title = var1_label,
                      opacity = 1)
        } else {


          map <- leaflet()
          if(input$show_backgroud_map == TRUE) {
            map <- map %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "carto")
          }

          map <- map %>%
            addBivariateChoropleth(
              add_legend = FALSE,
              map_data = results_for_maps()[[3]],
              labs_prefix = "Policy results",

              var1_name = var1_name,
              var2_name = var2_name,
              ntiles = 3,
              var1_label = var1_label,
              var2_label = var2_label,
              region_name = "ADM1_EN",
              fillOpacity = input$transparency_maps/ 100,
              color = "white",
              weight = 2,
              highlightOptions = leaflet::highlightOptions(color = "red", weight = 2, opacity = 1),
            )
        }
        map
      })

      ### Create Static maps



      # Dynamic title rendering
      output$baseline_title_map_ui <- renderUI({
        h3(paste0("Geospatial results: ", sim_res()[[1]]$policy_name))
      })

      output$policy_1_title_map_ui <- renderUI({
        h3(paste0("Geospatial results: ", sim_res()[[2]]$policy_name))
      })

      output$policy_2_title_map_ui <- renderUI({
        h3(paste0("Geospatial results: ", sim_res()[[3]]$policy_name))
      })


      rv_plot1 <- reactiveValues(plot = NULL)
      rv_plot2 <- reactiveValues(plot = NULL)
      rv_plot3 <- reactiveValues(plot = NULL)



      static_plot_1 <- reactive({

        stat_map_render_funct(input = input, results_for_maps = results_for_maps(), number = 1)
      })

      static_plot_2 <- reactive({

        stat_map_render_funct(input = input, results_for_maps = results_for_maps(), number = 2)
      })


      static_plot_3 <- reactive({

        stat_map_render_funct(input = input, results_for_maps = results_for_maps(), number = 3)
      })




      #
      output$static_map_1 <-   renderPlot({

        req(sim_res())
        req(input$show_static_map)
        req(static_plot_1())
        rv_plot1$plot <-static_plot_1()
        rv_plot1$plot

      }, res = 72)

      output$static_map_2 <-   renderPlot({

        req(sim_res())
        req(input$show_static_map)
        req(static_plot_2())
        rv_plot2$plot <-static_plot_2()
        rv_plot2$plot

      }, res = 72)


      output$static_map_3 <-   renderPlot({

        req(sim_res())
        req(input$show_static_map)
        req(static_plot_3())
        rv_plot3$plot <-static_plot_3()
        rv_plot3$plot

      }, res = 72)


      observeEvent(input$more_1, {
        esquisse::save_ggplot_modal(id = session$ns("export_map1"),
                                    title = "Export chart")
      })

      observeEvent(input$more_2, {
        esquisse::save_ggplot_modal(id = session$ns("export_map2"),
                                    title = "Export chart")
      })

      observeEvent(input$more_3, {
        esquisse::save_ggplot_modal(id = session$ns("export_map3"),
                                    title = "Export chart")
      })



      save_ggplot_server2("export_map1",
                          plot_rv = rv_plot1,
                          dpi = 450,
                          scale = 2)


      save_ggplot_server2("export_map2",
                          plot_rv = rv_plot2,
                          dpi = 450,
                          scale = 2)


      save_ggplot_server2("export_map3",
                          plot_rv = rv_plot3,
                          dpi = 450,
                          scale = 2)


      output$geo_results_table <- DT::renderDT({



        var_labels_unlisted <- unlist(var_labels)


        combined_df <- map_dfr(names(results_for_maps()), ~ mutate(results_for_maps()[[.x]], source = .x)) %>%
          sf::st_drop_geometry() %>%
          as_tibble() %>%
          select(-c(ADM1_KA, regno )) %>%
          pivot_longer(cols = -c(ADM1_EN, source),
                       names_to = "variable") %>%
          mutate(value = round(value, 2)) %>%
          pivot_wider(names_from = ADM1_EN,
                      values_from = value) %>%
          filter(!variable %in% c("yf_pc_poverty_pc")) %>%
          mutate(variable = case_when(
            variable %in% names(var_labels_unlisted) ~ var_labels_unlisted[variable],
            TRUE ~ variable
          ))


        if(length(sim_res()) == 2){
          combined_df <- combined_df %>%
          mutate(source = case_when(source == "baseline" ~ sim_res()[[1]]$policy_name,
                                    source == "policy_1" ~ sim_res()[[2]]$policy_name,
                                    TRUE ~ source
          ))
        }else{
          combined_df <- combined_df %>%
          mutate(source = case_when(source == "baseline" ~ sim_res()[[1]]$policy_name,
                                    source == "policy_1" ~ sim_res()[[2]]$policy_name,
                                    source == "policy_2" ~ sim_res()[[3]]$policy_name,
                                    TRUE ~ source
          ))
        }


        combined_df %>% fct_config_export_dt_updated("Geospatial Results")
      }, server = FALSE)



      # Dynamically create UI based on checkbox
      output$maps_display <- renderUI({

        if(length(sim_res()) ==2){
          if (input$compare_to_baseline) {
            # UI for maps with comparison
            list(
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("policy_1_title_map_ui"))),
                            card_body(shinycssloaders::withSpinner(leafletOutput(ns("map_1"))))
                )

              )
            )
          } else {
            # UI for maps without comparison
            list(
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("baseline_title_map_ui"))),
                     if(input$show_static_map){
                       card_body(shinycssloaders::withSpinner(plotOutput(ns("static_map_1"))),
                                 actionButton(
                                   inputId = ns("more_1"),
                                   label = "Save plot",
                                   icon = icon("download"),
                                   class = "btn-sm"
                                 ))
                     }else{
                       card_body(shinycssloaders::withSpinner(leafletOutput(ns("map_1_nocomp"))))
                     }

                     )
              ),
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("policy_1_title_map_ui"))),
                     if(input$show_static_map){
                       card_body(shinycssloaders::withSpinner(plotOutput(ns("static_map_2"))),
                                 actionButton(
                                   inputId = ns("more_2"),
                                   label = "Save plot",
                                   icon = icon("download"),
                                   class = "btn-sm"
                                 ))
                     }else{
                       card_body(shinycssloaders::withSpinner(leafletOutput(ns("map_2_nocomp"))))
                     }

                     )
              )
            )
          }
        }else{

          if (input$compare_to_baseline) {
            # UI for maps with comparison
            list(
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("policy_1_title_map_ui"))),
                     card_body(shinycssloaders::withSpinner(leafletOutput(ns("map_1"))))

                     )
              ),
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("policy_2_title_map_ui"))),
                     shinycssloaders::withSpinner(leafletOutput(ns("map_2"))))
              )
            )
          } else {
            # UI for maps without comparison
            list(
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("baseline_title_map_ui"))),

                     if(input$show_static_map){
                       card_body(shinycssloaders::withSpinner(plotOutput(ns("static_map_1"))),
                                 actionButton(
                                   inputId = ns("more_1"),
                                   label = "Save plot",
                                   icon = icon("download"),
                                   class = "btn-sm"
                                 ))
                     }else{
                     card_body(shinycssloaders::withSpinner(leafletOutput(ns("map_1_nocomp"))))
                       }

                     )
              ),
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("policy_1_title_map_ui"))),
                     if(input$show_static_map){
                       card_body(shinycssloaders::withSpinner(plotOutput(ns("static_map_2"))),
                                 actionButton(
                                   inputId = ns("more_2"),
                                   label = "Save plot",
                                   icon = icon("download"),
                                   class = "btn-sm"
                                 ))
                     }else{
                       card_body(shinycssloaders::withSpinner(leafletOutput(ns("map_2_nocomp"))))
                     }

                     )
              ),
              fluidRow(
                card(full_screen = TRUE,
                     card_header(uiOutput(ns("policy_2_title_map_ui"))),
                     if(input$show_static_map){
                       card_body(shinycssloaders::withSpinner(plotOutput(ns("static_map_3"))),
                                 actionButton(
                                   inputId = ns("more_3"),
                                   label = "Save plot",
                                   icon = icon("download"),
                                   class = "btn-sm"
                                 ))
                     }else{
                       card_body(shinycssloaders::withSpinner(leafletOutput(ns("map_3_nocomp"))))
                     }
                     )
              )
            )
          }
        }
      })
    })


  })
}

# Define the module UI
mod_map_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("map_ui"))
}
