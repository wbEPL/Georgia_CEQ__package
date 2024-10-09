#' Title
#'
#' @param inps input file
#' @param presim pressimulation file
#' @param ... other parametsr
#'
#' @export
#' @return completed microsimulation
full_ceq <- function(inps, presim, ...) {

  sim <- list()


  sim <- func_1_direct_transfers(sim, inps, presim)
  sim <- func_2_direct_tax(sim, inps, presim)
  sim <- func_3_capital_tax(sim, inps, presim)
  sim <- func_4_realestate(sim, inps, presim)
  sim <- func_5_landtax(sim, inps, presim)
  sim <- func_6_indtaxes(sim, inps, presim)
  sim <- func_7_subs_elec(sim, inps, presim)
  sim <- func_8_subs_gas(sim, inps, presim)
  sim <- func_9_conssub_water(sim, inps, presim)
  sim <- func_10_conssub_transport(sim, inps, presim)
  sim <- func_11_all_subsidies(sim = sim)
  sim <- func_12_health(sim = sim, inps = inps, presim = presim)
  sim <- func_13_education(sim = sim, inps = inps, presim = presim)

  # run final simulation
  out <- func_99_simulation(sim = sim, inps = inps, presim = presim)
  #sim$out
  out
}
