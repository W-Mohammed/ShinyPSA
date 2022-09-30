################################################################################
#
# Script Name:        compute_EVPPIs_.R
# Module Name:        Economic/PSA
# Script Description: Defines EVPPIs estimating function.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Compute the Expected Value of Perfect Partial Information (EVPPI).
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param .effs A matrix containing the \code{effects} from PSA. Number of
#' \code{columns} is equal to the interventions while the number of
#' \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .costs A matrix containing the \code{costs} from PSA. Number of
#' \code{columns} is equal to the interventions while the number of
#' \code{rows} is equal to the number of PSA simulations to be summarised.
#' @param .params A matrix containing parameters' configurations used in
#' the PSA.
#' @param .MAICER_ The Maximum acceptable incremental cost-effectiveness ratio.
#' (MAICER) to be considered in the summary table. Default value is
#' \code{30,000}.
#' @param .units_ A character, the units to associate with the
#' monitory values in the summary table. Default is sterling pounds
#' (GBP) \code{"\u00A3"}.
#' @param .individual_evppi_ Logical (default \code{TRUE}) to return per person
#' EVPPI, otherwise population EVPPI will be reported.
#' @param .evppi_population_ The size of the population that is annually
#' affected by the competing health technologies under evaluation.
#' @param .discount_rate_ The discount rate used to discount future affected
#' populations.
#' @param .time_horion_ The time expected to pass (in years) before the
#' interventions under consideration change (how long before the decision
#' under consideration become obsolete or requires updating).
#' @param .session
#'
#' @return A list containing the EVPPI results table and caption information.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' # Summarise PSA results:
#' PSA_summary = summarise_PSA_(
#'   .effs = ShinyPSA::Brennan_1K_PSA$e,
#'   .costs = ShinyPSA::Brennan_1K_PSA$c,
#'   .params = ShinyPSA::Brennan_1K_PSA$p,
#'   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
#'
#' # Estimate EVPPI:
#' EVPPI_ind_res <- compute_EVPPIs_(
#'     .PSA_data = PSA_summary,
#'     .MAICER_ = 30000)
#'
#' EVPPI_pop_res <- compute_EVPPIs_(
#'     .PSA_data = PSA_summary,
#'     .MAICER_ = 20000,
#'     .individual_evppi_ = FALSE,
#'     .evppi_population_ = 1000,
#'     .time_horion_ = 5)
#' }
compute_EVPPIs_ <- function(.PSA_data,
                            .effs = NULL,
                            .costs = NULL,
                            .params = NULL,
                            .MAICER_ = 30000,
                            .units_ = "\u00A3",
                            .individual_evppi_ = TRUE,
                            .evppi_population_ = NULL,
                            .discount_rate_ = 0.035,
                            .time_horion_ = NULL,
                            .session = NULL) {

  # Sanity checks:----
  if(is.null(.effs) | is.null(.costs) | is.null(.params)) {
    .effs = .PSA_data$e
    .costs = .PSA_data$c
    .params = .PSA_data$p
  }

  # Estimate individual EVPPI:----
  ## Calculate incremental net benefit (INB):----
  inb <- createInb( # Strong et al. function
    costs.int = .costs,
    effects.int = .effs,
    lambda = .MAICER_)

  EVPPI <- applyCalcSingleParamGam( # Strong et al. function
    .params = .PSA_data$p,
    nb = inb,
    session = .session)

  # Estimate population EVPPI if user provided necessary data:----
  discounted_population = 1
  table_caption = "Individual EVPPI"
  if(!.individual_evppi_) {
    if(is.null(.evppi_population_) | is.null(.time_horion_)) {
      .individual_evppi_ <- TRUE
      message("Population EVPPI or decision time horizon were not supplied.
              The function will calculate individual EVPPI")
    }
  }
  if(!.individual_evppi_) {
    ## Re-estimate discounted population for population EVPPI:----
    discounted_population <- sum(
      .evppi_population_ / ((1 + .discount_rate_)^(1:.time_horion_)))
    table_caption = paste0("Population EVPPI:- ",
                           "Population size: ", .evppi_population_, "; ",
                           "Time horizon: ", .time_horion_, " year(s); ",
                           "Discount rate: ", .discount_rate_ * 100, "%.")
  }

  # Prepare EVPI:----
  EVPI <- if(!is.null(.PSA_data[["EVPI"]])) {
    ## Get EVPI data from the PSA_data object:----
    dplyr::tibble(
      'EVPI_values' = .PSA_data[["EVPI"]] * discounted_population,
      ## put WTP in a column next to EVPI:----
      'WTP_values' = .PSA_data[["WTPs"]]) %>%
      ## filter and keep values corresponding to ones the in .MAICER_ vector:----
    dplyr::filter(WTP_values %in% .MAICER_) %>%
      ## rename WTP values to use as column names later:----
    dplyr::pull(var = EVPI_values)
  } else {
    ## Calculate EVPI from inputs if PSA_data object was not provided:----
    ShinyPSA::compute_EVPIs_(
      .effs = .effs,
      .costs = .costs)
  }

  # Prepare EVPPI results table:----
  ## Build the individual EVPPI results table:----
  tmp_name <- paste0("Per Person EVPPI (", .units_, "). MAICER = ",
                     scales::dollar(
                       x = .MAICER_,
                       prefix = .units_))
  ind_evppi <- dplyr::tibble(
    "Parameters" =
      colnames(.PSA_data$p),
    {{tmp_name}} :=
      round(EVPPI[, 1], 2),
    "Standard Error" =
      round(EVPPI[, 2], 2),
    "Indexed to Overall EVPI (%)" =
      scales::percent(round((EVPPI[, 1] / EVPI), 2)))
  ## Build the population EVPPI results table:----
  pop_evppi <- NULL
  if(!isTRUE(.individual_evppi_)) {
    tmp_name <- paste0("Population EVPPI over ", .time_horion_,
                       " years (", .units_, ")")
    pop_evppi <- ind_evppi %>%
      dplyr::mutate(
        {{tmp_name}} :=
          signif(EVPPI[, 1] * discounted_population, 4)
      )
  }

  if(!is.null(pop_evppi)) {
    return(
      list('Population EVPPI' = pop_evppi,
           'Caption' = table_caption))
  } else {
    return(
      list('Population EVPPI' = ind_evppi,
           'Caption' = table_caption))
  }
}

# The functions below were defined by Mark Strong, Penny Breeze, Chloe Thomas
# and Alan Brennan, the authors of SAVI - Sheffield Accelerated Value of
# Information. See [here](https://github.com/Sheffield-Accelerated-VoI/SAVI)!

# Calculating Incremental Net Benefits (INB)
#
# @param costs.int Costs data structure.
# @param effects.int Effects data structure.
# @param lambda Maximum Willingness-to-Pay.
#
# @return
#
# @examples
# \dontrun{
# inb <- createInb(costs, effects, lambda)
# }
createInb <- function(costs.int, effects.int, lambda) {

  inb <- as.matrix(effects.int) * lambda - as.matrix(costs.int)
  inb <- inb - inb[, 1]

  return(inb)
}

# Estimates EVPPI and SE via GAM
#
# @param .params A matrix containing parameters' configurations used in the PSA.
# @param NB Data structure containing Net Benefit (NB).
# @param sets Column containing PSA samples of the parameter of interest.
# @param s Number of simulations for the Monte Carlo computation of the SE.
# @param session Shiny session.
#
# @return
#
# @examples
gamFunc <- function(.params, NB, sets, s = 1000, session = NULL) {

  if(!is.null(dim(NB))) {
    NB <- NB - NB[, 1]
  } else {
    NB <- cbind(0, NB)
  }

  D <- ncol(NB)
  N <- nrow(NB)
  g.hat <- beta.hat <- Xstar <- V <- tilde.g <- vector("list", D)
  g.hat[[1]] <- 0

  input.parameters <- .params
  paramSet <- cbind(cbind(input.parameters)[, sets, drop=FALSE])

  constantParams <- (apply(paramSet, 2, var) == 0)

  if (sum(constantParams) == length(sets)) return(list(EVPI=0, SE=0)) # if all regressors are constant
  if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants

  # check for linear dependence and remove
  paramSet <- cbind(cbind(input.parameters)[, sets, drop=FALSE]) # now with constants removed

  rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)

  while(length(unique(rankifremoved)) > 1) {
    linearCombs <- which(rankifremoved == max(rankifremoved))
    print(paste("Linear dependence: removing column", colnames(paramSet)[max(linearCombs)]))
    paramSet <- cbind(paramSet[, -max(linearCombs), drop=FALSE])
    sets <- sets[-max(linearCombs)]
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }

  while(qr(paramSet)$rank == rankifremoved[1]) { # special case only lincomb left
    print(paste("Linear dependence: removing column", colnames(paramSet)[1]))
    paramSet <- cbind(paramSet[, -1, drop=FALSE])
    sets <- sets[-1]
    rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[,-x])$rank)
  }

  regression.model <- formulaGenerator(colnames(input.parameters)[sets])


  if(!is.null(session)) {
    progress <- shiny::Progress$new(session, min=1, max=D-1)
    on.exit(progress$close())
    progress$set(message = 'Calculating conditional expected net benefits',
                 detail = 'Please wait...')
  }

  for (d in 2:D) {

    if(!is.null(session)) {
      progress$set(value = d - 1)
    }

    print(paste("estimating g.hat for incremental NB for option", d ,"versus 1"))
    dependent <- NB[, d]
    f <- update(formula(dependent~.), formula(paste(".~", regression.model)))
    try_model <- try(model <- mgcv::gam(f, data = data.frame(input.parameters)))
    if (inherits(try_model, "try-error")) {
      regression.model <- formulaGenerator_s(colnames(input.parameters)[sets])
      f <- update(formula(dependent~.), formula(paste(".~", regression.model)))
      model <- mgcv::gam(f, data = data.frame(input.parameters))
    }
    g.hat[[d]] <- model$fitted
    beta.hat[[d]] <- model$coef
    Xstar[[d]] <- predict(model,type = "lpmatrix")
    V[[d]] <- model$Vp
  }

  perfect.info <- mean(do.call(pmax, g.hat))
  baseline <- max(unlist(lapply(g.hat, mean)))
  partial.evpi <- perfect.info - baseline ## estimate EVPI
  rm(g.hat); gc()

  print("computing standard error via Monte Carlo ...")

  for(d in 2:D) {
    sampled.coef <- rockchalk::mvrnorm(s, beta.hat[[d]], V[[d]])
    tilde.g[[d]] <- sampled.coef%*%t(Xstar[[d]])
  }

  tilde.g[[1]] <- matrix(0, nrow=s, ncol=N)
  rm(V, beta.hat, Xstar, sampled.coef);gc()

  sampled.perfect.info <- rowMeans(do.call(pmax, tilde.g))
  sampled.baseline <- do.call(pmax, lapply(tilde.g, rowMeans))
  rm(tilde.g); gc()

  sampled.partial.evpi <- sampled.perfect.info - sampled.baseline
  SE <- sd(sampled.partial.evpi)

  return(list(EVPI=partial.evpi, SE=SE))
}

# Generates the GAM model formulas from the list of parameter names
#
# @param namesList List of parameter names.
#
# @return
#
# @examples
# \dontrun{
# regression.model <- formulaGenerator(colnames(input.parameters)[sets])
# }
formulaGenerator <- function(namesList) {
  form <- paste(namesList, ",", sep = "", collapse = "")
  form <- substr(form, 1, nchar(form) - 1)
  if (length(namesList) == 4) {
    form <- paste("te(", form, ", k = 4)", sep = "") # restrict to 4 knots if 4 params
  } else {
    form <- paste("te(", form, ")", sep = "")
  }
  form
}

# Generates the GAM model formulas from the list of parameter names
#
# @param namesList List of parameter names.
#
# @return
#
# \dontrun{
# regression.model <- formulaGenerator_s(colnames(input.parameters)[sets])
# }
formulaGenerator_s <- function(namesList) {
  form <- paste0(namesList, ",", collapse = "")
  form <- substr(form, 1, nchar(form) - 1)
  if (length(namesList) == 4) {
    form <- paste0("te(", form, ", k = 4)") # restrict to 4 knots if 4 params
    return(form)
  }
  if (length(namesList) == 1) {
    form <- paste0("s(", form, ")") # if single GAM and try error
    print(form)
    return(form)
  }
  form <- paste0("te(", form, ")")
  return(form)
}

# Employ single GAM over supplied parameters
#
# @param .params A matrix containing parameters' configurations used in the PSA.
# @param nb Data structure containing Net Benefit (NB) or Incremental NB (INB).
# @param session Shiny session.
#
# @return
#
# @examples
# \dontrun{
# pEVPI <- applyCalcSingleParamGam(.params, inb, session)
# }
applyCalcSingleParamGam <- function(.params, nb, session = NULL) {

  .params <- as.matrix(.params)

  numVar <- NCOL(.params)

  if(!is.null(session)) {
    progress <- shiny::Progress$new(session, min=1, max=sum(numVar))
    on.exit(progress$close())
    progress$set(message = 'Calculation in progress',
                 detail = 'Please wait...')
  }

  res <- matrix(ncol = 2, nrow = NCOL(.params))

  for (i in 1:NCOL(.params)) {
    if(!is.null(session)) {
      progress$set(i)
    }

    result <- gamFunc(NB = nb, sets = i, s = 1000, .params = .params,
                      session = session)

    res[i, ] <- unlist(result)
  }

  return(res)
}
