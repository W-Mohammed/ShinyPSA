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
#' @param .EVPI A data table containing EVPI data.
#' @param .WTPs A vector containing the Willingness-to-pay values over which
#' EVPI values were estimated.
#' @param .params A matrix containing parameters' configurations used in
#' the PSA.
#' @param .set A vector of parameters' names for conditional EVPPI.
#' @param .set_names A vector of parameter-names to be used for EVPPI.
#' @param .subset_ Boolean for whether to estimate conditional EVPPI for a
#' subset of parameters.
#' @param .MAICER_ The Maximum acceptable incremental cost-effectiveness ratio.
#' (MAICER) to be considered in the summary table. Default value is
#' \code{30,000}.
#' @param .units_ A character, the units to associate with the
#' monitory values in the summary table. Default is sterling pounds
#' (GBP) \code{"\u00A3"}.
#' @param .individual_evppi_ Logical (default \code{TRUE}) to return per person
#' EVPPI, otherwise population EVPPI will be reported.
#' @param .discount_rate_ The discount rate used to discount future affected
#' populations.
#' @param .evppi_population_ The size of the population that is annually
#' affected by the competing health technologies under evaluation.
#' @param .time_horion_ The time expected to pass (in years) before the
#' interventions under consideration change (how long before the decision
#' under consideration become obsolete or requires updating).
#' @param .session Shiny app session.
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
#'
#' # Estimate Conditional EVPPI:
#' cEVPPI_ind_res <- compute_EVPPIs_(
#'     .PSA_data = PSA_summary,
#'     .set = c(6, 14, 15, 16),
#'     .subset_ = TRUE,
#'     .MAICER_ = 30000)
#'
#' cEVPPI_pop_res <- compute_EVPPIs_(
#'     .PSA_data = PSA_summary,
#'     .set_names = c("theta6", "theta14", "theta15", "theta16"),
#'     .subset_ = TRUE,
#'     .MAICER_ = 20000,
#'     .individual_evppi_ = FALSE,
#'     .evppi_population_ = 1000,
#'     .time_horion_ = 5)
#' }
compute_EVPPIs_ <- function(.PSA_data, .effs = NULL, .costs = NULL,
                            .EVPI = NULL, .WTPs = NULL, .params = NULL,
                            .set = NULL, .set_names = NULL, .subset_ = FALSE,
                            .MAICER_ = 30000, .units_ = "\u00A3",
                            .individual_evppi_ = TRUE, .discount_rate_ = 0.035,
                            .evppi_population_ = NULL, .time_horion_ = NULL,
                            .session = NULL) {

  # Sanity checks:----
  stopifnot(
    'Please pass necessary data for EVPPI estimation' =
      !is.null(.PSA_data) |
      !is.null(.effs) |
      !is.null(.costs) |
      !is.null(.params),
    'Please pass necessary data for EVPI estimation' =
      !is.null(.PSA_data) |
      !is.null(.EVPI) |
      !is.null(.WTPs)
    )
  if(is.null(.effs) | is.null(.costs) | is.null(.params)) {
    .effs = .PSA_data$e
    .costs = .PSA_data$c
    .params = .PSA_data$p
  }
  stopifnot(
    'Unequal dimensions in .effs and .costs' =
      dim(.effs) == dim(.costs)
  )
  if(is.null(.set) & is.null(.set_names)) {
    .subset_ <- FALSE
  }
  if(!is.null(.set) & is.null(.set_names)) {
    .set_names <- colnames(.params)[.set]
  }
  if(is.null(.set) & !is.null(.set_names)) {
    .set <- which(colnames(.params) %in% .set_names)
  }

  # Estimate individual EVPPI:----
  ## Calculate incremental net benefit (INB):----
  inb <- createInb( # Strong et al. function
    costs.int = .costs,
    effects.int = .effs,
    lambda = .MAICER_)
  ## Calculate per parameter or conditional EVPPI:----
  EVPPI <- if(!isTRUE(.subset_)) {
    applyCalcSingleParamGam( # Strong et al. function
      .params = .params,
      nb = inb,
      .session = .session)
  } else {
    t(
      as.matrix(
        unlist(
          calSubsetEvpi( # Strong et al. function
            .nb = inb,
            .effs = .effs,
            .costs = .costs,
            .params = .params,
            sets = .set,
            .sets_names = .set_names,
            lambda = .MAICER_,
            .session = .session))))}

  # Estimate population EVPPI if user provided necessary data:----
  discounted_population <- 1
  plot_caption <- table_caption <- paste0(
    "Per Individual EVPPI (", .units_, ") estimated at a ",
    scales::dollar(
      x = .MAICER_,
      prefix = .units_),
    " MAICER.
    Percentage values represent EVPPI values indexed to overall EVPI.")

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
    table_caption = paste0("Population EVPPI (", .units_, ") estimated for ",
                           .evppi_population_, " individuals over ",
                           .time_horion_, " year(s) at ", .discount_rate_ * 100,
                           "% annual discount rate.")
  }

  # Prepare EVPI:----
  EVPI <- if(!is.null(.PSA_data[["EVPI"]])) {
    ## Get EVPI data from the PSA_data object:----
    dplyr::tibble(
      'EVPI_values' = .PSA_data[["EVPI"]],
      ## put WTP in a column next to EVPI:----
      'WTP_values' = .PSA_data[["WTPs"]]) %>%
      ## filter and keep values corresponding to ones the in .MAICER_ vector:----
      dplyr::filter(WTP_values %in% .MAICER_) %>%
      ## rename WTP values to use as column names later:----
      dplyr::pull(var = EVPI_values)
  } else if(!is.null(.EVPI) & !is.null(.WTPs)) {
    dplyr::tibble(
      'EVPI_values' = .EVPI,
      ## put WTP in a column next to EVPI:----
      'WTP_values' = .WTPs) %>%
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
    "Parameters" = if(isTRUE(.subset_)) {
      paste(.set_names, collapse = " + ")
    } else {
      colnames(.params)},
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
          signif(EVPPI[, 1] * discounted_population, 4))
  }

  # Prepare results list:----
  if(!is.null(pop_evppi)) {
    return(
      list('Population EVPPI' = pop_evppi,
           'Table caption' = table_caption,
           'Plot caption' = plot_caption))
  } else {
    return(
      list('Individual EVPPI' = ind_evppi,
           'Table caption' = table_caption,
           'Plot caption' = plot_caption))
  }
}

# The functions below were defined by Mark Strong, Penny Breeze, Chloe Thomas
# and Alan Brennan, the authors of SAVI - Sheffield Accelerated Value of
# Information. See [here](https://github.com/Sheffield-Accelerated-VoI/SAVI)!

# GAM functions:----

# Calculating Incremental Net Benefits (INB)
#
# @param costs.int Costs data structure.
# @param effects.int Effects data structure.
# @param lambda Maximum Acceptable Incremental Cost-Effectiveness Ratio (MAICER).
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
# @param .session Shiny session.
#
# @return
#
# @examples
gamFunc <- function(.params, NB, sets, s = 1000, .session = NULL) {

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


  if(!is.null(.session)) {
    progress <- shiny::Progress$new(session, min=1, max=D-1)
    on.exit(progress$close())
    progress$set(message = 'Calculating conditional expected net benefits',
                 detail = 'Please wait...')
  }

  for (d in 2:D) {

    if(!is.null(.session)) {
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
    # sampled.coef <- rockchalk::mvrnorm(s, beta.hat[[d]], V[[d]])
    sampled.coef <- MASS::mvrnorm(s, beta.hat[[d]], V[[d]])
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

  return(form)
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
# pEVPI <- applyCalcSingleParamGam(.params, inb, .session)
# }
applyCalcSingleParamGam <- function(.params, nb, .session = NULL) {

  .params <- as.matrix(.params)

  numVar <- NCOL(.params)

  if(!is.null(.session)) {
    progress <- shiny::Progress$new(session, min=1, max=sum(numVar))
    on.exit(progress$close())
    progress$set(message = 'Calculation in progress',
                 detail = 'Please wait...')
  }

  res <- matrix(ncol = 2, nrow = NCOL(.params))

  for (i in 1:NCOL(.params)) {
    if(!is.null(.session)) {
      progress$set(i)
    }

    result <- gamFunc(NB = nb, sets = i, s = 1000, .params = .params,
                      .session = .session)

    res[i, ] <- unlist(result)
  }

  return(res)
}

# Functions to calculate the GP:----
## Bug fix 25th Jan 2019

# Inverse gamma distribution density function
#
# @param x
# @param alpha
# @param beta
#
# @return
#
# @examples
dinvgamma <- function(x, alpha, beta) {
  (beta ^ alpha) / gamma(alpha) * x ^ (-alpha - 1) * exp(-beta / x)
}

# Gaussian Correlation function?
#
# @param X
# @param phi
# @param m
#
# @return
#
# @examples
cor.Gaussian <- function(X, phi, m) { #  needs sorting...... probably with dist()
  txbuild1 <- function(h) exp(-rowSums(t((t(X) - h) / phi) ^ 2))
  apply(as.matrix(as.matrix(X)[1:m, ]), 1, txbuild1)
}

# Make a matrix with the Gaussian correlation function
#
# @param X
# @param phi
#
# @return
#
# @examples
makeA.Gaussian <- function(X, phi) {
  n <- NROW(X)
  if(length(phi) > 1) {
    b <- diag(phi ^ (-2))
  } else {
    b <- phi ^ (-2) }
  R <- X %*% as.matrix(b) %*% t(X)

  S <- matrix(diag(R), nrow = n, ncol = n)
  exp(2 * R - S - t(S))
}

# Calculate posterior density
#
# @param hyperparams
# @param NB Net Benefits (NB) matrix.
# @param input.m
#
# @return
#
# @examples
post.density <- function(hyperparams, NB, input.m) {

  input.m <- as.matrix(input.m, drop = FALSE)

  N <- nrow(input.m)
  p <- NCOL(input.m)
  H <- cbind(1, input.m)
  q <- ncol(H)

  a.sigma <- 0.001; b.sigma <- 0.001  ##  hyperparameters for IG prior for sigma^2
  a.nu <- 0.001; b.nu <- 1            ##  hyperparameters for IG prior for nu
  delta <- exp(hyperparams)[1:p]
  nu <- exp(hyperparams)[p + 1]

  A <- makeA.Gaussian(input.m, delta)
  Astar <- A + nu * diag(N)
  T <- chol(Astar)
  y <- backsolve(t(T), NB, upper.tri = FALSE)
  x <- backsolve(t(T), H, upper.tri = FALSE)
  tHAstarinvH <- t(x) %*% (x) + 1e-7* diag(q)
  betahat <- solve(tHAstarinvH) %*% t(x) %*% y
  residSS <- y %*% y -t(y) %*% x %*% betahat - t(betahat) %*% t(x) %*% y +
    t(betahat) %*% tHAstarinvH %*% betahat
  prior <- prod(dnorm(log(delta), 0, sqrt(1e5))) * dinvgamma(nu, a.nu, b.nu)
  l <- -sum(log(diag(T))) - 1 / 2 * log(det(tHAstarinvH)) -
    (N - q + 2 * a.sigma) / 2 * log(residSS / 2 + b.sigma) + log(prior)

  return(l)
}

# Estimate Hyper-parameters
#
# @param NB Net Benefits (NB) matrix.
# @param inputs
# @param .session Shiny app session.
#
# @return
#
# @examples
estimate.hyperparameters <- function(NB, inputs, .session = NULL) {

  p <- NCOL(inputs)
  D <- ncol(NB)

  hyperparameters <- vector("list", D)
  hyperparameters[[1]] <- NA

  if(!is.null(.session)){
    progress1 <- shiny::Progress$new(session, min=1, max=D)
    on.exit(progress1$close())
    progress1$set(message = 'Estimating GP hyperparameters',
                  detail = 'Please wait...')
    progress1$set(value = 1)
  }

  for(d in 2:D) {
    if(!is.null(.session)) {
      progress1$set(value = d)
    }

    initial.values <- rep(0, p + 1)
    repeat {
      print(paste("calling optim function for net benefit", d))
      log.hyperparameters <- optim(initial.values, fn=post.density,
                                   NB=NB[, d], input.m=inputs,
                                   method="Nelder-Mead",
                                   control=list(fnscale=-1, maxit=10000, trace=0))$par
      if (sum(abs(initial.values - log.hyperparameters)) < 0.05) {
        hyperparameters[[d]] <- exp(log.hyperparameters)
        break
      }
      initial.values <- log.hyperparameters
    }
  }

  return(hyperparameters)
}

# calculate the GP
#
# @param .params Parameters matrix.
# @param NB Net Benefits (NB) matrix.
# @param sets A vector of parameter-indexes
# @param s Number of simulations for the Monte Carlo computation of the SE.
# @param .session Shiny app session.
#
# @return
#
# @examples
gpFunc <- function(.params, NB, sets, s = 1000, .session = NULL) {

  input.parameters <- .params
  paramSet <- cbind(input.parameters[, sets])
  constantParams <- (apply(paramSet, 2, var) == 0)

  #remove constants
  if (sum(constantParams) == length(sets)) return(list(EVPI=0, SE=0)) # if all regressors are constant
  if (sum(constantParams) > 0) sets <- sets[-which(constantParams)] # remove constants

  # check for linear dependence and remove
  paramSet <- cbind(cbind(input.parameters)[, sets]) # now with constants removed
  rankifremoved <- sapply(1:NCOL(paramSet), function (x) qr(paramSet[, -x])$rank)
  while(length(unique(rankifremoved)) > 1) {
    linearCombs <- which(rankifremoved == max(rankifremoved))
    # print(linearCombs)
    print(paste("Linear dependence: removing column", colnames(paramSet)[max(linearCombs)]))
    paramSet <- cbind(paramSet[, -max(linearCombs)])
    sets <- sets[-max(linearCombs)]
    rankifremoved <- sapply(1:NCOL(paramSet), function(x) qr(paramSet[, -x])$rank)
  }
  if(qr(paramSet)$rank == rankifremoved[1]) {
    paramSet <- cbind(paramSet[, -1]) # special case only lincomb left
    sets <- sets[-1]
    print(paste("Linear dependence: removing column", colnames(paramSet)[1]))
  }

  inputs.of.interest <- sets
  p <- length(inputs.of.interest)

  if(!is.null(dim(NB))) {
    NB <- NB - NB[, 1]
  } else {
    NB <- cbind(0, NB)
  }

  maxSample <- min(7500, nrow(NB)) # to avoid trying to invert huge matrix
  NB <- as.matrix(NB[1:maxSample, ])
  D <- ncol(NB)

  input.matrix <- as.matrix(input.parameters[1:maxSample, inputs.of.interest, drop=FALSE])
  colmin <- apply(input.matrix, 2, min)
  colmax <- apply(input.matrix, 2, max)
  colrange <- colmax - colmin
  input.matrix <- sweep(input.matrix, 2, colmin, "-")
  input.matrix <- sweep(input.matrix, 2, colrange, "/")
  N <- nrow(input.matrix)
  p <- ncol(input.matrix)
  H <- cbind(1, input.matrix)
  q <- ncol(H)

  m <- min(30 * p, 250)
  m <- min(nrow(NB), m)
  setForHyperparamEst <- 1:m # sample(1:N, m, replace=FALSE)
  hyperparameters <- estimate.hyperparameters(NB[setForHyperparamEst, ],
                                              input.matrix[setForHyperparamEst, ],
                                              .session = .session)

  V <- g.hat <- vector("list", D)
  g.hat[[1]] <- rep(0, N)


  if(!is.null(.session)) {
    progress1 <- shiny::Progress$new(.session, min=1, max=D)
    on.exit(progress1$close())
    progress1$set(message = 'Calculating conditional expected net benefits',
                  detail = 'Please wait...')
    progress1$set(value = 1)
  }

  for(d in 2:D)
  {
    if(!is.null(.session)) {
      progress1$set(value = d)
    }
    print(paste("estimating g.hat for incremental NB for option", d, "versus 1"))
    delta.hat <- hyperparameters[[d]][1:p]
    nu.hat <- hyperparameters[[d]][p+1]
    A <- makeA.Gaussian(input.matrix, delta.hat)
    Astar <- A + nu.hat * diag(N)
    Astarinv <- chol2inv(chol(Astar))
    rm(Astar); gc()
    AstarinvY <- Astarinv %*% NB[, d]
    tHAstarinv <- t(H) %*% Astarinv
    tHAHinv <- solve(tHAstarinv %*% H + 1e-7* diag(q))
    betahat <- tHAHinv %*% (tHAstarinv %*% NB[, d])
    Hbetahat <- H %*% betahat
    resid <- NB[, d] - Hbetahat
    g.hat[[d]] <- Hbetahat+A %*% (Astarinv %*% resid)
    AAstarinvH <- A %*% t(tHAstarinv)
    sigmasqhat <- as.numeric(t(resid) %*% Astarinv %*% resid)/(N - q - 2)
    V[[d]] <- sigmasqhat*(nu.hat * diag(N) - nu.hat ^ 2 * Astarinv +
                            (H - AAstarinvH) %*% (tHAHinv %*% t(H - AAstarinvH)))
    rm(A, Astarinv, AstarinvY, tHAstarinv, tHAHinv, betahat, Hbetahat, resid, sigmasqhat);gc()
  }

  perfect.info <- mean(do.call(pmax, g.hat))
  baseline <- max(unlist(lapply(g.hat, mean)))

  partial.evpi <- perfect.info - baseline

  print("Computing standard error via Monte Carlo")
  tilde.g <- vector("list", D)
  tilde.g[[1]] <- matrix(0, nrow=s, ncol=min(N, 1000))   # bug fix 25.1.19

  if(!is.null(.session)) {
    progress2 <- shiny::Progress$new(session, min=1, max=D)
    on.exit(progress2$close())
    progress2$set(message = 'Calculating Standard Error',
                  detail = 'Please wait...')
    progress2$set(value = 1)
  }

  for(d in 2:D) {
    if(!is.null(.session)) {
      progress2$set(value = d)
    }
    tilde.g[[d]] <- MASS::mvrnorm(s,
                                  g.hat[[d]][1:(min(N, 1000))],
                                  V[[d]][1:(min(N, 1000)), 1:(min(N, 1000))])
  }
  if(!is.null(.session)) {
    progress2$close()
  }

  sampled.perfect.info <- rowMeans(do.call(pmax, tilde.g))
  sampled.baseline <- do.call(pmax, lapply(tilde.g, rowMeans))
  rm(tilde.g);gc()

  sampled.partial.evpi <- sampled.perfect.info - sampled.baseline
  SE <- sd(sampled.partial.evpi)
  rm(V, g.hat);gc()

  return(list(EVPI=partial.evpi, SE=SE))
}

# Calculate Parameters' Subset EVPPI
#
# @param .nb Net Benefits (NB) matrix.
# @param .effs A matrix containing the \code{effects} from PSA. Number of
# \code{columns} is equal to the interventions while the number of
# \code{rows} is equal to the number of PSA simulations to be summarised.
# @param .costs A matrix containing the \code{costs} from PSA. Number of
# \code{columns} is equal to the interventions while the number of
# \code{rows} is equal to the number of PSA simulations to be summarised.
# @param .params A matrix containing parameters' configurations used in
# the PSA.
# @param sets A vector of parameter-indexes to be used for EVPPI.
# @param .sets_names A vector of parameter-names to be used for EVPPI.
# @param lambda Maximum Acceptable Incremental Cost-Effectiveness Ratio (MAICER).
# @param .session Shiny app session.
#
# @return
#
# @examples
calSubsetEvpi <- function(.nb, .effs, .costs, .params, sets, .sets_names = NULL,
                          lambda, .session = NULL) {
  # grab parameter-indexes from the parameters dataset:
  if(is.null(sets)) {
   sets <- which(colnames(.params) %in% .sets_names)
  }

  numParams <- length(sets) # number of parameters in the set
  regressionFunction <- ifelse(numParams > 4, "gpFunc", "gamFunc") # change gp to ppr
  f <- formulaGenerator(sets)

  # calculate incremental net benefit (INB):
  inb <- if(is.null(.nb)) {
    createInb( # Strong et al. function
      costs.int = .costs,
      effects.int = .effs,
      lambda = lambda)
  } else {
    .nb
  }

  # estimate conditional EVPPI:
  output <- get(regressionFunction)(.params, inb, sets, s = 1000, .session)

  return(output)
}
