################################################################################
#
# Script Name:        compute_EVPPI_helpers.R
# Module Name:        Economic/PSA
# Script Description: Defines a set of EVPPI helper functions.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

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

# Calculating Net Benefits (NB)
#
# @param costs.int Costs data structure.
# @param effects.int Effects data structure.
# @param lambda Maximum Willingness-to-Pay.
#
# @return
# @export
#
# @examples
# \dontrun{
# inb <- createNb(costs, effects, lambda)
# }
createNb <- function(costs.int, effects.int, lambda) {

  nb <- as.matrix(effects.int) * lambda - as.matrix(costs.int)

  return(nb)
}

# Calculating Expected Value of Perfect Information (EVPI)
#
# @param costs.int Costs data matrix.
# @param effects.int Effects data matrix.
# @param lambda Maximum Willingness-to-Pay.
#
# @return
#
# @examples
# \dontrun{
# overallEvpi <- calcEvpi(costs, effects, lambda)
# }
calcEvpi <- function(costs.int, effects.int, lambda) {

  nb <- data.frame(as.matrix(effects.int) * lambda - as.matrix(costs.int))
  evpi <- mean(do.call(pmax, nb)) - max(colMeans(nb))

  return(evpi)
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

# Create EVPPI Bar Plot
#
# @param pEVPI.int EVPPI data.
# @param params Parameters data structure.
#
# @return
#
# @examples
# \dontrun{
# makeEvppiBar(.PSA_data$EVPPI[, 1], .PSA_data$p)
# }
makeEvppiBar <- function(EVPPI.int, params) {

  EVPPI <<- matrix(EVPPI.int[order(EVPPI.int)], ncol = length(EVPPI.int),
                  nrow = 1)
  colnames(EVPPI) <<- colnames(params[order(EVPPI.int)])

  op <- par(mar = c(5, 15, 4, 2) + 0.1, pty = "m")
  barplot(EVPPI, horiz = TRUE, cex.names=0.7, las=1,
          main= "Single parameter Partial EVPI per person",
          xlab = "Partial EVPI per person", cex.main=0.9)
  par(op)
}
