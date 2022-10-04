################################################################################
#
# Script Name:        ShinyPSA.R
# Module Name:        Economic/PSA
# Script Description: Defines an R6 class function that combines the
#                     functionalities of the ShinyPSA package.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

# ShinyPSA R6 class: ----
#' R6 Class representing a PSA summarising machine.
#'
#' @description
#' An instance of this class is expected to produce summary plots and
#' tables.
#' @format An [R6::R6Class] object.
#' @name ShinyPSA
NULL
#'
#' @rdname ShinyPSA
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' PSA_outputs <- ShinyPSA$new(
#'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
#'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
#'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
#'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats,
#'                   .evppi = TRUE,
#'                   .plot = TRUE)
#' }
#'
ShinyPSA <- R6::R6Class(
  # Object name:----
  classname = "ShinyPSA",
  # Public elements:----
  public = list(
    #' @field Summary_table a summary table with differentials, ICER(s), net
    #' benefits and probability being cost-effective.
    Summary_table = NULL,
    #' @field CEP_plot the Cost-Effectiveness plane.
    CEP_plot = NULL,
    #' @field CEAC_plot the Cost-Effectiveness Acceptability Curve.
    CEAC_plot = NULL,
    #' @field CEAF_plot the Cost-Effectiveness Acceptability Frontier.
    CEAF_plot = NULL,
    #' @field EVPI_plot the Expected Value of Perfect Information.
    EVPI_plot = NULL,
    #' @field eNMB_plot the expected Net Monitory Benefit.
    eNMB_plot = NULL,
    #' @field stability_plots PSA outputs' values' stability plots.
    stability_plots = NULL,
    #' @field  EVPPI_results the Expected Value of Partial Perfect Information
    #' (EVPPI) results.
    EVPPI_results = NULL,
    #' @field  EVPPI_plot the EVPPI results plot.
    EVPPI_plot = NULL,
    #' @field  EVPPI_Sub_results the subset EVPPI results table.
    EVPPI_Sub_results = NULL,
    #' @field app a list to store shiny app elements
    app = NULL,

    ## Initialize:----
    #' @description
    #' Initialisation method (triggered when a new object is created).
    #' Summary plots and table(s) are created alongside the construction
    #' of the plot.
    #'
    #' @param .effs A matrix containing the \code{effects} from PSA.
    #' Number of \code{columns} is equal to the interventions while the
    #' number of \code{rows} is equal to the number of PSA simulations to
    #' be summarised.
    #' @param .costs A matrix containing the \code{costs} from PSA.
    #' Number of \code{columns} is equal to the interventions while the
    #' number of \code{rows} is equal to the number of PSA simulations to
    #' be summarised.
    #' @param .params A matrix containing parameters' configurations used in
    #' the PSA. The Number of \code{rows} is equal to the number of PSA
    #' simulations to be summarised.
    #' @param .interventions A vector containing the names of all
    #' interventions. If not provided or less names than needed is
    #' provided, the function will generate generic names, for example
    #' \code{intervention 1}.
    #' @param .lambda Maximum acceptable ICER, default is 30,000.
    #' @param .ref An integer indicating the index of the reference
    #' intervention. This parameter is ignored if more than two
    #' \code{interventions} are under analysis.
    #' @param .Kmax The maximum willingness-to-pay threshold to use in the
    #' analysis. This parameter is ignored if \code{wtp} is provided.
    #' @param .wtp A vector of numerical values declaring the
    #' willingness-to-pay (WTP) values to use in the analysis. If
    #' \code{NULL} (default) a range of WTP values (up to \code{.Kmax}
    #' will be used.
    #' @param .evppi A boolean, FALSE (default), for whether to estimate
    #' Expected Value of Partial Perfect Information (EVPPI).
    #' @param .plot A boolean, FALSE (default), for whether to generate
    #' plots.
    #'
    #' @return A new `ShinyPSA` object.
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p)
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats,
    #'                   .evppi = TRUE)
    #' }
    initialize = function(.effs, .costs, .params, .interventions = NULL,
                          .lambda = 30000, .ref = NULL, .Kmax = 100000,
                          .wtp = NULL, .evppi = FALSE, .plot = TRUE) {
      private$effects <- dplyr::as_tibble(.effs)
      private$costs <- dplyr::as_tibble(.costs)
      private$params <- dplyr::as_tibble(.params)
      private$PSA_summary <- private$summarise_PSA_(
        .effs = .effs,
        .costs = .costs,
        .params = .params,
        .interventions = .interventions,
        .ref = .ref,
        .Kmax = .Kmax,
        .wtp = .wtp,
        .lambda = .lambda,
        .evppi = .evppi,
        .plot = .plot
      )

      invisible(self)

    },

    ## Get the results summary table:----
    #' @description
    #' Get the results summary table
    #'
    #' @param ... arguments passed to the drawing function. These include:
    #' .wtp_ A numeric vector containing the willingness-to-pay
    #' value(s) to be considered in the summary table. Default values are
    #' \code{c(20,000, 30,000)}
    #' .units_ A character, the units to associate with the
    #' monitory values in the summary table. Default is sterling pounds
    #' (GBP) \code{"\u00A3"}.
    #' .effects_label_ The label or name to be given to the effects
    #' column in the summary table. Default is QALYs.
    #' .beautify_ Return a visually improved version of the table. The
    #' returned version is built using DT::datatable()
    #' .long_ Logical (default \code{TRUE}) for whether a long version
    #' of the table is to be returned. If \code{FALSE}, a wide version of the
    #' table will be returned
    #'
    #' @return A table of class DT
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_Summary_table()
    #' }
    get_Summary_table = function(...) {
      # pass arguments to the plotting function:
      self$Summary_table <- private$draw_summary_table_(...)

      # return default plot if no arguments were passed to the function:
      dots_ <- list(...)
      if(length(dots_) == 0)
        return(private$PSA_summary[["Summary_table"]])

      # if any arguments exist, then return the new plot:
      return(self$Summary_table)
    },

    ## Get the Cost-Effectiveness plane:----
    #' @description
    #' Get the Cost-Effectiveness plane
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A ggplot2 object
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' # Get default plot:
    #' PSA_outputs$get_CEP()
    #'
    #' PSA_outputs$get_CEP(
    #'   .ref = 1,
    #'   .show_ICER = T,
    #'   .legend_pos = c(0.8, 0.2),
    #'   .show_wtp = T,
    #'   .zoom = T,
    #'   .wtp_threshold = c(20000, 500, 100, 50),
    #'   .nudge_labels = c(0.1, -0.1),
    #'   .zoom_cords = c(-0.001, 0.001, -5, 5)
    #'   )
    #' }
    get_CEP = function(...) {
      # pass arguments to the plotting function:
      self$CEP_plot <- private$plot_CEplane_(...)

      # return default plot if no arguments were passed to the function:
      dots_ <- list(...)
      if(length(dots_) == 0)
        return(private$PSA_summary[["CEP_plot"]])

      # if any arguments exist, then return the new plot:
      return(self$CEP_plot)
    },

    ## Get the Cost-Effectiveness Acceptability Curve:----
    #' @description
    #' Get the Cost-Effectiveness Acceptability Curve
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A ggplot2 object
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_CEAC()
    #' }
    get_CEAC = function(...) {
      # pass arguments to the plotting function:
      self$CEAC_plot <- private$plot_CEAC_(...)

      # return default plot if no arguments were passed to the function:
      dots_ <- list(...)
      if(length(dots_) == 0)
        return(private$PSA_summary[["CEAC_plot"]])

      # if any arguments exist, then return the new plot:
      return(self$CEAC_plot)
    },

    ## Get the Cost-Effectiveness Acceptability Frontier:----
    #' @description
    #' Get the Cost-Effectiveness Acceptability Frontier
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A ggplot2 object
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_CEAF()
    #' }
    get_CEAF = function(...) {
      # pass arguments to the plotting function:
      self$CEAF_plot <- private$plot_CEAF_(...)

      # return default plot if no arguments were passed to the function:
      dots_ <- list(...)
      if(length(dots_) == 0)
        return(private$PSA_summary[["CEAF_plot"]])

      # if any arguments exist, then return the new plot:
      return(self$CEAF_plot)
    },

    ## Get the expected Net Monitory Benefit:----
    #' @description
    #' Get the expected Net Monitory Benefit
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A ggplot2 object
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_eNMB()
    #' }
    get_eNMB = function(...) {
      # pass arguments to the plotting function:
      self$eNMB_plot <- private$plot_eNMB_(...)

      # return default plot if no arguments were passed to the function:
      dots_ <- list(...)
      if(length(dots_) == 0)
        return(private$PSA_summary[["eNMB_plot"]])

      # if any arguments exist, then return the new plot:
      return(self$eNMB_plot)
    },

    ## Get the Expected Value of Perfect Information:----
    #' @description
    #' Get the Expected Value of Perfect Information
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A ggplot2 object
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_EVPI()
    #' }
    get_EVPI = function(...) {
      # pass arguments to the plotting function:
      self$EVPI_plot <- private$plot_EVPI_(...)

      # return default plot if no arguments were passed to the function:
      dots_ <- list(...)
      if(length(dots_) == 0)
        return(private$PSA_summary[["EVPI_plot"]])

      # if any arguments exist, then return the new plot:
      return(self$EVPI_plot)
    },

    ## Get the EVPPI results table:----
    #' @description
    #' Get the EVPPI results table
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A list containing EVPPI results
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_EVPPI_results()
    #' }
    get_EVPPI_results = function(...) {
      dots_ <- list(...)
      self$EVPPI_results <- if(length(dots_) == 0) {
        if(is.null(private$PSA_summary[["EVPPI"]])) {
          # create object is missing:
          dots_$.PSA_data <- private$PSA_summary
          private$PSA_summary[["EVPPI"]] <- do.call(
            private$compute_EVPPIs_, dots_)
        }
        # return default object if no arguments were passed to the function:
        private$PSA_summary[["EVPPI"]]
      } else {
        # pass arguments to the plotting function:
        dots_$.PSA_data <- private$PSA_summary
        do.call(private$compute_EVPPIs_, dots_)
      }

      return(self$EVPPI_results)
    },

    ## Get the Subset EVPPI results table:----
    #' @description
    #' Get the Subset EVPPI results table
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A list containing Subset EVPPI results
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_Sub_EVPPI_results()
    #' }
    get_Sub_EVPPI_results = function(...) {
      dots_ <- list(...)
      # sanity checks:
      if(is.null(dots_$.set_names) & is.null(dots_$.set)) {
        print(
          paste("Unable to estimate EVPPI for a subset of parameters.",
                "Please pass parameters indexes via the argument '.set'",
                "or parameters names via the argument '.set_names'."))
      }

      # hard-code key arguments:
      dots_$.PSA_data <- private$PSA_summary
      dots_$.subset_ <- TRUE

      # compute EVPPI for the parameters subsets:
      self$EVPPI_Sub_results <- if(is.null(self$EVPPI_Sub_results)) {
        do.call(private$compute_EVPPIs_, dots_)
      } else {
        self$EVPPI_Sub_results %>%
          dplyr::bind_rows(
            do.call(private$compute_EVPPIs_, dots_))
      }

      return(self$EVPPI_Sub_results)
    },

    ## Get the EVPPI plot:----
    #' @description
    #' Get the EVPPI plot
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A ggplot2 object
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_EVPPI_plot()
    #' }
    get_EVPPI_plot = function(...) {
      dots_ <- list(...)
      self$EVPPI_plot <- if(length(dots_) == 0 &
                            !is.null(private$PSA_summary[["EVPPI_plot"]])) {
        # return default object if no arguments were passed to the function:
        private$PSA_summary[["EVPPI_plot"]]
      } else {
        if(is.null(private$PSA_summary[["EVPPI"]])) {
          # create EVPPIs results object if it does not exist:
          private$PSA_summary[["EVPPI"]] <- self$get_EVPPI_results()
        }
        # pass arguments to the plotting function:
        dots_$EVPPI_res <- private$PSA_summary[["EVPPI"]]
        do.call(private$plot_EVPPI_, dots_)
      }

      return(self$EVPPI_plot)
    },

    ## Get the PSA outputs stability plots:----
    #' @description
    #' Get the PSA outputs stability plots
    #'
    #' @param ... Extra arguments passed to the plotting functions
    #'
    #' @return A ggplot2 object
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_PSA_stabl_plots()
    #' }
    get_PSA_stabl_plots = function(...) {
      # pass arguments to the plotting function:
      self$stability_plots <- private$check_PSA_stability(...)

      # return default plot if no arguments were passed to the function:
      dots_ <- list(...)
      if(length(dots_) == 0)
        return(private$PSA_summary[["Stability_plots"]])

      # if any arguments exist, then return the new plot:
      return(self$stability_plots)
    },

    ## Get the willingness-to-pay values used in the analysis:----
    #' @description
    #' Get the willingness-to-pay values used in the analysis
    #'
    #' @return A numerical vector.
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Vaccine_PSA$p)
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_WTP()
    #' }
    get_WTP = function() {

      return(private$PSA_summary[["WTPs"]])
    }

  ),

  # Private elements:----
  private = list(

    effects = NULL,
    costs = NULL,
    params = NULL,
    PSA_summary = NULL,

    ## Summarise PSA outputs and report results:----
    #
    # @param .effs A matrix containing the \code{effects} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be summarised.
    # @param .costs A matrix containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be summarised.
    # @param .params A matrix containing parameters' configurations used in
    # the PSA. The Number of \code{rows} is equal to the number of PSA simulations
    # to be summarised.
    # @param .interventions A vector containing the names of all
    # interventions. If not provided or less names than needed is provided,
    # the function will generate generic names, for example
    # \code{intervention 1}.
    # @param .ref An integer indicating the index of the reference
    # intervention. This parameter is ignored if more than two
    # \code{interventions} are under analysis.
    # @param .Kmax The maximum willingness-to-pay threshold to use in the
    # analysis. This parameter is ignored if \code{wtp} is provided.
    # @param .wtp A vector of numerical values declaring the
    # willingness-to-pay (WTP) values to use in the analysis. If \code{NULL}
    # (default) a range of WTP values (up to \code{.Kmax} will be used.
    # @param .max_Kpoints Maximum number of willingness-to-pay values (default
    # 100) to use in the analysis.
    # @param .lambda Maximum acceptable ICER, default is 30,000.
    # @param .evppi A boolean, FALSE (default), for whether to estimate Expected
    # Value of Partial Perfect Information (EVPPI).
    # @param .plot A boolean, FALSE (default), for whether to generate plots.
    #
    # @return A list of class \code{psa} containing several objects.
    #
    # @examples
    # \dontrun{
    # library(ShinyPSA)
    #
    # PSA_summary = summarise_PSA_(
    #   .effs = ShinyPSA::Brennan_1K_PSA$e,
    #   .costs = ShinyPSA::Brennan_1K_PSA$c,
    #   .params = ShinyPSA::Brennan_1K_PSA$p,
    #   .interventions = ShinyPSA::Brennan_1K_PSA$treats,
    #   .evppi = TRUE,
    #   .plot = TRUE)
    # }
    summarise_PSA_ = function(.effs, .costs, .params, .interventions = NULL,
                              .ref = NULL, .Kmax = 100000, .wtp = NULL,
                              .max_Kpoints = 100, .lambda = 30000,
                              .evppi = FALSE, .plot = FALSE) {

      # Stop if .effs & .costs have different dimensions:
      stopifnot('Unequal dimensions in .effs and .costs' =
                  dim(.effs) == dim(.costs),
                'PSA results for less than two interventions is supplied' =
                  ncol(.effs) >= 2)

      # Simulations & interventions analysed:
      n.sim <- nrow(.effs) # Number of simulations
      n.comparators <- ncol(.effs) # Number of interventions
      n.comparisons <- n.comparators - 1 # Number of least possible comparisons
      v.ints <- 1:n.comparators # Vector with index of interventions'

      # Check supplied interventions labels, create ones if any is missing:
      if(!is.null(.interventions) & length(.interventions) != n.comparators) {
        .interventions <- NULL
      }
      if(is.null(.interventions)) {
        .interventions <- paste("intervention", 1:n.comparators)
      }

      # Set missing values or remove ones to be ignored:
      if(n.comparators == 2){
        # If no reference was provided in a non-incremental analysis:
        if(is.null(.ref)){
          .ref <- 1
          message(
            paste0(
              "You did not select a reference intervention. [",
              .interventions[.ref],
              "] will be used as reference for differential values and plots."))
        }
        comp <- v.ints[-.ref]
      } else {
        # Ignore .ref if the analysis will be an incremental one:
        if(!is.null(.ref)) {
          .ref <- NULL
          message("More than two interventions, .ref is ignored")
        }
        comp <- NULL
      }

      # Set up willingness-to-pay:
      if (is.null(.Kmax)) {
        .Kmax <- 100000
      }
      if (is.null(.wtp)) {
        .wtp <- c(20000, 30000, 50000)
      }
      n.points <- .Kmax/.max_Kpoints
      v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
      v.k <- c(v.k, .wtp)
      v.k <- sort(unique(v.k))
      n.k <- length(v.k)
      names(v.k) <- scales::dollar(
        x = v.k,
        prefix = "\u00A3"
      )

      # Ensure .effs and .costs are tibbles and name columns appropriately:
      .effs <- .effs %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        `colnames<-`(.interventions)
      .costs <- .costs %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        `colnames<-`(.interventions)

      # Compute effects and costs differentials:
      if(n.comparators == 2) {
        delta.effs <- private$calculate_differentials_(
          .data = .effs,
          .ref = .ref)
        delta.costs <- private$calculate_differentials_(
          .data = .costs,
          .ref = .ref)
      } else {
        delta.effs <- NULL
        delta.costs <- NULL
      }

      # Compute ICER(s):
      ICER <- private$compute_ICERs_(
        .icer_data = NULL,
        .effs = .effs,
        .costs = .costs,
        .interventions = .interventions)

      # Compute NMB or iNMB, e.NMB or e.iNMB and best option for each k:
      nmbs <- private$compute_NMBs_(
        .effs = .effs,
        .costs = .costs,
        .interventions = .interventions,
        .Kmax = .Kmax,
        .wtp = .wtp)
      NMB <- nmbs$nmb
      e.NMB <- nmbs$e.nmb
      best <- nmbs$best_interv
      best_name <- nmbs$best_interv_name
      check <- nmbs$check
      kstar <- nmbs$wtp_star

      # Compute CEAC:
      CEAC <- private$compute_CEACs_(
        .nmb = NMB)

      # Compute CEAF:
      CEAF <- private$compute_CEAFs_(
        .ceac = CEAC)

      # Compute EVPI:
      EVPIs <- private$compute_EVPIs_(
        .effs = .effs,
        .costs = .costs,
        .Kmax = .Kmax,
        .interventions = .interventions,
        .wtp = .wtp)
      U <- EVPIs$U
      Ustar <- EVPIs$Ustar
      ol <- EVPIs$ol
      vi <- EVPIs$vi
      EVPI <- EVPIs$evi

      ## Outputs of the function:
      results <- list(

        interventions = .interventions, ref = .ref, comp = comp, ICER = ICER,
        NMB = NMB, e.NMB = e.NMB, CEAC = CEAC, CEAF = CEAF, EVPI = EVPI,
        best_id = best, best_name = best_name, WTPs = v.k,
        WTPstar = kstar, U = U, Ustar = Ustar, vi = vi, ol = ol, e = .effs,
        c = .costs, p = .params, delta.e = delta.effs, delta.c = delta.costs,
        n.sim = n.sim, n.comparators = n.comparators, step = n.k, Kmax = .Kmax
      )

      class(results) <- "psa"

      # If requested, compute EVPPI:
      if(.evppi) {
        # Compute EVPPI:
        EVPPI <- private$compute_EVPPIs_(
          .PSA_data = results,
          .MAICER_ = .lambda)

        # Save EVPPI results table to the final results object:
        results <- c(results,
                     'EVPPI' = list(EVPPI))
      }

      # If requested, develop and save plots and table:
      if(.plot == TRUE) {
        Summary_table <- private$draw_summary_table_(
          .PSA_data = results
        )
        CEP_plot <- private$plot_CEplane_(
          .PSA_data = results,
          .ref = .ref
        )
        CEAC_plot <- private$plot_CEAC_(
          .PSA_data = results,
          .ref = .ref
        )
        CEAF_plot <- private$plot_CEAF_(
          .PSA_data = results
        )
        EVPI_plot <- private$plot_EVPI_(
          .PSA_data = results
        )
        eNMB_plot <- private$plot_eNMB_(
          .PSA_data = results
        )
        EVPPI_plot <- if(.evppi) {
          private$plot_EVPPI_(
            EVPPI_res = EVPPI
          )
        }
        stability_plots <- private$check_PSA_stability(
          .PSA_data = results
        )

        results <- c(results,
                     'Summary_table' = list(Summary_table),
                     'CEP_plot' = list(CEP_plot),
                     'CEAC_plot' = list(CEAC_plot),
                     'CEAF_plot' = list(CEAF_plot),
                     'EVPI_plot' = list(EVPI_plot),
                     'eNMB_plot' = list(eNMB_plot),
                     'EVPPI_plot' = list(EVPPI_plot),
                     'Stability_plots' = list(stability_plots))
      }

      return(results)
    },

    ## Compute ICER(s):----
    #
    # .icer_data A table containing average costs and QALYs data
    # .effs A tibble containing the \code{effects} from PSA.
    # Number of \code{columns} is equal to the interventions while the
    # number of \code{rows} is equal to the number of PSA simulations
    # to be summarised.
    # .costs A tibble containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .interventions A vector containing the names of all
    # interventions. If not provided or less names than needed is
    # provided, the function will generate generic names, for example
    # \code{intervention 1}.
    #
    # A dataframe with data from icer_data in addition to
    # \code{qalys and costs diffrential(s)}, \code{dominance} &
    # \code{icer(s)}
    #
    # \dontrun{}
    compute_ICERs_ = function(.icer_data, .effs = NULL, .costs = NULL,
                              .interventions = NULL) {
      # If a summary table of costs, effects and intervention names supplied:
      if(!is.null(.icer_data)) {
        # Check if missing key columns and create them if so:
        icer_tmp <- .icer_data %>%
          private$add_missing_columns_(
            .x = .,
            .characters = c("dominance", "icer_label"),
            .numerics = c(".id", "delta.e", "delta.c", "icer"))
      } else if(!is.null(.effs) & !is.null(.costs)) {

        # Stop if .effs & .costs are not of class tibble or have unequal dims:
        stopifnot('.effs is not a tibble' = "data.frame" %in% class(.effs),
                  '.costs is not a tibble' = "data.frame" %in% class(.costs),
                  '.effs and .costs have unequal dimensions' =
                    dim(.effs) == dim(.costs))

        # Get number of interventions in supplied matrix:
        n.comparators <- ncol(.effs) # Number of interventions

        # Check supplied interventions labels, create ones if any is missing:
        if(!is.null(.interventions) & length(.interventions) != n.comparators) {
          .interventions <- NULL
        }
        if(is.null(.interventions)) {
          .interventions <- paste("intervention", 1:n.comparators)
        }

        # Define ICER table:
        icer_tmp <- dplyr::tibble(
          'intervention' = .interventions,
          'qalys' = colMeans(.effs),
          'costs' = colMeans(.costs)) %>%
          private$add_missing_columns_(
            .x = .,
            .characters = c("dominance", "icer_label"),
            .numerics = c(".id", "delta.e", "delta.c", "icer"))
      } else {
        stop("Please supply costs and effects from PSA, each in a separate
         tibble/dataframe, or a summary table with interventions' names,
         and corresponding mean costs and mean qalys")
      }

      # Identify dominated interventions:
      icer_tmp <- icer_tmp %>%
        private$dominance_wraper_()

      # Compute ICER(s), before extended dominance checking:
      icer_tmp <- icer_tmp %>%
        private$calculate_ICERs_()

      # Identify any extendedly dominated interventions, and recompute ICER(s):
      icer_tmp <- icer_tmp %>%
        private$e.dominance_wraper_()

      # Drop .id:
      icer_tmp <- icer_tmp %>%
        dplyr::select(-.id)

      return(icer_tmp)
    },

    ### Identify dominated interventions:----
    #
    # .icer_data A table containing average costs and QALYs data
    # .qalys Character indicating the name of the column containing
    # Quality Adjusted Life Years (QALYs) in \code{.icer_data}
    # .costs Character indicating the name of the column
    # containing cost data in \code{.icer_data}
    #
    # A table containing \code{.icer_data} in addition to identified
    # dominance
    #
    # \dontrun{}
    identify_dominance_ = function(.icer_data, .qalys = qalys, .costs = costs) {
      # Check if missing key columns and create them if so:
      .icer_data <- .icer_data %>%
        private$add_missing_columns_(
          .x = .,
          .characters = c("dominance", "icer_label"),
          .numerics = c(".id", "delta.e", "delta.c", "icer"))

      # Identify dominated interventions:
      .icer_data <- .icer_data %>%
        dplyr::arrange({{.qalys}}) %>%
        dplyr::group_by(dominance) %>%
        dplyr::mutate(
          icer_label = dplyr::case_when(
            is.na(dominance) ~ dplyr::case_when(
              dplyr::lead({{.costs}}) < {{.costs}} ~ "SD"),
            TRUE ~ icer_label),
          dominance = dplyr::case_when(
            is.na(dominance) ~ dplyr::case_when(
              dplyr::lead({{.costs}}) < {{.costs}} ~ "SD"),
            TRUE ~ dominance)) %>%
        dplyr::ungroup()

      return(.icer_data)
    },

    ### Identify extendedly dominated interventions:----
    #
    # .icer_data A table containing average costs and QALYs data
    # .qalys Character indicating the name of the column containing
    # Quality Adjusted Life Years (QALYs) in \code{.icer_data}
    #
    # A vector stating whether any of the included interventions were
    # e.dominated
    #
    # \dontrun{}
    identify_e.dominance_ = function(.icer_data, .qalys = qalys) {
      # Check if missing key columns and create them if so:
      .icer_data <- .icer_data %>%
        private$add_missing_columns_(
          .x = .,
          .characters = c("dominance", "icer_label"),
          .numerics = c(".id", "delta.e", "delta.c", "icer"))

      # Identify extendedly dominated interventions:
      .icer_data <- .icer_data %>%
        dplyr::arrange({{.qalys}}) %>%
        dplyr::group_by(dominance) %>%
        dplyr::mutate(
          icer_label = dplyr::case_when(
            is.na(dominance) ~ dplyr::case_when(
              dplyr::lead(icer) < icer ~ "ED"),
            TRUE ~ icer_label),
          dominance = dplyr::case_when(
            is.na(dominance) ~ dplyr::case_when(
              dplyr::lead(icer) < icer ~ "ED"),
            TRUE ~ dominance)) %>%
        dplyr::ungroup()

      return(.icer_data)
    },

    ### Identify, iteratively, all dominated interventions:----
    #
    # .x A table containing average costs and QALYs data
    #
    # A dataframe with data from .x in addition to dominance
    # information, if any
    #
    # \dontrun{}
    dominance_wraper_ = function(.x) {
      # Check if missing key columns and create them if so:
      .x <- .x %>%
        private$add_missing_columns_(
          .x = .,
          .characters = c("dominance", "icer_label"),
          .numerics = c(".id", "delta.e", "delta.c", "icer"))

      # Check for unidentified dominance
      while (any("SD" %in%
                 (.x %>%
                  dplyr::filter(dplyr::if_any(dominance, ~ is.na(.))) %>%
                  private$identify_dominance_() %>%
                  dplyr::pull(dominance)))) {
        # Do until all dominated are identified
        .x <- .x %>%
          private$identify_dominance_()
      }

      return(.x)
    },

    ### Identify, iteratively, all extendedly dominated interventions:----
    #
    # .x A table containing average costs and QALYs data
    #
    # A dataframe with data from \code{.x} in addition to extended
    # dominance information, if any
    #
    # \dontrun{}
    e.dominance_wraper_ = function(.x) {
      # Check if missing key columns and create them if so:
      .x <- .x %>%
        private$add_missing_columns_(
          .x = .,
          .characters = c("dominance", "icer_label"),
          .numerics = c(".id", "delta.e", "delta.c", "icer"))

      # Check for any remaining e.dominance
      while (any("ED" %in%
                 (.x %>%
                  dplyr::filter(dplyr::if_any(dominance, ~ is.na(.))) %>%
                  private$identify_e.dominance_() %>%
                  dplyr::pull(dominance)))) {
        # Do until all extendedly dominated are identified:
        .x <- .x %>%
          private$identify_e.dominance_() %>%
          private$calculate_ICERs_() # ICER(s) for un-dominated/e.dominated
      }

      return(.x)
    },

    ### Calculate ICER(s) and effects and costs differentials:----
    #
    # .icer_data A table containing average costs and QALYs data
    # .qalys Character indicating the name of the column containing
    # Quality Adjusted Life Years (QALYs) data in .icer_data
    # .costs Character indicating the name of the column containing
    # cost data in .icer_data
    #
    # A table of \code{effects diffrentials}, \code{costs
    # differentials} & \code{icers}
    #
    # \dontrun{}
    calculate_ICERs_ = function(.icer_data, .qalys = qalys, .costs = costs) {
      # Check if missing key columns and create them if so:
      .icer_data <- .icer_data %>%
        private$add_missing_columns_(
          .x = .,
          .characters = c("dominance", "icer_label"),
          .numerics = c(".id", "delta.e", "delta.c", "icer"))

      # Compute Incremental Cost-Effectiveness Ratio (ICER):
      .icer_data <- .icer_data %>%
        dplyr::arrange({{.qalys}}) %>%
        dplyr::group_by(dominance) %>%
        dplyr::mutate(
          delta.e = dplyr::case_when(
            is.na(dominance) ~ c(NA, diff({{.qalys}}))),
          delta.c = dplyr::case_when(
            is.na(dominance) ~ c(NA, diff({{.costs}}))),
          icer = dplyr::case_when(
            is.na(dominance) ~ delta.c / delta.e),
          icer_label = dplyr::case_when(
            is.na(dominance) & !is.na(icer) ~ paste0(
              "ICER = ", scales::dollar(
                x = icer,
                prefix = "\u00A3"
              )
            ),
            is.na(dominance) & is.na(icer) ~ dplyr::case_when(
              dplyr::n() > 1 ~ paste0("reference"),
              TRUE ~ icer_label),
            TRUE ~ icer_label)) %>%
        dplyr::ungroup()

      return(.icer_data)
    },

    ## Compute Monetary Net-Benefit (NMB) or incremental NMB (iNMB):----
    #
    # .effs A tibble containing the \code{effects} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .costs A tibble containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .interventions A vector containing the names of all
    # interventions. If not provided or less names than needed is
    # provided, the function will generate generic names, for example
    # \code{intervention 1}.
    # .Kmax The maximum willingness-to-pay threshold to use in the
    # analysis. This parameter is ignored if \code{wtp} is provided.
    # .wtp A vector of numerical values declaring the
    # willingness-to-pay (WTP) values to use in the analysis.
    # If \code{NULL} (default) a range of WTP values (up to \code{.Kmax}
    # will be used.
    # .max_Kpoints Maximum number of willingness-to-pay values (default
    # 100) to use in the analysis.
    #
    # A list containing the NMB list, eNMB tibble, WTP tibble and
    # other objects.
    #
    # \dontrun{}
    compute_NMBs_ = function(.effs, .costs, .interventions = NULL,
                              .Kmax = NULL, .wtp = NULL,
                             .max_Kpoints = 100) {
      # Stop if .effs & .costs are not of class tibble or have unequal dims:
      stopifnot('.effs is a not tibble' = "data.frame" %in% class(.effs),
                '.costs is a not tibble' = "data.frame" %in% class(.costs),
                '.effs and .costs have unequal dimensions' =
                  dim(.effs) == dim(.costs))

      # Simulations & interventions analysed:
      n.comparators <- ncol(.effs) # Number of interventions
      n.comparisons <- n.comparators - 1 # Number of least possible comparisons
      v.ints <- 1:n.comparators # Vector with index of interventions'

      # Check supplied interventions labels, create ones if any is missing:
      if(!is.null(.interventions) & length(.interventions) != n.comparators) {
        .interventions <- NULL
      }
      if(is.null(.interventions)) {
        .interventions <- paste("intervention", 1:n.comparators)
      }

      # Name .effs and .costs columns appropriately:
      .effs <- .effs %>%
        `colnames<-`(.interventions)
      .costs <- .costs %>%
        `colnames<-`(.interventions)

      # Set up willingness-to-pay:
      if (is.null(.Kmax)) {
        .Kmax <- 100000
      }
      if (is.null(.wtp)) {
        .wtp <- c(20000, 30000, 50000)
      }
      n.points <- .Kmax/.max_Kpoints
      v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
      v.k <- c(v.k, .wtp)
      v.k <- sort(unique(v.k))
      n.k <- length(v.k)
      names(v.k) <- scales::dollar(
        x = v.k,
        prefix = "\u00A3"
      )

      # Compute monetary net benefit (NMB) (default):
      nmb <- purrr::map2(
        .x = .effs,
        .y = .costs,
        .f = function(.eff = .x, .cost = .y) {
          purrr::map_dfc(
            .x = as.list(v.k),
            .f = function(.k = .x) {
              .eff * .k - .cost})}) %>%
        purrr::transpose()

      # Compute expected net benefit (e.NMB):
      e.nmb <- nmb %>%
        purrr::map_dfr(
          .f = function(.x) {
            colMeans(dplyr::as_tibble(.x, .name_repair = "unique"))
          })

      # Select the best option for each willingness-to-pay value:
      best_interv <- e.nmb %>%
        max.col(ties.method = "first")
      best_interv_name <- .interventions[best_interv]

      # Finds the wtp value for which the optimal decision changes
      check <- c(0, diff(best_interv))
      wtp_star <- v.k[check != 0]

      return(list(nmb = nmb, e.nmb = e.nmb, check = check,
                  wtp_star = wtp_star, wtp = v.k,
                  best_interv = best_interv,
                  best_interv_name = best_interv_name))
    },

    ## Compute Cost-Effectiveness Acceptability Curve (CEAC):----
    #
    # .nmb A list (with similar features to a 3D-array) containing the
    # Net Monetary Benefits from each probabilistic sensitivity analysis
    # (PSA)
    # run for each intervention across a range of willingness-to-pay (WTP)
    # values. The dimensions of this list are:
    # \code{List:WTP, Tibble(Rows: PSA simulations, Cols: Interventions)}.
    # .effs A tibble containing the \code{effects} from PSA. Number of
    #  \code{columns} is equal to the interventions while the number of
    #  \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .costs A tibble containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .interventions A vector containing the names of all interventions.
    # If not provided or less names than needed is provided,
    # the function will generate generic names, for example
    # \code{intervention 1}.
    # .Kmax The maximum willingness-to-pay threshold to use in the
    # analysis. This parameter is ignored if \code{wtp} is provided.
    # .wtp A vector of numerical values declaring the
    # willingness-to-pay (WTP) values to use in the analysis. If
    # \code{NULL} (default) a range of WTP values (up to \code{.Kmax} will
    # be used.
    #
    # A tibble containing the probability of being cost-effective
    # for all interventions.
    #
    # \dontrun{}
    compute_CEACs_ = function(.nmb, .effs = NULL, .costs = NULL,
                              .interventions = NULL, .Kmax = NULL,
                              .wtp = NULL) {
      # If .nmb was not available but raw data were:
      if(is.null(.nmb) & !is.null(.effs) & !is.null(.costs)){
        .nmb <- private$compute_NMBs_(.effs = .effs,
                                      .costs = .costs,
                                      .interventions = .interventions,
                                      .Kmax = .Kmax,
                                      .wtp = .wtp)
        .nmb <- .nmb$nmb
      }

      # Stop if object .nmb is not of class list:
      stopifnot('.nmb is not a list' = "list" %in% class(.nmb))

      # CEAC in incremental analysis:
      ceac <- .nmb %>%
        purrr::map_dfr(
          .f = function(.x) {
            colMeans(
              do.call(
                pmax, dplyr::as_tibble(.x, .name_repair = "unique")) ==
                dplyr::as_tibble(.x, .name_repair = "unique"))})

      return(ceac)
    },

    ## Compute Cost-Effectiveness Acceptability Frontier:----
    #
    # .ceac A tibble containing the probability of being cost-effective
    # for all interventions.
    # .nmb A list (with similar features to a 3D-array) containing the
    # Net Monetary Benefits from each probabilistic sensitivity analysis
    # (PSA) run for each intervention across a range of willingness-to-pay
    # (WTP) values. The dimensions of this list are:
    # \code{List:WTP, Tibble(Rows: PSA simulations, Cols: Interventions)}.
    #
    # A tibble containing the probability of being cost-effective
    # for all interventions alongside the CEAF.
    #
    # \dontrun{}
    compute_CEAFs_ = function(.ceac, .nmb = NULL) {
      # Stop if object .ceac is not of class tibble:
      stopifnot('.ceac is a not tibble' = "data.frame" %in% class(.ceac))
      if(!is.null(.nmb))
        stopifnot('.nmb is not a list' = "list" %in% class(.nmb))

      # If .ceac was not available but .nmb was:
      if(is.null(.ceac) & !is.null(.nmb))
        .ceac <- private$compute_CEACs_(.nmb = .nmb)

      # Compute CEAF:
      ceaf <- .ceac %>%
        dplyr::mutate('ceaf' = do.call(pmax, .))

      return(ceaf)
    },

    ## Compute the Expected Value of Perfect Information (EVPI):----
    #
    # .effs A tibble containing the \code{effects} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .costs A tibble containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .interventions A vector containing the names of all interventions.
    # If not provided or less names than needed is provided,
    # the function will generate generic names, for example
    # \code{intervention 1}.
    # .Kmax The maximum willingness-to-pay threshold to use in the
    # analysis. This parameter is ignored if \code{wtp} is provided.
    # .wtp A vector of numerical values declaring the
    # willingness-to-pay (WTP) values to use in the analysis. If
    # \code{NULL} (default) a range of WTP values (up to \code{.Kmax} will
    # be used.
    # .max_Kpoints Maximum number of willingness-to-pay values (default
    # 100) to use in the analysis.
    #
    # A list containing the EVPI vector, value of information tibble,
    # opportunity lost tibble among others
    #
    # \dontrun{}
    compute_EVPIs_ = function(.effs, .costs, .interventions = NULL,
                              .Kmax = NULL, .wtp = NULL,
                              .max_Kpoints = 100) {
      # Stop if .effs & .costs are not of class tibble or have unequal dims:
      stopifnot('.effs is not a tibble' = "data.frame" %in% class(.effs),
                '.costs is not a tibble' = "data.frame" %in% class(.costs),
                '.effs and .costs have unequal dimensions' =
                  dim(.effs) == dim(.costs))

      # Simulations & interventions analysed:
      n.comparators <- ncol(.effs) # Number of interventions

      # Check supplied interventions labels, create ones if any is missing:
      if(!is.null(.interventions) & length(.interventions) != n.comparators) {
        .interventions <- NULL
      }
      if(is.null(.interventions)) {
        .interventions <- paste("intervention", 1:n.comparators)
      }

      # Name .effs and .costs columns appropriately:
      .effs <- .effs %>%
        `colnames<-`(.interventions)
      .costs <- .costs %>%
        `colnames<-`(.interventions)

      # Set up willingness-to-pay:
      if (is.null(.Kmax)) {
        .Kmax <- 100000
      }
      if (is.null(.wtp)) {
        .wtp <- c(20000, 30000, 50000)
      }
      n.points <- .Kmax/.max_Kpoints
      v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
      v.k <- c(v.k, .wtp)
      v.k <- sort(unique(v.k))
      n.k <- length(v.k)
      names(v.k) <- scales::dollar(
        x = v.k,
        prefix = "\u00A3"
      )

      # Compute monetary net benefit (NMB) (default):
      nmb <- purrr::map2(
        .x = .effs,
        .y = .costs,
        .f = function(.eff = .x, .cost = .y) {
          purrr::map_dfc(
            .x = as.list(v.k),
            .f = function(.k = .x) {
              .eff * .k - .cost})}) %>%
        purrr::transpose()

      # Compute expected net benefit (e.NMB):
      e.nmb <- nmb %>%
        purrr::map_dfr(
          .f = function(.x) {
            colMeans(dplyr::as_tibble(.x, .name_repair = "unique"))
          })

      # Identify the best option for each willingness-to-pay value:
      best_interv <- e.nmb %>%
        max.col(ties.method = "first")

      # Extract maximum nmb value at each iteration for each wtp/threshold:
      max_nmb_iter <- nmb %>%
        purrr::map_dfr(
          .f = function(.x) {
            do.call(pmax, dplyr::as_tibble(.x, .name_repair = "unique"))
          })

      # Compute opportunity loss (OL):
      ol <- purrr::pmap_dfc(
        .l = list(nmb, best_interv, max_nmb_iter),
        .f = function(.x, .y, .z) {
          .z - .x[[.y]]
        })

      # Compute value-of-information (VI):
      vi <- purrr::map2_dfc(
        .x = max_nmb_iter, .y = nmb,
        .f = function(.x, .y) {
          .x - max(colMeans(dplyr::as_tibble(.y, .name_repair = "unique")))
        })

      # Compute expected value-of-information (EVPI):
      evi <- colMeans(ol)

      return(list(U = nmb, Ustar = max_nmb_iter, vi = vi, ol = ol, evi = evi))
    },

    ## Draw results summary table:----
    #
    # @param .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # @param .wtp_ A numeric vector containing the willingness-to-pay
    # value(s) to be considered in the summary table. Default values are
    # \code{c(20,000, 30,000)}
    # @param .units_ A character, the units to associate with the
    # monitory values in the summary table. Default is sterling pounds
    # (GBP) \code{"\u00A3"}.
    # @param .effects_label_ The label or name to be given to the effects
    # column in the summary table. Default is QALYs.
    # @param .beautify_ Return a visually improved version of the table. The
    # returned version is built using DT::datatable()
    # @param .long_ Logical (default \code{TRUE}) for whether a long version
    # of the table is to be returned. If \code{FALSE}, a wide version of the
    # table will be returned
    # @param .evpi_population_ The size of the population that is annually
    # affected by the interventions under study
    # @param .discount_rate_ The discount rate used to discount future
    # affected populations.
    # @param .time_horion_ The time expected to pass (in years) before the
    # interventions under consideration change (how long before the decision
    # under consideration become obsolete or requires updating)
    #
    # @return A table, dataframe, tibble or DT objects.
    # @importFrom tidyselect vars_select_helpers
    # @export
    #
    # @examples
    # \dontrun{
    # library(ShinyPSA)
    #
    # PSA_summary <- summarise_PSA_(
    #   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #   .interventions = ShinyPSA::Smoking_PSA$treats)
    #
    # t <- draw_summary_table_(.PSA_data = PSA_summary,
    #                         .wtp_ = c(100, 20000, 30000),
    #                         .beautify_ = TRUE,
    #                         .long_ = TRUE)
    #                         t
    #
    # t <- draw_summary_table_(.PSA_data = PSA_summary,
    #                         .wtp_ = c(100, 20000, 30000),
    #                         .beautify_ = TRUE,
    #                         .long_ = F)
    #
    # t
    # }
    draw_summary_table_ = function(.PSA_data = private$PSA_summary,
                                   .wtp_ = c(20000, 30000),
                                   .units_ = "\u00A3",
                                   .effects_label_ = "QALYs",
                                   .beautify_ = TRUE,
                                   .long_ = TRUE,
                                   .individual_evpi_ = TRUE,
                                   .evpi_population_ = NULL,
                                   .discount_rate_ = 0.035,
                                   .time_horion_ = NULL) {
      ## Set currency label if none were provided:----
      if(is.null(.units_) | length(.units_) != 1) .units_ = "\u00A3"

      ## Sort out .wtp values:----
      ### Remove wtp values greater than max WTP set by user:
      if(is.null(.wtp_))
        .wtp_ <- c(20000, 30000)
      if(!is.null(.wtp_))
        if(length(.wtp_) < 1)
          .wtp_ <- NULL
      if(!is.null(.wtp_))
        .wtp_ <- .wtp_[!is.na(.wtp_)]
      if(any(.wtp_ > max(.PSA_data$WTPs)))
        .wtp_ <- c(.wtp_[.wtp_ < max(.PSA_data$WTPs)],
                   max(.PSA_data$WTPs))
      ### replace unevaluated wtp with nearest replacements:
      wtp_index_ <- purrr::map_dbl(
        .x = .wtp_,
        .f = function(wtp_ = .x) {
          which.min(
            abs(
              wtp_ - .PSA_data$WTPs
            )
          )
        }
      )
      .wtp_ <- unique(.PSA_data$WTPs[wtp_index_])

      ## Get the ICER table from the result's object:----
      ICER_tbl <- .PSA_data[["ICER"]]

      ## Get the eNMB values from the result's object:----
      eNMB <- .PSA_data[["e.NMB"]] %>%
        ### put WTP in a column next to interventions' expected NMB:
        dplyr::mutate('WTP' = .PSA_data[["WTPs"]]) %>%
        ### filter and keep values in .wtp_ vector:
        dplyr::filter(WTP %in% .wtp_) %>%
        ### rename WTP values to use as column names later:----
      dplyr::mutate(WTP = paste0("NMB @ ",
                                 scales::dollar(
                                   x = WTP,
                                   prefix = .units_))) %>%
        ### put everything in a long format:----
      tidyr::pivot_longer(
        cols = -WTP,
        names_to = 'intervention',
        values_to = 'NMB') %>%
        ### flip the table back to have each intervention in a row:----
      tidyr::pivot_wider(
        id_cols = 'intervention',
        names_from = 'WTP',
        values_from = 'NMB')

      ## Get the probability of being cost-effective from the result's object:----
      CEAF <- dplyr::tibble(
        'CEAF - values' = .PSA_data[["CEAF"]]$ceaf,
        ### put WTP in a column next to probability of CE:----
        'CEAF - WTP' = .PSA_data[["WTPs"]],
        ### put best interv name in a column next to probability of CE:----
        'intervention' = .PSA_data[["best_name"]]) %>%
        ### filter and keep values in .wtp_ vector:----
      dplyr::filter(`CEAF - WTP` %in% .wtp_) %>%
        ### rename WTP values to use as column names later:----
      dplyr::mutate(`CEAF - WTP` = paste0("Prob. CE @ ",
                                          scales::dollar(
                                            x = `CEAF - WTP`,
                                            accuracy = 1,
                                            prefix = .units_)))

      ## Get the EVPI from the result's object:----
      ### Estimate population EVPI if user provided necessary data:----
      discounted_population = 1
      table_caption = "Individual EVPI"
      if(!.individual_evpi_) {
        if(is.null(.evpi_population_) | is.null(.time_horion_)) {
          .individual_evpi_ <- TRUE
          message("Population EVPI or decision time horizon were not supplied.
              The function will calculate individual EVPI")
        }
      }
      if(!.individual_evpi_) {
        #### re-estimate discounted population for population EVPI:
        discounted_population <- sum(
          .evpi_population_ / ((1 + .discount_rate_)^(1:.time_horion_)))
        table_caption = paste0("Population EVPI:- ",
                               "Population size: ", .evpi_population_, "; ",
                               "Time horizon: ", .time_horion_, " year(s); ",
                               "Discount rate: ", .discount_rate_ * 100, "%.")
      }
      ### Join EVPI data:----
      EVPI <- dplyr::tibble(
        'EVPI - values' = .PSA_data[["EVPI"]] * discounted_population,
        ### put WTP in a column next to EVPI:----
        'EVPI - WTP' = .PSA_data[["WTPs"]],
        ### put best interv name in a column next to EVPI:----
        'intervention' = .PSA_data[["best_name"]]) %>%
        ### filter and keep values in .wtp_ vector:----
      dplyr::filter(`EVPI - WTP` %in% .wtp_) %>%
        ### rename WTP values to use as column names later:----
      dplyr::mutate(`EVPI - WTP` = paste0("EVPI @ ",
                                          scales::dollar(
                                            x = `EVPI - WTP`,
                                            prefix = .units_)))

      ## Put summary table together:----
      ### prepare a tidy evaluation variable:----
      incr_col_ <- paste("Incremental", .effects_label_)
      ### start building the final tibble:----
      Summary_tbl <- ICER_tbl %>%
        #### join the expected NMB to the ICER results by intervention name:----
      dplyr::left_join(x = ., y = eNMB, by = 'intervention') %>%
        #### join the probability of being cost-effective by intervention name:----
      dplyr::left_join(x = ., y = CEAF, by = 'intervention') %>%
        #### create probability CE columns from relevant row values:----
      tidyr::pivot_wider(
        names_from = `CEAF - WTP`,
        values_from = `CEAF - values`) %>%
        #### drop any NAs resulting from pivot_wider:----
      dplyr::select(tidyselect::vars_select_helpers$where(
        fn = function(.x) !all(is.na(.x)))) %>%
        #### join the EVPI:
        dplyr::left_join(x = ., y = EVPI, by = 'intervention') %>%
        #### create EVPI columns from relevant row values:----
      tidyr::pivot_wider(
        names_from = `EVPI - WTP`,
        values_from = `EVPI - values`) %>%
        #### drop any NAs resulting from pivot_wider:----
      dplyr::select(tidyselect::vars_select_helpers$where(
        fn = function(.x) !all(is.na(.x)))) %>%
        #### do some formatting:----
      ##### format currency columns:----
      dplyr::mutate(
        dplyr::across(
          tidyselect::vars_select_helpers$where(
            is.numeric) & !c(qalys, delta.e,
                             dplyr::starts_with("Prob.")),
          ~ scales::dollar(
            x = .x,
            prefix = .units_))) %>%
        ##### format effects columns:----
      dplyr::mutate(
        dplyr::across(c(qalys, delta.e, dplyr::starts_with("Prob.")),
                      ~ as.character(round(.x, digits = 4)))) %>%
        ##### drop dominance column if it exists:----
      dplyr::select(-dplyr::any_of('dominance')) %>%
        ##### rename columns to proper names:----
      dplyr::rename({{.effects_label_}} := qalys,
                    Comparators = intervention,
                    {{incr_col_}} := delta.e,
                    "Incremental Costs" = delta.c,
                    "ICER information" = icer_label) %>%
        ##### proper column name:----
      dplyr::rename_with(stringr::str_to_title, costs) %>%
        ##### convert column names to capital letters:----
      dplyr::rename_with(toupper, icer) %>%
        #### put values from ICER information to the ICER column:----
      dplyr::mutate(
        ICER = case_when(
          is.na(ICER) ~
            `ICER information`,
          TRUE ~ ICER)) %>%
        dplyr::select(-`ICER information`)
      ## Create a long format table:----
      if(.long_) {
        ### reorder some columns for DT::RowGroup option:----
        Summary_tbl <- Summary_tbl %>%
          dplyr::select(
            Costs, `Incremental Costs`, QALYs, `Incremental QALYs`,
            dplyr::everything()) %>%
          #### flip the dataset to have everything in long format:----
        tidyr::pivot_longer(
          cols = -Comparators,
          names_to = " ",
          values_to = "Values") %>%
          #### flip the dataset to keep the interventions' in the columns:----
        tidyr::pivot_wider(
          names_from = Comparators,
          values_from = Values)
      }
      ## Feedback if a beautiful version was not requested:----
      if(!.beautify_) {
        print(table_caption)
      }
      ## Beautified tables:----
      ### Long format beautified table:----
      if(.beautify_ & .long_) {
        #### Remove unnecessary strings:----
        Summary_tbl <- Summary_tbl %>%
          dplyr::mutate(dplyr::across(
            .cols = " ",
            .fns = function(.x) {
              stringr::str_replace_all(
                string = .x,
                pattern = c("NMB @ "),
                replacement = c("")
              )
            }
          )) %>%
          dplyr::mutate(dplyr::across(
            .cols = " ",
            .fns = function(.x) {
              stringr::str_replace_all(
                string = .x,
                pattern = c("Prob. CE @ "),
                replacement = c("")
              )
            }
          )) %>%
          dplyr::mutate(dplyr::across(
            .cols = " ",
            .fns = function(.x) {
              stringr::str_replace_all(
                string = .x,
                pattern = c("EVPI @ "),
                replacement = c("")
              )
            }
          ))
        #### Prepare DT-table helper columns:----
        Summary_tbl <- Summary_tbl %>%
          dplyr::mutate(
            ##### Prepare DT-table row groups:----
            RowGroup_ = c(rep(glue::glue("Costs ({.units_})"), 2),
                          rep("QALYs", 2),
                          "Incremental Cost-Effectiveness Ratio",
                          rep(glue::glue("Net Benefit ({.units_})"),
                              length(.wtp_)),
                          rep("Probability Cost-Effective",
                              length(.wtp_)),
                          rep(glue::glue("Expected Value of Perfect
                          Information ({.units_}) [1]"),
                              length(.wtp_))),
            ##### Prepare border info:----
            RowBorder_ = c(0, 1, 0, 1, 1,
                           rep(0, length(.wtp_) - 1), 1,
                           rep(0, length(.wtp_) - 1), 1,
                           rep(0, length(.wtp_) - 1), 1)
          )
        #### Number of columns to show:----
        ColShow_ <- nrow(ICER_tbl)
        #### JS function to add a second caption in the bottom:----
        notes_ <- c(
          "function(settings){",
          "  var datatable = settings.oInstance.api();",
          "  var table = datatable.table().node();",
          glue::glue("  var caption = '<sup><strong>[1] </strong></sup><em>{table_caption}</em>'"),
          "  $(table).append('<caption style=\"caption-side: bottom;text-align: left\">' +
      caption + '</caption>');",
          "}"
        )
        #### Build the table:----
        Summary_tbl <- Summary_tbl %>%
          DT::datatable(
            class = 'compact row-border stripe',
            options = list(
              ordering = FALSE, ## sorting table based on column's values
              paging = FALSE,  ## paginate the output
              pageLength = 15, ## rows number to output for each page
              scrollX = TRUE, ## enable scrolling on X axis
              scrollY = TRUE, ## enable scrolling on Y axis
              autoWidth = FALSE,## use smart column width handling
              server = FALSE,  ## use client-side processing
              dom = 'tB',      ## Bfrtip
              buttons = c('csv', 'excel', 'copy', 'pdf', "print"),
              rowGroup = list(
                dataSrc = ColShow_ + 1
              ), # Column names at the end of the table
              columnDefs = list(
                list( # Hide the column names
                  visible = FALSE,
                  targets = c(ColShow_ + 1, ColShow_ + 2)
                ),
                list(
                  className = 'dt-body-center',
                  targets = 1:ColShow_
                )
              ),
              drawCallback = htmlwidgets::JS(notes_)
            ),
            extensions = c('RowGroup', 'Buttons'),
            selection = 'none', ## enable selection of a single row
            filter = 'none', ## include column filters at the bottom
            rownames = FALSE,  ## don't show row numbers/names
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left;',
              htmltools::h3(
                "Probabilistic Sensitivity Analysis Results"
              )
            )
          ) %>%
          DT::formatStyle(
            columns = 0:(ColShow_ + 1),
            valueColumns = 'RowBorder_',
            `border-bottom` = DT::styleEqual(1, 'solid 1px')
          )
      }
      ### Wide format beautified table:----
      if(.beautify_ & !.long_) {
        #### custom table container to create column groups:----
        sketch_ <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 2, 'Comparators'), # 1 column (merge 2 rows)
              th(rowspan = 2, 'QALYs'), # 1 column (merge 2 rows)
              th(rowspan = 2, 'Costs'), # 1 column (merge 2 rows)
              th(colspan = 2, 'Incremental'), # span over 2 columns
              th(rowspan = 2, 'ICER'), # 1 column (merge 2 rows)
              th(colspan = length(.wtp_), 'Net Benefit'), # span over num .wtp_
              th(colspan = length(.wtp_), 'Probability cost-effective'),
              th(colspan = length(.wtp_), 'EVPI [1]'),
            ),
            tr(
              purrr::map(
                .x = c("QALYs", "Costs", # Incremental
                       rep(
                         scales::dollar(# Net Benefit, Prob. CE, EVPI
                           x = .wtp_,
                           prefix = .units_), 3)),
                .f = th)
            )
          )
        ))
        #### get columns where to border is to be drawn:----
        colBorder_ <- c(1:3, 5, 6,
                        6 + length(.wtp_),
                        6 + (length(.wtp_) * 2),
                        6 + (length(.wtp_) * 3))
        #### JS function to add a second caption in the bottom:----
        notes_ <- c(
          "function(settings){",
          "  var datatable = settings.oInstance.api();",
          "  var table = datatable.table().node();",
          glue::glue("  var caption = '<sup><strong>[1] </strong></sup><em>{table_caption}</em>'"),
          "  $(table).append('<caption style=\"caption-side: bottom;text-align: left\">' +
      caption + '</caption>');",
          "}"
        )
        #### Build the table:----
        Summary_tbl <- Summary_tbl %>%
          DT::datatable(
            class = 'compact row-border',
            options = list(
              ordering = FALSE, ## sorting table based on column's values
              paging = FALSE,  ## paginate the output
              pageLength = 15, ## rows number to output for each page
              scrollX = TRUE, ## enable scrolling on X axis
              scrollY = TRUE, ## enable scrolling on Y axis
              autoWidth = FALSE,## use smart column width handling
              server = FALSE,  ## use client-side processing
              dom = 'tB',      ## Bfrtip
              buttons = c('csv', 'excel', 'copy', 'pdf', "print"),
              columnDefs = list(
                list(
                  targets = 1:(ncol(Summary_tbl) - 1),
                  className = 'dt-body-center'
                ),
                list(
                  targets = 0,
                  className = 'dt-body-left'
                )
              ),
              drawCallback = htmlwidgets::JS(notes_)
            ),
            container = sketch_, ## object to use to draw table
            extensions = 'Buttons',
            selection = 'none', ## enable selection of a single row
            filter = 'none', ## include column filters at the bottom
            rownames = FALSE,  ## don't show row numbers/names
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left;',
              htmltools::h3(
                "Probabilistic Sensitivity Analysis Results"
              )
            )
          ) %>%
          DT::formatStyle(
            columns = colBorder_,
            `border-right` = 'solid 1px'
          )

      }

      return(Summary_tbl)
    },

    ## Plot Cost Effectiveness Acceptability Curve (CEAC):----
    #
    # .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # ... Additional arguments that include:
    # reference intervention \code{.ref = NULL} rescales interventions on
    # CEP, legend position \code{.legend_pos = c(0.8, 0.85)},
    # willingness-to-pay threshold(s)
    # \code{.wtp_threshold = c(20000, 30000)},
    # show WTP threshold(s) lines \code{.show_wtp = TRUE},
    # show WTP threshold(s) labels \code{.label_wtp' = TRUE},
    # zoom to min/max values \code{.zoom = FALSE},
    # zoom to supplied coordinates values \code{.zoom_cords = NULL},
    # show 20 points/shapes along the lines \code{.show_shapes = FALSE},
    # and add Cost Effectiveness Acceptability Curve \code{.add_CEAF =
    # FALSE}.
    #
    # An object of class ggplot.
    #
    # \dontrun{
    # library(ShinyPSA)
    #
    # PSA_summary <- summarise_PSA_(
    #   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #   .interventions = ShinyPSA::Smoking_PSA$treats)
    #
    # p <- plot_CEAC_(PSA_summary,
    #                .ref = 1,
    #                .legend_pos = NULL,
    #                .wtp_threshold = c(2000, 10000, 20000, 25000),
    #                .show_wtp = TRUE,
    #                .label_wtp = FALSE,
    #                .zoom = FALSE,
    #                .zoom_cords = NULL,
    #                .show_shapes = TRUE,
    #                .add_CEAF = TRUE)
    #
    # p
    # }
    #
    plot_CEAC_ = function(.PSA_data = private$PSA_summary, ...) {
      # Grab the function's environment for correct assignment in assign():
      env_ = environment()
      # Define defaults:
      default_args <- list(
        '.ref' = NULL, # Integer 1:length(interventions)
        '.legend_pos' = c(0.8, 0.85), # c(x, y) double between 0:1
        '.wtp_threshold' = c(20000, 30000),
        '.show_wtp' = TRUE, # TRUE/FALSE
        '.label_wtp' = FALSE, # TRUE/FALSE
        '.zoom' = FALSE, # TRUE/FALSE
        '.zoom_cords' = NULL, # c(x, x) double min and max x axis values
        '.show_shapes' = FALSE, # TRUE/FALSE
        '.add_CEAF' = FALSE) # TRUE/FALSE
      # Grab additional arguments:
      args_ <- list(...)
      # Assign additional arguments:
      private$assign_extraArgs_(.default_args_ = default_args,
                                .args_ = args_,
                                .env_ = env_)
      # Override .ref if more than two interventions exist:
      if(!is.null(.ref) & (length(.PSA_data$interventions) > 2)) .ref = NULL
      # Function to remove intervention from plot data:
      drop_intervention <- function(.data_, .ref = .ref) {
        if(!is.null(.ref)) .data_ <- .data_ %>%
            select(-dplyr::all_of(.ref))
        else .data_ <- .data_
        return(.data_)
      }

      # Plot data:
      ceac_df = .PSA_data$CEAC %>%
        drop_intervention(.data_ = ., .ref = .ref) %>%
        dplyr::mutate('WTP threshold' = .PSA_data$WTPs) %>%
        tidyr::pivot_longer(cols = -`WTP threshold`,
                            names_to = 'Option',
                            values_to = 'Probability cost-effective')

      # Zoom:
      if(.zoom | (!is.null(.zoom_cords) & is.numeric(.zoom_cords))) {
        .zoom = TRUE
        if(is.null(.zoom_cords) |
           (!is.null(.zoom_cords) & length(.zoom_cords) != 2))
          .zoom_cords = c(0, 31000)
      }

      # CEAC main plot:
      p <- ggplot2::ggplot() +
        ggplot2::coord_cartesian(ylim = c(0, 1), xlim = .zoom_cords, expand = FALSE) +
        ggplot2::geom_hline(
          yintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_vline(
          xintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_line(
          data = ceac_df,
          ggplot2::aes(x = `WTP threshold`,
                       y = `Probability cost-effective`,
                       color = Option),
          size = 0.4) +
        ggplot2::scale_x_continuous(
          labels = scales::dollar_format(prefix = "\u00A3")) +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::theme(
          plot.title.position = "plot", # Start title from near the margin
          legend.position = .legend_pos,
          legend.title = ggplot2::element_blank(),
          # Control legend text alignment:
          legend.text.align = 0, # 0 left (default), 1 right
          # Remove background and box around the legend:
          legend.background = ggplot2::element_rect(fill = NA, color = NA),
          legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
          legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
          # Add a box around the keys:
          legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
          legend.key.size = ggplot2::unit(0.35, "cm"),
          # Add a border and space around the plot:
          panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
          plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                      c("points", "cm", "points", "points"))) +
        ggplot2::labs(
          title = "Cost Effectiveness Acceptability Curve (CEAC)",
          x = "Willingness-to-pay (\u00A3)",
          y = "Probability cost-effective") +
        ggplot2::guides(
          # Increase the size of the points in the legend:
          color = ggplot2::guide_legend(
            override.aes = list(order = 1,
                                size = 1,
                                alpha = 1,
                                shape = NA)))

      # Show/hide WTP on the CEAC:
      if(.show_wtp) {
        ## CEAC plot willingness-to-pay (WTP) values:
        .wtp = .wtp_threshold %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            x_cord = .wtp_threshold,
            y_cord = 1,
            angle_cord = 0,
            label_cord = scales::dollar(
              x = .wtp_threshold,
              prefix = "\u00A3"),
            lty_ = "Willingness-to-pay (\u00A3)")

        ## Plot:
        p <- p +
          ggplot2::geom_vline(
            data = .wtp,
            ggplot2::aes(xintercept = x_cord,
                         linetype = lty_),
            colour = "dark gray") +
          ggplot2::scale_linetype_manual(
            breaks = .wtp$lty_[1], # keep one for the legend
            values = rep(3, nrow(.wtp))) +
          ggplot2::guides(
            # Remove the shapes from the line:
            linetype = ggplot2::guide_legend(
              override.aes = list(order = 3,
                                  shape = NA, # remove shape
                                  color = 'black')))
      }

      # Label WTP value(s) on the CEAC:
      if(.label_wtp) {
        p <- p +
          ggrepel::geom_text_repel(
            data = .wtp,
            ggplot2::aes(x = x_cord,
                         y = y_cord,
                         angle = angle_cord,
                         label = label_cord),
            size = 1.5,
            show.legend = FALSE)
      }

      # Show/hide shapes on the CEAC:
      if(.show_shapes) {
        ## Data:
        n_points <- .PSA_data$WTPstar
        n_points <- c(0, n_points,
                      seq(from = 0,
                          to = .PSA_data$WTPs[length(.PSA_data$WTPs)],
                          length.out = 20),
                      .PSA_data$WTPs[length(.PSA_data$WTPs)],
                      .wtp_threshold)
        n_points <- sort(
          unique(
            plyr::round_any(n_points, 100, f = ceiling)))

        ## Plot:
        p <- p +
          ggplot2::geom_point(
            data = ceac_df %>%
              dplyr::filter(`WTP threshold` %in% n_points),
            ggplot2::aes(x = `WTP threshold`,
                         y = `Probability cost-effective`,
                         shape = Option, color = Option),
            size = 1,
            show.legend = TRUE)
      }

      # Show/hide CEAF on the CEAC:
      if(.add_CEAF & (length(.PSA_data$interventions) > 2)) {
        ## Data:
        ### Select a few points:
        n_points <- .PSA_data$WTPstar
        n_points <- c(0, n_points,
                      seq(from = 0,
                          to = .PSA_data$WTPs[length(.PSA_data$WTPs)],
                          length.out = 20),
                      .PSA_data$WTPs[length(.PSA_data$WTPs)],
                      .wtp_threshold)
        n_points <- sort(
          unique(
            plyr::round_any(n_points, 100, f = ceiling)))

        ### CEAF:
        ceaf_df = .PSA_data$CEAF %>%
          dplyr::mutate('Best option' = .PSA_data$best_name,
                        'WTP threshold' = .PSA_data$WTPs)

        ## Plot:
        p <- p +
          ggplot2::geom_point(
            data = ceaf_df %>%
              dplyr::filter(`WTP threshold` %in% n_points),
            ggplot2::aes(x = `WTP threshold`,
                         y = ceaf),
            size = 2,
            stroke = 1,
            alpha = 0.8,
            shape = 21,
            color = "black",
            show.legend = TRUE) +
          ggplot2::scale_fill_manual(
            values = c("CEAF" = "black")) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(
              override.aes = list(order = 2,
                                  shape = 21,
                                  size = 2.5,
                                  stroke = 1)))
      }

      return(p)
    },

    ## Plot Cost Effectiveness Acceptability Frontier (CEAF):----
    #
    # .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # ... Additional arguments that include:
    # legend position \code{.legend_pos = c(0.8, 0.85)},
    # willingness-to-pay threshold(s)
    # \code{.wtp_threshold = c(20000, 30000)},
    # show WTP threshold(s) lines \code{.show_wtp = TRUE},
    # show WTP threshold(s) labels \code{.label_wtp' = TRUE},
    # zoom to min/max values \code{.zoom = FALSE},
    # zoom to supplied coordinates values \code{.zoom_cords = NULL}, and
    # show 20 points/shapes along the lines \code{.show_shapes = FALSE}.
    #
    # An object of class ggplot.
    #
    # \dontrun{
    # library(ShinyPSA)
    #
    # PSA_summary <- summarise_PSA_(
    #   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #   .interventions = ShinyPSA::Smoking_PSA$treats)
    #
    # p <- plot_CEAF_(PSA_summary,
    #                .legend_pos = NULL,
    #                .wtp_threshold = c(2000, 10000, 20000, 25000),
    #                .show_wtp = TRUE,
    #                .label_wtp = FALSE,
    #                .zoom = FALSE,
    #                .zoom_cords = NULL,
    #                .show_shapes = TRUE)
    #
    # p
    # }
    #
    plot_CEAF_ = function(.PSA_data = private$PSA_summary, ...) {
      # Grab the function's environment for correct assignment in assign():
      env_ = environment()
      # Define defaults:
      default_args <- list(
        '.legend_pos' = c(0.8, 0.85), # c(x, y) double between 0:1
        '.wtp_threshold' = c(20000, 30000),
        '.show_wtp' = TRUE, # TRUE/FALSE
        '.label_wtp' = FALSE, # TRUE/FALSE
        '.zoom' = FALSE, # TRUE/FALSE
        '.zoom_cords' = NULL, # c(x, x) double min and max x axis values
        '.show_shapes' = FALSE) # TRUE/FALSE
      # Grab additional arguments:
      args_ <- list(...)
      # Assign additional arguments:
      private$assign_extraArgs_(.default_args_ = default_args,
                                .args_ = args_,
                                .env_ = env_)

      # Plot data:
      ceaf_df = .PSA_data$CEAF %>%
        dplyr::mutate('Best option' = .PSA_data$best_name,
                      'WTP threshold' = .PSA_data$WTPs)

      # Zoom:
      if(.zoom | (!is.null(.zoom_cords) & is.numeric(.zoom_cords))) {
        .zoom = TRUE
        if(is.null(.zoom_cords) |
           (!is.null(.zoom_cords) & length(.zoom_cords) != 2))
          .zoom_cords = c(0, 31000)
      }

      # CEAC main plot:
      p <- ggplot2::ggplot() +
        ggplot2::coord_cartesian(ylim = c(0, 1), xlim = .zoom_cords, expand = FALSE) +
        ggplot2::geom_hline(
          yintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_vline(
          xintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_line(
          data = ceaf_df,
          ggplot2::aes(x = `WTP threshold`,
                       y = ceaf,
                       group = 1,
                       color = `Best option`),
          size = 0.4) +
        ggplot2::scale_x_continuous(labels = scales::dollar_format(
          prefix = "\u00A3")) +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::theme(
          plot.title.position = "plot", # Start title from near the margin
          legend.position = .legend_pos,
          legend.title = ggplot2::element_blank(),
          # Control legend text alignment:
          legend.text.align = 0, # 0 left (default), 1 right
          # Remove background and box around the legend:
          legend.background = ggplot2::element_rect(fill = NA, color = NA),
          legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
          legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
          # Add a box around the keys:
          legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
          legend.key.size = ggplot2::unit(0.35, "cm"),
          # Add a border and space around the plot:
          panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
          plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                      c("points", "cm", "points", "points"))) +
        ggplot2::labs(
          title = "Cost Effectiveness Acceptability Frontier (CEAF)",
          x = "Willingness-to-pay (\u00A3)",
          y = "Probability cost-effective") +
        ggplot2::guides(
          # Increase the size of the points in the legend:
          color = ggplot2::guide_legend(
            override.aes = list(order = 1,
                                size = 1,
                                alpha = 1,
                                shape = NA)))

      # Show/hide WTP on the CEAF:
      if(.show_wtp) {
        ## CEAF plot willingness-to-pay (WTP) values:
        .wtp = .wtp_threshold %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            x_cord = .wtp_threshold,
            y_cord = 1,
            angle_cord = 0,
            label_cord = scales::dollar(
              x = .wtp_threshold,
              prefix = "\u00A3"),
            lty_ = "Willingness-to-pay (\u00A3)")

        ## Plot:
        p <- p +
          ggplot2::geom_vline(
            data = .wtp,
            ggplot2::aes(xintercept = x_cord,
                         linetype = lty_),
            colour = "dark gray") +
          ggplot2::scale_linetype_manual(
            breaks = .wtp$lty_[1], # keep one for the legend
            values = rep(3, nrow(.wtp))) +
          ggplot2::guides(
            # Remove the shapes from the line:
            linetype = ggplot2::guide_legend(
              override.aes = list(order = 2,
                                  shape = NA, # remove shape
                                  color = 'black')))
      }

      # Label WTP value(s) on the CEAF:
      if(.label_wtp) {
        p <- p +
          ggrepel::geom_text_repel(
            data = .wtp,
            ggplot2::aes(x = x_cord,
                         y = y_cord,
                         angle = angle_cord,
                         label = label_cord),
            size = 1.5,
            show.legend = FALSE)
      }

      # Show/hide shapes on the CEAF:
      if(.show_shapes) {
        ## Data:
        ### Select a few points:
        n_points <- .PSA_data$WTPstar
        n_points <- c(0, n_points,
                      seq(from = 0,
                          to = .PSA_data$WTPs[length(.PSA_data$WTPs)],
                          length.out = 20),
                      .PSA_data$WTPs[length(.PSA_data$WTPs)],
                      .wtp_threshold)
        n_points <- sort(
          unique(
            plyr::round_any(n_points, 100, f = ceiling)))

        ## Plot:
        p <- p +
          ggplot2::geom_point(
            data = ceaf_df %>%
              dplyr::filter(`WTP threshold` %in% n_points),
            ggplot2::aes(x = `WTP threshold`,
                         y = ceaf,
                         color = `Best option`,
                         shape = `Best option`),
            size = 1.5,
            alpha = 0.8,
            show.legend = TRUE)
      }

      return(p)
    },

    ## Plot Cost Effectiveness Plane (CEP):----
    #
    # .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # ... Additional arguments that include:
    # reference intervention \code{.ref = NULL} rescales interventions on
    # CEP, legend position \code{.legend_pos = c(0.8, 0.2)},
    # show ICER information \code{.show_ICER' = TRUE},
    # nudge ICER labels \code{.nudge_labels' = c(NULL, NULL)},
    # willingness-to-pay threshold(s)
    # \code{.wtp_threshold = c(20000, 30000)},
    # show WTP threshold(s) lines and labels \code{.show_wtp = TRUE}, and
    # seed number to set \code{.seed_no = 1}.
    #
    # An object of class ggplot.
    #
    # \dontrun{
    # library(ShinyPSA)
    #
    # PSA_summary <- summarise_PSA_(
    #   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #
    # p <- plot_CEplane_(PSA_summary,
    #                  .ref = 1,
    #                  .show_ICER = TRUE,
    #                  .legend_pos = c(0.8, 0.2),
    #                  .show_wtp = FALSE,
    #                  .zoom = T,
    #                  .wtp_threshold = c(200),
    #                  tst = "PRINT", # this will be ignored
    #                  .nudge_labels = c(0.1, -0.1),
    #                  .zoom_cords = c(-0.001, 0.001, -5, 5)))
    #
    # p
    # }
    #
    plot_CEplane_ = function(.PSA_data = private$PSA_summary, ...) {
      # Grab the function's environment for correct assignment in assign():
      env_ = environment()
      # Define defaults:
      default_args <- list(
        '.ref' = NULL, # Integer 1:length(interventions)
        '.legend_pos' = c(0.8, 0.2), # c(x, y) double between 0:1
        '.show_ICER' = TRUE, # TRUE/FALSE
        '.nudge_labels' = c(NULL, NULL), # c(x, y) double between 0:1
        '.wtp_threshold' = c(20000, 30000),
        '.show_wtp' = TRUE, # TRUE/FALSE
        '.zoom' = FALSE, # TRUE/FALSE
        '.zoom_cords' = NULL) # double c(min x, max x, min y, max y)
      # Grab additional arguments:
      args_ <- list(...)
      # Assign additional arguments:
      private$assign_extraArgs_(
        .default_args_ = default_args,
        .args_ = args_,
        .env_ = env_)

      # Plot data:
      ## CE plot points:
      if(is.null(.ref)) { # No rescaling of point data
        ce_plane_dt <- .PSA_data$e %>%
          dplyr::mutate(sims = dplyr::row_number()) %>%
          tidyr::pivot_longer(
            cols = -sims,
            names_to = "interventions",
            values_to = "Effects") %>%
          dplyr::left_join(
            x = .,
            y = .PSA_data$c %>%
              dplyr::mutate(sims = dplyr::row_number()) %>%
              tidyr::pivot_longer(
                cols = -sims,
                names_to = "interventions",
                values_to = "Costs"),
            by = c("sims", "interventions"))
        # Labels:
        .title_lab = "Cost Effectiveness Plane"
        .x_lab = "Effects"
        .y_lab = "Costs (\u00A3)"
      } else { # Rescale point data
        ce_plane_dt <- .PSA_data$e %>%
          private$calculate_differentials_(.ref = .ref) %>%
          dplyr::mutate(sims = dplyr::row_number()) %>%
          tidyr::pivot_longer(
            cols = -sims,
            names_to = "interventions",
            values_to = "Effects") %>%
          dplyr::left_join(
            x = .,
            y = .PSA_data$c %>%
              private$calculate_differentials_(.ref = .ref) %>%
              dplyr::mutate(sims = dplyr::row_number()) %>%
              tidyr::pivot_longer(
                cols = -sims,
                names_to = "interventions",
                values_to = "Costs"),
            by = c("sims", "interventions"))
        # Labels:
        .title_lab = "Cost Effectiveness Plane"
        .x_lab = "Effectiveness differential"
        .y_lab = "Cost differential (\u00A3)"
      }

      ## CE plot mean values:
      ce_plane_mean_dt <- ce_plane_dt %>%
        dplyr::group_by(interventions) %>%
        dplyr::summarise(
          Effects = mean(Effects),
          Costs = mean(Costs)) %>%
        dplyr::left_join(
          x = .,
          y = .PSA_data$ICER %>%
            select(intervention, icer_label) %>%
            rename("interventions" = intervention),
          by = "interventions") %>%
        dplyr::rename("Label" = icer_label)

      # Plot:
      p <- ggplot2::ggplot() +
        ggplot2::geom_hline(
          yintercept = 0, colour = "dark gray") +
        ggplot2::geom_vline(
          xintercept = 0, colour = "dark gray") +
        ggplot2::geom_point(
          data = ce_plane_dt,
          ggplot2::aes(
            x = Effects,
            y = Costs,
            color = interventions),
          size = 1, alpha = 0.5) +
        ggplot2::scale_y_continuous(
          labels = scales::dollar_format(prefix = "\u00A3",
                                         big.mark = ",")) +
        ggplot2::geom_point(
          data = ce_plane_mean_dt,
          ggplot2::aes(
            x = Effects,
            y = Costs,
            fill = interventions),
          shape = 21, colour = "black", show.legend = TRUE,
          size = 2, alpha = 1, stroke = 0.6) +
        ## Keep one value in the legend:
        ggplot2::scale_fill_discrete(
          breaks = ce_plane_mean_dt$interventions[1], # keep one
          labels = "Mean effects/costs") + # change its label
        ggplot2::theme(
          plot.title.position = "plot", # Start title from near the margin
          legend.position = .legend_pos,
          legend.title = ggplot2::element_blank(),
          # Control legend text alignment:
          legend.text.align = 0, # 0 left (default), 1 right
          # Remove background and box around the legend:
          legend.background = ggplot2::element_rect(fill = NA, color = NA),
          legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
          legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
          # Add a box around the keys:
          legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
          legend.key.size = ggplot2::unit(0.35, "cm"),
          # Add a border around the plot:
          panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
          plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                      c("points", "cm", "points", "points"))) +
        ggplot2::labs(
          title = .title_lab,
          x = .x_lab,
          y = .y_lab) +
        ggplot2::guides(
          # Increase the size of the points in the legend:
          color = ggplot2::guide_legend(
            override.aes = list(order = 1,
                                size = 1.5,
                                alpha = 1,
                                stroke = NA, # remove stroke
                                linetype = 0)), # remove line
          # Remove the fill colour in shape 21, generalising it to all options:
          fill = ggplot2::guide_legend(
            override.aes = list(order = 2,
                                size = 2.5,
                                alpha = 1,
                                fill = NA, # remove fill
                                linetype = 0))) # remove line

      # Show/hide ICER label(s) on the CE plot:
      if(.show_ICER) {
        ## CE plot ICER labels nudging values:
        .nudge_labels[1] = max(ce_plane_dt$Effects) *
          .nudge_labels[1]
        .nudge_labels[2] = (max(ce_plane_dt$Costs) - min(ce_plane_dt$Costs)) *
          .nudge_labels[2]

        ## Plot:
        p <- p +
          ggrepel::geom_text_repel(
            data = ce_plane_mean_dt,
            ggplot2::aes(
              x = Effects,
              y = Costs,
              label = Label),
            force_pull = 8,
            size = 2.5,
            point.padding = 0,
            nudge_x = .nudge_labels[1],
            nudge_y = .nudge_labels[2],
            segment.curvature = 1e-8,
            arrow = ggplot2::arrow(length = ggplot2::unit(0.015, "npc")),
            max.overlaps = Inf,
            min.segment.length = 0)
      }

      # Show/hide WTP label(s) on the CE plot:
      if(.show_wtp) {
        ## CE plot willingness-to-pay (WTP) values:
        if(max(ce_plane_dt$Effects) < max(abs(ce_plane_dt$Effects))){
          ### Get labels' coordinates dynamically:
          x_cord = ifelse(rep(min(ce_plane_dt$Costs), length(.wtp_threshold)) <
                            0,
                          ifelse((min(ce_plane_dt$Costs) / .wtp_threshold) <=
                                   min(ce_plane_dt$Effects),
                                 min(ce_plane_dt$Effects),
                                 (min(ce_plane_dt$Costs) / .wtp_threshold)),
                          ifelse(-(min(ce_plane_dt$Costs) / .wtp_threshold) <=
                                   min(ce_plane_dt$Effects),
                                 min(ce_plane_dt$Effects),
                                 -(min(ce_plane_dt$Costs) / .wtp_threshold)))
          y_cord = ifelse(rep(max(ce_plane_dt$Costs), length(.wtp_threshold)) <
                            0,
                          0,
                          ifelse((min(ce_plane_dt$Effects) * .wtp_threshold) <=
                                   min(ce_plane_dt$Costs),
                                 min(ce_plane_dt$Costs),
                                 (min(ce_plane_dt$Effects) * .wtp_threshold)))
        } else {
          ### Get labels' coordinates dynamically:
          x_cord = ifelse((max(ce_plane_dt$Costs) / .wtp_threshold) >=
                            max(ce_plane_dt$Effects),
                          max(ce_plane_dt$Effects),
                          (max(ce_plane_dt$Costs) / .wtp_threshold))
          y_cord = ifelse(rep(max(ce_plane_dt$Costs), length(.wtp_threshold)) <
                            0,
                          0,
                          ifelse((max(ce_plane_dt$Effects) * .wtp_threshold) >=
                                   max(ce_plane_dt$Costs),
                                 max(ce_plane_dt$Costs),
                                 (max(ce_plane_dt$Effects) * .wtp_threshold)))
        }

        ### Get axis scale to correctly set the labels:
        x_range <- ggplot2::layer_scales(p)$x$range$range
        y_range <- ggplot2::layer_scales(p)$y$range$range
        x_to_y <- (x_range[2] - x_range[1])/(y_range[2] - y_range[1])
        ### Calculate angles:
        angle_cord <- atan(.wtp_threshold * x_to_y) * 180/pi
        ### Put .wtp data on tibble:
        .wtp = .wtp_threshold %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            x_cord = x_cord,
            y_cord = y_cord,
            angle_cord = angle_cord,
            label_cord = scales::dollar(
              x = .wtp_threshold,
              prefix = "\u00A3"
            ),
            lty_ = "Willingness-to-pay (\u00A3)")

        ## Plot:
        p <- p +
          ggplot2::geom_abline(
            data = .wtp,
            ggplot2::aes(
              intercept = 0,
              slope = value,
              linetype = lty_),
            show.legend = TRUE) +
          ggplot2::scale_linetype_manual(
            breaks = .wtp$lty_[1], # keep one for the legend
            values = rep(3, nrow(.wtp))) +
          ggrepel::geom_text_repel(
            data = .wtp,
            ggplot2::aes(
              x = x_cord,
              y = y_cord,
              #angle = angle_cord,
              label = label_cord),
            size = 1.5,
            show.legend = FALSE) +
          ggplot2::guides(
            # Remove the stroke from the line:
            linetype = ggplot2::guide_legend(
              override.aes = list(order = 3,
                                  stroke = NA)) # remove stroke
          )
      }

      # Zoom to max x and y values:
      if(.zoom &
         (is.null(.zoom_cords) |
          if(!is.null(.zoom_cords)) length(.zoom_cords) < 4 else TRUE)) {

        ## CE plot x and y axis limits:
        x_lim = c(NA, max(ce_plane_dt$Effects))
        y_lim = c(NA, max(ce_plane_dt$Costs))

        # Plot:
        p <- p +
          ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim, expand = !.zoom,
                                   default = .zoom)
      }

      if(.zoom & !is.null(.zoom_cords) &
         if(!is.null(.zoom_cords)) length(.zoom_cords) == 4 else FALSE) {
        ## CE plot x and y axis limits:
        x_lim = c(.zoom_cords[1], .zoom_cords[2])
        y_lim = c(.zoom_cords[3], .zoom_cords[4])

        # Plot:
        p <- p +
          ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim, expand = !.zoom,
                                   default = .zoom)
      }

      return(p)
    },

    ## Plot the Expected Value of Perfect Information (EVPI):----
    #
    # .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # ... Additional arguments that include:
    # legend position \code{.legend_pos = "bottom"},
    # willingness-to-pay threshold(s)
    # \code{.wtp_threshold = c(20000, 30000)},
    # show WTP threshold(s) lines \code{.show_wtp = TRUE},
    # show WTP threshold(s) labels \code{.label_wtp' = TRUE},
    # plot individual EVPI \code{.individual_evpi' = TRUE},
    # time horizon to estimate population EVPI \code{.time_horion = 5},
    # discount rate to estimate population EVPI
    # \code{.discount_rate = 0.035},
    # population size for population EVPI \code{.population' = 15000},
    # zoom to min/max values \code{.zoom = FALSE}, and
    # zoom to supplied coordinates values \code{.zoom_cords = NULL}.
    #
    # An object of class ggplot.
    #
    # \dontrun{
    # library(ShinyPSA)
    #
    # PSA_summary <- summarise_PSA_(
    #   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #   .interventions = ShinyPSA::Smoking_PSA$treats)
    #
    # p <- plot_EVPI_(PSA_summary,
    #                .legend_pos = NULL,
    #                .wtp_threshold = c(2000, 10000, 20000, 25000),
    #                .show_wtp = TRUE,
    #                .label_wtp = FALSE,
    #                .individual_evpi = FALSE,
    #                .time_horion = 1,
    #                .discount_rate = 0.035,
    #                .population = 15000,
    #                .zoom = FALSE,
    #                .zoom_cords = NULL)
    #
    # p
    # }
    #
    plot_EVPI_ = function(.PSA_data = private$PSA_summary, ...) {
      # Grab the function's environment for correct assignment in assign():
      env_ = environment()
      # Define defaults:
      default_args <- list(
        '.legend_pos' = "bottom", # c(x, y) double between 0:1 or character
        '.wtp_threshold' = c(20000, 30000),
        '.show_wtp' = TRUE, # TRUE/FALSE
        '.label_wtp' = FALSE, # TRUE/FALSE
        '.individual_evpi' = TRUE, # TRUE/FALSE
        '.time_horion' = 5, # Integer
        '.discount_rate' = 0.035, # double 0:1
        '.population' = 15000, # Integer
        '.zoom' = FALSE, # TRUE/FALSE
        '.zoom_cords' = NULL) # c(x, x) double min and max x axis values
      # Grab additional arguments:
      args_ <- list(...)
      # Assign additional arguments:
      private$assign_extraArgs_(
        .default_args_ = default_args,
        .args_ = args_,
        .env_ = env_)

      # Plot data:
      discounted_population = 1
      subtitle_lab = "    Individual EVPI"

      ## Population EVPI:
      if(!.individual_evpi){
        ## Population EVPI:
        discounted_population <- sum(
          .population / ((1 + .discount_rate)^(1:.time_horion)))
        subtitle_lab = paste0("    Population EVPI: [",
                              "Population size: ", .population, "; ",
                              "Time horizon: ", .time_horion, " year(s); ",
                              "Discount rate: ", .discount_rate, ".]")
      }
      ## EVPI data:
      evpi_df <- dplyr::tibble(
        'EVPI' = .PSA_data$EVPI * discounted_population,
        'WTP threshold' = .PSA_data$WTPs,
        'Best option' = .PSA_data$best_name)

      # Zoom:
      y_cords <- NULL
      if(.zoom | (!is.null(.zoom_cords) & is.numeric(.zoom_cords))) {
        .zoom = TRUE
        if(is.null(.zoom_cords) |
           (!is.null(.zoom_cords) & length(.zoom_cords) != 2))
          .zoom_cords = c(0, 31000)
      }

      # CEAC main plot:
      p <- ggplot2::ggplot() +
        ggplot2::coord_cartesian(ylim = y_cords, xlim = .zoom_cords, expand = FALSE) +
        ggplot2::geom_hline(
          yintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_vline(
          xintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_line(
          data = evpi_df,
          ggplot2::aes(
            x = `WTP threshold`,
            y = EVPI),
          size = 0.4) +
        ggplot2::scale_x_continuous(labels = scales::dollar_format(
          prefix = "\u00A3")) +
        ggplot2::scale_y_continuous(labels = scales::dollar_format(
          prefix = "\u00A3")) +
        ggplot2::theme(
          # Adjust title size and position:
          plot.title.position = "plot", # Start title from near the margin
          plot.subtitle = ggplot2::element_text(size = 6, face = "italic"),
          legend.position = .legend_pos,
          legend.title = ggplot2::element_blank(),
          # Control legend text alignment:
          legend.text.align = 0, # 0 left (default), 1 right
          # Remove background and box around the legend:
          legend.background = ggplot2::element_rect(fill = NA, color = NA),
          legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
          legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
          legend.margin=ggplot2::margin(t = -8), # remove space between it x-axis
          # Add a box around the keys:
          legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
          legend.key.size = ggplot2::unit(0.35, "cm"),
          # Add a border and space around the plot:
          panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
          plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                      c("points", "cm", "points", "points"))) +
        ggplot2::labs(
          title = "Expected Value of Perfect Information (EVPI)",
          x = "Willingness-to-pay (\u00A3)",
          y = "Expected value of perfect information",
          subtitle = subtitle_lab)

      # Show/hide WTP on the CEAF:
      if(.show_wtp) {
        ## CEAF plot willingness-to-pay (WTP) values:
        .wtp = .wtp_threshold %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            x_cord = .wtp_threshold,
            y_cord = max(evpi_df$EVPI),
            angle_cord = 0,
            label_cord = scales::dollar(
              x = .wtp_threshold,
              prefix = "\u00A3"),
            lty_ = "Willingness-to-pay (\u00A3)")

        ## Plot:
        p <- p +
          ggplot2::geom_vline(
            data = .wtp,
            ggplot2::aes(
              xintercept = x_cord,
              linetype = lty_),
            colour = "dark gray") +
          ggplot2::scale_linetype_manual(
            breaks = .wtp$lty_[1], # keep one for the legend
            values = rep(3, nrow(.wtp))) +
          ggplot2::guides(
            # Remove the shapes from the line:
            linetype = ggplot2::guide_legend(
              override.aes = list(order = 2,
                                  shape = NA, # remove shape
                                  color = 'black')))
      }

      # Label WTP value(s) on the CEAF:
      if(.label_wtp) {
        p <- p +
          ggrepel::geom_text_repel(
            data = .wtp,
            ggplot2::aes(
              x = x_cord,
              y = y_cord,
              angle = angle_cord,
              label = label_cord),
            size = 1.5,
            show.legend = FALSE)
      }

      return(p)
    },

    ## Plot Expected Net Monetary Benefit (eNMB):----
    #
    # .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # ... Additional arguments that include:
    # legend position \code{.legend_pos = "bottom"},
    # willingness-to-pay threshold(s)
    # \code{.wtp_threshold = c(20000, 30000)},
    # show WTP threshold(s) lines \code{.show_wtp = TRUE},
    # show WTP threshold(s) labels \code{.label_wtp' = TRUE},
    # zoom to min/max values \code{.zoom = FALSE}, and
    # zoom to supplied coordinates values \code{.zoom_cords = NULL}.
    #
    # An object of class ggplot.
    #
    # \dontrun{
    # library(ShinyPSA)
    #
    # PSA_summary <- summarise_PSA_(
    #   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #   .interventions = ShinyPSA::Smoking_PSA$treats)
    #
    # p <- plot_eNMB_(PSA_summary,
    #                .legend_pos = NULL,
    #                .wtp_threshold = c(2000, 10000, 20000, 25000),
    #                .show_wtp = TRUE,
    #                .label_wtp = FALSE,
    #                .zoom = FALSE,
    #                .zoom_cords = NULL)
    #
    # p
    # }
    #
    plot_eNMB_ = function(.PSA_data = private$PSA_summary, ...) {
      # Grab the function's environment for correct assignment in assign():
      env_ = environment()
      # Define defaults:
      default_args <- list(
        '.legend_pos' = c(0.22, 0.78), # c(x, y) between 0:1 or character
        '.wtp_threshold' = c(20000, 30000),
        '.show_wtp' = TRUE, # TRUE/FALSE
        '.label_wtp' = FALSE, # TRUE/FALSE
        '.zoom' = FALSE, # TRUE/FALSE
        '.zoom_cords' = NULL) # c(x, x)  min and max x axis values
      # Grab additional arguments:
      args_ <- list(...)
      # Assign additional arguments:
      private$assign_extraArgs_(
        .default_args_ = default_args,
        .args_ = args_,
        .env_ = env_)

      # Plot data:
      enmb_df <- .PSA_data$e.NMB %>%
        dplyr::as_tibble() %>%
        dplyr::mutate('WTP threshold' = .PSA_data$WTPs,
                      'Best option' = .PSA_data$best_name) %>%
        tidyr::pivot_longer(
          cols = colnames(.PSA_data$e.NMB),
          names_to = 'Option',
          values_to = 'eNMB')

      # Zoom:
      y_cords <- NULL
      if(.zoom | (!is.null(.zoom_cords) & is.numeric(.zoom_cords))) {
        .zoom = TRUE
        if(is.null(.zoom_cords) |
           (!is.null(.zoom_cords) & length(.zoom_cords) != 2))
          .zoom_cords = c(0, 31000)
      }

      # CEAC main plot:
      p <- ggplot2::ggplot() +
        ggplot2::coord_cartesian(ylim = y_cords, xlim = .zoom_cords, expand = FALSE) +
        ggplot2::geom_hline(
          yintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_vline(
          xintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_line(
          data = enmb_df,
          ggplot2::aes(
            x = `WTP threshold`,
            y = eNMB,
            group = Option,
            linetype = Option,
            color = Option),
          size = 0.4) +
        ggplot2::scale_x_continuous(labels = scales::dollar_format(
          prefix = "\u00A3")) +
        ggplot2::scale_y_continuous(labels = scales::dollar_format(
          prefix = "\u00A3")) +
        ggplot2::theme(
          plot.title.position = "plot", # Start title from near the margin
          legend.position = .legend_pos,
          legend.title = ggplot2::element_blank(),
          # Control legend text alignment:
          legend.text.align = 0, # 0 left (default), 1 right
          # Remove background and box around the legend:
          legend.background = ggplot2::element_rect(fill = NA, color = NA),
          legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
          legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
          #legend.margin=margin(t = -8), # remove space between it x-axis
          # Add a box around the keys:
          legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
          legend.key.size = ggplot2::unit(0.35, "cm"),
          # Add a border and space around the plot:
          panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
          plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                      c("points", "cm", "points", "points"))) +
        ggplot2::labs(
          title = "Expected Net Monetary Benefit (eNMB)",
          x = "Willingness-to-pay (\u00A3)",
          y = "Expected Net Monetary Benefit (\u00A3)")


      # Show/hide WTP on the CEAF:
      if(.show_wtp) {
        ## CEAF plot willingness-to-pay (WTP) values:
        .wtp = .wtp_threshold %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            x_cord = .wtp_threshold,
            y_cord = max(enmb_df$eNMB),
            angle_cord = 0,
            label_cord = scales::dollar(
              x = .wtp_threshold,
              prefix = "\u00A3"),
            lty_ = "Willingness-to-pay (\u00A3)")

        ## Plot:
        p <- p +
          ggplot2::geom_vline(
            data = .wtp,
            ggplot2::aes(
              xintercept = x_cord,
              alpha = lty_),
            color = 'dark gray',
            linetype = 3) +
          ggplot2::scale_alpha_manual(
            breaks = .wtp$lty_[1], # keep one for the legend
            values = rep(1, nrow(.wtp))) +
          # scale_colour_manual(
          #   breaks = .wtp$lty_[1], # keep one for the legend
          #   values = rep("dark gray", nrow(.wtp)))
          ggplot2::guides(
            # Remove the shapes from the line:
            alpha = ggplot2::guide_legend(
              override.aes = list(order = 2,
                                  shape = NA, # remove shape
                                  color = 'black')))
      }

      # Label WTP value(s) on the CEAF:
      if(.label_wtp) {
        p <- p +
          ggrepel::geom_text_repel(
            data = .wtp,
            ggplot2::aes(
              x = x_cord,
              y = y_cord,
              angle = angle_cord,
              label = label_cord),
            size = 1.5,
            show.legend = FALSE)
      }

      return(p)
    },

    ## Check stability of PSA outputs:----
    # This function produces a set of plots to allow modellers to investigate if
    # the number of PSA runs were sufficient or not!
    #
    # @param .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # @param .effs A tibble containing the \code{effects} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be summarised.
    # @param .costs A tibble containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be summarised.
    # @param .interventions A vector containing the names of all interventions.
    # If not provided or less names than needed is provided, the function will
    # generate generic names, for example \code{intervention 1}.
    # @param ... Additoinal plot arguments, which currently supports:
    # .legend_pos (legend position): format (x, y) where x and y are between
    # \code{0, 1}. If missing, the function uses default (0.8, 0.85).
    #
    # @return A list with objects of class ggplot.
    #
    # @examples
    # \dontrun{
    # library(ShinyPSA)
    #
    # p <- check_PSA_stability(
    #          .effs = ShinyPSA::Hypothetical_PSA$e,
    #         .costs = ShinyPSA::Hypothetical_PSA$c,
    #         .interventions = ShinyPSA::Hypothetical_PSA$treats)
    #
    # p[["Effects stability"]]
    # p[["Costs stability"]]
    # p[["Cost per effect stability"]]
    # }
    check_PSA_stability = function(.PSA_data = private$PSA_summary,
                                   .effs = NULL, .costs = NULL,
                                   .interventions = NULL, ...) {
      # Grab effects and costs if .PSA_data was supplied but not .effs and .costs:----
      if(is.null(.effs)) {
        .effs = .PSA_data$e
      }
      if(is.null(.costs)) {
        .costs = .PSA_data$c
      }
      if(is.null(.interventions)) {
        .interventions = .PSA_data$interventions
      }

      # Stop if .effs & .costs are not of class tibble or have unequal dims:----
      stopifnot('.effs is not a tibble' = "data.frame" %in% class(.effs),
                '.costs is not a tibble' = "data.frame" %in% class(.costs),
                '.effs and .costs have unequal dimensions' =
                  dim(.effs) == dim(.costs))

      # Get number of interventions in supplied matrix:----
      n.comparators <- ncol(.effs) # Number of interventions

      # Check supplied interventions labels, create ones if any is missing:----
      if(is.null(.interventions)) {
        .interventions <- paste("intervention", 1:n.comparators)
      }
      if(length(.interventions) != n.comparators) {
        .interventions <- paste("intervention", 1:n.comparators)
      }

      # Ensure .effs and .costs are tibbles and name columns appropriately:----
      .effs <- .effs %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        `colnames<-`(.interventions)
      .costs <- .costs %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        `colnames<-`(.interventions)

      # Estimate PSA outputs' stability:----
      effs_stab <- purrr::map_dfc(
        .x = .effs,
        .f = dplyr::cummean)
      csts_stab <- purrr::map_dfc(
        .x = .costs,
        .f = dplyr::cummean)
      csts_per_effs <- csts_stab / effs_stab

      # Plot data:----
      ## Effects plot data:----
      effs_stab_df <- effs_stab  %>%
        dplyr::mutate(
          `PSA runs` = 1:nrow(.)) %>%
        tidyr::pivot_longer(
          cols = -`PSA runs`,
          names_to = 'Interventions',
          values_to = 'Effects')
      ## Costs plot data:----
      csts_stab_df <- csts_stab %>%
        dplyr::mutate(
          `PSA runs` = 1:nrow(.)) %>%
        tidyr::pivot_longer(
          cols = -`PSA runs`,
          names_to = 'Interventions',
          values_to = 'Costs')
      ## Cost per Effect data:----
      csts_per_effs_df <- csts_per_effs %>%
        dplyr::mutate(
          `PSA runs` = 1:nrow(.)) %>%
        tidyr::pivot_longer(
          cols = -`PSA runs`,
          names_to = 'Interventions',
          values_to = 'Cost (£) per effect')

      # Stability plot defaults:----
      ## Grab the function's environment for correct assignment in assign():----
      env_ = environment()
      ## Define defaults:----
      default_args <- list(
        '.arrange' = "all", # all/v/h
        '.zoom' = FALSE, # TRUE/FALSE
        '.zoom_cords' = NULL, # c(x, x) double min and max x axis values
        '.legend_pos' = c(0.8, 0.85)) # c(x, y) double values between 0:1
      ## Grab additional arguments:----
      args_ <- list(...)
      ## Assign additional arguments:----
      assign_extraArgs_(
        .default_args_ = default_args,
        .args_ = args_,
        .env_ = env_)

      # Stability main plots:----
      plots_list <- NULL
      ## Effects plot:----
      plots_list[['Effects stability']] <- private$plot_stability_lines(
        df = effs_stab_df,
        x_var = "`PSA runs`",
        y_var = "Effects",
        plot_group = "Interventions",
        title = "Stability of PSA mean effects",
        x_lab = NULL,
        y_lab = "Mean effects",
        .legend_pos_ = .legend_pos,
        .zoom_ = .zoom,
        .zoom_cords_ = .zoom_cords,
        .add_ylabel = FALSE)
      ## Costs plot:----
      plots_list[['Costs stability']] <- private$plot_stability_lines(
        df = csts_stab_df,
        x_var = "`PSA runs`",
        y_var = "Costs",
        plot_group = "Interventions",
        title = "Stability of PSA mean costs",
        x_lab = NULL,
        y_lab = "Mean costs (\u00A3)",
        .legend_pos_ = .legend_pos,
        .zoom_ = .zoom,
        .zoom_cords_ = .zoom_cords,
        .add_ylabel = TRUE)
      ## Cost per effect stability plot:----
      plots_list[['Cost per effect stability']] <- private$plot_stability_lines(
        df = csts_per_effs_df,
        x_var = "`PSA runs`",
        y_var = "`Cost (£) per effect`",
        plot_group = "Interventions",
        title = "Stability of PSA mean cost per effect",
        x_lab = "PSA run",
        y_lab = "Mean cost per effect (\u00A3)",
        .legend_pos_ = .legend_pos,
        .zoom_ = .zoom,
        .zoom_cords_ = .zoom_cords,
        .add_ylabel = TRUE)
      ## Combine plots into one compass:----
      if(.arrange == 'all') {
        grouped_plots <- ggpubr::ggarrange(
          plotlist = plots_list[3], # plots_list[c(1, 2)]
          ggpubr::ggarrange(
            plotlist = plots_list[c(1, 2)],
            nrow = 1,
            legend = 'none'),
          nrow = 2,
          legend = "top",
          common.legend = TRUE)
      } else if(.arrange == 'v') {
        grouped_plots <- ggpubr::ggarrange(
          plotlist = plots_list,
          nrow = 1,
          legend = "top",
          common.legend = TRUE)
      } else if (.arrange == 'h') {
        grouped_plots <- ggpubr::ggarrange(
          plotlist = plots_list,
          ncol = 1,
          legend = "bottom",
          common.legend = TRUE)
      }

      return(grouped_plots)
    },

    ## Plot PSA stability graphs:----
    #
    # @param df Long format PSA cumulative output dataset
    # @param x_var Name of x-axis variable, expects \code{"PSA run"}.
    # @param y_var Name of y-axis variable, expects \code{"Mean effects",
    # "Mean costs (\u00A3)", "Mean cost per effect (\u00A3)"}.
    # @param plot_group Name of grouping variable, expects \code{"Interventions"}.
    # @param title Plot title.
    # @param x_lab X axis label.
    # @param y_lab Y axis label.
    # @param .legend_pos_ Legend position based on x and y axis relative location.
    # @param .zoom_ Restrict plot to a specific range of PSA runs, if \code{TRUE}.
    # @param .zoom_cords_ Range of X axis values \code{c(minimum value,
    # maximum value)}
    # @param .add_ylabel Add \u00A3 to Y axis if monetary variable is supplied to
    # the function.
    #
    # @return An object of class ggplot2.
    #
    # @examples
    # \dontrun{
    # ## Effects plot:
    # plots_list[['Effects stability']] <- plot_stability_lines(
    #   df = effs_stab_df,
    #   x_var = "`PSA runs`",
    #   y_var = "Effects",
    #   plot_group = "Interventions",
    #   title = "Stability of PSA mean effects",
    #   x_lab = "PSA run",
    #   y_lab = "Mean effects")
    # ## Costs plot:
    # plots_list[['Costs stability']] <- plot_stability_lines(
    #   df = csts_stab_df,
    #   x_var = "`PSA runs`",
    #   y_var = "Costs",
    #   plot_group = "Interventions",
    #   title = "Stability of PSA mean costs",
    #   x_lab = "PSA run",
    #   y_lab = "Mean costs (\u00A3)",
    #   .add_ylabel = TRUE)
    # ## Cost per effect stability plot:
    # plots_list[['Cost per effect stability']] <- plot_stability_lines(
    #   df = csts_per_effs_df,
    #   x_var = "`PSA runs`",
    #   y_var = "`Cost (£) per effect`",
    #   plot_group = "Interventions",
    #   title = "Stability of PSA mean cost per effect",
    #   x_lab = "PSA run",
    #   y_lab = "Mean cost per effect (\u00A3)",
    #   .add_ylabel = TRUE)
    # }
    plot_stability_lines = function(df, x_var, y_var, plot_group, title,
                                     x_lab, y_lab, .legend_pos_ = .legend_pos,
                                     .zoom_ = .zoom, .zoom_cords_ = .zoom_cords,
                                     .add_ylabel = FALSE) {
      # Zoom:
      if(.zoom_ | (!is.null(.zoom_cords_) & is.numeric(.zoom_cords_))) {
        .zoom_ = TRUE
        if(is.null(.zoom_cords_) |
           (!is.null(.zoom_cords_) & length(.zoom_cords_) != 2))
          .zoom_cords_ = c(0, 1000)
      }

      p <- ggplot2::ggplot() +
        ggplot2::coord_cartesian(
          ylim = .zoom_cords_,
          xlim = c(0, NA),
          expand = FALSE) +
        ggplot2::geom_hline(
          yintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_vline(
          xintercept = 0,
          color = 'grey',
          size = 0.1) +
        ggplot2::geom_line(
          data = df,
          ggplot2::aes_string(
            x = x_var,
            y = y_var,
            color = plot_group),
          size = 0.4) +
        ggplot2::theme(
          # Start title from near the margin:
          plot.title.position = "plot",
          # Control legend's position and remove its title:
          legend.position = .legend_pos_,
          legend.title = ggplot2::element_blank(),
          # Control legend text alignment:
          legend.text.align = 0, # 0 left (default), 1 right
          # Remove background and box around the legend:
          legend.background = ggplot2::element_rect(fill = NA, color = NA),
          legend.spacing = ggplot2::unit(0, "cm"), # spacing between legend items
          legend.spacing.y = ggplot2::unit(-0.195, "cm"), # bring legends closer
          # Add a box around the keys:
          legend.key = ggplot2::element_rect(fill = "white", colour = "grey"),
          legend.key.size = ggplot2::unit(0.35, "cm"),
          # Add a border and space around the plot:
          panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
          plot.margin = ggplot2::unit(c(5.5, 1, 5.5, 5.5), # more space LHS
                                      c("points", "cm", "points", "points"))) +
        ggplot2::labs(
          subtitle = title,
          x = x_lab,
          y = y_lab)

      if(.add_ylabel) {
        p <- p +
          ggplot2::scale_y_continuous(labels = scales::dollar_format(
            prefix = "\u00A3"))
      }

      return(p)
    },

    ## Check and add any missing columns expected by ICER computation:----
    # functions
    #
    # .characters the columns to ensure in returned data table that are
    # expected to contain characters.
    # .numerics the columns to ensure in returned data table that are
    # expected to contain numerics.
    # .x the data table to which the columns are to be added.
    #
    # A tibble with at least the set of variables/columns provided by the
    # user.
    #
    # \dontrun{}
    add_missing_columns_ = function(.x, .characters, .numerics) {
      # Check for missing columns:
      missing_nms <- dplyr::setdiff(c(.numerics, .characters), names(.x))

      # In case there were missing columns:
      if(!length(missing_nms) == 0) {
        # Create missing columns:
        .x <- .x %>%
          cbind(missing_nms %>%
                  `names<-`(missing_nms) %>%
                  purrr::map_dfc(
                    .f = function(.x) {
                      .x = ifelse(.x %in% .numerics, NA_real_, NA_character_)
                    }))
      }
      .x <- .x %>%
        dplyr::select(".id", dplyr::everything()) %>%
        dplyr::mutate(.id = dplyr::row_number())

      return(.x)
    },

    ## Calculate differential costs and QALYs:----
    #
    # .data_ A dataframe containing costs or QALYs data for which the
    # function is to estimate differential values
    # .ref An integer indicating the index of the reference intervention
    #
    # A tibble with differential costs and QALYs.
    #
    # \dontrun{}
    calculate_differentials_ = function(.data_, .ref) {
      differentials_data <- .data_ %>%
        dplyr::mutate(dplyr::across(.fns = function(.x) {
          .x - .data_ %>%
            dplyr::pull({{.ref}})
        }))

      return(differentials_data)
    },

    ## Assign extra arguments/parameters in parent function:----
    #
    # .default_args_ # A list containing default arguments names and
    # their values.
    # .env_ # Environment object grabbed from the parent function's
    # environment to correctly assign arguments to that function.
    # .args_ # A list containing supplied/additional arguments names
    # and their values. Arguments in .default_args_ but existing in .args_
    # will be assigned values from .args_ and vice versa.
    #
    # This function assigns variables/objects in the parent's function
    # environment, hence it returns nothing.
    #
    # \dontrun{}
    assign_extraArgs_ = function(.default_args_, .env_, .args_) {
      # Grab default arguments' names:
      if(is.null(names(.default_args_)))
        stop(".default_args_ should contain named objects")
      if(length(names(.default_args_)) != length(.default_args_))
        stop("all arguments in .default_args_ should be named")
      expected_args_names <- names(.default_args_)
      # Grab additional arguments' names:
      supplied_args_names <- names(.args_)
      # Let the user know if any of the supplied arguments were unrecognised:
      if(any(!supplied_args_names %in% expected_args_names))
        message("Argument(s) [",
                paste(supplied_args_names[!supplied_args_names %in%
                                            expected_args_names]),
                "] is/are unknown, and therefore ignored")
      # Set additional arguments:
      purrr::walk(
        .x = expected_args_names,
        .f = function(.arg) {
          assign(.arg,
                 if(is.null(.args_[[.arg]])) {
                   .default_args_[[.arg]]
                 } else {
                   .args_[[.arg]]
                 }, envir = .env_)
        })
    },

    ## Compute the Expected Value of Perfect Partial Information (EVPPI):----
    #
    # @param .PSA_data A list of class shinyPSA that contains summary PSA
    # results.
    # @param .effs A matrix containing the \code{effects} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be summarised.
    # @param .costs A matrix containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be summarised.
    # @param .EVPI A data table containing EVPI data.
    # @param .WTPs A vector containing the Willingness-to-pay values over which
    # EVPI values were estimated.
    # @param .params A matrix containing parameters' configurations used in
    # the PSA.
    # @param .set A vector of parameters' names for conditional EVPPI.
    # @param .set_names A vector of parameter-names to be used for EVPPI.
    # @param .subset_ Boolean for whether to estimate conditional EVPPI for a
    # subset of parameters.
    # @param .MAICER_ The Maximum acceptable incremental cost-effectiveness
    # ratio (MAICER) to be considered in the summary table. Default value is
    # \code{30,000}.
    # @param .units_ A character, the units to associate with the
    # monitory values in the summary table. Default is sterling pounds
    # (GBP) \code{"\u00A3"}.
    # @param .individual_evppi_ Logical (default \code{TRUE}) to return per person
    # EVPPI, otherwise population EVPPI will be reported.
    # @param .discount_rate_ The discount rate used to discount future affected
    # populations.
    # @param .evppi_population_ The size of the population that is annually
    # affected by the competing health technologies under evaluation.
    # @param .time_horion_ The time expected to pass (in years) before the
    # interventions under consideration change (how long before the decision
    # under consideration become obsolete or requires updating).
    # @param .session Shiny app session.
    #
    # @return A list containing the EVPPI results table and caption information.
    #
    # @examples
    # \dontrun{
    # library(ShinyPSA)
    #
    # # Summarise PSA results:
    # PSA_summary = summarise_PSA_(
    #   .effs = ShinyPSA::Brennan_1K_PSA$e,
    #   .costs = ShinyPSA::Brennan_1K_PSA$c,
    #   .params = ShinyPSA::Brennan_1K_PSA$p,
    #   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #
    # # Estimate EVPPI:
    # EVPPI_ind_res <- compute_EVPPIs_(
    #     .PSA_data = PSA_summary,
    #     .MAICER_ = 30000)
    #
    # EVPPI_pop_res <- compute_EVPPIs_(
    #     .PSA_data = PSA_summary,
    #     .MAICER_ = 20000,
    #     .individual_evppi_ = FALSE,
    #     .evppi_population_ = 1000,
    #     .time_horion_ = 5)
    #
    # # Estimate Conditional EVPPI:
    # cEVPPI_ind_res <- compute_EVPPIs_(
    #     .PSA_data = PSA_summary,
    #     .set = c(6, 14, 15, 16),
    #     .subset_ = TRUE,
    #     .MAICER_ = 30000)
    #
    # cEVPPI_pop_res <- compute_EVPPIs_(
    #     .PSA_data = PSA_summary,
    #     .set_names = c("theta6", "theta14", "theta15", "theta16"),
    #     .subset_ = TRUE,
    #     .MAICER_ = 20000,
    #     .individual_evppi_ = FALSE,
    #     .evppi_population_ = 1000,
    #     .time_horion_ = 5)
    # }
    compute_EVPPIs_ = function(.PSA_data,
                               .effs = NULL, .costs = NULL,
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
      inb <- private$createInb( # Strong et al. function
        costs.int = .costs,
        effects.int = .effs,
        lambda = .MAICER_)
      ## Calculate per parameter or conditional EVPPI:----
      EVPPI <- if(!isTRUE(.subset_)) {
        private$applyCalcSingleParamGam( # Strong et al. function
          .params = .params,
          nb = inb,
          .session = .session)
      } else {
        t(
          as.matrix(
            unlist(
              private$calSubsetEvpi( # Strong et al. function
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
        private$compute_EVPIs_(
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
    },

    # The functions below were defined by Mark Strong, Penny Breeze, Chloe Thomas
    # and Alan Brennan, the authors of SAVI - Sheffield Accelerated Value of
    # Information. See [here](https://github.com/Sheffield-Accelerated-VoI/SAVI)!

    ### GAM functions:----

    # Calculating Incremental Net Benefits (INB)
    #
    # @param costs.int Costs data structure.
    # @param effects.int Effects data structure.
    # @param lambda Maximum Acceptable Incremental Cost-Effectiveness Ratio
    # (MAICER).
    #
    # @return
    #
    # @examples
    # \dontrun{
    # inb <- createInb(costs, effects, lambda)
    # }
    createInb = function(costs.int, effects.int, lambda) {

      inb <- as.matrix(effects.int) * lambda - as.matrix(costs.int)
      inb <- inb - inb[, 1]

      return(inb)
    },

    # Estimates EVPPI and SE via GAM
    #
    # @param .params A matrix containing parameters' configurations used in the
    # PSA.
    # @param NB Data structure containing Net Benefit (NB).
    # @param sets Column containing PSA samples of the parameter of interest.
    # @param s Number of simulations for the Monte Carlo computation of the SE.
    # @param .session Shiny session.
    #
    # @return
    #
    # @examples
    gamFunc = function(.params, NB, sets, s = 1000, .session = NULL) {

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
        rankifremoved <- sapply(1:NCOL(paramSet), function (x)
          qr(paramSet[,-x])$rank)
      }

      regression.model <- private$formulaGenerator(
        colnames(input.parameters)[sets])


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

        print(paste("estimating g.hat for incremental NB for option", d ,
                    "versus 1"))
        dependent <- NB[, d]
        f <- update(formula(dependent~.), formula(paste(".~", regression.model)))
        try_model <- try(model <- mgcv::gam(f,
                                            data = data.frame(input.parameters)))
        if (inherits(try_model, "try-error")) {
          regression.model <- private$formulaGenerator_s(
            colnames(input.parameters)[sets])
          f <- update(formula(dependent~.), formula(
            paste(".~", regression.model)))
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
    },

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
    formulaGenerator = function(namesList) {
      form <- paste(namesList, ",", sep = "", collapse = "")
      form <- substr(form, 1, nchar(form) - 1)
      if (length(namesList) == 4) {
        form <- paste("te(", form, ", k = 4)", sep = "") # restrict to 4 knots if 4 params
      } else {
        form <- paste("te(", form, ")", sep = "")
      }

      return(form)
    },

    # Generates the GAM model formulas from the list of parameter names
    #
    # @param namesList List of parameter names.
    #
    # @return
    #
    # \dontrun{
    # regression.model <- formulaGenerator_s(colnames(input.parameters)[sets])
    # }
    formulaGenerator_s = function(namesList) {
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
    },

    # Employ single GAM over supplied parameters
    #
    # @param .params A matrix containing parameters' configurations used in the
    # PSA.
    # @param nb Data structure containing Net Benefit (NB) or Incremental NB
    # (INB).
    # @param session Shiny session.
    #
    # @return
    #
    # @examples
    # \dontrun{
    # pEVPI <- applyCalcSingleParamGam(.params, inb, .session)
    # }
    applyCalcSingleParamGam = function(.params, nb, .session = NULL) {

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
        cat("evppi before gamFunc")

        result <- private$gamFunc(NB = nb, sets = i, s = 1000,
                                  .params = .params,
                                  .session = .session)

        res[i, ] <- unlist(result)
      }

      return(res)
    },

    ### GP functions:----
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
    dinvgamma = function(x, alpha, beta) {
      (beta ^ alpha) / gamma(alpha) * x ^ (-alpha - 1) * exp(-beta / x)
    },

    # Gaussian Correlation function?
    #
    # @param X
    # @param phi
    # @param m
    #
    # @return
    #
    # @examples
    cor.Gaussian = function(X, phi, m) {
      txbuild1 <- function(h) exp(-rowSums(t((t(X) - h) / phi) ^ 2))
      apply(as.matrix(as.matrix(X)[1:m, ]), 1, txbuild1)
    },

    # Make a matrix with the Gaussian correlation function
    #
    # @param X
    # @param phi
    #
    # @return
    #
    # @examples
    makeA.Gaussian = function(X, phi) {
      n <- NROW(X)
      if(length(phi) > 1) {
        b <- diag(phi ^ (-2))
      } else {
        b <- phi ^ (-2) }
      R <- X %*% as.matrix(b) %*% t(X)

      S <- matrix(diag(R), nrow = n, ncol = n)
      exp(2 * R - S - t(S))
    },

    # Calculate posterior density
    #
    # @param hyperparams
    # @param NB Net Benefits (NB) matrix.
    # @param input.m
    #
    # @return
    #
    # @examples
    post.density = function(hyperparams, NB, input.m) {

      input.m <- as.matrix(input.m, drop = FALSE)

      N <- nrow(input.m)
      p <- NCOL(input.m)
      H <- cbind(1, input.m)
      q <- ncol(H)

      a.sigma <- 0.001; b.sigma <- 0.001  ##  hyperparameters for IG prior for sigma^2
      a.nu <- 0.001; b.nu <- 1            ##  hyperparameters for IG prior for nu
      delta <- exp(hyperparams)[1:p]
      nu <- exp(hyperparams)[p + 1]

      A <- private$makeA.Gaussian(input.m, delta)
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
    },

    # Estimate Hyper-parameters
    #
    # @param NB Net Benefits (NB) matrix.
    # @param inputs
    # @param .session Shiny app session.
    #
    # @return
    #
    # @examples
    estimate.hyperparameters = function(NB, inputs, .session = NULL) {

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
                                       control=list(fnscale=-1, maxit=10000,
                                                    trace=0))$par
          if (sum(abs(initial.values - log.hyperparameters)) < 0.05) {
            hyperparameters[[d]] <- exp(log.hyperparameters)
            break
          }
          initial.values <- log.hyperparameters
        }
      }

      return(hyperparameters)
    },

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
    gpFunc = function(.params, NB, sets, s = 1000, .session = NULL) {

      input.parameters <- .params
      paramSet <- cbind(input.parameters[, sets])
      constantParams <- (apply(paramSet, 2, var) == 0)

      #remove constants
      if (sum(constantParams) == length(sets))
        return(list(EVPI=0, SE=0)) # if all regressors are constant
      if (sum(constantParams) > 0)
        sets <- sets[-which(constantParams)] # remove constants

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

      input.matrix <- as.matrix(
        input.parameters[1:maxSample, inputs.of.interest, drop=FALSE])
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
      hyperparameters <- private$estimate.hyperparameters(
        NB[setForHyperparamEst, ],
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
        print(paste("estimating g.hat for incremental NB for option", d,
                    "versus 1"))
        delta.hat <- hyperparameters[[d]][1:p]
        nu.hat <- hyperparameters[[d]][p+1]
        A <- private$makeA.Gaussian(input.matrix, delta.hat)
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
                                (H - AAstarinvH) %*%
                                (tHAHinv %*% t(H - AAstarinvH)))
        rm(A, Astarinv, AstarinvY, tHAstarinv, tHAHinv, betahat, Hbetahat,
           resid, sigmasqhat);gc()
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
        tilde.g[[d]] <- MASS::mvrnorm(s, g.hat[[d]][1:(min(N, 1000))],
                                      V[[d]][1:(min(N, 1000)),
                                             1:(min(N, 1000))])
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
    },

    ### Subset EVPPI:----
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
    # @param lambda Maximum Acceptable Incremental Cost-Effectiveness Ratio
    # (MAICER).
    # @param .session Shiny app session.
    #
    # @return
    #
    # @examples
    calSubsetEvpi = function(.nb, .effs, .costs, .params, sets,
                              .sets_names = NULL, lambda, .session = NULL) {
      # grab parameter-indexes from the parameters dataset:
      if(is.null(sets)) {
        sets <- which(colnames(.params) %in% .sets_names)
      }

      numParams <- length(sets) # number of parameters in the set
      # regressionFunction <- ifelse(numParams > 4, "private$gpFunc",
      #                              "private$gamFunc") # change gp to ppr
      f <- private$formulaGenerator(sets)

      # calculate incremental net benefit (INB):
      inb <- if(is.null(.nb)) {
        private$createInb( # Strong et al. function
          costs.int = .costs,
          effects.int = .effs,
          lambda = lambda)
      } else {
        .nb
      }

      # estimate conditional EVPPI:
      # output <- get(regressionFunction)(.params, inb, sets, s = 1000, .session)
      output <-
        if(numParams > 4) {
          private$gpFunc(.params, inb, sets, s = 1000, .session)
        } else {
          private$gamFunc(.params, inb, sets, s = 1000, .session)
        }

      return(output)
    },

    ## Plot the EVPPI results:----
    # Plot the Expected Value of Perfect Partial Information (EVPPI) results.
    #
    # @param EVPPI_res A list containing EVPPI results.
    # @param .show_percent Bolean for whether to show the percentage of overall
    # EVPI on the bars.
    # @param .min_percent Only parameters with percentage of overall EVPI equal to
    # or higher than .min_percent will make it to the plot.
    # @param .params_num The number of parameters to show in the bar plot.
    #
    # @return An Object of class ggplot2.
    #
    # @examples
    # \dontrun{
    # library(ShinyPSA)
    #
    # # Summarise PSA results:
    # PSA_summary = summarise_PSA_(
    #   .effs = ShinyPSA::Brennan_1K_PSA$e,
    #   .costs = ShinyPSA::Brennan_1K_PSA$c,
    #   .params = ShinyPSA::Brennan_1K_PSA$p,
    #   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #
    # # Estimate EVPPI:
    # EVPPI_ind_res <- compute_EVPPIs_(
    #   .PSA_data = PSA_summary,
    #   .MAICER_ = 30000)
    #
    # # Plot EVPPI results:
    # plot_EVPPI_(
    #   EVPPI_res = EVPPI_ind_res,
    #   .params_num = 15,
    #   .min_percent = NULL)
    # }
    plot_EVPPI_ = function(EVPPI_res, .show_percent = TRUE, .min_percent = 1,
                           .params_num = NULL) {
      # Prepare plot data:----
      EVPPI_data <- EVPPI_res[[1]] %>%
        ## subset data columns:----
      `colnames<-`(c("Parameters", "Per Person EVPPI", colnames(.)[-c(1,2)])) %>%
        ## create a numeric percentage EVPPI of overall EVPI:----
      dplyr::mutate(
        'percent' = as.numeric(
          gsub(pattern = '%',
               replacement = "",
               x = EVPPI_res[[1]][[4]]))) %>%
        ## rename exiting string percent variable to a usable name:----
      dplyr::rename("Percent overall EVPI" = "Indexed to Overall EVPI (%)") %>%
        ## sort EVPPI values in descending order:----
      dplyr::arrange(dplyr::desc(`Per Person EVPPI`)) %>%
        ## if a subset of EVPPI is requested:----
      {if(is.null(.params_num)) {
        .
      } else {
        if(is.numeric(.params_num)) {
          dplyr::slice_head(.data = ., n = .params_num)
        } else {
          .}}} %>%
        ## if a user wants :----
      {if(is.null(.min_percent)) {
        .
      } else {
        if(is.numeric(.min_percent)) {
          dplyr::filter(.data = ., percent >= .min_percent)
        } else {
          .}}}

      # Build EVPPI plot:----
      p <- ggplot2::ggplot(data = EVPPI_data) +
        ggplot2::coord_cartesian(xlim = c(0, NA)) +
        ## add bar plot:----
      ggplot2::geom_bar(
        ggplot2::aes(
          x = `Per Person EVPPI`,
          y = forcats::fct_reorder(Parameters, `Per Person EVPPI`)),
        fill = "#EFC00099", #"#EFC000FF"
        stat = "identity",
        position = ggplot2::position_dodge2()) +
        ## add axis lines:----
      ggplot2::geom_hline(
        yintercept = 0,
        color = 'grey',
        size = 0.1) +
        ggplot2::geom_vline(
          xintercept = 0,
          color = 'grey',
          size = 0.1) +
        ## fine tuning the plot:----
      ggplot2::scale_x_continuous(labels = scales::dollar_format(
        prefix = "\u00A3")) +
        ggplot2::theme(
          # axis.ticks.length.y = element_text(hjust = -2),
          plot.title.position = "plot", # Start title from near the margin
          ### Add a border and space around the plot:
          # panel.border = ggplot2::element_rect(colour = 'black', fill = NA),
          plot.margin = ggplot2::unit(c(5.5, 0.5, 5.5, 5.5), # more space LHS
                                      c("points", "cm", "points", "points"))) +
        # panel.grid = element_blank(),
        # panel.border = element_blank()) +
        ggplot2::labs(
          title = "Per Person EVPPI",
          caption = if(!is.null(.min_percent)) {
            if(is.numeric(.min_percent)) {
              paste0(EVPPI_res[['Plot caption']], "
                 Parameters with EVPPI less than ", .min_percent,
                     "% of overall EVPI are excluded from plot.")
            }
          } else {
            EVPPI_res[['Plot caption']]
          },
          x = "EVPPI (\u00A3)",
          y = "Parameters")

      # If user wants to see percentages on bars:
      if(isTRUE(.show_percent)) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              x = `Per Person EVPPI`,
              y = forcats::fct_reorder(Parameters, `Per Person EVPPI`),
              label = `Percent overall EVPI`),
            ## reduce label size with large number of params
            size = if(nrow(EVPPI_data) > 15) 2.5 else NA)
      }

      return(p)

    }

  )
)

