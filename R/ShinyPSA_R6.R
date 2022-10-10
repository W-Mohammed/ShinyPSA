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
#'
#' PSA_outputs <- ShinyPSA$new(
#'                   .effs = as_tibble(ShinyPSA::Brennan_10K_PSA$e),
#'                   .costs = as_tibble(ShinyPSA::Brennan_10K_PSA$c),
#'                   .params = as_tibble(ShinyPSA::Brennan_10K_PSA$p),
#'                   .interventions = ShinyPSA::Brennan_10K_PSA$treats,
#'                   .evppi = FALSE,
#'                   .plot = FALSE)
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
    #' @param .params A matrix containing parameters' configurations used
    #' in the PSA. The Number of \code{rows} is equal to the number of PSA
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
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
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
    #' of the table is to be returned. If \code{FALSE}, a wide version of
    #' the table will be returned
    #'
    #' @return A table of class DT
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_Summary_table()
    #' }
    get_Summary_table = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$Summary_table <-
        if(length(dots_) == 0 &
           !is.null(private$PSA_summary[["Summary_table"]])) {
          # return default object if no arguments were passed to the function:
          private$PSA_summary[["Summary_table"]]
        } else {
          if(is.null(private$PSA_summary[["Summary_table"]])) {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            private$PSA_summary[["Summary_table"]] <-
              do.call(private$draw_summary_table_, dots_)
            # return default object after creation:
            private$PSA_summary[["Summary_table"]]
          } else {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            do.call(private$draw_summary_table_, dots_)
          }
        }

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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
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
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$CEP_plot <-
        if(length(dots_) == 0 &
           !is.null(private$PSA_summary[["CEP_plot"]])) {
          # return default object if no arguments were passed to the function:
          private$PSA_summary[["CEP_plot"]]
        } else {
          if(is.null(private$PSA_summary[["CEP_plot"]])) {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            private$PSA_summary[["CEP_plot"]] <-
              do.call(private$plot_CEplane_, dots_)
            # return default object after creation:
            private$PSA_summary[["CEP_plot"]]
          } else {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            do.call(private$plot_CEplane_, dots_)
          }
        }

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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_CEAC()
    #' }
    get_CEAC = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$CEAC_plot <-
        if(length(dots_) == 0 &
           !is.null(private$PSA_summary[["CEAC_plot"]])) {
          # return default object if no arguments were passed to the function:
          private$PSA_summary[["CEAC_plot"]]
        } else {
          if(is.null(private$PSA_summary[["CEAC_plot"]])) {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            private$PSA_summary[["CEAC_plot"]] <-
              do.call(private$plot_CEAC_, dots_)
            # return default object after creation:
            private$PSA_summary[["CEAC_plot"]]
          } else {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            do.call(private$plot_CEAC_, dots_)
          }
        }

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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_CEAF()
    #' }
    get_CEAF = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$CEAF_plot <-
        if(length(dots_) == 0 &
           !is.null(private$PSA_summary[["CEAF_plot"]])) {
          # return default object if no arguments were passed to the function:
          private$PSA_summary[["CEAF_plot"]]
        } else {
          if(is.null(private$PSA_summary[["CEAF_plot"]])) {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            private$PSA_summary[["CEAF_plot"]] <-
              do.call(private$plot_CEAF_, dots_)
            # return default object after creation:
            private$PSA_summary[["CEAF_plot"]]
          } else {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            do.call(private$plot_CEAF_, dots_)
          }
        }

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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_eNMB()
    #' }
    get_eNMB = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$eNMB_plot <-
        if(length(dots_) == 0 &
           !is.null(private$PSA_summary[["eNMB_plot"]])) {
          # return default object if no arguments were passed to the function:
          private$PSA_summary[["eNMB_plot"]]
        } else {
          if(is.null(private$PSA_summary[["eNMB_plot"]])) {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            private$PSA_summary[["eNMB_plot"]] <-
              do.call(private$plot_eNMB_, dots_)
            # return default object after creation:
            private$PSA_summary[["eNMB_plot"]]
          } else {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            do.call(private$plot_eNMB_, dots_)
          }
        }

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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_EVPI()
    #' }
    get_EVPI = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$EVPI_plot <-
        if(length(dots_) == 0 &
           !is.null(private$PSA_summary[["EVPI_plot"]])) {
          # return default object if no arguments were passed to the function:
          private$PSA_summary[["EVPI_plot"]]
        } else {
          if(is.null(private$PSA_summary[["EVPI_plot"]])) {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            private$PSA_summary[["EVPI_plot"]] <-
              do.call(private$plot_EVPI_, dots_)
            # return default object after creation:
            private$PSA_summary[["EVPI_plot"]]
          } else {
            # pass arguments to the relevant function:
            dots_$.PSA_data <- private$PSA_summary
            do.call(private$plot_EVPI_, dots_)
          }
        }

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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_EVPPI_results()
    #' }
    get_EVPPI_results = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)

      # check if current subset was not processed before:
      settings_ <- dplyr::tibble(
        "MAICER" = if(is.null(dots_[[".MAICER_"]])) {
          30000
        } else {
          dots_[[".MAICER_"]]},
        "Population EVPPI" = if(is.null(dots_[[".individual_evppi_"]])) {
          dots_[[".individual_evppi_"]] <- TRUE
          FALSE
        } else {
          !isTRUE(dots_[[".individual_evppi_"]])},
        "Population" = if(is.null(dots_[[".evppi_population_"]])) {
          NA
        } else {
          if(isTRUE(dots_[[".individual_evppi_"]])) {
            NA
          } else if(dots_[[".individual_evppi_"]] == FALSE) {
            dots_[[".evppi_population_"]]
          } else {
            NA}},
        "Discount rate" = if(is.null(dots_[[".discount_rate_"]])) {
          0.035
        } else {
          dots_[[".discount_rate_"]]},
        "Time horizon" = if(is.null(dots_[[".time_horion_"]])) {
          NA
        } else {
          if(isTRUE(dots_[[".individual_evppi_"]])) {
            NA
          } else if(dots_[[".individual_evppi_"]] == FALSE) {
            dots_[[".time_horion_"]]
          } else {
            NA}})

      EVPPI_exists <- if(is.null(private$EVPPI_settings)) {
        FALSE
      } else {
        # compare settings:
        compare_settings <- dplyr::semi_join(
          private$EVPPI_settings,
          settings_)

        if(nrow(compare_settings) != 0) {
          # if passed settings are not unique:
          settings_id <- compare_settings$id
          TRUE
        } else {
          FALSE
        }
      }
      # save of all permutations of passed parameter-subset for future calls:
      if(!isTRUE(EVPPI_exists)) {
        # add new settings to previously processed ones:
        settings_ <- settings_ %>%
          dplyr::mutate(
            "id" = if(is.null(private$EVPPI_settings)) {
              1
            } else {
              max(private$EVPPI_settings$id) + 1
            }
          )
        private$EVPPI_settings <- private$EVPPI_settings %>%
          dplyr::bind_rows(settings_)
      }

      # depending on passed arguments:
      self$EVPPI_results <-
        if(!isTRUE(EVPPI_exists)) {
          # new settings passed to functions:
          dots_$.PSA_data <- private$PSA_summary
          # save new settings output to a tmp object:
          tmp <- do.call(private$compute_EVPPIs_, dots_)
          # add tmp object to private object:
          private$PSA_summary[['EVPPI']] <-
            c(private$PSA_summary[['EVPPI']],
              list(tmp))
          # return tmp object:
          tmp
        } else {
          print("These EVPPI settings were processed before.")
          private$PSA_summary[['EVPPI']][[settings_id]]
        }

      return(self$EVPPI_results)
    },

    ## Get the Subset EVPPI results table:----
    #' @description
    #' Get the Subset EVPPI results table
    #'
    #' @param ... Extra arguments passed to the plotting functions.
    #' Should at least contain either:
    #' \code{.set}: Parameters' indexes.
    #' \code{.set_names}: Parameters' names.
    #'
    #' @return A list containing Subset EVPPI results
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_Sub_EVPPI_results(
    #'   .set = c(14, 16, 5))
    #' PSA_outputs$get_Sub_EVPPI_results(
    #'   .set_names = c("theta5", "theta14", "theta16"))
    #' }
    get_Sub_EVPPI_results = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # sanity checks:
      if(is.null(dots_[['.set_names']]) & is.null(dots_[['.set']])) {
        print(
          paste("Unable to estimate EVPPI for a subset of parameters.",
                "Please pass parameters indexes via the argument '.set'",
                "or parameters names via the argument '.set_names'."))
      }

      if(is.null(dots_[['.set']]) & !is.null(dots_[['.set_names']])) {
        stopifnot(
          "One or more of the passed parameters' names are incorrect." =
            all(dots_[['.set_names']] %in% colnames(private$params)),
          "Please pass characters (Parameters' names)" =
            is.character(dots_[['.set_names']]))

        dots_[['.set']] <- which(
          colnames(private$params) %in% dots_[['.set_names']])
      }

      stopifnot(
        'Please pass positive intergers to the ".set" argument.' =
          is.numeric(dots_[['.set']]),
        'One or more parameter index are out-of-bound.' =
          max(dots_[['.set']]) <= length(colnames(private$params)),
        'One or more parameter index are out-of-bound.' =
          min(dots_[['.set']]) >= 1)

      # keep unique params indexes:
      dots_[['.set']] <- unique(dots_[['.set']])

      # check if current subset was not processed before:
      params_set <- dplyr::tibble(
        "combis" = paste0(dots_[['.set']], collapse = '_'),
        "MAICER" = if(is.null(dots_[[".MAICER_"]])) {
          30000
        } else {
          dots_[[".MAICER_"]]},
        "Population EVPPI" = if(is.null(dots_[[".individual_evppi_"]])) {
          dots_[[".individual_evppi_"]] <- TRUE
          FALSE
        } else {
          !isTRUE(dots_[[".individual_evppi_"]])},
        "Population" = if(is.null(dots_[[".evppi_population_"]])) {
          NA
        } else {
          if(isTRUE(dots_[[".individual_evppi_"]])) {
            NA
          } else if(dots_[[".individual_evppi_"]] == FALSE) {
            dots_[[".evppi_population_"]]
          } else {
            NA}},
        "Discount rate" = if(is.null(dots_[[".discount_rate_"]])) {
          0.035
        } else {
          dots_[[".discount_rate_"]]},
        "Time horizon" = if(is.null(dots_[[".time_horion_"]])) {
          NA
        } else {
          if(isTRUE(dots_[[".individual_evppi_"]])) {
            NA
          } else if(dots_[[".individual_evppi_"]] == FALSE) {
            dots_[[".time_horion_"]]
          } else {
            NA}})

      EVPPI_subset_exists <- if(is.null(private$EVPPI_subsets)) {
         FALSE
      } else {
        # create a comparison dataset and keep unique rows:
        compare_sets <- dplyr::semi_join(
          private$EVPPI_subsets,
          params_set)

        if(nrow(compare_sets) != 0) {
          # if new parameters set and settings is not unique:
          TRUE
        } else {
          FALSE
        }
      }
      # save of all permutations of passed parameter-subset for future calls:
      if(!isTRUE(EVPPI_subset_exists)) {
        # get the list of all permutations of parameters subset defined by user:
        params_perm <- combinat::permn(dots_[['.set']])
        # convert the permutations list to a matrix:
        params_perm <- do.call(rbind, params_perm)
        # take the unique permutations:
        params_perm <- unique(params_perm)
        # collapse permutations to help compare with existing/passed subsets:
        params_perm <- params_perm %>%
          dplyr::as_tibble(
            .name_repair = ~ make.names(., unique = TRUE)) %>%
          tidyr::unite("combis", everything(), sep = "_") %>%
          dplyr::mutate(
            "MAICER" = if(is.null(dots_[[".MAICER_"]])) {
              30000
            } else {
              dots_[[".MAICER_"]]},
            "Population EVPPI" = if(is.null(dots_[[".individual_evppi_"]])) {
              dots_[[".individual_evppi_"]] <- TRUE
              FALSE
            } else {
              !isTRUE(dots_[[".individual_evppi_"]])},
            "Population" = if(is.null(dots_[[".evppi_population_"]])) {
              NA
            } else {
              if(isTRUE(dots_[[".individual_evppi_"]])) {
                NA
              } else if(dots_[[".individual_evppi_"]] == FALSE) {
                dots_[[".evppi_population_"]]
              } else {
                NA}},
            "Discount rate" = if(is.null(dots_[[".discount_rate_"]])) {
              0.035
            } else {
              dots_[[".discount_rate_"]]},
            "Time horizon" = if(is.null(dots_[[".time_horion_"]])) {
              NA
            } else {
              if(isTRUE(dots_[[".individual_evppi_"]])) {
                NA
              } else if(dots_[[".individual_evppi_"]] == FALSE) {
                dots_[[".time_horion_"]]
              } else {
                NA}},
            "id" = if(is.null(private$EVPPI_settings)) {
              1
            } else {
              max(private$EVPPI_settings$id) + 1})
        # add new permutations to previously processed ones:
        private$EVPPI_subsets <- private$EVPPI_subsets %>%
          dplyr::bind_rows(params_perm)
      }

      # hard-code key arguments:
      dots_$.PSA_data <- private$PSA_summary
      dots_$.subset_ <- TRUE

      # compute EVPPI for the parameters subsets:
      self$EVPPI_Sub_results <- if(!isTRUE(EVPPI_subset_exists)) {
        print("Processing a new EVPPI parameter-subset")
        if(!is.null(self$EVPPI_Sub_results)) {
          self$EVPPI_Sub_results %>%
            dplyr::bind_rows(
              do.call(private$compute_EVPPIs_, dots_))
        } else {
          do.call(private$compute_EVPPIs_, dots_)
        }
      } else {
        print(
          "This parameter-subset (or its permutation) was processed before.")
        self$EVPPI_Sub_results
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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_EVPPI_plot()
    #' }
    get_EVPPI_plot = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$EVPPI_plot <-
        if(is.null(self$EVPPI_results)) {
          # create EVPPIs results object if it does not exist:
          self$get_EVPPI_results()
          # pass arguments to the plotting function:
          dots_$EVPPI_res <- self$EVPPI_results
          private$PSA_summary[["EVPPI_plot"]] <-
            do.call(private$plot_EVPPI_, dots_)
          # return default object after creation:
          private$PSA_summary[["EVPPI_plot"]]
        } else {
          # pass arguments to the plotting function:
          dots_$EVPPI_res <- self$EVPPI_results
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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_PSA_stabl_plots()
    #' }
    get_PSA_stabl_plots = function(...) {
      # grab passed arguments, if any:
      dots_ <- list(...)
      # depending on passed arguments:
      self$stability_plots <-
        if(length(dots_) == 0 &
           !is.null(private$PSA_summary[["Stability_plots"]])) {
        # return default object if no arguments were passed to the function:
        private$PSA_summary[["Stability_plots"]]
      } else {
        if(is.null(private$PSA_summary[["Stability_plots"]])) {
          # pass arguments to the drawing function:
          dots_$.PSA_data <- private$PSA_summary
          # create default object as it does not exist:
          private$PSA_summary[["Stability_plots"]] <-
            do.call(private$check_PSA_stability, dots_)
          # return default object after creation:
          private$PSA_summary[["Stability_plots"]]
        } else {
          # pass arguments to the drawing function:
          dots_$.PSA_data <- private$PSA_summary
          do.call(private$check_PSA_stability, dots_)
        }
      }

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
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_WTP()
    #' }
    get_WTP = function() {

      return(private$PSA_summary[["WTPs"]])
    },

    ## Get parameters names:----
    #' @description
    #' Get the parameters' names.
    #'
    #' @return A character vector.
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #'                   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p)
    #'                   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
    #'
    #' PSA_outputs$get_params_names()
    #' }
    get_params_names = function() {

      return(colnames(private$params))
    }

  ),

  # Private elements:----
  private = list(

    effects = NULL,
    costs = NULL,
    params = NULL,
    PSA_summary = NULL,
    EVPPI_settings = NULL,
    EVPPI_subsets = NULL,

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
      # pass arguments through to the package function:
      return(
        ShinyPSA::summarise_PSA_(
          .effs = .effs,
          .costs = .costs,
          .params = .params,
          .interventions = .interventions,
          .ref = .ref,
          .Kmax = .Kmax,
          .wtp = .wtp,
          .max_Kpoints = .max_Kpoints,
          .lambda = .lambda,
          .evppi = .evppi,
          .plot = .plot)
      )
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
    # @param .effs_accuracy_ Number of digits for effects measure; default is 3
    # and is expressed as 1e-3 or 0.001.
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
                                   .time_horion_ = NULL,
                                   .effs_accuracy_ = 1e-3) {
      # pass arguments through to the package function:
      return(
        ShinyPSA::draw_summary_table_(
          .PSA_data = .PSA_data,
          .wtp_ = .wtp_,
          .units_ = .units_,
          .effects_label_ = .effects_label_,
          .beautify_ = .beautify_,
          .long_ = .long_,
          .individual_evpi_ = .individual_evpi_,
          .evpi_population_ = .evpi_population_,
          .discount_rate_ = .discount_rate_,
          .time_horion_ = .time_horion_,
          .effs_accuracy_ = .effs_accuracy_)
      )
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
      # pass arguments through to the package function:
      return(
        ShinyPSA::plot_CEAC_(
          .PSA_data = .PSA_data,
          ... = ...)
      )
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
      # pass arguments through to the package function:
      return(
        ShinyPSA::plot_CEAF_(
          .PSA_data = .PSA_data,
          ... = ...)
      )
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
    #   .effs = as_tibble(ShinyPSA::Brennan_1K_PSA$e),
    #   .costs = as_tibble(ShinyPSA::Brennan_1K_PSA$c),
    #   .params = as_tibble(ShinyPSA::Brennan_1K_PSA$p),
    #   .interventions = ShinyPSA::Brennan_1K_PSA$treats)
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
      # pass arguments through to the package function:
      return(
        ShinyPSA::plot_CEplane_(
          .PSA_data = .PSA_data,
          ... = ...)
      )
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
      # pass arguments through to the package function:
      return(
        ShinyPSA::plot_EVPI_(
          .PSA_data = .PSA_data,
          ... = ...)
      )
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
      # pass arguments through to the package function:
      return(
        ShinyPSA::plot_eNMB_(
          .PSA_data = .PSA_data,
          ... = ...)
      )
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
      # pass arguments through to the package function:
      return(
        ShinyPSA::check_PSA_stability(
          .PSA_data = .PSA_data,
          .effs = NULL,
          .costs = NULL,
          .interventions = NULL,
          ... = ...)
      )
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
    compute_EVPPIs_ = function(.PSA_data = private$PSA_summary,
                               .effs = NULL, .costs = NULL,
                               .EVPI = NULL, .WTPs = NULL, .params = NULL,
                               .set = NULL, .set_names = NULL,
                               .subset_ = FALSE, .MAICER_ = 30000,
                               .units_ = "\u00A3",
                               .individual_evppi_ = TRUE,
                               .discount_rate_ = 0.035,
                               .evppi_population_ = NULL,
                               .time_horion_ = NULL,
                               .session = NULL) {
      # pass arguments through to the package function:
      return(
        ShinyPSA::compute_EVPPIs_(
          .PSA_data = .PSA_data,
          .effs = .effs,
          .costs = .costs,
          .EVPI = .EVPI,
          .WTPs = .WTPs,
          .params = .params,
          .set = .set,
          .set_names = .set_names,
          .subset_ = .subset_,
          .MAICER_ = .MAICER_,
          .units_ = .units_,
          .individual_evppi_ = .individual_evppi_,
          .discount_rate_ = .discount_rate_,
          .evppi_population_ = .evppi_population_,
          .time_horion_ = .time_horion_,
          .session = .session)
      )
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
      # pass arguments through to the package function:
      return(
        ShinyPSA::plot_EVPPI_(
          EVPPI_res = EVPPI_res,
          .show_percent = .show_percent,
          .min_percent = .min_percent,
          .params_num = .params_num
        )
      )
    }
  )
)

