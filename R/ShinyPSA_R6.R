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
ShinyPSA <- R6::R6Class(
  # Object name:
  classname = "ShinyPSA",
  # Public elements:
  public = list(

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
    #' @field app a list to store shiny app elements
    app = NULL,

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
    #' @param .interventions A vector containing the names of all
    #' interventions. If not provided or less names than needed is
    #' provided, the function will generate generic names, for example
    #' \code{intervention 1}.
    #' @param .ref An integer indicating the index of the reference
    #' intervention. This parameter is ignored if more than two
    #' \code{interventions} are under analysis.
    #' @param .Kmax The maximum willingness-to-pay threshold to use in the
    #' analysis. This parameter is ignored if \code{wtp} is provided.
    #' @param .wtp A vector of numerical values declaring the
    #' willingness-to-pay (WTP) values to use in the analysis. If
    #' \code{NULL} (default) a range of WTP values (up to \code{.Kmax}
    #' will be used.
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
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #' }
    initialize = function(.effs, .costs, .interventions = NULL,
                          .ref = NULL, .Kmax = 100000, .wtp = NULL,
                          .plot = TRUE) {
      private$effects <- dplyr::as_tibble(.effs)
      private$costs <- dplyr::as_tibble(.costs)
      private$PSA_summary <- private$summarise_PSA_(
        .effs = .effs,
        .costs = .costs,
        .interventions = .interventions,
        .ref = .ref,
        .Kmax = .Kmax,
        .wtp = .wtp,
        .plot = .plot
      )

      invisible(self)

    },

    #' @description
    #' Get the default results summary table
    #'
    #' @param .wtp_ A numeric vector containing the willingness-to-pay
    #' value(s) to be considered in the summary table. Default values are
    #' \code{c(20,000, 30,000)}
    #' @param .units_ A character, the units to associate with the
    #' monitory values in the summary table. Default is sterling pounds
    #' (GBP) \code{£}.
    #' @param .effects_label The label or name to be given to the effects
    #' column in the summary table. Default is QALYs.
    #'
    #'
    #' @return A ggplot2 object
    #' @importFrom tidyselect vars_select_helpers
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Vaccine_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Vaccine_PSA$c),
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
    #'
    #' PSA_outputs$get_Summary_table()
    #' }
    get_Summary_table = function(.wtp_ = c(20000, 30000), .units_ = "£",
                                 .effects_label = "QALYs",
                                 .beautify_ = TRUE, .long_ = TRUE) {
      # Set currency label if none were provided:
      if(is.null(.units_) | length(.units_) != 1) .units_ = "£"
      # ICER:
      ICER_tbl <- private$PSA_summary[["ICER"]]
      # eNMB:
      eNMB <- private$PSA_summary[["e.NMB"]] %>%
        dplyr::mutate('WTP' = private$PSA_summary[["WTPs"]]) %>%
        dplyr::filter(WTP %in% .wtp_) %>%
        dplyr::mutate(WTP = paste0("NMB @ ",
                                   scales::dollar(
                                     x = WTP,
                                     accuracy = 1,
                                     prefix = .units_))) %>%
        tidyr::pivot_longer(
          cols = -WTP,
          names_to = 'intervention',
          values_to = 'NMB') %>%
        tidyr::pivot_wider(
          id_cols = 'intervention',
          names_from = 'WTP',
          values_from = 'NMB')
      # CEAF:
      CEAF <- dplyr::tibble(
        'CEAF - values' = private$PSA_summary[["CEAF"]]$ceaf,
        'CEAF - WTP' = private$PSA_summary[["WTPs"]],
        'intervention' = private$PSA_summary[["best_name"]]) %>%
        dplyr::filter(`CEAF - WTP` %in% .wtp_) %>%
        dplyr::mutate(`CEAF - WTP` = paste0("Prob. CE @ ",
                                            scales::dollar(
                                              x = `CEAF - WTP`,
                                              accuracy = 1,
                                              prefix = .units_)))

      # EVPI:
      EVPI <- dplyr::tibble(
        'EVPI - values' = private$PSA_summary[["EVPI"]],
        'EVPI - WTP' = private$PSA_summary[["WTPs"]],
        'intervention' = private$PSA_summary[["best_name"]]) %>%
        dplyr::filter(`EVPI - WTP` %in% .wtp_) %>%
        dplyr::mutate(`EVPI - WTP` = paste0("EVPI @ ",
                                            scales::dollar(
                                              x = `EVPI - WTP`,
                                              accuracy = 1,
                                              prefix = .units_)))

      # Summary table:
      differential_col <- paste("Incremental", .effects_label)
      Summary_tbl <- ICER_tbl %>%
        # join the expected NMB to the ICER results:
        dplyr::left_join(x = ., y = eNMB, by = 'intervention') %>%
        # join the probability of being cost-effective:
        dplyr::left_join(x = ., y = CEAF, by = 'intervention') %>%
        tidyr::pivot_wider(
          names_from = `CEAF - WTP`,
          values_from = `CEAF - values`) %>%
        # drop any NAs resulting from pivot_wider:
        dplyr::select(tidyselect::vars_select_helpers$where(
          fn = function(.x) !all(is.na(.x)))) %>%
        # join the EVPI:
        dplyr::left_join(x = ., y = EVPI, by = 'intervention') %>%
        tidyr::pivot_wider(
          names_from = `EVPI - WTP`,
          values_from = `EVPI - values`) %>%
        # drop any NAs resulting from pivot_wider:
        dplyr::select(tidyselect::vars_select_helpers$where(
          fn = function(.x) !all(is.na(.x)))) %>%
        # do some formatting:
        dplyr::mutate(
          dplyr::across(
            tidyselect::vars_select_helpers$where(
              is.numeric) & !c(qalys, delta.e,
                               dplyr::starts_with("Prob.")),
            ~ scales::dollar(
              x = .x,
              accuracy = 0.1,
              prefix = .units_))) %>%
        dplyr::mutate(
          dplyr::across(c(qalys, delta.e, dplyr::starts_with("Prob.")),
                        ~ as.character(round(.x, digits = 4)))) %>%
        dplyr::select(-dplyr::any_of('dominance')) %>%
        dplyr::rename({{.effects_label}} := qalys,
                      Comparators = intervention,
                      {{differential_col}} := delta.e,
                      "Incremental Costs" = delta.c,
                      "ICER information" = icer_label) %>%
        dplyr::rename_with(stringr::str_to_title, costs) %>%
        dplyr::rename_with(toupper, icer) %>%
        dplyr::mutate(
          ICER = case_when(
            is.na(ICER) ~
              `ICER information`,
            TRUE ~ ICER)) %>%
        dplyr::select(-`ICER information`)
      # Display the table in a long format:
      if(.long_) {
        # Reorder some columns for DT::RowGroup option:
        Summary_tbl <- Summary_tbl %>%
          dplyr::select(
            Costs, `Incremental Costs`, QALYs, `Incremental QALYs`,
            dplyr::everything()) %>%
          # Flip the dataset to have everything in long format:
          tidyr::pivot_longer(
            cols = -Comparators,
            names_to = " ",
            values_to = "Values") %>%
          # Flip the dataset to have the interventions in wide format:
          tidyr::pivot_wider(
            names_from = Comparators,
            values_from = Values)
      }
      # To get a nice looking table:
      ## Long format beautified table:
      if(.beautify_ & .long_) {
        # Remove unnecessary strings:
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
        # Prepare DT-table row groups:
        Summary_tbl <- Summary_tbl %>%
          dplyr::mutate(
            RowGroup_ = c(rep(glue::glue("Costs ({.units_})"), 2),
                          rep("QALYs", 2),
                          "Incremental Cost-Effectiveness Ratio",
                          rep(glue::glue("Net Benefit ({.units_})"),
                              length(.wtp_)),
                          rep("Probability Cost-Effective",
                              length(.wtp_)),
                          rep(glue::glue("Expected Value of Perfect
                                      Information ({.units_})"),
                              length(.wtp_))
            )
          )
        # Prepare border info:
        bottom_border_ <- c(0, 1, 0, 1, 1,
                            rep(0, length(.wtp_) - 1), 1,
                            rep(0, length(.wtp_) - 1), 1,
                            rep(0, length(.wtp_) - 1), 1)
        Summary_tbl <- Summary_tbl %>%
          dplyr::mutate(
            RowBorder_ = bottom_border_
          )
        # Number of columns to show:
        ColShow_ <- nrow(ICER_tbl)
        # Build the table:
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
              rowGroup = list(
                dataSrc = ColShow_ + 1
              ), # Column names at the end of the table
              columnDefs = list(
                # list(
                #   targets = '_all',
                #   className = 'dt-center'
                # ),
                list(
                  visible = FALSE,
                  targets = c(ColShow_ + 1, ColShow_ + 2)
                )
              ) # Hide the column names
            ),
            extensions = c('RowGroup', 'Buttons'),
            selection = 'none', ## enable selection of a single row
            filter = 'none', ## include column filters at the bottom
            rownames = FALSE  ## don't show row numbers/names
          ) %>%
          DT::formatStyle(
            columns = 0:(ColShow_ + 1),
            valueColumns = 'RowBorder_',
            `border-bottom` = DT::styleEqual(1, 'solid 1px')
          )
      }
      ## Wide format beautified table:
      if(.beautify_ & !.long_) {
        # custom table container to create column groups:
        sketch_ <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 2, 'Comparators'), # 1 column (name 2 rows)
              th(rowspan = 2, 'QALYs'), # 1 column (name 2 rows)
              th(rowspan = 2, 'Costs'), # 1 column (name 2 rows)
              th(colspan = 2, 'Incremental'),
              th(rowspan = 2, 'ICER'), # 1 column (name 2 rows)
              th(colspan = length(.wtp_), 'Net Benefit'),
              th(colspan = length(.wtp_), 'Probability cost-effective'),
              th(colspan = length(.wtp_), 'EVPI'),
            ),
            tr(
              purrr::map(.x = c(
                "QALYs", "Costs", # Incremental
                rep(
                  scales::dollar(# Net Benefit, CEAF, EVPI
                    x = .wtp_,
                    accuracy = 1,
                    prefix = .units_), 3)),
                .f = th)
            )
          )
        ))
        # Build the table:
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
                # list(
                #   targets = '_all',
                #   className = 'dt-center'
                # ),
              )
            ),
            container = sketch_, ## object to use to draw table
            extensions = 'Buttons',
            selection = 'none', ## enable selection of a single row
            filter = 'none', ## include column filters at the bottom
            rownames = FALSE  ## don't show row numbers/names
          )

      }

      ## Save a copy:
      private$Summary_table <- Summary_tbl

      return(private$Summary_table)
    },

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
    #'                   .interventions = ShinyPSA::Vaccine_PSA$treats)
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
    #'                   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #'                   .interventions = ShinyPSA::Smoking_PSA$treats)
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
    #'                   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #'                   .interventions = ShinyPSA::Smoking_PSA$treats)
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
    #'                   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #'                   .interventions = ShinyPSA::Smoking_PSA$treats)
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
    #'                   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #'                   .interventions = ShinyPSA::Smoking_PSA$treats)
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

    #' @description
    #' Get the willingness-to-pay values used in the analysis
    #'
    #' @return An integer
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' # Instantiate a copy of class ShinyPSA:
    #' PSA_outputs <- ShinyPSA$new(
    #'                   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
    #'                   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
    #'                   .interventions = ShinyPSA::Smoking_PSA$treats)
    #'
    #' PSA_outputs$get_WTP()
    #' }
    get_WTP = function() {

      return(private$PSA_summary[["WTPs"]])
    }

  ),

  # Private elements:
  private = list(

    effects = NULL,
    costs = NULL,
    PSA_summary = NULL,
    # Summary_table a summary table with differentials, ICER(S),
    # net benefits and probability being cost-effective.
    Summary_table = NULL,

    # Summarise PSA outputs and report results
    #
    # .effs A matrix containing the \code{effects} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .costs A matrix containing the \code{costs} from PSA. Number of
    # \code{columns} is equal to the interventions while the number of
    # \code{rows} is equal to the number of PSA simulations to be
    # summarised.
    # .interventions A vector containing the names of all
    # interventions. If not provided or less names than needed are
    # provided, the function will generate generic names, for example
    # \code{intervention 1}.
    # .ref An integer indicating the index of the reference
    # intervention. This parameter is ignored if more than two
    # \code{interventions} are under analysis.
    # .Kmax The maximum willingness-to-pay threshold to use in the
    # analysis. This parameter is ignored if \code{wtp} is provided.
    # .wtp A vector of numerical values declaring the
    # willingness-to-pay (WTP) values to use in the analysis. If
    # \code{NULL} (default) a range of WTP values (up to \code{.Kmax} will
    # be used.
    # .plot A boolean, FALSE (default), for whether to generate plots.
    #
    # A list of class \code{psa} with \code{24} elements.
    #
    # \dontrun{}
    summarise_PSA_ = function(.effs, .costs, .interventions = NULL,
                              .ref = NULL, .Kmax = 100000, .wtp = NULL,
                              .plot = FALSE) {

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

      # Associate .interventions with number IDs for cleaner plots' labels:
      .interventions <- paste0(1:length(.interventions),
                               ": ",
                               .interventions)

      # Set missing values or remove ones to be ignored:
      if(n.comparators == 2){
        # If no reference was provided in a non-incremental analysis:
        if(is.null(.ref)){
          .ref <- 1
          message(paste0("You did not select a reference intervention. [",
                         .interventions[.ref], "] will be used as reference for differential values and plots."))
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
      if (!is.null(.wtp)) {
        .wtp <- sort(unique(.wtp))
        .Kmax <- max(.wtp)
        v.k <- .wtp
        n.k <- length(.wtp)
        names(v.k) <- paste0("£", format(v.k, big.mark = ","))
      } else {
        n.points <- .Kmax/100
        v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
        v.k <- c(v.k, 20000, 30000, 50000)
        v.k <- sort(unique(v.k))
        n.k <- length(v.k)
        names(v.k) <- paste0("£", format(v.k, big.mark = ","))
      }

      # Ensure .effs and .costs are tibbles and name columns appropriately:
      .effs <- .effs %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        `colnames<-`(.interventions)
      .costs <- .costs %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        `colnames<-`(.interventions)

      # Compute effects and costs differentials:
      if(n.comparators == 2) {
        delta.effs <- private$calculate_differentials_(.data = .effs, .ref = .ref)
        delta.costs <- private$calculate_differentials_(.data = .costs, .ref = .ref)
      } else {
        delta.effs <- NULL
        delta.costs <- NULL
      }

      # Compute ICER(s):
      ICER <- private$compute_ICERs_(.icer_data = NULL, .effs = .effs, .costs = .costs,
                                     .interventions = .interventions)

      # Compute NMB or iNMB, e.NMB or e.iNMB and best option for each k:
      nmbs <- private$compute_NMBs_(.effs = .effs, .costs = .costs,
                                    .interventions = .interventions, .Kmax = .Kmax,
                                    .wtp = .wtp)
      NMB <- nmbs$nmb
      e.NMB <- nmbs$e.nmb
      best <- nmbs$best_interv
      best_name <- nmbs$best_interv_name
      check <- nmbs$check
      kstar <- nmbs$wtp_star

      # Compute CEAC:
      CEAC <- private$compute_CEACs_(.nmb = NMB)

      # Compute CEAF:
      CEAF <- private$compute_CEAFs_(.ceac = CEAC)

      # Compute EVPI:
      EVPIs <- private$compute_EVPIs_(.effs = .effs, .costs = .costs, .Kmax = .Kmax,
                                      .interventions = .interventions, .wtp = .wtp)
      U <- EVPIs$U
      Ustar <- EVPIs$Ustar
      ol <- EVPIs$ol
      vi <- EVPIs$vi
      EVPI <- EVPIs$evi

      ## Outputs of the function
      results <- list(

        interventions = .interventions, ref = .ref, comp = comp,
        ICER = ICER, NMB = NMB, e.NMB = e.NMB, CEAC = CEAC, CEAF = CEAF,
        EVPI = EVPI, best_id = best, best_name = best_name, WTPs = v.k,
        WTPstar = kstar, U = U, Ustar = Ustar, vi = dplyr::as_tibble, ol = ol, e = .effs,
        c = .costs, delta.e = delta.effs, delta.c = delta.costs, n.sim = n.sim,
        n.comparators = n.comparators, step = n.k, Kmax = .Kmax
      )

      class(results) <- "psa"

      # If requested, develop and save plots:
      if(.plot == TRUE) {
        # Cost-Effectiveness plane:
        CEP_plot <- private$plot_CEplane_(.PSA_data = results, .ref = .ref)
        CEAC_plot <- private$plot_CEAC_(.PSA_data = results, .ref = .ref)
        CEAF_plot <- private$plot_CEAF_(.PSA_data = results)
        EVPI_plot <- private$plot_EVPI_(.PSA_data = results)
        eNMB_plot <- private$plot_eNMB_(.PSA_data = results)
        results <- c(results,
                     'CEP_plot' = list(CEP_plot),
                     'CEAC_plot' = list(CEAC_plot),
                     'CEAF_plot' = list(CEAF_plot),
                     'EVPI_plot' = list(EVPI_plot),
                     'eNMB_plot' = list(eNMB_plot))
      }

      return(results)
    },

    # Identify dominated interventions
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
    identify_dominance_ = function(.icer_data, .qalys = qalys,
                                   .costs = costs) {
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

    # Identify extendedly dominated interventions
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

    # Calculate ICER(s) and effects and costs differentials
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
    calculate_ICERs_ = function(.icer_data, .qalys = qalys,
                                .costs = costs) {
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
            is.na(dominance) & !is.na(icer) ~ paste0("ICER = ",
                                                     scales::dollar(
                                                       x = icer,
                                                       accuracy = 0.1,
                                                       prefix = "£"),
                                                     "; vs ",
                                                     dplyr::lag(.id)),
            is.na(dominance) & is.na(icer) ~ dplyr::case_when(
              dplyr::n() > 1 ~ paste0("reference"),
              TRUE ~ icer_label),
            TRUE ~ icer_label)) %>%
        dplyr::ungroup()

      return(.icer_data)
    },

    # Identify, iteratively, all dominated interventions
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

    # Identify, iteratively, all extendedly dominated interventions
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

    # Compute ICER(s)
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
          # Associate .interventions with number IDs for cleaner plots' labels:
          .interventions <- paste0(1:length(.interventions),
                                   ": ",
                                   .interventions)
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

    # Compute Monetary Net-Benefit (NMB) or incremental NMB (iNMB)
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
    #
    # A list containing the NMB list, eNMB tibble, WTP tibble and
    # other objects.
    #
    # \dontrun{}
    compute_NMBs_ = function(.effs, .costs, .interventions = NULL,
                             .Kmax = NULL, .wtp = NULL) {
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
        # Associate .interventions with number IDs for cleaner plots' labels:
        .interventions <- paste0(1:length(.interventions),
                                 ": ",
                                 .interventions)
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
      if (!is.null(.wtp)) {
        .wtp <- sort(unique(.wtp))
        .Kmax <- max(.wtp)
        v.k <- .wtp
        n.k <- length(.wtp)
        names(v.k) <- paste0("£", format(v.k, big.mark = ","))
      } else {
        n.points <- .Kmax/100
        v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
        v.k <- c(v.k, 20000, 30000, 50000)
        v.k <- sort(unique(v.k))
        n.k <- length(v.k)
        names(v.k) <- paste0("£", format(v.k, big.mark = ","))
      }

      # Compute monetary net benefit (NMB) (default):
      nmb <- purrr::map2(.x = .effs,
                         .y = .costs,
                         .f = function(.eff = .x, .cost = .y) {
                           purrr::map_dfc(as.list(v.k),
                                          .f = function(.k = .x) {
                                            .eff * .k - .cost})}) %>%
        purrr::transpose()

      # Compute expected net benefit (e.NMB):
      e.nmb <- nmb %>%
        purrr::map_dfr(.f = function(.x) {
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

    # Compute Cost-Effectiveness Acceptability Curve (CEAC)
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
        purrr::map_dfr(.f = function(.x) {
          colMeans(do.call(pmax, dplyr::as_tibble(.x, .name_repair = "unique")) ==
                     dplyr::as_tibble(.x, .name_repair = "unique"))})

      return(ceac)
    },

    # Compute Cost-Effectiveness Acceptability Frontier
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
        dplyr::mutate('ceaf' = if(any(rowSums(.) != 1)) NA_real_
                      else do.call(pmax, .))

      return(ceaf)
    },

    # Compute the Expected Value of Perfect Information (EVPI)
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
    #
    # A list containing the EVPI vector, value of information tibble,
    # opportunity lost tibble among others
    #
    # \dontrun{}
    compute_EVPIs_ = function(.effs, .costs, .interventions = NULL,
                              .Kmax = NULL, .wtp = NULL) {
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
        # Associate .interventions with number IDs for cleaner plots' labels:
        .interventions <- paste0(1:length(.interventions),
                                 ": ",
                                 .interventions)
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
      if (!is.null(.wtp)) {
        .wtp <- sort(unique(.wtp))
        .Kmax <- max(.wtp)
        v.k <- .wtp
        n.k <- length(.wtp)
        names(v.k) <- paste0("£", format(v.k, big.mark = ","))
      } else {
        n.points <- .Kmax/100
        v.k <- seq(from = 0, to = .Kmax, length.out = n.points + 1)
        v.k <- c(v.k, 20000, 30000, 50000)
        v.k <- sort(unique(v.k))
        n.k <- length(v.k)
        names(v.k) <- paste0("£", format(v.k, big.mark = ","))
      }

      # Compute monetary net benefit (NMB) (default):
      nmb <- purrr::map2(.x = .effs, .y = .costs,
                         .f = function(.eff = .x, .cost = .y) {
                           purrr::map_dfc(as.list(v.k),
                                          .f = function(.k = .x) {
                                            .eff * .k - .cost})}) %>%
        purrr::transpose()

      # Compute expected net benefit (e.NMB):
      e.nmb <- nmb %>%
        purrr::map_dfr(.f = function(.x) {
          colMeans(dplyr::as_tibble(.x, .name_repair = "unique"))
        })

      # Identify the best option for each willingness-to-pay value:
      best_interv <- e.nmb %>%
        max.col(ties.method = "first")

      # Extract maximum nmb value at each iteration for each wtp/threshold:
      max_nmb_iter <- nmb %>%
        purrr::map_dfr(.f = function(.x) {
          do.call(pmax, dplyr::as_tibble(.x, .name_repair = "unique"))
        })

      # Compute opportunity loss (OL):
      ol <- purrr::pmap_dfc(.l = list(nmb, best_interv, max_nmb_iter),
                            .f = function(.x, .y, .z) {
                              .z - .x[[.y]]
                            })

      # Compute value-of-information (VI):
      vi <- purrr::map2_dfc(.x = max_nmb_iter, .y = nmb,
                            .f = function(.x, .y) {
                              .x - max(colMeans(dplyr::as_tibble(.y, .name_repair = "unique")))
                            })

      # Compute expected value-of-information (EVPI):
      evi <- colMeans(ol)

      return(list(U = nmb, Ustar = max_nmb_iter, vi = vi, ol = ol, evi = evi))
    },

    # Plot Cost Effectiveness Acceptability Curve (CEAC)
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
        ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
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
          x = "Willingness-to-pay (£)",
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
              accuracy = 1,
              prefix = "£"),
            lty_ = "Willingness-to-pay (£)")

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

    # Plot Cost Effectiveness Acceptability Frontier (CEAF)
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
        ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
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
          x = "Willingness-to-pay (£)",
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
              accuracy = 0.1,
              prefix = "£"),
            lty_ = "Willingness-to-pay (£)")

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

    # Plot Cost Effectiveness Plane (CEP).
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
      # Get the function's environment for correct assignment in assign():
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
      private$assign_extraArgs_(.default_args_ = default_args,
                                .args_ = args_,
                                .env_ = env_)

      # Plot data:
      ## CE plot points:
      if(is.null(.ref)) { # No rescaling of point data
        ce_plane_dt <- .PSA_data$e %>%
          dplyr::mutate(sims = dplyr::row_number()) %>%
          tidyr::pivot_longer(cols = -sims,
                              names_to = "interventions",
                              values_to = "Effects") %>%
          dplyr::left_join(x = .,
                           y = .PSA_data$c %>%
                             dplyr::mutate(sims = dplyr::row_number()) %>%
                             tidyr::pivot_longer(cols = -sims,
                                                 names_to = "interventions",
                                                 values_to = "Costs"),
                           by = c("sims", "interventions"))
        # Labels:
        .title_lab = "Cost Effectiveness Plane"
        .x_lab = "Effects"
        .y_lab = "Costs (£)"
      } else { # Rescale point data
        ce_plane_dt <- .PSA_data$e %>%
          private$calculate_differentials_(.ref = .ref) %>%
          dplyr::mutate(sims = dplyr::row_number()) %>%
          tidyr::pivot_longer(cols = -sims,
                              names_to = "interventions",
                              values_to = "Effects") %>%
          dplyr::left_join(x = .,
                           y = .PSA_data$c %>%
                             private$calculate_differentials_(.ref = .ref) %>%
                             dplyr::mutate(sims = dplyr::row_number()) %>%
                             tidyr::pivot_longer(cols = -sims,
                                                 names_to = "interventions",
                                                 values_to = "Costs"),
                           by = c("sims", "interventions"))
        # Labels:
        .title_lab = "Cost Effectiveness Plane"
        .x_lab = "Effectiveness differential"
        .y_lab = "Cost differential (£)"
      }

      ## CE plot mean values:
      ce_plane_mean_dt <- ce_plane_dt %>%
        dplyr::group_by(interventions) %>%
        dplyr::summarise(
          Effects = mean(Effects),
          Costs = mean(Costs))

      # Plot:
      p <- ggplot2::ggplot() +
        ggplot2::geom_hline(
          yintercept = 0, colour = "dark gray") +
        ggplot2::geom_vline(
          xintercept = 0, colour = "dark gray") +
        ggplot2::geom_point(
          data = ce_plane_dt,
          ggplot2::aes(x = Effects,
                       y = Costs,
                       color = interventions),
          size = 1, alpha = 0.5) +
        ggplot2::scale_y_continuous(
          labels = scales::dollar_format(prefix = "£",
                                         big.mark = ",")) +
        ggplot2::geom_point(
          data = ce_plane_mean_dt,
          ggplot2::aes(x = Effects,
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
          # Remove the fill colour in shape 21, generalising it to all
          # options:
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
            ggplot2::aes(x = Effects,
                         y = Costs,
                         label = .PSA_data$ICER$icer_label),
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
              accuracy = 1,
              prefix = "£"),
            lty_ = "Willingness-to-pay (£)")

        ## Plot:
        p <- p +
          ggplot2::geom_abline(
            data = .wtp,
            ggplot2::aes(intercept = 0,
                         slope = value,
                         linetype = lty_),
            show.legend = TRUE) +
          ggplot2::scale_linetype_manual(
            breaks = .wtp$lty_[1], # keep one for the legend
            values = rep(3, nrow(.wtp))) +
          ggrepel::geom_text_repel(
            data = .wtp,
            ggplot2::aes(x = x_cord,
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

    # Plot the Expected Value of Perfect Information (EVPI)
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
      private$assign_extraArgs_(.default_args_ = default_args,
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
      evpi_df <- dplyr::tibble('EVPI' = .PSA_data$EVPI * discounted_population,
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
          ggplot2::aes(x = `WTP threshold`,
                       y = EVPI),
          size = 0.4) +
        ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
        ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
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
          x = "Willingness-to-pay (£)",
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
              accuracy = 1,
              prefix = "£"),
            lty_ = "Willingness-to-pay (£)")

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

      return(p)
    },

    # Plot Expected Net Monetary Benefit (eNMB)
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
      private$assign_extraArgs_(.default_args_ = default_args,
                                .args_ = args_,
                                .env_ = env_)

      # Plot data:
      enmb_df <- .PSA_data$e.NMB %>%
        dplyr::as_tibble() %>%
        dplyr::mutate('WTP threshold' = .PSA_data$WTPs,
                      'Best option' = .PSA_data$best_name) %>%
        tidyr::pivot_longer(cols = colnames(.PSA_data$e.NMB),
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
          ggplot2::aes(x = `WTP threshold`,
                       y = eNMB,
                       group = Option,
                       linetype = Option,
                       color = Option),
          size = 0.4) +
        ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
        ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
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
          x = "Willingness-to-pay (£)",
          y = "Expected Net Monetary Benefit (£)")


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
              accuracy = 1,
              prefix = "£"),
            lty_ = "Willingness-to-pay (£)")

        ## Plot:
        p <- p +
          ggplot2::geom_vline(
            data = .wtp,
            ggplot2::aes(xintercept = x_cord,
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
            ggplot2::aes(x = x_cord,
                         y = y_cord,
                         angle = angle_cord,
                         label = label_cord),
            size = 1.5,
            show.legend = FALSE)
      }

      return(p)
    },

    # Check and add any missing columns expected by ICER computation
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
                  purrr::map_dfc(.f = function(.x) {
                    .x = ifelse(.x %in% .numerics, NA_real_, NA_character_)
                  }))
      }
      .x <- .x %>%
        dplyr::select(".id", dplyr::everything()) %>%
        dplyr::mutate(.id = dplyr::row_number())

      return(.x)
    },

    # Calculate differential costs and QALYs
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

    # Assign extra arguments/parameters in parent function
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
    }

  )
)

