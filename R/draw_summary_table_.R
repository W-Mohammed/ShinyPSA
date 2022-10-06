################################################################################
#
# Script Name:        draw_summary_table_.R
# Module Name:        Economic/PSA
# Script Description: Defines the function that creates a table that
#                     summarises the PSA results.
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

#' Draw results summary table
#'
#' @param .PSA_data A list of class shinyPSA that contains summary PSA
#' results.
#' @param .wtp_ A numeric vector containing the willingness-to-pay
#' value(s) to be considered in the summary table. Default values are
#' \code{c(20,000, 30,000)}
#' @param .units_ A character, the units to associate with the
#' monitory values in the summary table. Default is sterling pounds
#' (GBP) \code{"\u00A3"}.
#' @param .effects_label_ The label or name to be given to the effects
#' column in the summary table. Default is QALYs.
#' @param .beautify_ Return a visually improved version of the table. The
#' returned version is built using DT::datatable()
#' @param .long_ Logical (default \code{TRUE}) for whether a long version
#' of the table is to be returned. If \code{FALSE}, a wide version of the
#' table will be returned
#' @param .individual_evpi_ Logical (default \code{TRUE}) to return
#' individual EVPI, otherwise population EVPI will be reported
#' @param .evpi_population_ The size of the population that is annually
#' affected by the interventions under study
#' @param .discount_rate_ The discount rate used to discount future
#' affected populations.
#' @param .time_horion_ The time expected to pass (in years) before the
#' interventions under consideration change (how long before the decision
#' under consideration become obsolete or requires updating)
#' @param .effs_accuracy_ Number of digits for effects measure; default is 3 and
#' is expressed as 1e-3 or 0.001.
#'
#' @return A table, dataframe, tibble or DT objects.
#' @importFrom tidyselect vars_select_helpers
#' @export
#'
#' @examples
#' \dontrun{
#' library(ShinyPSA)
#'
#' PSA_summary <- summarise_PSA_(
#'   .effs = as_tibble(ShinyPSA::Smoking_PSA$e),
#'   .costs = as_tibble(ShinyPSA::Smoking_PSA$c),
#'   .interventions = ShinyPSA::Smoking_PSA$treats)
#'
#' t <- draw_summary_table_(.PSA_data = PSA_summary,
#'                         .wtp_ = c(100, 20000, 30000),
#'                         .beautify_ = TRUE,
#'                         .long_ = TRUE)
#' t
#'
#' t <- draw_summary_table_(.PSA_data = PSA_summary,
#'                         .wtp_ = c(100, 20000, 30000),
#'                         .beautify_ = TRUE,
#'                         .long_ = F)
#' t
#' }
#'
draw_summary_table_ <- function(.PSA_data,
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

  ## Get the [95% CI] of costs and consequences:----
  effs_95_label <- paste0(.effects_label_, " 95% CI")
  ci_95 <- ShinyPSA::generate_95_ci(
    ### Effects 95% CI:----
    .data_ = .PSA_data[['e']],
    .interventions = .PSA_data[['interventions']],
    .accuracy_ = .effs_accuracy_,
    .units_ = "") %>%
    dplyr::rename({{effs_95_label}} := `[95% CI]`) %>%
    ### Costs 95% CI:----
    dplyr::right_join(
      x = .,
      y = generate_95_ci(
    .data_ = .PSA_data[['c']],
    .interventions = .PSA_data[['interventions']],
    .accuracy_ = 1,
    .units_ = .units_),
      by = "intervention") %>%
    dplyr::rename(`Costs 95% CI` := `[95% CI]`)

  ## Put summary table together:----
  ### prepare a tidy evaluation variable:----
  incr_col_ <- paste("Incremental", .effects_label_)
  effs_mu_95_label = paste0({{.effects_label_}}, " [95% CI]")
  ### start building the final tibble:----
  Summary_tbl <- ICER_tbl %>%
    #### join the 95% CI data by intervention name:----
    dplyr::left_join(x = ., y = ci_95, by = 'intervention') %>%
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
        tidyselect::vars_select_helpers$where(is.numeric) &
          !c(qalys, delta.e,
             dplyr::starts_with("Prob."),
             dplyr::contains("95% CI")), ~
          scales::dollar(
            x = .x,
            prefix = .units_,
            accuracy = 1))) %>%
    ##### format effects columns:----
    dplyr::mutate(
      dplyr::across(c(qalys, delta.e, dplyr::starts_with("Prob.")),
                    ~ as.character(round(.x, digits = 3)))) %>%
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
        is.na(ICER) ~ `ICER information`,
        TRUE ~ ICER)) %>%
      dplyr::select(-`ICER information`) %>%
      dplyr::select(
        Comparators, Costs, `Costs 95% CI`, `Incremental Costs`,
        {{.effects_label_}}, {{effs_95_label}}, {{incr_col_}},
        dplyr::everything()) %>%
    #### rename QALYs and Costs to mean:----
        dplyr::mutate(
        "Costs [95% CI]" = paste0(
          Costs, " ", `Costs 95% CI`),
        {{effs_mu_95_label}} := paste0(
          .data[[{{.effects_label_}}]], " ", .data[[{{effs_95_label}}]])) %>%
        dplyr::select(
          Comparators, `Costs [95% CI]`, {{effs_mu_95_label}},
          `Incremental Costs`, {{incr_col_}}, dplyr::everything()) %>%
        dplyr::select(
          -Costs, -`Costs 95% CI`, -{{.effects_label_}},
          -{{effs_95_label}})

  ## Create a long format table:----
  if(.long_) {
    ### reorder some columns for DT::RowGroup option:----
    Summary_tbl <- Summary_tbl %>%
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
        RowGroup_ = c(rep(glue::glue("Costs ({.units_})"), 1),
                      rep(.effects_label_, 1),
                      rep("Incremental Analysis", 3),
                      rep(glue::glue("Net Benefit ({.units_})"),
                          length(.wtp_)),
                      rep("Probability Cost-Effective",
                          length(.wtp_)),
                      rep(glue::glue("Expected Value of Perfect
                          Information ({.units_}) [1]"),
                          length(.wtp_))),
        ##### Prepare border info:----
        RowBorder_ = c(1, 1, 0, 0, 1,
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
            "Probabilistic Sensitivity Analysis Summary Table"
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
    #### reorder table for wide format:----
    #### custom table container to create column groups:----
    sketch_ <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Comparators'), # 1 column (merge 2 rows)
          th(rowspan = 1, glue::glue("Costs ({.units_})")),
          th(rowspan = 1, 'QALYs'), # 1 column (merge 2 rows)
          th(colspan = 3, 'Incremental analysis'), # span over 2 columns
          th(colspan = length(.wtp_),
             glue::glue("Net Benefit ({.units_})")), # span over num .wtp_
          th(colspan = length(.wtp_), 'Probability cost-effective'),
          th(colspan = length(.wtp_),
             glue::glue("EVPI ({.units_}) [1]")),
        ),
        tr(
          purrr::map(# "costs", "effects, "c.inc", "e.inc"
            .x = c("Mean [95% CI]", "Mean [95% CI]",
                   glue::glue("Costs ({.units_})"), "QALYs", "ICER",
                   rep(
                     scales::dollar(# Net Benefit, Prob. CE, EVPI
                       x = .wtp_,
                       prefix = .units_), 3)),
            .f = th)
        )
      )
    ))
    #### get columns where to border is to be drawn:----
    colBorder_ <- c(1:3, 6,
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
            "Probabilistic Sensitivity Analysis Summary Table"
          )
        )
      ) %>%
      DT::formatStyle(
        columns = colBorder_,
        `border-right` = 'solid 1px'
      )

  }

  return(Summary_tbl)
}
