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
#'                         t
#'
#' t <- draw_summary_table_(.PSA_data = PSA_summary,
#'                         .wtp_ = c(100, 20000, 30000),
#'                         .beautify_ = TRUE,
#'                         .long_ = F)
#'
#' t
#' }
#'
draw_summary_table_ = function(.PSA_data, .wtp_ = c(20000, 30000),
                               .units_ = "\u00A3",
                               .effects_label_ = "QALYs",
                               .beautify_ = TRUE,
                               .long_ = TRUE) {
  # Set currency label if none were provided:
  if(is.null(.units_) | length(.units_) != 1) .units_ = "\u00A3"

  # Get the ICER table from the result's object:
  ICER_tbl <- .PSA_data[["ICER"]]

  # Get the eNMB values from the result's object:
  eNMB <- .PSA_data[["e.NMB"]] %>%
    # put WTP in a column next to interventions' expected NMB:
    dplyr::mutate('WTP' = .PSA_data[["WTPs"]]) %>%
    # filter and keep values in .wtp_ vector:
    dplyr::filter(WTP %in% .wtp_) %>%
    # rename WTP values to use as column names later:
    dplyr::mutate(WTP = paste0("NMB @ ",
                               scales::dollar(
                                 x = WTP,
                                 prefix = .units_))) %>%
    # put everything in a long format:
    tidyr::pivot_longer(
      cols = -WTP,
      names_to = 'intervention',
      values_to = 'NMB') %>%
    # flip the table back to have each intervention in a row:
    tidyr::pivot_wider(
      id_cols = 'intervention',
      names_from = 'WTP',
      values_from = 'NMB')

  # Get the probability of being cost-effective from the result's object:
  CEAF <- dplyr::tibble(
    'CEAF - values' = .PSA_data[["CEAF"]]$ceaf,
    # put WTP in a column next to probability of CE:
    'CEAF - WTP' = .PSA_data[["WTPs"]],
    # put best interv name in a column next to probability of CE:
    'intervention' = .PSA_data[["best_name"]]) %>%
    # filter and keep values in .wtp_ vector:
    dplyr::filter(`CEAF - WTP` %in% .wtp_) %>%
    # rename WTP values to use as column names later:
    dplyr::mutate(`CEAF - WTP` = paste0("Prob. CE @ ",
                                        scales::dollar(
                                          x = `CEAF - WTP`,
                                          accuracy = 1,
                                          prefix = .units_)))

  # Get the EVPI from the result's object:
  EVPI <- dplyr::tibble(
    'EVPI - values' = .PSA_data[["EVPI"]],
    # put WTP in a column next to EVPI:
    'EVPI - WTP' = .PSA_data[["WTPs"]],
    # put best interv name in a column next to EVPI:
    'intervention' = .PSA_data[["best_name"]]) %>%
    # filter and keep values in .wtp_ vector:
    dplyr::filter(`EVPI - WTP` %in% .wtp_) %>%
    # rename WTP values to use as column names later:
    dplyr::mutate(`EVPI - WTP` = paste0("EVPI @ ",
                                        scales::dollar(
                                          x = `EVPI - WTP`,
                                          prefix = .units_)))

  # Put summary table together:
  # prepare a tidy evaluation variable:
  incr_col_ <- paste("Incremental", .effects_label_)
  # start building the final tibble:
  Summary_tbl <- ICER_tbl %>%
    # join the expected NMB to the ICER results by intervention name:
    dplyr::left_join(x = ., y = eNMB, by = 'intervention') %>%
    # join the probability of being cost-effective by intervention name:
    dplyr::left_join(x = ., y = CEAF, by = 'intervention') %>%
    # create probability CE columns from relevant row values:
    tidyr::pivot_wider(
      names_from = `CEAF - WTP`,
      values_from = `CEAF - values`) %>%
    # drop any NAs resulting from pivot_wider:
    dplyr::select(tidyselect::vars_select_helpers$where(
      fn = function(.x) !all(is.na(.x)))) %>%
    # join the EVPI:
    dplyr::left_join(x = ., y = EVPI, by = 'intervention') %>%
    # create EVPI columns from relevant row values:
    tidyr::pivot_wider(
      names_from = `EVPI - WTP`,
      values_from = `EVPI - values`) %>%
    # drop any NAs resulting from pivot_wider:
    dplyr::select(tidyselect::vars_select_helpers$where(
      fn = function(.x) !all(is.na(.x)))) %>%
    # do some formatting:
    # format currency columns:
    dplyr::mutate(
      dplyr::across(
        tidyselect::vars_select_helpers$where(
          is.numeric) & !c(qalys, delta.e,
                           dplyr::starts_with("Prob.")),
        ~ scales::dollar(
          x = .x,
          accuracy = 0.1,
          prefix = .units_))) %>%
    # format effects columns:
    dplyr::mutate(
      dplyr::across(c(qalys, delta.e, dplyr::starts_with("Prob.")),
                    ~ as.character(round(.x, digits = 4)))) %>%
    # drop dominance column if it exists:
    dplyr::select(-dplyr::any_of('dominance')) %>%
    # rename columns to proper names:
    dplyr::rename({{.effects_label_}} := qalys,
                  Comparators = intervention,
                  {{incr_col_}} := delta.e,
                  "Incremental Costs" = delta.c,
                  "ICER information" = icer_label) %>%
    # proper column name:
    dplyr::rename_with(stringr::str_to_title, costs) %>%
    # convert column names to capital letters:
    dplyr::rename_with(toupper, icer) %>%
    # put values from ICER information to the ICER column:
    dplyr::mutate(
      ICER = case_when(
        is.na(ICER) ~
          `ICER information`,
        TRUE ~ ICER)) %>%
    dplyr::select(-`ICER information`)
  # Create a long format table:
  if(.long_) {
    # reorder some columns for DT::RowGroup option:
    Summary_tbl <- Summary_tbl %>%
      dplyr::select(
        Costs, `Incremental Costs`, QALYs, `Incremental QALYs`,
        dplyr::everything()) %>%
      # flip the dataset to have everything in long format:
      tidyr::pivot_longer(
        cols = -Comparators,
        names_to = " ",
        values_to = "Values") %>%
      # flip the dataset to keep the interventions' in the columns:
      tidyr::pivot_wider(
        names_from = Comparators,
        values_from = Values)
  }
  # Beautified tables:
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
    # Prepare DT-table helper columns:
    Summary_tbl <- Summary_tbl %>%
      dplyr::mutate(
        # Prepare DT-table row groups:
        RowGroup_ = c(rep(glue::glue("Costs ({.units_})"), 2),
                      rep("QALYs", 2),
                      "Incremental Cost-Effectiveness Ratio",
                      rep(glue::glue("Net Benefit ({.units_})"),
                          length(.wtp_)),
                      rep("Probability Cost-Effective",
                          length(.wtp_)),
                      rep(glue::glue("Expected Value of Perfect
                                      Information ({.units_})"),
                          length(.wtp_))),
        # Prepare border info:
        RowBorder_ = c(0, 1, 0, 1, 1,
                       rep(0, length(.wtp_) - 1), 1,
                       rep(0, length(.wtp_) - 1), 1,
                       rep(0, length(.wtp_) - 1), 1)
      )
    # Number of columns to show:
    ColShow_ <- nrow(ICER_tbl)
    # Build the table:
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
          )
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
          th(rowspan = 2, 'Comparators'), # 1 column (merge 2 rows)
          th(rowspan = 2, 'QALYs'), # 1 column (merge 2 rows)
          th(rowspan = 2, 'Costs'), # 1 column (merge 2 rows)
          th(colspan = 2, 'Incremental'), # span over 2 columns
          th(rowspan = 2, 'ICER'), # 1 column (merge 2 rows)
          th(colspan = length(.wtp_), 'Net Benefit'), # span over num .wtp_
          th(colspan = length(.wtp_), 'Probability cost-effective'),
          th(colspan = length(.wtp_), 'EVPI'),
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
    # get columns where to border is to be drawn:
    colBorder_ <- c(1:3, 5, 6,
                    6 + length(.wtp_),
                    6 + (length(.wtp_) * 2),
                    6 + (length(.wtp_) * 3))
    # build the table:
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
          )
        ),
        container = sketch_, ## object to use to draw table
        extensions = 'Buttons',
        selection = 'none', ## enable selection of a single row
        filter = 'none', ## include column filters at the bottom
        rownames = FALSE  ## don't show row numbers/names
      ) %>%
      DT::formatStyle(
        columns = colBorder_,
        `border-right` = 'solid 1px'
      )

  }

  return(Summary_tbl)
}
