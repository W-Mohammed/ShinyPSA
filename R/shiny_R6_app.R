################################################################################
#
# Script Name:        shiny_R6_elements.R
# Module Name:        Economic/PSA/demo app
# Script Description: Defines and triggers the shiny app
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

# Shiny_R6_app class:----
#' R6 Class representing a Shiny app.
#'
#' @description
#' An instance of this class is a fully fledged shiny app.
#' @format An [R6::R6Class] object.
#' @name ShinyPSA_R6_App
NULL
#'
#' @rdname ShinyPSA_R6_App
#' @export
ShinyPSA_R6_App <- R6::R6Class(
  classname = 'ShinyPSA_R6_App',
  public = list(
    ## Fields:----
    ### Global elements:----
    iContainer = list(), # inputs container
    ### Page elements:----
    theme = NULL,
    ### Securing app:----
    credentials = NULL,
    make_secure = TRUE,

    ## Methods:----
    ### Initialise:----
    initialize = function() {
      self$credentials  <- data.frame(
        user = c("Harry.Heyburn@dhsc.gov.uk",
                 "Stephen.Ashton@dhsc.gov.uk",
                 "c.thomas@sheffield.ac.uk",
                 "admin"), # mandatory
        password = c("Harry.Heyburn@dhsc.gov.uk",
                     "Stephen.Ashton@dhsc.gov.uk",
                     "c.thomas@sheffield.ac.uk",
                     "WaelMohammed"), # mandatory
        start = c("2019-04-15"), # optional (all others)
        expire = c(NA, NA, NA, NA),
        admin = c(FALSE, FALSE, FALSE, TRUE),
        stringsAsFactors = FALSE
      )
      self$theme <- bslib::bs_theme()
      self$iContainer[["themeSwch"]] <- prettySwitch$new(
        .label_ = "light_mode"
      )
      self$iContainer[["addBtn"]] <- actionButton$new(
        .label_ = "Summarise selected PSA dataset"
      )
      self$iContainer[["uplBtn"]] <- actionButton$new(
        .label_ = "Summarise uploaded PSA dataset"
      )
      self$iContainer[["intrNme"]] <- textInput$new(
        .label_ = "Enter interventions' names (comma delimited):"
      )
      self$iContainer[["getData"]] <- inputSelection$new(
        .label_ = NULL
      )
      self$iContainer[["maxWTP1"]] <- numericInput$new(
        .label_ = "Set maximum Willingness-to-pay"
      )
      self$iContainer[["maxWTP2"]] <- numericInput$new(
        .label_ = "Set maximum Willingness-to-pay"
      )
      self$iContainer[["sumTbl"]] <-  dataTableDT$new(
        .label_ = "PSA summary table"
      )
      self$iContainer[["CEP"]] <- ggplot2Plot$new(
        .label_ = "CEP"
      )
      self$iContainer[["CEAC"]] <- ggplot2Plot$new(
        .label_ = "CEAC"
      )
      self$iContainer[["CEAF"]] <- ggplot2Plot$new(
        .label_ = "CEAF"
      )
      self$iContainer[["NMB"]] <- ggplot2Plot$new(
        .label_ = "NMB"
      )
      self$iContainer[["EVPI"]] <- ggplot2Plot$new(
        .label_ = "EVPI"
      )
      self$iContainer[["updBtn0"]] <- actionButton$new(
        .label_ = "Update summary table"
      )
      self$iContainer[["RstBtn0"]] <- actionButton$new(
        .label_ = "Reset summary table"
      )
      self$iContainer[["orinSch"]] <- prettySwitch$new(
        .label_ = "Wide table format"
      )
      self$iContainer[["wtpTxt0"]] <- textInput$new(
        .label_ = "Enter WTP vector (comma delimited):"
      )
      self$iContainer[["getRef1"]] <- inputSelection$new(
        .label_ = "Select the reference intervention:"
      )
      self$iContainer[["icrSch"]] <- prettySwitch$new(
        .label_ = "Show ICER information"
      )
      self$iContainer[["zomSch1"]] <- prettySwitch$new(
        .label_ = "Zoom to min/max values"
      )
      self$iContainer[["wtpSch1"]] <- prettySwitch$new(
        .label_ = "Show WTP information"
      )
      self$iContainer[["updBtn1"]] <- actionButton$new(
        .label_ = "Update CEP plot"
      )
      self$iContainer[["RstBtn1"]] <- actionButton$new(
        .label_ = "Reset CEP plot"
      )
      self$iContainer[["lgdSldX1"]] <- sliderInput$new(
        .label_ = "Legend X coordinate:"
      )
      self$iContainer[["lgdSldY1"]] <- sliderInput$new(
        .label_ = "Legend Y coordinate:"
      )
      self$iContainer[["wtpTxt1"]] <- textInput$new(
        .label_ = "Enter WTP vector (comma delimited):"
      )
      self$iContainer[["lblSldX"]] <- sliderInput$new(
        .label_ = "ICER label X nudge:"
      )
      self$iContainer[["lblSldY"]] <- sliderInput$new(
        .label_ = "ICER label Y nudge:"
      )
      self$iContainer[["getRef2"]] <- inputSelection$new(
        .label_ = "Select the reference intervention:"
      )
      self$iContainer[["zomSch2"]] <- prettySwitch$new(
        .label_ = "Zoom to min/max values"
      )
      self$iContainer[["wtpSch2"]] <- prettySwitch$new(
        .label_ = "Show WTP lines"
      )
      self$iContainer[["wtpLSch2"]] <- prettySwitch$new(
        .label_ = "Show WTP labels"
      )
      self$iContainer[["ceafSch2"]] <- prettySwitch$new(
        .label_ = "Show CEAF on CEAC"
      )
      self$iContainer[["shpSch2"]] <- prettySwitch$new(
        .label_ = "Show shapes on CEAC"
      )
      self$iContainer[["updBtn2"]] <- actionButton$new(
        .label_ = "Update CEAC plot"
      )
      self$iContainer[["RstBtn2"]] <- actionButton$new(
        .label_ = "Reset CEAC plot"
      )
      self$iContainer[["lgdSldX2"]] <- sliderInput$new(
        .label_ = "Legend X coordinate:"
      )
      self$iContainer[["lgdSldY2"]] <- sliderInput$new(
        .label_ = "Legend Y coordinate:"
      )
      self$iContainer[["wtpTxt2"]] <- textInput$new(
        .label_ = "Enter WTP vector (comma delimited):"
      )
      self$iContainer[["zomSch3"]] <- prettySwitch$new(
        .label_ = "Zoom to min/max values"
      )
      self$iContainer[["wtpSch3"]] <- prettySwitch$new(
        .label_ = "Show WTP lines"
      )
      self$iContainer[["wtpLSch3"]] <- prettySwitch$new(
        .label_ = "Show WTP labels"
      )
      self$iContainer[["shpSch3"]] <- prettySwitch$new(
        .label_ = "Show shapes on CEAF"
      )
      self$iContainer[["updBtn3"]] <- actionButton$new(
        .label_ = "Update CEAF plot"
      )
      self$iContainer[["RstBtn3"]] <- actionButton$new(
        .label_ = "Reset CEAF plot"
      )
      self$iContainer[["lgdSldX3"]] <- sliderInput$new(
        .label_ = "Legend X coordinate:"
      )
      self$iContainer[["lgdSldY3"]] <- sliderInput$new(
        .label_ = "Legend Y coordinate:"
      )
      self$iContainer[["wtpTxt3"]] <- textInput$new(
        .label_ = "Enter WTP vector (comma delimited):"
      )
      self$iContainer[["zomSch4"]] <- prettySwitch$new(
        .label_ = "Zoom to min/max values"
      )
      self$iContainer[["wtpSch4"]] <- prettySwitch$new(
        .label_ = "Show WTP lines"
      )
      self$iContainer[["wtpLSch4"]] <- prettySwitch$new(
        .label_ = "Show WTP labels"
      )
      self$iContainer[["updBtn4"]] <- actionButton$new(
        .label_ = "Update NMB plot"
      )
      self$iContainer[["RstBtn4"]] <- actionButton$new(
        .label_ = "Reset NMB plot"
      )
      self$iContainer[["lgdSldX4"]] <- sliderInput$new(
        .label_ = "Legend X coordinate:"
      )
      self$iContainer[["lgdSldY4"]] <- sliderInput$new(
        .label_ = "Legend Y coordinate:"
      )
      self$iContainer[["wtpTxt4"]] <- textInput$new(
        .label_ = "Enter WTP vector (comma delimited):"
      )
      self$iContainer[["zomSch5"]] <- prettySwitch$new(
        .label_ = "Zoom to min/max values"
      )
      self$iContainer[["wtpSch5"]] <- prettySwitch$new(
        .label_ = "Show WTP lines"
      )
      self$iContainer[["wtpLSch5"]] <- prettySwitch$new(
        .label_ = "Show WTP labels"
      )
      self$iContainer[["updBtn5"]] <- actionButton$new(
        .label_ = "Update EVPI plot"
      )
      self$iContainer[["RstBtn5"]] <- actionButton$new(
        .label_ = "Reset EVPI plot"
      )
      self$iContainer[["lgdSldX5"]] <- sliderInput$new(
        .label_ = "Legend X coordinate:"
      )
      self$iContainer[["lgdSldY5"]] <- sliderInput$new(
        .label_ = "Legend Y coordinate:"
      )
      self$iContainer[["wtpTxt5"]] <- textInput$new(
        .label_ = "Enter WTP vector (comma delimited):"
      )
      self$iContainer[["popEVPI"]] <- prettySwitch$new(
        .label_ = "Estimate individual EVPI"
      )
      self$iContainer[["timEVPI"]] <- numericInput$new(
        .label_ = "Population EVPI time horizon:"
      )
      self$iContainer[["dsRtEVPI"]] <- numericInput$new(
        .label_ = "Population discount rate:"
      )
      self$iContainer[["popSzEVPI"]] <- numericInput$new(
        .label_ = "Population EVPI sample size:"
      )

    },
    ### UI:----
    ui = function() {
      ui <- fluidPage(
        theme = self$theme,
        waiter::use_waiter(),
        #### Title panel:----
        titlePanel(
          windowTitle = "ShinyPSA demo",
          div(
            class = "d-flex p-4 bd-highlight",
            ##### App logo:----
            img(
              src = "https://pbs.twimg.com/profile_images/959365885537456128/tC4OVmkX_400x400.jpg",
              height = "45px",
              class = "pr-2 pb-0"),
            ##### Title:----
            span("ShinyPSA demo app!"),
            ##### Theme switcher:----
            self$iContainer[["themeSwch"]]$
              ui_input(
                .value_ = TRUE
              )
          )
        ),
        #### Main body:----
        fluidRow(
          column(
            width = 12,
            offset = 0,
            class = "pl-5 pr-5",
            tabsetPanel(
              id = "dataName",
              ##### Settings page:----
              tabPanel(
                title = "Setup",
                icon = icon("cog"),
                fluidRow(
                  column(
                    width = 5,
                    offset = 1,
                    style = "border-right: 1px solid",
                    div(
                      class = "pt-10 pb-0 mb-0 pl-5",
                      h4(
                        class = "pt-2",
                        "Option 1: Choose from existing datasets:"
                      ),
                      fluidRow(
                        ###### Data selection:----
                        self$iContainer[["getData"]]$
                          ui_input(
                            .choices_ = NULL,
                            .style_ = "display: flex;
                          margin-top: 2rem !important;",
                            .class_ = "pl-4 d-flex align-items-start",
                            .width_ = NULL
                          )
                      ),
                      fluidRow(
                        self$iContainer[["maxWTP1"]]$
                          ui_input(
                            .value_ = 1e+5,
                            .min_ = 0,
                            .max_ = 1e+6,
                            .class_ = "pl-4 d-flex align-items-start"
                          )
                      ),
                    )
                  ),
                  column(
                    width = 5,
                    div(
                      class = "pt-10 pb-0 mb-0 pl-5",
                      h4(
                        class = "pt-2",
                        "Option 2: Upload your own data:"
                      ),
                      fluidRow(
                        tagList(
                          ###### Data uploading:----
                          # Input: upload costs file:
                          div(
                            style = "display: flex;
                          margin-top: 2rem !important;",
                            class = "pl-4 d-flex align-items-start",
                            fileInput(
                              buttonLabel = "Browse costs file...",
                              inputId = "c",
                              label = NULL,
                              multiple = FALSE,
                              accept = c(".csv")
                            )
                          )
                        )
                      ),
                      fluidRow(
                        tagList(
                          # Input: upload effects file:
                          div(
                            style = "display: flex;
                          margin-top: 0rem !important;",
                            class = "pl-4 d-flex align-items-start",
                            fileInput(
                              buttonLabel = "Browse effects file...",
                              inputId = "e",
                              label = NULL,
                              multiple = FALSE,
                              accept = c(".csv")
                            )
                          ),
                        )
                      ),
                      fluidRow(
                        tagList(
                          div(
                            style = "display: flex;
                          margin-top: 0rem !important;",
                            class = "pl-4 d-flex align-items-start",
                            # Input: add interventions' names:
                            self$iContainer[["intrNme"]]$
                              ui_input(
                                .value_ = NULL
                              )
                          )
                        )
                      ),
                      fluidRow(
                        self$iContainer[["maxWTP2"]]$
                          ui_input(
                            .value_ = 1e+5,
                            .min_ = 0,
                            .max_ = 1e+6,
                            .class_ = "pl-4 d-flex align-items-start"
                          )
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 5,
                    offset = 1,
                    style = "border-right: 1px solid",
                    div(
                      style = "display: flex;
                          margin-top: 0rem !important;",
                      class = "d-flex flex-row-reverse",
                      ###### Selection confirmation button:----
                      self$iContainer[["addBtn"]]$
                        ui_input(
                          .style_ = "display: flex;
                          margin-top: 0rem !important;",
                          .class_ = "ml-2 d-flex align-items-end
                          text-right"
                        )
                    )
                  ),
                  column(
                    width = 5,
                    div(
                      style = "display: flex;
                          margin-top: 0rem !important;",
                      class = "d-flex flex-row-reverse",
                      # upload effects button:
                      self$iContainer[["uplBtn"]]$
                        ui_input(
                          .style_ = "display: flex;
                          margin-top: 0rem !important;",
                          .class_ = "ml-2 d-flex align-items-end
                          text-right"
                        )
                    )
                  )
                )
              ),
              ##### Outputs pages:----
              tabPanel(
                title = uiOutput(
                  outputId = "selectedData",
                  inline = TRUE
                ),
                tabsetPanel(
                  id = "outputs",
                  ###### Summary table:----
                  bslib::nav(
                    width = 9,
                    title = "Summary table",
                    fluidRow(
                      column(
                        width = 2,
                        self$iContainer[["updBtn0"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                        hr(),
                        self$iContainer[["orinSch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left",
                            .value_ = FALSE
                          ),
                        self$iContainer[["wtpTxt0"]]$
                          ui_input(),
                        self$iContainer[["RstBtn0"]]$
                          ui_input(
                            .width_ = "100%"
                          )
                      ),
                      column(
                        width = 10,
                        div(
                          class = "pb-5 pt-2 pr-1
                          d-flex d-flex align-items-center",
                          #style = ,
                          self$iContainer[["sumTbl"]]$
                            ui_output()
                        )
                      )
                    ),
                  ),
                  ###### Cost-Effectiveness Plane:----
                  bslib::nav(
                    width = 9,
                    title = "Cost-Effectiveness Plane",
                    fluidRow(
                      column(
                        width = 2,
                        self$iContainer[["updBtn1"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                        hr(),
                        self$iContainer[["getRef1"]]$
                          ui_input(
                            .choices_ = NULL
                          ),
                        self$iContainer[["wtpSch1"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpTxt1"]]$
                          ui_input(),
                        self$iContainer[["lgdSldX1"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.8
                          ),
                        self$iContainer[["lgdSldY1"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.2
                          ),
                        self$iContainer[["icrSch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["lblSldX"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.1
                          ),
                        self$iContainer[["lblSldY"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.1
                          ),
                        self$iContainer[["zomSch1"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["RstBtn1"]]$
                          ui_input(
                            .width_ = "100%"
                          )
                      ),
                      column(
                        width = 10,
                        self$iContainer[["CEP"]]$
                          ui_output()
                      )
                    )
                  ),
                  ###### Cost-Effectiveness Acceptability Curve:----
                  bslib::nav(
                    width = 9,
                    title = "Cost-Effectiveness Acceptability Curve",
                    fluidRow(
                      column(
                        width = 2,
                        self$iContainer[["updBtn2"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                        hr(),
                        self$iContainer[["getRef2"]]$
                          ui_input(
                            .choices_ = NULL
                          ),
                        self$iContainer[["wtpSch2"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpLSch2"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["ceafSch2"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["shpSch2"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpTxt2"]]$
                          ui_input(),
                        self$iContainer[["lgdSldX2"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.8
                          ),
                        self$iContainer[["lgdSldY2"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.2
                          ),
                        self$iContainer[["zomSch2"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["RstBtn2"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                      ),
                      column(
                        width = 10,
                        self$iContainer[["CEAC"]]$
                          ui_output()
                      )
                    )
                  ),
                  ###### Cost-Effectiveness Acceptability Frontier:----
                  bslib::nav(
                    width = 9,
                    title = "Cost-Effectiveness Acceptability Frontier",
                    fluidRow(
                      column(
                        width = 2,
                        self$iContainer[["updBtn3"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                        hr(),
                        self$iContainer[["wtpSch3"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpLSch3"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["shpSch3"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpTxt3"]]$
                          ui_input(),
                        self$iContainer[["lgdSldX3"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.8
                          ),
                        self$iContainer[["lgdSldY3"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.2
                          ),
                        self$iContainer[["zomSch3"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["RstBtn3"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                      ),
                      column(
                        width = 10,
                        self$iContainer[["CEAF"]]$
                          ui_output()
                      )
                    )
                  ),
                  ###### Expected Net Monitory Benefit:----
                  bslib::nav(
                    width = 9,
                    title = "Expected Net Monitory Benefit",
                    fluidRow(
                      column(
                        width = 2,
                        self$iContainer[["updBtn4"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                        hr(),
                        self$iContainer[["wtpSch4"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpLSch4"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpTxt4"]]$
                          ui_input(),
                        self$iContainer[["lgdSldX4"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.8
                          ),
                        self$iContainer[["lgdSldY4"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.2
                          ),
                        self$iContainer[["zomSch4"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["RstBtn4"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                      ),
                      column(
                        width = 10,
                        self$iContainer[["NMB"]]$
                          ui_output()
                      )
                    )
                  ),
                  ###### Expected Value of Perfect Information:----
                  bslib::nav(
                    width = 9,
                    title = "Expected Value of Perfect Information",
                    fluidRow(
                      column(
                        width = 2,
                        self$iContainer[["updBtn5"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                        hr(),
                        self$iContainer[["popEVPI"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["timEVPI"]]$
                          ui_input(
                            .min_ = 1,
                            .max_ = 100,
                            .value_ = 1
                          ),
                        self$iContainer[["dsRtEVPI"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.035
                          ),
                        self$iContainer[["popSzEVPI"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1e8,
                            .value_ = 1.5e3
                          ),
                        self$iContainer[["wtpSch5"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpLSch5"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpTxt5"]]$
                          ui_input(),
                        self$iContainer[["lgdSldX5"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.8
                          ),
                        self$iContainer[["lgdSldY5"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.2
                          ),
                        self$iContainer[["zomSch5"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["RstBtn5"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                      ),
                      column(
                        width = 10,
                        self$iContainer[["EVPI"]]$
                          ui_output()
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      ### Secure UI:----
      if(self$make_secure) {
        ui <- shinymanager::secure_app(
          ui = ui,
          theme = bslib::bs_theme(
            bg = "white",
            fg = "black"
          )
        )
      } else {
        ui
      }
    },
    ### Waiter:----
    waiter_dev = function(.info_) {
      div(
        style = "
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content:center;
            color: white;
            opacity: 1 !important;
          ",
        h4(.info_),
        h4("Please wait."),
        br(),br(),
        waiter::spin_wandering_cubes()
      )
    },
    ### Server:----
    server = function(input, output, session) {
      #### Secure server:----
      if(self$make_secure) {
        res_auth <- shinymanager::secure_server(
          check_credentials = shinymanager::check_credentials(
            self$credentials
          ),
          timeout = 30
        )
      } else {
        res_auth <- reactiveValues(user = "No Credentials")
      }
      #### Data handlers:----
      ##### Data drop-down list:----
      self$iContainer[["getData"]]$
        server(
          session = session,
          input = input,
          output = output,
          .choices_ = c("Hyperphosphatemia_PSA", "Vaccine_PSA", "Smoking_PSA", "Hypothetical_PSA", "Clopidogrel_PSA")
        )
      ##### Uploaded data:----
      incoming <- reactive({
        req(
          input$c,
          input$e,
          input[[self$iContainer[["intrNme"]]$
                   get_uiInId()]]
        )
        list(
          "c" = read_csv(
            file = input$c$datapath,
            show_col_types = FALSE
          ),
          "e" = read_csv(
            file = input$e$datapath,
            show_col_types = FALSE
          ),
          "treats" = strsplit(input[[self$iContainer[["intrNme"]]$
                                       get_uiInId()]],
                              ",") %>%
            unlist() %>%
            gsub(pattern = " ", replacement = "")
        )
      })
      uData_list <- reactive({
        validate(
          need(!is.null(incoming()$c),
               "Costs file is empty or could not be processed"),
          need(!is.null(incoming()$e),
               "Effects file is empty or could not be processed"),
          need(dim(incoming()$c) == dim(incoming()$e),
               "Costs/effects don't have equal rows and/or columns"),
          need(length(incoming()$treats) == ncol(incoming()$c),
               "Provided names are not equal to data columns")
        )
        incoming()
      })

      ##### Reactive/static data set and data name object:----
      sData_list <- reactiveValues()
      sData_name <- reactiveVal()

      ##### Reactive container for R6 objects:----
      rContainer <- reactiveValues()

      #### Renderers:----
      ##### Actions on add button:----
      observeEvent(
        eventExpr = input[[self$iContainer[["addBtn"]]$
                             get_uiInId()]],
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Summarising selected PSA data..."
              ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Store name and data set for later:----
          sData_name(
            input[[self$iContainer[["getData"]]$
                     get_uiInId()]]
          )
          sData_list[[sData_name()]] <- get(
            x = input[[self$iContainer[["getData"]]$
                         get_uiInId()]],
            pos = "package:ShinyPSA"
          )
          ###### Render the name of the summarised data:----
          output$selectedData <- renderText({
            sData_name()
          })
          ###### Create an instance of class ShinyPSA using the data:----
          rContainer[[sData_name()]] <- ShinyPSA$new(
            .effs = sData_list[[sData_name()]]$e,
            .costs = sData_list[[sData_name()]]$c,
            .interventions = sData_list[[sData_name()]]$treats,
            .Kmax = input[[self$iContainer[["maxWTP1"]]$
                             get_uiInId()]]
          )
          ###### Retrieve the Summary table from the ShinyPSA object:----
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_Summary_table(
                  .beautify_ = TRUE,
                  .long_ = TRUE
                ),
              .readyDT_ = TRUE
            )
          ###### Retrieve the CEP from the ShinyPSA object:----
          self$iContainer[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEP()
            )
          ###### Retrieve intervention names for CEP options:----
          self$iContainer[["getRef1"]]$
            server(
              session = session,
              input = input,
              output = output,
              .choices_ = c("Do not set a reference",
                            sData_list[[sData_name()]]$treats)
            )
          ###### Retrieve the CEAC from the ShinyPSA object:----
          self$iContainer[["CEAC"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAC()
            )
          ###### Retrieve intervention names for CEAC options:----
          self$iContainer[["getRef2"]]$
            server(
              session = session,
              input = input,
              output = output,
              .choices_ = c("Do not set a reference",
                            sData_list[[sData_name()]]$treats)
            )
          ###### Retrieve the CEAF from the ShinyPSA object:----
          self$iContainer[["CEAF"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAF()
            )

          ###### Retrieve the NMB from the ShinyPSA object:----
          self$iContainer[["NMB"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_eNMB()
            )

          ###### Retrieve the EVPI from the ShinyPSA object:----
          self$iContainer[["EVPI"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_EVPI()
            )

        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on upload button:----
      observeEvent(
        eventExpr = input[[self$iContainer[["uplBtn"]]$
                             get_uiInId()]],
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Summarising uploaded PSA data..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Store name and data set for later:----
          sData_name(
            "uploaded data"
          )
          req(uData_list())
          sData_list[[sData_name()]] <- uData_list()
          ###### Render the name of the summarised data:----
          output$selectedData <- renderText({
            sData_name()
          })
          ###### Create an instance of class ShinyPSA using the data:----
          rContainer[[sData_name()]] <- ShinyPSA$new(
            .effs = sData_list[[sData_name()]]$e,
            .costs = sData_list[[sData_name()]]$c,
            .interventions = sData_list[[sData_name()]]$treats,
            .Kmax = input[[self$iContainer[["maxWTP2"]]$
                             get_uiInId()]]
          )
          ###### Retrieve the Summary table from the ShinyPSA object:----
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_Summary_table(
                  .beautify_ = TRUE,
                  .long_ = TRUE
                ),
              .readyDT_ = TRUE
            )
          ###### Retrieve the CEP from the ShinyPSA object:----
          self$iContainer[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEP()
            )
          ###### Retrieve intervention names for CEP options:----
          self$iContainer[["getRef1"]]$
            server(
              session = session,
              input = input,
              output = output,
              .choices_ = c("Do not set a reference",
                            sData_list[[sData_name()]]$treats)
            )
          ###### Retrieve the CEAC from the ShinyPSA object:----
          self$iContainer[["CEAC"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAC()
            )
          ###### Retrieve intervention names for CEAC options:----
          self$iContainer[["getRef2"]]$
            server(
              session = session,
              input = input,
              output = output,
              .choices_ = c("Do not set a reference",
                            sData_list[[sData_name()]]$treats)
            )

          ###### Retrieve the CEAF from the ShinyPSA object:----
          self$iContainer[["CEAF"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAF()
            )

          ###### Retrieve the NMB from the ShinyPSA object:----
          self$iContainer[["NMB"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_eNMB()
            )

          ###### Retrieve the EVPI from the ShinyPSA object:----
          self$iContainer[["EVPI"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_EVPI()
            )

        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on Summary table update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn0"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Summary table..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Update Summary Table:----
          # Get user defined WTP values:
          wtp_ <- input[[self$iContainer[["wtpTxt0"]]$
                           get_uiInId()]]
          wtp_ <- strsplit(wtp_, ",") %>%
            unlist() %>%
            as.numeric()
          # Remove wtp values greater than max WTP set by user:
          if(!is.null(wtp_))
            if(length(wtp_) < 1)
              wtp_ <- NULL
          if(!is.null(wtp_))
            wtp_ <- wtp_[!is.na(wtp_)]
          if(any(wtp_ > max(rContainer[[sData_name()]]$
                            get_WTP())))
            wtp_ <- c(wtp_[wtp_ < max(rContainer[[sData_name()]]$
                                        get_WTP())],
                      max(rContainer[[sData_name()]]$
                            get_WTP()))
          # replace unevaluated wtp with nearest replacements:
          wtp_index_ <- purrr::map_dbl(
            .x = wtp_,
            .f = function(.wtp_ = .x) {
              which.min(
                abs(
                  .wtp_ - rContainer[[sData_name()]]$
                    get_WTP()
                )
              )
            }
          )
          wtp_ <- unique(rContainer[[sData_name()]]$
                           get_WTP()[wtp_index_])
          # Pass values to the get_Summary_table function:
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_Summary_table(
                  .beautify_ = TRUE,
                  .long_ = !input[[self$iContainer[["orinSch"]]$
                                     get_uiInId()]],
                  .wtp_ = wtp_
                ),
              .readyDT_ = TRUE
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on Summary table reset button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["RstBtn0"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Resetting Summary table..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Reset Summary Table:----
          # Pass values to the get_Summary_table function:
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_Summary_table(
                  .beautify_ = TRUE,
                  .long_ = TRUE
                ),
              .readyDT_ = TRUE
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEP update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn1"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Cost-effectiveness plane..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Update CEP:----
          # Get the reference intervention, if any:
          ref_ <- which(sData_list[[sData_name()]]$treats %in%
                          input[[self$iContainer[["getRef1"]]$
                                   get_uiInId()]])
          if(length(ref_) != 1)
            ref_ <- NULL
          # Get user defined WTP values:
          wtp_ <- input[[self$iContainer[["wtpTxt1"]]$
                           get_uiInId()]]
          wtp_ <- strsplit(wtp_, ",") %>%
            unlist() %>%
            as.numeric()
          if(!is.null(wtp_))
            wtp_ <- wtp_[!is.na(wtp_)]
          if(!is.null(wtp_))
            if(length(wtp_) < 1)
              wtp_ <- NULL
          # Pass values to the get_CEP function:
          self$iContainer[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEP(
                  .ref = ref_,
                  .zoom = input[[self$iContainer[["zomSch1"]]$
                                   get_uiInId()]],
                  .show_ICER = input[[self$iContainer[["icrSch"]]$
                                        get_uiInId()]],
                  .show_wtp = input[[self$iContainer[["wtpSch1"]]$
                                       get_uiInId()]],
                  .wtp_threshold = wtp_,
                  .legend_pos = c(input[[self$iContainer[["lgdSldX1"]]$
                                           get_uiInId()]],
                                  input[[self$iContainer[["lgdSldY1"]]$
                                           get_uiInId()]]),
                  .nudge_labels = c(input[[self$iContainer[["lblSldX"]]$
                                             get_uiInId()]],
                                    input[[self$iContainer[["lblSldY"]]$
                                             get_uiInId()]])
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEP reset button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["RstBtn1"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Resetting Cost-effectiveness plane..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Reset CEP:----
          self$iContainer[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEP()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEAC update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn2"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Cost-effectiveness acceptability curve..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Update CEAC:----
          # Get the reference intervention, if any:
          ref_ <- which(sData_list[[sData_name()]]$treats %in%
                          input[[self$iContainer[["getRef2"]]$
                                   get_uiInId()]])
          if(length(ref_) != 1)
            ref_ <- NULL
          # Get user defined WTP values:
          wtp_ <- input[[self$iContainer[["wtpTxt2"]]$
                           get_uiInId()]]
          wtp_ <- strsplit(wtp_, ",") %>%
            unlist() %>%
            as.numeric()
          # Remove wtp values greater than max WTP set by user:
          if(!is.null(wtp_))
            if(length(wtp_) < 1)
              wtp_ <- NULL
          if(!is.null(wtp_))
            wtp_ <- wtp_[!is.na(wtp_)]
          if(any(wtp_ > max(rContainer[[sData_name()]]$
                            get_WTP())))
            wtp_ = c(wtp_[wtp_ < max(rContainer[[sData_name()]]$
                                       get_WTP())],
                     max(rContainer[[sData_name()]]$
                           get_WTP()))
          wtp_ = unique(wtp_)
          # Pass values to the get_CEAC function:
          self$iContainer[["CEAC"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAC(
                  .ref = ref_,
                  .zoom = input[[self$iContainer[["zomSch2"]]$
                                   get_uiInId()]],
                  .show_wtp = input[[self$iContainer[["wtpSch2"]]$
                                       get_uiInId()]],
                  .wtp_threshold = wtp_,
                  .label_wtp = input[[self$iContainer[["wtpLSch2"]]$
                                        get_uiInId()]],
                  .legend_pos = c(input[[self$iContainer[["lgdSldX2"]]$
                                           get_uiInId()]],
                                  input[[self$iContainer[["lgdSldY2"]]$
                                           get_uiInId()]]),
                  .show_shapes = input[[self$iContainer[["shpSch2"]]$
                                          get_uiInId()]],
                  .add_CEAF = input[[self$iContainer[["ceafSch2"]]$
                                       get_uiInId()]]
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEAC reset button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["RstBtn2"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Resetting Cost-effectiveness acceptability curve..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Reset CEAC:----
          self$iContainer[["CEAC"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAC()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEAF update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn3"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Cost-effectiveness acceptability frontier..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Updating CEAF:----
          # Get user defined WTP values:
          wtp_ <- input[[self$iContainer[["wtpTxt3"]]$
                           get_uiInId()]]
          wtp_ <- strsplit(wtp_, ",") %>%
            unlist() %>%
            as.numeric()
          # Remove wtp values greater than max WTP set by user:
          if(!is.null(wtp_))
            if(length(wtp_) < 1)
              wtp_ <- NULL
          if(!is.null(wtp_))
            wtp_ <- wtp_[!is.na(wtp_)]
          if(any(wtp_ > max(rContainer[[sData_name()]]$
                            get_WTP())))
            wtp_ = c(wtp_[wtp_ < max(rContainer[[sData_name()]]$
                                       get_WTP())],
                     max(rContainer[[sData_name()]]$
                           get_WTP()))
          wtp_ = unique(wtp_)
          # Pass values to the get_CEAF function:
          self$iContainer[["CEAF"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAF(
                  .legend_pos = c(input[[self$iContainer[["lgdSldX3"]]$
                                           get_uiInId()]],
                                  input[[self$iContainer[["lgdSldY3"]]$
                                           get_uiInId()]]),
                  .wtp_threshold = wtp_,
                  .show_wtp = input[[self$iContainer[["wtpSch3"]]$
                                       get_uiInId()]],
                  .label_wtp = input[[self$iContainer[["wtpLSch3"]]$
                                        get_uiInId()]],
                  .show_shapes = input[[self$iContainer[["shpSch3"]]$
                                          get_uiInId()]],
                  .zoom = input[[self$iContainer[["zomSch3"]]$
                                   get_uiInId()]]
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEAF reset button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["RstBtn3"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Reseting Cost-effectiveness acceptability frontier..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Resetting CEAF:----
          self$iContainer[["CEAF"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEAF()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on NMB update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn4"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Net Monitory Benefit plot..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Updating NMB:----
          # Get user defined WTP values:
          wtp_ <- input[[self$iContainer[["wtpTxt4"]]$
                           get_uiInId()]]
          wtp_ <- strsplit(wtp_, ",") %>%
            unlist() %>%
            as.numeric()
          # Remove wtp values greater than max WTP set by user:
          if(!is.null(wtp_))
            if(length(wtp_) < 1)
              wtp_ <- NULL
          if(!is.null(wtp_))
            wtp_ <- wtp_[!is.na(wtp_)]
          if(any(wtp_ > max(rContainer[[sData_name()]]$
                            get_WTP())))
            wtp_ = c(wtp_[wtp_ < max(rContainer[[sData_name()]]$
                                       get_WTP())],
                     max(rContainer[[sData_name()]]$
                           get_WTP()))
          wtp_ = unique(wtp_)
          # Pass values to the get_NMB function:
          self$iContainer[["NMB"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_eNMB(
                  .legend_pos = c(input[[self$iContainer[["lgdSldX4"]]$
                                           get_uiInId()]],
                                  input[[self$iContainer[["lgdSldY4"]]$
                                           get_uiInId()]]),
                  .wtp_threshold = wtp_,
                  .show_wtp = input[[self$iContainer[["wtpSch4"]]$
                                       get_uiInId()]],
                  .label_wtp = input[[self$iContainer[["wtpLSch4"]]$
                                        get_uiInId()]],
                  .zoom = input[[self$iContainer[["zomSch4"]]$
                                   get_uiInId()]]
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on NMB reset button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["RstBtn4"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Resetting Net Monitory Benefit plot..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Resetting NMB:----
          self$iContainer[["NMB"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_eNMB()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on EVPI update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn5"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Expected value of perfect information plot..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Updating EVPI:----
          # Get user defined WTP values:
          wtp_ <- input[[self$iContainer[["wtpTxt5"]]$
                           get_uiInId()]]
          wtp_ <- strsplit(wtp_, ",") %>%
            unlist() %>%
            as.numeric()
          # Remove wtp values greater than max WTP set by user:
          if(!is.null(wtp_))
            if(length(wtp_) < 1)
              wtp_ <- NULL
          if(!is.null(wtp_))
            wtp_ <- wtp_[!is.na(wtp_)]
          if(any(wtp_ > max(rContainer[[sData_name()]]$
                            get_WTP())))
            wtp_ = c(wtp_[wtp_ < max(rContainer[[sData_name()]]$
                                       get_WTP())],
                     max(rContainer[[sData_name()]]$
                           get_WTP()))
          wtp_ = unique(wtp_)
          # Pass values to the get_EVPI function:
          self$iContainer[["EVPI"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_EVPI(
                  .legend_pos = c(input[[self$iContainer[["lgdSldX5"]]$
                                           get_uiInId()]],
                                  input[[self$iContainer[["lgdSldY5"]]$
                                           get_uiInId()]]),
                  .wtp_threshold = wtp_,
                  .show_wtp = input[[self$iContainer[["wtpSch5"]]$
                                       get_uiInId()]],
                  .label_wtp = input[[self$iContainer[["wtpLSch5"]]$
                                        get_uiInId()]],
                  .individual_evpi =
                    input[[self$iContainer[["popEVPI"]]$
                             get_uiInId()]],
                  .time_horion = input[[self$iContainer[["timEVPI"]]$
                                          get_uiInId()]],
                  .discount_rate = input[[self$iContainer[["dsRtEVPI"]]$
                                            get_uiInId()]],
                  .population = input[[self$iContainer[["popSzEVPI"]]$
                                         get_uiInId()]],
                  .zoom = input[[self$iContainer[["zomSch5"]]$
                                   get_uiInId()]]
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on EVPI reset button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["RstBtn5"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Resetting Expected value of perfect information plot..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Resetting EVPI:----
          self$iContainer[["EVPI"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_EVPI()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      #### Global handlers:----
      ##### Theme switcher:----
      observe(
        x = {
          session$setCurrentTheme(
            if(isTRUE(
              input[[self$iContainer[["themeSwch"]]$
                     get_uiInId()]]
            )) {
              bslib::bs_theme()
            } else {
              bslib::bs_theme(
                bg = "black",
                fg = "white",
                primary = "orange"
              )
            }
          )
        }
      )
    }

  ),

  private = list(

  )
)
