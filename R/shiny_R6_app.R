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
    tags = NULL,
    icon = NULL,
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
      self$tags <-   tags$head(
        tags$link(
          rel = "icon",
          type = "image/png",
          sizes = "32x32",
          href = self$icon
        ),
        tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    * {
      font-family: 'Inter', sans-serif;
    }
    #dataName .nav-item {
      text-transform: uppercase;
      font-weight: bold;
      font-size: 1.1em
    }
    .waiter-overlay  {
      position: fixed;
      height: 919px;
      width: 1375px;
      top: 0px;
      left: 0px;
      background-color: rgba(51, 62, 72, 0.5) !important;"))
      )
      self$icon <- "https://pbs.twimg.com/profile_images/1587395963785826305/5fUKyVVe_400x400.jpg"
      self$iContainer[["themeSwch"]] <- prettySwitch$new(
        .label_ = "Light mode"
      )
      self$iContainer[["startBtn"]] <- actionButton$new(
        .label_ = "Start"
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
      self$iContainer[["sumTbl"]] <-  dataTableGT$new(
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
      self$iContainer[["EVPPI"]] <- ggplot2Plot$new(
        .label_ = "EVPPI"
      )
      self$iContainer[["EVPPITbl"]] <-  dataTableDT$new(
        .label_ = "EVPPI summary table"
      )
      self$iContainer[["EVPPISTbl"]] <-  dataTableDT$new(
        .label_ = "Subset EVPPI summary table"
      )
      self$iContainer[["PSA_stability"]] <- ggplot2Plot$new(
        .label_ = "PSA Stability"
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
      self$iContainer[["popEVPI0"]] <- prettySwitch$new(
        .label_ = "Estimate individual EVPI"
      )
      self$iContainer[["timEVPI0"]] <- numericInput$new(
        .label_ = "Population EVPI time horizon:"
      )
      self$iContainer[["dsRtEVPI0"]] <- numericInput$new(
        .label_ = "Population discount rate:"
      )
      self$iContainer[["popSzEVPI0"]] <- numericInput$new(
        .label_ = "Population EVPI sample size:"
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
      self$iContainer[["updBtn6"]] <- actionButton$new(
        .label_ = "Update EVPPI table"
      )
      self$iContainer[["lambdaEVPPI"]] <- numericInput$new(
        .label_ = "Maiximum acceptable ICER for EVPPI:"
      )
      self$iContainer[["PcntSch"]] <- prettySwitch$new(
        .label_ = "Show percentage values on bar plot"
      )
      self$iContainer[["MinPcnt"]] <- numericInput$new(
        .label_ = "Minimum EVPPI/EVPI (%) for parameters in bar plot:"
      )
      self$iContainer[["MaxParam"]] <- numericInput$new(
        .label_ = "Maximum number of parameters shown in bar plot:"
      )
      self$iContainer[["popEVPPI"]] <- prettySwitch$new(
        .label_ = "Estimate individual EVPPI"
      )
      self$iContainer[["timEVPPI"]] <- numericInput$new(
        .label_ = "Population EVPPI time horizon:"
      )
      self$iContainer[["dsRtEVPPI"]] <- numericInput$new(
        .label_ = "Population discount rate:"
      )
      self$iContainer[["popSzEVPPI"]] <- numericInput$new(
        .label_ = "Population EVPPI sample size:"
      )
      self$iContainer[["updBtn7"]] <- actionButton$new(
        .label_ = "Update Subset EVPPI plot"
      )
      self$iContainer[["lambdaEVPPIS"]] <- numericInput$new(
        .label_ = "Maiximum acceptable ICER for Subset EVPPI:"
      )
      self$iContainer[["subsetEVPPIS"]] <- pickerInput$new(
        .label_ = "Parameters' subset for Subset EVPPI:"
      )
      self$iContainer[["popEVPPIS"]] <- prettySwitch$new(
        .label_ = "Estimate individual Subset EVPPI"
      )
      self$iContainer[["timEVPPIS"]] <- numericInput$new(
        .label_ = "Population Subset EVPPI time horizon:"
      )
      self$iContainer[["dsRtEVPPIS"]] <- numericInput$new(
        .label_ = "Population discount rate:"
      )
      self$iContainer[["popSzEVPPIS"]] <- numericInput$new(
        .label_ = "Population Subset EVPPI sample size:"
      )
      self$iContainer[["updBtn8"]] <- actionButton$new(
        .label_ = "Update EVPPI plot"
      )
      self$iContainer[["updBtn9"]] <- actionButton$new(
        .label_ = "Update stability plot"
      )
      self$iContainer[["zomSch6"]] <- prettySwitch$new(
        .label_ = "Zoom to min/max X axis values"
      )
      self$iContainer[["zomLbX"]] <- numericInput$new(
        .label_ = "Zoom: X axis lower bound value:"
      )
      self$iContainer[["zomUbX"]] <- numericInput$new(
        .label_ = "Zoom: X axis upper bound value:"
      )
      self$iContainer[["costs"]] <-  dataTableDT$new(
        .label_ = "Summarised Costs:"
      )
      self$iContainer[["effects"]] <-  dataTableDT$new(
        .label_ = "Summarised Effects:"
      )
      self$iContainer[["parameters"]] <-  dataTableDT$new(
        .label_ = "PSA Simulated Parameters:"
      )
    },
    ### UI:----
    ui = function() {
      ui <- fluidPage(
        self$tags,
        theme = self$theme,
        waiter::use_waiter(),
        shinyjs::useShinyjs(), # to remove landing page
        #### Title panel:----
        titlePanel(
          windowTitle = "ShinyPSA demo",
          tagList(
            div(
              class = "d-flex p-4 bd-highlight",
              ##### App logo:----
              img(
                src = self$icon,
                height = "90px",
                class = "pr-2 pb-0"),
              div(
                style = "margin-right: 1rm; margin-top: auto;
                margin-bottom: auto;",
                ##### Title:----
                span("ShinyPSA demo app!"),
                ##### Theme switcher:----
              ),
              div(
                style = "margin-top: auto; margin-bottom: auto;
                margin-left: auto; margin-right: 0;",
                self$iContainer[["themeSwch"]]$
                  ui_input(
                    .value_ = TRUE
                  )
              )
            )
          )
        ),
        #### Landing page:----
        div(
          id = "landing-page",
          style = "
    position: absolute;
    right:0;left:0;top:0;bottom:0;
    background-color: white;
    background: radial-gradient(circle, white, #cecece);
    background-image: url('https://pbs.twimg.com/profile_images/959365885537456128/tC4OVmkX_400x400.jpg'), radial-gradient(circle, white, #cecece);
    background-size: auto;
    background-position: 30% 30%;
    background-repeat: no-repeat;
    z-index: 9;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content:center;
    color: black;
    text-shadow: 2px 2px 8px white;",
          h1("OHID project"),
          h3("ShinyPSA demo app!"),
          self$iContainer[["startBtn"]]$
            ui_input(
              .style_ = NULL,
              .class_ = "btn-primary btn-lg mt-5"
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
                          # Input: upload effects file:
                          div(
                            style = "display: flex;
                          margin-top: 0rem !important;",
                            class = "pl-4 d-flex align-items-start",
                            fileInput(
                              buttonLabel = "Browse parameters file...",
                              inputId = "p",
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
                    width = 12,
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
                        self$iContainer[["popEVPI0"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["timEVPI0"]]$
                          ui_input(
                            .min_ = 1,
                            .max_ = 100,
                            .value_ = 1
                          ),
                        self$iContainer[["dsRtEVPI0"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.035
                          ),
                        self$iContainer[["popSzEVPI0"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1e8,
                            .value_ = 1.5e3
                          ),
                        self$iContainer[["RstBtn0"]]$
                          ui_input(
                            .width_ = "100%"
                          )
                      ),
                      column(
                        width = 10,
                        div(
                          class = "pb-5 pt-2 pr-0
                          d-flex align-items-center",
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
                    title = "CEAC",
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
                    title = "CEAF",
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
                    title = "eNMB",
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
                    title = "EVPI",
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
                  ),
                  ###### Partial EVPI (EVPPI):----
                  bslib::nav(
                    width = 9,
                    title = "EVPPI",
                    tabsetPanel(
                      id = "EVPPI_sub_panel",
                      bslib::nav(
                        title = "EVPPI Summary Table",
                        fluidRow(
                          column(
                            width = 2,
                            self$iContainer[["updBtn6"]]$
                              ui_input(
                                .width_ = "100%"
                              ),
                            hr(),
                            self$iContainer[["lambdaEVPPI"]]$
                              ui_input(
                                .min_ = 0,
                                .max_ = 1e6,
                                .value_ = 30000
                              ),
                            self$iContainer[["popEVPPI"]]$
                              ui_input(
                                .class_ = "pl-2 flex-fill text-left"
                              ),
                            self$iContainer[["timEVPPI"]]$
                              ui_input(
                                .min_ = 1,
                                .max_ = 100,
                                .value_ = 1
                              ),
                            self$iContainer[["dsRtEVPPI"]]$
                              ui_input(
                                .min_ = 0,
                                .max_ = 1,
                                .value_ = 0.035
                              ),
                            self$iContainer[["popSzEVPPI"]]$
                              ui_input(
                                .min_ = 0,
                                .max_ = 1e8,
                                .value_ = 1.5e3
                              )
                          ),
                          column(
                            width = 10,
                            self$iContainer[["EVPPITbl"]]$
                              ui_output()
                          )
                        )
                      ),
                      bslib::nav(
                        title = "EVPPI Bar Plot",
                        fluidRow(
                          column(
                            width = 2,
                            self$iContainer[["updBtn8"]]$
                              ui_input(
                                .width_ = "100%"
                              ),
                            hr(),
                            self$iContainer[["PcntSch"]]$
                              ui_input(
                                .class_ = "pl-2 flex-fill text-left"
                              ),
                            self$iContainer[["MinPcnt"]]$
                              ui_input(
                                .min_ = 0,
                                .max_ = 100,
                                .value_ = NULL
                              ),
                            self$iContainer[["MaxParam"]]$
                              ui_input(
                                .min_ = 0,
                                .max_ = 100,
                                .value_ = 100
                              )
                          ),
                          column(
                            width = 10,
                            self$iContainer[["EVPPI"]]$
                              ui_output()
                          )
                        )
                      )
                    )
                  ),
                  ###### Subset EVPPI:----
                  bslib::nav(
                    width = 9,
                    title = "Subset EVPPI",
                    fluidRow(
                      column(
                        width = 2,
                        self$iContainer[["updBtn7"]]$
                          ui_input(
                            .width_ = "100%"
                          ),
                        hr(),
                        self$iContainer[["lambdaEVPPIS"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1e6,
                            .value_ = 30000
                          ),
                        self$iContainer[["subsetEVPPIS"]]$
                          ui_input(
                            .choices_ = NULL,
                            .info_ = "Select two or more parameters",
                            .more_info_ = "More parameters = more processing"
                          ),
                        self$iContainer[["popEVPPIS"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["timEVPPIS"]]$
                          ui_input(
                            .min_ = 1,
                            .max_ = 100,
                            .value_ = 1
                          ),
                        self$iContainer[["dsRtEVPPIS"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.035
                          ),
                        self$iContainer[["popSzEVPPIS"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1e8,
                            .value_ = 1.5e3
                          )
                      ),
                      column(
                        width = 10,
                        self$iContainer[["EVPPISTbl"]]$
                          ui_output()
                      )
                    )
                  ),
                  ###### PSA Inputs checks and Outputs stability:----
                  bslib::nav(
                    width = 9,
                    title = "Checks",
                    tabsetPanel(
                      id = "Checks_sub_panel",
                      bslib::nav(
                        title = "PSA Stability plots",
                        fluidRow(
                          column(
                            width = 2,
                            self$iContainer[["updBtn9"]]$
                              ui_input(
                                .width_ = "100%"
                              ),
                            hr(),
                            self$iContainer[["zomSch6"]]$
                              ui_input(
                                .class_ = "pl-2 flex-fill text-left"
                              ),
                            self$iContainer[["zomLbX"]]$
                              ui_input(
                                .min_ = 0,
                                .max_ = 1e4,
                                .value_ = 0
                              ),
                            self$iContainer[["zomUbX"]]$
                              ui_input(
                                .min_ = 100,
                                .max_ = 1e6,
                                .value_ = 1000
                              )
                          ),
                          column(
                            width = 10,
                            self$iContainer[["PSA_stability"]]$
                              ui_output()
                          )
                        )
                      ),
                      bslib::nav(
                        title = "Inputs",
                        fluidRow(
                          column(
                            width = 10,
                            offset = 1,
                            h3("PSA outputs:"),
                            br(),
                            HTML('<hr style="color: black;">'),
                            h4("Costs:"),
                            self$iContainer[["costs"]]$
                              ui_output(),
                            hr(),
                            HTML('<hr style="color: black;">'),
                            h4("Effects:"),
                            self$iContainer[["effects"]]$
                              ui_output(),
                            hr(),
                            HTML('<hr style="color: black;">'),
                            h3("PSA inputs:"),
                            self$iContainer[["parameters"]]$
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
        )
      )
      ### Secure UI:----
      if(self$make_secure) {
        ui <- shinymanager::secure_app(
          ui = ui,
          theme = bslib::bs_theme()
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
      #### Exit landing page:----
      observeEvent(
        eventExpr = input[[self$iContainer[["startBtn"]]$
                             get_uiInId()]],
        handlerExpr = {
          shinyjs::runjs("
    function removeFadeOut( el) {
      el.style.transition = 'opacity 1s ease';
      el.style.opacity = 0;
      setTimeout(function() {
        el.parentNode.removeChild(el);
      }, 1000);
    }
    const landingPage = document.getElementById('landing-page');
    removeFadeOut(landingPage);"
          )
        })
      #### Data handlers:----
      ##### Data drop-down list:----
      self$iContainer[["getData"]]$
        server(
          session = session,
          input = input,
          output = output,
          .choices_ = c("Hyperphosphatemia_PSA", "Brennan_1K_PSA",
                        "Brennan_10K_PSA", "Brennan_50K_PSA",
                        "Hypothetical_PSA", "Clopidogrel_PSA")
        )
      ##### Uploaded data:----
      ##### Input validation:----
      ###### defining some reactive values to reduce data reading times:
      c_rExp <- reactive({
        req(input$c)
        read_csv(
          file = input$c$datapath,
          show_col_types = FALSE
          )
        })
      e_rExp <- reactive({
        req(input$e)
        read_csv(
          file = input$e$datapath,
          show_col_types = FALSE
        )
      })
      p_rExp <- reactive({
        req(input$p)
        read_csv(
          file = input$p$datapath,
          show_col_types = FALSE
        )
      })
      ###### define a parent input validator:
      iv <- shinyvalidate::InputValidator$new()
      ###### define children input validators:
      costs_iv <- shinyvalidate::InputValidator$new()
      effs_iv <- shinyvalidate::InputValidator$new()
      params_iv <- shinyvalidate::InputValidator$new()
      dims_iv <- shinyvalidate::InputValidator$new()
      simulations_c_iv <- shinyvalidate::InputValidator$new()
      simulations_e_iv <- shinyvalidate::InputValidator$new()
      ###### add children input validators to the parent one:
      iv$add_validator(costs_iv)
      iv$add_validator(effs_iv)
      iv$add_validator(params_iv)
      iv$add_validator(dims_iv)
      iv$add_validator(simulations_c_iv)
      iv$add_validator(simulations_e_iv)
      ###### add rules to the children input validators:
      costs_iv$add_rule(
        inputId = "c",
        rule = shinyvalidate::sv_optional())
      costs_iv$add_rule(
        inputId = "c",
        rule = shinyvalidate::compose_rules(
          ~ {ShinyPSA::check_missings_(
            .data_ = c_rExp(), .label_ = "Costs")},
          ~ {ShinyPSA::check_numerics_(
            .data_ = c_rExp(), .label_ = "Costs")},
          ~ {ShinyPSA::check_options_(
            .data_ = c_rExp(), .label_ = "costs")}))
      effs_iv$add_rule(
        inputId = "e",
        rule = shinyvalidate::sv_optional())
      effs_iv$add_rule(
        inputId = "e",
        rule = shinyvalidate::compose_rules(
          ~ {ShinyPSA::check_missings_(
            .data_ = e_rExp(), .label_ = "Effects")},
          ~ {ShinyPSA::check_numerics_(
            .data_ = e_rExp(), .label_ = "Effects")},
          ~ {ShinyPSA::check_options_(
            .data_ = e_rExp(), .label_ = "effects")}))
      params_iv$add_rule(
        inputId = "p",
        rule = shinyvalidate::sv_optional())
      params_iv$add_rule(
        inputId = "p",
        rule = shinyvalidate::compose_rules(
          ~ {ShinyPSA::check_missings_(
            .data_ = p_rExp(), .label_ = "Parameters")},
          ~ {ShinyPSA::check_numerics_(
            .data_ = p_rExp(), .label_ = "Parameters")},
          ~ {ShinyPSA::check_uniqueness_(
            .data_ = p_rExp(), .label_ = "parameters")}))
      dims_iv$condition(~ (shiny::isTruthy(input$c) &
                             shiny::isTruthy(input$e)))
      dims_iv$add_rule(
        inputId = "c",
        rule = ~ {
          ShinyPSA::check_PSA_inputs(
            .costs_ = c_rExp(),
            .effs_ = e_rExp(),
            .params_ = NULL,
            .id_ = "c")
        })
      dims_iv$add_rule(
        inputId = "e",
        rule = ~ {
          check_PSA_inputs(
            .costs_ = c_rExp(),
            .effs_ = e_rExp(),
            .params_ = NULL,
            .id_ = "e")
        })
      simulations_c_iv$condition(~
          (shiny::isTruthy(input$p) & shiny::isTruthy(input$c))
      )
      simulations_c_iv$add_rule(
        inputId = "c",
        rule = ~ {
          ShinyPSA::check_PSA_inputs(
            .costs_ = c_rExp(),
            .effs_ = NULL,
            .params_ = p_rExp(),
            .id_ = "c")
        })
      simulations_c_iv$add_rule(
        inputId = "p",
        rule = ~ {
          check_PSA_inputs(
            .costs_ = c_rExp(),
            .effs_ = NULL,
            .params_ = p_rExp(),
            .id_ = "p")
        })
      simulations_e_iv$condition(~
        (shiny::isTruthy(input$p) & shiny::isTruthy(input$e))
      )
      simulations_e_iv$add_rule(
        inputId = "e",
        rule = ~ {
          check_PSA_inputs(
            .costs_ = NULL,
            .effs_ = e_rExp(),
            .params_ = p_rExp(),
            .id_ = "e")
        })
      simulations_e_iv$add_rule(
        inputId = "p",
        rule = ~ {
          check_PSA_inputs(
            .costs_ = NULL,
            .effs_ = e_rExp(),
            .params_ = p_rExp(),
            .id_ = "p")
        })

      ###### start displaying errors in the UI:
      iv$enable()

      ##### Uploaded data list:----
      uData_list <- reactive({
        req(
          iv$is_valid()
        )
        list(
          "c" = c_rExp(),
          "e" = e_rExp(),
          "p" = p_rExp(),
          "treats" = tryCatch(
            expr = {
              strsplit(input[[self$iContainer[["intrNme"]]$
                                get_uiInId()]],
                       ",") %>%
                unlist() %>%
                gsub(pattern = " ", replacement = "")
            }, error = function(e) {
              NULL
            })
        )
      })

      ##### Reactive/static data set, data source and data name objects:----
      sData_list <- reactiveValues()
      sData_name <- reactiveVal()
      sData_source <- reactiveVal()

      ##### Reactive to control reaction to action button:----
      buttons_reactive <- reactiveValues()

      ##### Reactive container for R6 objects:----
      rContainer <- reactiveValues()

      ##### Reactives to react to radio buttons:----
      ###### Add button control:----
      observeEvent(
        eventExpr = input[[self$iContainer[["addBtn"]]$get_uiInId()]],
        handlerExpr = {
          ###### Define source of data set for later:----
          sData_source("selected")
          ###### Store name of data set for later:----
          sData_name(
            input[[self$iContainer[["getData"]]$get_uiInId()]]
          )
          ###### Store data set for later:----
          sData_list[[sData_name()]] <- get(
            x = input[[self$iContainer[["getData"]]$get_uiInId()]],
            pos = "package:ShinyPSA"
          )
          ###### Switch reactive object to trigger main functions:----
          buttons_reactive[["add"]] <- TRUE
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)
      ###### Upload button control:----
      observeEvent(
        eventExpr = input[[self$iContainer[["uplBtn"]]$get_uiInId()]],
        handlerExpr = {
          # if all input validations passed:
          if(iv$is_valid()) {
            ###### Define source of data set for later:----
            sData_source("uploaded")
            ###### Store name of data set for later:----
            sData_name("uploaded data")
            ###### Store data set for later:----
            sData_list[[sData_name()]] <- uData_list()
            ###### Switch reactive object to trigger main functions:----
            buttons_reactive[["upload"]] <- TRUE
          } else {
            # if input validation failed:
            shiny::showNotification(
              ui = "Please fix issues in the inputs tab!",
              duration = 10,
              type = "error"
            )
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)

      #### Renderers:----
      ##### Actions on add or upload button:----
      observeEvent(
        eventExpr = c(buttons_reactive[["add"]], buttons_reactive[["upload"]]),
        handlerExpr = {
          ###### Reset reactive controls:----
          buttons_reactive[["add"]] <- NULL
          buttons_reactive[["upload"]] <- NULL
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = glue::glue("Summarising {sData_source()} PSA data...")
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Render the name of the summarised data:----
          output$selectedData <- renderText({
            sData_name()
          })
          ###### Create an instance of class ShinyPSA using the data:----
          rContainer[[sData_name()]] <- ShinyPSA$new(
            .effs = sData_list[[sData_name()]]$e,
            .costs = sData_list[[sData_name()]]$c,
            .params = sData_list[[sData_name()]]$p,
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
                  .long_ = TRUE,
                  .latex_ = TRUE,
                  .latex_title_ = gt::md("**PSA Results**"),
                  .latex_subtitle_ = gt::md("**_Sumamry table_**"),
                  .latex_code_ = FALSE,
                  .footnotes_sourcenotes_ = TRUE,
                  .all_sourcenotes_ = FALSE,
                  .dominance_footnote_ = FALSE,
                  .subset_tab_ = FALSE,
                  .subset_group_ = c("All")
                ),
              .width_ = "100%",
              .height_ = "100%",
              .align_ = NULL
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

          ###### Retrieve parameters names for Subset EVPPI options:----
          self$iContainer[["subsetEVPPIS"]]$
            server(
              session = session,
              input = input,
              output = output,
              .choices_ = rContainer[[sData_name()]]$
                get_params_names()
            )

          ###### Retrieve Stability plots from the ShinyPSA object:----
          self$iContainer[["PSA_stability"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_PSA_stabl_plots()
            )

          ###### Retrieve costs:----
          self$iContainer[["costs"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = sData_list[[sData_name()]]$c,
              .readyDT_ = FALSE
            )

          ###### Retrieve effects:----
          self$iContainer[["effects"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = sData_list[[sData_name()]]$e,
              .readyDT_ = FALSE
            )

          ###### Retrieve parameters:----
          self$iContainer[["parameters"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = sData_list[[sData_name()]]$p,
              .readyDT_ = FALSE
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
                  .wtp_ = wtp_,
                  .individual_evpi_ =
                    input[[self$iContainer[["popEVPI0"]]$
                             get_uiInId()]],
                  .evpi_population_ =
                    input[[self$iContainer[["popSzEVPI0"]]$
                             get_uiInId()]],
                  .discount_rate_ =
                    input[[self$iContainer[["dsRtEVPI0"]]$
                             get_uiInId()]],
                  .time_horion_ =
                    input[[self$iContainer[["timEVPI0"]]$
                             get_uiInId()]],
                  .latex_ = TRUE,
                  .latex_code_ = FALSE,
                  .footnotes_sourcenotes_ = TRUE,
                  .all_sourcenotes_ = FALSE,
                  .dominance_footnote_ = FALSE,
                  .subset_tab_ = FALSE,
                  .subset_group_ = c("All")
                )
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
                  .long_ = TRUE,
                  .latex_ = TRUE,
                  .latex_code_ = FALSE,
                  .footnotes_sourcenotes_ = TRUE,
                  .all_sourcenotes_ = FALSE,
                  .dominance_footnote_ = FALSE,
                  .subset_tab_ = FALSE,
                  .subset_group_ = c("All")
                )
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

      ##### Actions on EVPPI update table button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn6"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Expected value of partial perfect information table..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Updating EVPPI:----
          ####### Pass values to the get_EVPPI_results function:----
          self$iContainer[["EVPPITbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_EVPPI_results(
                  .MAICER_ = input[[self$iContainer[["lambdaEVPPI"]]$
                                      get_uiInId()]],
                  .individual_evppi_ =
                    input[[self$iContainer[["popEVPPI"]]$
                             get_uiInId()]],
                  .discount_rate_ = input[[self$iContainer[["dsRtEVPPI"]]$
                                             get_uiInId()]],
                  .evppi_population_ =
                    input[[self$iContainer[["popSzEVPPI"]]$
                             get_uiInId()]],
                  .time_horion_ = input[[self$iContainer[["timEVPPI"]]$
                                           get_uiInId()]],
                  .session = session
                  )[[1]],
              .readyDT_ = TRUE
            )

        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on EVPPI update plot button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn8"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Expected value of partial perfect information plot..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Updating EVPPI:----
          ####### Pass values to the get_EVPPI_plot function:----
          self$iContainer[["EVPPI"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_EVPPI_plot(
                  .show_percent = input[[self$iContainer[["PcntSch"]]$
                                           get_uiInId()]],
                  .min_percent = input[[self$iContainer[["MinPcnt"]]$
                                          get_uiInId()]],
                  .params_num = input[[self$iContainer[["MaxParam"]]$
                                         get_uiInId()]]
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on Subset EVPPI update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn7"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating Expected value of subset perfect information table..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ####### Pass values to the get_EVPPI_results function:----
          self$iContainer[["EVPPISTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_Sub_EVPPI_results(
                  .MAICER_ = input[[self$iContainer[["lambdaEVPPIS"]]$
                                      get_uiInId()]],
                  .set_names = input[[self$iContainer[["subsetEVPPIS"]]$
                                        get_uiInId()]],
                  .individual_evppi_ =
                    input[[self$iContainer[["popEVPPIS"]]$
                             get_uiInId()]],
                  .discount_rate_ =
                    input[[self$iContainer[["dsRtEVPPIS"]]$
                             get_uiInId()]],
                  .evppi_population_ =
                    input[[self$iContainer[["popSzEVPPIS"]]$
                             get_uiInId()]],
                  .time_horion_ = input[[self$iContainer[["timEVPPIS"]]$
                                           get_uiInId()]],
                  .session = session
                )[[1]],
              .readyDT_ = TRUE
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on Stability plot button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["updBtn9"]]$
                              get_uiInId()]]),
        handlerExpr = {
          ###### Waiter:----
          waiter <- waiter::Waiter$new(
            html = self$waiter_dev(
              .info_ = "Updating PSA stability plots..."
            ),
            hide_on_render  = FALSE
          )
          waiter$show()
          on.exit(waiter$hide())
          ###### Updating PSA stability plots:----
          ####### Pass values to the get_PSA_stabl_plots function:----
          self$iContainer[["PSA_stability"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_PSA_stabl_plots(
                  .zoom = input[[self$iContainer[["zomSch6"]]$
                                   get_uiInId()]],
                  .zoom_cords = c(
                    input[[self$iContainer[["zomLbX"]]$
                             get_uiInId()]],
                    input[[self$iContainer[["zomUbX"]]$
                             get_uiInId()]]
                  )
                )
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
              bslib::bs_theme(
                bootswatch = "minty")
            } else {
              bslib::bs_theme(
                bootswatch = "cyborg" #"darkly" #"solar" #"solar" #"sandstone"
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
