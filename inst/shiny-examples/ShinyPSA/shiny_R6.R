################################################################################
#
# Script Name:        shiny_R6_elements.R
# Module Name:        Economic/PSA/demo app
# Script Description: Defines and triggers the shiny app
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

ShinyPSA_R6_App <- R6::R6Class(
  classname = 'ShinyPSA_R6_App',
  public = list(
    # Fields:
    ## Global elements:----
    oContainer = list(), # objects container
    iContainer = list(), # inputs container
    ## Page elements:----
    theme = NULL,

    # Methods:----
    ## Initialise:----
    initialize = function() {
      self$theme <- bslib::bs_theme(bg = "black",
                                    fg = "white",
                                    primary = "purple")
      self$iContainer[["themeSwch"]] <- prettySwitch$new(
        .label_ = "light_mode"
      )
      self$iContainer[["addBtn"]] <- actionButton$new(
        .label_ = "Add"
      )
      self$iContainer[["getData"]] <- inputSelection$new(
        .label_ = "Choose a dataset:"
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
      self$iContainer[["getRef"]] <- inputSelection$new(
        .label_ = "Select the reference intervention:"
      )
      self$iContainer[["icrSwch"]] <- prettySwitch$new(
        .label_ = "Show ICER information")
      self$iContainer[["wtpSwch"]] <- prettySwitch$new(
        .label_ = "Show WTP information")
      self$iContainer[["zmSldr"]] <- sliderInput$new(
        .label_ = "Test slider"
      )

    },
    ## UI:----
    ui = function() {
      fluidPage(
        theme = self$theme,
        waiter::use_waiter(),
        ### Title panel:----
        titlePanel(
          windowTitle = "ShinyPSA demo",
          div(
            class = "d-flex p-2 bd-highlight",
            #### App logo:----
            img(
              src = "https://pbs.twimg.com/profile_images/959365885537456128/tC4OVmkX_400x400.jpg",
              height = "35px",
              class = "pr-2 mb-1"),
            #### Title:----
            span("ShinyPSA demo app!"),
            div(
              class = "pt-3 pb-0 mb-0 pl-3",
              fluidRow(
                tagList(
                  #### Data drop-list:----
                  self$iContainer[["getData"]]$
                    ui_input(
                      .choices_ = NULL,
                      .class_ = "d-flex align-items-center"
                    ),
                  #### Selection confirmation button:----
                  self$iContainer[["addBtn"]]$
                    ui_input(
                      .class_ = "ml-2 pt-3 d-flex
                      align-items-center text-right"
                    )
                )
              )
            ),
            #### Theme switcher:----
            self$iContainer[["themeSwch"]]$
              ui_input()
          )
        ),
        ### Main body:----
        fluidRow(
          column(
            width = 12,
            offset = 0,
            class = "pl-5 pr-5",
            tabsetPanel(
              id = "outputs",
              tabPanel(
                title = "Summary table",
                self$iContainer[["sumTbl"]]$
                  ui_output()
              ),
              bslib::nav(
                width = 9,
                title = "CEP",
                fluidRow(
                  column(
                    width = 2,
                    self$iContainer[["getRef"]]$
                      ui_input(
                        .choices_ = NULL
                      ),
                    hr(),
                    self$iContainer[["icrSwch"]]$
                      ui_input(
                      ),
                    h4("Willingness-to-pay:"),
                    self$iContainer[["wtpSwch"]]$
                      ui_input(
                      ),
                    hr(),
                    self$iContainer[["zmSldr"]]$
                      ui_input(
                        .min_ = 0,
                        .max_ = 1,
                        .value_ = c(0.4, 0.6)
                      ),
                  ),
                  column(
                    width = 10,
                    self$iContainer[["CEP"]]$
                      ui_output()
                  )
                )
              )
            ),
          )
        )
      )
    },
    # Server:----
    server = function(input, output, session) {
      ## Data selector:----
      ### Data drop-down list:----
      self$iContainer[["getData"]]$
        server(
          session = session,
          input = input,
          output = output,
          .choices_ = c("Vaccine_PSA", "Smoking_PSA")
        )

      ### Reactive data set object:----
      dataList <- reactive(
        get(
          x = input[[self$iContainer[["getData"]]$
                       get_uiInId()]],
          pos = "package:ShinyPSA"
        )
      )

      ### Actions once user adds a dataset:----
      observeEvent(
        # eventExpr = input[[self$actionButton1$get_uiInId()]],
        eventExpr = input[[self$iContainer[["addBtn"]]$
                             get_uiInId()]],
        handlerExpr = {
          #### Reactive data name object:----
          data_name <- reactive(
            input[[self$iContainer[["getData"]]$
                     get_uiInId()]]
          )
          #### Create an instance of class ShinyPSA using the data:----
          self$oContainer[[data_name()]] <- ShinyPSA$new(
            .effs = dataList()$e,
            .costs = dataList()$c,
            .interventions = dataList()$treats
          )
          #### Retrieve the CEP from the ShinyPSA object:----
          self$iContainer[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = self$oContainer[[data_name()]]$
                get_CEP()
            )
          self$iContainer[["zmSldr"]]$
            server(
              session = session,
              input = input,
              output = output,
              .min_ = min(dataList()$e),
              .max_ = max(dataList()$c)
            )
          self$iContainer[["getRef"]]$
            server(
              session = session,
              input = input,
              output = output,
              .choices_ = dataList()$treats
            )
          #### Retrieve the Summary table from the ShinyPSA object:----
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = self$oContainer[[data_name()]]$
                get_Summary_table()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ## Theme switcher:----
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
                primary = "purple"
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

app = ShinyPSA_R6_App$new()

shiny::shinyApp(app$ui(), app$server)
