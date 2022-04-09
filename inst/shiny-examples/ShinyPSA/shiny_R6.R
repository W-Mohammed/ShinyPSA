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
    oContainer = NULL, # objects container
    iContainer = NULL, # inputs container
    ## Page elements:----
    theme = NULL,
    ## Input elements:----
    actionButton2 = NULL,
    inputChoices2 = NULL,

    # Methods:----
    ## Initialise:----
    initialize = function() {
      self$theme <- bslib::bs_theme(bg = "black",
                                    fg = "white",
                                    primary = "purple")
      self$oContainer <- R6_container$new()
      self$iContainer <- R6_container$new()
      self$iContainer$add(
        .objectName_ = "themeSwch",
        .object_ = prettySwitch$new(
          .label_ = "light_mode"
        )
      )
      self$iContainer$add(
        .objectName_ = "addBtn",
        .object_ = actionButton$new(
          .label_ = "Add"
        )
      )
      self$iContainer$add(
        .objectName_ = "getData",
        .object_ = inputSelection$new(
          .label_ = "Choose a dataset:"
        )
      )
      self$iContainer$add(
        .objectName_ = "CEP",
        .object_ = ggplot2Plot$new(
          .label_ = "CEP"
        )
      )
      self$iContainer$add(
        .objectName_ = "sumTbl",
        .object_ = dataTableDT$new(
          .label_ = "PSA summary table"
        )
      )

      self$actionButton2 <- actionButton$new(
        .label_ = "Remove"
      )
      self$inputChoices2 <- inputSelection$new(
        .label_ = "UN-USED"
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
                  self$iContainer$store[["getData"]]$
                    ui_input(
                      .choices_ = NULL,
                      .class_ = "d-flex align-items-center"
                    ),
                  #### Selection confirmation button:----
                  self$iContainer$store[["addBtn"]]$
                    ui_input(
                      .class_ = "ml-2 pt-3 d-flex
                      align-items-center text-right"
                    )
                )
              )
            ),
            #### Theme switcher:----
            self$iContainer$store[["themeSwch"]]$
              ui_input()
          )
        ),
        fluidRow(
          column(
            width = 2,
            offset = 0,
            class = "mt-2 ml-2", # width 11 to work
            tagList(
              tags$hr(),
              self$actionButton2$ui_input()
            )
          ),
          column(
            width = 9,
            offset = 0,
            class = "mr-5 pb-5",
            tabsetPanel(
              id = "outputs"
            ),
            self$iContainer$store[["CEP"]]$
              ui_output(),
            self$iContainer$store[["sumTbl"]]$
              ui_output()

          )
        )
      )
    },
    # Server:----
    server = function(input, output, session) {
      ## Data selector:----
      ### Data drop-down list:----
      self$iContainer$store[["getData"]]$
        server(
          session = session,
          input = input,
          output = output,
          .choices_ = c("Vaccine_PSA", "Smoking_PSA")
        )

      ### Reactive data set object:----
      dataList <- reactive(
        get(
          x = input[[self$iContainer$store[["getData"]]$
                       get_uiInId()]],
          pos = "package:ShinyPSA"
        )
      )

      ### Actions once user adds a dataset:----
      observeEvent(
        # eventExpr = input[[self$actionButton1$get_uiInId()]],
        eventExpr = input[[self$iContainer$store[["addBtn"]]$
                             get_uiInId()]],
        handlerExpr = {
          #### Reactive data name object:----
          data_name <- reactive(
            input[[self$iContainer$store[["getData"]]$
                     get_uiInId()]]
          )
          #### Create an instance of class ShinyPSA using the data:----
          self$oContainer$add(
            .objectName_ = data_name(),
            .object_ = ShinyPSA$new(
              .effs = dataList()$e,
              .costs = dataList()$c,
              .interventions = dataList()$treats
            )
          )
          #### Retrieve the CEP from the ShinyPSA object:----
          self$iContainer$store[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = self$oContainer$store[[data_name()]]$
                get_CEP()
            )
          #### Retrieve the Summary table from the ShinyPSA object:----
          self$iContainer$store[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = self$oContainer$store[[data_name()]]$
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
              # input[[self$prettySwitch1$get_uiInId()]]
              input[[self$iContainer$store[["themeSwch"]]$
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
