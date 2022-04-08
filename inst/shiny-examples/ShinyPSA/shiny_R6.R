# pacman::p_load(httr, RJSONIO, osmdata, sf, spData, ggmap, tidyverse,
#                leaflet, leaflet.extras, R6, bslib, uuid)

Healthsites_App <- R6::R6Class(
  classname = 'Healthsites_App',
  public = list(
    # Global elements:----
    container = NULL, # objects container
    # Page elements:----
    theme = NULL,
    # Input elements:----
    prettySwitch1 = NULL,
    actionButton1 = NULL,
    actionButton2 = NULL,
    inputChoices1 = NULL,
    inputChoices2 = NULL,
    ggplotPlot1 = NULL,

    # Methods:
    ## Initialise:
    initialize = function() {
      self$theme <- bslib::bs_theme(bg = "black",
                                    fg = "white",
                                    primary = "purple")
      self$container <- R6_container$new()
      self$prettySwitch1 <- prettySwitch$new(
        .label_ = "light_mode"
      )
      self$actionButton1 <- actionButton$new(
        .label_ = "Add"
      )
      self$actionButton2 <- actionButton$new(
        .label_ = "Remove"
      )
      self$inputChoices1 <- inputSelection$new(
        .label_ = "Choose a dataset:"
      )
      self$inputChoices2 <- inputSelection$new(
        .label_ = "UN-USED"
      )
      self$ggplotPlot1 <- ggplot2Plot$new(
        .label_ = "CEP")
    },
    ## UI:
    ui = function() {
      # Main UI:
      fluidPage(
        theme = self$theme,
        waiter::use_waiter(),
        # Title
        titlePanel(
          windowTitle = "ShinyPSA demo",
          div(
            class = "d-flex p-2 bd-highlight",
            img(
              src = "https://pbs.twimg.com/profile_images/959365885537456128/tC4OVmkX_400x400.jpg",
              height = "35px",
              class = "pr-2 mb-1"),
            span("ShinyPSA demo app!"),
            div(
              class = "d-flex align-items-center",
              fluidRow(
                tagList(
                  self$inputChoices1$ui_input(.choices_ = NULL),
                  self$actionButton1$ui_input()
                )
              )
            ),
            self$prettySwitch1$ui_input()
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
            self$ggplotPlot1$ui_output()
          )
        )
      )
    },
    # Server:----
    server = function(input, output, session) {
      observeEvent(
        eventExpr = input[[self$actionButton1$get_uiInId()]],
        handlerExpr = {
          print('1')
          session$setCurrentTheme(
            bslib::bs_theme(bg = "black",
                            fg = "white",
                            primary = "purple")
          )
        })

      observeEvent(
        eventExpr = input[[self$actionButton2$get_uiInId()]],
        handlerExpr = {
          print('black')
          session$setCurrentTheme(
            bslib::bs_theme()
          )
        })

      ## Data selector:----
      self$inputChoices1$server(
        session = session,
        input = input,
        output = output,
        .choices_ = c("Vaccine_PSA", "Smoking_PSA")
      )

      dataList <- reactive(
        get(
          x = input[[self$inputChoices1$get_uiInId()]],
          pos = "package:ShinyPSA"
        )
      )

      observeEvent(
        eventExpr = input[[self$actionButton1$get_uiInId()]],
        handlerExpr = {
          data_name <- reactive(
            input[[self$inputChoices1$get_uiInId()]]
          )

          self$container$add(
            .objectName_ = data_name(),
            .object_ = ShinyPSA$new(
              .effs = dataList()$e,
              .costs = dataList()$c,
              .interventions = dataList()$.interventions
            )
          )

            self$ggplotPlot1$server(
              session = session,
              input = input,
              output = output,
              .plot_ = self$container$store[[data_name()]]$get_CEP()
            )
          # output[["CEP"]] <- renderPlot({
          #   self$container$store[[data_name()]]$get_CEP()
          # })
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ## Theme switcher:----
      observe(
        x = {
          session$setCurrentTheme(
            if(isTRUE(
              input[[self$prettySwitch1$get_uiInId()]]
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

app = Healthsites_App$new()

shiny::shinyApp(app$ui(), app$server)
