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
        .label_ = "Show ICER information"
      )
      self$iContainer[["zomSwch"]] <- prettySwitch$new(
        .label_ = "Zoom to min/max values"
      )
      self$iContainer[["wtpSwch"]] <- prettySwitch$new(
        .label_ = "Show WTP information"
      )
      self$iContainer[["CEPBtn"]] <- actionButton$new(
        .label_ = "Update CEP plot"
      )
      self$iContainer[["CEPRstBtn"]] <- actionButton$new(
        .label_ = "Reset CEP plot"
      )
      self$iContainer[["lgdSldrX"]] <- sliderInput$new(
        .label_ = "Legend X coordinate:"
      )
      self$iContainer[["lgdSldrY"]] <- sliderInput$new(
        .label_ = "Legend Y coordinate:"
      )
      # self$iContainer[["zmSldrX"]] <- sliderInput$new(
      #   .label_ = "Zoom slider:"
      # )

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
              id = "dataName",
              tabPanel(
                title = uiOutput(
                  outputId = "selectedData",
                  inline = TRUE
                ),
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
                        self$iContainer[["CEPBtn"]]$
                          ui_input(
                            # .class_ = " align-items-left text-right",
                            .width_ = "100%"
                          ),
                        self$iContainer[["getRef"]]$
                          ui_input(
                            .choices_ = NULL
                          ),
                        self$iContainer[["icrSwch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpSwch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["zomSwch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["lgdSldrX"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.8
                          ),
                        self$iContainer[["lgdSldrY"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.2
                          ),
                        # h4("Zoom controls:"),
                        # self$iContainer[["zmSldrX"]]$
                        #   ui_input(
                        #     .min_ = 0,
                        #     .max_ = 1,
                        #     .value_ = c(0.4, 0.6)
                        #   ),
                        # hr(),
                        self$iContainer[["CEPRstBtn"]]$
                          ui_input(
                            # .class_ = " align-items-left text-right",
                            .width_ = "100%"
                          ),
                      ),
                      column(
                        width = 10,
                        self$iContainer[["CEP"]]$
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
      data_list <- reactive(
        get(
          x = input[[self$iContainer[["getData"]]$
                       get_uiInId()]],
          pos = "package:ShinyPSA"
        )
      )
      ### Reactive data name object:----
      data_name <- reactive(
        input[[self$iContainer[["getData"]]$
                 get_uiInId()]]
      )

      ### Reactive/static data set and data name object:----
      sData_list <- reactiveValues()
      sData_name <- reactiveVal()

      ### Reactive container for R6 objects:----
      rContainer <- reactiveValues()

      ### Actions on add button:----
      observeEvent(
        eventExpr = input[[self$iContainer[["addBtn"]]$
                             get_uiInId()]],
        handlerExpr = {
          #### Store name and data set for later:----
          sData_name(
            data_name()
          )
          sData_list[[sData_name()]] <- data_list()
          #### Render the name of the summarised data:----
          output$selectedData <- renderText({
            sData_name()
          })
          #### Create an instance of class ShinyPSA using the data:----
          rContainer[[sData_name()]] <- ShinyPSA$new(
            .effs = data_list()$e,
            .costs = data_list()$c,
            .interventions = data_list()$treats
          )
          #### Retrieve the CEP from the ShinyPSA object:----
          self$iContainer[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[data_name()]]$
                get_CEP()
            )
          # self$iContainer[["zmSldrX"]]$
          #   server(
          #     session = session,
          #     input = input,
          #     output = output,
          #     .min_ = min(data_list()$e),
          #     .max_ = max(data_list()$c)
          #   )
          self$iContainer[["getRef"]]$
            server(
              session = session,
              input = input,
              output = output,
              .choices_ = c("NULL", data_list()$treats)
            )
          #### Retrieve the Summary table from the ShinyPSA object:----
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[data_name()]]$
                get_Summary_table()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ### Actions on CEP update button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["CEPBtn"]]$
                              get_uiInId()]]),
        handlerExpr = {
          # Get the reference intervention, if any:
          ref_ <- which(sData_list[[sData_name()]]$treats %in%
                          input[[self$iContainer[["getRef"]]$
                                   get_uiInId()]])
          if(length(ref_) != 1)
            ref_ <- NULL
          print(ref_)
          # Pass values to the get_CEP function:
          self$iContainer[["CEP"]]$
            server(
              session = session,
              input = input,
              output = output,
              .plot_ = rContainer[[sData_name()]]$
                get_CEP(
                  .ref = ref_,
                  .zoom = input[[self$iContainer[["zomSwch"]]$
                                   get_uiInId()]],
                  .show_ICER = input[[self$iContainer[["icrSwch"]]$
                                        get_uiInId()]],
                  .show_wtp = input[[self$iContainer[["wtpSwch"]]$
                                       get_uiInId()]],
                  .legend_pos = c(input[[self$iContainer[["lgdSldrX"]]$
                                           get_uiInId()]],
                                  input[[self$iContainer[["lgdSldrY"]]$
                                           get_uiInId()]])
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ### Actions on CEP reset button:----
      observeEvent(
        eventExpr = (input[[self$iContainer[["CEPRstBtn"]]$
                              get_uiInId()]]),
        handlerExpr = {
          print(sData_name()) # try reset
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
