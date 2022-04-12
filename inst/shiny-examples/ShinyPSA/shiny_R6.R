################################################################################
#
# Script Name:        shiny_R6_elements.R
# Module Name:        Economic/PSA/demo app
# Script Description: Defines and triggers the shiny app
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

# Define a shiny R6 class:----
ShinyPSA_R6_App <- R6::R6Class(
  classname = 'ShinyPSA_R6_App',
  public = list(
    ## Fields:----
    ### Global elements:----
    iContainer = list(), # inputs container
    ### Page elements:----
    theme = NULL,

    ## Methods:----
    ### Initialise:----
    initialize = function() {
      self$theme <- bslib::bs_theme(bg = "black",
                                    fg = "white",
                                    primary = "purple")
      self$iContainer[["themeSwch"]] <- prettySwitch$new(
        .label_ = "light_mode"
      )
      self$iContainer[["addBtn"]] <- actionButton$new(
        .label_ = "Summarise selected PSA dataset"
      )
      self$iContainer[["uplBtn"]] <- actionButton$new(
        .label_ = "Summarise uploaded PSA dataset"
      )
      self$iContainer[["getData"]] <- inputSelection$new(
        .label_ = NULL
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
      self$iContainer[["wtpTxt"]] <- textInput$new(
        .label_ = "Enter WTP vector (comma delimited):"
      )
      self$iContainer[["lblSldrX"]] <- sliderInput$new(
        .label_ = "ICER label X nudge:"
      )
      self$iContainer[["lblSldrY"]] <- sliderInput$new(
        .label_ = "ICER label Y nudge:"
      )
      self$iContainer[["intrNme"]] <- textInput$new(
        .label_ = "Enter interventions' names (comma delimited):"
      )

    },
    ### UI:----
    ui = function() {
      fluidPage(
        theme = self$theme,
        waiter::use_waiter(),
        #### Title panel:----
        titlePanel(
          windowTitle = "ShinyPSA demo",
          div(
            class = "d-flex p-2 bd-highlight",
            ##### App logo:----
            img(
              src = "https://pbs.twimg.com/profile_images/959365885537456128/tC4OVmkX_400x400.jpg",
              height = "35px",
              class = "pr-2 mb-1"),
            ##### Title:----
            span("ShinyPSA demo app!"),
            ##### Theme switcher:----
            self$iContainer[["themeSwch"]]$
              ui_input()
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
              tabPanel(
                title = "Settings",
                icon = icon("cog"),
                fluidRow(
                  column(
                    width = 5,
                    style = "border-right: 1px solid",
                    div(
                      class = "pt-10 pb-0 mb-0 pl-5",
                      h4(
                        class = "pt-2",
                        "Option 1: Choose from existing datasets:"
                      ),
                      fluidRow(
                        tagList(
                          ##### Data drop-list:----
                          self$iContainer[["getData"]]$
                            ui_input(
                              .choices_ = NULL,
                              .style_ = "display: flex;
                          margin-top: 2rem !important;",
                              .class_ = "pl-4 d-flex align-items-start",
                              #.width_ = "200%"
                            )
                        )
                      ),
                      div(
                        style = "display: flex;
                          margin-top: 9rem !important;",
                        class = "d-flex flex-row-reverse",
                        ##### Selection confirmation button:----
                        self$iContainer[["addBtn"]]$
                          ui_input(
                            .style_ = "display: flex;
                          margin-top: 2rem !important;",
                            .class_ = "ml-2 d-flex align-items-end
                          text-right"
                          )
                      )
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
                          ##### Data uploading:----
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
                      )
                    ),
                    div(
                      style = "display: flex;
                          margin-top: 0rem !important;",
                      class = "d-flex flex-row-reverse",
                      # upload effects button:
                      self$iContainer[["uplBtn"]]$
                        ui_input(
                          .style_ = "display: flex;
                          margin-top: 0rem !important;",
                          .class_ = "ml-2 d-flex align-items-start
                          text-right"
                        )
                    )
                  )
                )
              ),
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
                            .choices_ = NULL,
                          ),
                        self$iContainer[["wtpSwch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["wtpTxt"]]$
                          ui_input(),
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
                        self$iContainer[["icrSwch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
                          ),
                        self$iContainer[["lblSldrX"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.1
                          ),
                        self$iContainer[["lblSldrY"]]$
                          ui_input(
                            .min_ = 0,
                            .max_ = 1,
                            .value_ = 0.1
                          ),
                        self$iContainer[["zomSwch"]]$
                          ui_input(
                            .class_ = "pl-2 flex-fill text-left"
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
    ### Server:----
    server = function(input, output, session) {
      #### Data handlers:----
      ##### Data drop-down list:----
      self$iContainer[["getData"]]$
        server(
          session = session,
          input = input,
          output = output,
          .choices_ = c("Vaccine_PSA", "Smoking_PSA")
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
            .interventions = sData_list[[sData_name()]]$treats
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
              .choices_ = c("Do not set a reference",
                            sData_list[[sData_name()]]$treats)
            )
          ###### Retrieve the Summary table from the ShinyPSA object:----
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_Summary_table()
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
            .interventions = sData_list[[sData_name()]]$treats
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
              .choices_ = c("Do not set a reference",
                            sData_list[[sData_name()]]$treats)
            )
          ###### Retrieve the Summary table from the ShinyPSA object:----
          self$iContainer[["sumTbl"]]$
            server(
              session = session,
              input = input,
              output = output,
              .table_ = rContainer[[sData_name()]]$
                get_Summary_table()
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEP update button:----
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
          # Get user defined WTP values:
          wtp_ <- input[[self$iContainer[["wtpTxt"]]$
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
                  .zoom = input[[self$iContainer[["zomSwch"]]$
                                   get_uiInId()]],
                  .show_ICER = input[[self$iContainer[["icrSwch"]]$
                                        get_uiInId()]],
                  .show_wtp = input[[self$iContainer[["wtpSwch"]]$
                                       get_uiInId()]],
                  .wtp_threshold = wtp_,
                  .legend_pos = c(input[[self$iContainer[["lgdSldrX"]]$
                                           get_uiInId()]],
                                  input[[self$iContainer[["lgdSldrY"]]$
                                           get_uiInId()]]),
                  .nudge_labels = c(input[[self$iContainer[["lblSldrX"]]$
                                             get_uiInId()]],
                                    input[[self$iContainer[["lblSldrY"]]$
                                             get_uiInId()]])
                )
            )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      ##### Actions on CEP reset button:----
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

# Instantiate a copy of class ShinyPSA_R6_App:----
app = ShinyPSA_R6_App$new()

# Run the app:----
#thematic::thematic_shiny(font = "auto") # allows themes on ggplot2 plots
shiny::shinyApp(app$ui(), app$server)
