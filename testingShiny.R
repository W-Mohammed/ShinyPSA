handlerExpr = {
  insertTab(
    inputId = "countries",
    tab = tabPanel(
      title = data_name() ,
      tagList(
        tabsetPanel(
          id = paste0(data_name(), "pill_card"),
          ### Summary table:----
          tabPanel(
            title = 'Summary table',
            div(
              class = "card",
              fluidRow(
                column(
                  class = "px-1 py-1",
                  width = 12,
                  self$iContainer$store[[paste0(data_name(), "_sumTbl")]]$
                    ui_output()
                )
              )
            )
          ),
          ### CE Plane:----
          tabPanel(
            title = 'CE Plane',
            div(
              class = "card",
              fluidRow(
                column(
                  width = 2,
                  offset = 0,
                  #class = "mt-2 ml-2", # width 11 to work
                  tagList(
                    self$iContainer$add(
                      .objectName_ = paste0(data_name(), "_ZmSwch"),
                      .object_ = prettySwitch$new(
                        .label_ = "light_mode"
                      )
                    )
                  )
                ),
                column(
                  width = 9,
                  offset = 0,
                  class = "mr-5 pb-5",
                  DT::dataTableOutput(
                    width = "100%",
                    outputId = paste0(.id_(), "_WB_I_Data")
                  )
                )
              )
            )
          ),
          ### CEAC plot:----
          tabPanel(
            title = 'CEAC plot',
            div(
              class = "card",
              fluidRow(
                column(
                  width = 2,
                  offset = 0,
                  class = "mt-2 ml-2", # width 11 to work
                ),
                column(
                  width = 9,
                  offset = 0,
                  class = "mr-5 pb-5",
                  DT::dataTableOutput(
                    width = "100%",
                    outputId = paste0(.id_(), "_WB_I_Data")
                  )
                )
              )
            )
          ),
          ### CEAF plot:----
          tabPanel(
            title = 'CEAF plot',
            div(
              class = "card",
              fluidRow(
                column(
                  width = 2,
                  offset = 0,
                  class = "mt-2 ml-2", # width 11 to work
                ),
                column(
                  width = 9,
                  offset = 0,
                  class = "mr-5 pb-5",
                  DT::dataTableOutput(
                    width = "100%",
                    outputId = paste0(.id_(), "_WB_I_Data")
                  )
                )
              )
            )
          ),
          ### NMB plot:----
          tabPanel(
            title = 'NMB plot',
            div(
              class = "card",
              fluidRow(
                column(
                  width = 2,
                  offset = 0,
                  class = "mt-2 ml-2", # width 11 to work
                ),
                column(
                  width = 9,
                  offset = 0,
                  class = "mr-5 pb-5",
                  DT::dataTableOutput(
                    width = "100%",
                    outputId = paste0(.id_(), "_WB_I_Data")
                  )
                )
              )
            )
          ),
          ### EVPI plot:----
          tabPanel(
            title = 'EVPI plot',
            div(
              class = "card",
              fluidRow(
                column(
                  width = 2,
                  offset = 0,
                  class = "mt-2 ml-2", # width 11 to work
                ),
                column(
                  width = 9,
                  offset = 0,
                  class = "mr-5 pb-5",
                  DT::dataTableOutput(
                    width = "100%",
                    outputId = paste0(.id_(), "_WB_I_Data")
                  )
                )
              )
            )
          ),
          ### Raw data:----
          navbarMenu(
            title = "Raw data", menuName = "data_",
            bslib::nav(
              title = "Effects",
              div(
                class = "card px-3 py-3",
                # leaflet::leafletOutput(
                #   height = "70vh",
                #   outputId = paste0(.id_(), "_map"))
              )
            ),
            bslib::nav(
              title = "Costs",
              div(
                class = "card px-3 py-3",
                # DT::dataTableOutput(
                #   outputId = paste0(.id_(), "_stats")
                # )
              )
            )
          )
        )
      )
    ),
    target = "outputs"
  )
}

############################################################


fluidPage(
  fluidRow(
    column(4,

           # Copy the line below to make a slider bar
           sliderInput("slider1", label = h3("Slider"), min = 0,
                       max = 100, value = 50)
    ),
    column(4,

           # Copy the line below to make a slider range
           sliderInput("slider2", label = h3("Slider Range"), min = 0,
                       max = 100, value = c(40, 60))
    )
  ),

  hr(),

  fluidRow(
    column(4, verbatimTextOutput("value")),
    column(4, verbatimTextOutput("range"))
  )

)


function(input, output) {

  # You can access the value of the widget with input$slider1, e.g.
  output$value <- renderPrint({ input$slider1 })

  # You can access the values of the second widget with input$slider2, e.g.
  output$range <- renderPrint({ input$slider2 })
}

## Only run examples in interactive R sessions
if (interactive()) {

  ui <- fluidPage(
    checkboxGroupInput("variable", "Variables to show:",
                       c("Cylinders" = "cyl",
                         "Transmission" = "am",
                         "Gears" = "gear")),
    tableOutput("data")
  )

  server <- function(input, output, session) {
    output$data <- renderTable({
      mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }

  shinyApp(ui, server)

  # ui <- fluidPage(
  #   checkboxGroupInput("icons", "Choose icons:",
  #                      choiceNames =
  #                        list(icon("calendar"), icon("bed"),
  #                             icon("cog"), icon("bug")),
  #                      choiceValues =
  #                        list("calendar", "bed", "cog", "bug")
  #   ),
  #   textOutput("txt")
  # )
  #
  # server <- function(input, output, session) {
  #   output$txt <- renderText({
  #     icons <- paste(input$icons, collapse = ", ")
  #     paste("You chose", icons)
  #   })
  # }
  #
  # shinyApp(ui, server)
}

extract_numerics_ <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}
extract("2, 4, 1")
text <- gsub(" ", "", "2, 4, 1")
split <- strsplit(text, ",", fixed = FALSE)[[1]]
as.numeric(split)
