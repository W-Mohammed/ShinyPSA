################################################################################
#
# Ground work: ----
api_key <- get_hs_API_key(env_var_name = "Healthsites_API_key")
countries <- sort(spData::world$name_long)
light <- bslib::bs_theme()
dark <- bslib::bs_theme(bg = "#131313", fg = "white", primary = "darkmagenta")

# Server: ----
server <- function(input, output, session) {


  observeEvent(input$exit_landing,{
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


  # Grab the country name and save it to id:
  .id_ <- reactive({
    input$add_country
  })

  # Reactive environment:
  Earth <- World_R6$new()

  # Add a country:
  observeEvent(
    eventExpr = input$add,
    handlerExpr = {
      insertTab(
        inputId = "countries",
        tab = tabPanel(
          title = .id_() ,
          tagList(
            tabsetPanel(
              id = paste0(.id_(), "pill_card"),

              tabPanel(
                  title = "DHS Surveys",
                  div(
                    class = "card",
                    div(
                      style = "display: flex;",
                      class = "px-5 py-4",
                        selectInput(
                          inputId = paste0(.id_(), "_DHS_S_dropList"),
                          label = "DHS survey year",
                          choices = NULL,
                          selectize = TRUE
                        ),
                        div(
                          class = "ml-3",
                          style = "margin-top: 2rem !important;",
                          actionButton(
                            inputId = paste0(.id_(), "get_DHS_servey"),
                            label = "Fetch",
                            class = "btn-primary"
                        )
                      )
                    ),
                  fluidRow(
                    column(
                      class = "px-5 py-3",
                      width = 12,
                        DT::dataTableOutput(
                          width = "100%",
                          outputId = paste0(.id_(), "_DHS_S_Data")
                        )
                    )
                  )
              )
              ),


              tabPanel(
                title = 'DHS Indicators',
                div(
                  class = "card",
                  div(
                    style = "display: flex;",
                    class = "px-5 py-4",
                  selectizeInput(
                    inputId = paste0(.id_(), "_DHS_I_dropList"),
                    label = "DHS survey indicators",
                    choices = NULL,
                    width = "50%",
                    multiple = TRUE
                  ),
                  div(
                    class = "ml-3",
                    style = "margin-top: 2rem !important;",
                  actionButton(
                    inputId = paste0(.id_(), "get_DHS_indc"),
                    label = "Fetch",
                    class = "btn-primary"
                  )
                  )
                  ),
                  fluidRow(
                    column(
                      class = "px-5 py-3",
                      width = 12,
                      DT::dataTableOutput(
                        width = "100%",
                        outputId = paste0(.id_(), "_DHS_I_Data")
                      )
                    )
                  )
                ),
              ),
              tabPanel(
                title = 'GHO Indicators',
                div(
                  class = "card",
                  div(
                    style = "display: flex;",
                    class = "px-5 py-4",
                  selectizeInput(
                    inputId = paste0(.id_(), "_GHO_I_dropList"),
                    label = "GHO indicators",
                    choices = NULL,
                    width = "50%",
                    multiple = FALSE
                  ),
                  div(
                    class = "ml-3",
                    style = "margin-top: 2rem !important;",
                    actionButton(
                      inputId = paste0(.id_(), "get_GHO_indc"),
                      label = "Fetch",
                      class = "btn-primary"
                    )
                  )
                ),
                fluidRow(
                  column(
                    class = "px-5 py-3",
                    width = 12,
                  DT::dataTableOutput(
                    width = "100%",
                    outputId = paste0(.id_(), "_GHO_I_Data")
                  )
                  )
                )
                )
              ),
              tabPanel(
                title = 'WB Indicators',
                div(
                  class = "card",
                  div(
                    style = "display: flex;",
                    class = "px-5 py-4",
                  selectizeInput(
                    inputId = paste0(.id_(), "_WB_I_dropList"),
                    label = "WB indicators",
                    choices = NULL,
                    multiple = FALSE
                  ),
                  div(
                    class = "ml-3",
                    style = "margin-top: 2rem !important;",
                    actionButton(
                      inputId = paste0(.id_(), "get_WB_indc"),
                      label = "Fetch",
                      class = "btn-primary"
                    )
                  )
                ),
                fluidRow(
                  column(
                    class = "px-5 py-3",
                    width = 12,
                    DT::dataTableOutput(
                      width = "100%",
                      outputId = paste0(.id_(), "_WB_I_Data")
                    )
                  )
                )
              )
              ),
              navbarMenu(
                title = "Health facilities", menuName = "HS_items",
                bslib::nav(
                  title = "Map",
                  div(
                    class = "card px-3 py-3",
                    leaflet::leafletOutput(
                      height = "70vh",
                      outputId = paste0(.id_(), "_map"))
                  )
                ),
                bslib::nav(
                  title = "Stats",
                  div(
                    class = "card px-3 py-3",
                    DT::dataTableOutput(
                      outputId = paste0(.id_(), "_stats")
                      )
                  )
                ),
                bslib::nav(
                  title = "Data",
                  div(
                    class = "card px-3 py-3",
                    DT::dataTableOutput(
                      outputId = paste0(.id_(), "_data")
                      )
                  )
                )
              )
            )
          )
        ),
        target = "World"
      )
    },
    ignoreInit = TRUE)

  # Facilities map:
  observeEvent(
    eventExpr = input$add,
    handlerExpr = {
      # Let the user know shiny is processing their query:
      waiter <- waiter::Waiter$new(
        # id = c("add", "remove"),
        html = div(
          style = "
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content:center;
            color: white;
            opacity: 1 !important;
          ",
          h4("Retrieving data from API..."),
          h4("Please wait."),
          br(),br(),
          waiter::spin_wandering_cubes()
        ),
        hide_on_render  = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())
      # Instantiate a country R6 class for the chosen country in the
      # world's (Earth) object:
      Earth$
        # add new data to the Earth object (stats and plot will update):
        add_country(
          country_name = .id_(),
          country_object = Country_R6$
            new(country_name = .id_(), hs_API_key = API_key())$
            # Query the api for health facilities:
            query_health_facilities())
      # Render country facilities map:
      output[[paste0(.id_(), "_map")]] <- leaflet::renderLeaflet({
        Earth$country_data[[input$countries]]$get_facilities_map()
      })
      # Render country facilities data:
      output[[paste0(.id_(), "_data")]] <- DT::renderDataTable(server=FALSE,{

        facility_data = Earth$country_data[[input$countries]]$get_facilities_data()
        facility_data$geometry = NULL
        facility_data = facility_data[,c("attributes.amenity","attributes.name","attributes.changeset_id","attributes.healthcare","lng","lat")]

        DT::datatable(
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            scrollX = T, pageLength = 10,
            buttons = c('csv', 'excel')
            ),
          facility_data
        )

      })
      # Render country facilities stats:
      output[[paste0(.id_(), "_stats")]] <- DT::renderDataTable(server=FALSE,{

        DT::datatable(
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            scrollX = T, pageLength = 10,
            buttons = c('csv', 'excel')
          ),
          Earth$country_data[[input$countries]]$get_facilities_stats()
        )

      })
      # Render world facilities map:
      output$world_map <- leaflet::renderLeaflet({
        Earth$get_world_map()
      })
      # Render world facilities data:
      output$world_stats <- DT::renderDataTable(
        Earth$get_world_stats()
      )
    },
    ignoreInit = TRUE)

  # Update drop-down lists:
  updated <- reactive({
    print("Check if drop-down list were updated...")
    print(input$countries)
    if(!is.null(input[[paste0(input$countries, "_DHS_S_dropList")]])) {
      print(input[[paste0(input$countries, "_DHS_S_dropList")]])
      return(input[[paste0(input$countries, "_DHS_S_dropList")]] == "" &
               input$countries != "World")
    } else {
      return(FALSE)
    }

  })
  observe({
    if(updated()) {
      if(input$countries != "World") {
        print("Updating drop-down lists...")
        print(paste0(input$countries))
        # Render DHS surveys list:
        dhs_S_dropList_choices <- Earth$country_data[[input$countries]]$
          dhs_survey_years$SurveyYear
        if(is.null(dhs_S_dropList_choices))
          dhs_S_dropList_choices <- glue::glue("No DHS data found for {input$countries}")
        updateSelectizeInput(
          session = session,
          inputId = paste0(input$countries, "_DHS_S_dropList"),
          choices = dhs_S_dropList_choices,
          server = TRUE
        )
        print("- 1st list")
        # Render DHS surveys indicators:
        dhs_I_dropList_choices <- Earth$country_data[[input$countries]]$
          dhs_indicators$Indicator
        if(is.null(dhs_I_dropList_choices))
          dhs_I_dropList_choices <- glue::glue("No DHS data found for {input$countries}")
        updateSelectizeInput(
          session = session,
          inputId = paste0(input$countries, "_DHS_I_dropList"),
          choices = dhs_I_dropList_choices,
          server = TRUE
        )
        print("- 2nd list")
        # Render DHS surveys indicators:
        gho_I_dropList_choices <- Earth$country_data[[input$countries]]$
          get_gho_ind_list()$IndicatorName
        if(is.null(gho_I_dropList_choices))
          gho_I_dropList_choices <- glue::glue("No GHO data found for {input$countries}")
        updateSelectizeInput(
          session = session,
          inputId = paste0(input$countries, "_GHO_I_dropList"),
          choices = gho_I_dropList_choices,
          server = TRUE
        )
        print("- 3rd list")
        # Render World Bank indicators:
        updateSelectizeInput(
          session = session,
          inputId = paste0(input$countries, "_WB_I_dropList"),
          choices = Earth$country_data[[input$countries]]$
            wb_indicators$name,
          server = TRUE
        )
        print("- 4th list")
      }
    }
  })

  # Render country DHS survey data:
  observeEvent(
    eventExpr = input[[paste0(input$countries, 'get_DHS_servey')]],
    handlerExpr = {
      # Let the user know shiny is processing their query:
      waiter <- waiter::Waiter$new(
        id = paste0(input$countries, 'get_DHS_servey'),
        hide_on_render  = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())
      # Render country survey data:
      output[[paste0(input$countries, "_DHS_S_Data")]] <-
        DT::renderDataTable(server=FALSE,{
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = 'Bfrtip',
              scrollX = T, pageLength = 10,
              buttons = c('csv', 'excel')
            ),
            Earth$country_data[[input$countries]]$
              get_dhs_survey_data(
                survey =
                  isolate(
                    input[[paste0(input$countries, "_DHS_S_dropList")]]
                  ),
              filter_var = NULL
            )
            )
        })
    },
    ignoreInit = TRUE
  )

  # Render country DHS indicator data:
  observeEvent(
    eventExpr = input[[paste0(input$countries, 'get_DHS_indc')]],
    handlerExpr = {
      # Let the user know shiny is processing their query:
      waiter <- waiter::Waiter$new(
        id = paste0(input$countries, 'get_DHS_indc'),
        hide_on_render  = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())
      # Render country survey data:
      output[[paste0(input$countries, "_DHS_I_Data")]] <-
        DT::renderDataTable(server=FALSE,{
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = 'Bfrtip',
              scrollX = T, pageLength = 10,
              buttons = c('csv', 'excel')
            ),
          Earth$country_data[[input$countries]]$
            get_dhs_ind_data(
              indicator_name =
                isolate(
                  input[[paste0(input$countries, "_DHS_I_dropList")]]
                ),
              filter_var = NULL
            )
          )
        })
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  # Render country GHO indicator data:
  observeEvent(
    eventExpr = input[[paste0(input$countries, 'get_GHO_indc')]],
    handlerExpr = {
      # Let the user know shiny is processing their query:
      waiter <- waiter::Waiter$new(
        id = paste0(input$countries, 'get_GHO_indc'),
        hide_on_render  = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())
      # Render country survey data:
      output[[paste0(input$countries, "_GHO_I_Data")]] <-
        DT::renderDataTable(
          server=FALSE,{
            DT::datatable(
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                scrollX = T, pageLength = 10,
                buttons = c('csv', 'excel')
              ),
              Earth$country_data[[input$countries]]$
                get_gho_ind_data(
                  indicator_code_ =
                    Earth$country_data[[input$countries]]$
                    get_gho_ind_list() %>%
                    dplyr::filter(
                      isolate(
                        input[[paste0(input$countries, "_GHO_I_dropList")]]
                      ) == IndicatorName
                    ) %>%
                    dplyr::pull(IndicatorCode)
                  )
            )
        })
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  # Render country WB indicator data:
  observeEvent(
    eventExpr = input[[paste0(input$countries, 'get_WB_indc')]],
    handlerExpr = {
      # Let the user know shiny is processing their query:
      waiter <- waiter::Waiter$new(
        id = paste0(input$countries, 'get_WB_indc'),
        hide_on_render  = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())
      # Query WB API:
      wb_I_name <- input[[paste0(input$countries, "_WB_I_dropList")]]
      wb_I_label <- Earth$country_data[[input$countries]]$
        wb_indicators %>%
        dplyr::filter(wb_I_name == name) %>%
        dplyr::pull(indicator)
      # Render country survey data:
      output[[paste0(input$countries, "_WB_I_Data")]] <-
        DT::renderDataTable(server=FALSE,{
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = 'Bfrtip',
              scrollX = T, pageLength = 10,
              buttons = c('csv', 'excel')
            ),
          Earth$country_data[[input$countries]]$
            set_wb_data(
              indicator_label = isolate(
                wb_I_label
              ))$
            get_wb_ind_data()
          )
        })
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  # Render outputs on World tab:
  output$world_mapUI <- renderUI({
    tagList(
      leaflet::leafletOutput(
        height = "70vh",
        outputId = "world_map"
      ),
    )
  })
  output$world_statsUI <- renderUI({
    tagList(
      DT::dataTableOutput(
        outputId = "world_stats"
      )
    )
  })

  # Dynamic choices list:
  react_vars <- reactiveValues(add_v = NULL,
                               rem_v = countries)

  observeEvent(
    eventExpr = input$add,
    handlerExpr = {
      react_vars$rem_v <- setdiff(react_vars$rem_v, input$add_country)
      react_vars$add_v <- union(react_vars$add_v, input$add_country)
    },
    ignoreInit = TRUE)

  observeEvent(
    eventExpr = input$remove,
    handlerExpr = {
      react_vars$add_v <- setdiff(react_vars$add_v, input$remove_country)
      react_vars$rem_v <- union(react_vars$rem_v, input$remove_country)
    },
    ignoreInit = TRUE)

  observeEvent(
    eventExpr = c(input$add, input$remove),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "add_country",
        choices = react_vars$rem_v
      )
      updateSelectInput(
        session = session,
        inputId = "remove_country",
        choices = react_vars$add_v
      )
    },
    ignoreInit = TRUE)

  observeEvent(
    eventExpr = input$remove,
    handlerExpr = {
      removeTab(
        inputId = "countries",
        target = input$remove_country)
    },
    ignoreInit = TRUE)

  output$remove_list <- renderUI({
    if(length(react_vars$add_v) > 0) {
      tagList(
        selectInput(
          inputId = "remove_country",
          label = "Remove country",
          choices = c(react_vars$add_v),
          selected = isolate(react_vars$add_v[1]),
          selectize = TRUE
        )
      )
    }
  })

  output$remove_button <- renderUI({
    if(length(react_vars$add_v) > 0) {
      tagList(
        actionButton(
          inputId = "remove",
          label = "Remove")
      )
    }
  })

  # API key:
  output$api_key_ui <- renderUI({
    if(is.null(api_key)) {
      tagList(
        textInput(
          inputId = "api_key",
          label = "Could not find an api key, please provide one:"
        )
      )
    }
  })

  API_key <- reactive({
    if(is.null(api_key)) {
      input$api_key
    } else {
      api_key
    }
  })

  # Theme changing checkbox:
  observe(session$setCurrentTheme(
    if (isTRUE(input$light_mode)) light else dark
  ))

  # output$api_key <- renderText({
  #   if(is.null(api_key)) {
  #     input$api_key
  #   } else {
  #     api_key
  #   }
  # })

}
