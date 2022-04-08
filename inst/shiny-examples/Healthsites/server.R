################################################################################
#
# Script Name:        server.R
# Module Name:        HealthsitesShinyApp
# Script Description: Defines the server part of the healthsites api
#                     example shiny app.
# Author:             Wael Mohammed
# Organisation:       Dark Peak Analytics
# Email:              wmohammed@darkpeakanalytics.com
#
################################################################################

# Ground work: ----
api_key <- get_hs_API_key(env_var_name = "Healthsites_API_key")
countries <- sort(spData::world$name_long)
light <- bslib::bs_theme()
dark <- bslib::bs_theme(bg = "black", fg = "white", primary = "purple")

# Server: ----
server <- function(input, output, session) {
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
        inputId = "tabs",
        tab = tabPanel(
          title = .id_(),
          tagList(
            "This a dynamically-added tab",
            bslib::navs_pill_card(
              id = "country_pill_card",
              bslib::nav(title = "Map",
                         leaflet::leafletOutput(
                           outputId = paste0(.id_(), "_map"))
              ),
              bslib::nav(title = "Stats",
                         shiny::dataTableOutput(
                           outputId = paste0(.id_(), "_stats"))
              ),
              bslib::nav(title = "Data",
                         shiny::dataTableOutput(
                           outputId = paste0(.id_(), "_data"))
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
        id = c("add", "remove"),
        hide_on_render  = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())
      # Instantiate a Healthsites R6 class for the chosen country:
      country_obj_ <- Country_R6$
        new(country_name = .id_(), hs_API_key = API_key())$
        # Query the api for health facilities: (outputs generated auto.)
        query_health_facilities()
      # In the world's plot:
      Earth$
        # add new data to the Earth object (stats and plot will update):
        add_country(country_name = .id_(),
                    country_object = country_obj_)
      # Render country facilities map:
      output[[paste0(.id_(), "_map")]] <- leaflet::renderLeaflet({
        country_obj_$get_facilities_map()
      })
      # Render country facilities data:
      output[[paste0(.id_(), "_data")]] <- shiny::renderDataTable({
        country_obj_$get_facilities_data()
      })
      # Render country facilities stats:
      output[[paste0(.id_(), "_stats")]] <- shiny::renderDataTable({
        country_obj_$get_facilities_stats()
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
  remove_vars <- reactiveValues(add_v = NULL,
                                rem_v = countries)

  observeEvent(
    eventExpr = input$add,
    handlerExpr = {
      remove_vars$rem_v <- setdiff(remove_vars$rem_v, input$add_country)
      remove_vars$add_v <- union(remove_vars$add_v, input$add_country)
    },
    ignoreInit = TRUE)

  observeEvent(
    eventExpr = input$remove,
    handlerExpr = {
      remove_vars$add_v <- setdiff(remove_vars$add_v, input$remove_country)
      remove_vars$rem_v <- union(remove_vars$rem_v, input$remove_country)
    },
    ignoreInit = TRUE)

  observeEvent(
    eventExpr = c(input$add, input$remove),
    handlerExpr = {
      updateSelectInput(
        session = session,
        inputId = "add_country",
        choices = remove_vars$rem_v)
      updateSelectInput(
        session = session,
        inputId = "remove_country",
        choices = remove_vars$add_v)
    },
    ignoreInit = TRUE)

  observeEvent(
    eventExpr = input$remove,
    handlerExpr = {
      removeTab(
        inputId = "tabs",
        target = input$remove_country)
    },
    ignoreInit = TRUE)

  output$remove_list <- renderUI({
    if(length(remove_vars$add_v) > 0) {
      tagList(
        selectInput(
          inputId = "remove_country",
          label = "Choose a country to remove",
          choices = c(remove_vars$add_v),
          selected = isolate(remove_vars$add_v[1])
        ),
        actionButton(
          inputId = "remove",
          label = "Remove a country")
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
