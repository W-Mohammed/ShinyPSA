################################################################################
#
# Script Name:        ui.R
# Module Name:        HealthsitesShinyApp
# Script Description: Defines the UI part of the healthsites api example
#                     shiny app.
# Author:             Wael Mohammed
# Organisation:       Dark Peak Analytics
# Email:              wmohammed@darkpeakanalytics.com
#
################################################################################

# Ground work: ----
dark <- bslib::bs_theme(bg = "black", fg = "white", primary = "purple")
countries <- sort(spData::world$name_long)

# UI: ----
ui <- fluidPage(
  theme = dark,
  waiter::use_waiter(),
  sidebarLayout(
    sidebarPanel(
      shinyWidgets::prettySwitch("light_mode", "Light mode"),
      hr(),
      uiOutput(
        outputId = "api_key_ui"),
      # textOutput(
      #   outputId = "api_key"),
      selectInput(
        inputId = "add_country",
        label = "Select a country:",
        choices = countries,
        selected = "Kenya"),
      actionButton(
        inputId = "add",
        label = "Add a country"),
      br(),
      hr(),
      uiOutput(
        outputId = "remove_list")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "World",
          tagList(
            "Please choose a country and click \"Add\" to query the api",
            bslib::navs_pill_card(
              id = "world_pill_card",
              bslib::nav(title = "Map",
                         uiOutput(outputId = "world_mapUI")
              ),
              bslib::nav(title = "Statistics",
                         uiOutput(outputId = "world_statsUI")
              )
            )
          )
        )
      )
    )
  )
)
