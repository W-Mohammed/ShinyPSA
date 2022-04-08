# Ground work: ----
dark <- bslib::bs_theme(bg = "#131313", fg = "white", primary = "darkmagenta")
countries <- sort(spData::world$name_long)

# UI: ----
ui <- fluidPage(

  tags$head(
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "https://decidehealth.world/sites/default/files/Favicon_RGB_300dpi.png"
    ),
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    * {
      font-family: 'Inter', sans-serif;
    }
    #countries .nav-item {
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
      background-color: rgba(51, 62, 72, 0.5) !important;

                    "))
    ),


  titlePanel(
    windowTitle = "api demo",
  div(
    class="d-flex p-2 bd-highlight",
    img(
      src = "https://decidehealth.world/sites/default/files/Favicon_RGB_300dpi.png",
      height = "35px",
      class = "pr-2 mb-1"),
    span("Bolster APIs demo app"),
    div(
      class = "flex-fill text-right",
      style = "font-size: 1rem;",
    shinyWidgets::prettySwitch("light_mode", "Light mode", inline = T)
    )
  )
  ),

  # landing page
  div(
    id = "landing-page",
    style = "
    position: absolute;
    right:0;left:0;top:0;bottom:0;
    background-color: white;
    background: radial-gradient(circle, white, #cecece);
    background-image: url('https://decidehealth.world/sites/default/files/Favicon_RGB_300dpi.png'), radial-gradient(circle, white, #cecece);
    background-size: auto;
    background-position: 30% 30%;
    background-repeat: no-repeat;
    z-index: 9;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content:center;
    color: black;
    text-shadow: 2px 2px 8px white;

    ",
      h1("Bolster project"),
      h3("API demo application"),
      actionButton("exit_landing", "Start", class = "btn-primary btn-lg mt-5")
  ),

  theme = dark,
  waiter::use_waiter(),
  shinyjs::useShinyjs(),

  fluidRow(

    column(
      width = 2, offset = 0,
      class = "mt-2 ml-2",
      selectInput(
        inputId = "add_country",
        label = "Add country:",
        choices = countries,
        selected = "Kenya",
        selectize = TRUE
        ),

      div(
        class = "w-100 text-right pr-2 pm-3",
        actionButton(
          inputId = "add",
          label = "Add",
          class = "btn-primary",
          ),
      ),
      uiOutput(
        outputId = "remove_list"),
      div(
        class = "w-100 text-right pr-2",
        uiOutput(
          outputId = "remove_button"
        ),
      ),
      uiOutput(
        outputId = "api_key_ui")
    ),

    column(
      width = 9, offset = 0,
      class = "mr-5 pb-5",

      tabsetPanel(
        id = "countries",
        # well = F,
        # widths = c(2, 10),
        tabPanel(
          title = "World",
          tagList(
            bslib::navs_pill_card(
              id = "world_pill_card",
              navbarMenu(
                title = "Health facilities",
                menuName = "Extras",
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
  ),
  fluidRow(

  )
)

