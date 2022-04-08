#############################################
#############################################
### Under construction:
#############################################
#############################################

pacman::p_load(shiny, R6, uuid, ggplot2)

# Define an R6 object.

shinyHealthsites_R6 <- R6Class(
  
  public = list(
    
    id_input = NULL,
    id_plot = NULL,
    id_table = NULL,
    data = NULL,
    columns = NULL,
    settings = reactiveValues(),
    
    initialize = function(data){
      
      # Assign random IDs for both UI methods.
      self$id_input <- uuid::UUIDgenerate()
      self$id_plot <- uuid::UUIDgenerate()
      
      self$data <- data
      self$columns <- names(data)
      
    },
    
    # UI function for input fields (choosing columns from the data)
    ui_input = function(ns = NS(NULL)){
      
      ns <- NS(ns(self$id_input))
      
      tagList(
        
        selectInput(ns("txt_xvar"), "X variable", choices = self$columns),
        selectInput(ns("txt_yvar"), "Y variable", choices = self$columns),
        actionButton(ns("btn_save_vars"), "Save", icon = icon("save"))
        
      )
      
    },
    
    # UI function for the plot output
    ui_plot = function(ns = NS(NULL)){
      
      ns <- NS(ns(self$id_plot))
      
      plotOutput(ns("plot_main"))
      
    },
    
    # Call the server function for saving chosen variables
    store_variables = function(){
      
      callModule(private$store_server, id = self$id_input)
      
    },
    
    # Call the server function for rendering the plot
    render_plot = function(){
      
      callModule(private$plot_server, id = self$id_plot)
      
    }
    
  ),
  
  private = list(
    
    # Server function for column selection
    # This way, input data can be collected in a neat way,
    # and stored inside our object.
    store_server = function(input, output, session){
      
      observeEvent(input$btn_save_vars, {
        
        self$settings$xvar <- input$txt_xvar
        self$settings$yvar <- input$txt_yvar
        
      })
      
    },
    
    # Server function for making the plot
    plot_server = function(input, output, session){
      
      output$plot_main <- renderPlot({
        
        req(self$settings$xvar)
        req(self$settings$yvar)
        
        x <- self$settings$xvar
        y <- self$settings$yvar
        
        ggplot(self$data, aes(!!sym(x), !!sym(y))) +
          geom_point()
      })
      
      
    }
    
  )
)

# Make a new object, only here do we have to pass a data object.
# This makes it easy to manage many objects, with different settings.
xy_mtcars <- bivariateClass$new(data = mtcars)


# UI
# Here we only have to call the UI methods. 
ui <- fluidPage(
  
  xy_mtcars$ui_input(),
  
  tags$hr(),
  
  xy_mtcars$ui_plot()
  
)

# And here we just have to call the server methods.
server <- function(input, output, session) {
  
  xy_mtcars$store_variables()
  
  xy_mtcars$render_plot()
  
  
}

shinyApp(ui, server)