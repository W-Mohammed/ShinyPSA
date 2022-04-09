################################################################################
#
# Script Name:        shiny_R6_elements.R
# Module Name:        Economic/PSA/demo app
# Script Description: Defines a set of classes used in the shiny app
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

# Objects container:----
R6_container = R6::R6Class(
  classname = "R6_container",

  public = list(
    ## Fields:----
    store = list(),

    ## Methods:----
    ### Add R6 objects to container:----
    add = function(.objectName_, .object_) {
      self$store[[.objectName_]] <- .object_

      invisible(self)
    }
  ),

  private = list(
    ## Fields:----

    ## Methods:----

  )
)

# Pretty Switch:----
prettySwitch = R6::R6Class(
  classname = 'prettySwitch',

  public = list(
    ## Fields:----

    ## Methods:----
    ### Initialise:----
    initialize = function(.label_) {

      private$label_ = .label_
      private$id_ = uuid::UUIDgenerate()
      private$ns_ = NS(private$id_)

    },
    ### UI:----
    #### UI input:----
    ui_input = function(.tag_ = "UI_input", .value_ = FALSE) {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          class = "flex-fill text-right",
          style = "font-size: 1rem;",
          shinyWidgets::prettySwitch(
            inputId = private$uiInput_Id_,
            label = private$label_,
            inline = TRUE,
            value = .value_
          )
        )
      )

    },
    #### UI output:----
    ui_output = function(.tag_ = "UI_output") {

      private$uiOutput_Id_ <- private$ns_(.tag_)

      tagList(
        # renderUI(
        #   inputId = private$uiOutput_Id_,
        #   label = self$label_
        # )
      )

    },
    #### UI rendered output:----
    ui_render_output = function(.tag_ = "UI_render_output") {

      private$uiRenderedOutput_Id_ <- private$ns_(.tag_)

      tagList(
        # renderUI(
        #   inputId = private$uiRenderedOutput_Id_,
        #   label = self$label_
        # )
      )

    },
    ### Server:----
    server = function(input, output, session) {
    },
    ### Getters:----
    #### Get UI input ID:----
    get_uiInId = function() {

      return(private$uiInput_Id_)
    },
    #### Get UI output ID:----
    get_uiOtId = function() {

      return(private$uiOutput_Id_)
    },
    #### Get UI rendered output ID:----
    get_uiRdOtId = function() {

      return(private$uiRenderedOutput_Id_)
    }

  ),

  private = list(
    ## Fields:----
    label_ = NULL,
    id_ = NULL,
    ns_ = NULL,
    uiInput_Id_ = NULL,
    uiOutput_Id_ = NULL,
    uiRenderedOutput_Id_ = NULL

    ## Methods:----

  )
)

# Action buttons:----
actionButton = R6::R6Class(
  classname = 'actionButton',

  public = list(
    ## Fields:----

    ## Methods:----
    ### Initialise:----
    initialize = function(.label_) {

      private$label_ = .label_
      private$id_ = uuid::UUIDgenerate()
      private$ns_ = NS(private$id_)

    },
    ### UI:----
    #### UI input:----
    ui_input = function(.tag_ = "UI_input",
                        .class_ = "ml-2 pt-2 d-flex align-items-center text-right") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          class = .class_, #ml-2 "d-flex align-items-center text-right"
          style = "display: flex; margin-top: 2rem !important;",
          actionButton(
            inputId = private$uiInput_Id_,
            label = private$label_,
            class = "btn-primary"
          )
        )
      )

    },
    #### UI output:----
    ui_output = function(.tag_ = "UI_output") {

      private$uiOutput_Id_ <- private$ns_(.tag_)

      tagList(
        # renderUI(
        #   inputId = private$uiOutput_Id_,
        #   label = private$label_
        # )
      )

    },
    #### UI rendered output:----
    ui_render_output = function(.tag_ = "UI_render_output") {

      private$uiRenderedOutput_Id_ <- private$ns_(.tag_)

      tagList(
        # renderUI(
        #   inputId = private$uiRenderedOutput_Id_,
        #   label = private$label_
        # )
      )

    },
    ### Server:----
    server = function(input, output, session) {
      # observeEvent(
      #   eventExpr = input$self$label]],
      #   handlerExpr = {
      #     session$setCurrentTheme(
      #       bslib::bs_theme(bg = "black",
      #                       fg = "white",
      #                       primary = "purple")
      #     )
      #   })

      #output$text = renderText({ msg })
    },
    ### Getters:----
    #### Get UI input ID:----
    get_uiInId = function() {

      return(private$uiInput_Id_)
    },
    #### Get UI output ID:----
    get_uiOtId = function() {

      return(private$uiOutput_Id_)
    },
    #### Get UI rendered output ID:----
    get_uiRdOtId = function() {

      return(private$uiRenderedOutput_Id_)
    }

  ),

  private = list(
    ## Fields:----
    label_ = NULL,
    id_ = NULL,
    ns_ = NULL,
    uiInput_Id_ = NULL,
    uiOutput_Id_ = NULL,
    uiRenderedOutput_Id_ = NULL

    ## Methods:----

  )
)

# Input selection:----
inputSelection = R6::R6Class(
  classname = 'inputSelection',
  public = list(
    ## Fields:----

    ## Methods:----
    ### Initialise:----
    initialize = function(.label_) {

      private$label_ = .label_
      private$id_ = uuid::UUIDgenerate()
      private$ns_ = NS(private$id_)

    },
    ### UI:----
    #### UI input:----
    ui_input = function(.tag_ = "UI_input", .choices_, .selected_ = NULL,
                        .multiple_ = FALSE, .width_ = "100%",
                        .class_ = "d-flex align-items-center") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          style = "display: flex; margin-top: 2rem !important;",
        class = .class_, #d-flex align-items-center text-right #px-5 py-4
          selectizeInput(
            inputId = private$uiInput_Id_,
            label = private$label_,
            choices = .choices_,
            selected = .selected_,
            multiple = .multiple_,
            width = .width_
          )
        )
      )

    },
    #### UI output:----
    ui_output = function(.tag_ = "UI_output") {

      private$uiOutput_Id_ <- private$ns_(.tag_)

      tagList(
        # renderUI(
        #   inputId = private$uiOutput_Id_,
        #   label = private$label_
        # )
      )

    },
    #### UI rendered output:----
    ui_render_output = function(.tag_ = "UI_render_output") {

      private$uiRenderedOutput_Id_ <- private$ns_(.tag_)

      tagList(
        # renderUI(
        #   inputId = private$uiRenderedOutput_Id_,
        #   label = private$label_
        # )
      )

    },

    ### server:----
    server = function(input, output, session, .choices_ = .choices_) {

      updateSelectizeInput(
        session = session,
        inputId = private$uiInput_Id_,
        choices = .choices_,
        server = TRUE
      )

    },
    ### Getters:----
    #### Get UI input ID:----
    get_uiInId = function() {

      return(private$uiInput_Id_)
    },
    #### Get UI output ID:----
    get_uiOtId = function() {

      return(private$uiOutput_Id_)
    },
    #### Get UI rendered output ID:----
    get_uiRdOtId = function() {

      return(private$uiRenderedOutput_Id_)
    }

  ),

  private = list(
    ## Fields:----
    label_ = NULL,
    id_ = NULL,
    ns_ = NULL,
    uiInput_Id_ = NULL,
    uiOutput_Id_ = NULL,
    uiRenderedOutput_Id_ = NULL

    ## Methods:----

  )
)

# Plot - ggplot2:----
ggplot2Plot = R6::R6Class(
  classname = 'ggplot2Plot',
  public = list(
    ## Fields:----

    ## Methods:----
    ### Initialise:----
    initialize = function(.label_) {

      private$label_ = .label_
      private$id_ = uuid::UUIDgenerate()
      private$ns_ = NS(private$id_)

    },
    ### UI:----
    #### UI input:----
    ui_input = function(.tag_ = "UI_input") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(

      )

    },
    #### UI output:----
    ui_output = function(.tag_ = "UI_output", .height_ = "700px") {

      private$uiOutput_Id_ <- private$ns_(.tag_)

      tagList(
        plotOutput(outputId = private$uiOutput_Id_,
                   height = .height_)
      )

    },
    #### UI rendered output:----
    ui_render_output = function(.tag_ = "UI_render_output") {

      private$uiRenderedOutput_Id_ <- private$ns_(.tag_)

    },

    ### server:----
    server = function(input, output, session, .plot_) {

      output[[private$uiOutput_Id_]] <- renderPlot({
        .plot_
      })

    },
    ### Getters:----
    #### Get UI input ID:----
    get_uiInId = function() {

      return(private$uiInput_Id_)
    },
    #### Get UI output ID:----
    get_uiOtId = function() {

      return(private$uiOutput_Id_)
    },
    #### Get UI rendered output ID:----
    get_uiRdOtId = function() {

      return(private$uiRenderedOutput_Id_)
    }

  ),

  private = list(
    ## Fields:----
    label_ = NULL,
    id_ = NULL,
    ns_ = NULL,
    uiInput_Id_ = NULL,
    uiOutput_Id_ = NULL,
    uiRenderedOutput_Id_ = NULL

    ## Methods:----

  )
)

# Table - DT:----
dataTableDT = R6::R6Class(
  classname = 'dataTableDT',
  public = list(
    ## Fields:----

    ## Methods:----
    ### Initialise:----
    initialize = function(.label_) {

      private$label_ = .label_
      private$id_ = uuid::UUIDgenerate()
      private$ns_ = NS(private$id_)

    },
    ### UI:----
    #### UI input:----
    ui_input = function(.tag_ = "UI_input") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(

      )

    },
    #### UI output:----
    ui_output = function(.tag_ = "UI_output", .width_ = "100%") {

      private$uiOutput_Id_ <- private$ns_(.tag_)

      tagList(
        DT::dataTableOutput(outputId = private$uiOutput_Id_,
                            width = .width_)
      )

    },
    #### UI rendered output:----
    ui_render_output = function(.tag_ = "UI_render_output") {

      private$uiRenderedOutput_Id_ <- private$ns_(.tag_)

    },

    ### server:----
    server = function(input, output, session, .table_) {

      output[[private$uiOutput_Id_]] <- DT::renderDataTable(
        server = FALSE,
        expr = {
          DT::datatable(
            extensions = 'Buttons',
            options = list(
              dom = 'Bfrtip',
              scrollX = FALSE,
              pageLength = 10,
              buttons = c('csv', 'excel'),
              filter = c("none")
            ),
            .table_
          )
        }
      )

    },
    ### Getters:----
    #### Get UI input ID:----
    get_uiInId = function() {

      return(private$uiInput_Id_)
    },
    #### Get UI output ID:----
    get_uiOtId = function() {

      return(private$uiOutput_Id_)
    },
    #### Get UI rendered output ID:----
    get_uiRdOtId = function() {

      return(private$uiRenderedOutput_Id_)
    }

  ),

  private = list(
    ## Fields:----
    label_ = NULL,
    id_ = NULL,
    ns_ = NULL,
    uiInput_Id_ = NULL,
    uiOutput_Id_ = NULL,
    uiRenderedOutput_Id_ = NULL

    ## Methods:----

  )
)
