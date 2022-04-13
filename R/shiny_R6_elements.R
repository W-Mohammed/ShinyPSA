################################################################################
#
# Script Name:        shiny_R6_app.R
# Module Name:        Economic/PSA/demo app
# Script Description: Defines a set of classes used in the shiny app
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################
# prettySwitch:----
#' R6 Class employing shinyWidgets::prettySwitch.
#'
#' @description
#' An instance of this class is used in similar way to
#' shinyWidgets::prettySwitch
#' @format An [R6::R6Class] object.
#' @name prettySwitch
NULL
#'
#' @rdname prettySwitch
#' @export
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
    ui_input = function(.tag_ = "UI_input", .value_ = FALSE,
                        .class_ = "flex-fill text-right") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          class = .class_, #"flex-fill text-right"
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
#' R6 Class employing shiny::actionButton
#'
#' @description
#' An instance of this class is used in similar way to shiny::actionButton
#' @format An [R6::R6Class] object.
#' @name actionButton
NULL
#'
#' @rdname actionButton
#' @export
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
                        .class_ = "ml-2 pt-2 d-flex align-items-center
                        text-right",
                        .style_ = "display: flex;
                        margin-top: 2rem !important;",
                        .width_ = NULL) {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          class = .class_, #ml-2 "d-flex align-items-center text-right"
          style = .style_, #"display: flex; margin-top: 2rem !important;",
          actionButton(
            inputId = private$uiInput_Id_,
            label = private$label_,
            class = "btn-primary",
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
#' R6 Class employing shiny::inputSelection
#'
#' @description
#' An instance of this class is used in similar way to
#' shiny::inputSelection
#' @format An [R6::R6Class] object.
#' @name inputSelection
NULL
#'
#' @rdname inputSelection
#' @export
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
                        .class_ = "d-flex align-items-center",
                        .style_ = "display: flex; margin-top:
                        2rem !important;") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          style = .style_, #"display: flex; margin-top: 2rem !important;"
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
#' R6 Class employing shiny::renderPlot & shiny::plotOutput
#'
#' @description
#' An instance of this class is used in similar way to shiny::renderPlot &
#' shiny::plotOutput
#' @format An [R6::R6Class] object.
#' @name ggplot2Plot
NULL
#'
#' @rdname ggplot2Plot
#' @export
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
    ui_output = function(.tag_ = "UI_output", .height_ = "800px") {

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

      output[[private$uiOutput_Id_]] <- renderPlot(
        expr = {
          .plot_
        },
        res = 148, # 96 120 are good values for resolution in shiny
        bg = "transparent")

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
#' R6 Class employing DT::renderDataTable & DT::dataTableOutput
#'
#' @description
#' An instance of this class is used in similar way to DT::renderDataTable
#' & DT::dataTableOutput
#' @format An [R6::R6Class] object.
#' @name dataTableDT
NULL
#'
#' @rdname dataTableDT
#' @export
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

# sliderInput:----
#' R6 Class employing shiny::sliderInput
#'
#' @description
#' An instance of this class is used in similar way to shiny::sliderInput
#' @format An [R6::R6Class] object.
#' @name sliderInput
NULL
#'
#' @rdname sliderInput
#' @export
sliderInput = R6::R6Class(
  classname = 'sliderInput',
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
    ui_input = function(.tag_ = "UI_input", .min_, .max_, .value_,
                        .class_ = "d-flex align-items-center") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          style = "display: flex;", #"margin-top: 2rem !important;",
          class = .class_, #d-flex align-items-center text-right #px-5 py-4
          sliderInput(
            inputId = private$uiInput_Id_,
            label = private$label_,
            min = .min_,
            max = .max_,
            value = .value_,
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
    server = function(input, output, session, .min_, .max_) {

      updateSliderInput(
        session = session,
        inputId = private$uiInput_Id_,
        min = .min_,
        max = .max_
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

# textInput:----
#' R6 Class employing shiny::textInput
#'
#' @description
#' An instance of this class is used in similar way to shiny::textInput
#' @format An [R6::R6Class] object.
#' @name textInput
NULL
#'
#' @rdname textInput
#' @export
textInput = R6::R6Class(
  classname = 'textInput',
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
    ui_input = function(.tag_ = "UI_input", .width_ = "100%",
                        .class_ = "d-flex align-items-center",
                        .value_ = "500, 20000, 30000, 50000") {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          style = "display: flex;", # "margin-top: 2rem !important;",
          class = .class_, #d-flex align-items-center text-right #px-5 py-4
          textInput(
            inputId = private$uiInput_Id_,
            label = private$label_,
            width = .width_,
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
    server = function(input, output, session, .min_, .max_) {

      # updateSliderInput(
      #   session = session,
      #   inputId = private$uiInput_Id_,
      #   min = .min_,
      #   max = .max_
      # )

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


# numericInput:----
#' R6 Class employing shiny::numericInput
#'
#' @description
#' An instance of this class is used in similar way to shiny::numericInput
#' @format An [R6::R6Class] object.
#' @name numericInput
NULL
#'
#' @rdname numericInput
#' @export
numericInput = R6::R6Class(
  classname = 'numericInput',
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
    ui_input = function(.tag_ = "UI_input", .width_ = "100%",
                        .class_ = "d-flex align-items-center",
                        .value_ = "500, 20000, 30000, 50000",
                        .min_ = 0, .max_ = 1) {

      private$uiInput_Id_ <- private$ns_(.tag_)

      tagList(
        div(
          style = "display: flex;", # "margin-top: 2rem !important;",
          class = .class_, #d-flex align-items-center text-right #px-5 py-4
          numericInput(
            inputId = private$uiInput_Id_,
            label = private$label_,
            width = .width_,
            value = .value_,
            min = .min_,
            max = .max_
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
    server = function(input, output, session, .min_, .max_) {

      # updateSliderInput(
      #   session = session,
      #   inputId = private$uiInput_Id_,
      #   min = .min_,
      #   max = .max_
      # )

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
