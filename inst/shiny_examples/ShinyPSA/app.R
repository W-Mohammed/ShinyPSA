################################################################################
#
# Script Name:        app.R
# Module Name:        Economic/PSA/demo app
# Script Description: Triggers the shiny app
# Author:             WM-University of Sheffield (wmamohammed1@sheffield.ac.uk)
#
################################################################################

# Load ShinyPSA library:----
# install it locally and rsconnect will take care of the reset
library("devtools")
library("ShinyPSA")

# Instantiate a copy of class ShinyPSA_R6_App:----
app = ShinyPSA_R6_App$new()

# Run the app:----
#thematic::thematic_shiny(font = "auto") # allows themes on ggplot2 plots
shiny::shinyApp(app$ui(), app$server)
