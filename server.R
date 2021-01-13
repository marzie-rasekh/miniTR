library(h5vc)
library(rhdf5)
library(ggplot2)
library(data.table)
library(dplyr)

library(rtracklayer)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  output$browser <- renderUI({
    my_test <- tags$iframe(src=paste0("https://genome.ucsc.edu/cgi-bin/hgRenderTracks?db=hg38&position=", 
                                      input$search, 
                                      "&hgt.customText=http://mzrasekh.com/refset.bed"), 
                           height=800, 
                           width="100%")
    my_test
  })
})