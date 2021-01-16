library(h5vc)
library(rhdf5)
library(ggplot2)
library(data.table)
library(dplyr)
library(stringr)
library(shiny)
#library(bedr)
library(ggplot2)
#library(rtracklayer)
require(ggseqlogo)
source("tools/wrapAroundAlign.R")

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  server_values = reactiveValues()
  
  # queryby is used for conditional inputs in the input
  observeEvent(input$search, {
    # TR characteristics
    copy_limit = input$query_copynumber
    pattern_limit = input$query_patternlength
    array_limit = input$query_arraysize
    # position
    position = input$query_position
    # by gene
    gene = input$query_gene
    upstream = input$query_upstream
    downstream = input$query_upstream
    # make trs and update the browseTR selectinput
    refset = fread(file = "data/refset.tsv")
    idx = rep(T, nrow(refset))
    if(input$by_copynumber) {
      idx = idx & 
        refset$copy_number >= input$query_copynumber[1] &
        refset$copy_number <= input$query_copynumber[2]
    }
    if(input$by_patternlength) {
      idx = idx & 
        refset$pattern >= input$query_patternlength[1] &
        refset$pattern <= input$query_patternlength[2]
    }
    if(input$by_arraysize) {
      idx = idx & 
        refset$pattern >= input$query_arraysize[1] &
        refset$pattern <= input$query_arraysize[2]
    }
    # by position
    if(input$by_position) {
      idx = idx & refset$chr == input$query_chr
      if (input$query_position[1] > 0) {
        idx = idx & refset$start >= input$query_position[1]
      }
      if (input$query_position[2] > 0) {
        idx = idx & refset$end <= input$query_position[2]
      }
    }
    # by gene
    if(input$by_gene) {
      gencode = fread("data/gencode.v34.basic.annotation.bed")[annotation == "gene" &
                                                               name == input$query_gene]
      if(nrow(gencode) == 0) {
        showNotification(ui = "Gene not found", 
                         type = "error",
                         closeButton = TRUE)
        return(c())
      }
      for (i in 1:nrow(gencode)) {
        gene = gencode[eval(i)]
        idx = idx & 
          refset$chr == gene$chr
          refset$end >= gene$start & 
          refset$start <= gene$end
      }
    }
    
    server_values$trs = as.character(refset[idx,]$TRid)
    updateSelectInput(session, 
                      inputId = "brosweTR", 
                      choices = server_values$trs)
    server_values$trs
  })
  
  output$browser <- renderUI({
    data = system(paste("grep 182168805", #input$brosweTR, 
                 "data/refset_full.tsv"), intern = T)
    if (data == "") {
      showNotification(ui = "The selected TR was not in the reference set", 
                       type = "error",
                       closeButton = TRUE)
      return(NULL)
    }
    data = str_split(string = data, pattern = "\t")[[1]]
    
    position = data[2]
    
    list(tags$a(href=paste0("http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&position=", position), 
                position), 
         tags$iframe(src = paste0("https://genome.ucsc.edu/cgi-bin/hgRenderTracks?db=hg38&position=", 
                                  position, 
                                  "&hgt.customText=http://mzrasekh.com/refset.bed"), 
                           height=800, 
                           width="100%"))
    
  })
  
  output$selected_trs <- renderDataTable({
    refset = fread("data/refset.tsv")
    if(!"trs" %in% names(server_values))
    {
      server_values$trs = c()
    } 
    trs = server_values$trs
    
    if(length(trs) > 0) {
      print(paste("selected_trs:", length(trs)))
        refset[TRid %in% trs]
     } else {
        refset
    }
  })
  
  # the fancy display of the motif
  output$pattern_logo <- renderPlot({
    print(input$brosweTR)
    if(input$brosweTR == "") {
      showNotification(ui = "Choose one TR to show motif alignment", 
                       type = "error",
                       closeButton = TRUE)
      return(NULL)
    }
    data = system(paste("grep", input$brosweTR, 
                        "data/refset_full.tsv"), intern = T)
    if (data == "") {
      showNotification(ui = "The selected TR was not in the reference set", 
                       type = "error",
                       closeButton = TRUE)
      return(NULL)
    }
    data = str_split(string = data, pattern = "\t")[[1]]
    #FlankingRight1000 = data[6]
    #FlankingLeft1000 = data[7]
    ArraySequence = data[9]
    PatternSequence = data[8]
    alignment = wrapAroundAlign(pattern = PatternSequence, 
                                sequence = ArraySequence)
    print(length(alignment$alignment))
      ggplot() + 
        geom_logo(data = alignment$alignment , method = "prob") +
        theme_logo()
  })

})