library(h5vc)
library(rhdf5)
library(ggplot2)
library(data.table)
library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
require(ggseqlogo)
library(DT)
source("tools/wrapAroundAlign.R")
source("tools/comparisonTools.R")

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
    downstream = input$query_upstream
    # make trs and update the browseTR selectinput
    refset = fread(file = "data/refset.tsv")
    idx = rep(T, nrow(refset))
    if(input$by_copynumber) {
      print(paste("Copy number:", 
                  input$input$query_copynumber[1],
                  "-",
                  input$input$query_copynumber[2]))
      idx = idx & 
        refset$copy_number >= input$query_copynumber[1] &
        refset$copy_number <= input$query_copynumber[2]
    }
    if(input$by_patternlength) {
      print(paste("Pattern:", 
                  input$query_patternlength[1],
                  "-",
                  input$query_patternlength[2]))
      idx = idx & 
        refset$pattern >= input$query_patternlength[1] &
        refset$pattern <= input$query_patternlength[2]
    }
    if(input$by_arraysize) {
      print(paste("Array:", 
                  input$query_arraysize[1],
                  "-",
                  input$query_arraysize[2]))
      idx = idx & 
        refset$pattern >= input$query_arraysize[1] &
        refset$pattern <= input$query_arraysize[2]
    }
    # by position
    if(input$by_position) {
      print(paste0("Position at ", 
                  input$query_chr, ":",
                  input$query_position[1],
                  "-",
                  input$query_position[2]))
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
      print(paste("Gene", input$query_gene))
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
    
    if(sum(idx) > 0) {
      print(paste("Total TRs ", sum(idx)))
      server_values$TRs =refset[idx,]
      
      updateSelectInput(session, 
                        inputId = "browseTR", 
                        choices = server_values$TRs$TRid) 
    } else {
      showNotification(ui = "No TRs found with given criteria.", 
                       type = "error",
                       closeButton = TRUE)
    }
    
  })
  
  output$browser <- renderUI({
    data = system(paste("grep ", input$browseTR, 
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
    if(!"TRs" %in% names(server_values))
    {
      refset
    } else {
      refset[TRid %in% server_values$TRs$TRid]
    }
  }, selection = "single")
  
  observeEvent(input$selected_trs_rows_selected, {
    ids = input$selected_trs_rows_selected
    print(ids)
    if (length(ids > 0)) {
      if(is.null(server_values$TRs)) {
        tr = refset = fread("data/refset.tsv")$TRid[ids]
        updateSelectInput(session, 
                          inputId = "browseTR", 
                          choices = tr, 
                          selected = tr)
      } else {
        updateSelectInput(session, 
                          inputId = "browseTR", 
                          choices = server_values$TRs$TRid, 
                          selected = server_values$TRs$TRid[ids])
      }
       
    }
  })
  # the fancy display of the motif
  output$pattern_logo <- renderPlot({
    if ("tandem_repeat" %in% names(server_values)) {
      ggplot() + 
        geom_logo(data = server_values$tandem_repeat$alignment , 
                  method = "prob",
                  seq_type='dna') +
        theme_logo()
    } else {
      showNotification(ui = "The selected TR was not in the reference set", 
                       type = "error",
                       closeButton = TRUE)
      return(NULL)
    }
      
  })
  output$motif_plot <- renderUI({
    print(input$browseTR)
    if(input$browseTR == "") {
      showNotification(ui = "Choose one TR to show motif alignment", 
                       type = "error",
                       closeButton = TRUE)
      return(NULL)
    }
    data = system(paste("grep", input$browseTR, 
                        "data/refset_full.tsv"), intern = T)
    if (data == "") {
      showNotification(ui = "The selected TR was not in the reference set", 
                       type = "error",
                       closeButton = TRUE)
      return(NULL)
    }
    data = str_split(string = data, pattern = "\t")[[1]]
    ArraySequence = data[9]
    PatternSequence = data[8]
    LeftFlank = data[7]
    RightFlank = data[6]
    tandem_repeat = wrapAroundAlign(pattern = PatternSequence, 
                                    sequence = ArraySequence)
    LeftFlank = paste0(LeftFlank, tandem_repeat$left_flank)
    tandem_repeat$left_flank = substr(x = LeftFlank, 
                                      start = str_length(LeftFlank)-50, 
                                      stop = str_length(LeftFlank))
    tandem_repeat$right_flank = substr(x = paste0(tandem_repeat$right_flank, RightFlank), 
                                       start = 1, 
                                       stop = 50)
    server_values$tandem_repeat = tandem_repeat
    print(str_length(tandem_repeat$consensus))
    
    plotOutput("pattern_logo", 
               width = 17*str_length(tandem_repeat$consensus),
               height = 150)
  })
   output$query_js_wraparound_display <- renderUI({
     if ("tandem_repeat" %in% names(server_values)) {
       alignmentsVisualization(server_values$tandem_repeat$alignment, 
                               server_values$tandem_repeat$consensus)
     } else {
       showNotification(ui = "The selected TR was not in the reference set", 
                        type = "error",
                        closeButton = TRUE)
       return(NULL)
     }
  })
})


