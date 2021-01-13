library(h5vc)
library(rhdf5)
library(ggplot2)
library(data.table)
library(dplyr)
library(stringr)

library(rtracklayer)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  # queryby is used for conditional inputs in the input
  queryby <- reactive({
    switch(input$criteria,
           "TRDB id" = 1,
           "position" = 2,
           "gene" = 3,
           4)
  })
  output$queryby <- reactive({
    queryby()
  })
  # tr_list keeps all the queried tr ids
  tr_list <- reactive({
    refset = fread(file = "data/refset.tsv")
    criteria = queryby()
    if (criteria == 1 ) {
      #by TR id
      trs = str_trim(string = input$query_trid, side = "both")
    } else if (criteria == 2) {
      c = ""
      s = -1
      e = -1
      # by hg38 position
      ##### TODO: move to tools
      position = input$query_position
      print(position)
      if(str_detect(position, "chr[1-22|X|Y|M]:[0-9]+-[0-9]+")) {
        # chrX:1000-2000
        c   = str_extract(position, "chr[1-22|X|Y|M]")
        s = str_extract(position, ":[0-9]+")
        s = substring(text = s, first = 2)
        e   = str_extract(position, "-[0-9]+")
        e   = substring(text = e, first = 2)
      } else if (str_detect(position, "[1-22|X|Y|M]:[0-9]+-[0-9]+")) {
        # X:1000-2000
        c   = str_extract(position, "[1-22|X|Y|M]:")
        c   = paste0("chr", 
                      substring(text = c, first = 1, last = str_length(string = c)-1))
        s = str_extract(position, ":[0-9]+")
        s = substring(text = s, first = 2)
        e   = str_extract(position, "-[0-9]+")
        e   = substring(text = e, first = 2)
      } else if (str_detect(position, "chr[1-22|X|Y|M]\t[0-9]+\t[0-9]+")) {
        # X:1000-2000
        array   = strsplit(position, "\t")[[1]]
        c = array[1]
        s = as.numeric(array[2])
        e   = as.numeric(array[3])
      } else if (str_detect(position, "[1-22|X|Y|M]\t[0-9]+\t[0-9]+")) {
        # X:1000-2000
        array   = strsplit(position, "\t")[[1]]
        c   = paste0("chr", array[1])
        s = as.numeric(array[2])
        e   = as.numeric(array[3])
      } else if (str_detect(position, "[1-22|X|Y|M]")) {
        # X
        c   = str_extract(position, "[1-22|X|Y|M]")
        c   = paste0("chr", c)
        s = -1
        e = -1
      }
       else {
         # error # TODO
         print("error")
       }
      print(c(c, s, e))
      ##### 
      if (s == -1) {
        trs = refset[chr==c]$TRid
      } else {
        trs = refset[chr == c & 
                ((start <= s & end >= e) |
                 (start >= s & start <= e) | 
                 (end >= s & end <= e))]$TRid
      }
    }
    print(paste("Selected", length(trs), "TRs."))
    as.character(trs)
  })
  output$tr_list <- reactive({
    tr_list()
  })
  
  output$browser <- renderUI({
    trs = tr_list()
    if(length(trs) > 0) {
      refset = fread("data/refset.tsv")[TRid == input$brosweTR]
      print(input$brosweTR)
      position = paste(refset$chr, ":", refset$start, "-", refset$end)
      list(tags$a(href=paste0("http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&position=", position), 
                  position), 
           tags$iframe(src = paste0("https://genome.ucsc.edu/cgi-bin/hgRenderTracks?db=hg38&position=", 
                                    position, 
                                    "&hgt.customText=http://mzrasekh.com/refset.bed"), 
                             height=800, 
                             width="100%"))
    }
    
  })
  
  output$selected_trs <- renderDataTable({
    refset = fread("data/refset.tsv")
    trs = tr_list()
    print(paste("selected_trs:", length(trs)))
    if(length(trs) > 0) {
      updateSelectInput(session, inputId = "brosweTR", choices = trs)
      refset[TRid %in% trs]
    } else {
      refset
    }
  })
  outputOptions(output, "queryby", suspendWhenHidden = FALSE) 
  outputOptions(output, "tr_list", suspendWhenHidden = FALSE) 
})