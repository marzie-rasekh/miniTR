## ui.R ##
library(shinydashboard)
library(shinyLP)

dashboardPage(
  dashboardHeader(title = "MiniPOP"),
  dashboardSidebar(
    sidebarMenu(
      selectInput(inputId = "brosweTR", 
                  label = "Select TR id", 
                  choices = c()),
      menuItem("Query", tabName = "query", icon = icon("dashboard")),
      menuItem("Genome Browser", tabName = "browser", icon = icon("dashboard")),
      menuItem("Genotypes", tabName = "genotype", icon = icon("exclamation-circle")),
      menuItem("Population biased alleles", tabName = "population", icon = icon("exclamation-circle")),
      menuItem("eQTLs", tabName = "eqtl", icon = icon("exclamation-circle")),
      menuItem("VNTRs under selection", tabName = "selection", icon = icon("exclamation-circle")),
      menuItem("Haplotypes", tabName = "haplotypes", icon = icon("exclamation-circle")),
      menuItem("Tools", tabName = "help", icon = icon("question-circle")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      menuItem("Citation", tabName = "citation", icon = icon("exclamation-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "query",
              fluidRow(
                box(
                  width = 2, 
                  title = "Query the minisatellites",
                  height = "100%",
                  selectInput(inputId = "criteria", 
                              label = "Query by ", 
                              choices = c("TRDB id", 
                                          "position",
                                          "gene"), 
                              selected = "position"),
                  conditionalPanel(condition = "output.queryby == 1",
                                   textInput(inputId = "query_trid", 
                                             label = "TR id", 
                                             value = "")),
                  conditionalPanel(condition = "output.queryby == 2",
                                   textInput(inputId = "query_position", 
                                             label = "(hg38)", 
                                             value = "")),
                  conditionalPanel(condition = "output.queryby == 3",
                                   textInput(inputId = "query_gene", 
                                             label = "gene name", 
                                             value = ""),
                                   numericInput(inputId = "query_upstream", 
                                                label = "Upstream", 
                                                value = 0, 
                                                step = 1000),
                                   numericInput(inputId = "query_upstream", 
                                                label = "Downstream", 
                                                value = 0, 
                                                step = 1000))
                  
                ),
                box(width = 10, title = "Results",
                    dataTableOutput(outputId = "selected_trs"))
              )
      ),
      tabItem(tabName = "browser",
              fluidRow(
                box(
                  width = 12,
                  height = '100%',
                  h3("Genome Browser"),
                  htmlOutput("browser")
                )
              )
      )
    )
  )
)
