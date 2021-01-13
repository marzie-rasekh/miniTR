## ui.R ##
library(shinydashboard)
library(shinyLP)

dashboardPage(
  dashboardHeader(title = "MiniPOP"),
  dashboardSidebar(
    sidebarMenu(
      textInput(inputId = "search",
                width = '90%', 
                label = "Search:", 
                value = "chr7:156101194-156101584", 
                placeholder = "Search by position (hg38), gene, or TR id."),
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
      # First tab content
      tabItem(tabName = "browser",
              fluidRow(
                box(
                  width = 12,
                  height = '100%',
                  h3("Results for TRs:"),
                  sliderInput("windowsize", 
                              "Windowsize:", 
                              min = 10,
                              max = 200,
                              value = 50,
                              step = 5),
                  htmlOutput("browser")
                )
              )
      )
    )
  )
)
