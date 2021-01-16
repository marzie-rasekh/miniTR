## ui.R ##
library(shinydashboard)
library(shinyLP)
library(shinyWidgets)


dashboardPage(
  dashboardHeader(title = "MiniPOP"),
  dashboardSidebar(
    sidebarMenu(
      checkboxInput(
        inputId = "by_copynumber",
        label = "Query by copy number",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.by_copynumber == true",
        numericRangeInput(
          inputId = "query_copynumber",
          label = "limit",
          separator = " to ",
          value = c(1.75, 630)
        )
      ),
      checkboxInput(
        inputId = "by_patternlength",
        label = "Query by pattern length",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.by_patternlength == true",
        numericRangeInput(
          inputId = "query_patternlength",
          label = "from",
          separator = " to ",
          value = c(7, 1994)
        )
      ),
      checkboxInput(
        inputId = "by_arraysize",
        label = "Query by array size",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.by_arraysize == true",
        numericRangeInput(
          inputId = "query_arraysize",
          label = "from",
          separator = " to ",
          value = c(13, 108907)
        )
      ),
      checkboxInput(
        inputId = "by_position",
        label = "Query by position (hg38)",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.by_position == true",
        selectInput(
          inputId = "query_chr",
          label = "chr",
          choices = c(1:22, "X", "Y")
        ),
        numericRangeInput(
          inputId = "query_position",
          label = "from",
          separator = " to ",
          value = c(0, 0)
        )
      ),
      checkboxInput(
        inputId = "by_gene",
        label = "Query by gene overlap",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.by_gene == true",
        textInput(
          inputId = "query_gene",
          label = "gene name",
          value = ""
        ),
        numericInput(
          inputId = "query_upstream",
          label = "Upstream",
          value = 0,
          step = 1000
        ),
        numericInput(
          inputId = "query_downstream",
          label = "Downstream",
          value = 0,
          step = 1000
        )
      ),
      actionButton(inputId = "search", label = "Search")
    )
  ),
  dashboardBody(
    selectInput(
      inputId = "brosweTR",
      label = "Select TR id",
      choices = c()
    ),
    fluidPage(
      tabBox(
        id = "display",
        width = 12,
        tabPanel(
          id = "query",
          title = "Reference TRs",
          dataTableOutput(outputId = "selected_trs")
        ),
        tabPanel(
          id = "motif",
          title = "Motif Display",
          plotOutput(outputId = "pattern_logo", height = "100px"),
          htmlOutput(outputId = "query_js_wraparound_display")
        ),
        tabPanel(
          id = "browser",
          title = "Genome Browser",
          h3("Genome Browser"),
          htmlOutput("browser")
        ),
        tabPanel(id = "genotypes",
                 title = "Genotypes"),
        tabPanel(id = "population",
                 title = "Population-biased"),
        tabPanel(id = "eqtl",
                 title = "eQTLs"),
        tabPanel(id = "selection",
                 title = "Rare alleles"),
        tabPanel(id = "haplotypes",
                 title = "Haplotypes",
                 h2("Coming soon..."))
      )
    )
  )
)
  