install.packages("shiny", dependencies = T)
install.packages("shinydashboard", dependencies = T)
install.packages("shinyLP", dependencies = T)
install.packages("htmltools", dependencies = T)
install.packages("shinyWidgets", dependencies = T)
install.packages("ggplot2")
install.packages("ggseqlogo")
install.packages("ggplot2")
install.packages("DT")
install.packages("data.table")
install.packages("stringr")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("h5vc")
BiocManager::install("rhdf5")

