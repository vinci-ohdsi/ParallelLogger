
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2) 


# Define UI for application that draws a histogram
ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Base log file", placeholder = "logMain.txt"),
      fileInput("file2", "Test log file", placeholder = "logDevelop.txt"),
      actionButton("parseData", "Run")
    ),
    
    mainPanel(
      plotOutput("resultsPlot", width = "60%")
    )
  )
)


server <- function(input, output) {
  results <- eventReactive(input$parseData, {
    req(input$file1)
    file1Data <- read.table(input$file1$datapath, sep = "\t")
    
    names(file1Data) <- c("timestamp", "description") 
    file1Data <- filter(file1Data, grepl("benchmark \\|", description, ignore.case = TRUE))
    file1Data <- tidyr::separate_wider_delim(file1Data, description, 
                                             delim = "|", 
                                             names = c("tmp", "group", "description", "event"))
    
    file1Data <- select(file1Data, -tmp)
    file1Data$timestamp <- as.POSIXct(file1Data$timestamp)
    file1Data <- mutate(file1Data,
                        across(where(is.character), trimws))
    file1Data <- distinct(file1Data)
    file1Data <- pivot_wider(file1Data, names_from = event, values_from = timestamp)
    file1Data$elapsed <- as.numeric(difftime(file1Data$finish, file1Data$start), units = "mins")
    
    ####
    req(input$file2)
    file2Data <- read.table(input$file2$datapath, sep = "\t")
    
    names(file2Data) <- c("timestamp", "description") 
    file2Data <- filter(file2Data, grepl("benchmark \\|", description, ignore.case = TRUE))
    file2Data <- tidyr::separate_wider_delim(file2Data, description, 
                                             delim = "|", 
                                             names = c("tmp", "group", "description", "event"))
    
    file2Data <- select(file2Data, -tmp)
    file2Data$timestamp <- as.POSIXct(file2Data$timestamp)
    file2Data <- mutate(file2Data,
                        across(where(is.character), trimws))
    file2Data <- distinct(file2Data)
    file2Data <- pivot_wider(file2Data, names_from = event, values_from = timestamp)
    file2Data$elapsed <- as.numeric(difftime(file2Data$finish, file2Data$start), units = "mins")
    
    combined <- rbind(file1Data, file2Data)
    
    refGroup <- distinct(file1Data, group) %>% pull()
    
    combined <- combined %>% 
      group_by(description) %>% 
      mutate(normalizedTime = elapsed / elapsed[group == refGroup]) %>%
      ungroup() 
    
    ggplot(combined, 
           aes(x = normalizedTime, 
               y = group, 
               fill = group)) +
      geom_col() +
      facet_wrap(~description, ncol = 1)
    
  })
  
  output$resultsPlot <- renderPlot(results())
}

# Run the application 
shinyApp(ui = ui, server = server)
