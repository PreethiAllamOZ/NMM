#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("./global.R")
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  shinyjs::useShinyjs()
  shinyalert::useShinyalert()
  
  reactiveDF<-reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    k<-Model(df)
    return(k)
    
  })
  
  output$mytable = DT::renderDataTable({
    req(input$file1)
    
    #datatable(reactiveDF)%>%formatStyle('CloseValue_WM')
    return(DT::datatable(reactiveDF(), options = list(pageLength = 100), filter = c("top"))%>% 
             formatStyle('CloseValue_WM', backgroundColor =  c('lightyellow'))%>%
             formatStyle('CloseValue_Carra_Model_No_V', backgroundColor =  c('lightgreen'))%>%
             formatStyle('CloseValue_Carra_Model', backgroundColor =  c('lightgray'))%>%
             formatStyle('CloseValue_Starra', backgroundColor =  c('wheat'))%>%
             formatStyle('CloseValue_Promhill_Model', backgroundColor =  c('azure')))
  })
  
  #'CloseValue_Carra_Model_No_V','CloseValue_Carra_Model','CloseValue_Starra','CloseValue_Promhill_Model'
  # Downloadable csv of selected dataset ----
  output$DownloadData2 <- downloadHandler(
    filename = function() {
      paste("Sample", ".csv", sep="")
    },
    content = function(file) {
      write.csv(Samplefile, file, row.names = FALSE)
    }
  )
  
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveDF(), file, row.names = FALSE)
    }
  )
  
  
  
    

})
