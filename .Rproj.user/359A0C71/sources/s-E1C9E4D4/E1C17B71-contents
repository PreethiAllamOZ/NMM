#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("./global.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  shinyjs::useShinyjs()
  shinyalert::useShinyalert()
  
  reactiveDF<-reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    k<-Model(df)
    #df$predictions<-predict(irisModel, newdata = df, type ="class")
    return(k)
    
  })
  
  output$mytable = DT::renderDataTable({
    req(input$file1)
    
    return(DT::datatable(reactiveDF(), options = list(pageLength = 100), filter = c("top")))
  })
  
  
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
