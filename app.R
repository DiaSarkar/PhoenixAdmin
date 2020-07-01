# Download Library
install.packages('shinydashboard')
library(shiny)
library(shinydashboard)
library(entropy)
library(FSelector)
library(shinythemes)
library(ggplot2)
library(dygraphs)
library(coefplot)
library(xts)
library(forecast)
library(tidyverse)

ui <-
  
  dashboardPage(
    skin = "purple",
    dashboardHeader(),
    dashboardSidebar(sidebarMenu(
      id = "tabs",
      menuItem(
        "Data",
        tabName = "data",
        icon = icon("download")
        ),
        menuItem(
          "Descriptive Analytics",
          tabName = "analytics",
          icon = icon("dashboard")
        ),
        menuItem(
          "Predictive Modeling",
          tabName = "modeling",
          icon = icon("chart-line")
        )
      )), 
    dashboardBody(
      tags$head(tags$style(
        HTML(
          '
                                            .main-header .logo {
                                            font-family: "Georgia", Times, "Times New Roman", serif;
                                            font-weight: bold;
                                            font-size: 24px;
                                              }
    '
        )
      )),
      tabItems(
        # First tab content
        tabItem(
          tabName = "data",
          
          verticalLayout(
            titlePanel("Upload Supplier Historic Data"),
            fileInput(
              "historicdatafile",
              "Choose CSV File",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
              
            ),
            
            actionButton(
              inputId = "uploaddata",
              label = "Upload",
              style = "gradient",
              color = "danger",
              icon = icon("thumbs-up")
            ),
            
            
            
            titlePanel("Client Data"),
            box(
              title = "Case Analyses Details",
              status = "primary",
              height =
                "595",
              width = "15",
              solidHeader = T,
              column(
                width = 12,
                
                DT::dataTableOutput("contents"),
                style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
              )
            )
            
          )
          
        ),
        
        # Second tab content
        tabItem(tabName = "analytics",
                
                verticalLayout(
                  h1("Descriptive Analytics"),
                  h1(""),
                  
                  box(
                    title = "Target Selection",
                    status = "primary",
                    height =
                      "300",
                    width = "20",
                    solidHeader = T,
                    column(
                      5,
                      
                      
                      selectInput(
                        inputId = "picktarget",
                        label = "Target Variable",
                        choices = NULL,
                        
                      ),
                      hr(),
                      verbatimTextOutput("targetvariabletb"),
                      
                      
                      actionButton(
                        inputId = "analysisbutton",
                        label = "Analysis",
                        style = "gradient",
                        color = "danger",
                        icon = icon("thumbs-up")
                      ),
                      
                      
                    ),
                    column(
                      5,
                      selectInput(
                        inputId = "dateindex",
                        label = "Select Date Index",
                        choices = NULL,
                        
                      )  ,
                      hr(),
                      verbatimTextOutput("dateIndextb"),
                      
                    )
                    
                  ),
                  
                  # mainPanel(
                  # Outputs excluded for brevity
                  box(
                    title = "Data Overview",
                    status = "primary",
                    height =
                      "595",
                    width = "100",
                    solidHeader = T,
                    dygraphOutput("trendselectorplot")
                  ),
                  
                  #h2("Informgation Gain"),
                  box(
                    title = "Information Gain",
                    status = "primary",
                    height =
                      "595",
                    width = "100",
                    solidHeader = T,
                    h3(textOutput("entropydisplay")),
                    plotOutput("infoplot")
                  ),
                  
                  box(
                    title = "Analysis",
                    status = "primary",
                    height =
                      "1200",
                    width = "100",
                    solidHeader = T,
                    column(
                      6,
                      
                      box(
                        title = "Hourly Plot",
                        status = "primary",
                        height =
                          "500",
                        width = "50",
                        solidHeader = T,
                        
                        plotOutput("hourlyplot")
                      )
                    ),
                    column(
                      6,
                      
                      box(
                        title = "Daily Plot",
                        status = "primary",
                        height =
                          "500",
                        width = "50",
                        solidHeader = T,
                        
                        plotOutput("dailyplot")
                      )
                      
                    ),
                    column(
                      6,
                      
                      box(
                        title = "Weekly Plot",
                        status = "primary",
                        height =
                          "500",
                        width = "50",
                        solidHeader = T,
                        
                        plotOutput("weeklyplot")
                      )
                      
                    ),
                    column(
                      6,
                      
                      box(
                        title = "Monthly Plot",
                        status = "primary",
                        height =
                          "500",
                        width = "50",
                        solidHeader = T,
                        
                        plotOutput("monthlyplot")
                      )
                      
                    )
                  ),
                  box(
                    title = "Decompose",
                    status = "primary",
                    height =
                      "600",
                    width = "100",
                    solidHeader = T,
                    plotOutput("Decompose"),
                    actionButton(
                      inputId = "nexttab",
                      label = "Model Fitting",
                      style = "gradient",
                      color = "danger",
                      icon = icon("Next")
                    )
                    
                  )
                  
                  
                )),
        
        tabItem(tabName = "modeling",
                h2("Widgets tab content"),
                fluidRow(
                  column(5,
                         # Inputs excluded for brevity
                         verticalLayout(
                           box(
                             title = "Select Forecast Method",
                             status = "primary",
                             height =
                               "200",
                             width = "15",
                             solidHeader = T,
                             selectInput(
                               inputId = "selectforecast",
                               label = "Forecast Method",
                               choices = c(
                                 "ARIMA",
                                 "SARIMAX",
                                 "Holt's Trend",
                                 "Holt's Winter",
                                 "VAR",
                                 "TBATS",
                                 "MSTL",
                                 "Naive Bayes",
                                 "Multiple Regression",
                                 "Random Forest",
                                 "Decision Tree"
                               )
                               
                             ),
                             
                             actionButton(
                               inputId = "modelbutton",
                               label = "Model Fitting",
                               style = "gradient",
                               color = "danger",
                               icon = icon("thumbs-up")
                             )
                             
                           ),
                           box(
                             title = "Forecast Stats",
                             status = "primary",
                             height =
                               "200",
                             width = "100",
                             solidHeader = T,
                             tableOutput("forecaststats"),
                             
                           )
                           
                           
                         )),
                  column(
                    7,
                    box(
                      title = "Forecast",
                      status = "primary",
                      height =
                        "595",
                      width = "100",
                      solidHeader = T,
                      plotOutput("modelplot")
                    ),
                    box(
                      title = "Residuals",
                      status = "primary",
                      height =
                        "595",
                      width = "150",
                      solidHeader = T,
                      plotOutput("modelresiduals")
                    )
                  )
                  
                  
                  
                  
                ))
      )
    )
  )

#))


server <- function(input, output, session) {
  customerdata <- reactive({
    input$historicdatafile
  })
  
  ######################################################################################
  #FUNCTIONS#
  #######################################################################################
  
  #Function to get the dataframe column names for the uploaded file
  customerdatacolumn <- function() {
    customerdf = read.csv(customerdata()$datapath)
    customer = as.data.frame(customerdf)
    customerdatalist <- list(colnames(customerdf))
    print(colnames(customerdf))
    
  }
  
  #Function to calculate the entropy
  entropyCalc <- function() {
    smart <- read.csv(customerdata()$datapath)
    print(c("Class of smartdf : ", class(smart)))
    inputTargetValue <- input$picktarget
    print(c("TargetValue : ", inputTargetValue))
    smarttarget <- table(smart[inputTargetValue])
    print(c("Smart Target", summary(smarttarget)))
    entropydata <- entropy(smarttarget, unit = "log2")
    print(c("Entropy of Dara : ", entropydata))
    return(entropydata)
    
  }
  
  #Function to add seperate hour, day, month, year column for aggregation analysis
  #This function returns a new data frame ammending new columns with respect to hour, day, month, year
  layeranalysis <- function() {
    dateIndextb <- input$dateindex
    datnew = read.csv(customerdata()$datapath)
    datnew$Weekday <- weekdays(as.Date(datnew[[dateIndextb]]))
    
    datnew[[dateIndextb]] <-
      as.POSIXct(datnew[[dateIndextb]], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    DateTime_Split_orginal = strsplit(as.character(datnew[[dateIndextb]]), " ")
    DateTime_Split = sapply(DateTime_Split_orginal, "[", 1)
    
    Time_Split = sapply(DateTime_Split_orginal, "[", 2)
    Date_Split = strsplit(as.character(DateTime_Split), "-")
    Year = sapply(Date_Split, "[", 1)
    Month = sapply(Date_Split, "[", 2)
    Day = sapply(Date_Split, "[", 3)
    Time_Split = strsplit(as.character(Time_Split), ":")
    Hour = sapply(Time_Split, "[", 1)
    datnewlayer = cbind(datnew[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)], Day, Month, Year, Hour)
    print(str(datnewlayer))
    return(datnewlayer)
  }
  
  
  ######################################################################################
  #This section has the complete Output render functions , modifying the output widgets#
  #######################################################################################
  
  #
  #Update the output table with selected file details
  customerdata <- reactive({
    input$historicdatafile
  })
  
  output$contents <- DT::renderDataTable({
    if (is.null(customerdata()))
      return(NULL)
    
    read.csv(customerdata()$datapath)
    
  })
  
  
  
  observeEvent(input$uploaddata, {
    customerdf = read.csv(customerdata()$datapath)
    
    updateSelectInput(session, "picktarget", choices = colnames(customerdf))
    updateSelectInput(session, "dateindex", choices = colnames(customerdf))
    updateTabItems(session, "tabs", "analytics")
    
    
  })
  
  
  
  output$targetvariabletb <- renderPrint({
    input$picktarget
  })
  
  output$dateIndextb <- renderPrint({
    input$dateindex
  })
  
  observeEvent(input$analysisbutton, {
    print("I am here")
    
    
    
    
    output$entropydisplay <- renderText({
      c("Calculated Entropy is : ", entropyCalc())
      
    })
    output$infoplot <- renderPlot({
      smartinfo <- read.csv(customerdata()$datapath)
      inputTargetValueinfo <- input$picktarget
      
      
      smartinfodisp <-
        smartinfo[,-which(names(smartinfo) %in% c(inputTargetValueinfo))]
      print(as.formula(paste(input$picktarget, " ~ ", ".")))
      
      infogain <-
        information.gain(as.formula(paste(input$picktarget, " ~ ", ".")), smartinfo, unit = "log2")
      print(colnames(smartinfodisp))
      barplot(
        unlist(infogain),
        names.arg = colnames(smartinfodisp),
        las = 2,
        col = "lightblue",
        main = "FEATURE INFORMATION GAIN"
      )
    })
    
    output$trendselectorplot <- renderDygraph({
      smarthhnew <- read.csv(customerdata()$datapath)
      print("I am 2nd")
      
      dateIndextb = input$dateindex
      xtsdata <-
        xts(smarthhnew[input$picktarget],
            order.by = as.POSIXct(smarthhnew[[dateIndextb]]),
            frequency = 48)
      dygraph(xtsdata, main = "Consumption Overtime") %>% dyRangeSelector()
    })
    
    
    dataanalysisdf <- layeranalysis()
    output$hourlyplot <- renderPlot({
      datahourdf <-
        aggregate(
          dataanalysisdf[input$picktarget],
          by = list(dataanalysisdf$Hour),
          FUN = mean,
          na.rm = TRUE
        )
      colnames(datahourdf) <- c("Hour", "Demand")
      
      
      barplot(
        datahourdf$Demand,
        main = "Aggregate Demand per Hour of the Day",
        xlab = "Hour",
        ylab = "Demand",
        names = datahourdf$Hour,
        names.arg = c(
          "0",
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9",
          "10",
          "11",
          "12",
          "13",
          "14",
          "15",
          "16",
          "17",
          "18",
          "19",
          "20",
          "21",
          "22",
          "23"
        ),
        col = "darkblue"
      )
    })
    output$dailyplot <- renderPlot({
      datedf <-
        aggregate(
          dataanalysisdf[input$picktarget],
          by = list(dataanalysisdf$Day),
          FUN = mean,
          na.rm = TRUE
        )
      
      colnames(datedf) <- c("Date", "Demand")
      
      barplot(
        datedf$Demand,
        main = "Aggregate Demand per Date of the Month",
        xlab = "Date",
        ylab = "Demand",
        names = datedf$Date,
        names.arg = c(
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9",
          "10",
          "11",
          "12",
          "13",
          "14",
          "15",
          "16",
          "17",
          "18",
          "19",
          "20",
          "21",
          "22",
          "23",
          "24",
          "25",
          "26",
          "27",
          "28",
          "29",
          "30",
          "31"
        ),
        col = "darkblue"
      )
    })
    output$weeklyplot <- renderPlot({
      weekday.df <-
        aggregate(
          dataanalysisdf[input$picktarget],
          by = list(dataanalysisdf$Weekday),
          FUN = mean,
          na.rm = TRUE
        )
      
      
      colnames(weekday.df) <- c("WeekDay", "Energy")
      
      barplot(
        weekday.df$Energy,
        main = "Aggregate Demand per WeekDay",
        xlab = "WeekDay",
        ylab = "Energy",
        names = weekday.df$WeekDay,
        names.arg = c("Fri", "Mon", "Sat", "Sun", "Thu", "Tue", "Wed"),
        col = "darkblue"
      )
    })
    output$monthlyplot <- renderPlot({
      month.df <-
        aggregate(
          dataanalysisdf[input$picktarget],
          by = list(dataanalysisdf$Month),
          FUN = mean,
          na.rm = TRUE
        )
      month.df
      
      colnames(month.df) <- c("Months", "Demand")
      
      barplot(
        month.df$Demand,
        main = "Aggregate Demand per Month",
        xlab = "Month",
        ylab = "Demand",
        names = month.df$Months,
        
        col = "darkblue"
      )
    })
    
    output$Decompose <- renderPlot({
      dateIndextb = input$dateindex
      smartfinaldf <- read.csv(customerdata()$datapath)
      xtssmartfinaldf <-
        ts(smartfinaldf[input$picktarget],
           frequency = 48,
           start = as.Date(smartfinaldf[[dateIndextb]][1]))
      
      print("decompose")
      print(xtssmartfinaldf)
      plot(decompose(xtssmartfinaldf))
    })
    
  })
  observeEvent(input$modelbutton, {
    loadsmartXmodel <- readRDS("smartX_model.rda")
    
    output$forecaststats <- renderTable({
      summary(loadsmartXmodel)
      
    })
    output$coefplottt <- renderPlot({
      coefplot(loadsmartXmodel$eq[[1]])
    })
    output$modelresiduals <- renderPlot({
      checkresiduals(loadsmartXmodel)
    })
    output$modelplot <- renderPlot({
      smarthhtest <- read.csv("test.csv")
      smarthhtestfilter <- smarthhtest[0:1439, ]
      inputdate <- input$dateindex
      
      smarthhtestfilterfinal <-
        subset(smarthhtestfilter, select = -c(X , time , energy))
      xregsmarttest = data.matrix(smarthhtestfilterfinal)
      forecastsmarthh <-
        forecast(loadsmartXmodel, h = 1440, xreg = xregsmarttest)
      
      autoplot(forecastsmarthh, main = "Consumption Overtime") %>% dyRangeSelector()
    })
    
    
  })
  
  observeEvent(input$nexttab, {
    updateTabItems(session, "tabs", "modeling")
  })
  
  
  
}



shinyApp(ui, server)