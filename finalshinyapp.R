library(shiny)
library(dplyr)
library(ggplot2)
library(fpp3)
library(markdown)
library(shinyWidgets)
library(ggeasy)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("ggdark")
library(ggdark)
library(shinydashboard)
data("souvenirs")
ui <- fluidPage(theme = shinytheme("cyborg"),
  titlePanel(h2("E-Z Time Series: Time Series Analysis Made Simple ",align = "center")),
  sidebarLayout(
    sidebarPanel(
    pickerInput("PlotType", "Time Series Plots and Modeling Options", choices = c("About","Time Series","Decomposition","Seasonal","Autocorrelation",
    "Seasonal Naive", "Random Walk","Drift", "ETS", "Holt-Winters", "Auto ARIMA", "ARIMA Manual", "More"), selected =NULL, options = list(`actions-box` = TRUE), multiple = F)),
          
    mainPanel(
    conditionalPanel(condition="input.PlotType=='About'",includeMarkdown("intro.Rmd")),
    conditionalPanel(condition="input.PlotType=='Time Series'", plotOutput("ts_plot"),includeMarkdown("time.Rmd")),
    conditionalPanel(condition= "input.PlotType=='Decomposition'",plotOutput("decomp_plot"),includeMarkdown("decomp.Rmd")),
    conditionalPanel(condition="input.PlotType=='Seasonal'",plotOutput("season_plot"),includeMarkdown("season.Rmd")),
    conditionalPanel(condition="input.PlotType=='Autocorrelation'",plotOutput("auto_plot"),includeMarkdown("autocor.Rmd")),
    conditionalPanel(condition="input.PlotType=='Random Walk'", plotOutput("rw_plot"),includeMarkdown("random_walk.Rmd")),
    conditionalPanel(condition="input.PlotType=='Seasonal Naive'", plotOutput("snaive_plot"),includeMarkdown("season_naive.Rmd")),
    conditionalPanel(condition="input.PlotType=='Mean'", plotOutput("mean_plot"),includeMarkdown("mean.Rmd")),
    conditionalPanel(condition="input.PlotType=='Drift'", plotOutput("drift_plot"),includeMarkdown("drift.Rmd")),
    conditionalPanel(condition="input.PlotType=='ETS'", plotOutput("ets_plot"),includeMarkdown("exponential.Rmd")),
    conditionalPanel(condition="input.PlotType=='Holt-Winters'", plotOutput("hw_plot"),includeMarkdown("hw.Rmd")),
    conditionalPanel(condition="input.PlotType=='Auto ARIMA'", plotOutput("arima_plot"),includeMarkdown("ARIMA.Rmd")),
    conditionalPanel(condition="input.PlotType=='ARIMA Manual'",plotOutput("arima_man_plot"), 
    numericInput("p", "Number of Autoregressive terms (p):", 2,
    min = 0, max = 100),
    numericInput("d", "Difference in nonseasonal observations (d):", 1,
    min = 0, max = 100),
    numericInput("q", "Size of moving average window (q) :", 0,
     min = 0, max = 100),includeMarkdown("ARIMA_man.Rmd")),
    conditionalPanel(condition="input.PlotType=='More'",includeMarkdown("more.Rmd"))
      )
    )
  )
server <- shinyServer(
  
  function(input,output){
    
     #time series plot
    output$ts_plot <- renderPlot({
     souvenirs%>%
    autoplot(Sales, colour="blue")+labs(title="Souvenir Sales")+easy_center_title()+ggdark::dark_theme_gray()
      })
    #decomposition plot
    output$decomp_plot <- renderPlot({
      dcmp <- souvenirs %>%
        model(stl = STL(Sales))
      components(dcmp) %>%
        autoplot(Sales)+easy_center_title()+ggdark::dark_theme_gray()
      })
    #autocorrelation plot
    output$auto_plot <- renderPlot({
      souvenirs%>%
      ACF(Sales) %>%
      autoplot() + labs(title="Autocorrelation Plot: Sales")+easy_center_title()+ggdark::dark_theme_gray()
    })
    #seasonal plot
    output$season_plot <- renderPlot({
    souvenirs%>%
    gg_season(Sales,labels="right")+labs(title="Seasonal Plot: Souvenir Sales")+easy_center_title()+ggdark::dark_theme_gray()
    })
    
    #Random Walk Model
    output$rw_plot <- renderPlot({
      souvenirs%>%
        model(RW(Sales)) %>%
        fabletools::forecast(h = 14) -> FIT
      FIT %>%
      autoplot(souvenirs)+labs(title="Random Walk Model")+ggdark::dark_theme_gray()+easy_center_title()
    })
    #SNaive Model
    output$snaive_plot <- renderPlot({
      souvenirs%>%
        model(SNAIVE(Sales ~ lag("year"))) %>%
        fabletools::forecast(h = 14) -> FIT2
      FIT2 %>%
        autoplot(souvenirs)+labs(title="Seasonal Naive Model")+ggdark::dark_theme_gray()+easy_center_title()
})
    #Mean model
    output$mean_plot <- renderPlot({
    souvenirs%>%
    model(MEAN(Sales)) %>%
    fabletools::forecast(h = 14) -> FIT3
    FIT3 %>%
    autoplot(souvenirs)+lab(title="Mean Model")+ggdark::dark_theme_gray()+easy_center_title()
    })
    #Drift model
    output$drift_plot <- renderPlot({
      souvenirs%>%
        model(RW(Sales ~ drift())) %>%
        fabletools::forecast(h = 14) -> FIT4
      FIT4 %>%
        autoplot(souvenirs)+labs(title="Drift Model")+ggdark::dark_theme_gray()+easy_center_title()
        
    })
    #ETS model
    output$ets_plot <- renderPlot({
      souvenirs%>%
        model(ETS(Sales ~ error("A") + trend("N") + season("N"))) %>%
        fabletools::forecast(h = 14) -> FIT5
      FIT5 %>%
        autoplot(souvenirs)+labs(title="Exponential Smoothing Model")+ggdark::dark_theme_gray()+easy_center_title()
    
    })
    #Holt_Winters model
    output$hw_plot <- renderPlot({
      souvenirs%>%
        model(ETS(Sales ~ error("A") + trend("A") + season("A")))%>%
        fabletools::forecast(h = 14) -> FIT6
      FIT6 %>%
        autoplot(souvenirs)+labs(title="Holt-Winters Model")+ggdark::dark_theme_gray()+easy_center_title()
    })
    
    #ARIMA-auto plot
    output$arima_plot <- renderPlot({
      souvenirs%>%
        model(ARIMA(Sales))%>%
        fabletools::forecast(h = 14) -> FITZ
      FITZ %>%
        autoplot(souvenirs)+labs(title="ARIMA Model")+ggdark::dark_theme_gray()+easy_center_title()
    })
    
  #Manual ARIMA
    output$arima_man_plot <- renderPlot({
      souvenirs%>%
      model(arimamanual = ARIMA(Sales ~ pdq(input$p,input$d,input$q)))%>%
      fabletools::forecast(h = 14) -> FIT8
      FIT8 %>%
        autoplot(souvenirs)+labs(title="ARIMA Model (Custom Parameters)")+ggdark::dark_theme_gray()+easy_center_title()
    })
  })


shinyApp(ui, server)