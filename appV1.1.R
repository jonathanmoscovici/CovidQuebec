#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(knitr)
library(tidyverse)
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
library(plotly)
library(reshape2)
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
library(shinydashboard)

cfile<-"https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv"


covfile<-read.csv(cfile)

#cfile<-"https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv"
#
#covfile<-read.csv(cfile)
#
#covqc<- covfile %>% filter(province=="Quebec")%>% mutate(date=date_report)
#
#covqc$date_report<-as.Date(covqc$date, "%d-%m-%Y")  
#
#covdcount<- covqc %>% count(date_report) %>% mutate(cum=cumsum(n), dperc=((cum/lag(cum))-1)*100, dblt=log(2)/log((dperc/100)+1), cases=cum)
#
#covdcount<-covdcount %>% mutate(previous=cum-n)
#
#cov2<-covdcount %>% select(date_report,previous,n) %>% melt(id.var="date_report")
#
#cov3 <- covdcount %>% melt(id.var="date_report")


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Canadian COVID-19 Cases by Province"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
#            sliderInput("house1",
#                        "House total downpayment (k$)",
#                        min = 1,
#                        max = 100,
#                        value = 100),
#            sliderInput("house2",
#                        "Amount contributed to downpayment (k$)",
#                        min = 1,
#                        max = 50,
#                        value = 20),
#            numericInput("house3", "Personal Amount Contributed to other house expenses (mortgage, insurance, repairs, etc)", value="1000"),
#           numericInput("house4", "Total Amount (both) Spent On other house expenses (mortgage, insurance, repairs, etc)",value="4500")
            
            selectInput("prov1","Province: ", choices=c("Alberta","BC", "Manitoba", "New Brunswick","Nova Scotia", "Ontario",  "PEI", "NL", "NWT", "Quebec", "Saskatchewan", "Yukon"),selected="Quebec"),
               
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            #plotOutput("distPlot")
            #textOutput("comptext")
            #plotOutput("plot1", hover="plot_hover"),
            verbatimTextOutput("info2"),
            plotlyOutput("plot3"),
            plotlyOutput("plot2"),
            plotlyOutput("plot1"),
            plotlyOutput("plot4")
            
        )
    ),
    hr(),
    print("Note: Doubling Rate refers to the number of days required to double the current number of cases at the current rate. Larger is better."),
    HTML(paste('<br/>')),
    print("*Data obtained from the COVID-19 Canada Open Data Working Group. Epidemiological Data from the COVID-19 Outbreak in Canada. https://github.com/ishaberry/Covid19Canada")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    

    covqc <- reactive({
        covqc1<- covfile %>% filter(province==input$prov1)%>% mutate(date=date_report)
        covqc1$date_report<-as.Date(covqc1$date, "%d-%m-%Y")
        covqc1
    })
        

    
   
    covdcount <- reactive({
    covdcount<- covqc() %>% count(date_report) %>% mutate(cum=cumsum(n), dperc=((cum/lag(cum))-1)*100, dblt=log(2)/log((dperc/100)+1), cases=cum)
    
    covdcount<-covdcount %>% mutate(previous=cum-n)
    covdcount
    })
    
    cov2 <- reactive({
    cov2<-covdcount %>% select(date_report,previous,n) %>% melt(id.var="date_report")
    cov2
    })
    
    cov3 <- reactive({
    cov3 <- covdcount() %>% melt(id.var="date_report")
    cov3
    })
    
    cov4 <- reactive({
      cov4 <- cov3() %>% filter(variable=="dperc")
      cov4
    })
    
    cov5 <- reactive({
      cov5 <- cov3() %>% filter(variable=="n")
      cov5
    })
    
    cov6 <- reactive({
      cov6 <- cov3() %>% filter(variable=="cum")
      cov6
    })
    
    cov7 <- reactive({
      cov7 <- cov3() %>% filter(variable=="dblt")
      cov7
    })
    
    output$comptext <- renderText({
        x<-(input$house2+input$house3)/(input$house1+input$house4)
        
        #plot(covdcount$date_report,covdcount$cum)
        
        #paste("The percentage of the house owned is ", covfile[1,3],"%")
        
    })        
    
    
    output$plot1<- renderPlotly({
        

        
        #g1<-ggplot(cov2,aes(x=factor(date_report),y=value,text = paste0("Percentage: ",value,"%", "\n")))+
        #    geom_bar(stat="identity", aes(fill=forcats::fct_rev(variable)), position="stack")+
        #    theme(axis.text.x = element_text(angle = 75, vjust = 0.5))+
        #    labs(x="Date",y="Total Cases")+
        #    ggtitle("Cumulative COVID-19 Cases in Quebec")
        
        cov4 <- reactive({
        cov4 <- cov3() %>% filter(variable=="dperc")
        cov4
        })
        
        g1<-ggplot(cov4(),aes(x=as.Date(date_report),y=value,text = paste0("Increase: ",round(value,digits=2),"%", "\n", date_report)))+
            geom_bar(stat="identity", fill="green")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            labs(x="Date",y="Percentage Increase")+
            ggtitle(paste0("Percentage Increase Day-Over-Day of COVID-19 Cases in ", input$prov1))
        
        
        ggplotly(g1, tooltip=c("text"))%>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
        
      
        
    })
    
    
    output$plot2<- renderPlotly({
        
     
        cov5 <- reactive({
        cov5 <- cov3() %>% filter(variable=="n")
        cov5
        })
        
        g2<-ggplot(cov5(),aes(x=as.Date(date_report),y=value,text = paste0("Daily New cases: ",round(value,digits=2)," cases", "\n", date_report)))+
            geom_bar(stat="identity", fill="green")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            labs(x="Date",y="Daily New Cases")+
            ggtitle(paste0("Daily New COVID-19 Cases in ", input$prov1))
        
        
        ggplotly(g2, tooltip=c("text"))%>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
        
       
        
    })
    
    output$plot3<- renderPlotly({
        
        
        cov6 <- reactive({
        cov6 <- cov3() %>% filter(variable=="cum")
        cov6
        })
        
        g3<-ggplot(cov6(),aes(x=as.Date(date_report),y=value,text = paste0("Total Cumulative: ",round(value,digits=2)," cases", "\n", date_report)))+
            geom_bar(stat="identity", fill="green")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            labs(x="Date",y="Total Cases")+
            ggtitle(paste0("Total Cumulative COVID-19 Cases in ", input$prov1))
        
        
        ggplotly(g3, tooltip=c("text"))%>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
        
      
        
    })
    
    output$plot4<- renderPlotly({
        
        
        cov7 <- reactive({
        cov7 <- cov3() %>% filter(variable=="dblt")
        cov7
        })
        
        g4<-ggplot(cov7(),aes(x=as.Date(date_report),y=value,text = paste0("Doubling Rate: ",round(value,digits=2)," days", "\n", date_report)))+
            geom_bar(stat="identity", fill="green")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
            labs(x="Date",y="Doubling Rate (in Days)")+
            ggtitle(paste0("Doubling Rate of COVID-19 Cases in ", input$prov1))
        
        
        
        ggplotly(g4, tooltip=c("text")) %>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
        
       
        
    })
    
    output$info<- renderText({
      paste0("x=", input$plot_hover$x, "\ny=", input$plot_hover$y)
      
    })
    
    
    output$info2<- renderText({
        paste0(input$prov1,": ", "\n",
               "Latest total cumulative cases: ", cov6()$value[nrow(cov6())], " cases", "\n",
               
              "Latest new daily cases: ", cov5()$value[nrow(cov5())], " cases", "\n",
              "Latest daily case percentage increase: ", round(cov4()$value[nrow(cov4())],digits=2), " %", "\n",
               
              "Latest doubling rate: ", round(cov7()$value[nrow(cov7())],digits=2), " days", "\n"
               
               
               ) 
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
