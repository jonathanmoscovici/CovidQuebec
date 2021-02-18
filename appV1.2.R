
    library(readr)
    library(knitr)
    library(tidyverse)
    #if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
    library(plotly)
    library(reshape2)
    #if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
    library(shinydashboard)
    
    #cfile<-"https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv"
    cfile<-"https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/individual_level/cases_2021.csv"
    
    
    covfile<-read.csv(cfile)
    
    
    library(shiny)
    
    #header<- dashboardHeader(title = h4(HTML("Covid-19 Cases<br/>by Canadian Province")))
    header<- dashboardHeader(title ="2021 COVID-19 Cases by Canadian Province", titleWidth = 400)
    
    
    ui <- dashboardPage(
        header,
        dashboardSidebar(disable=TRUE),
        dashboardBody(
            fluidRow(
                selectInput("prov1","Province: ", choices=c("Canada", "Alberta","BC", "Manitoba", "New Brunswick","Nova Scotia", "Ontario",  "PEI", "NL", "NWT", "Quebec", "Saskatchewan", "Yukon"),selected="Quebec"),
                #box(width = 2, actionButton("count", "Count")),
                #infoBoxOutput("ibox1"),
                #infoBoxOutput("ibox2"),
                #infoBoxOutput("ibox3"),
                #infoBoxOutput("ibox4"),
                valueBoxOutput("rbox1"),
                valueBoxOutput("rbox2"),
                valueBoxOutput("rbox3"),
                valueBoxOutput("rbox4")
            
            ),
            plotlyOutput("plot3"),
            plotlyOutput("plot2"),
            plotlyOutput("plot1"),
            plotlyOutput("plot4"),
            
            hr(),
            print("Note: Doubling Rate refers to the number of days required to double the current number of cases at the current rate. Larger is better."),
            HTML(paste('<br/>')),
            print("*Data obtained from the COVID-19 Canada Open Data Working Group. Epidemiological Data from the COVID-19 Outbreak in Canada. https://github.com/ishaberry/Covid19Canada"),
            HTML(paste('<br/>')),
            print("Created by Jonathan L. Moscovici. https://github.com/jonathanmoscovici/CovidQuebec")
            
        )
    )
    
    server <- function(input, output) {
        
        
        covqc<-reactive({
        
        if (input$prov1!="Canada") {
            
                covqc1<- covfile %>% filter(province==input$prov1)%>% mutate(date=date_report)
                covqc1$date_report<-as.Date(covqc1$date, "%d-%m-%Y")
                covqc1
        }
        
        else {
            
                covqc1<- covfile %>% filter(province %in% c("Alberta","BC", "Manitoba", "New Brunswick","Nova Scotia", "Ontario",  "PEI", "NL", "NWT", "Quebec", "Saskatchewan", "Yukon"))%>% mutate(date=date_report)
                covqc1$date_report<-as.Date(covqc1$date, "%d-%m-%Y")
                covqc1
                
            
        }
        
        
        
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
        
        
        
        
        output$plot1<- renderPlotly({
            
            
            
            
            cov4 <- reactive({
                cov4 <- cov3() %>% filter(variable=="dperc")
                cov4
            })
            
            g1<-ggplot(cov4(),aes(x=as.Date(date_report),y=value,text = paste0("Increase: ",round(value,digits=2),"%", "\n", date_report)))+
                geom_bar(stat="identity", fill="green")+
                theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
                labs(x="Date",y="Percentage Increase")+
                ggtitle(paste0("Percentage Increase Day-Over-Day of Cases in ", input$prov1))
            
            
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
                ggtitle(paste0("Daily New Cases in ", input$prov1))
            
            
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
                ggtitle(paste0("Total Cumulative Cases in ", input$prov1))
            
            
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
                ggtitle(paste0("Doubling Rate of Cases in ", input$prov1))
            
            
            
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
        
    
        output$rbox1 <- renderValueBox({
            valueBox(
                formatC(paste(cov6()$value[nrow(cov6())],"cases"), format="d", big.mark=',')
                ,paste('Total Cases:',paste(cov6()$value[nrow(cov6())],"cases"))
                ,icon = icon("stats",lib='glyphicon')
                ,color = "purple")  
        })
        
        output$rbox2 <- renderValueBox({
            valueBox(
                formatC(paste(cov5()$value[nrow(cov5())],"cases"), format="d", big.mark=',')
                ,paste('Daily New Cases:',paste(cov5()$value[nrow(cov5())],"cases"))
                ,icon = icon("stats",lib='glyphicon')
                ,color = "blue")  
        })
        
        output$rbox3 <- renderValueBox({
            valueBox(
                formatC(paste(round(cov4()$value[nrow(cov4())],digits=2),"%"), format="d", big.mark=',')
                ,paste('Daily New Case Percentage Increase:',round(cov4()$value[nrow(cov4())],digits=2),"%")
                ,icon = icon("stats",lib='glyphicon')
                ,color = "red")  
        })
        
        output$rbox4 <- renderValueBox({
            valueBox(
                formatC(paste(round(cov7()$value[nrow(cov7())],digits=2),"Days"), format="d", big.mark=',')
                ,paste('Doubling Rate',round(cov7()$value[nrow(cov7())],digits=2),"Days")
                ,icon = icon("stats",lib='glyphicon')
                ,color = "green")  
        })
        
        
        
        output$ibox1 <- renderInfoBox({
            infoBox(
                "Latest Total Cumulative Cases",
                paste(cov6()$value[nrow(cov6())],"cases")
                #icon = icon("credit-card")
            )
        })
        
        output$ibox2 <- renderInfoBox({
            infoBox(
                "Latest Daily New Cases",
                paste(cov5()$value[nrow(cov5())],"cases")
                #icon = icon("credit-card")
            )
        })
        
        #title3<-HTML("Covid-19 Cases<br/>by Canadian Province")
        
        output$ibox3 <- renderInfoBox({
            infoBox(
                HTML("Latest Daily Case<br/>Percentage Increase"),
                paste(round(cov4()$value[nrow(cov4())],digits=2),"%")
                
            )
        })
        
        output$ibox4 <- renderInfoBox({
            infoBox(
                "Latest Doubling Rate",
                paste(round(cov7()$value[nrow(cov7())],digits=2),"days")
                #icon = icon("credit-card")
            )
        })
        
        output$vbox <- renderValueBox({
            valueBox(
                "Title",
                input$count,
                icon = icon("credit-card")
            )
        })
    }
    
    shinyApp(ui, server)

