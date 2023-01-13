library(DT)
library(here)
library("readxl")
library(dplyr)
library(r2d3)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(packcircles)
library(ggplot2)
library(RColorBrewer)
library(htmlwidgets)
library("digest")
library("bit")
library(shinySignals)
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(devtools)
library(ggthemes)
library(shinyWidgets)
library(rsconnect)
library(RColorBrewer)

DS<- read_excel("Data/DS courses UU.xlsx")
short<- read_excel("Data/short.xlsx")
levels_names<-c("1","2","3","M","Post-academic")
levels_names_discr<-c("Introduction Bachelor","Intermediate Bachelor","Advance Bachelor","Master","Post-academic")
course_names<-c("Course","Practical","Research project","summer","winter","online")
course_names_discr<-c("Course","Practical","Research project","Summer courses","Winter courses","Online")
faculty_names<-c(unique(DS$Faculty)[complete.cases(unique(DS$Faculty))])
topic_choices<-c("Strong theoretical base of data science practice and ethics",	"Causal inference",	"Data Collection methods",	"Database Management",	"nonSQL databases",	"Relational databases",	"Data Imputation"	,"Supervised Machine Learning","Data wrangling",	"Deep learning",	"Network science",	"Unsupervised Machine Learning",	"Programming", 	"SQL",	"Python", 	"Data clean-up",	"QGIS",	"PCRaster",	"R"	,"Rmardown",	"Epidemiology",	"Bayesian statistics",	"Statistical models",	"Stochastic Modeling",	"Explorative Data Analysis",	"Specific Data Analysis",	"Simulation models",	"PAC",	"Data mining",	"Big data",	"Visualizations",	"Graph Analysis/Graph construction",	"Data manipulation",	"SPSS",	"Complex systems",	"MAL",	"HLM",	"Text mining",	"Study design",	"Stata",	"JASP",	"Haskell",	"Mplus","Visual Studio",
                 "All"="all")


function(input, output, session) { 
  
  level=reactive(input$level)
  course=reactive(input$course)
  faculty=reactive(input$faculty)
  
  observeEvent(input$Reset_table, {
    session$sendCustomMessage(type = "resetValue", message = "click_event")
  })
  
  observe({
    x <- input$topic
    
    # Can use character(0) to remove all choices
    if (is.null(x)){
      updateSelectInput(session, "topic",
                        label = paste("No topic chosen"),
                        choices = topic_choices,
                        selected = "")
    }else if(length(x)>1 && any(x=="all")){
      idx<-which(x=="all")
      updateSelectInput(session, "topic",
                        label = paste("Topics to compare:"),
                        choices = topic_choices,
                        selected =x[-idx])
    }else if(length(x)==1 && x=="all"){
      updateSelectInput(session, "topic",
                        label = paste("Topics to compare:"),
                        choices = topic_choices,
                        selected =x)
    }
    
  })
  output$d3 <- renderD3({
    if(level()=="all"){
      data <- DS %>% select(c(1,2,4,6,18:61)) %>% filter(Faculty==faculty(),Course_type==course()) %>%
        pivot_longer(where(is.numeric)) %>% group_by(name) %>% 
        summarise(value=sum(value,na.rm =T)) %>% filter(value>0)
      colnames(data)<-c("id","value")
      data<-left_join(data,short, by="id")
      r2d3(data=data, d3_version = 4, script ="bubble.js")
    }else{
      data <- DS %>% select(c(1,2,4,6,18:61)) %>% filter(Faculty==faculty(),Level==level())
      if(course() %in% data$Course_type){
        data<-data %>% filter(Course_type==course()) %>% pivot_longer(where(is.numeric)) %>%
          group_by(name) %>% summarise(value=sum(value,na.rm =T)) %>% filter(value>0)
        colnames(data)<-c("id","value")
        data<-left_join(data,short, by="id")
        r2d3(data=data, d3_version = 4, script ="bubble.js")
      }else{
        data<-data.frame(id="No data",value=20,short="No data avaiable")
        r2d3(data=data, d3_version = 4, script ="bubble.js")
      }
    }
  })
  
  output$text<-renderText({
    if(is.null(input$click_event)){
      paste0("The database of courses")
    }else{
      paste0("The database of courses that cover the ", input$click_event," topics")
    }
  })
  
  
  
  input_plots <- reactiveValues()
  observeEvent(input$action1,{
    input_plots$over<-input$overview
    input_plots$min_course<-input$min_courses
    if(input_plots$over=='Faculty'){
      names_for_bubble<-get("faculty_names")
      plot_titles<-get("faculty_names")
    }else if(input_plots$over=='Level'){
      names_for_bubble<-get("levels_names")
      plot_titles<-get("levels_names_discr")
    }else if(input_plots$over=='Course_type'){
      names_for_bubble<-get("course_names")
      plot_titles<-get("course_names_discr")}
    for (i in 1:length(names_for_bubble)) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        data <- DS %>% select(c(1,2,4,6,18:61))
        names<-names_for_bubble[i]
        output[[plotname]] <- renderD3({
          data<- data  %>% filter(!!sym(input_plots$over)==names) %>% pivot_longer(where(is.numeric)) %>% group_by(name) %>%
            summarise(value=sum(value,na.rm =T)) 
          if(all(data$value<input_plots$min_course)){
            data<-data.frame(id="No data",value=20,short="No data avaiable")
          }else{
            data<-data %>% filter(value>(input_plots$min_course-1))
            colnames(data)<-c("id","value")
            data<-left_join(data,short, by="id")
          }
          r2d3(data=data, d3_version = 4, script ="bubble_2.js")})
      })
    }
    output$plots <- renderUI({
      
      
      plot_output_list <- lapply(1:length(names_for_bubble), function(i) {
        plotname <- paste("plot", i, sep="")
        column(width = 6,
               h4(plot_titles[i],align = "center"),
               tags$div(style = "margin-top: 0px; margin-bottom: 0px;", d3Output(plotname))
        )
      } )
      
      do.call(tagList, plot_output_list)
    })})
  
  output$table <- DT::renderDataTable({
    
    if(is.null(input$click_event)){
      if(level()=='all'){
        DS %>% filter(Faculty==faculty(),DS$Course_type==course()) %>% select(c(1,3,5,7,8,9,12))
      }else{
        DS %>% filter(Faculty==faculty(),Level==level(),DS$Course_type==course())%>%select(c(1,3,5,7,8,9,12))
      }}else{
        if(level()=='all'){
          DS %>% filter(Faculty==faculty(),DS$Course_type==course(),(!!sym(input$click_event))==1)%>% select(c(1,3,5,7,8,9,12))
        }else{
          DS %>% filter(Faculty==faculty(),Level==level(),DS$Course_type==course(),(!!sym(input$clik_event))==1)%>% select(c(1,3,5,7,8,9,12))
        }
      }
  },server = F,options = list(searching = TRUE))
  
  
  output$plots2 <- renderPlotly({
    
    short2<-DS %>% select(c(1,2,4,6,18:61)) %>%pivot_longer(where(is.numeric)) %>% group_by(name,Faculty) %>% summarise(total=sum(value,na.rm =T)) %>% na.omit()
    
    plot1_perc<-short2 %>% mutate(total=round((total/sum(total))*100,2))
    if(any(input$topic!="all")){
      plot1_perc<-plot1_perc %>% filter(name %in% input$topic)
    }
    
    ggplotly(
      plot1_perc %>% ggplot(aes(x=total,y=name,fill=Faculty))+geom_col() + scale_fill_brewer(palette="Set2") +ylab("")+xlab("percentage (%)")+theme_minimal()
    ) %>% layout(height = 800, width = 1000) })
  
  
  
}