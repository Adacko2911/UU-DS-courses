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

fluidPage(navbarPage("Data Science Related Courses at UU", theme = shinytheme("paper"),
                     tabPanel("Topics overview",
                              sidebarLayout(
                                column(width = 4,
                                       wellPanel(style = "background: #e8f0ff",
                                                 h5("The dashboard is an interactive tool for exploring Utrecht University's data science courses."),
                                                 
                                       ),
                                       wellPanel(
                                         shinyjs::useShinyjs(),
                                         h4("Filters:"), 
                                         selectInput("level", "Education Level:",choices = 
                                                       c("Introduction Bachelor" = "1",
                                                         "Intermediate Bachelor" =  "2" ,
                                                         "Advance Bachelor" = "3",
                                                         "Master"="M",
                                                         "Post-academic"="Post-academic",
                                                         "All"="all"),selected ='all'),
                                         selectInput("faculty", "The Faculty:",
                                                     c("Faculty of Social Sciences" = "Faculty of Social Sciences",
                                                       "Faculty of Science" = "Faculty of Science",
                                                       "Faculty of Medicine"="Faculty of Medicine",
                                                       "Faculty of Veterinary"= "Faculty of Veterinary Medicine",
                                                       "Faculty of Geoscience" = "Faculty of Geoscience")
                                         ),
                                         selectInput("course", "Course type:",
                                                     list(`Regular` = c("Course","Practical","Research project"),
                                                          `Summer School` = c("summer","winter","online"))
                                         ),
                                         actionButton(inputId = "Reset_table", label = "Reset table"),
                                         tags$script("
                                    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                      Shiny.onInputChange(variableName, null);
                                    });
                                  ")
                                       )
                                       
                                ),
                                column(width = 8,
                                       
                                       h4("Main topics covered",align = "center"),
                                       d3Output("d3",height = "600px",width = "900px"),
                                       h5(textOutput("text")),
                                       DT::dataTableOutput('table')
                                       
                                )
                              )
                              
                     ),
                     tabPanel("Facet overview",
                              column(width = 12,
                                     wellPanel(
                                       style = "background: #e8f0ff",
                                       h5("Below the facet overview of the topics taught by different Faculties, on different levels of education and for different type of course."),
                                       h6("Manual:
                                       First, choose the desired facet condition and click 'Show facet view' button. Note that the size of the bubbles represents the number of courses where a particular topic is taught. To get a clearer output, the user may set the minimum number of courses to 2 or more to get the most popular topics within each facet.")
                                     ),
                                     wellPanel(
                                       selectInput("overview", "Overview:",choices = 
                                                     c("Faculty" = "Faculty",
                                                       "Level"="Level",
                                                       "Course type"="Course_type")),
                                       numericInput("min_courses","Minimum number of courses: ",min = 1,max=120,value = 1),
                                       actionButton("action1", "Show facet view")
                                       
                                     )),
                              column(width = 12,
                                     uiOutput("plots")
                              )
                              
                     ),
                     tabPanel("Faculty input",
                              sidebarLayout(
                                column(width = 4,
                                       wellPanel(
                                         style = "background: #e8f0ff",
                                         h5("On the left, you can see the impact that each faculty has on teaching a certain data science-related topic."),
                                         h6("Below user may chose topics of interests for more clear ouput.")
                                       ),
                                       wellPanel(
                                         selectizeInput(inputId = "topic", label = "Topics to compare:",choices = topic_choices
                                                        ,selected ='all', multiple = T,
                                                        options = list(
                                                          placeholder = 'select topic name')
                                         )
                                       )),
                                column(width = 8,
                                       h4("Faculty inpact on topics taught",align = "center"),
                                       plotlyOutput("plots2")
                                )
                              ))
))