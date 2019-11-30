# Packages

library(shiny)
library(shinydashboard)
library(rAmCharts)
library(dplyr)
library(purrr)
library(rlang)
library(stringr)
library(tidyverse)
library(DT)
library(r2d3)
library(htmltools)
library(odbc)
library(DBI)
library(config)
library(scales)
library(shinythemes)
library(lubridate)
library(ggthemes)
library(scales)
library(ggThemeAssist)
library(viridis)
library(RColorBrewer)
library(ggsci)
library(unikn)
library(jcolors)


#traffic = read.csv('traffic1.csv')
#traffic_graph1  = traffic %>% 
    #filter(!is.na(month_)) %>%
    #count(year,month_,sort = TRUE)
    
# Renaming weekdays 
#traf = traffic %>% mutate(new_day = ifelse(day %in% 'MONDAY', 'Mon.', ifelse(day %in% 'TUESDAY', 'Tues.',ifelse(day %in% 'WEDNESDAY', 'Wed.', ifelse(day %in% 'THURSDAY', 'Thurs.', ifelse(day %in% 'FRIDAY', 'Friday', ifelse(day %in% 'SATURDAY', 'Sat.', ifelse(day %in% 'SUNDAY', 'Sun.', 'Fri.'))))))))


# Grouping Ages 
#age1 = 16:20
#age2 = 21:30
#age3 = 31:40
#age4 = 41:50
#age5 = 51:60
#age6 = 61:70
#age7 = 71:80
#age8 = 81:95

#traffic_age = traffic %>% mutate(age_group = ifelse(age %in% age1, '16-20', ifelse(age %in% age2, '21-30', ifelse(age %in% age3, '31-40', ifelse(age %in% age4, '41-50', ifelse(age %in% age5, '51-60', ifelse(age %in% age6, '61-70', ifelse(age %in% age7, '71-80', '81-95'))))))))
#traffic = traffic_age

# The Top Ten Descriptions 

de = c('SPEED IN ZONE', 'UNINSPECTED MOTOR VEHICLE', 'DISOBEYED TRAFFIC DEVICE', 'AGGRAVATED UNLIC OPER 3RD MISD', ' UNREGISTERED MOTOR VEHICLE', 'OPERATING REGISTRATION SUSP/REVOKED', 'FLD TO STOP AT STOP SIGN ', 'UNLICENSED OPERATOR', 'NO SEAT BELT ADULT', 'OPERATING MV MOBILE PHONE ', 'SPEED OVER 55 ZONE')
traffic2 = traffic %>% filter(description %in% de)
traf2 = traffic2  %>% 
    filter(!is.na(month_) & gender != 'C') %>%
    count(year,month_,agency, description, sort = TRUE) %>% 
    mutate( rate = round(traf2$n/1e2)) %>% 
    mutate( rate  = as.integer(rate))


header <- dashboardHeader(title = "Traffic Violation Dashboard")

sidebar <- dashboardSidebar(
    sidebarMenu(
        tags$h3('Drucila LeFevre'), 
        menuItem('Dashboard', tabName= 'dashboard', icon = icon('home')))) 

body <- dashboardBody(
    tabItems(
        tabItem(tabName= 'dashboard',
                fluidRow(
                    tags$h2('Visualization of Traffic Violations given during the years of 2014 - 2017.')),
                fluidRow(
                    selectizeInput(inputId= 'year', 
                                   label = 'Choose the Year:', choices = unique(traffic[, 'year'])),
                    selectizeInput(inputId= 'agency',
                                   label = 'Agency' , choices = unique(traffic[, 'agency']))), 
                fluidRow(
                    box(width = 6, plotOutput("plot1"), title = " Ticket Violations Per Year"),
                    box(width = 6, plotOutput("plot2"), title = " Ticket Violations Per Week")),
                fluidRow(
                    box(width = 6, plotOutput("plot3"), title = "Age and Gender"),
                    box(width = 6, plotOutput("plot4"), title = "Traffic Violation Description")))))

ui <- dashboardPage(
    header,
    sidebar,
    body,
    skin = 'blue')


# Define server logic required to draw a histogram
server <- (function(input, output) {
    
    observe({
        year = unique(traffic %>% 
                          filter(traffic$year == input$year))})
    
    observe({
        agency = unique(traffic %>% 
                          filter(traffic$agency == input$agency))})
    
    
 
    output$plot1 = renderPlot(
        traffic  %>% 
            filter(!is.na(month_)) %>%
            count(year,month_,agency,sort = TRUE) %>% 
            filter(year == input$year) %>% 
            filter(agency == input$agency) %>% 
            ggplot(aes(x = reorder(month_, n), y = n)) +
            geom_bar(stat = 'identity', width = 0.5, 
                     fill = 'steelblue4') +
            labs( x = 'Month' , y = 'Total') )
    
    output$plot2 = renderPlot(
        traffic  %>% 
            filter(!is.na(month_)) %>%
            count(year,new_day,agency,sort = TRUE) %>% 
            filter(year == input$year) %>% 
            filter(agency == input$agency) %>% 
            ggplot(aes(x = reorder(new_day, n), y = n)) +
            geom_bar(stat = 'identity', width = 0.5, 
                     fill = 'lightsteelblue2') +
            labs( x = 'Day' , y = 'Total') )
    
    output$plot3 = renderPlot(
        traffic  %>% 
            filter(!is.na(month_) & gender != 'C') %>%
            count(year,month_,age_group,gender,agency, sort = TRUE) %>% 
            filter(year == input$year) %>% 
            filter(agency == input$agency) %>% 
            ggplot(aes(x = age_group, y = n, fill = gender)) +
            geom_col(position= 'dodge') + labs( x = 'Age Group' , y = 'Total') + 
            scale_fill_brewer(palette='Blues'))
    
    output$plot4 = renderPlot(
        traf2  %>% 
            filter(year == input$year) %>% 
            filter(agency == input$agency) %>% 
            ggplot(aes(x = reorder(description,n), y = n)) +
            geom_bar(stat = 'identity', width = 0.5, 
                     fill = 'lightsteelblue3') +
            labs( x = 'Description' , y = 'Total') + coord_flip())
    
})

# Run the application 
shinyApp(ui = ui, server = server)

