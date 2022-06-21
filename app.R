options(shiny.maxRequestSize=200*1024^2)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

#SERVER
library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)
library(magrittr)
library(ggplot2)
library(dplyr)
library(plotly)
library(httr)
library(sodium)
library(shinyjs)
library(DT)

source('convertor/convertor.R')
source('global.R')

#setwd('C:/Users/ebongo/Documents/iHAP/DEVELOPMENT/Datim-Generator v3/genv3')
#library('rsconnect')
#deployApp()

#git push -u origin main
#Just a test!!


loginpage <- div(
      style = "margin-left : 30%;margin-top:10%",
      box(width = 5, "",status = "warning",
            tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
            textInput("userName", placeholder="Login", label = tagList(icon("user"), "Login")),
            passwordInput("passwd", placeholder="Mot de passe", label = tagList(icon("unlock-alt"), "Mot de passe")),
            br(),
            div(
                 style = "text-align: center;",
                 actionButton("login", "LOG IN", style = "color: white; background-color:#6D071A;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                 shinyjs::hidden(
                    div(id = "nomatch",
                           tags$p("Login ou mot de passe incorrect!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br()
             )
    )
)

dashboard <- tabItems(
  
  
  tabItem(tabName = "generate",
          # h3("Dashboard tab content"),

          fluidRow(
            
            column(width = 3,
                   
                   box(width = NULL, status = "warning",
                       
                       selectInput("period", label = h3("Période"), 
                                   choices = period, 
                                   selected = "2020Q1"),
                       
                       #selectInput("mechanism", label = h3("Mécanisme"), 
                                   #choices = list("JVafaPfopJf" = "JVafaPfopJf"), 
                                   #selected = "JVafaPfopJf"),
                       
                       
                       fileInput('data_file', 'Chargez les données', multiple = FALSE)
                       
                   )
            ),
            column(width = 9,
                   box(width = NULL, status = "warning",
                       h3("Résulat"),
                       DT::dataTableOutput("contents") %>% withSpinner(color="#6D071A"),
                       downloadButton('download_file', 'Téléchargez')
                   )
            )
          )
  ),
  
  tabItem(tabName = "conversion",
          
          fluidRow(
            column(width = 3,
              box(width = NULL, status = "warning",
                  
                  fileInput('file_to_convert', 'Chargez le fichier DATIM', multiple = FALSE)
                  
              )
            ),
            column(width = 9,
                   h3("Fichier converti"),
                   DT::dataTableOutput("converted_content") %>% withSpinner(color="#6D071A"),
                   downloadButton('download_converted', 'Téléchargez')
            )
          )
  ),
  
  tabItem(tabName = "config",
          
          fluidRow(
            
            column(width = 3,
                   
                   box(width = NULL, status = "warning",
                       
                       selectInput("periodicity", label = h3("Périodicité"), 
                                   choices = periodicity, 
                                   selected = "TRIMESTRE"),
                       
                       dateInput('start_date','Date début'),
                       
                       
                       dateInput('end_date','Date fin'),
                       
                       actionButton("import_dhis2", "VALIDER", style = "color: white; background-color:#6D071A;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                    font-size: 18px; font-weight: 600;")
                       
                   )
            ),
            column(width = 9,
                   box(width = NULL, status = "warning",
                       h3("Résultat"),
                       DT::dataTableOutput("dhis2_data") %>% withSpinner(color="#6D071A"),
                       downloadButton('download_dhis2_data', 'Téléchargez')
                   )
            )
          )
    )
)

sidebar <-  sidebarMenu(
  #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Générer le fichier", icon = icon("file"), tabName = "generate"),
  menuItem("Fichier converti", icon = icon("th"), tabName = "conversion")
  
  #menuItem("Configuration", icon = icon("cog"), tabName = "config")
)

ui <- dashboardPage(
  
  skin = "red",
  
  dashboardHeader(title="Datim Generator v3",uiOutput("logoutbtn")),
  
  dashboardSidebar(
    
    #sidebar
    uiOutput("sidebarpanel")
  ),
  
  dashboardBody(
    #dashboard
    shinyjs::useShinyjs(),
    uiOutput("body")
  )
)

#SERVER
server <- function(input, output){
  
  #----------DEALING WITH LOGIN-------
  login = T
  
  USER <- reactiveValues(login = login)
  
  observe({ 
    
    if (USER$login == FALSE) {
      
      if (!is.null(input$login)) {
        
        if (input$login > 0) {
          
          username <- isolate(input$userName)
          
          password <- isolate(input$passwd)
      
          
          if(nchar(username) > 0 && nchar(password) > 0) { 
            
            request <- BASE.URL
            
            res <- GET(request,authenticate(username,password))
            
            
            if(res$status == 200L) {
              
              USER$login <- TRUE
              
            } else {
              
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              
            }
          } else {
            
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  
  output$logoutbtn <- renderUI({
    
    req(USER$login)
    
    tags$li(a(icon("fa fa-sign-out"), "Déconnexion", 
              href="javascript:window.location.reload(true)"),
              class = "dropdown", 
              style = "font-color:#ffffff;background-color: #6D071A !important; border: 1px solid #6D071A;
              font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebar
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      dashboard
    }
    else {
      loginpage
    }
  })
  
  #-----------END LOGIN--------
  
  dataset <- reactive({
    
    file <- input$data_file
    
    if(is.null(file))
      return(NULL)
    
    period<- input$period
    
    attribute_combo <- mechanism#input$mechanism
    
    
    fac <- read.xlsx("OU.xlsx")
    
    facilities <- fac %>%
      select(orgUnit_dhis2, orgUnit_datim)
    
    de_cc <- read.xlsx("de_cc.xlsx")
    
    master <- read.csv(file$datapath)
    
    de_cc_cleaned <- de_cc %>%
      select(option_combo_uid_dhis2,option_combo_uid_datim,
             dataelementuid_dhis2,dataelementuid_datim)
    
    master_cleaned <- master %>%
      rename(dataelementuid_dhis2 = de_uid,orgUnit_dhis2 = orgunit_uuid,
             option_combo = category_combo_uid ,Value = value) %>%
      mutate(option_combo_uid_dhis2 = paste0(dataelementuid_dhis2,".",option_combo)) %>%
      select(dataelementuid_dhis2,orgUnit_dhis2,option_combo_uid_dhis2,Value) %>%
      arrange(dataelementuid_dhis2,orgUnit_dhis2,option_combo_uid_dhis2,Value)
    
   
    
    ou <- unique(master_cleaned$orgUnit_dhis2)
    
    
    for (i in 1:nrow(data.frame(ou))) {
      
      #dataelementuid_dhis2 <- merge_ou$dataelementuid_dhis2[i]
      
      ou_ID <- ou[i]#master_cleaned$orgUnit_dhis2[i]
      
      
      #Filter data for this facility
      filtered <- master_cleaned %>%
        filter(orgUnit_dhis2 == ou_ID)
      
      sum_connu_neg <- 0
      
      for (j in 1:nrow(filtered)[1]) {
        
        dataelementuid <- filtered$dataelementuid_dhis2[j]
        
        value <- filtered$Value[j]
        
        cc_uid = filtered$option_combo_uid_dhis2[j]
        
        if(dataelementuid == CONNU_NEGATIF){
          
          if(!is.na(value) && (value > 0)){
            
            #if(sum_connu_neg > 0){
            
            cc <- str_replace(cc_uid, CONNU_NEGATIF,DENOMINATEUR)
            
            newRow <- data.frame(dataelementuid_dhis2 = DENOMINATEUR,orgUnit_dhis2 = ou_ID,option_combo_uid_dhis2 = cc,Value = (-1)*value)
            
            master_cleaned <- rbind(master_cleaned,newRow)
            
            #}
          }
          
        }
        
        #PP_PREV
        if(dataelementuid == PP_PREV){
          #Deal with PP_PREV matching with different DATIM dataelement
          
          de_uid_pp = paste0(dataelementuid,"-","PP")
          
          cc <- str_replace(cc_uid, dataelementuid,de_uid_pp)
          
          
          r <- data.frame(dataelementuid_dhis2 = de_uid_pp,orgUnit_dhis2 = ou_ID,option_combo_uid_dhis2 = cc,Value = value)
          
          master_cleaned <- rbind(master_cleaned,r)

        }
      }
      
    }
    
    #We have the same orgunit codes, no need to join
    merge_ou <- left_join(master_cleaned,facilities, by = "orgUnit_dhis2")
    
    
    merge_cc <- left_join(merge_ou, de_cc_cleaned, by = "option_combo_uid_dhis2")
  
    
    merge_cc$Period <- period
    
    merge_cc$AttributeOptionComboID <- attribute_combo
    
    last_master <- merge_cc %>% 
      select(dataelementuid_datim,Period,orgUnit_datim,AttributeOptionComboID,option_combo_uid_datim,Value) %>%
      
      rename(dataelementID = dataelementuid_datim, categoryOptionComboID = option_combo_uid_datim,orgUnitID = orgUnit_datim) %>%
      
      arrange(dataelementID, Period,orgUnitID, categoryOptionComboID,AttributeOptionComboID,Value) %>% 
      drop_na() %>%
      group_by(dataelementID, Period, orgUnitID, categoryOptionComboID,AttributeOptionComboID) %>%
      summarize(Value = sum(Value)) %>%
      ungroup() 

    
    write.csv(last_master,"data_for_datim.csv",quote=F,row.names = F)
    
    print(nrow(last_master))
    
    sample <- last_master[1:100,]
    
    convert()
    
    return(sample)
  })
  
  dataset_converted <- reactive({
    
    file <- input$file_to_convert
    
    if(is.null(file))
      return(NULL)
    
    master <- read.csv(file$datapath)
    
    write.csv(master,"data_for_datim.csv",quote=F,row.names = F)
    
    convert()
    
    return(loadConverted())
  })
  
  
  chart_data <- reactive({
    
    if (!is.null(input$indicator)) {
      
        ind <- input$indicator
        
        if(ind != ''){
          
          data <- read.xlsx("converted.xlsx")
          
          data <- data %>%
            #filter(orgunit_parent == hz) %>% 
            group_by(orgunit_name,dataelementID,shortname,orgUnitID, dataelementID) %>%
            select(-c(orgUnitID,dataelementID)) %>%
            summarize(Value = sum(Value)) %>%
            ungroup()
          
          return(data)
          
        }else{
          return(NULL)
        }
    }
    
    
  })
  
  output$download_file <- downloadHandler(
    
    filename = function(){
      "data_for_datim.csv"
    },
    content = function(file){
      
      file.copy("data_for_datim.csv", file)
    }
  )
  
  #output$contents <- renderTable({
   # dataset()
  #},hover = TRUE,bordered = TRUE,striped = TRUE,iDisplayLength = 50)
  
  output$contents <- DT::renderDataTable(
    DT::datatable(
      dataset(), options = list(
        lengthMenu = table_pagination,#list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = table_page_length#15
      )
    )
  )
  

  #output$converted_content <- renderTable({
    #loadConverted()
    
  #},hover = TRUE,bordered = TRUE,striped = TRUE,iDisplayLength = 50)
  output$converted_content <- DT::renderDataTable(
    DT::datatable(
      dataset_converted(), options = list(
        lengthMenu = table_pagination,#list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = table_page_length#15
      )
    )
  )
  
  #output$converted_content <- DT::renderDataTable(
  #DT::datatable(
  #loadConverted(), options = list(
  #lengthMenu = table_pagination,#list(c(5, 15, -1), c('5', '15', 'All')),
  #pageLength = table_page_length#15
  #)
  #)
  #)
  
  output$download_converted <- downloadHandler(
    
    filename = function(){
      "converted.xlsx"
    },
    content = function(file){
      
      file.copy("converted.xlsx", file)
    }
  )
  
  
  output$infoBox <- renderInfoBox({
    
    ind <- input$indicator
    
    data <- chart_data() %>%
      summarize(value = sum(Value))

    infoBox(
        ind, data, icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow"
      )
    
  })
  
  output$facility_data <- DT::renderDataTable(
    
    DT::datatable(
      chart_data(), options = list(
        lengthMenu = table_pagination,#list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = table_page_length#15
      )
    )
  )
  
}

shinyApp(ui, server)
