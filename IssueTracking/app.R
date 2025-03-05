### Issue Tracking App
# By: FE
# Last changed: 11/21/2024

# Set Up -----
# Load necessary libraries
# SET UP
##0: load libraries --------------
#shiny
library(shiny)
#pool for database connections
library(pool)
#odbc for database connections
library(odbc)
#tidyverse for data manipulations
library(tidyverse)
#shinythemes for colors
library(shinythemes)
#lubridate to work with dates
library(lubridate)
#shinyjs() to use easy java script functions
library(shinyjs)
#DT for datatables
library(DT)
#reactable themes
library(reactablefmtr)
#reactable for reactable tables
library(reactable)
#excel download
library(xlsx)
library(DBI)
#Rpostgres for dbcon
library(RPostgres)
# package versioning
library(renv)
#Not in logical
`%!in%` <- Negate(`%in%`)

##1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connections are unified
#gets environmental variables saved in local or pwdrstudio environment
conn <- dbPool(RPostgres::Postgres(),
                 dbname = 'mars_data', 
                 host = 'PWDMARSDBS1', 
                 port = 5434, 
                 user = Sys.getenv("shiny_uid"),
                 password = Sys.getenv("shiny_pwd"))

# fiscal quarter lookup
q_list  <- dbGetQuery(conn,"select * from admin.tbl_fiscal_quarter_lookup") %>%
  select(fiscal_quarter) %>%
  arrange(tolower(fiscal_quarter), fiscal_quarter) %>%
  pull
#system ids
system_id <- odbc::dbGetQuery(conn, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) %>% 
  dplyr::arrange(system_id) %>%  
  dplyr::pull()
# component ids
# component_and_asset_query <- paste0("SELECT * FROM external.mat_assets")
# component_and_asset <- dbGetQuery(conn, component_and_asset_query)
# 
# components_id <- odbc::dbGetQuery(conn, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) 

# load the issue types
issue_types <- odbc::dbGetQuery(conn, paste0("SELECT * FROM fieldwork.issue_type_lookup"))
issue_choices <- issue_types %>% 
  select(issue_type) %>%
  arrange(tolower(issue_type), issue_type) %>%
  pull

#disconnect from db on stop 
onStop(function(){
  poolClose(conn)
})

# UI -----

# Define UI
ui <- tagList(useShinyjs(), navbarPage("Issue Tracking App", id = "TabPanelID", theme = shinytheme("cyborg"),
                                       tabPanel("Issues Table", value = "status", 
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectizeInput ("system_id", "System ID", choices = NULL),
                                                    selectInput("f_q", "Fiscal Quarter", choices = c("All", q_list), selected = "All"),
                                                    selectInput("issues", "Issues", choices = c("All", issue_choices)),
                                                    selectInput("status", "Status", choices = c("All", "Open", "In Progress", "Closed")),
                                                    downloadButton("download_table", "Download")
                                                  ),
                                                  mainPanel(

                                                  )
                                                )
                                       ),
                                       tabPanel("Add/Edit Issues", value = "add_edit", 
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectizeInput ("system_id_edit", "System ID", choices = NULL),
                                                    selectInput("component_id", "Component ID", choices = "", selected = NULL),
                                                    selectInput("issues_edit", "Issues", choices = c("", issue_choices), selected = ""),
                                                    dateInput("date_observed", "Date Observed", value = as.Date(NA)),
                                                    textInput("image_link", "Link to Image"),
                                                    textInput("reporter_initials", "Reporter Initials"),
                                                    selectInput("priority", "Priority Level", choices = c("","Low", "Medium", "High"), selected = ""),
                                                    textAreaInput("inspector_note", "Inspector Note"),
                                                    actionButton("submit_btn", "Submit Issue")
                                                  ),
                                                  mainPanel(
                                                    
                                                  )
                                                )
                                                )
)
)

# Server -----
server <- function(input, output, session) {
  
  #initialzie reactive values ------
  rv <- reactiveValues()
  
  # server-side selectizeinput for system ids across the tabs
  updateSelectizeInput(session, 'system_id', choices = c("All", system_id), server = TRUE)
  updateSelectizeInput(session, 'system_id_edit', choices = system_id, selected = character(0), server = TRUE)
  
  
  
  #show component IDs based on SMPs/sites ------
  #component IDs
  #adjust query to accurately target NULL values once back on main server
  rv$component_and_asset_query <- reactive(paste0("SELECT component_id, asset_type FROM external.mat_assets_ict_limited WHERE system_id = '", input$system_id_edit, "' AND component_id != 'NULL'"))
  rv$component_and_asset <- reactive(odbc::dbGetQuery(conn, rv$component_and_asset_query()))
  
  rv$asset_comp <- reactive(rv$component_and_asset() %>% 
                              mutate("asset_comp_code" = paste(component_id, asset_type, sep = " | ")))
  
  rv$asset_combo <- reactive(rv$asset_comp()$asset_comp_code)
  
  observe(updateSelectInput(session, "component_id", choices = c("", rv$asset_combo())))
  
  # all issues
  rv$issues <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.viw_issues_full"))
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

# end ; close the DB connection 
