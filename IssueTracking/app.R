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
# package versioning
library(renv)
#Not in logical
`%!in%` <- Negate(`%in%`)

##1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
poolConn <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

# fiscal quarter lookup
q_list  <- dbGetQuery(poolConn,"select * from admin.tbl_fiscal_quarter_lookup") %>%
  select(fiscal_quarter) %>%
  pull
#system ids
system_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) %>% 
  dplyr::arrange(system_id) %>%  
  dplyr::pull()
# component ids
component_and_asset_query <- paste0("SELECT * FROM external.mat_assets_ict_limited WHERE component_id != 'NULL'")
component_and_asset <- dbGetQuery(poolConn, component_and_asset_query)

#disconnect from db on stop 
onStop(function(){
  poolClose(poolConn)
})

# UI -----

# Define UI
ui <- tagList(useShinyjs(), navbarPage("Issue Tracking App", id = "TabPanelID", theme = shinytheme("cyborg"),
                                       tabPanel("Issues Table", value = "status", 
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("system_id", "System ID", choices = c("All", system_id)),
                                                    selectInput("f_q", "Fiscal Quarter", choices = q_list, selected = "FY24Q2"),
                                                    selectInput("problem", "Problem", choices = c("Low", "Medium", "High")),
                                                    dateInput("date_observed", "Date Observed"),
                                                    selectInput("status", "Status", choices = c("Open", "In Progress", "Closed")),
                                                    downloadButton("download_table", "Download")
                                                  ),
                                                  mainPanel(

                                                  )
                                                )
                                       ),
                                       tabPanel("Add/Edit Issues", value = "add_edit", 
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("system_id", "System ID", choices = c("All", system_id)),
                                                    selectInput("component_id", "Component ID", choices = c("Low", "Medium", "High")),
                                                    selectInput("problem", "Problem", choices = c("Low", "Medium", "High")),
                                                    dateInput("date_observed", "Date Observed"),
                                                    textInput("image_link", "Link to Image"),
                                                    textInput("reporter_initials", "Reporter Initials"),
                                                    selectInput("priority", "Priority Level", choices = c("Low", "Medium", "High")),
                                                    selectInput("status", "Status", choices = c("Open", "In Progress", "Closed")),
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
  
}

# Run the application
shinyApp(ui = ui, server = server)
