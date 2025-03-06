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

cw_conn <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "PWDCWSQLP",
                 Database = "PWD_Cityworks",
                 uid = Sys.getenv("cw_uid"),
                 pwd= Sys.getenv("cw_pwd"))

# fiscal quarter lookup
q_list  <- dbGetQuery(conn,"select * from admin.tbl_fiscal_quarter_lookup") %>%
  select(fiscal_quarter) %>%
  arrange(tolower(fiscal_quarter), fiscal_quarter) %>%
  pull

#system ids
system_id <- odbc::dbGetQuery(conn, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) %>% 
  dplyr::arrange(system_id) %>%  
  dplyr::pull()

# load the issue types
issue_types <- odbc::dbGetQuery(conn, paste0("SELECT * FROM fieldwork.issue_type_lookup"))
issue_choices <- issue_types %>% 
  select(category) %>%
  distinct() %>%
  arrange(tolower(category), category) %>%
  pull

# status
status_choices <- odbc::dbGetQuery(conn, paste0("SELECT * FROM fieldwork.issue_status_lookup")) %>% 
  select(status) %>%
  pull

#disconnect from db on stop 
onStop(function(){
  poolClose(conn)
})

# #replace special characters with friendlier characters
special_char_replace <- function(note){
  
  note_fix <- note %>%
    str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
  
  return(note_fix)
  
}


# UI -----

# Define UI
ui <- tagList(useShinyjs(), navbarPage("Issue Tracking App", id = "TabPanelID", theme = shinytheme("cyborg"),
                                       tabPanel("Issues Table", value = "status", 
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectizeInput ("system_id", "System ID", choices = NULL),
                                                    selectInput("f_q", "Fiscal Quarter", choices = c("All", q_list), selected = "All"),
                                                    selectInput("issues", "Issue Category", choices = c("All", issue_choices)),
                                                    selectInput("status", "Status", choices = c("All", status_choices)),
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
                                                    selectInput("issues_edit", "Issue Category", choices = c("", issue_choices), selected = ''),
                                                    conditionalPanel(condition = "input.issues_edit !== ''",
                                                                     selectInput("issues_sub", "Issue", choices = "", selected = NULL)),
                                                    dateInput("date_observed", "Date Observed", value = as.Date(NA)),
                                                    textInput("image_link", "Link to Image"),
                                                    textInput("reporter_initials", "Reporter Initials"),
                                                    selectInput("priority", "Priority Level", choices = c("","Low", "Medium", "High"), selected = ""),
                                                    numericInput( 
                                                      "numeric", 
                                                      "Cityworks Workorder ID", 
                                                      value = NULL,
                                                      step = 1,
                                                      min = 1, 
                                                      max = 1000000 
                                                    ),
                                                    textAreaInput("inspector_note", "Inspector Note", height = 150),
                                                    disabled(actionButton("submit_btn", "Save/Edit Issue")),
                                                    actionButton("clear", "Clear All Fields")
                                                    
                                                  ),
                                                  mainPanel(
                                                    
                                                    conditionalPanel(condition = "input.system_id_edit",
                                                                     h4(textOutput("current_header")),
                                                                     reactableOutput("open_issues_table"),
                                                                     h4(textOutput("past_header")), 
                                                                     reactableOutput("closed_issues_table"))
                                                    
                                                  )
                                                )
                                                )
)
)

# Server -----
server <- function(input, output, session) {
  
  #initialzie reactive values ------
  rv <- reactiveValues()
  
  # all issues
  rv$issues <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.viw_issues_full"))
  
  # issue lookup
  rv$issues_lookup <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.issue_wo_lookup"))
  
  # cityworks status
  rv$cw_status <- reactive(dbGetQuery(cw_conn, paste("SELECT WORKORDERID, STATUS FROM Azteca.WORKORDER where WORKORDERID in ('", toString(rv$issues_lookup()$workorder_id),"')", sep = "")) %>%
                             select(workorder_id = WORKORDERID, status = STATUS))
  
  # server-side selectizeinput for system ids across the tabs
  updateSelectizeInput(session, 'system_id', choices = c("All", system_id), server = TRUE)
  updateSelectizeInput(session, 'system_id_edit', choices = c('', system_id), selected = '', server = TRUE)
  
  #process text field to prevent sql injection
  rv$inspector_note <- reactive(gsub('\'', '\'\'',  input$inspector_note))
  rv$input_note  <- reactive(special_char_replace(rv$inspector_note()))

  
  #show component IDs and Issues based on Systems + Issue Category ------
  #component IDs
  
  # toggle component id-activate if a system is selected
  observe(toggleState("component_id", condition = input$system_id_edit != '' & length(rv$asset_combo()) > 0))
  #adjust query to accurately target NULL values once back on main server
  rv$component_and_asset_query <- reactive(paste0("SELECT component_id, asset_type FROM external.mat_assets WHERE system_id = '", input$system_id_edit, "' AND component_id IS NOT NULL"))
  rv$component_and_asset <- reactive(odbc::dbGetQuery(conn, rv$component_and_asset_query()))
  
  rv$asset_comp <- reactive(rv$component_and_asset() %>% 
                              mutate("asset_comp_code" = ifelse(is.na(component_id), paste("No Component ID", asset_type, sep = " | "), paste(component_id, asset_type, sep = " | "))))
  
  rv$asset_combo <- reactive(rv$asset_comp() %>%
                               select(asset_comp_code) %>%
                               arrange(tolower(asset_comp_code), asset_comp_code) %>%
                               pull)
  
  observe(updateSelectInput(session, "component_id", choices = c("", rv$asset_combo())))
  
  
  # update sub issue
  rv$sub_issue <- reactive(issue_types %>%
                             filter(category == input$issues_edit) %>%
                             select(issue) %>%
                             pull)
  observe(updateSelectInput(session, "issues_sub", choices = c("", rv$sub_issue())))
  
  # Fiscal Quarter Processing -----
  #get quarters as dates
  rv$start_quarter <- reactive(case_when(str_sub(input$f_q, 5, 7) == "Q3" ~ "1/1", 
                                                 str_sub(input$f_q, 5, 7) == "Q4" ~ "4/1", 
                                                 str_sub(input$f_q, 5, 7) == "Q1" ~ "7/1", 
                                                 str_sub(input$f_q, 5, 7) == "Q2" ~ "10/1"))
  
  rv$end_quarter <- reactive(case_when(str_sub(input$f_q, 5, 7) == "Q3" ~ "3/31", 
                                               str_sub(input$f_q, 5, 7) == "Q4" ~ "6/30", 
                                               str_sub(input$f_q, 5, 7) == "Q1" ~ "9/30", 
                                               str_sub(input$f_q, 5, 7) == "Q2" ~ "12/31"))
  
  # parse the year component from this format "FY24Q2"
  rv$year <- reactive(str_sub(input$f_q, 3, 4))
  
  #convert FY/Quarter to a real date
  rv$start_date <- reactive(lubridate::mdy(paste0(rv$start_quarter(), "/", ifelse(str_sub(input$f_q, 5, 7) == "Q1" | str_sub(input$f_q, 5, 7) == "Q2", as.numeric(rv$year())-1, rv$year()))))
  rv$end_date <- reactive(lubridate::mdy(paste0(rv$end_quarter(), "/", ifelse(str_sub(input$f_q, 5, 7) == "Q1" | str_sub(input$f_q, 5, 7) == "Q2", as.numeric(rv$year())-1, rv$year()))))
  
  # headers and sub tables -----
  #table header-current
  output$current_header <- renderText(
    paste("Ongoing Issues for ", input$system_id_edit)
  )
  #table header-past
  output$past_header <- renderText(
    paste("Past Issues for  ", input$system_id_edit)
  )
  
  # Open issues 
  
  rv$open_issues <- reactive(
    rv$issues() %>%
      left_join(rv$cw_status(), by = "workorder_id") %>%
      filter((is.na(status) | status == "REQUESTED"| status == "ASSIGNED"| status == "SCHEDULED") & system_id == input$system_id_edit)
    
  )
  
  # Open issue table 
  output$open_issues_table <- renderReactable(
    reactable(rv$open_issues() %>%
                select("Comp ID" = component_id, "Date Observed" = date_observed, "Reporter" = initials, "Priority" = priority, "Issue" = issue, "Entry Date" = date_entered, "Workorder ID" = workorder_id, "Status" = status),
              theme = darkly(),
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              selectionId = "current_issue_selected",
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 400)
    )
  
  
  # Past issues
  rv$closed_issues <- reactive(
    rv$issues() %>%
      left_join(rv$cw_status(), by = "workorder_id") %>%
      filter((status == "CLOSED" | status == "CANCEL" | status == "WORK COMPLETE" ) & system_id == input$system_id_edit)
    
  )
  
  # Closed issue table 
  output$closed_issues_table <- renderReactable(
    reactable(rv$closed_issues() %>%
                select("Comp ID" = component_id, "Date Observed" = date_observed, "Reporter" = initials, "Priority" = priority, "Issue" = issue, "Entry Date" = date_entered, "Workorder ID" = workorder_id, "Status" = status),
              theme = darkly(),
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              selectionId = "current_issue_selected",
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 400)
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)

# end ; close the DB connection 
