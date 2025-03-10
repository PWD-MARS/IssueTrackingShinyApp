### Issue Tracking App
# By: FE
# Last changed: 11/21/2024

# Set Up -----
# Load necessary libraries
# SET UP
## Load libraries --------------
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

# Database connection and global options --------

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

#this function adds a little red star to indicate that a field is required. It uses HTML, hence "html_req"
html_req <- function(label){
  HTML(paste(label, tags$span(style="color:red", tags$sup("*"))))
}

# Global variables -----

# fiscal quarter lookup
q_list  <- dbGetQuery(conn,"select * from admin.tbl_fiscal_quarter_lookup where fiscal_quarter_lookup_uid > 35") %>%
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

# cw status
cw_status_choices <- odbc::dbGetQuery(conn, paste0("SELECT * FROM fieldwork.issue_cwstatus_lookup")) %>% 
  select(cw_status) %>%
  pull

# gso status
gso_status <- odbc::dbGetQuery(conn, paste0("SELECT * FROM fieldwork.issue_gsostatus_lookup"))
gso_status_choices <- gso_status %>% 
  select(gso_status) %>%
  pull

# priority lookup
priority <- odbc::dbGetQuery(conn, paste0("SELECT * FROM fieldwork.issue_priority_lookup")) 
priority_choices <- priority %>% 
  select(priority) %>%
  pull
# wo id list
woid <- dbGetQuery(cw_conn, "SELECT distinct(WORKORDERID) FROM Azteca.WORKORDER where INITIATEDATE > '2020-01-01'") %>%
                      pull

# UI -----

# Define UI
ui <- tagList(useShinyjs(), navbarPage("Issue Tracking App", id = "TabPanelID", theme = shinytheme("cyborg"),
                                       tabPanel("Issues Table", value = "status",  ## First tab -----
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectizeInput ("system_id", "System ID", choices = NULL),
                                                    selectInput("f_q", "Entry Fiscal Quarter", choices = c("All", q_list)),
                                                    selectInput("issues", "Issue Category", choices = c("All", issue_choices)),
                                                    selectInput("status", "Cityworks Status", choices = c("All", cw_status_choices)),
                                                    selectInput("gso_status", "GSO Status", choices = c("All", gso_status_choices)),
                                                    selectInput("priority_filter", "Priority Level", choices = c("All", priority_choices)),
                                                    downloadButton("download_table", "Download"),
                                                    actionButton("clear_all", "Clear All Fields"),
                                                    width = 3
                                                    
                                                  ),
                                                  mainPanel(
                                                    strong(span(textOutput("table_name"), style = "font-size:22px")),
                                                    reactableOutput("all_issues_table"),
                                                    width = 9
                                                    

                                                  )
                                                )
                                       ),
                                       tabPanel("Add/Edit Issues", value = "add_edit", ## Second tab -----
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectizeInput ("system_id_edit", html_req("System ID"), choices = NULL),
                                                    selectInput("component_id", "Component ID", choices = "", selected = NULL),
                                                    selectInput("issues_edit", html_req("Issue Category"), choices = c("", issue_choices), selected = ''),
                                                    conditionalPanel(condition = "input.issues_edit !== ''",
                                                                     selectInput("issues_sub", html_req("Issue"), choices = "", selected = NULL)),
                                                    dateInput("date_observed", html_req("Date Observed"), value = as.Date(NA)),
                                                    textInput("image_link", "Link to Image"),
                                                    textInput("reporter_initials", html_req("Reporter Initials")),
                                                    selectInput("priority", html_req("Priority Level"), choices = c("", priority_choices), selected = ""),
                                                    selectInput("gso_status_edit", html_req("GSO Status"), choices = c("", gso_status_choices), selected = ""),
                                                    selectizeInput ("char_woid", "Cityworks Workorder ID", choices = NULL),
                                                    textAreaInput("inspector_note", "Inspector Notes", height = 100),
                                                    textAreaInput("gso_note", "GSO Notes", height = 100),
                                                    #disabled(actionButton("submit_btn", "Save/Edit Issue")),
                                                    actionButton("submit_btn", "Save/Edit Issue"),
                                                    actionButton("clear_edit", "Clear All Fields"),
                                                    actionButton("update_wo", "Update WO IDs"),
                                                    fluidRow(HTML(paste(html_req(""), " indicates required field for submission. "))),
                                                    width = 3
                                                    
                                                  ),
                                                  mainPanel(
                                                    
                                                    conditionalPanel(condition = "input.system_id_edit",
                                                                     h4(textOutput("current_header")),
                                                                     reactableOutput("open_issues_table"),
                                                                     h4(textOutput("past_header")), 
                                                                     reactableOutput("closed_issues_table")),
                                                    width = 9
                                                    
                                                  )
                                                )
                                                )
),

# Custom CSS to change the text color of inspector_note and gso_note to black
tags$style(HTML("
    #image_link, #char_woid, #reporter_initials, #inspector_note, #gso_note {
      color: black !important;  /* Ensures text color is black */
    }
  
  "))

)



# Server -----
server <- function(input, output, session) {
  
  
  # Reactive Updates (toggles, labells, input options) -----
  ## initialzie reactive values 
  rv <- reactiveValues()
  
  # row references 
  rv$all_issues_row <- reactive(getReactableState("all_issues_table", "selected"))
  rv$open_issues_row <- reactive(getReactableState("open_issues_table", "selected"))
  rv$closed_issues_row <- reactive(getReactableState("closed_issues_table", "selected"))
  
  
  # all issues
  rv$issues <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.viw_issues_full"))
  
  # cityworks status
  rv$cw_status <- reactive(dbGetQuery(cw_conn, paste("SELECT WORKORDERID, STATUS FROM Azteca.WORKORDER where WORKORDERID in (", toString(paste("'", rv$issues()$workorder_id, "'", sep = "")),")", sep = "")) %>%
                             select(workorder_id = WORKORDERID, status = STATUS))
  
  # server-side selectizeinput for system ids across the tabs
  updateSelectizeInput(session, 'system_id', choices = c("All", system_id), server = TRUE)
  updateSelectizeInput(session, 'system_id_edit', choices = c('', system_id), selected = '', server = TRUE)
  updateSelectizeInput(session, 'char_woid', choices = c('', "1000", "1281325", woid), selected = '', server = TRUE)
  
  
  # update Workorders on click
  observeEvent(input$update_wo, {
    # workorder ids choices
    rv$woid <- reactive(dbGetQuery(cw_conn, "SELECT distinct(WORKORDERID) FROM Azteca.WORKORDER where INITIATEDATE > '2020-01-01'") %>%
      pull)
    updateSelectizeInput(session, 'char_woid', choices = c('', rv$woid()), selected = '', server = TRUE)
    
    showModal(modalDialog(title = "Workorder IDs Updated!", size = "s", easyClose = TRUE))

  })
  
  #process text field to prevent sql injection
  rv$inspector_note <- reactive(gsub('\'', '\'\'',  input$inspector_note))
  rv$inspector_note_trimmed  <- reactive(special_char_replace(rv$inspector_note()))
  
  rv$gso_note <- reactive(gsub('\'', '\'\'',  input$gso_note))
  rv$gso_note_trimmed  <- reactive(special_char_replace(rv$gso_note()))
  

  #show component IDs and Issues based on Systems + Issue Category 
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
  
  
  # toggle submit button
  observe(toggleState(id = "submit_btn", input$system_id_edit != ""
                      & input$issues_edit != ""
                      & input$issues_sub != "" 
                      & length(input$date_observed) > 0
                      & input$reporter_initials != ''
                      & input$priority != ''
                      & input$gso_status_edit != ''
  ))
  
  
  #add/edit button toggle
  rv$label <- reactive(if(!is.null(rv$open_issues_row()) | !is.null(rv$closed_issues_row())) "Edit Selected" else "Add New")
  observe(updateActionButton(session, "submit_btn", label = rv$label()))
  
  
  # update sub issue
  rv$sub_issue <- reactive(issue_types %>%
                             filter(category == input$issues_edit) %>%
                             select(issue) %>%
                             pull)
  observe(updateSelectInput(session, "issues_sub", choices = c("", rv$sub_issue())))
  
  # Fiscal Quarter Processing 
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
  
  # headers and sub tables 
  #table header-current
  output$current_header <- renderText(
    paste("Pending/On Hold Issues for ", input$system_id_edit)
  )
  #table header-past
  output$past_header <- renderText(
    paste("Resolved Issues for  ", input$system_id_edit)
  )
  
  #table header-all
  output$table_name <- renderText(
    if(input$f_q == "All" | input$f_q == "") {
      paste("Issues to Date")
    } else {
      paste("Issues Entered from", rv$start_date(), "to", rv$end_date(), sep = " ")
    }
  )
  
  
  # Open Issues Sub Table -----
  
  # select an open issue row
  observeEvent(rv$open_issues_row(), {
    
    # populate component combo
    rv$selected_combo_open <- reactive(rv$asset_comp() %>%
                                    filter(component_id ==  rv$open_issues()$component_id[rv$open_issues_row()]) %>%
                                    select(asset_comp_code) %>%
                                    pull)
    
    updateReactable("closed_issues_table", selected = NA)
    updateSelectInput(session, "component_id", selected = rv$selected_combo_open())
    updateSelectInput(session, "issues_edit", selected = rv$open_issues()$category[rv$open_issues_row()])
    delay(150 , updateSelectInput(session, "issues_sub", selected = rv$open_issues()$issue[rv$open_issues_row()])) # delay enusres sub issues input is enabled before update
    updateSelectInput(session, "date_observed", selected = rv$open_issues()$date_entered[rv$open_issues_row()])
    updateTextAreaInput(session, "image_link", value = rv$open_issues()$link_image[rv$open_issues_row()])
    updateTextAreaInput(session, "reporter_initials", value = rv$open_issues()$initials[rv$open_issues_row()])
    updateSelectInput(session, "priority", selected = rv$open_issues()$priority[rv$open_issues_row()])
    updateSelectInput(session, "gso_status_edit", selected = rv$open_issues()$gso_status[rv$open_issues_row()])
    updateSelectInput(session, "char_woid", selected = rv$open_issues()$workorder_id[rv$open_issues_row()])
    updateTextAreaInput(session, "inspector_note", value = rv$open_issues()$inspector_notes[rv$open_issues_row()])
    updateTextAreaInput(session, "gso_note", value = rv$open_issues()$notes[rv$open_issues_row()])
    
    
    
  })
  
  # Open issues 
  rv$open_issues <- reactive(
    rv$issues() %>%
      left_join(rv$cw_status(), by = "workorder_id") %>%
      filter((is.na(gso_status) | gso_status == "Pending"| gso_status == "On Hold") & system_id == input$system_id_edit) %>%
      arrange(desc(date_entered))
  
  )
  
  # Open issue table 
  output$open_issues_table <- renderReactable(
    reactable(rv$open_issues() %>%
                select("Comp ID" = component_id, "Date Observed" = date_observed, "Reporter" = initials, "Priority" = priority, "Issue" = issue, "Entry Date" = date_entered, "Workorder ID" = workorder_id, "GSO Status" = gso_status, "CW Status" = status),
              theme = darkly(),
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 450,
              columns = list(
                "Issue" = colDef(width = 350),
                "Comp ID" = colDef(width = 150),
                "GSO Status" = colDef(
                  style = function(value) {
                    if (value == "Resolved") {
                      return(list(background = "green", color = "white", fontweight = "bold"))
                    } else if (value == "On Hold") {
                      return(list(background = "yellow", color = "black", fontweight = "bold"))
                    } else if (value == "Pending") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                ),
                "CW Status" = colDef(
                  style = function(value) {
                    if (value == "CLOSED" | value == "WORK COMPLETE") {
                      return(list(background = "green", color = "white", fontweight = "bold"))
                    } else if (value == "REQUESTED" | value == "ASSIGNED" | value == "SCHEDULED") {
                      return(list(background = "lightgreen", color = "black", fontweight = "bold"))
                    } else if (value == "CANCEL") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                ),
                "Priority" = colDef(
                  style = function(value) {
                    if (value == "Low") {
                      return(list(background = "pink", color = "black", fontweight = "bold"))
                    } else if (value == "Medium") {
                      return(list(background = "orange", color = "black", fontweight = "bold"))
                    } else if (value == "High") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                )
              ),
              details = function(index) {
                note_link <- rv$open_issues()[rv$open_issues()$issue_uid == rv$open_issues()$issue_uid[index], ] %>%
                  select("Inspector Note" = inspector_notes, "GSO Notes" = notes, "Image Link" = link_image)
                htmltools::div(style = "padding: 1rem",
                               reactable(note_link, 
                                         theme = darkly(),
                                         outlined = TRUE)
                )
              })
    )
  
  
  # Closed Issues Sub Table ----
  
  # select an closed issue row
  observeEvent(rv$closed_issues_row(), {
    
    # populate component combo
    rv$selected_combo_closed <- reactive(rv$asset_comp() %>%
                                    filter(component_id ==  rv$closed_issues()$component_id[rv$closed_issues_row()]) %>%
                                    select(asset_comp_code) %>%
                                    pull)
    
    updateReactable("open_issues_table", selected = NA)
    updateSelectInput(session, "component_id", selected = rv$selected_combo_closed())
    updateSelectInput(session, "issues_edit", selected = rv$closed_issues()$category[rv$closed_issues_row()])
    delay(150 , updateSelectInput(session, "issues_sub", selected = rv$closed_issues()$issue[rv$closed_issues_row()])) # delay enusres sub issues input is enabled before update
    updateSelectInput(session, "date_observed", selected = rv$closed_issues()$date_entered[rv$closed_issues_row()])
    updateTextAreaInput(session, "image_link", value = rv$closed_issues()$link_image[rv$closed_issues_row()])
    updateTextAreaInput(session, "reporter_initials", value = rv$closed_issues()$initials[rv$closed_issues_row()])
    updateSelectInput(session, "priority", selected = rv$closed_issues()$priority[rv$closed_issues_row()])
    updateSelectInput(session, "gso_status_edit", selected = rv$closed_issues()$gso_status[rv$closed_issues_row()])
    updateSelectInput(session, "char_woid", selected = rv$closed_issues()$workorder_id[rv$closed_issues_row()])
    updateTextAreaInput(session, "inspector_note", value = rv$closed_issues()$inspector_notes[rv$closed_issues_row()])
    updateTextAreaInput(session, "gso_note", value = rv$closed_issues()$notes[rv$closed_issues_row()])
    
    
  })
  
  # Past issues
  rv$closed_issues <- reactive(
    rv$issues() %>%
      left_join(rv$cw_status(), by = "workorder_id") %>%
      filter(gso_status == "Resolved" & system_id == input$system_id_edit) %>%
      arrange(desc(date_entered))
  )
  
  # Closed issue table 
  output$closed_issues_table <- renderReactable(
    reactable(rv$closed_issues() %>%
                select("Comp ID" = component_id, "Date Observed" = date_observed, "Reporter" = initials, "Priority" = priority, "Issue" = issue, "Entry Date" = date_entered, "Workorder ID" = workorder_id, "GSO Status" = gso_status, "CW Status" = status),
              theme = darkly(),
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 450,
              columns = list(
                "Issue" = colDef(width = 350),
                "Comp ID" = colDef(width = 150),
                "GSO Status" = colDef(
                  style = function(value) {
                    if (value == "Resolved") {
                      return(list(background = "green", color = "white", fontweight = "bold"))
                    } else if (value == "On Hold") {
                      return(list(background = "yellow", color = "black", fontweight = "bold"))
                    } else if (value == "Pending") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                ),
                "CW Status" = colDef(
                  style = function(value) {
                    if (value == "CLOSED" | value == "WORK COMPLETE") {
                      return(list(background = "green", color = "white", fontweight = "bold"))
                    } else if (value == "REQUESTED" | value == "ASSIGNED" | value == "SCHEDULED") {
                      return(list(background = "lightgreen", color = "black", fontweight = "bold"))
                    } else if (value == "CANCEL") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                ),
                "Priority" = colDef(
                  style = function(value) {
                    if (value == "Low") {
                      return(list(background = "pink", color = "black", fontweight = "bold"))
                    } else if (value == "Medium") {
                      return(list(background = "orange", color = "black", fontweight = "bold"))
                    } else if (value == "High") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                )
              ),
              details = function(index) {
                note_link <- rv$closed_issues()[rv$closed_issues()$issue_uid == rv$closed_issues()$issue_uid[index], ] %>%
                  select("Inspector Note" = inspector_notes, "GSO Notes" = notes, "Image Link" = link_image)
                htmltools::div(style = "padding: 1rem",
                               reactable(note_link, 
                                         theme = darkly(),
                                         outlined = TRUE)
                )
              })
  )
  
  ## Add/Edit Open/Closed issues ----
  # return ids
  rv$input_issue_type_uid <- reactive(issue_types %>%
    filter(issue == input$issues_sub) %>%
    select(issue_type_uid) %>%
    pull)
  
  rv$input_priority_uid <- reactive(priority %>%
    filter(priority == input$priority) %>%
    select(priority_uid) %>%
    pull)
  
  rv$input_gsostatus_uid <- reactive(gso_status %>%
    filter(gso_status == input$gso_status_edit) %>%
    select(gsostatus_uid) %>%
    pull)
  # populate component combo
  rv$new_comp_id <- reactive(rv$asset_comp() %>%
                                       filter(asset_comp_code ==  input$component_id) %>%
                                       select(component_id) %>%
                                       pull)
  
  # On click "submit_btn"
  observeEvent(input$submit_btn, {
    if(is.null(rv$open_issues_row()) & is.null(rv$closed_issues_row())) {
    
      new_issue_df <- data.frame(system_id = input$system_id_edit,
                                 component_id = ifelse(length(rv$new_comp_id()) == 0, NA, rv$new_comp_id()),
                                 issue_type_uid = rv$input_issue_type_uid(),
                                 date_observed = input$date_observed,
                                 link_image = ifelse(input$image_link == '', NA, input$image_link),
                                 inspector_notes = ifelse(rv$inspector_note_trimmed() == '', NA, rv$inspector_note_trimmed()),
                                 notes = ifelse(rv$gso_note_trimmed() == '', NA, rv$gso_note_trimmed()),
                                 initials = ifelse(input$reporter_initials == '', NA, input$reporter_initials),
                                 priority_uid = ifelse(length(rv$input_priority_uid()) == 0, NA, rv$input_priority_uid()),
                                 date_entered = Sys.Date(),
                                 gsostatus_uid = ifelse(length(rv$input_gsostatus_uid()) == 0, NA, rv$input_gsostatus_uid()),
                                 workorder_id = ifelse(input$char_woid == '', NA, input$char_woid)
                                 )
      
      odbc::dbWriteTable(conn, Id(schema = "fieldwork", table = "issues"), new_issue_df, append= TRUE, row.names = FALSE )
      
      # reset and pull
      
      rv$issues <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.viw_issues_full"))
      reset("component_id")
      reset("issues_edit")
      reset("issues_sub")
      reset("date_observed")
      reset("image_link")
      reset("reporter_initials")
      reset("priority")
      reset("char_woid")
      reset("inspector_note")
      reset("gso_note")
      reset("gso_status_edit")
      reset("open_issues_table")
      reset("closed_issues_table")
      
      
  
    } else if (!is.null(rv$open_issues_row()) & is.null(rv$closed_issues_row())) {
      
      edit_open_issue_query <- paste0("Update fieldwork.issues SET component_id= ", ifelse(length(rv$new_comp_id()) == 0, 'NULL', paste("'", rv$new_comp_id(), "'", sep = "")), 
                                      ", issue_type_uid = ", rv$input_issue_type_uid(), ", date_observed = '", input$date_observed, "', link_image = '", input$image_link,
                                      "', inspector_notes = ", ifelse(rv$inspector_note_trimmed() == '', 'NULL', paste("'",rv$inspector_note_trimmed(),"'", sep = "")),
                                      ", initials = ", paste("'", input$reporter_initials,"'", sep = ""),
                                      ", notes = ", ifelse(rv$gso_note_trimmed() == '', 'NULL', paste("'", rv$gso_note_trimmed(),"'", sep = "")),
                                      ", priority_uid = ", rv$input_priority_uid(),
                                      ", gsostatus_uid = ", rv$input_gsostatus_uid(),
                                      ", workorder_id = ", ifelse(input$char_woid == '', 'NULL', paste("'", input$char_woid, "'", sep = "")), " where issue_uid = " , rv$open_issues()$issue_uid[rv$open_issues_row()], sep = "")
      odbc::dbGetQuery(conn, edit_open_issue_query)
      
      # reset and pull
      
      
      rv$issues <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.viw_issues_full"))
      reset("component_id")
      reset("issues_edit")
      reset("issues_sub")
      reset("date_observed")
      reset("image_link")
      reset("reporter_initials")
      reset("priority")
      reset("char_woid")
      reset("inspector_note")
      reset("gso_note")
      reset("gso_status_edit")
      reset("open_issues_table")
      reset("closed_issues_table")
      
    } else if (is.null(rv$open_issues_row()) & !is.null(rv$closed_issues_row())) {
      
      edit_closed_issue_query <- paste0("Update fieldwork.issues SET component_id= ", ifelse(length(rv$new_comp_id()) == 0, 'NULL', paste("'", rv$new_comp_id(), "'", sep = "")), 
                                      ", issue_type_uid = ", rv$input_issue_type_uid(), ", date_observed = '", input$date_observed, "', link_image = '", input$image_link,
                                      "', inspector_notes = ", ifelse(rv$inspector_note_trimmed() == '', 'NULL', paste("'",rv$inspector_note_trimmed(),"'", sep = "")),
                                      ", initials = ", paste("'", input$reporter_initials,"'", sep = ""),
                                      ", notes = ", ifelse(rv$gso_note_trimmed() == '', 'NULL', paste("'", rv$gso_note_trimmed(),"'", sep = "")),
                                      ", priority_uid = ", rv$input_priority_uid(),
                                      ", gsostatus_uid = ", rv$input_gsostatus_uid(),
                                      ", workorder_id = ", ifelse(input$char_woid == '', 'NULL', paste("'", input$char_woid, "'", sep = "")), " where issue_uid = " , rv$closed_issues()$issue_uid[rv$closed_issues_row()], sep = "")
      odbc::dbGetQuery(conn, edit_closed_issue_query)
      
      # reset and pull
      
      rv$issues <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.viw_issues_full"))
      reset("component_id")
      reset("issues_edit")
      reset("issues_sub")
      reset("date_observed")
      reset("image_link")
      reset("reporter_initials")
      reset("priority")
      reset("char_woid")
      reset("inspector_note")
      reset("gso_note")
      reset("gso_status_edit")
      reset("open_issues_table")
      reset("closed_issues_table")
      
    }
    
    
    
    
  })
  
  

  # All Issues Sub Table ------
  ### Reactive Filtering
  
  # system id filtering
  rv$system_filter <- reactive(
    if(input$system_id == "" | input$system_id == "All") {
      c(system_id, NA)                # show NAs too
    } else{
      input$system_id
    }
  )
  
  # issue filtering
  rv$issue_filter <- reactive(
    if(input$issues == "" | input$issues == "All") {
      c(issue_choices, NA)            # show NAs too
    } else{
      input$issues
    }
  )
  
  # CW status filtering
  rv$status_filter <- reactive(
    if(input$status == "" | input$status == "All") {
      c(cw_status_choices, NA)          # show NAs too
    } else{
      input$status
    }
  )
  
  # gso status filtering
  rv$gso_status_filter <- reactive(
    if(input$gso_status == "" | input$gso_status == "All") {
      c(gso_status_choices, NA)          # show NAs too
    } else{
      input$gso_status
    }
  )
  
  
  # gso status filtering
  rv$priority_filter <- reactive(
    if(input$priority_filter == "" | input$priority_filter == "All") {
      c(priority_choices, NA)          # show NAs too
    } else{
      input$priority_filter
    }
  )
  
  
# Switch Tabs if a row from the first tab selected
  observeEvent(rv$all_issues_row(), {
    updateTabsetPanel(session, "TabPanelID", selected = "add_edit")
    updateSelectInput(session, "system_id_edit", selected = rv$all_issues()$system_id[rv$all_issues_row()])
    updateReactable("all_issues_table", selected = NA)
    
  }
  )
  
  
# Clear fields if system id updates in second tab 
  observeEvent(input$system_id_edit, {
    reset("component_id")
    reset("issues_edit")
    reset("issues_sub")
    reset("date_observed")
    reset("image_link")
    reset("reporter_initials")
    reset("priority")
    reset("char_woid")
    reset("inspector_note")
    reset("gso_note")
    
  }
  )
    
  
  # All issues
  rv$all_issues <- reactive(
    if(input$f_q == "All"){
      rv$issues() %>%
        left_join(rv$cw_status(), by = "workorder_id") %>%
        filter(system_id %in% rv$system_filter()) %>%
        filter(category %in% rv$issue_filter()) %>%
        filter(status %in% rv$status_filter()) %>%
        filter(gso_status %in% rv$gso_status_filter()) %>%
        filter(priority %in% rv$priority_filter()) %>%
        arrange(desc(date_entered))
      
    } else {
      rv$issues() %>%
        left_join(rv$cw_status(), by = "workorder_id") %>%
        filter(system_id %in% rv$system_filter()) %>%
        filter(category %in% rv$issue_filter()) %>%
        filter(status %in% rv$status_filter()) %>%
        filter(gso_status %in% rv$gso_status_filter()) %>%
        filter(date_entered <= rv$end_date() & date_entered >= rv$start_date()) %>%
        filter(priority %in% rv$priority_filter()) %>%
        arrange(desc(date_entered))
      
    }

    
  )
  
  # All issue table 
  output$all_issues_table <- renderReactable(
    reactable(rv$all_issues() %>%
                select("System ID" = system_id, "Comp ID" = component_id, "Date Observed" = date_observed, "Reporter" = initials, "Issue" = issue,"Priority" = priority, "Entry Date" = date_entered, "GSO Status" = gso_status, "CW Status" = status),
              theme = darkly(),
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              columns = list(
                "Issue" = colDef(width = 350),
                "Comp ID" = colDef(width = 200),
                "Date Observed" = colDef(width = 150),
                "GSO Status" = colDef(
                  style = function(value) {
                    if (value == "Resolved") {
                      return(list(background = "green", color = "white", fontweight = "bold"))
                    } else if (value == "On Hold") {
                      return(list(background = "yellow", color = "black", fontweight = "bold"))
                    } else if (value == "Pending") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                ),
                "CW Status" = colDef(
                  style = function(value) {
                    if (value == "CLOSED" | value == "WORK COMPLETE") {
                      return(list(background = "green", color = "white", fontweight = "bold"))
                    } else if (value == "REQUESTED" | value == "ASSIGNED" | value == "SCHEDULED") {
                      return(list(background = "lightgreen", color = "black", fontweight = "bold"))
                    } else if (value == "CANCEL") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                ),
                "Priority" = colDef(
                  style = function(value) {
                    if (value == "Low") {
                      return(list(background = "pink", color = "black", fontweight = "bold"))
                    } else if (value == "Medium") {
                      return(list(background = "orange", color = "black", fontweight = "bold"))
                    } else if (value == "High") {
                      return(list(background = "#A70D2A", color = "white", fontweight = "bold"))
                    } else {
                      # Handle any unexpected values gracefully (default to white)
                    }
                  }
                )
              ),
              details = function(index) {
                note_link <- rv$all_issues()[rv$all_issues()$issue_uid == rv$all_issues()$issue_uid[index], ] %>%
                  select("Inspector Note" = inspector_notes, "GSO Notes" = notes, "Image Link" = link_image)
                htmltools::div(style = "padding: 1rem",
                               reactable(note_link, 
                                         theme = darkly(),
                                         outlined = TRUE)
                )
              })
  )
  
  # Download -----
  output$download_table <- downloadHandler(
    
    filename = function() {
      paste("IssueTrackingTable", "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(filename){
      
      df_list <- list(rv$issues() %>%
                        left_join(rv$cw_status(), by = "workorder_id") %>% 
                        select("SystemID" = system_id, "CompID" = component_id, "DateObserved" = date_observed, "Reporter" = initials, "Issue" = issue, "EntryDate" = date_entered, "GSOStatus" = gso_status, "CWStatus" = status, "Priority" = priority, "InspectorNote" = inspector_notes, "GSONotes" = notes, "ImageLink" = link_image)
      )
      write.xlsx(x = df_list , file = filename, row.names = FALSE)
    }
  )
  
  # Clear buttons -----
  # first tab
  observeEvent(input$clear_all, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton("confirm_clear_pcs", "Yes")))
  })
  
  
  observeEvent(input$confirm_clear_pcs, {
    reset("system_id")
    reset("status")
    reset("gso_status")
    reset("issues")
    reset("f_q")
    
    removeModal()
  })
  
  # second tab
  observeEvent(input$clear_edit, {
    showModal(modalDialog(title = "Clear All Fields", 
                          "Are you sure you want to clear all fields on this tab?", 
                          modalButton("No"), 
                          actionButton("confirm_clear_pcs", "Yes")))
  })
  
  observeEvent(input$confirm_clear_pcs, {
    reset("system_id_edit")
    reset("component_id")
    reset("issues_edit")
    reset("issues_sub")
    reset("date_observed")
    reset("image_link")
    reset("reporter_initials")
    reset("priority")
    reset("char_woid")
    reset("inspector_note")
    reset("gso_note")
    reset("gso_status_edit")
    
    
    removeModal()
  })
  
  

}

# Run the application
shinyApp(ui = ui, server = server)

# end ; close the DB connection 
