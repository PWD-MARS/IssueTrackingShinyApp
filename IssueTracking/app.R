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
                                                    selectInput("f_q", "Entry Fiscal Quarter", choices = c("All", q_list)),
                                                    selectInput("issues", "Issue Category", choices = c("All", issue_choices)),
                                                    selectInput("status", "Status", choices = c("All", status_choices)),
                                                    downloadButton("download_table", "Download"),
                                                    actionButton("clear_all", "Clear All Fields")
                                                    
                                                  ),
                                                  mainPanel(
                                                    strong(span(textOutput("table_name"), style = "font-size:22px")),
                                                    reactableOutput("all_issues_table")

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
                                                      "numeric_woid", 
                                                      "Cityworks Workorder ID", 
                                                      value = NULL,
                                                      step = 1,
                                                      min = 1, 
                                                      max = 100000000 
                                                    ),
                                                    textAreaInput("inspector_note", "Inspector Notes", height = 93),
                                                    textAreaInput("gso_note", "GSO Notes", height = 93),
                                                    disabled(actionButton("submit_btn", "Save/Edit Issue")),
                                                    actionButton("clear_edit", "Clear All Fields")
                                                    
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
),

# Custom CSS to change the text color of inspector_note and gso_note to black
tags$style(HTML("
    #image_link, #numeric_woid, #reporter_initials, #inspector_note, #gso_note {
      color: black !important;  /* Ensures text color is black */
    }
  
  "))

)



# Server -----
server <- function(input, output, session) {
  
  ## initialzie reactive values ----
  rv <- reactiveValues()
  
  # row references 
  rv$all_issues_row <- reactive(getReactableState("all_issues_table", "selected"))
  rv$open_issues_row <- reactive(getReactableState("open_issues_table", "selected"))
  rv$closed_issues_row <- reactive(getReactableState("closed_issues_table", "selected"))
  
  
  # all issues
  rv$issues <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.viw_issues_full"))
  
  # issue lookup
  rv$wo_lookup <- reactive(dbGetQuery(conn, "SELECT * FROM fieldwork.issue_wo_lookup"))
  
  # cityworks status
  rv$cw_status <- reactive(dbGetQuery(cw_conn, paste("SELECT WORKORDERID, STATUS FROM Azteca.WORKORDER where WORKORDERID in (", toString(paste("'", rv$wo_lookup()$workorder_id, "'", sep = "")),")", sep = "")) %>%
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
  
  #table header-all
  output$table_name <- renderText(
    paste("All Issues")
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
    reset("numeric_woid")
    reset("inspector_note")
    reset("gso_note")
    
    removeModal()
  })
  
  
  
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
    delay(10 , updateSelectInput(session, "issues_sub", selected = rv$open_issues()$issue[rv$open_issues_row()])) # delay enusres sub issues input is enabled before update
    updateSelectInput(session, "date_observed", selected = rv$open_issues()$date_entered[rv$open_issues_row()])
    updateTextAreaInput(session, "image_link", value = rv$open_issues()$link_image[rv$open_issues_row()])
    updateTextAreaInput(session, "reporter_initials", value = rv$open_issues()$initials[rv$open_issues_row()])
    updateSelectInput(session, "priority", selected = rv$open_issues()$priority[rv$open_issues_row()])
    updateSelectInput(session, "numeric_woid", selected = as.numeric(rv$open_issues()$workorder_id[rv$open_issues_row()]))
    updateTextAreaInput(session, "inspector_note", value = rv$open_issues()$inspector_notes[rv$open_issues_row()])
    updateTextAreaInput(session, "gso_note", value = rv$open_issues()$notes[rv$open_issues_row()])
    
    
    
  })
  
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
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 430,
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
    delay(10 , updateSelectInput(session, "issues_sub", selected = rv$closed_issues()$issue[rv$closed_issues_row()])) # delay enusres sub issues input is enabled before update
    updateSelectInput(session, "date_observed", selected = rv$closed_issues()$date_entered[rv$closed_issues_row()])
    updateTextAreaInput(session, "image_link", value = rv$closed_issues()$link_image[rv$closed_issues_row()])
    updateTextAreaInput(session, "reporter_initials", value = rv$closed_issues()$initials[rv$closed_issues_row()])
    updateSelectInput(session, "priority", selected = rv$closed_issues()$priority[rv$closed_issues_row()])
    updateSelectInput(session, "numeric_woid", selected = as.numeric(rv$closed_issues()$workorder_id[rv$closed_issues_row()]))
    updateTextAreaInput(session, "inspector_note", value = rv$closed_issues()$inspector_notes[rv$closed_issues_row()])
    updateTextAreaInput(session, "gso_note", value = rv$closed_issues()$notes[rv$closed_issues_row()])
    
    
  })
  
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
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 430,
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
  
  # status filtering
  rv$status_filter <- reactive(
    if(input$status == "" | input$status == "All") {
      c(status_choices, NA)          # show NAs too
    } else{
      input$status
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
    reset("numeric_woid")
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
        filter(status %in% rv$status_filter())
    } else {
      rv$issues() %>%
        left_join(rv$cw_status(), by = "workorder_id") %>%
        filter(system_id %in% rv$system_filter()) %>%
        filter(category %in% rv$issue_filter()) %>%
        filter(status %in% rv$status_filter()) %>%
        filter(date_entered <= rv$end_date() & date_entered >= rv$start_date())
    }

    
  )
  
  # All issue table 
  output$all_issues_table <- renderReactable(
    reactable(rv$all_issues() %>%
                select("System ID" = system_id, "Comp ID" = component_id, "Date Observed" = date_observed, "Reporter" = initials, "Priority" = priority, "Issue" = issue, "Entry Date" = date_entered, "Workorder ID" = workorder_id, "Status" = status),
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
                "Issue" = colDef(width = 200),
                "Comp ID" = colDef(width = 200)
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
  
}

# Run the application
shinyApp(ui = ui, server = server)

# end ; close the DB connection 
