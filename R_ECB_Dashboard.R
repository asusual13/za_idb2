#install.packages(c("shiny", "shinydashboard", "shinythemes", "readxl","DT","reshape2","shinyBS","dplyr","rqdatatable"))
library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(DT)
library(reshape2)
library(shinyBS)
library(dplyr)
library(rqdatatable)
setwd("/Users/andrejmatic/Desktop/R project")
#global variable declaration
example_data <- read_excel("SHSGMetrics4Dash.xlsx", 
                           sheet = "example")
traffic_data = read_excel("SHSGMetrics4Dash.xlsx", sheet = "TrafficLight")


ui <- fluidPage(  
  theme = shinytheme("spacelab"),
  navbarPage(title = "Dashboard", id = "dashboard", 
             tabPanel(title = "TRAFFIC LIGHT", value = "panel1",
                      mainPanel(
                        tags$head(tags$style(".modal-dialog{ width:1000px}")),
                        tags$head(tags$style(".modal-body{ min-height:700px}")),
                        uiOutput("popup1"),
                        dataTableOutput('table1')  
                        ,width = 12) 
             ),
             tabPanel(title = "SOURCE DATA",
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     selectInput('col1','Select Exception',choices =unique(example_data$IP),selected = NULL, multiple = T
                                     ),
                                     selectInput('col2','Select Clean Copy ID',choices = unique(example_data$CC_SERIAL_NUMBER),selected = NULL, multiple = T
                                     ),
                                     selectInput('col3','Select Group ID',choices = unique(example_data$GROUP_ID),selected = NULL, multiple = T
                                     ), 
                        ),
                        mainPanel("My table", width = 10,
                                  dataTableOutput("gapminder_table")
                        )
                      )
             )
             
  )
  
)

server <- function(input, output, session) { 
  
  # If both inputs are not null, filter the table
  output$gapminder_table <- renderDataTable({
    df <-  datatable(example_data, options = list( pageLength = 5,scrollX=T))
    print(input$col1)
    if(!is.null(input$col1)&& !is.null(input$col2) &&!is.null(input$col3))
    {
      #filter sample list
      sample_space <-dput(as.character(input$col1))
      sample_space2 <-dput(as.character(input$col2))
      sample_space3 <-dput(as.character(input$col3))  
      
      #filtered dataframes
      fd_1 = example_data[grep(paste0("^",sample_space,collapse = "|"),example_data$IP ),] 
      fd_2 = example_data[grep(paste0("^",sample_space2,collapse = "|"),example_data$CC_SERIAL_NUMBER ),]
      fd_3 = example_data[grep(paste0("^",sample_space3,collapse = "|"),example_data$GROUP_ID ),]
      
      #extract similar rows
      fil_1 = inner_join(fd_1, fd_2)
      fil_2= inner_join(fil_1, fd_3)
      
      df <-  datatable(fil_2, options = list( pageLength = 5,scrollX=T))
    }
    df
  })
  
  #helper function for datatable button insert
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  output$table1 <- renderDataTable({ 
    
    group_id = example_data$GROUP_ID[example_data$GROUP_ID %in% traffic_data$`Group ID`]
    ip_out = example_data$IP[example_data$GROUP_ID %in% traffic_data$`Group ID`]
    ips_i = unique(data.frame(group_id,ip_out))
    
    d_out = dcast(ips_i, group_id~ ip_out)
    
    rename <- function(df, column, new){
      x <- names(df)                                
      if (is.numeric(column)) column <- x[column]   
      names(df)[x %in% column] <- new              
      return(df)
    }
    
    d_out = rename(d_out, 'group_id', 'Group ID') 
    
    df_sync = natural_join(d_out, traffic_data, 
                           by = "Group ID",
                           jointype = "FULL")
    
    df_final = df_sync[names(traffic_data)]
    df_final[is.na(df_final)] <- ""
    df_final = df_final[ order(match(df_final$`Group ID`,traffic_data$`Group ID`)), ]
    
    
    df_final$`Global metric`= ifelse(df_final$`Group ID` %in% d_out$`Group ID`,"-","")
    
    my_table <- cbind(rownames(df_final), df_final) 
    colnames(my_table)[1] <- 'Group ID' 
    my_table$`Group ID` <- shinyInput(actionButton, NROW(df_final), 'button_', label = "Details", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')  
    
    datatable(my_table, escape = F)%>%
      formatStyle(columns = c("Global metric","IP1","IP2","IP3","IP4","EP1","EP2"), 
                  background = styleEqual(c("-","IP1","IP2","IP3","IP4","EP1","EP2"), c("pink","pink","pink","pink","pink","pink","pink")))%>%
      formatStyle(columns = c("Global metric","IP1","IP2","IP3","IP4","EP1","EP2"), 
                  background = styleEqual(c("","","","","","",""), c("lightgreen","lightgreen","lightgreen",
                                                                     "lightgreen","lightgreen","lightgreen","lightgreen")))
    
  }, escape = FALSE)
  
  df <- reactiveValues(
    
    data = data.frame( 
      Name =   dput(as.character(paste(traffic_data$`Group ID`))) 
    ) 
  ) 
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    myValue$global_id <<- paste(df$data[selectedRow,1])
  })
  myValue <- reactiveValues(global_id = '') 
  
  observeEvent(input$select_button, {
    toggleModal(session, "modal1", "open")
  })
  
  
  output$dfs <- renderDT({
    print(myValue$global_id)
    sample_space <-c( myValue$global_id) 
    sample_space = example_data[example_data$GROUP_ID  %in% sample_space,]
    
    datatable(sample_space, options = list( 
      pageLength = 5,
      scrollX=T) 
    ) 
  },)  
  
  output$popup1 <- renderUI({ 
    bsModal("modal1", title='Available datasets', trigger='select_button', 
            tags$b('Click on a row to select a dataset.'),
            br(),  
            br(),  
            DT::dataTableOutput('dfs') 
            
    )  
  })
  
}

shinyApp(ui, server)
