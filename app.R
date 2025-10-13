library(shiny)
library(jsonlite)
library(bslib)
library(stringr)
library(dplyr)
library(shinyvalidate)
library(shinyFiles)
library(processx)
library(digest)
library(hover)

source('global.R')

sidebar <- sidebar(
  #title = '',
  tagList(
    tags$div(
      #style = "background-color:powderblue;",
      #This create the dropdown list with available pipelines, the config.json is maintained manually
      create_conditional_ui(jsonlite::fromJSON("config.json", simplifyDataFrame = FALSE))
    ),

    # these are the inputs loaded from the corresponding pipelines/json file
    uiOutput('pipeline_inputs'),
    
  )
)
ui <- page_navbar(
  use_hover(),
  sidebar = sidebar,
  theme = bs_theme(bootswatch = 'yeti', primary = '#196F3D'),
  ########## controls
  tags$div(
    hover_action_button('start', 'Start', icon = icon('play'), button_animation = 'icon-fade'),
    #actionButton("run", "Run pipeline"),
    #actionButton('stop', 'Stop')
    hover_action_button('reset', 'Reset', icon = icon('rotate'), button_animation = 'icon-fade')
  ),
  tags$hr(),
  ########## controls
  card(
    card_header('Pipeline runs'),
    card_body(
    h4("Saved State JSON Preview:"),
    verbatimTextOutput("save_preview")
    ) 
  ),
  card(
    card_header("Selected pipeline output"),
    verbatimTextOutput('stdout')
  )
)

server <- function(input, output, session) {
  # which pipeline
  json <- reactive({
    read_json(path = fs::path('pipelines', input$pipelines), simplifyDataFrame = FALSE)
  })
  
  # render pipeline inputs based on pipeline selected
  output$pipeline_inputs <- renderUI({
    create_conditional_ui(json())
  })
  
  # special case shinyFiles - shinyDirChoose bindings in server
  #shinyDirChoose(input, 'fastq', root=c(root=Sys.getenv('HOME')))
  observeEvent(input$pipelines, {
    bind_shinyfiles(input = input, config = json())
  })
  
  
  # Validations
  iv <- InputValidator$new()
  
  observe({
    
    lapply(json(), function(p){
      if (p$required) {
        iv$add_rule(p$inputId, sv_required(message = 'Parameter is required!'))
      }
    })
  })
  
  # Reactive value to store the JSON string for display
  saved_json_state <- reactiveVal("")
  
  # --- Logic to save the input state ---
  observeEvent(input$start, {
    
    #iv$enable()
    # # Check if all inputs are valid before proceeding
    # if (!iv$is_valid()) {
    #   showNotification("Please correct the required fields!", type = "warning")
    #   # Stop the rest of the execution
    #   return() 
    # }
    
    # Load the original configuration file once (using a reactiveVal or simple variable)
    config_data_list <- read_json(path = fs::path('pipelines', input$pipelines), simplifyDataFrame = FALSE)
    
    # 1. Initialize an empty list to store the final state
    final_state_list <- list()
    
    # 2. Iterate through the original configuration items
    for (config_item in config_data_list) {
      
      id <- config_item$inputId
      
      # 3. Retrieve the current value from the 'input' object
      # Use isolate() to ensure this code only runs when the button is clicked,
      # not when the input values change.
      current_value <- isolate(input[[id]]) 
      
      ########################################
      # take only datapath for fileInputs
      if(config_item$type == 'fileInput') {
        current_value <- isolate(input[[id]]$datapath)
      
      # shinyFiles cases
      } else if (config_item$type == 'shinyDirButton') {
        current_value <- parseDirPath(roots = c(wd = "/"), selection = input[[id]])
      } else if (config_item$type == 'shinyFilesButton') {
        parsed_file <- parseFilePaths(roots = c(wd = "/"), selection = input[[id]])
        current_value <- parsed_file$datapath
      }
      ########################################
      
      # 4. Construct the output object for this input, note setNames
      output_item <- setNames(list(id = current_value),id)
      
      # 5. Append to the final list
      final_state_list <- append(final_state_list, output_item)
    }
    
    # 6. Convert the list to a formatted JSON string
    json_output <- jsonlite::toJSON(
      final_state_list, 
      pretty = TRUE, 
      auto_unbox = TRUE
    )
    
    # 7. Write the JSON string to a file (e.g., 'saved_state.json')
    json_to_write <- str_remove_all(json_output, "^\\[|\\]$")
    write(json_to_write, "saved_state.json")
    
    # Update the reactive value for display
    saved_json_state(json_output)
    
    showNotification("Configuration state saved to 'saved_state.json'!", type = "message")
  })
  
  # --- Display the saved JSON ---
  output$save_preview <- renderPrint({
    cat(saved_json_state())
  })
  
}

shinyApp(ui, server)
