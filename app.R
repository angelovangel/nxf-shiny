library(shiny)
library(jsonlite)
library(bslib)
library(bsicons)
library(stringr)
library(dplyr)
library(shinyvalidate)
library(shinyFiles)
library(processx)
library(digest)
library(hover)
library(reactable)

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
  
  fillable = F,
  title = tags$span(
    tags$span(
      "NXF - Shiny",
      style = "font-size: 1.3rem; font-weight: bold; margin-left: 0em; color: #0073b7;"
    ),
    tags$span(
      #icon('align-center'),
      "Run any Nextflow pipeline using a JSON params spec file",
      tags$a(
        href = "https://github.com/angelovangel/nxf-shiny",
        target = "_blank",
        #rel="noopener noreferrer",
        bs_icon('github')
      ),
      style = "font-size: 0.9rem; font-weight: normal; margin-left: 8em; color: #0073b7;"
    )
  ),
  sidebar = sidebar,
  theme = bs_theme(bootswatch = 'yeti', primary = '#196F3D'),
  ########## controls
  tags$div(
    hover_action_button('start', 'Start', icon = icon('play'), button_animation = 'overline-reveal'),
    hover_action_button('show_session', 'Show session', icon = icon('expand'), button_animation = 'overline-reveal'),
    hover_reload_button('reset', 'Reset', icon = icon('rotate'), button_animation = 'overline-reveal'),
    hover_action_button('kill', 'Kill session', icon = icon('xmark'), style = 'color:#0073b7;', button_animation = 'overline-reveal'),
  ),
  tags$hr(),
  ########## controls
  card(
    card_header(
      id = 'header1', 
      class = 'bg-secondary', 
      tags$a('Sessions', tooltip(bsicons::bs_icon("question-circle"), 'Currently active tmux sessions'))
    ),
    max_height = 280,
    card_body(
      #reactableOutput('table')
      verbatimTextOutput('table')
    )
  ),
  card(
    card_header(
      id = 'header2', 
      class = 'bg-secondary', 
      tags$a('Session output', tooltip(bsicons::bs_icon("question-circle"), 'Output from the selected tmux session'))
    ),
    height = 450,
    card_body(
      verbatimTextOutput('stdout')
    )
  )
)

server <- function(input, output, session) {
  # check nextflow, docker and tmux are on path
  if (!bin_on_path('nextflow') | !bin_on_path('docker') | !bin_on_path('tmux')) {
    showNotification('nextflow and/or docker and/or tmux not found!', type = 'error')
  } else {
    showNotification('The server is ready!', type = 'message')
  }
  
  
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
  output$table <- renderPrint({
    cat(saved_json_state())
  })
  
}

shinyApp(ui, server)
