library(shiny)
library(jsonlite)
library(bslib)
library(stringr)

create_input <- function(params) {
  input_type <- params$type
  
  # Remove keys not relevant to the Shiny function call
  params$type <- NULL
  params$panel_condition <- NULL
  
  # This prevents arguments from being passed as lists/data.frames, which causes the match.arg error.
  params <- lapply(params, unlist)
  
  # The value for a select input's 'choices' and 'selected' might still need special handling
  # depending on how jsonlite parsed the original config, but unlist often resolves it.
  
  do.call(input_type, params)
}

# Function to wrap conditional inputs in conditionalPanel()
create_conditional_ui <- function(config) {
  lapply(config, function(params) {
    if (!is.null(params$panel_condition)) {
      # This input is conditional, wrap it in conditionalPanel
      conditionalPanel(
        condition = params$panel_condition,
        create_input(params)
      )
    } else {
      # This input is not conditional, render it directly
      create_input(params)
    }
  })
}

sidebar <- sidebar(
  tagList(
    #selectInput('pipelines', 'Pipelines', choices = list.files('pipelines'), multiple = FALSE),
    create_conditional_ui(jsonlite::fromJSON("config.json", simplifyDataFrame = FALSE)),
    uiOutput('pipeline_inputs'),
    actionButton("save_state", "Save Current Configuration to JSON"),
  )
)
ui <- page_navbar(
  sidebar = sidebar,
  theme = bs_theme(bootswatch = 'yeti', primary = '#196F3D'),
  card(
    card_header(
      'header'
    ),
    card_body(
    plotOutput("my_plot"),
    h4("Saved State JSON Preview:"),
    verbatimTextOutput("save_preview")
    ) 
  )
)

server <- function(input, output, session) {
  # render pipeline inputs based on pipeline selected
  output$pipeline_inputs <- renderUI({
    json <- read_json(path = fs::path('pipelines', input$pipelines), simplifyDataFrame = FALSE)
    create_conditional_ui(json)
  })
  
  
  # Reactive value to store the JSON string for display
  saved_json_state <- reactiveVal("")
  
  # --- Logic to save the input state ---
  observeEvent(input$save_state, {
    
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
      # take only datapath for fileInputs
      if(config_item$type == 'fileInput') {
        current_value <- isolate(input[[id]]$datapath)
      }
      
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
