library(shiny)
library(jsonlite)
library(bslib)

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
    create_conditional_ui(jsonlite::fromJSON("conditional-config.json", simplifyDataFrame = FALSE)),
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
  
  # Load the original configuration file once (using a reactiveVal or simple variable)
  config_data_list <- jsonlite::fromJSON("conditional-config.json", simplifyDataFrame = FALSE)
  
  # Reactive value to store the JSON string for display
  saved_json_state <- reactiveVal("")
  
  # --- Logic to save the input state ---
  observeEvent(input$save_state, {
    
    # 1. Initialize an empty list to store the final state
    final_state_list <- list()
    
    # 2. Iterate through the original configuration items
    for (config_item in config_data_list) {
      
      id <- config_item$inputId
      
      # 3. Retrieve the current value from the 'input' object
      # Use isolate() to ensure this code only runs when the button is clicked,
      # not when the input values change.
      current_value <- isolate(input[[id]]) 
      
      # 4. Construct the output object for this input
      output_item <- list(
        inputId = id,
        type = config_item$type,
        user_value = current_value
      )
      
      # 5. Append to the final list
      final_state_list <- append(final_state_list, list(output_item))
    }
    
    # 6. Convert the list to a formatted JSON string
    json_output <- jsonlite::toJSON(
      final_state_list, 
      pretty = TRUE, 
      auto_unbox = TRUE
    )
    
    # 7. Write the JSON string to a file (e.g., 'saved_state.json')
    write(json_output, "saved_state.json")
    
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
