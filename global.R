
# for one parameter defined in the json file(and passed as a list after reading), construct the Shiny input object
create_input <- function(param) {
  input_type <- param$type
  
  # Remove keys not relevant to the Shiny function call, these are documented in the README.md
  param$type <- NULL
  param$panel_condition <- NULL
  param$required <- NULL
  
  # This prevents arguments from being passed as lists/data.frames, which causes the match.arg error.
  param <- lapply(param, unlist)
  
  # The value for a select input's 'choices' and 'selected' might still need special handling
  # depending on how jsonlite parsed the original config, but unlist often resolves it.
  
  do.call(input_type, param)
}

# Function to wrap conditional inputs in conditionalPanel() and use lapply to process all json params defined
create_conditional_ui <- function(config) {
  lapply(config, function(p) {
    if (!is.null(p$panel_condition)) {
      # This input is conditional, wrap it in conditionalPanel
      conditionalPanel(
        condition = p$panel_condition,
        create_input(p)
      )
    } else {
      # This input is not conditional, render it directly
      create_input(p)
    }
  })
}



