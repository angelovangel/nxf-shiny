
# return nextflow log table for a nextflow log in a specific folder
nxf_log <- function(path) {
  
  log <- processx::run('nextflow', 'log', wd = path, error_on_status = F)
  if (log$status == 0) {
    read.table(text = log$stdout, header = T, sep = '\t')
  } else {
    data.frame(
      TIMESTAMP = NA,
      DURATION = NA,
      RUN.NAME = NA,
      STATUS = NA,
      REVISION.ID = NA,
      SESSION.ID = NA,
      COMMAND = NA
    )
  }
  
}



bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

# for one parameter defined in the json file(and passed as a list after reading), construct the Shiny input object
create_input <- function(param) {
  input_type <- param$type
  
  # add help icons to label
  # tags$a('Sessions', tooltip(bsicons::bs_icon("question-circle"), 'Currently active tmux sessions'))
  if (!is.null(param$help)) {
    label <- tags$a(
        param$label, 
        tooltip(
          bsicons::bs_icon("question-circle"),
          param$help,
          placement = "right")
      )
    
    param$label <- HTML(as.character(label))
    #param$label <- "Test"
  }
  
  # This prevents arguments from being passed as lists/data.frames, which causes the match.arg error.
  # param <- lapply(param, unlist)
  # The value for a select input's 'choices' and 'selected' might still need special handling
  # depending on how jsonlite parsed the original config, but unlist often resolves it.
  
  # Remove keys not relevant to the Shiny function call, these are documented in the README.md
  param$type <- NULL
  param$panel_condition <- NULL
  param$required <- NULL
  param$help <- NULL
  
  # make shiny input
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

# in case there are shinyFiles params, make the required bindings in server
# that is shinyDirChoose(input, 'fastq_pass', root=c(root=Sys.getenv('HOME')), session = session)

bind_shinyfiles <- function(config, input) {
  # which params are shinyDirButton or shinyFilesButton, get their id and return shinyDirChoose(input, id, ...)
  lapply(config, function(p){
    if (str_detect(string = p$type, pattern = "shinyDirButton")) {
      shinyDirChoose(input, p$inputId, roots = c(wd = "/"), allowDirCreate = FALSE)
    } else if (str_detect(string = p$type, pattern = "shinyFilesButton")) {
      shinyFileChoose(input, p$inputId, roots = c(wd = "/"))
    } else {
      return()
    }
  })
}

# Helper to check if pipeline is finished, based on the tmux_sessions() df
pipeline_finished <- function(id, df) {
  status_val <- df[df$session_id == id, ]$status
  if (length(status_val) > 0 && !is.na(status_val) && str_detect(status_val, 'OK')) {
    TRUE
  } else {
    FALSE
  }
  #file.exists(file.path("output", session_id, "00-sample-status-summary.html"))
}

