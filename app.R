library(shiny)
library(jsonlite)
library(bslib)
library(bsicons)
library(stringr)
library(dplyr)
library(shinyvalidate)
library(shinyFiles)
library(shinyjs)
library(processx)
library(digest)
library(hover)
library(reactable)
library(prettyunits)

source('global.R')

sidebar <- sidebar(
  #title = '',
  tagList(
    tags$div(
      #style = "background-color:powderblue;",
      #This create the dropdown list with available pipelines, the config.json is maintained manually
      create_conditional_ui(jsonlite::fromJSON("config.json", simplifyDataFrame = FALSE)),
      uiOutput('pipeline_url')
    ),

    # these are the inputs loaded from the corresponding pipelines/json file
    uiOutput('pipeline_inputs'),
  )
)

card1 <- card(
  card_header(
    id = 'header1', 
    class = 'bg-secondary', 
    tags$a('Sessions', tooltip(bsicons::bs_icon("question-circle"), 'Currently active tmux sessions'))
  ),
  max_height = 200,
  card_body(
    reactableOutput('table')
  )
)

card2 <-  card(
  card_header(
    id = 'header2', 
    class = 'bg-secondary', 
    tags$a('Session output', tooltip(bsicons::bs_icon("question-circle"), 'Output from the selected tmux session'))
  ),
  max_height = 450,
  card_body(
    verbatimTextOutput('stdout')
  )
)
  
ui <- page_navbar(
  use_hover(),
  useShinyjs(),
  
  # Inject custom CSS here to fix stdout opacity issue
  tags$head(
    tags$style(
      HTML("
        #stdout { 
          color: #212529 !important; /* Force a dark text color (e.g., a standard dark Bootstrap color) */
          opacity: 1 !important;    /* Force full opacity */
        }
      ")
    )
  ),
  
  
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
    hover_action_button('start', 'Start', icon = icon('play'), button_animation = 'overline-reveal'), #style = "height: 15px;"),
    hover_action_button('show_session', 'Show session', icon = icon('expand'), button_animation = 'overline-reveal'),
    hover_reload_button('reset', 'Reset', icon = icon('rotate'), button_animation = 'overline-reveal'),
    hover_action_button('kill', 'Kill session', icon = icon('xmark'), style = 'color:#0073b7;', button_animation = 'overline-reveal'),
  ),
  ########## controls
  tags$hr(),
  card1, card2
)

server <- function(input, output, session) {
  # tmux session template df
  empty_df <- data.frame(
    session_id = NA,
    pipeline = NA,
    started = NA,
    runtime = NA,
    pipeline_runtime = NA,
    status = NA,
    results = NA
  )
  
  # check nextflow, docker and tmux are on path
  if (!bin_on_path('nextflow') | !bin_on_path('docker') | !bin_on_path('tmux')) {
    showNotification('nextflow and/or docker and/or tmux not found!', type = 'error')
  } else {
    showNotification('The server is ready!', type = 'message')
  }
  
  
  # currently selected pipeline
  json <- reactive({
    read_json(path = fs::path('pipelines', input$pipelines), simplifyDataFrame = FALSE)
  })
  
  # render pipeline inputs based on pipeline selected
  output$pipeline_inputs <- renderUI({
    create_conditional_ui(json()$shiny_inputs)
  })
  
  output$pipeline_url <- renderUI({
    HTML(
      paste0("<a href='", json()$url, "'target='_blank'>Pipeline repository</a>")
    )
  })
  
  # special case shinyFiles - shinyDirChoose bindings in server
  #shinyDirChoose(input, 'fastq', root=c(root=Sys.getenv('HOME')))
  observeEvent(input$pipelines, {
    bind_shinyfiles(input = input, config = json()$shiny_inputs)
  })
  
  
  # Validations
  iv <- InputValidator$new()
  
  observe({

    lapply(json()$shiny_inputs, function(p){
      if (p$required) {
        iv$add_rule(p$inputId, sv_required(message = 'Parameter is required!'))
      }
    })
  })
  
  # NXF log
  nxflog <- tempfile(fileext = ".csv")
  reactive({
    invalidateLater(3000, session = session)
    write_nxf_status(nxflog)
  })
  
  # Monitor tmux sessions ##################################
  tmux_sessions <- reactive({
    invalidateLater(3000, session)
    oldw <- getOption("warn")
    options(warn = -1)
    tmuxinfo <- system2("bin/tmux-info.sh", stdout = TRUE, stderr = TRUE)
    ### write nxf log
    write_nxf_status(nxflog)
    nxf_info <- read.csv(nxflog, header = T, check.names = T)
    #################
    options(warn = oldw)
    
    if (any(str_detect(tmuxinfo, 'no server|error'))) {
      empty_df
    } else {
      df <- data.frame(
        session_id = str_split_i(tmuxinfo, " ", 2),
        pipeline = NA,
        started = str_split_i(tmuxinfo, " ", 1) %>% as.numeric() %>% as.POSIXct(),
        runtime = NA,
        pipeline_runtime = NA,
        status = NA,
        results = NA
      )
      
      # add status etc from nxflog
      # matching between tmux session id and nextflow run is the session id which is found in the COMMAND column
      df$status <- sapply(df$session_id, function(x) {
        nxf_run_index <- which(str_detect(string = nxf_info$COMMAND, pattern = paste0(x, '$')))
        status_info <- nxf_info[nxf_run_index[1], ]$STATUS
        # just formatting status
        if (is.na(status_info)) {
          status_info <- 'NA'
        }
        if (length(status_info) == 0) {
          "<a style='color:orange';> STARTING </a>"
        } else if (str_trim(status_info) == "-") {
          "<a style='color:orange;'> RUNNING </a>"
        } else if (str_trim(status_info) == "OK") {
          #"OK"
          "<a style='color:green;'> OK </a>"
        } else if(str_trim(status_info) == "ERR") {
          "<a style='color:red;'> ERR </a>"
        } else {
          status_info
        }
      })
      df$status <- as.character(df$status) # avoid a warning if status is not atomic
      df$pipeline_runtime = sapply(df$session_id, function(x) { nxf_info[str_detect(nxf_info$COMMAND, x), ]$DURATION })
      df$pipeline = sapply(df$session_id, function(x) {
        command <- nxf_info[str_detect(nxf_info$COMMAND, x), ]$COMMAND
        str_extract(command, "(?<=nextflow run\\s)\\S+") 
      }) 
      df <- df %>% mutate(
        status = ifelse(str_detect(status, "-"), "RUNNING", status),
        runtime = prettyunits::pretty_dt(difftime(Sys.time(), started), compact = T)
      ) %>%
        arrange(started)
      
      # Add direct download link if tarball exists
      df$results <- vapply(df$session_id, function(id) {
        tar_name <- paste0(id, ".tar.gz")
        tar_path <- file.path("www", tar_name)
        if (!is.na(id) && file.exists(tar_path)) {
          paste0('<a href="', tar_name, '" download>Download ', id, '</a>')
        } else {
          "NA"
        }
      }, character(1))
    df
    }
    
  })
  # Monitor tmux sessions ##################################
  
  # row and session selected  ##################################
  row_sel <- reactive({
    getReactableState('table', 'selected')
  })
  row_selected <- row_sel %>% debounce(1000)
  
  session_selected <- reactive({
    tmux_sessions()[row_selected(), ]$session_id
  })
  ##################################
  
  # Render and update tmux sessions table 
  ##################################
  output$table <- renderReactable({
    reactable(
      empty_df,
      #tmux_sessions(),
      pagination = FALSE, highlight = TRUE, height = 150, compact = T, fullWidth = T, 
      selection = 'single', onClick = 'select', #defaultSelected = 1,
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #0073b7;")
      ),
      style = list(fontSize = '90%'),
      columns = list(
        started = colDef(format = colFormat(datetime = T, locales = 'en-GB'), minWidth = 150),
        pipeline = colDef(minWidth = 200),
        results = colDef(html = TRUE),
        status = colDef(html = TRUE, minWidth = 50)
      )
    )
  })
  
  observe({
    updateReactable(
      'table', 
      data = tmux_sessions(), 
      selected = row_selected(), 
      session = session
    )
  })
  ##################################
  
  # tar whnen ready and place in www
  ##################################
  # decide if ready by looking at the nxflog (OK for finished)
  observe({
    df <- tmux_sessions()
    for (id in df$session_id) {
      if (!is.na(id) && pipeline_finished(df = df, id = id)) {
        #if ( !is.na(id) && (df[df$session_id == id, ]$status == 'OK') ) {
        tar_path <- file.path("www", paste0(id, ".tar.gz"))
        outdir <- file.path("output", id)
        if (!file.exists(tar_path) && dir.exists(outdir)) {
          # Create tarball in www folder
          system2("tar", args = c("-czf", tar_path, "-C", "output", id))
        }
      }
    }
  })
  ##################################
  
  # Reactive value to store the JSON string for display
  saved_json_state <- reactiveVal("")
  # --- Logic to save the input state ---
  observeEvent(input$start, {
    
    session_id <- digest(runif(1), algo = 'crc32')
    
    #iv$enable()
    # # Check if all inputs are valid before proceeding
    # if (!iv$is_valid()) {
    #   showNotification("Please correct the required fields!", type = "warning")
    #   # Stop the rest of the execution
    #   return() 
    # }
    
    # Collect inputs to build -params-file json
    ############################################
    # 1. Initialize an empty list to store the final state
    final_state_list <- list()
    
    # 2. Iterate through the original configuration items
    for (config_item in json()$shiny_inputs) {
      
      id <- config_item$inputId
      
      # 3. Retrieve the current value from the 'input' object
      # Use isolate() to ensure this code only runs when the button is clicked,
      # not when the input values change.
      current_value <- isolate(input[[id]]) 
      
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
      
      # 4. Construct the output object for this input, note setNames
      output_item <- setNames(list(id = current_value),id)
      
      # 5. Append to the final list
      final_state_list <- append(final_state_list, output_item)
    }
    
    # 6. Convert the list to a formatted JSON string
    json_output <- jsonlite::toJSON(
      final_state_list, 
      pretty = TRUE, 
      auto_unbox = TRUE, na = "string", null = 'null'
    )
    
    # 7. Write the JSON string to a file (e.g., 'saved_state.json')
    json_to_write <- str_remove_all(json_output, "^\\[|\\]$")
    
    saved_params_file <- tempfile(fileext = ".json")
    write(json_to_write, saved_params_file)
    
    # Update the reactive value for display
    saved_json_state(json_output)
    
    showNotification("Configuration state saved to 'saved_state.json'!", type = "message")
    ############################################
    
    # Execute pipeline
    # 1. Launch new clean! tmux session
    args1 <- c('new', '-d', '-s', session_id, '-x', '120', '-y', '30', "'bash --login'") # add 'bash --login' to prevent R from inheriting from previous tmux sessions?
    system2('tmux', args = args1)
    
    # 2. Start pipeline in new session
    tmux_command <- paste(
      'nextflow', 'run', json()$fullname,
      '-params-file', saved_params_file,
      '-o', file.path('output', session_id),
      '-w', file.path('work', session_id),
      sep = ' '
    )
    
    tmux_command <- paste0("'", tmux_command, "'") # wrap the whole command in single quotes
    args2 <- c('send-keys', '-t', session_id, tmux_command, 'C-m')
    system2('tmux', args = args2)
    
    showNotification(ui = paste0('Started session: ', session_id), type = 'message')
  })
  
  # Show session
  observeEvent(input$show_session, {
    withCallingHandlers({
      shinyjs::html(id = "stdout", "")
      #args <- paste0(' a', ' -t ', session_selected)
      args <- c('capture-pane', '-S', '-', '-E', '-', '-pt', session_selected())
      
      p <- processx::run(
        'tmux', args = args,
        stdout_callback = function(line, proc) {message(line)},
        #stdout_line_callback = function(line, proc) {message(line)},
        stderr_to_stdout = TRUE,
        error_on_status = FALSE
      )
    },
    message = function(m) {
      shinyjs::html(id = 'stdout', html = paste0('Session: ', session_selected(), '\n'), add = T);
      shinyjs::html(id = "stdout", html = m$message, add = T);
      #runjs("document.getElementById('stdout').parentElement.scrollTo(0,1e9);")
      runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });")
    }
    )
  })
  
}

shinyApp(ui, server)
