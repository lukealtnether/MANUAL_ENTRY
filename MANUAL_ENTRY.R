library(shiny)
library(jsonlite)
library(shinyjs)
library(jsonvalidate)
library(readxl)
library(writexl)
library(tidyverse)
library(arsenal)


ui <-  
  fluidPage(
    useShinyjs(),
    titlePanel("Example Data Entry"),
    
    tags$style(HTML("
      #example_text {
        white-space: pre-wrap;
        word-wrap: break-word;
        max-height: 400px;
        overflow-y: auto;
        border: 1px solid #ddd;
        padding: 8px;
        background-color: #f9f9f9;
        border-radius: 4px;
      }
    ")),
    
    tags$style(HTML("
  #prompt_text {
    white-space: pre-wrap;
    word-wrap: break-word;
    border: 1px solid #ddd;
    padding: 8px;
    background-color: #f9f9f9;
    border-radius: 4px;
    max-height: 400px;
    overflow-y: auto;
  }
")),
    
    fluidRow(
      column(4, fileInput(("schema_file"), "Upload JSON Schema (.json/.txt)")),
      column(4, fileInput(("empty_examples"), "Upload Examples (.xlsx)")),
      column(4, fileInput(("prompt_text_file"), "Upload Prompt (.txt)"))
      ),
    h4("Prompt"),
    verbatimTextOutput("prompt_text"),
    tags$hr(),
    fluidRow(
      column(8,
        h4("Example Text"),
        verbatimTextOutput(("example_text")),
        fluidRow(
          column(6, actionButton(("previous_button"), "START", style = "width: 100%;", class = "btn btn-success")),
          column(6, actionButton(("next_button"), "STOP/NEXT", style = "width: 100%;", class = "btn btn-danger"))
          ),
        textOutput(("example_counter"))),
      column(4, 
        uiOutput(("dynamic_form")),
        actionButton(("add_row"), "Add Row", class = "btn btn-success"),
        actionButton(("remove_row"), "Remove Last Row", class = "btn btn-danger"),
        h4("Preview"),
        verbatimTextOutput(("data_output")))),
    tags$hr(),
    textInput(("filename_xlsx"), "Enter file name (without exteion):", value = ""),
    downloadButton(("download_xlsx"), "Download XLSX for Validation")
      )




server <- function(input, output, session) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  schema <- reactiveVal(NULL)
  raw_schema_text <- reactiveVal(NULL)
  example_data <- reactiveVal(NULL)
  example_index <- reactiveVal(1)
  timer_start <- reactiveVal(NULL)
  manual_times <- reactiveVal(numeric())  
  
  # Store a separate entries dataframe, invalid cells, and messages per example
  entries_list <- reactiveVal(list())
  invalid_cells_list <- reactiveVal(list())
  validation_messages <- reactiveVal(list())
  add_row_toggle <- reactiveVal(list())
  prompt_text <- reactiveVal("")
  
  observeEvent(input$prompt_text_file, {
    req(input$prompt_text_file)
    
    ext <- tools::file_ext(input$prompt_text_file$name)
    if (tolower(ext) != "txt") {
      showModal(modalDialog(
        title = "Invalid file type",
        "Please upload a valid .txt file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    tryCatch({
      txt <- readLines(input$prompt_text_file$datapath, warn = FALSE)
      prompt_text(paste(txt, collapse = "\n"))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error reading prompt",
        paste("An error occurred while reading the file:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  
  output$prompt_text <- renderText({
    prompt_text()
  })
  
  
  
  # input examples and json
  observeEvent(input$empty_examples, {
    req(input$empty_examples)
    
    # Ensure it's an .xlsx file
    ext <- tools::file_ext(input$empty_examples$name)
    if (tolower(ext) != "xlsx") {
      showModal(modalDialog(
        title = "Invalid file type",
        "Please upload a valid .xlsx file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    # Try reading the file safely
    tryCatch({
      df <- readxl::read_excel(input$empty_examples$datapath, col_names = FALSE)
      
      if (ncol(df) >= 1) {
        example_data(df[[1]])
        example_index(1)
        manual_times(numeric(length(df[[1]])))
      } else {
        showModal(modalDialog(
          title = "Invalid file content",
          "The file appears to be empty or malformed.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error reading file",
        paste("An error occurred while reading the file:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  observeEvent(input$schema_file, {
    req(input$schema_file)
    
    # Ensure file has a .json extension
    ext <- tools::file_ext(input$schema_file$name)
    if (tolower(ext) != "json" & tolower(ext) != "txt") {
      showModal(modalDialog(
        title = "Invalid file type",
        "Please upload a valid .json schema file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    # Try reading and parsing the JSON
    tryCatch({
      raw_json <- readLines(input$schema_file$datapath, warn = FALSE)
      json <- jsonlite::fromJSON(input$schema_file$datapath, simplifyVector = FALSE)
      
      # Save the parsed schema
      schema(json)
      raw_schema_text(raw_json)
      set_add_row_toggle(1)
      
      # Clear stored entries
      entries_list(list())
      validation_messages(list())
      invalid_cells_list(list())
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error parsing JSON",
        paste("An error occurred while parsing the schema:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # Helper functions for storing validation and example data
  get_entries <- function() {
    idx <- as.character(example_index())
    entries_list()[[idx]] %||% list()
  }
  
  set_entries <- function(val) {
    idx <- as.character(example_index())
    current <- entries_list()
    current[[idx]] <- val
    entries_list(current)
  }
  
  get_invalid_cells <- function() {
    idx <- as.character(example_index())
    invalid_cells_list()[[idx]] %||% data.frame(row = integer(), column = character())
  }
  
  set_invalid_cells <- function(val) {
    idx <- as.character(example_index())
    current <- invalid_cells_list()
    current[[idx]] <- val
    invalid_cells_list(current)
  }
  
  get_validation_message <- function() {
    idx <- as.character(example_index())
    validation_messages()[[idx]] %||% ""
  }
  
  set_validation_message <- function(val) {
    idx <- as.character(example_index())
    current <- validation_messages()
    current[[idx]] <- val
    validation_messages(current)
  }
  
  # Navigation buttons
  observeEvent(input$previous_button, {
    req(example_data())
    i <- example_index()
    
    # START timing
    timer_start(Sys.time())
    
    # Just display the current example again (reset)
    example_index(i)
  })
  
  observeEvent(input$next_button, {
    req(example_data())
    i <- example_index()
    
    # Stop timer and record
    start_time <- timer_start()
    if (!is.null(start_time)) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      times <- manual_times()
      times[i] <- elapsed
      manual_times(times)
    }
    
    # Advance to next example
    if (i < length(example_data())) {
      example_index(i + 1)
    }
    
    # Reset timer and hide text
    timer_start(NULL)
  })
  
  
  # Show current example text and counter
  output$example_text <- renderText({
    req(example_data())
    if (is.null(timer_start())) return("")  
    example_data()[example_index()]
  })
  
  output$example_counter <- renderText({
    req(example_data())
    paste("Row", example_index(), "of", length(example_data()))
  })
  
  
  
  is_array_schema <- function(sch) {
    !is.null(sch$properties$data$type) && sch$properties$data$type == "array"
  }
  
  get_schema_properties <- function(sch) {
    # Check for data block
    if (!is.null(sch$properties$data$items$properties)) {
      return(sch$properties$data$items$properties)
    } else if (!is.null(sch$type) && sch$type == "array") {
      return(sch$items$properties)
    } else {
      return(sch$properties$data$properties)
    }
  }
  
  get_property_types <- function(schema_props) {
    extract_type <- function(field) {
      if (!is.null(field$type)) {
        return(field$type)
      } else if (!is.null(field$anyOf)) {
        types <- vapply(field$anyOf, function(x) x$type %||% NA_character_, character(1))
        primary <- types[!is.na(types) & types != "null"]
        if (length(primary) > 0) return(primary[1])
      }
      return("string")
    }
    sapply(schema_props, extract_type)
  }
  
  generateFormInputs <- function(schema_props, prefix = "") {
    lapply(names(schema_props), function(prop_name) {
      field <- schema_props[[prop_name]]
      input_id <- paste0(prefix, prop_name)
      label <- tags$b(prop_name)
      null_id <- paste0(input_id, "_null")
      
      allows_null <- !is.null(field$anyOf) && any(vapply(
        field$anyOf, function(x) is.list(x) && identical(x$type, "null"), logical(1)
      ))
      
      enum_choices <- NULL
      if (!is.null(field$enum)) {
        enum_choices <- field$enum
      } else if (!is.null(field$anyOf)) {
        enum_choices <- unlist(lapply(field$anyOf, function(x) x$enum), use.names = FALSE)
        enum_choices <- unique(enum_choices[!is.na(enum_choices) & enum_choices != "null"])
      }
      
      input_ui <- if (!is.null(enum_choices) && length(enum_choices) > 0) {
        selectInput(input_id, NULL, choices = c("", enum_choices))
      } else if (!is.null(field$pattern)) {
        textInput(input_id, NULL)
      } else {
        textInput(input_id, NULL)
      }
      
      conditional_input <- if (allows_null) {
        tagList(
          checkboxInput(null_id, "Null", value = FALSE),
          conditionalPanel(
            condition = paste0("!input['", null_id, "']"),
            input_ui
          )
        )
      } else {
        input_ui
      }
      
      tagList(label, conditional_input)
    })
  }
  
  output$dynamic_form <- renderUI({
    req(schema())
    props <- get_schema_properties(schema())
    do.call(tagList, generateFormInputs(props, prefix = "row_"))
  })
  
  observeEvent(input$add_row, {
    req(schema())
    
    # Prevent adding if it's an object and one row already exists
    if (!is_array_schema(schema()) && get_add_row_toggle() == 0) return(NULL)
    
    props <- names(get_schema_properties(schema()))
    row <- list()
    
    for (p in props) {
      input_id <- paste0("row_", p)
      null_check <- input[[paste0(input_id, "_null")]]
      val <- if (!is.null(null_check) && null_check) NA else input[[input_id]]
      row[[p]] <- val
    }
    
    cur_entries <- get_entries()
    if (is_array_schema(schema())) {
      set_entries(append(cur_entries, list(row)))
    } else {
      set_entries(list(row))
      set_add_row_toggle(0)  # Only allow one row for object-type
    }
    
    # Validation
    json_out <- reactive_json_output()
    if (!is.null(json_out)) {
      validator <- jsonvalidate::json_schema$new(input$schema_file$datapath)
      valid <- validator$validate(json_out, verbose = TRUE)
      
      if (isTRUE(valid)) {
        set_validation_message("✅ Valid")
        set_invalid_cells(data.frame(row = integer(), column = character()))
      } else {
        errors <- attr(valid, "errors")
        error_info <- data.frame(
          row = sapply(errors$instancePath, function(path) {
            idx <- as.integer(stringr::str_extract(path, "(?<=/)[0-9]+"))
            if (is.na(idx)) 1 else idx + 1
          }),
          column = sapply(errors$instancePath, function(path) {
            parts <- unlist(strsplit(path, "/"))
            tail(parts, 1)
          }),
          stringsAsFactors = FALSE
        ) |> dplyr::distinct()
        
        set_invalid_cells(error_info)
        set_validation_message("❌ Invalid")
      }
    }
  })
  
  get_add_row_toggle <- function() {
    idx <- as.character(example_index())
    add_row_toggle()[[idx]] %||% 1  
  }
  
  set_add_row_toggle <- function(val) {
    idx <- as.character(example_index())
    current <- add_row_toggle()
    current[[idx]] <- val
    add_row_toggle(current)
  }
  
  observeEvent(input$remove_row, {
    cur <- get_entries()
    if (is_array_schema(schema())) {
      if (length(cur) > 0) set_entries(cur[-length(cur)])
    } else {
      set_entries(list())
      set_add_row_toggle(1)  
    }
  })
  
  output$data_output <- renderPrint({
    rows <- get_entries()
    if (length(rows) == 0) return(NULL)
    
    df <- as.data.frame(do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE))))
    errors <- get_invalid_cells()
    
    if (nrow(errors) > 0) {
      for (i in seq_len(nrow(errors))) {
        row <- errors$row[i]
        col <- errors$column[i]
        if (!is.null(df[[col]]) && row <= nrow(df)) {
          val <- df[row, col]
          df[row, col] <- paste0("?", val, "?")
        }
      }
    }
    
    cat(get_validation_message(), "\n\n")
    
    # Show detailed errors if invalid
    if (get_validation_message() == "❌ Invalid") {
      valid <- jsonvalidate::json_schema$new(input$schema_file$datapath)$validate(
        reactive_json_output(), verbose = TRUE
      )
      errors <- attr(valid, "errors")
      
      if (!is.null(errors) && nrow(errors) > 0) {
        for (i in seq_len(nrow(errors))) {
          cat(paste0("• [", errors$instancePath[i], "] ", errors$message[i]), "\n")
        }
        cat("\n")
      }
    }
    
    print(df)
  })
  
  reactive_json_output <- reactive({
    req(schema())
    rows <- get_entries()
    if (length(rows) == 0) return(NULL)
    
    props <- get_schema_properties(schema())
    types <- get_property_types(props)
    
    coerce_row <- function(row) {
      for (name in names(row)) {
        val <- row[[name]]
        if (is.null(val) || is.na(val)) next
        if (types[[name]] == "integer") {
          row[[name]] <- as.integer(val)
        } else if (types[[name]] == "number") {
          row[[name]] <- as.numeric(val)
        } else {
          row[[name]] <- as.character(val)
        }
      }
      row
    }
    
    coerced_rows <- lapply(rows, coerce_row)
    
    if (is_array_schema(schema())) {
      full_object <- list(data = coerced_rows)
    } else {
      full_object <- list(data = coerced_rows[[1]])
    }
    
    jsonlite::toJSON(full_object, auto_unbox = TRUE, pretty = TRUE)
  })
  
  get_full_output <- function() {
    ex <- example_data()
    times <- manual_times()
    if (is.null(ex)) return(NULL)
    
    idxs <- seq_along(ex)
    data_entries <- entries_list()
    
    data_list <- lapply(idxs, function(i) {
      row_entries <- data_entries[[as.character(i)]] %||% list()
      if (length(row_entries) == 0) return(NULL)
      
      # Convert list-of-rows to dataframe, preserving types
      df <- as.data.frame(do.call(rbind, lapply(row_entries, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      })))
      
      # Convert column types based on schema
      sch <- schema()
      props <- get_schema_properties(sch)
      types <- get_property_types(props)
      
      for (col in names(df)) {
        t <- types[[col]] %||% "string"
        if (t == "integer") df[[col]] <- as.integer(df[[col]])
        else if (t == "number") df[[col]] <- as.numeric(df[[col]])
        else df[[col]] <- as.character(df[[col]])
      }
      
      df
    })
    
    tibble::tibble(
      examples = ex,
      manual_time = times,
      data = data_list
    )
  }
  
  observe({
    toggleState("previous_button", is.null(timer_start()))
    toggleState("next_button", !is.null(timer_start()))
  })
  
  
  output$download_xlsx <- downloadHandler(
    filename = function() {
      name <- input$filename_xlsx
      if (name == "") name <- "preview"
      paste0(name, ".xlsx")
    },
    content = function(file) {
      output_data <- get_full_output()
      if (is.null(output_data)) return()
      
      flat_df <- dplyr::bind_rows(lapply(seq_len(nrow(output_data)), function(i) {
        ex <- output_data$examples[i]
        t <- output_data$manual_time[i]
        dat <- output_data$data[[i]]
        if (is.null(dat)) {
          out <- data.frame(examples = ex, manual_time = t, stringsAsFactors = FALSE)
        } else {
          out <- cbind(examples = ex, manual_time = t, dat)
        }
        out
      }))
      
      
      writexl::write_xlsx(flat_df, file)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
