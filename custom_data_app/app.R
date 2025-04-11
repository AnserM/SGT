library("shiny")
library("Rsamtools")
library("GenomicRanges")
library("VariantAnnotation")
library("tidyverse")
library("dplyr")
library("DT")
library("openxlsx")
library("shinyjs")
library("shinyWidgets")
library("shinyFiles")





template_positions <- c(
  "Chr03,45301350,Glyma.03G258700,Light_Tawny,Td,td_W177*",
  "Chr03,45301275,Glyma.03G258700,Light_Tawny,Td,td_S202R"
)


# Convert predefined positions into a template data frame
template_data <- do.call(rbind, lapply(template_positions, function(x) {
  pos <- strsplit(x, ",")[[1]]
  data.frame(
    Chromosome = pos[1],
    Position = as.numeric(pos[2]),
    Gene_ID = pos[3],
    Locus_Name = pos[4],
    Ref = pos[5],
    Alt = pos[6],
    stringsAsFactors = FALSE
  )
}))

predefined_df <- read.csv("predefined_positions.csv", header = TRUE, stringsAsFactors = FALSE)
position_choices <- split(
  apply(predefined_df[, -1], 1, paste, collapse = ","),  # collapse remaining columns into one string
  predefined_df$Category
)


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color: 	#FFFFFF; /* Light background color */
      }
      .btn {
        background-color: #fff;
        color: black;
        border: none;
        border-radius: 5px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 14px;
        transition-duration: 0.4s;
        cursor: pointer;
      }
      .btn:hover {
        background-color: #fff;
        color: black;
        border: 2px solid #909090;
      }
      .center-title {
        text-align: center;
        margin-bottom: 30px;
        background-color: #01796F;
        color: white;
        padding: 20px;
        border-radius: 10px;
      }
      .sidebar {
        background-color: #fff;
        padding: 15px;
        border-radius: 10px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      .main-panel {
        background-color: #fff;
      }
    "))
  ),
  div(class = "center-title",
      titlePanel("Genotyping Tool")),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      #tags$h4(tags$b("Gmax genome version Wm82.a2.v1"), style = "text-align: center;"),
      #hr(),
      ############### for custom datasets ##################
      tags$h4("Path to directory containing gzipped and indexed VCF files"),
      shinyDirButton("vcf_dir", "Please select a directory", "Please select a folder"),
      verbatimTextOutput("dir_path"),
      ######################################################
      hr(),
      
      tags$h4("Pre-Determined Positions"),
      
      pickerInput(
        inputId = "predefined_positions",
        label = "Select Positions",
        choices = position_choices,
        options = list(`actions-box` = TRUE),
        selected = NULL,
        multiple = TRUE),
      
      
      tags$h4("Upload Custom Positions"),
      
      fileInput("custom_positions_file", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      actionButton("clear_file", "Clear File"),
      
      downloadButton("download_template", "Download Template CSV"),
      
      tags$h4("Add Custom Positions"),
      
      
      # Container for dynamic input rows
      tags$div(id = "input_rows",
               uiOutput("dynamic_inputs")  # Use uiOutput to render dynamic inputs
      ),
      
      actionButton("add_row", "Add Row", icon = icon("plus")),
      
      hr(),
      
      actionButton("submit", "Submit", icon = icon("paper-plane")),
      actionButton("display", "Display", disabled = TRUE),
      downloadButton("download_excel", "Download Excel*", disabled = TRUE),
      hr(),
      uiOutput("position_error")
    ),
    
    mainPanel(
      tags$div(
        id = "loading-message",
        "Preparing the output, please wait...",
        style = "font-weight: bold; font-size: 20px; color: red; display: none;"
      ),
      width = 6,
      tabsetPanel(
        tabPanel("Found Positions", DTOutput("data_table")),  # Tab for found positions
        tabPanel("Unfound Positions", DTOutput("unfound_positions_table"))  # Tab for unfound positions
      )
    )
  )
)

server <- function(input, output, session) {
  
  shinyjs::hide("loading-message")
  
  
  ######################### For Custom Datasets #########################
  #choose directory
  volumes <- c("DataFolder" = normalizePath("../data"))
  shinyDirChoose(input, "vcf_dir", roots = volumes, session = session)
  
  vcf_directory <- reactive({
    req(input$vcf_dir)
    parseDirPath(volumes, input$vcf_dir)
  })
  
  output$dir_path <- renderPrint({
    vcf_directory()
  })
  
  #######################################################################
  
  
  input_count <- reactiveVal(1)
  
  observeEvent(input$add_row, {
    new_count <- input_count() + 1
    input_count(new_count)
    
    insertUI(
      selector = "#input_rows",
      ui = fluidRow(
        id = paste0("row_", new_count),
        column(2, textInput(paste0("chromosome_", new_count), "Chromosome", "")),
        column(2, numericInput(paste0("position_", new_count), "Position", value = NULL, min = 1)),
        column(2, textInput(paste0("gene_id_", new_count), "Gene ID", "")),
        column(2, textInput(paste0("locus_name_", new_count), "Locus Name", "")),
        column(2, textInput(paste0("ref_allele_", new_count), "Ref", "")),
        column(2, textInput(paste0("alt_allele_", new_count), "Alt", "")),
        column(1, actionButton(paste0("delete_row_", new_count), "️Trash", icon = icon("trash"), style = "color: red;"))
      )
    )
  })
  
  # Observer to handle delete button clicks
  observe({
    lapply(1:input_count(), function(i) {
      local({
        row_id <- i
        observeEvent(input[[paste0("delete_row_", row_id)]], {
          removeUI(selector = paste0("#row_", row_id))
          
          # Update the input count
          new_count <- input_count() - 1
          input_count(new_count)
        })
      })
    })
  })
  
  custom_file_df <- reactiveVal(NULL)
  
  # Observe the file input to process the uploaded file
  observeEvent(input$custom_positions_file, {
    req(input$custom_positions_file)  # Ensure the file input is not NULL
    
    # Read the file data
    file_data <- read.csv(input$custom_positions_file$datapath, 
                          header = TRUE, 
                          stringsAsFactors = FALSE, 
                          colClasses = "character")
    file_data$Position <- as.numeric(file_data$Position)
    
    # Validate the required columns
    if (all(c("Chromosome", "Position", "Gene_ID", "Locus_Name", "Ref", "Alt") %in% names(file_data))) {
      custom_file_df(file_data)  # Store the data in the reactiveVal if valid
    } else {
      showNotification("Uploaded file must contain the required columns: Chromosome, Position, Gene_ID, Locus_Name, Ref, Alt", type = "error")
      custom_file_df(NULL)  # Clear the data if validation fails
    }
  })
  
  observeEvent(input$clear_file, {
    custom_file_df(NULL)  # Clear the custom file data when the 'Clear' button is pressed
    showNotification("Custom file data has been cleared", type = "message")  # Optionally show a message
    reset("custom_positions_file")
  })
  
  
  positions <- reactive({
    
    print(input$predefined_positions)
    
    ################### for custom datasets ######################
    # Initialize a data frame with selected predefined positions
    parsed <- strsplit(input$predefined_positions, ",")
    
    rows <- lapply(parsed, function(x) {
      x <- trimws(x)
      
      if (length(x) == 6) {
        data.frame(
          Chromosome = x[1],
          Position = as.numeric(x[2]),
          Gene_ID = x[3],
          Locus_Name = x[4],
          Ref = x[5],
          Alt = x[6],
          stringsAsFactors = FALSE
        )
      } else {
        warning(sprintf("Skipping malformed input: %s", paste(x, collapse = ",")))
        NULL
      }
    })
    
    # Combine rows into one data frame
    predefined_positions_df <- do.call(rbind, rows[!sapply(rows, is.null)])
    ##############################################################
    # Combine predefined positions with custom positions
    custom_positions_df <- data.frame(
      Chromosome = unlist(lapply(1:input_count(), function(i) input[[paste0("chromosome_", i)]])),
      Position = unlist(lapply(1:input_count(), function(i) input[[paste0("position_", i)]])),
      Gene_ID = unlist(lapply(1:input_count(), function(i) input[[paste0("gene_id_", i)]])),
      Locus_Name = unlist(lapply(1:input_count(), function(i) input[[paste0("locus_name_", i)]])),
      Ref = unlist(lapply(1:input_count(), function(i) input[[paste0("ref_allele_", i)]])),
      Alt = unlist(lapply(1:input_count(), function(i) input[[paste0("alt_allele_", i)]])),
      stringsAsFactors = FALSE
    )
    
    
    if (!is.null(custom_file_df())) {
      custom_positions_df <- rbind(custom_positions_df, custom_file_df())
    }
    
    # Combine both data frames
    combined_positions <- bind_rows(predefined_positions_df, custom_positions_df)
    return(combined_positions)
  })
  
  # Download handler for the template CSV
  output$download_template <- downloadHandler(
    filename = function() {
      paste("template_positions", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(template_data, file, row.names = FALSE)
    }
  )
  
  
  output$meta_data <- downloadHandler(
    filename = function() {
      paste("Soy2939_Metadata", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(soy_metadata, file, row.names = FALSE)
    }
  )
  
  results_list <- reactiveVal(data.frame())
  
  observeEvent(input$submit, {
    shinyjs::show("loading-message")
    on.exit(shinyjs::hide("loading-message"))
    
    shinyjs::disable("download_excel")
    
    req(positions())
    
    pos_data <- positions()
    
    
    # Convert Position to character and check numeric
    positions_char <- as.character(pos_data$Position)
    pos_numeric <- suppressWarnings(as.numeric(positions_char))
    
    # Identify invalid entries
    non_numeric <- is.na(pos_numeric)
    valid_numeric <- pos_numeric[!non_numeric]
    
    count_valid <- length(valid_numeric)  # Counting valid numeric values
    print(count_valid)
    
    # Check if any validation fails
    if (length(non_numeric[non_numeric]) > 0 || count_valid > 200) {
      output$position_error <- renderUI({
        div(class = "alert alert-danger", role = "alert",
            tags$h4(icon("exclamation-circle"), "Invalid Input"),
            "Please choose less than 200 numeric positions"
        )
      })
      return()
    }
    
    # Clear any previous errors if validation passes
    output$position_error <- renderUI(NULL)
    
    ###################### for custom datasets #####################
    
    req(vcf_directory())
    
    vcf_directory <- vcf_directory()
    ###############################################################
    
    all_files <- list.files(vcf_directory, 
                            pattern = "\\.vcf\\.gz", 
                            full.names = TRUE)
    
    vcf_files <- all_files[!grepl("\\.tbi$", all_files)]
    
    results <- list()
    
    for (vcf_file in vcf_files) {
      index_file <- paste0(vcf_file, ".tbi")
      
      if (!file.exists(vcf_file) || !file.exists(index_file)) {
        message(sprintf("File or index missing for: %s", vcf_file))
        next
      }
      
      vcf_tabix <- TabixFile(vcf_file, index = index_file)
      
      ####################### For custom datasets #########################
      #chr_name <- sub(".*_(Chr\\d+)_nohets_dr_renamed.*", "\\1", vcf_file)
      
      for (chr in unique(positions()$Chromosome)) {
        if (chr %in% seqnamesTabix(vcf_tabix)) {
          
          pos_chr <- positions() %>% filter(Chromosome == chr)
          
          snp_granges <- GRanges(
            seqnames = pos_chr$Chromosome,
            ranges = IRanges(start = pos_chr$Position, end = pos_chr$Position)
          )
          
          vcf_subset <- readVcf(vcf_tabix, param = snp_granges)
          
          if (nrow(vcf_subset) == 0) {
            message(sprintf("No positions found in VCF subset for chromosome %s", chr))
            next
          }
          
          print(sprintf("Processing file: %s for chromosome %s", vcf_file, chr))
          
          chromosomes <- seqnames(rowRanges(vcf_subset))
          positions_vcf <- start(rowRanges(vcf_subset))
          refs <- rowRanges(vcf_subset)$REF
          alts <- sapply(rowRanges(vcf_subset)$ALT, function(alt) paste(alt, collapse = ","))
          geno_matrix <- geno(vcf_subset)$GT
          
          zdf <- data.frame(
            Chromosome = chromosomes,
            Position = positions_vcf,
            Ref = refs,
            Alt = alts,
            stringsAsFactors = FALSE
          )
          
          genotypes_df <- as.data.frame(geno_matrix)
          colnames(genotypes_df) <- colnames(geno_matrix)
          
          df <- cbind(zdf, genotypes_df) %>%
            rename_with(~ sub("^geno\\.", "", .), starts_with("geno")) %>%
            select(Chromosome, Position, Ref, Alt, everything())
          
          modified_df <- df %>%
            mutate(across(5:(ncol(df)), ~ case_when(
              . == "0/0" ~ "Ref",
              . == "0|0" ~ "Ref",
              . == "./." ~ "N",
              . == ".|." ~ "N",
              grepl("^(\\d)/(\\1)$", .) ~ paste0("Alt", sub("/.*", "", .)),
              grepl("^(\\d)\\|\\1$", .) ~ paste0("Alt", sub("\\|.*", "", .)),
              TRUE ~ "Het"
            ))) %>%
            inner_join(positions(), by = c("Chromosome", "Position")) %>% 
            mutate(Alt = paste(Alt.x, Alt.y, sep = ": "),
                   Ref = paste(Ref.x, Ref.y, sep = ": ")) %>%
            select(-Alt.x, -Alt.y, -Ref.x, -Ref.y) %>%
            select(Chromosome, Position, Ref, Alt, Gene_ID, Locus_Name, everything())
          
          # Store each chromosome's result separately or combine them later
          results[[paste0(vcf_file, "_", chr)]] <- modified_df
        } else {
          message(sprintf("Chromosome %s not found in VCF index: %s", chr, vcf_file))
        }
      }
    }
    
    if (!is.null(results) && length(results) > 0) {
      found_df <- bind_rows(results)%>%
        data.frame() %>% select(Chromosome, Position, Gene_ID, Locus_Name, Ref, Alt)} else {
          # If any columns are missing, set found_df to an empty data frame
          found_df <- data.frame(Chromosome = character(),
                                 Position = numeric(),
                                 Gene_ID = character(),
                                 Locus_Name = character(),
                                 Ref = character(),
                                 Alt = character())
        }
    
    #print(found_df)
    
    if (nrow(found_df) == 0) {
      showModal(modalDialog(
        title = "No Positions Found",
        "No matching positions were found for the input data. Please check your input and try again.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    combined_results <- bind_rows(results) %>% 
      t()%>%
      data.frame() %>%
      rownames_to_column(var = "Row_Name") %>%
      setNames(c("Info", paste0(1:(ncol(.) - 1))))%>%
      mutate(Info = gsub("\\.", "-", Info))
    
    
    
    new_colnames <- c("Info", sapply(2:ncol(combined_results), function(i) {
      # Get the first four values of the column, collapsing them with "_"
      first_four_values <- combined_results[, i][1:2]  # Take the first four values
      paste(na.omit(first_four_values), collapse = "_")  # Concatenate non-NA values
    }))
    
    # Rename columns
    colnames(combined_results) <- new_colnames
    
    # Update the reactive value with combined results
    results_list(combined_results)
    
    
    # create the unfound df
    positions_data <- positions()
    positions_data %>% select(Chromosome, Position, Gene_ID, Locus_Name, Ref, Alt)
    
    unfound_df <- positions_data[!(positions_data$Chromosome %in% found_df$Chromosome & 
                                     positions_data$Position %in% found_df$Position), ]
    
    output$unfound_positions_table <- DT::renderDataTable({
      unfound_df%>% select(Chromosome, Position, Gene_ID, Locus_Name, Ref, Alt)
    }, 
    rownames = FALSE)
    
    options(shiny.maxRequestSize = 1000 * 1024^2)
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("VCF_Positions_Summary_", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        
        tmp_file <- tempfile(fileext = ".xlsx")
        
        # Get the data
        df <- results_list()
        
        # Create a new workbook
        wb <- createWorkbook()
        
        # Add worksheets for found and unfound variants
        addWorksheet(wb, "Found Variants")
        writeData(wb, "Found Variants", df, rowNames = FALSE)
        
        addWorksheet(wb, "Unfound Variants")
        writeData(wb, "Unfound Variants", unfound_df, rowNames = FALSE)
        
        # Calculate the total number of cells
        total_cells <- nrow(df) * ncol(df)
        
        # Apply styles only if the total number of cells is less than
        if (total_cells < 10000) {
          rows <- 1:nrow(df) + 1  # +1 to account for the header row
          cols <- 1:ncol(df)
          
          for (row in rows) {
            for (col in cols) {
              cell_value <- df[row - 1, col]
              cell_style <- NULL  # Initialize as NULL
              
              # Apply cell colors based on conditions in the data frame
              if (!is.na(cell_value)) {
                if (grepl("Ref", cell_value)) {
                  cell_style <- createStyle(fgFill = "#faf3dd")  # Light yellow
                } else if (grepl("Alt", cell_value)) {
                  cell_style <- createStyle(fgFill = "#7FC8F5")  # Light blue
                } else if (cell_value == "Het") {
                  cell_style <- createStyle(fgFill = "#c8d5b9")  # Light green
                }
              }
              
              # Apply style if it's set
              if (!is.null(cell_style)) {
                addStyle(wb, sheet = "Found Variants", style = cell_style, rows = row, cols = col, gridExpand = TRUE)
              }
            }
          }
        } else {
          message("Skipping cell styling because the total number of cells exceeds 2000.")
        }
        
        
        saveWorkbook(wb, tmp_file, overwrite = TRUE)
        zip::zipr(file, files = tmp_file)
        
      }
    )
    shinyjs::enable("display")
    shinyjs::enable("download_excel")
  })
  
  observeEvent(input$display, {
    req(results_list())
    shinyjs::disable("display")
    
    
    output$data_table <- renderDT({
      datatable(
        results_list(), 
        options = list(pageLength = 24), 
        rownames = TRUE,
        callback = JS(
          "table.on('draw', function() {",
          "  var rows = table.rows().nodes();",
          "  $(rows).each(function(index, row) {",
          "    $('td', row).each(function(index, cell) {",
          "      var cellValue = $(cell).text();",
          "      if (cellValue.includes('Ref')) {",
          "        $(cell).css('background-color', '#faf3dd');",
          "      } else if (cellValue.includes('Alt')) {",
          "        $(cell).css('background-color', '#7FC8F5');",
          "      } else if (cellValue === 'Het') {",
          "        $(cell).css('background-color', '#c8d5b9');",
          "      }",
          "    });",
          "  });",
          "});"
        )
      ) 
    })
  })
  
  observeEvent(input$submit, {
    output$data_table <- NULL  # Clear the data table on new submission
    #shinyjs::disable("display")  # Disable Display button again after new submission
  })
}


#options(shiny.trace = TRUE)
shinyApp(ui, server)
