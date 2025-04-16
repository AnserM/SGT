if (!requireNamespace("rstudioapi", quietly = TRUE))
  install.packages("rstudioapi")

library("rstudioapi")

setwd(dirname(getSourceEditorContext()$path))

######################## Install required packages #########################

cran_packages <- c("shiny", "tidyverse", "dplyr", "DT", "openxlsx", "shinyjs", "shinyWidgets")
bioc_packages <- c("Rsamtools", "GenomicRanges", "VariantAnnotation")

installed <- rownames(installed.packages())

for (pkg in cran_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg)
  }
}


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


################## maximum file upload size #################
options(shiny.maxRequestSize = 5000 * 1024^2)


################# template df for custom positions ###########
template_positions <- c(
  "Chr03,45301350,Glyma.03G258700,Light_Tawny,Td,td_W177*",
  "Chr03,45301275,Glyma.03G258700,Light_Tawny,Td,td_S202R"
)


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


############### read in predefined positions file ###############
predefined_df <- read.csv("predefined_positions.csv", header = TRUE, stringsAsFactors = FALSE)
position_choices <- split(
  apply(predefined_df[, -1], 1, paste, collapse = ","),
  predefined_df$Category
)


##################set UI #####################
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
      titlePanel("Soybean Genotyping Tool")),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      ################## File upload ######################
      tags$h4("Upload VCF files (gzipped and indexed)"),
      fileInput("vcf_files", "Choose VCF and index (.vcf.gz and .vcf.gz.tbi) files",
                multiple = TRUE,
                accept = c(".vcf.gz", ".vcf.gz.tbi")),
      hr(),
      ################## Pre-specified position ##############
      tags$h4("Pre-Determined Positions"),
      
      pickerInput(
        inputId = "predefined_positions",
        label = "Select Positions",
        choices = position_choices,
        options = list(`actions-box` = TRUE),
        selected = NULL,
        multiple = TRUE),
      
      
      hr(),
      ############### custom positions ############
      tags$h4("Upload Custom Positions"),
      
      fileInput("custom_positions_file", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      actionButton("clear_file", "Clear File"),
      
      
      ############### template download #############
      downloadButton("download_template", "Download Template CSV"),
      
      hr(),
      
      
      ############## custom postions using dynamic rows #############
      tags$h4("Add Custom Positions"),
      
      
      # Container for dynamic input rows
      tags$div(id = "input_rows",
               uiOutput("dynamic_inputs")
      ),
      
      actionButton("add_row", "Add Row", icon = icon("plus")),
      hr(),
      
      ############ submit and down load buttons #################
      actionButton("submit", "Submit", icon = icon("paper-plane")),
      downloadButton("download_excel", "Download Excel", disabled = TRUE),
      hr(),
      uiOutput("position_error")
    ),
    
    ############## main panel for outputs #######################
    mainPanel(
      tags$div(
        id = "loading-message",
        "Preparing the output, please wait...",
        style = "font-weight: bold; font-size: 20px; color: red; display: none;"
      ),
      width = 6,
      tabsetPanel(
        tabPanel("Found Positions", DTOutput("data_table")),
        tabPanel("Unfound Positions", DTOutput("unfound_positions_table"))
      )
    )
  )
)


################ server code ##################
server <- function(input, output, session) {
  
  shinyjs::hide("loading-message")
  
  ################# read inputs from the dynamic rows ###################
  input_count <- reactiveVal(1)
  
  observeEvent(input$add_row, {
    new_count <- input_count() + 1
    input_count(new_count)
    
    insertUI(
      selector = "#input_rows",
      ui = fluidRow(
        id = paste0("row_", new_count),
        column(2, textInput(paste0("chromosome_", new_count), "Chrom")),
        column(2, numericInput(paste0("position_", new_count), "Position", value = NULL, min = 1)),
        column(2, textInput(paste0("gene_id_", new_count), "Gene ID", "")),
        column(2, textInput(paste0("locus_name_", new_count), "Locus", "")),
        column(2, textInput(paste0("ref_allele_", new_count), "Ref", "")),
        column(2, textInput(paste0("alt_allele_", new_count), "Alt", "")),
        column(1, actionButton(paste0("delete_row_", new_count), "️Trash", icon = icon("trash"), style = "color: red;"))
      )
    )
  })
  
  ############### Handle delete button clicks #################
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
  
  ################ read in custom input file #################
  custom_file_df <- reactiveVal(NULL)
  
  # Observe the file input to process the uploaded file
  observeEvent(input$custom_positions_file, {
    req(input$custom_positions_file)
    
    # Read the file data
    file_data <- read.csv(input$custom_positions_file$datapath, 
                          header = TRUE, 
                          stringsAsFactors = FALSE, 
                          colClasses = "character")
    file_data$Position <- as.numeric(file_data$Position)
    
    # Validate the required columns
    if (all(c("Chromosome", "Position", "Gene_ID", "Locus_Name", "Ref", "Alt") %in% names(file_data))) {
      custom_file_df(file_data)
    } else {
      showNotification("Uploaded file must contain the required columns: Chromosome, Position, Gene_ID, Locus_Name, Ref, Alt", type = "error")
      custom_file_df(NULL)
    }
  })
  
  observeEvent(input$clear_file, {
    custom_file_df(NULL)
    showNotification("Custom file data has been cleared", type = "message")
    reset("custom_positions_file")
  })
  
  
  positions <- reactive({
    
    ################### Read in defined position selection ######################
    # Initialize a data frame with selected predefined positions
    predefined_positions_df <- data.frame()
    
    if (!is.null(input$predefined_positions) && length(input$predefined_positions) > 0) {
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
      
      predefined_positions_df <- do.call(rbind, rows[!sapply(rows, is.null)])
    }
    #####################  combine with custom positions ########################
    
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
    
    
    
    # Ensure both data frames are valid before combining
    if (!is.null(custom_file_df()) && nrow(custom_file_df()) > 0) {
      custom_positions_df <- bind_rows(custom_positions_df, custom_file_df())
    }
    
    # Combine everything with predefined positions (safe even if one is empty)
    combined_positions <- bind_rows(predefined_positions_df, custom_positions_df)
    
    print(combined_positions)
    return(combined_positions)
  })
  
  ############### Download handler for template ##################
  output$download_template <- downloadHandler(
    filename = function() {
      paste("template_positions", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(template_data, file, row.names = FALSE)
    }
  )
  
  
  ############## initialize results ##############
  results_list <- reactiveVal(data.frame())
  
  observeEvent(input$submit, {
    shinyjs::show("loading-message")
    on.exit(shinyjs::hide("loading-message"))
    shinyjs::disable("download_excel")
    
    ############### read in specified positions ###########
    req(positions())
    
    pos_data <- positions()
    
    
    # Convert Position to character and check numeric
    positions_char <- as.character(pos_data$Position)
    pos_numeric <- suppressWarnings(as.numeric(positions_char))
    
    # Identify invalid entries
    non_numeric <- is.na(pos_numeric)
    valid_numeric <- pos_numeric[!non_numeric]
    
    count_valid <- length(valid_numeric)
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
    
    
    #################### read in the vcf files and indexes  #####################
    # Map uploaded files by name
    file_paths <- setNames(input$vcf_files$datapath, input$vcf_files$name)
    
    # Match .vcf.gz and .vcf.gz.tbi files
    vcf_files <- file_paths[grepl("\\.vcf\\.gz$", names(file_paths))]
    tbi_files <- file_paths[grepl("\\.vcf\\.gz\\.tbi$", names(file_paths))]
    
    # Pair each VCF with its .tbi file
    paired_files <- lapply(names(vcf_files), function(vcf_name) {
      index_name <- paste0(vcf_name, ".tbi")
      if (index_name %in% names(tbi_files)) {
        list(vcf = vcf_files[[vcf_name]], index = tbi_files[[index_name]], name = vcf_name)
      } else {
        warning(sprintf("Missing index for: %s", vcf_name))
        NULL
      }
    })
    paired_files <- Filter(Negate(is.null), paired_files)
    
    
    results <- list()
    
    ################## search positions within vcf files ##################
    for (pair in paired_files) {
      vcf_tabix <- TabixFile(pair$vcf, index = pair$index)
      
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
          
          
          
          chromosomes <- seqnames(rowRanges(vcf_subset))
          positions_vcf <- start(rowRanges(vcf_subset))
          refs <- rowRanges(vcf_subset)$REF
          alts <- sapply(rowRanges(vcf_subset)$ALT, function(alt) paste(alt, collapse = ","))
          geno_matrix <- geno(vcf_subset)$GT
          
          ################### convert found positions into a df #################
          zdf <- data.frame(
            Chromosome = chromosomes,
            Position = positions_vcf,
            Ref = refs,
            Alt = alts,
            stringsAsFactors = FALSE
          )
          
          genotypes_df <- as.data.frame(geno_matrix)
          colnames(genotypes_df) <- colnames(geno_matrix)
          
          ############### bind genotypes to the df ##################
          df <- cbind(zdf, genotypes_df) %>%
            rename_with(~ sub("^geno\\.", "", .), starts_with("geno")) %>%
            select(Chromosome, Position, Ref, Alt, everything())
          
          ############## modify values in the df ####################
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
          
          ################ Store each chromosome's result separately ################
          results[[paste0(pair$name, "_", chr)]] <- modified_df
        } else {
          message(sprintf("Chromosome %s not found in VCF index: %s", chr, pair$name))
        }
      }
    }
    
    ################### combine results in to one df ########################
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
    
    
    ############### message if no positions found ################
    
    if (nrow(found_df) == 0) {
      showModal(modalDialog(
        title = "No Positions Found",
        "No matching positions were found for the input data. Please check your input and try again.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    ################# combine results into one df #####################
    combined_results <- bind_rows(results) %>% 
      t()%>%
      data.frame() %>%
      rownames_to_column(var = "Row_Name") %>%
      setNames(c("Info", paste0(1:(ncol(.) - 1))))%>%
      mutate(Info = gsub("\\.", "-", Info))
    
    
    if (length(input$genotype) > 0) {
      combined_results <- combined_results %>%
        filter(
          Info == "Chromosome" | Info == "Position" | Info == "Ref" | Info == "Alt" |
            Info == "Gene_ID" | Info == "Locus_Name" |
            Info %in% input$genotype
        )
    }
    
    
    new_colnames <- c("Info", sapply(2:ncol(combined_results), function(i) {
      # Get the first four values of the column, collapsing them with "_"
      first_four_values <- combined_results[, i][1:2]
      paste(na.omit(first_four_values), collapse = "_")
    }))
    
    # Rename columns
    colnames(combined_results) <- new_colnames
    
    # Update the reactive value with combined results
    results_list(combined_results)
    
    
    ############## create the unfound positions df ##################
    positions_data <- positions()
    positions_data %>% select(Chromosome, Position, Gene_ID, Locus_Name, Ref, Alt)
    
    unfound_df <- positions_data[!(positions_data$Chromosome %in% found_df$Chromosome & 
                                     positions_data$Position %in% found_df$Position), ]
    
    output$unfound_positions_table <- DT::renderDataTable({
      unfound_df%>% select(Chromosome, Position, Gene_ID, Locus_Name, Ref, Alt)
    }, 
    rownames = FALSE)
    
    ################## output results with styling ##################
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
    
    
    ######################## output excel file for download ####################
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
          rows <- 1:nrow(df) + 1
          cols <- 1:ncol(df)
          
          for (row in rows) {
            for (col in cols) {
              cell_value <- df[row - 1, col]
              cell_style <- NULL
              
              # Apply cell colors based on conditions in the data frame
              if (!is.na(cell_value)) {
                if (grepl("Ref", cell_value)) {
                  cell_style <- createStyle(fgFill = "#faf3dd")
                } else if (grepl("Alt", cell_value)) {
                  cell_style <- createStyle(fgFill = "#7FC8F5")
                } else if (cell_value == "Het") {
                  cell_style <- createStyle(fgFill = "#c8d5b9")
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
    shinyjs::enable("download_excel")
  })
  
  
  
}

shinyApp(ui, server)
