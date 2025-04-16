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




################# template df for custom positions ###########
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


################# Meta data Soy2939 ################

soy_metadata <- read.csv("./info_files/Soy2939_metadata_dr.txt")


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
      tags$h4(tags$b("Gmax genome version Wm82.a2.v1"), style = "text-align: center;"),
      hr(),
      
      ################## Predefined positions ######################
      
      tags$h4("Pre-Determined Positions"),
      
      pickerInput(
        inputId = "predefined_positions",
        label = "Select Positions",
        choices = list(
          "Soybean Cyst Nematode" = c(
            "Chr02,44697705,Glyma.02G260400,SCN_GmSNAP02,GmSNAP02-WT,GmSNAP02-INS",
            "Chr02,44695753,Glyma.02G260400,SCN_GmSNAP02,GmSNAP02-WT,GmSNAP02-DEL",
            "Chr08,8361148,Glyma.08G108900,SCN_GmSHMT08/Rhg4,Rhg4,rhg4",
            "Chr11,32969916,Glyma.11G234500,SCN_Rhg2/GmSNAP11,Rhg2,rhg2",
            "Chr15,20631002,Glyma.15G191200,SCN_GmSNAP15,GmSNAP15,GmSNAP15-R",
            "Chr18,1643660,Glyma.18G022500,SCN_Rhg1/GmSNAP18,Rhg1-c,rhg1-a",
            "Chr18,1643643,Glyma.18G022500,SCN_Rhg1/GmSNAP18,Rhg1-c,rhg1-b",
            "Chr18,1645409,Glyma.18G022500,SCN_Rhg1/GmSNAP18,Rhg1-c,rhg1-a/b"
          ),
          "Pod Wall" = c(
            "Chr03,528386,Glyma.03G005700,Pod_wall_L2,l2_REF,L2_T3A",
            "Chr03,528236,Glyma.03G005700,Pod_wall_L2,REF,l2_Q53*",
            "Chr03,528212,Glyma.03G005700,Pod_wall_L2,REF,l2_L61F",
            "Chr03,528046,Glyma.03G005700,Pod_wall_L2,REF,l2_W116*",
            "Chr03,528245,Glyma.03G005700,Pod_wall_L2,REF,l2_Q50*",
            "Chr03,528076,Glyma.03G005700,Pod_wall_L2,REF,l2_R106fs",
            "Chr19,37806160,Glyma.19G120400,Pod_wall_L1,l1,L1_RI"
          ),
          "Maturity" = c(
            "Chr04,4080056,Glyma.04G050200,Maturity_Long_Juvenile,J,j_R73G",
            "Chr04,36758368,Glyma.04G156400,Maturity_E1LA,E1LA,E1la:K82E",
            "Chr06,20207322,Glyma.06G207800,Maturity_e1-as/E1,e1-as,E1",
            "Chr10,45310798,Glyma.10G221500,Maturity_E2/e2,E2,e2",
            "Chr12,5520945,Glyma.12G073900,Maturity_DTF2,tof12-1,Tof12",
            "Chr19,47657041,Glyma.19G224400,Maturity_E3/e3-tr_associated_K115Q,E3,e3_ass."
          ),
          "Seed Compostion" = c(
            "Chr05,308412,Glyma.05G003900,Composition_RS3_snp6,RS3snp6,rs3snp6",
            "Chr05,307744,Glyma.05G003900,Composition_rs3_G75E,RS3,rs3G75E",
            "Chr05,2910312,Glyma.05G033500,Aromatic_BAD2_indel,BAD2,bad2",
            "Chr06,15222105,Glyma.06G179200,Composition_RS2_W331-,RS2,rs2",
            "Chr07,43139742,Glyma.07G254600,Composition_sg-1o,SG-10,sg-10",
            "Chr10,50014632,Glyma.10G278000,Composition_FAD2-1A_indel,AA,aa",
            "Chr13,43771864,Glyma.13G347600,Composition_Lipoxygenase_1_del,LX1,lx1",
            "Chr13,43764664,Glyma.13G347500,Composition_Lipoxygenase_2_H532Q,LX2,lx2",
            "Chr14,45937922,Glyma.14G194300,Composition_FAD3A_splice,AA,aa",
            "Chr15,2123901,Glyma.15G026300,Composition_Lipoxygenase_3_indel,LX3,lx3",
            "Chr15,3876981,Glyma.15G049200,Protein_SWEET,low_pro,hi_pro",
            "Chr18,5647138,Glyma.18G062000,Composition_FAD3C,CC,cc",
            "Chr20,33119281,Glyma.20G085100,Protein_POWR1,cct,CCT",
            "Chr20,35318088,Glyma.20G111000,Composition_FAD2-1B_P137R,BB,bb"
          ),
          "Flower, Seed Coat, Pubesence Color" = c(
            "Chr03,45301350,Glyma.03G258700,Light_Tawny,Td,td_W177*",
            "Chr03,45301275,Glyma.03G258700,Light_Tawny,Td,td_S202R",
            "Chr03,45301305,Glyma.03G258700,Light_Tawny,Td,td_F192fs",
            "Chr03,45301379,Glyma.03G258700,Light_Tawny,Td,td_A168fs",
            "Chr03,45301502,Glyma.03G258700,Light_Tawny,Td,td_P127S",
            "Chr06,18737366,Glyma.06G202300,Tawny,T,t",
            "Chr08,8578437,Glyma.08G111400,DomI_assoc.,ii,I_assoc.",
            "Chr09,45759137,Glyma.09G235100,R_Black/Brown,R,r_R75fs",
            "Chr09,45759165,Glyma.09G235100,R_Black/Brown,R,r_splice",
            "Chr09,45758856,Glyma.09G235100,R_Black/Brown,R,r_W32S",
            "Chr09,45759100,Glyma.09G235100,R_Black/Brown,R,r_G63fs",
            "Chr13,17316756,Glyma.13G072100,W1,w1,W1"
          ),
          "Shattering" = c(
            "Chr07,4332840,Glyma.07G050600,Pod_shatter,nst1a,NST1A",
            "Chr16,29944393,NA,Pod_shatter,pdh1,Pdh1"
          ),
          
          "Plant Architecture" = c(
            "Chr18,55642486,Glyma.18G273600,Semi-determinate,dt2,Dt2",
            "Chr19,45183701,Glyma.19G194300,Dt1,DT1,dt1",
            "Chr20,35828042,Glyma.20G116200,Narrow_leaflets,Ln,ln"
          )
          
          
        ),
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
      
      tags$h4("Add Custom Positions (Chromsomes named as Chr01 ...)"),
      
      
      ############## custom postions using dynamic rows #############
      tags$div(id = "input_rows",
               uiOutput("dynamic_inputs") 
      ),
      
      actionButton("add_row", "Add Row", icon = icon("plus")),
      hr(),
      
      ############## Genotype selection #############
      
      selectizeInput(
        inputId = "genotype",  # Updated input ID
        label = "Select genotypes or paste a comma separated list:",
        choices = NULL,  
        multiple = TRUE, 
        options = list(placeholder = "Select or search genotypes...", maxOptions = 20,
                       create = TRUE, 
                       delimiter = ",")
      ),
      
      ############## dowload metadata #############
      
      downloadButton("meta_data", "Download Metadata"),
      hr(),
      
      ############ submit and download buttons #################
      
      actionButton("submit", "Submit", icon = icon("paper-plane")),
      downloadButton("download_excel", "Download Excel", disabled = TRUE),
      hr(),
      uiOutput("position_error")
    ),
    
    ############## main panel for outputs #################
    
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
  
  ################# read in file containing genotype names ###################
  
  genotype_data_path <- "./info_files/genotypes.csv"
  
  genotype_file_data <- reactive({
    req(file.exists(genotype_data_path))
    
    # Read the file into a data frame
    genotype_data <- read.csv(genotype_data_path, header = FALSE, stringsAsFactors = FALSE)
    unique_genotypes <- unique(genotype_data$V1)
    return(unique_genotypes)
  })
  
  ################# Fill in genotype choices ###################
  
  observe({
    genotype_data <- genotype_file_data()
    updateSelectizeInput(session, "genotype", 
                         choices = genotype_data,
                         selected = NULL,
                         server= TRUE)
  })
  
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
  
  ################### Read in selection from predefined positions ######################
  positions <- reactive({
    # Initialize a data frame with selected predefined positions
    predefined_positions_df <- data.frame(
      Chromosome = sapply(input$predefined_positions, function(x) strsplit(x, ",")[[1]][1]),
      Position = as.numeric(sapply(input$predefined_positions, function(x) strsplit(x, ",")[[1]][2])),
      Gene_ID = sapply(input$predefined_positions, function(x) strsplit(x, ",")[[1]][3]),
      Locus_Name = sapply(input$predefined_positions, function(x) strsplit(x, ",")[[1]][4]),
      Ref = sapply(input$predefined_positions, function(x) strsplit(x, ",")[[1]][5]),
      Alt = sapply(input$predefined_positions, function(x) strsplit(x, ",")[[1]][6]),
      stringsAsFactors = FALSE
    )
    
    ################### Combine predefined positions with custom positions ######################
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
  
  ############### Download handler for template ##################
  output$download_template <- downloadHandler(
    filename = function() {
      paste("template_positions", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(template_data, file, row.names = FALSE)
    }
  )
  
  ############### Download handler for metadata ##################
  output$meta_data <- downloadHandler(
    filename = function() {
      paste("Soy2939_Metadata", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(soy_metadata, file, row.names = FALSE)
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
    vcf_directory <- "./data/output/"
    
    all_files <- list.files(vcf_directory, 
                            pattern = "Soy2939_Chr\\d+_nohets_dr_renamed_GTonly\\.vcf\\.gz", 
                            full.names = TRUE)
    
    vcf_files <- all_files[!grepl("\\.tbi$", all_files)]
    
    results <- list()
    
    ################## search positions within vcf files ##################
    
    for (vcf_file in vcf_files) {
      index_file <- paste0(vcf_file, ".tbi")
      
      if (!file.exists(vcf_file) || !file.exists(index_file)) {
        message(sprintf("File or index missing for: %s", vcf_file))
        next
      }
      
      vcf_tabix <- TabixFile(vcf_file, index = index_file)
      chr_name <- sub(".*_(Chr\\d+)_nohets_dr_renamed.*", "\\1", vcf_file)
      
      if (chr_name %in% positions()$Chromosome) {
        snp_granges <- GRanges(
          seqnames = positions()$Chromosome[positions()$Chromosome == chr_name],
          ranges = IRanges(start = positions()$Position[positions()$Chromosome == chr_name], 
                           end = positions()$Position[positions()$Chromosome == chr_name])
        )
        
        vcf_subset <- readVcf(vcf_tabix, param = snp_granges)
        
        # Check if vcf_subset is empty
        if (nrow(vcf_subset) == 0) {
          message(sprintf("No positions found in VCF subset for file: %s", vcf_file))
          next  # Skip to the next VCF file
        }
        print(sprintf("Processing file: %s for chromosome %s", vcf_file, chr_name))
        
        chromosomes <- seqnames(rowRanges(vcf_subset))
        positions <- start(rowRanges(vcf_subset))
        refs <- rowRanges(vcf_subset)$REF
        alts <- sapply(rowRanges(vcf_subset)$ALT, function(alt) paste(alt, collapse = ","))
        geno_matrix <- geno(vcf_subset)$GT
        
        ################### convert found positions into a df #################
        zdf <- data.frame(
          Chromosome = chromosomes,
          Position = positions,
          Ref = refs,
          Alt = alts,
          stringsAsFactors = FALSE
        )
        
        # Add each genotype column to the data frame
        genotypes_df <- as.data.frame(geno_matrix)
        colnames(genotypes_df) <- colnames(geno_matrix)
        
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
          mutate(Alt = paste(Alt.x, Alt.y, sep = ": ")) %>%
          mutate(Ref = paste(Ref.x, Ref.y, sep = ": ")) %>%
          select(-Alt.x, -Alt.y, -Ref.x, -Ref.y) %>%
          select(Chromosome, Position, Ref, Alt, Gene_ID, Locus_Name, everything()) 
        
        results[[vcf_file]] <- modified_df
      } else {
        message(sprintf("Chromosome %s not found in positions data; skipping file: %s", chr_name, vcf_file))
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
    
    combined_results <- bind_rows(results) %>% 
      t()%>%
      data.frame() %>%
      rownames_to_column(var = "Row_Name") %>%
      setNames(c("Info", paste0(1:(ncol(.) - 1))))%>%
      mutate(Info = gsub("\\.", "-", Info))
    
    ################# combine results into one df #####################
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
    
    options(shiny.maxRequestSize = 1000 * 1024^2)
    
    
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


#options(shiny.trace = TRUE)
shinyApp(ui, server)
