#!/usr/bin/env Rscript

# ==============================================================================
# UN Country Contributions PDF Parser
# ==============================================================================
# 
# Comprehensive R script to parse UN country contributions PDF files and extract
# structured data including Year, Mission, Country/Nationality, Personnel Type,
# Female count, Male count, and Total count.
#
# Handles multiple PDF formats spanning 2015-2025 with diagnostic tools,
# error handling, and progress tracking.
# ==============================================================================

# Required libraries
required_packages <- c("stringr", "dplyr", "readr", "tibble", "purrr", "lubridate")

# Function to install and load required packages
setup_packages <- function() {
  cat("Setting up required packages...\n")
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(paste("Installing", pkg, "...\n"))
      install.packages(pkg, repos = "https://cran.rstudio.com/", quiet = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  cat("All packages loaded successfully.\n\n")
}

# ==============================================================================
# DIAGNOSTIC FUNCTIONS
# ==============================================================================

#' Inspect raw PDF text content
inspect_pdf_raw <- function(pdf_path, lines_to_show = 50) {
  cat(paste("Inspecting PDF:", basename(pdf_path), "\n"))
  cat(paste("File size:", file.size(pdf_path), "bytes\n"))
  
  # Extract text using pdftotext
  temp_file <- tempfile(fileext = ".txt")
  system_command <- paste("pdftotext -layout", shQuote(pdf_path), shQuote(temp_file))
  
  result <- try({
    system_result <- system(system_command, intern = FALSE)
    if (system_result != 0) {
      stop("pdftotext command failed")
    }
    
    if (!file.exists(temp_file)) {
      stop("Text extraction failed - no output file created")
    }
    
    text_lines <- readLines(temp_file, warn = FALSE)
    unlink(temp_file)
    
    cat(paste("Total lines extracted:", length(text_lines), "\n"))
    cat(paste("Showing first", min(lines_to_show, length(text_lines)), "lines:\n"))
    cat(paste(rep("=", 70), collapse = ""), "\n")
    
    for (i in 1:min(lines_to_show, length(text_lines))) {
      cat(sprintf("%3d: %s\n", i, text_lines[i]))
    }
    
    return(text_lines)
  }, silent = TRUE)
  
  if (inherits(result, "try-error")) {
    cat("Error extracting PDF text:", result$message, "\n")
    return(NULL)
  }
  
  return(result)
}

#' Identify PDF format based on content patterns
identify_pdf_format <- function(text_lines) {
  if (is.null(text_lines) || length(text_lines) == 0) {
    return("unknown")
  }
  
  text_content <- paste(text_lines, collapse = " ")
  
  # Pattern matching for different formats
  if (str_detect(text_content, "UN Mission's Summary")) {
    return("format_2015_2018")
  } else if (str_detect(text_content, "Summary of Contributions to UN Peacekeeping")) {
    return("format_2019_2022")
  } else if (str_detect(text_content, "Contribution of Uniformed Personnel to UN")) {
    return("format_2023_plus")
  } else {
    return("unknown")
  }
}

#' Analyze table structure in PDF
analyze_table_structure <- function(text_lines) {
  if (is.null(text_lines) || length(text_lines) == 0) {
    return(list(format = "unknown", data_lines = 0, countries = 0))
  }
  
  format_type <- identify_pdf_format(text_lines)
  
  # Count potential data lines (lines with numbers)
  data_lines <- sum(str_detect(text_lines, "\\d+"))
  
  # Count potential country names (lines that start with capital letters and aren't headers)
  potential_countries <- text_lines[str_detect(text_lines, "^[A-Z][a-zA-Z]") & 
                                   !str_detect(text_lines, "UN Mission|Country|POST|Mission|Male|Female|Total")]
  
  list(
    format = format_type,
    total_lines = length(text_lines),
    data_lines = data_lines,
    potential_countries = length(potential_countries),
    sample_countries = head(potential_countries, 5)
  )
}

# ==============================================================================
# PDF PARSING FUNCTIONS
# ==============================================================================

#' Extract text from PDF using pdftotext
extract_pdf_text <- function(pdf_path) {
  if (!file.exists(pdf_path)) {
    warning(paste("PDF file not found:", pdf_path))
    return(NULL)
  }
  
  temp_file <- tempfile(fileext = ".txt")
  system_command <- paste("pdftotext -layout", shQuote(pdf_path), shQuote(temp_file))
  
  result <- try({
    system_result <- system(system_command, intern = FALSE)
    if (system_result != 0) {
      stop("pdftotext command failed")
    }
    
    text_lines <- readLines(temp_file, warn = FALSE)
    unlink(temp_file)
    return(text_lines)
  }, silent = TRUE)
  
  if (inherits(result, "try-error")) {
    warning(paste("Failed to extract text from:", basename(pdf_path)))
    return(NULL)
  }
  
  return(result)
}

#' Parse 2015-2018 format PDFs
parse_format_2015_2018 <- function(text_lines, year, month) {
  results <- data.frame()
  current_country <- ""
  
  for (i in seq_along(text_lines)) {
    line <- str_trim(text_lines[i])
    
    # Skip empty lines, headers, and page markers
    if (line == "" || str_detect(line, "UN Mission|Country|Description|Totals|Page \\d+ of \\d+")) {
      next
    }
    
    # Check if line starts with a country name (not indented, alphabetic)
    if (str_detect(line, "^[A-Z][a-zA-Z]+$") && !str_detect(line, "^\\s")) {
      current_country <- str_trim(line)
      next
    }
    
    # Parse data lines - look for lines with mission names and numbers
    if (current_country != "" && str_detect(line, "\\d")) {
      # Look for lines with mission names (UN*, MIN*, MON*)
      mission_match <- str_extract(line, "(UN[A-Z]+|MIN[A-Z]+|MON[A-Z]+)")
      
      if (!is.na(mission_match)) {
        # Extract personnel type
        personnel_type <- case_when(
          str_detect(line, "Individual Police") ~ "Individual Police",
          str_detect(line, "Contingent Troop") ~ "Contingent Troops", 
          str_detect(line, "Experts on Mission") ~ "Experts on Mission",
          TRUE ~ "Other"
        )
        
        # Extract all numbers from the line
        numbers <- str_extract_all(line, "\\d+")[[1]]
        numbers <- as.numeric(numbers)
        
        # Look for pattern: male female total (usually last 3 numbers)
        if (length(numbers) >= 3) {
          # Take the last 3 numbers as Male, Female, Total
          male <- numbers[length(numbers)-2]
          female <- numbers[length(numbers)-1]
          total <- numbers[length(numbers)]
          
          results <- rbind(results, data.frame(
            Year = year,
            Month = month,
            Mission = mission_match,
            Country = current_country,
            Personnel_Type = personnel_type,
            Male = male,
            Female = female,
            Total = total,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(results)
}

#' Parse 2019-2022 format PDFs
parse_format_2019_2022 <- function(text_lines, year, month) {
  results <- data.frame()
  current_country <- ""
  current_mission <- ""
  
  for (i in seq_along(text_lines)) {
    line <- str_trim(text_lines[i])
    
    # Skip empty lines, headers, and page markers
    if (line == "" || str_detect(line, "Country Name|POST|MALE|FEMALE|TOTAL|Summary of Contributions|Page \\d+")) {
      next
    }
    
    # Check for country lines (numbered)
    country_match <- str_match(line, "^(\\d+)\\s+([A-Za-z].*?)$")
    if (!is.na(country_match[1])) {
      current_country <- str_trim(country_match[3])
      next
    }
    
    # Look for mission information in subsequent lines
    mission_match <- str_extract(line, "(UN[A-Z]+|MIN[A-Z]+|MON[A-Z]+)")
    if (!is.na(mission_match)) {
      current_mission <- mission_match
    }
    
    # Parse data lines with personnel information
    if (current_country != "" && str_detect(line, "\\d")) {
      # Look for lines with personnel type and numbers
      if (str_detect(line, "(Contingent Troops|Individual Police|Experts on Mission)")) {
        
        # Extract personnel type
        personnel_type <- case_when(
          str_detect(line, "Contingent Troops") ~ "Contingent Troops",
          str_detect(line, "Individual Police") ~ "Individual Police", 
          str_detect(line, "Experts on Mission") ~ "Experts on Mission",
          TRUE ~ "Other"
        )
        
        # Extract all numbers from the line
        numbers <- str_extract_all(line, "\\d+")[[1]]
        numbers <- as.numeric(numbers)
        
        if (length(numbers) >= 3) {
          # Take the last 3 numbers as Male, Female, Total
          male <- numbers[length(numbers)-2]
          female <- numbers[length(numbers)-1]
          total <- numbers[length(numbers)]
          
          results <- rbind(results, data.frame(
            Year = year,
            Month = month,
            Mission = ifelse(current_mission != "", current_mission, "Various"),
            Country = current_country,
            Personnel_Type = personnel_type,
            Male = male,
            Female = female,
            Total = total,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(results)
}

#' Parse 2023+ format PDFs
parse_format_2023_plus <- function(text_lines, year, month) {
  results <- data.frame()
  current_mission <- ""
  current_country <- ""
  
  for (i in seq_along(text_lines)) {
    line <- str_trim(text_lines[i])
    
    # Skip empty lines, headers, and page markers
    if (line == "" || str_detect(line, "Mission|Personnel Type|Male|Female|Total|Contribution of Uniformed|Page \\d+|Report Generated|Grand Total")) {
      next
    }
    
    # Check for mission lines (ALL CAPS, no leading spaces, typically 3-8 chars)
    if (str_detect(line, "^[A-Z]{3,8}$")) {
      current_mission <- line
      current_country <- ""
      next
    }
    
    # Check for country lines (indented country names)
    if (str_detect(line, "^\\s+[A-Z][a-zA-Z\\s]+$") && !str_detect(line, "Total")) {
      current_country <- str_trim(line)
      next
    }
    
    # Parse data lines with personnel information
    if (current_mission != "" && str_detect(line, "\\d")) {
      # Look for lines with personnel types and numbers
      if (str_detect(line, "(Individual Police|Experts on Mission|Staff Officer|Troops|Formed Police Units)")) {
        
        # Extract personnel type
        personnel_type <- case_when(
          str_detect(line, "Individual Police") ~ "Individual Police",
          str_detect(line, "Experts on Mission") ~ "Experts on Mission", 
          str_detect(line, "Staff Officer") ~ "Staff Officer",
          str_detect(line, "Troops") ~ "Troops",
          str_detect(line, "Formed Police Units") ~ "Formed Police Units",
          TRUE ~ "Other"
        )
        
        # Extract all numbers from the line
        numbers <- str_extract_all(line, "\\d+")[[1]]
        numbers <- as.numeric(numbers)
        
        if (length(numbers) >= 3) {
          # Take the last 3 numbers as Male, Female, Total
          male <- numbers[length(numbers)-2]
          female <- numbers[length(numbers)-1]
          total <- numbers[length(numbers)]
          
          # Use the country from the line if available, otherwise use current_country
          country_in_line <- str_extract(line, "^\\s*([A-Za-z\\s]+?)\\s+(Individual Police|Experts on Mission|Staff Officer|Troops|Formed Police Units)")
          
          if (!is.na(country_in_line)) {
            extracted_country <- str_trim(str_replace(country_in_line, "(Individual Police|Experts on Mission|Staff Officer|Troops|Formed Police Units).*", ""))
          } else {
            extracted_country <- current_country
          }
          
          if (extracted_country != "" && !str_detect(extracted_country, "Total")) {
            results <- rbind(results, data.frame(
              Year = year,
              Month = month,
              Mission = current_mission,
              Country = extracted_country,
              Personnel_Type = personnel_type,
              Male = male,
              Female = female,
              Total = total,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  return(results)
}

#' Validate and clean extracted data
validate_and_clean_data <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  
  cat("Validating and cleaning data...\n")
  original_rows <- nrow(data)
  
  # Remove rows with invalid personnel types (numbers only)
  valid_personnel_types <- c("Individual Police", "Experts on Mission", "Contingent Troops", 
                           "Staff Officer", "Troops", "Formed Police Units", "Other")
  
  # Clean up personnel types that are just numbers
  data$Personnel_Type[str_detect(data$Personnel_Type, "^\\d+$")] <- "Other"
  
  # Remove rows where country names are actually numbers
  data <- data[!str_detect(data$Country, "^\\d+$"), ]
  
  # Remove rows with invalid totals (where Male + Female != Total, allowing for small discrepancies)
  data$calculated_total <- data$Male + data$Female
  data <- data[abs(data$Total - data$calculated_total) <= 1, ]  # Allow 1 unit discrepancy
  data$calculated_total <- NULL
  
  # Clean country names (remove extra spaces, fix encoding issues)
  data$Country <- str_trim(str_squish(data$Country))
  
  # Remove empty or very short country names
  data <- data[nchar(data$Country) >= 3, ]
  
  # Remove obvious header/footer text that got parsed as countries
  header_patterns <- c("Page", "Report", "Total", "Mission", "Country", "Personnel")
  data <- data[!str_detect(data$Country, paste(header_patterns, collapse = "|")), ]
  
  cleaned_rows <- nrow(data)
  removed_rows <- original_rows - cleaned_rows
  
  cat(paste("Removed", removed_rows, "invalid records\n"))
  cat(paste("Retained", cleaned_rows, "valid records\n"))
  
  return(data)
}

#' Parse a single PDF file
parse_single_pdf <- function(pdf_path) {
  filename <- basename(pdf_path)
  
  # Extract year and month from filename
  date_match <- str_match(filename, "UN_country_contributions_(\\d{4})_(\\d{2})\\.pdf")
  if (is.na(date_match[1])) {
    warning(paste("Could not extract date from filename:", filename))
    return(data.frame())
  }
  
  year <- as.numeric(date_match[2])
  month <- as.numeric(date_match[3])
  
  cat(paste("Processing:", filename, "(", year, "-", sprintf("%02d", month), ")\n"))
  
  # Extract text
  text_lines <- extract_pdf_text(pdf_path)
  if (is.null(text_lines)) {
    return(data.frame())
  }
  
  # Identify format and parse accordingly
  format_type <- identify_pdf_format(text_lines)
  cat(paste("  Format:", format_type, "\n"))
  
  result <- switch(format_type,
    "format_2015_2018" = parse_format_2015_2018(text_lines, year, month),
    "format_2019_2022" = parse_format_2019_2022(text_lines, year, month),
    "format_2023_plus" = parse_format_2023_plus(text_lines, year, month),
    {
      warning(paste("Unknown format for file:", filename))
      data.frame()
    }
  )
  
  cat(paste("  Extracted", nrow(result), "records\n"))
  
  # Validate and clean the data
  result <- validate_and_clean_data(result)
  cat(paste("  Final valid records:", nrow(result), "\n"))
  
  return(result)
}

# ==============================================================================
# MAIN PROCESSING FUNCTIONS
# ==============================================================================

#' Parse all PDF files in a directory
parse_all_pdfs <- function(pdf_dir = ".", pattern = "UN_country_contributions_\\d{4}_\\d{2}\\.pdf", max_files = NULL) {
  pdf_files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
  pdf_files <- sort(pdf_files)  # Process in chronological order
  
  if (length(pdf_files) == 0) {
    stop("No PDF files found matching pattern in directory:", pdf_dir)
  }
  
  if (!is.null(max_files)) {
    pdf_files <- head(pdf_files, max_files)
  }
  
  cat(paste("Found", length(pdf_files), "PDF files to process\n"))
  cat("Starting processing...\n\n")
  
  all_results <- data.frame()
  processed_count <- 0
  error_count <- 0
  
  for (pdf_file in pdf_files) {
    result <- try({
      file_result <- parse_single_pdf(pdf_file)
      processed_count <- processed_count + 1
      
      # Progress indicator
      if (processed_count %% 10 == 0) {
        cat(paste("Progress:", processed_count, "/", length(pdf_files), "files processed\n"))
      }
      
      return(file_result)
    }, silent = TRUE)
    
    if (inherits(result, "try-error")) {
      cat(paste("ERROR processing", basename(pdf_file), ":", result$message, "\n"))
      error_count <- error_count + 1
      result <- data.frame()
    }
    
    if (nrow(result) > 0) {
      all_results <- rbind(all_results, result)
    }
  }
  
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("PROCESSING SUMMARY:\n")
  cat(paste("Total files processed:", processed_count, "\n"))
  cat(paste("Files with errors:", error_count, "\n"))
  cat(paste("Total records extracted:", nrow(all_results), "\n"))
  
  if (nrow(all_results) > 0) {
    cat(paste("Date range:", min(all_results$Year), "-", max(all_results$Year), "\n"))
    cat(paste("Countries found:", length(unique(all_results$Country)), "\n"))
    cat(paste("Missions found:", length(unique(all_results$Mission)), "\n"))
  }
  
  return(all_results)
}

#' Export results to CSV
export_to_csv <- function(data, filename = NULL) {
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("un_contributions_", timestamp, ".csv")
  }
  
  if (nrow(data) == 0) {
    warning("No data to export")
    return(NULL)
  }
  
  # Sort data by year, month, mission, country
  data <- data[order(data$Year, data$Month, data$Mission, data$Country), ]
  
  write_csv(data, filename)
  cat(paste("Data exported to:", filename, "\n"))
  cat(paste("File size:", file.size(filename), "bytes\n"))
  
  return(filename)
}

#' Generate sample output for verification
generate_sample_output <- function(data, n_samples = 20) {
  if (nrow(data) == 0) {
    cat("No data available for sampling\n")
    return()
  }
  
  cat("SAMPLE DATA EXTRACTION:\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Show basic statistics
  cat("DATA SUMMARY:\n")
  cat(paste("Total records:", nrow(data), "\n"))
  cat(paste("Year range:", min(data$Year, na.rm = TRUE), "-", max(data$Year, na.rm = TRUE), "\n"))
  cat(paste("Unique countries:", length(unique(data$Country)), "\n"))
  cat(paste("Unique missions:", length(unique(data$Mission)), "\n"))
  cat(paste("Personnel types:", paste(unique(data$Personnel_Type), collapse = ", "), "\n"))
  
  # Show sample records
  cat("\nSAMPLE RECORDS:\n")
  sample_data <- head(data, n_samples)
  print(sample_data)
  
  # Show summary by year
  cat("\nRECORDS BY YEAR:\n")
  year_summary <- table(data$Year)
  print(year_summary)
  
  # Show top countries by total personnel
  cat("\nTOP 10 COUNTRIES BY TOTAL PERSONNEL:\n")
  country_totals <- aggregate(Total ~ Country, data, sum)
  country_totals <- country_totals[order(country_totals$Total, decreasing = TRUE), ]
  print(head(country_totals, 10))
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Show script usage
show_usage <- function() {
  cat("UN Country Contributions PDF Parser\n")
  cat("==================================\n\n")
  cat("Usage examples:\n")
  cat("# Setup packages\n")
  cat("setup_packages()\n\n")
  cat("# Inspect a single PDF\n")
  cat("inspect_pdf_raw('UN_country_contributions_2023_01.pdf')\n\n")
  cat("# Process all PDFs and export to CSV\n")
  cat("results <- parse_all_pdfs()\n")
  cat("export_to_csv(results)\n\n")
  cat("# Process sample files only\n")
  cat("results <- parse_all_pdfs(max_files = 5)\n")
  cat("generate_sample_output(results)\n\n")
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

#' Main function to execute the complete parsing workflow
main <- function() {
  cat("UN Country Contributions PDF Parser\n")
  cat("===================================\n\n")
  
  # Setup
  setup_packages()
  
  # Process all PDFs
  cat("Starting comprehensive PDF processing...\n")
  results <- parse_all_pdfs()
  
  if (nrow(results) > 0) {
    # Export results
    csv_file <- export_to_csv(results)
    
    # Generate sample output
    generate_sample_output(results)
    
    cat("\nProcessing completed successfully!\n")
    cat(paste("Results saved to:", csv_file, "\n"))
  } else {
    cat("No data was extracted. Please check the PDF files and try again.\n")
  }
}

# Execute main function if script is run directly
if (!interactive()) {
  main()
} else {
  cat("Script loaded. Available functions:\n")
  cat("- setup_packages(): Install required packages\n")
  cat("- inspect_pdf_raw(): Examine PDF content\n")
  cat("- analyze_table_structure(): Analyze PDF structure\n")
  cat("- parse_all_pdfs(): Process all PDF files\n")
  cat("- export_to_csv(): Export data to CSV\n")
  cat("- generate_sample_output(): Show sample results\n")
  cat("- main(): Run complete workflow\n")
  cat("- show_usage(): Show detailed usage examples\n\n")
  cat("Run main() to execute the complete workflow.\n")
}