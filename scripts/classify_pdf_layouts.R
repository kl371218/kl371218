#!/usr/bin/env Rscript

# PDF Layout Classifier for UN Country Contributions
# Automatically detects layout type for each PDF file to enable correct parser selection

library(pdftools)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)

#' Extract report date from filename or PDF content
#' @param filename PDF filename
#' @param text First page text content
extract_report_date <- function(filename, text) {
  # Try to extract from filename first (format: UN_country_contributions_YYYY_MM.pdf)
  date_match <- str_extract(filename, "\\d{4}_\\d{2}")
  if (!is.na(date_match)) {
    return(gsub("_", "-", date_match))
  }
  
  # Try to extract from text content
  date_patterns <- c(
    "\\d{2}/\\d{2}/\\d{4}",  # DD/MM/YYYY
    "\\d{2}-\\w{3}-\\d{2}",  # DD-MMM-YY
    "As of: \\d{2}/\\d{2}/\\d{4}",  # As of: DD/MM/YYYY
    "Month of Report.*?:\\s*(\\d{2}-\\w{3}-\\d{2})"  # Month of Report : DD-MMM-YY
  )
  
  for (pattern in date_patterns) {
    match <- str_extract(text, pattern)
    if (!is.na(match)) {
      return(match)
    }
  }
  
  return(NA_character_)
}

#' Get sample header line from PDF text
#' @param text First page text content
get_sample_header <- function(text) {
  # Split into lines and find the first line with column headers
  lines <- strsplit(text, "\n")[[1]]
  lines <- str_trim(lines)
  lines <- lines[nchar(lines) > 10]  # Filter out very short lines
  
  # Look for lines containing key header words
  header_keywords <- c("country", "mission", "description", "personnel", "post", "male", "female", "total")
  
  for (line in lines[1:min(20, length(lines))]) {  # Check first 20 lines
    line_lower <- tolower(line)
    keyword_count <- sum(sapply(header_keywords, function(kw) grepl(kw, line_lower)))
    if (keyword_count >= 3) {  # Line contains at least 3 keywords
      return(str_squish(line))
    }
  }
  
  # Fallback: return first non-empty line with reasonable length
  for (line in lines[1:min(10, length(lines))]) {
    if (nchar(line) > 20 && nchar(line) < 200) {
      return(str_squish(line))
    }
  }
  
  return("Header not detected")
}

#' Classify PDF layout based on text patterns and token positions
#' @param pdf_file Path to PDF file
#' @param verbose Whether to print detailed classification logic
classify_pdf_layout <- function(pdf_file, verbose = TRUE) {
  if (verbose) cat(sprintf("Classifying: %s\n", basename(pdf_file)))
  
  tryCatch({
    # Extract text and structured data from first page
    text <- pdf_text(pdf_file)[1]
    pdf_data <- pdf_data(pdf_file)[[1]]
    
    if (verbose) cat("  - Text extracted successfully\n")
    
    # Clean text for analysis
    text_lower <- tolower(text)
    text_clean <- str_squish(text_lower)
    
    # Extract metadata
    report_date <- extract_report_date(basename(pdf_file), text)
    sample_header <- get_sample_header(text)
    
    # Initialize classification variables
    layout <- "unknown"
    notes <- character()
    
    # Pattern-based classification (primary method)
    
    # Look for header line containing column names to determine layout
    header_line <- ""
    lines <- strsplit(text, "\n")[[1]]
    lines <- str_trim(lines)
    
    # Find the header line (contains multiple column keywords)
    header_keywords <- c("country", "mission", "description", "personnel", "post", "male", "female", "total")
    
    for (line in lines[1:min(15, length(lines))]) {
      line_lower <- tolower(line)
      keyword_count <- sum(sapply(header_keywords, function(kw) grepl(kw, line_lower)))
      if (keyword_count >= 3) {
        header_line <- line_lower
        break
      }
    }
    
    if (verbose) cat(sprintf("  - Header line: %s\n", str_squish(header_line)))
    
    # Classification based on header structure
    if (nchar(header_line) > 0) {
      # Check specific patterns in order
      
      # Pattern A: Country appears first in header (older format)
      # "Country   UN Mission   Description"
      if (grepl("^\\s*country.*mission.*description", header_line) ||
          grepl("^\\s*country.*un mission.*description", header_line)) {
        layout <- "A_mission_country"
        notes <- c(notes, "Header pattern: Country appears first, then Mission")
        if (verbose) cat("  - Layout A detected: Country-first header pattern\n")
      }
      
      # Pattern B: Mission appears first in header (newer format)
      # "Mission   Country   Description" or "Mission   Country   Personnel Type"
      else if (grepl("^\\s*mission.*country", header_line)) {
        layout <- "B_country_post"
        notes <- c(notes, "Header pattern: Mission appears first, then Country")
        if (verbose) cat("  - Layout B detected: Mission-first header pattern\n")
      }
      
      # Pattern C: Special case with "country | un mission | description"
      else if (grepl("country.*\\|.*un mission.*\\|.*description", header_line)) {
        layout <- "C_country_unmission"
        notes <- c(notes, "Header pattern: Country | UN Mission | Description with separators")
        if (verbose) cat("  - Layout C detected: Pipe-separated header pattern\n")
      }
      
      # Fallback: Use text position analysis
      else if (grepl("mission", header_line) && grepl("country", header_line)) {
        mission_pos <- str_locate(header_line, "mission")[1, 1]
        country_pos <- str_locate(header_line, "country")[1, 1]
        
        if (!is.na(mission_pos) && !is.na(country_pos)) {
          if (country_pos < mission_pos) {
            layout <- "A_mission_country"
            notes <- c(notes, sprintf("Position analysis: Country (pos=%d) before Mission (pos=%d)", country_pos, mission_pos))
            if (verbose) cat("  - Layout A detected: Country before Mission in header\n")
          } else {
            layout <- "B_country_post"
            notes <- c(notes, sprintf("Position analysis: Mission (pos=%d) before Country (pos=%d)", mission_pos, country_pos))
            if (verbose) cat("  - Layout B detected: Mission before Country in header\n")
          }
        }
      }
    }
    
    # Secondary method: Use pdf_data token positions for more precise analysis
    if (layout == "unknown" && nrow(pdf_data) > 0) {
      if (verbose) cat("  - Trying token position analysis...\n")
      
      # Find tokens containing key words
      mission_tokens <- pdf_data[grepl("mission", tolower(pdf_data$text)), ]
      country_tokens <- pdf_data[grepl("country", tolower(pdf_data$text)), ]
      
      if (nrow(mission_tokens) > 0 && nrow(country_tokens) > 0) {
        # Use the first occurrence of each term (likely in header)
        mission_x <- mission_tokens$x[1]
        country_x <- country_tokens$x[1]
        
        if (mission_x < country_x) {
          layout <- "A_mission_country"
          notes <- c(notes, sprintf("Token position analysis: mission (x=%d) before country (x=%d)", mission_x, country_x))
          if (verbose) cat(sprintf("  - Layout A detected via tokens: mission (x=%d) < country (x=%d)\n", mission_x, country_x))
        } else {
          layout <- "C_country_unmission"
          notes <- c(notes, sprintf("Token position analysis: country (x=%d) before mission (x=%d)", country_x, mission_x))
          if (verbose) cat(sprintf("  - Layout C detected via tokens: country (x=%d) < mission (x=%d)\n", country_x, mission_x))
        }
      }
    }
    
    # Fallback heuristics based on file year patterns (observed from sample analysis)
    if (layout == "unknown") {
      year_match <- str_extract(basename(pdf_file), "\\d{4}")
      if (!is.na(year_match)) {
        year <- as.numeric(year_match)
        # Based on observed patterns:
        # 2015-2018: Country first in header (Layout A)
        # 2019+: Mission first in header (Layout B)
        if (year <= 2018) {
          layout <- "A_mission_country"
          notes <- c(notes, sprintf("Fallback: files from %d typically use Country-first layout (A)", year))
          if (verbose) cat(sprintf("  - Layout A assigned by year fallback (%d)\n", year))
        } else {
          layout <- "B_country_post"
          notes <- c(notes, sprintf("Fallback: files from %d typically use Mission-first layout (B)", year))
          if (verbose) cat(sprintf("  - Layout B assigned by year fallback (%d)\n", year))
        }
      }
    }
    
    if (verbose) cat(sprintf("  - Final classification: %s\n", layout))
    
    return(tibble(
      filename = basename(pdf_file),
      layout = layout,
      report_date = report_date,
      sample_header_line = sample_header,
      notes = paste(notes, collapse = "; ")
    ))
    
  }, error = function(e) {
    if (verbose) cat(sprintf("  - Error processing file: %s\n", e$message))
    return(tibble(
      filename = basename(pdf_file),
      layout = "error",
      report_date = NA_character_,
      sample_header_line = "Error reading PDF",
      notes = paste("Error:", e$message)
    ))
  })
}

#' Main function to classify all UN PDF files in repository
#' @param repo_path Path to repository root
#' @param output_path Path for output CSV file
#' @param verbose Whether to print detailed progress
classify_all_pdfs <- function(repo_path = ".", output_path = "outputs/pdf_layouts.csv", verbose = TRUE) {
  
  cat("PDF Layout Classifier for UN Country Contributions\n")
  cat("================================================\n\n")
  
  # Find all matching PDF files
  pdf_files <- list.files(
    path = repo_path, 
    pattern = "^UN_country_contributions_.*\\.pdf$", 
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(pdf_files) == 0) {
    stop("No PDF files matching pattern found in repository")
  }
  
  cat(sprintf("Found %d PDF files to classify\n\n", length(pdf_files)))
  
  # Classify each PDF
  results <- map_dfr(pdf_files, ~classify_pdf_layout(.x, verbose = verbose))
  
  # Create output directory if it doesn't exist
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save results
  write_csv(results, output_path)
  cat(sprintf("\nResults saved to: %s\n", output_path))
  
  # Print summary
  cat("\nSUMMARY\n")
  cat("=======\n")
  layout_counts <- table(results$layout)
  for (layout in names(layout_counts)) {
    cat(sprintf("%-20s: %d files\n", layout, layout_counts[layout]))
  }
  
  # List unknown files
  unknown_files <- results$filename[results$layout == "unknown"]
  if (length(unknown_files) > 0) {
    cat(sprintf("\nFiles labeled as unknown (%d):\n", length(unknown_files)))
    for (file in unknown_files) {
      cat(sprintf("  - %s\n", file))
    }
  }
  
  # List error files
  error_files <- results$filename[results$layout == "error"]
  if (length(error_files) > 0) {
    cat(sprintf("\nFiles with errors (%d):\n", length(error_files)))
    for (file in error_files) {
      cat(sprintf("  - %s\n", file))
    }
  }
  
  cat(sprintf("\nClassification complete! Check %s for detailed results.\n", output_path))
  
  return(results)
}

# Run classification if script is executed directly
if (!interactive()) {
  classify_all_pdfs()
}