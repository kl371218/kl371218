#!/usr/bin/env Rscript

# Example runner script for PDF layout classification
# This script demonstrates how to run the classifier on UN PDF files

library(dplyr)

# Source the main classifier script
source("scripts/classify_pdf_layouts.R")

cat("UN PDF Layout Classification Example\n")
cat("===================================\n\n")

# Option 1: Run on a small sample of files for quick testing
run_sample_classification <- function(sample_size = 10) {
  cat(sprintf("Running classification on sample of %d files...\n\n", sample_size))
  
  # Find all PDF files
  pdf_files <- list.files(
    path = ".", 
    pattern = "^UN_country_contributions_.*\\.pdf$", 
    full.names = TRUE
  )
  
  if (length(pdf_files) == 0) {
    stop("No PDF files found in repository")
  }
  
  # Select a representative sample across different years
  sample_files <- head(pdf_files[order(pdf_files)], sample_size)
  
  cat("Sample files selected:\n")
  for (file in sample_files) {
    cat(sprintf("  - %s\n", basename(file)))
  }
  cat("\n")
  
  # Classify each file individually for demonstration
  results <- tibble()
  for (file in sample_files) {
    result <- classify_pdf_layout(file, verbose = TRUE)
    results <- bind_rows(results, result)
    cat("\n")
  }
  
  # Save sample results
  write_csv(results, "outputs/pdf_layouts_sample.csv")
  cat(sprintf("Sample results saved to: outputs/pdf_layouts_sample.csv\n\n"))
  
  # Print summary
  cat("SAMPLE SUMMARY\n")
  cat("==============\n")
  layout_counts <- table(results$layout)
  for (layout in names(layout_counts)) {
    cat(sprintf("%-20s: %d files\n", layout, layout_counts[layout]))
  }
  
  return(results)
}

# Option 2: Run full classification on all files
run_full_classification <- function() {
  cat("Running full classification on all PDF files...\n\n")
  results <- classify_all_pdfs(verbose = TRUE)
  return(results)
}

# Main execution
if (!interactive()) {
  # Check command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0 && args[1] == "full") {
    # Run full classification
    results <- run_full_classification()
  } else {
    # Run sample classification by default
    sample_size <- if (length(args) > 0) as.numeric(args[1]) else 10
    results <- run_sample_classification(sample_size)
  }
  
  cat("\nExample run complete!\n")
  cat("\nUsage notes:\n")
  cat("- Run 'Rscript scripts/run_classification_example.R' for sample of 10 files\n")
  cat("- Run 'Rscript scripts/run_classification_example.R 20' for sample of 20 files\n")
  cat("- Run 'Rscript scripts/run_classification_example.R full' for all files\n")
} else {
  cat("This script can be run interactively or from command line:\n")
  cat("- run_sample_classification(10)  # for sample\n")
  cat("- run_full_classification()      # for all files\n")
}