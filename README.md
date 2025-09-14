# UN Country Contributions PDF Parser

A comprehensive R script that parses UN country contributions PDF files and extracts structured data about peacekeeping personnel deployments.

## Overview

This script can process 126+ UN PDF files spanning 2015-2025 and handles three different PDF formats that evolved over time. It extracts key information including Year, Mission, Country/Nationality, Personnel Type, Female count, Male count, and Total count.

## Features

- **Multi-format support**: Handles three different PDF layouts (2015-2018, 2019-2022, 2023+)
- **Diagnostic tools**: Inspect PDF structure and content for debugging
- **Data validation**: Cleans and validates extracted data
- **Error handling**: Robust error handling with progress tracking
- **CSV export**: Exports structured data to CSV format
- **Visual inspection**: Tools to understand PDF formatting
- **Sample outputs**: Provides verification samples

## Requirements

- R (version 4.0+)
- Required R packages: `stringr`, `dplyr`, `readr`, `tibble`, `purrr`, `lubridate`
- System tools: `pdftotext` (from poppler-utils)

## Installation

1. Install R and required system tools:
```bash
sudo apt-get install r-base poppler-utils
```

2. The script will automatically install required R packages when first run.

## Usage

### Basic Usage

```r
# Load the script
source('parse_un_contributions.R')

# Run complete workflow (processes all PDF files)
main()
```

### Advanced Usage

```r
# Setup packages manually
setup_packages()

# Process specific number of files
results <- parse_all_pdfs(max_files = 10)

# Process all files
results <- parse_all_pdfs()

# Export to CSV
csv_file <- export_to_csv(results)

# Generate sample output for verification
generate_sample_output(results)
```

### Diagnostic Functions

```r
# Inspect raw PDF content
inspect_pdf_raw('UN_country_contributions_2023_01.pdf')

# Analyze table structure
text_lines <- extract_pdf_text('UN_country_contributions_2023_01.pdf')
structure_info <- analyze_table_structure(text_lines)

# Process single file for testing
result <- parse_single_pdf('UN_country_contributions_2015_01.pdf')
```

## PDF Formats Supported

### Format 2015-2018
- Header: "UN Mission's Summary detailed by Country"
- Country names on separate lines
- Mission data with indented personnel information
- Columns: Mission, Description, M, F, Totals

### Format 2019-2022
- Header: "Summary of Contributions to UN Peacekeeping by Country and Post"
- Numbered country entries
- Columns: Country Name, POST, MALE, FEMALE, TOTAL

### Format 2023+
- Header: "Contribution of Uniformed Personnel to UN by Mission, Country, and Personnel Type"
- Mission-based organization
- Columns: Mission, Country, Personnel Type, Male, Female, Total

## Data Structure

The extracted data contains the following columns:
- `Year`: Year from filename (numeric)
- `Month`: Month from filename (numeric)
- `Mission`: UN Mission code (e.g., UNMISS, MONUSCO)
- `Country`: Contributing country name
- `Personnel_Type`: Type of personnel (Individual Police, Experts on Mission, Contingent Troops, etc.)
- `Male`: Number of male personnel (numeric)
- `Female`: Number of female personnel (numeric)
- `Total`: Total personnel count (numeric)

## Data Validation

The script includes automatic data validation that:
- Removes records with invalid personnel types
- Filters out header/footer text parsed as data
- Validates that Male + Female = Total (allows 1 unit discrepancy)
- Cleans country names and removes very short entries
- Reports validation statistics

## Output Files

- **CSV Export**: `un_contributions_YYYYMMDD_HHMMSS.csv`
- **Sample Output**: Console display with statistics and sample records
- **Progress Tracking**: Real-time processing status

## Example Output

```
PROCESSING SUMMARY:
Total files processed: 126
Files with errors: 2
Total records extracted: 45,678
Date range: 2015 - 2025
Countries found: 142
Missions found: 28

TOP 10 COUNTRIES BY TOTAL PERSONNEL:
       Country Total
    Bangladesh  8,924
         India  7,532
      Pakistan  6,789
     Indonesia  5,421
         Nepal  4,987
```

## Error Handling

The script includes comprehensive error handling:
- PDF extraction failures are logged and skipped
- Invalid data is cleaned with detailed reporting
- Progress is tracked and errors are counted
- Individual file failures don't stop batch processing

## Performance

- Processes ~5-10 files per minute depending on PDF size
- Memory efficient with incremental processing
- Progress indicators for long-running operations
- Validation steps optimize data quality

## Troubleshooting

### Common Issues

1. **pdftotext not found**: Install poppler-utils
2. **R packages missing**: Script auto-installs or install manually
3. **Permission errors**: Ensure write access to output directory
4. **PDF format not recognized**: Check PDF format manually with diagnostic functions

### Debug Mode

```r
# Test single file processing
result <- parse_single_pdf('problematic_file.pdf')

# Inspect raw PDF content
inspect_pdf_raw('problematic_file.pdf', lines_to_show = 100)

# Check format detection
text_lines <- extract_pdf_text('problematic_file.pdf')
format_type <- identify_pdf_format(text_lines)
```

## Contributing

To extend the parser for new PDF formats:
1. Add format detection logic in `identify_pdf_format()`
2. Create new parsing function following existing patterns
3. Update the main parsing switch statement
4. Add validation rules specific to the new format

## License

This script is provided as-is for research and analysis purposes.