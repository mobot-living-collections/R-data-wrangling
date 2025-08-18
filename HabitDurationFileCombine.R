#August 15, 2025
#script to combine output files from Chet and Lorraine's AI query for habit and duration data

library(readxl)
library(dplyr)
library(purrr)

# Get list of all Excel files in the directory
excel_files <- list.files(pattern = "\\.(xlsx|xls)$", full.names = TRUE)

# Function to read and process each file
read_and_process <- function(file_path) {
  # Read the Excel file without headers
  data <- read_excel(file_path, col_names = FALSE, col_types = "text")
  
  # check number of columns
  num_cols <- ncol(data)
  
  if (num_cols >= 4) {
    #select columns 2-4 (taxon, duration, habit)
    processed_data <- data[, 2:4]
    names(processed_data) <- c("taxon", "duration", "habit")
    
    #check if there's a 5th column for confidence level
    if (num_cols >= 5) {
      processed_data$confidence_level <- data[[5]]
    } else {
      processed_data$confidence_level <- NA
    }
  
  } else if (num_cols >=3) {
    #handle case where there are only 3 columns total
    processed_data <- data[, 2:3]
    names(processed_data) <- c("taxon", "duration")
    processed_data$habit <- NA
    processed_data$confidence_level <- NA
  } else {
    #handle case with fewer than 3 columns just in case
    warning(paste("File", basename(file_path), "has fever than 3 columns"))
    return(NULL)
  } 
  
  #add source file column
  processed_data$source_file <- basename(file_path)
  
  return(processed_data)
  
}

# Read and combine all files
data_list <- lapply(excel_files, read_and_process)

#remove any null entries
data_list <- data_list[!sapply(data_list, is.null)]

#combine all data frames
combined_data <- do.call(rbind, data_list)

#remove completely empty rows
empty_rows <- (is.na(combined_data$taxon) | combined_data$taxon == "") & 
              (is.na(combined_data$duration) | combined_data$duration == "") & 
              (is.na(combined_data$habit) | combined_data$habit == "")
combined_data <- combined_data[!empty_rows, ]

# View the result
head(combined_data)
print(paste("Total rows:", nrow(combined_data)))

write.csv(combined_data, "combined_data.csv", row.names = FALSE)

#test the dataset for duplicates

# Check for complete duplicates (all columns identical)
complete_duplicates <- duplicated(combined_data)
num_complete_duplicates <- sum(complete_duplicates)
print(paste("Number of complete duplicate rows:", num_complete_duplicates))

# View the duplicate rows if any exist
if (num_complete_duplicates > 0) {
  print("Complete duplicate rows:")
  print(combined_data[complete_duplicates, ])
  
}

# Check for duplicates based only on taxon, duration, habit (excluding source_file)
data_cols <- combined_data[, c("taxon", "duration", "habit")]
content_duplicates <- duplicated(data_cols)
num_content_duplicates <- sum(content_duplicates)
print(paste("Number of duplicate rows (ignoring source file):", num_content_duplicates))

# Summary of unique vs duplicate counts
print("Summary:")
print(paste("Total rows:", nrow(combined_data)))
print(paste("Unique rows (complete):", nrow(combined_data) - num_complete_duplicates))
print(paste("Unique rows (content only):", nrow(unique(data_cols))))

#remove complete duplicates (keeps first occurrence)
combined_data_no_complete_dups <- combined_data[!duplicated(combined_data), ]

#save as a csv
write.csv(combined_data_no_complete_dups, "combined_data_no_dups.csv", row.names = FALSE)
