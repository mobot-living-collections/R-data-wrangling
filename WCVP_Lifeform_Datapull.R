# Set the working directory
setwd("H:/Plant Records/Collections Development/Focus Regions/Global_WCVP")

# Read the pipe-separated file
wcvp_data <- read.csv("wcvp_names.csv", sep = "|")

# Check the structure of the data
str(wcvp_data)

# Print column names
print(colnames(wcvp_data))

# View the first few rows
head(wcvp_data)

# Access the lifeform_description column
lifeform_values <- wcvp_data$lifeform_description

# Print the first few values
print(head(lifeform_values))

# Get unique lifeform descriptions
unique_lifeforms <- unique(lifeform_values)

# Print unique values
print(unique_lifeforms)

# Count unique values
print(length(unique_lifeforms))

# Create a frequency table
lifeform_freq <- table(lifeform_values)

# Sort the frequency table in descending order
lifeform_freq_sorted <- sort(lifeform_freq, decreasing = TRUE)

# Print sorted frequency table
print(lifeform_freq_sorted)

# Save frequency table to CSV
write.csv(data.frame(lifeform = names(lifeform_freq_sorted), 
                     frequency = as.vector(lifeform_freq_sorted)), 
          "lifeform_frequencies.csv", row.names = FALSE)