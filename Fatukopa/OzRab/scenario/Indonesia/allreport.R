# Collapse the results of the previous step into a single data frame
# Load necessary library
library(data.table)

# Set the base path where the folders are located
base_path <- "G:/WUR/Thesis/Data/Model with HH/Fatukopa/OzRab/results/default_results"


# List all directories that match the pattern 'result_'
folder_list <- list.dirs(path = base_path, full.names = TRUE, recursive = FALSE)
result_folders <- grep("result_", folder_list, value = TRUE)

# Initialize an empty data frame or data table to store the combined data
combined_data <- data.table()

# Loop through each folder and read the 'measuresReport.txt' file
for (folder in result_folders) {
  file_path <- file.path(folder, "measuresReport.txt")
  if (file.exists(file_path)) {
    # Read the file
    data <- fread(file_path)
    # Combine the data
    combined_data <- rbindlist(list(combined_data, data), fill = TRUE)
  }
}

# View the combined data
print(combined_data)
writexl::write_xlsx(combined_data, "G:/WUR/Thesis/Data/Model with HH/Fatukopa/OzRab/scenario/Indonesia/allreport.xlsx")
