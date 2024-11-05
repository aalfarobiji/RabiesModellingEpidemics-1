# Collapse the results of the previous step into a single data frame
# Load necessary library
library(data.table)
library(writexl)

# Fungsi cbind.fill
cbind.fill <- function(...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(NA, n - nrow(x), ncol(x)))))
}

# Set the base path where the folders are located
base_path <- "G:/WUR/Thesis/Data/Model with HH/Fatukopa/OzRab/results/default_results"


# List all directories that match the pattern 'result_'
folder_list <- list.dirs(path = base_path, full.names = TRUE, recursive = FALSE)
result_folders <- grep("result_", folder_list, value = TRUE)

# Initialize an empty data frame or data table to store the combined data
data_frames <- list()

# Loop through each folder and read the 'nb_secCases.txt' file
for (folder in result_folders) {
  file_path <- file.path(folder, "nb_secCases.txt")
  if (file.exists(file_path)) {
    # Read the file
    data <- fread(file_path, select = c(1,2))
    
    # Append data frame to the list
    data_frames[[length(data_frames) + 1]] <- data
  }
}

# Combine data frames using cbind.fill
combined_data <- do.call(cbind.fill, data_frames)

# View the combined data
print(combined_data)

# Convert matrix to data frame
combined_df <- as.data.frame(combined_data)

# Write the combined data to an Excel file
write_xlsx(combined_df, "G:/WUR/Thesis/Data/Model with HH/Fatukopa/OzRab/scenario/Indonesia/allreport(nb_secCases)_Fatukopa.xlsx")
