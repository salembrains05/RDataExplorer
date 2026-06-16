# ============================================================
# R Data Explorer
# Author: Cynthia Moyo
# Description:
#   A simple console-based R program that loads a CSV file,
#   displays summary statistics, filters data, generates random
#   numbers, creates a histogram plot, and saves results to a file.
# ============================================================

# ------------------------------------------------------------
# Function: load_data
# Purpose: Loads a CSV file into a data frame.
# ------------------------------------------------------------
load_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Error: File does not exist.")
  }
  data <- read.csv(file_path)
  return(data)
}

# ------------------------------------------------------------
# Function: summarize_data
# Purpose: Prints summary statistics for numeric columns.
# ------------------------------------------------------------
summarize_data <- function(df) {
  cat("\n=== Summary Statistics ===\n")
  print(summary(df))
}

# ------------------------------------------------------------
# Function: filter_data
# Purpose: Filters rows based on a numeric column and threshold.
# ------------------------------------------------------------
filter_data <- function(df, column, threshold) {
  if (!column %in% names(df)) {
    stop("Error: Column not found in dataset.")
  }
  filtered <- df[df[[column]] > threshold, ]
  return(filtered)
}

# ------------------------------------------------------------
# Function: generate_random_numbers
# Purpose: Generates a vector of random numbers.
# ------------------------------------------------------------
generate_random_numbers <- function(count, min_val, max_val) {
  nums <- runif(count, min = min_val, max = max_val)
  return(nums)
}

# ------------------------------------------------------------
# Function: create_histogram
# Purpose: Creates a histogram for a numeric column.
# ------------------------------------------------------------
create_histogram <- function(df, column) {
  if (!column %in% names(df)) {
    stop("Error: Column not found.")
  }
  hist(df[[column]],
       main = paste("Histogram of", column),
       xlab = column,
       col = "skyblue",
       border = "black")
}

# ------------------------------------------------------------
# Function: save_results
# Purpose: Saves text output to a file.
# ------------------------------------------------------------
save_results <- function(text, file_path) {
  write(text, file = file_path)
  cat("\nResults saved to:", file_path, "\n")
}

# ------------------------------------------------------------
# Function: main_menu
# Purpose: Displays the menu and handles user input.
# ------------------------------------------------------------
main_menu <- function() {
  cat("=====================================\n")
  cat("        R DATA EXPLORER TOOL         \n")
  cat("=====================================\n")

  # file_path <- readline("Enter CSV file path: ")
file_path <- "C:/Users/Cee/Documents/RDataExplorer/sample.csv"


  # Load dataset
  df <- tryCatch({
    load_data(file_path)
  }, error = function(e) {
    cat("Failed to load file:", e$message, "\n")
    return(NULL)
  })

  if (is.null(df)) return()

  repeat {
    cat("\nChoose an option:\n")
    cat("1. View summary statistics\n")
    cat("2. Filter data\n")
    cat("3. Generate random numbers\n")
    cat("4. Create histogram\n")
    cat("5. Save summary to file\n")
    cat("6. Exit\n")

    choice <- readline("Enter choice: ")

    if (choice == "1") {
      summarize_data(df)

    } else if (choice == "2") {
      column <- readline("Enter column name: ")
      threshold <- as.numeric(readline("Enter threshold value: "))
      filtered <- filter_data(df, column, threshold)
      print(filtered)

    } else if (choice == "3") {
      count <- as.numeric(readline("How many numbers? "))
      min_val <- as.numeric(readline("Minimum value: "))
      max_val <- as.numeric(readline("Maximum value: "))
      nums <- generate_random_numbers(count, min_val, max_val)
      print(nums)

    } else if (choice == "4") {
      column <- readline("Enter column name for histogram: ")
      create_histogram(df, column)

    } else if (choice == "5") {
      output <- capture.output(summary(df))
      save_results(output, "summary_output.txt")

    } else if (choice == "6") {
      cat("Exiting program...\n")
      break

    } else {
      cat("Invalid choice. Try again.\n")
    }
  }
}

# ------------------------------------------------------------
# Run the program
# ------------------------------------------------------------
main_menu()
