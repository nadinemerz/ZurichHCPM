library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(gridExtra)
library(hhi)
library(tibble)
setwd("04_Exploratory Research HHI Zurich:Bern:Geneva")


## Load case number data for mamma-resections, vaginal births & heart failure in ZH, BE & GE (Source: Qualitätsindikatoren Abfrage)
path <- "02_Datasets/Fallzahlen_ZHBEGE.xlsx"
sheet_names <- excel_sheets(path)
df_list <- list()
for(i in 1:length(sheet_names)){ 
  df <- read_excel(path, sheet = sheet_names[i])
  for(col in 2:ncol(df)) {  # Convert all columns (except the first one) to numeric where possible
    df[[col]] <- suppressWarnings(ifelse(is.na(as.numeric(as.character(df[[col]]))), NA, df[[col]])) # Replace non-convertible values (e.g. "<10") with NA
    df[[col]] <- as.numeric(df[[col]])
  }
  df_list[[i]] <- df
}
names(df_list) <- sheet_names 
list2env(df_list, .GlobalEnv)



## Clean the naming of the hospital column by removing any characters before and including "– "
for (i in seq_along(df_list)) {
  df_list[[i]]$Hospital <- sub(".*–\\s*", "", df_list[[i]]$Hospital)
}

list2env(df_list, .GlobalEnv)


# -------------------------------------
# TABLES & GRAPHS WITH RAW DATA
# -------------------------------------

# Create output folder
save_path <- "04_Exploratory Research HHI Zurich:Bern:Geneva/Tables Raw"
if (!dir.exists(save_path)) {
  dir.create(save_path)
}

## Tables: case numbers per hospital
for(i in 1:length(df_list)){
  table_grob <- tableGrob(df_list[[i]])
  pdf_file <- file.path(save_path, paste0("table_", names(df_list[i]), ".pdf"))
  ggsave(pdf_file, table_grob, width = 15, height = 11)
}

## Graphs: case numbers per hospital
save_path <- "04_Exploratory Research HHI Zurich:Bern:Geneva/Graphs Raw"
if (!dir.exists(save_path)) {
  dir.create(save_path)
}

# Predefine titles
titles <- c(
  "Zurich: Vaginal Birth Cases", "Zurich: Resections of the Mamma in Breast Cancer Cases",
  "Zurich: Heart Failure Cases", "Zurich: Stroke Cases", "Bern: Vaginal Birth Cases",
  "Bern: Resections of the Mamma in Breast Cancer Cases", "Bern: Heart Failure Cases",
  "Bern: Stroke Cases", "Geneva: Vaginal Birth Cases", "Geneva: Resections of the Mamma in Breast Cancer Cases",
  "Geneva: Heart Failure Cases", "Geneva: Stroke Cases"
)

# Generate and save plots
for (i in seq_along(df_list)) {
  # Exclude "total" rows and drop rows with NA in the years columns
  df <- df_list[[i]] %>%
    filter(Hospital != "Total") %>%
    drop_na(`2008`:`2019`)
  
  # Reshape the data for plotting
  df <- reshape2::melt(df, id.vars = "Hospital", variable.name = "Year", value.name = "Number of Cases")
  
  # Convert Year and Number of Cases to numeric (safety)
  df$Year <- as.numeric(as.character(df$Year))
  df$`Number of Cases` <- as.numeric(as.character(df$`Number of Cases`))
  
  # Create the plot
  p <- ggplot(df, aes(x = Year, y = `Number of Cases`, group = Hospital, color = Hospital)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
    labs(title = titles[i], x = "Year", y = "Number of Cases") +
    scale_x_continuous(breaks = seq(2008, 2019, by = 2)) +
    theme(legend.text = element_text(size = 10), 
          legend.title = element_blank(), 
          legend.position = "top", 
          plot.margin = unit(c(1, 1, 1, 2), "cm"),
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5))

  ggsave(file.path(save_path, paste0("plot_", names(df_list)[i], ".png")), plot = p, width = 18, height = 10, dpi = 300)
}



# -------------------------------------
# PRE-PROCESSING FOR HERFINDAHL-HIRSCHMANN INDEX
# -------------------------------------

# Calculating each hospitals yearly share of total patients in a canton
for (i in seq_along(df_list)) {
  total_row <- df_list[[i]][df_list[[i]]$Hospital == "Total",]
  
  for (year in 2008:2019) {
    year_col <- as.character(year)
    share_col <- paste0(year, "_share")
    
    if (year_col %in% names(df_list[[i]])) {
      df_list[[i]] <- df_list[[i]] %>%
        mutate(!!share_col := .data[[year_col]] / total_row[[year_col]] * 100) %>%
        select(Hospital, sort(setdiff(names(.), "Hospital")))
    }
  }
}

list2env(df_list, .GlobalEnv)

# Removing "Total" Rows
df_list <- lapply(df_list, function(df) df[df$Hospital != "Total",])

list2env(df_list, .GlobalEnv)

# Creating Yearly Subsets and Removing NAs
subset_list <- list()
subset_names <- c()

for (i in seq_along(df_list)) {
  df <- df_list[[i]]
  
  for (year in 2008:2019) {
    subset_df <- df[, c(1, (year - 2008) * 2 + 2:(2 + 1))]
    subset_df <- subset_df[complete.cases(subset_df), , drop = FALSE]
    
    subset_name <- paste0(names(df_list)[i], "_", year)
    
    # Convert subset_df to a regular data frame before assigning to subset_list
    subset_list[[subset_name]] <- as.data.frame(subset_df)
    subset_names <- c(subset_names, subset_name)
    
    print(paste0("Subset data frame:", subset_name))
    print(dim(subset_df))
  }
}

list2env(subset_list, .GlobalEnv)


# Name all Share Columns in the Subsets v_shares
for (name in names(subset_list)) {
  df <- get(name)
  year <- as.numeric(str_extract(name, "\\d{4}"))
  
  df <- df %>%
    mutate(v_shares = .data[[paste0(year, "_share")]]) %>%
    select(-one_of(paste0(year, "_share")))
  
  assign(name, df, envir = .GlobalEnv)
}


# Validating the sums & subtracting from largest share if they do not add up to 100%
invalid_subsets <- c()

for (name in names(subset_list)) {
  df <- get(name)
  sum_shares <- sum(df$v_shares, na.rm = TRUE)
  
  if (abs(sum_shares - 100) > 0) {
    invalid_subsets <- c(invalid_subsets, name)
    max_share_index <- which.max(df$v_shares)
    
    # Adjust the largest v_share to make sum_shares equal to 100
    df$v_shares[max_share_index] <- df$v_shares[max_share_index] + (100 - sum_shares)
    
    assign(name, df, envir = .GlobalEnv)
  }
}
if (length(invalid_subsets) > 0) {
  print("These subsets did not sum up to 100 in the 'v_shares' column and have been adjusted:")
  print(invalid_subsets)
} else {
  print("All subsets sum up to 100 in the 'v_shares' column.")
}




# -------------------------------------
# CALCULATING HERFINDAHL-HIRSCHMANN INDEX
# -------------------------------------

# Add HHI to Each Subset and Create HHI Vectors
vectors_list <- list()

for (name in names(subset_list)) {
  df <- get(name)
  
  # Calculate and add HHI to the data frame
  new_hhi_value <- hhi(df, "v_shares")  # Compute HHI value
  new_row <- setNames(list("HHI", NA, new_hhi_value), c("Hospital", names(df)[2], "v_shares"))
  
  assign(name, rbind(df, new_row), envir = .GlobalEnv)  # Add new row to df
  
  # Construct HHI vector
  components <- strsplit(name, "_")[[1]]
  vector_name <- paste("hhi", components[1], components[2], sep = "_")
  
  vectors_list[[vector_name]] <- c(vectors_list[[vector_name]], new_hhi_value)

}
# List Vectors in Environment
list2env(vectors_list, envir = .GlobalEnv)

# Clean Environment
rm(list = c(subset_names, "new_row", "df", "df_list", "subset_df", "total_row"))



# -------------------------------------
# CREATE HHI DATA FRAMES
# -------------------------------------

years <- 2008:2019
headers <- c("canton", years)
rows <- c("Zurich", "Bern", "Geneva")
diseases <- c("Geburten", "Brustkrebs", "Herzinsuffizienz", "Stroke")
canton_codes <- c("ZH", "BE", "GE")
df_names <- paste0("hhi_", diseases)
dfs <- list()

for (j in seq_along(diseases)) {
  df <- data.frame(matrix(ncol = length(headers), nrow = length(rows)), stringsAsFactors = FALSE)
  names(df) <- headers
  df$canton <- rows
  
  for (i in seq_along(canton_codes)) {
    vec_name <- paste0("hhi_", canton_codes[i], "_", diseases[j])
    
    if (exists(vec_name)) {
      df[i, -1] <- get(vec_name)
    } else {
      warning(paste("Vector", vec_name, "does not exist."))
    }
  }
  
  dfs[[df_names[j]]] <- df
}

# Print names and assign data frames to global environment
print(names(dfs))
list2env(dfs, envir = .GlobalEnv)


# -------------------------------------
# HHI GRAPHS
# -------------------------------------

# Create output folder
save_path <- "04_Exploratory Research HHI Zurich:Bern:Geneva/Linear Graphs HHI"
if (!dir.exists(save_path)) {
  dir.create(save_path)
}

# Titles for the plots
titles <- c("HHI: Vaginal Births", 
            "HHI: Resections of the Mamma in Breast Cancer", 
            "HHI: Heart Failures", 
            "HHI: Strokes")

# Define dataframes and their corresponding names
dataframes <- list(hhi_Geburten, hhi_Brustkrebs, hhi_Herzinsuffizienz, hhi_Stroke)
dataframe_names <- c("hhi_Geburten", "hhi_Brustkrebs", "hhi_Herzinsuffizienz", "hhi_Stroke")

# Loop through dataframes, create and save plots
for (i in seq_along(dataframes)) {
  df <- dataframes[[i]]  # Extract dataframe
  
  # Transform data to long format and filter for desired year range
  df_long <- df %>% 
    pivot_longer(cols = -canton, names_to = "year", values_to = "hhi") %>% 
    mutate(year = as.numeric(year)) %>%  # Convert year to numeric
    filter(between(year, 2008, 2019))  # Filter for the specified year range
  
  # Generate the plot
  p <- ggplot(df_long, aes(x = year, y = hhi, color = canton, group = canton)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "black") +
    labs(title = titles[i], x = "Year", y = "HHI", color = "Cantons") +
    theme_light() +
    theme(
      plot.title = element_text(size = 22),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      panel.grid.major = element_line(color = "white", size = 1.1),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f2f2f2")
    )  +
    scale_x_continuous(
      breaks = seq(2008, 2019, by = 1),
      limits = c(2008, 2019),
      minor_breaks = NULL,
      expand = c(0.01,0.01) 
    ) 
  
  
  # Save the plot
  ggsave(
    filename = file.path(save_path, paste0(dataframe_names[i], "_graph.png")),
    plot = p, 
    width = 15, 
    height = 10, 
    dpi = 300
  )
}



# -------------------------------------
# HHI TABLES
# -------------------------------------

# Create output folder
save_path <- "04_Exploratory Research HHI Zurich:Bern:Geneva/Tables HHI"
if (!dir.exists(save_path)) {
  dir.create(save_path)
}

# Create tables comparing the cantons based on their HHI values for each service group
for(i in seq_along(df_names)){
  
  # Retrieve the data frame using its name and round numerical values
  df <- get(df_names[i]) %>% 
    mutate(across(where(is.numeric), round))
  
  # Create a table & save it
  table_grob <- tableGrob(df)
  pdf_file_path <- file.path(save_path, paste0("table_", df_names[i], ".pdf"))
  ggsave(pdf_file_path, table_grob, width = 15, height = 11)
}

