
# Loading required libraries for data manipulation, visualization, and analysis.
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(grid)
library(gridExtra)
library(tibble)
library(writexl)
library(Synth)
library(purrr)
library(scales)

# Setting the working directory to the folder containing quality indicator datasets for each canton.
# Note: For shared use, ensure this path is relative or set appropriately on your machine.
setwd("05_Descriptive Analyis All/Quality Indicators")

# Loading Quality Indicator Datasets for each canton
# [Note: The specific code for loading datasets needs to be added here.]


# Define service groups, cantons, dataset paths and names
groups <- c("Brustkrebs", "Geburt")
cantons <- c("AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL", "GR", "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ", "TG", "TI", "UR", "VS", "VD", "ZG", "ZH")
paths <- c("/Users/Nadine/Desktop/Thesis/02_Datasets/Qualitätsindikatoren Mamma-Resections & Births/Absolute Number/G12P._Geburt_all_abs.xlsx", 
           "/Users/Nadine/Desktop/Thesis/02_Datasets/Qualitätsindikatoren Mamma-Resections & Births/Absolute Number/G53P_Brustkrebs_all_abs.xlsx")
df_names <- cantons %>%
  outer(groups, paste, sep = "_") %>%  # First as matrix
  as.vector()  # Flatten the matrix to a vector


# Load each sheet as separate dataframe to directory, including basic data cleaning steps (from exploratory steps)
for (idx in seq_along(groups)) {
  path <- paths[idx]
  sheet_names <- excel_sheets(path)
  df_list <- list()
  for(i in 1:length(sheet_names)){
    df <- read_excel(path, sheet = sheet_names[i])
    
    # Replace "<10" with NA
    df <- df %>%
      mutate(across(where(is.character), 
                    ~str_replace(., "<\\d+", NA_character_))) %>% 
      mutate(across(-Hospital, as.numeric))  # Convert all columns except "Hospital" to numeric
    
    df_list[[i]] <- df
  }
  names(df_list) <- sheet_names
  
  # Remove everything before the first "-" (city name)
  for (i in seq_along(df_list)) {
    df_list[[i]]$Hospital <- sub(".*–\\s*", "", df_list[[i]]$Hospital)
    
  }
  list2env(df_list, .GlobalEnv)
}



# -------------------------------------
# DATA CLEANING AND HANDLING
# -------------------------------------


# -------------------------------------
#i) Removing non "Listenspitäler" & dealing with M&As during observation period (fuse retroatcively by summing up patient numbers)
# -------------------------------------

# Define the hospitals to merge or remove
# Note: Technically, dealing with M&As is irrelevant here because we are interested in the number of births with perineal tear & breast-conserving mamma-resections summed up over all hospitals
# This is only done to generate tables with Quality Indicators comparable to those from the indicators calculation

fusion_list <- list(
  "AG" = list(
    removals = c("Klinik Sonnenblick")
  ),
  "AR" = list(
    removals = c(),
    hospital_fusions = list(
      list(
        hospitals = c("Spitalverbund AR Spital Heiden", "Spitalverbund AR Spital Herisau", "Spitalverbund Appenzell Ausserrhoden"),
        new_hospital_name = "Spitalverbund Appenzell Ausserrhoden"
      )
    )
  ),
  "BE" = list(
    removals = c("Privatklinik Piano"),
    hospital_fusions = list(
      list(
        hospitals = c("Hirslanden Bern AG", "Klinik Permanence", "Salem-Spital"),
        new_hospital_name = "Hirslanden Bern AG"
      ),
      list(
        hospitals = c("Insel Gruppe AG (universitär)", "Insel Gruppe AG (nicht-universitär)"),
        new_hospital_name = "Insel Gruppe AG"
      ),
      list(
        hospitals = c("Klinik Sonnenhof AG", "Lindenhofgruppe AG", "Lindenhofspital"),
        new_hospital_name = "Lindenhofgruppe AG"
      )
    )
  ),
  "BL" = list(
    removals = c("Ita Wegman Geburtshaus", "Universitäts-Kinderspital beider Basel (BS/BL)"),
    hospital_fusions = list(
      list(
        hospitals = c("Kantonsspital Bruderholz", "Kantonsspital Laufen", "Kantonsspital Liestal", "Kantonsspital Baselland"),
        new_hospital_name = "Kantonsspital Baselland"
      )
    )
  ),
  "BS" = list(
    removals = c("Geburtshaus Basel", "Geburtsstätte Basel")
  ),
  "FR" = list(
    removals = c(),
    hospital_fusions = list(
      list(
        hospitals = c("HFR - Hôpital fribourgeois", "Hôpital fribourgeois - HFR Fribourg - Hôpital cantonal", 
                      "Freiburger Spital - HFR Meyriez-Murten", "Hôpital fribourgeois - HFR Riaz, Billens, Châtel-St-Denis", 
                      "Freiburger Spital - HFR Tafers"),
        new_hospital_name = "HFR - Hôpital fribourgeois"
      )
    )
  ),
  "GE" = list(
    removals = c("Clinique de Carouge AG", "Nouvelle Clinique Vert-Pré", "Clinique de Joli-Mont", "Clinique des Vergers SA"),
    hospital_fusions = list(
      list(
        hospitals = c("Les Hôpitaux Universitaires de Genève HUG", "HUG - Soins aigus"),
        new_hospital_name = "Les Hôpitaux Universitaires de Genève HUG"
      )
    )
  ),
  "GR" = list(
    removals = c("Geburtshaus Graubünden")
  ),
  "JU" = list(
    removals = c(),
    hospital_fusions = list(
      list(
        hospitals = c("Hôpital du Jura", "Hôpital du Jura, soins aigus"),
        new_hospital_name = "Hôpital du Jura"
      )
    )
  ),
  "NE" = list(
    removals = c("Hôpital Neuchâtelois HNE, soins palliatifs", "Clinique Volta SA"),
    hospital_fusions = list(
      list(
        hospitals = c("Fondation de l'Hôpital de la Providence", "Clinique Montbrillant SA", "Swiss Medical Network Hospitals SA Neuchâtel"),
        new_hospital_name = "Swiss Medical Network Hospitals SA Neuchâtel"
      )
    )
  ),
  "SG" = list(
    removals = c("Berit Klinik Goldach")
  ),
  "SO" = list(
    removals = c(),
    hospital_fusions = list(
      list(
        hospitals = c("Solothurner Spitäler AG", "Solothurner Spitäler AG, Akutspital"),
        new_hospital_name = "Solothurner Spitäler AG"
      )
    )
  ),
  "SZ" = list(
    removals = c("Aeskulap-Klinik")
  ),
  "TI" = list(
    removals = c("Ospedale Malcantonese Fondazione Giuseppe Rossi"),
    hospital_fusions = list(
      list(
        hospitals = c("Ospedale Regionale di Bellinzona e Valli - Sede Acquarossa", "EOC Ente ospedaliero cantonale", "Ospedale Regionale Bellinzona & Valli Sede di Bellinzona",
                      "Ospedale Regionale di Bellinzona e Valli - Sede Faido", "Ospedale Regionale di Locarno", "Cardiocentro Ticino (CCT)", "Ospedale Regionale di Lugano"),
        new_hospital_name = "EOC Ente ospedaliero cantonale"
      ),
      list(
        hospitals = c("Clinica Luganese Moncucco SA", "Clinica Luganese Moncucco SA Sede San Rocco"),
        new_hospital_name = "Clinica Luganese Moncucco SA"
      )
    )
  ),
  "VS" = list(
    removals = c(),
    hospital_fusions = list(
      list(
        hospitals = c("Hôpital du Valais - Centre hospitalier du centre du Valais (CHCVs)", "Hôpital du Valais - Centre Hospitalier du Centre du Valais (CHCVs), Soins aigus", "Hôpital du Valais Centre hospitalier du Valais Romand CHVR"),
        new_hospital_name = "Centre hospitalier du centre du Valais (CHCVs)"
      ),
      list(
        hospitals = c("Spital Wallis - Spitalzentrum Oberwallis (SZO)", "Spital Wallis - Spitalzentrum Oberwallis (SZO), Soins aigus"),
        new_hospital_name = "Spital Wallis - Spitalzentrum Oberwallis (SZO)"
      )
    )
  ),
  "VD" = list(
    removals = c(),
    hospital_fusions = list(
      list(
        hospitals = c("Hôpital du Chablais VD", "Clinique CIC Riviera", "Hôpital Riviera-Chablais Vaud-Valais"),
        new_hospital_name = "HRC"
      ),
      list(
        hospitals = c("Clinique Cecil SA", "Hirslanden Lausanne SA"),
        new_hospital_name = "Hirslanden Lausanne SA"
      )
    )
  ),
  "ZH" = list(
    removals = c("Klinik Pyramide Schwerzenbach", "Eulachklinik AG", "GSMN Schweiz AG Privatklinik Bethanien",
                 "Klinik Tiefenbrunnen AG", "GSMN Schweiz AG Privatklinik Lindberg", "Klinik Im Park", "Klinik Pyramide am See AG")
  )
)

# Define cleaning function incorporating fusion and removal
clean_df <- function(canton, group, fusion_list) {
  df_name <- paste(canton, group, sep = "_")
  df <- get(df_name)
  
  # If there are hospitals to remove in this canton
  if (length(fusion_list[[canton]]$removals) > 0) {
    df <- df %>% filter(!Hospital %in% fusion_list[[canton]]$removals)
  }
  
  # If there are hospital fusions in this canton
  if (length(fusion_list[[canton]]$hospital_fusions) > 0) {
    for (fusion in fusion_list[[canton]]$hospital_fusions) {
      # Merge specified hospitals and rename
      df_to_merge <- df %>% filter(Hospital %in% fusion$hospitals)
      df_merged <- summarise_all(df_to_merge, 
                                 function(x) ifelse(is.numeric(x), sum(x, na.rm = TRUE), 0))
      df_merged$Hospital <- fusion$new_hospital_name
      
      # Remove individual hospitals from original df and append merged version
      df <- df %>% filter(!Hospital %in% fusion$hospitals)
      df <- bind_rows(df, df_merged)
    }
  }
  return(df)
}


# Execute cleaning function for all cantons and groups 
for (canton in cantons) {
  for (group in groups) {
    print(paste("Processing", canton, group))
    cleaned_df <- clean_df(canton, group, fusion_list)
    assign(paste(canton, group, sep = "_"), cleaned_df, envir = .GlobalEnv)
  }
}


# -------------------------------------
#ii) Remove naming elements related to legal form
# -------------------------------------

for (canton in cantons) {
  for (group in groups) {
    df_name <- paste(canton, group, sep = "_")
    
    if(exists(df_name)) {
      df <- get(df_name)
      
      # Replace " AG", " GmbH", and " SA" with "" in the first column
      df[[1]] <- df[[1]] %>%
        stringr::str_replace_all(" AG", "") %>%
        stringr::str_replace_all(" GmbH", "") %>%
        stringr::str_replace_all(" SA", "")
      assign(df_name, df, envir = .GlobalEnv)
    }
  }
}


# -------------------------------------
#iii) Export clean dfs for storage
# -------------------------------------

base_path <- "/Users/Nadine/Desktop/Thesis/02_Datasets/Qualitätsindikatoren Mamma-Resections & Births/Absolute Number/Clean DFs (separate for each canton)"

for (df_name in df_names) {
  df <- get(df_name)
    
    if (exists(df_name)) {
      df <- get(df_name)
      
      # Construct the filename
      file_path <- file.path(base_path, paste0(df_name, ".xlsx"))
      
      # Write to Excel
      write_xlsx(df, file_path)
    }  
}


# ==================================================================
# AGGREGATE TO CANTONAL QUALITY INDICATORS
# ==================================================================
# Goal: Generate cantonal indicators by aggregating hospital-level data
# Steps:
# i. Aggregate affected patient numbers across hospitals
# ii. Retrieve clean patient volume datasets
# iii. Calculate the ratio of affected patients to total patients


# --------------------------------------------------
# i) Aggregate yearly "affected" numbers across hospitals for each canton
# --------------------------------------------------
# Sum values for each year across all hospitals resulting in 
# a dataframe with a single row per canton-group combination for the yearly total

for (df_name in df_names) {
  df <- get(df_name, envir = .GlobalEnv)
  
  # Convert columns (excluding first) to numeric
  df[,-1] <- lapply(df[,-1], function(x) as.numeric(as.character(x)))
  
  # Aggregate, ignoring NAs
  total_values <- colSums(df[, -1], na.rm = TRUE)
  
  # If all values for a year are NA, set the corresponding Total to NA
  for (col_name in colnames(df)[-1]) {
    if(all(is.na(df[[col_name]]))) {
      total_values[col_name] <- NA
    }
  }
  
  # Frame the "Total" row
  total_df <- data.frame(matrix(ncol = ncol(df), nrow = 1))
  colnames(total_df) <- colnames(df)
  total_df[1, 1] <- "Total"
  for(col_name in names(total_values)) {
    total_df[[col_name]] <- total_values[col_name]
  }
  
  # Store the updated dataframe
  assign(df_name, total_df, envir = .GlobalEnv)
}


# --------------------------------------------------
# ii) Load and aggregate yearly total patient numbers (denominator) 
# --------------------------------------------------
# Aggregation is done the same way as in (i) by summing up across hospitals per canton & year

# Load Excel files from the specified directory
files <- list.files(path = "/Users/Nadine/Desktop/Thesis/02_Datasets/Fallzahlen Mamma-Resections & Births/Clean DFs (separate for each canton)", pattern = "\\.xlsx$", full.names = TRUE)

for (file in files) {
  df_name <- tools::file_path_sans_ext(basename(file))
  df <- read_excel(file)
  
  # Append "_patients" suffix to the dataframe's name for distinction
  df_name_patients <- paste0(df_name, "_patients")
  
  # Convert all columns (excluding the first one) to numeric
  df[,-1] <- lapply(df[,-1], function(x) as.numeric(as.character(x)))
  
  # Aggregate data for each year, ignoring NAs
  total_values <- colSums(df[, -1], na.rm = TRUE)
  
  # Set the Total to NA for years with all NA values
  for (col_name in colnames(df)[-1]) {
    if(all(is.na(df[[col_name]]))) {
      total_values[col_name] <- NA
    }
  }
  
  # Create a new dataframe to store the aggregated "Total" row
  total_df <- data.frame(matrix(ncol = ncol(df), nrow = 1))
  colnames(total_df) <- colnames(df)
  total_df[1, 1] <- "Total"
  for(col_name in names(total_values)) {
    total_df[[col_name]] <- total_values[col_name]
  }
  
  # Save the aggregated dataframe with "_patients" suffix to the global environment
  assign(df_name_patients, total_df, envir = .GlobalEnv)
}



# --------------------------------------------------
# iii) Divide to get cantonal quality indicators
# --------------------------------------------------
# Perform step iii: Calculate the ratio of affected patients to total patients
# For each canton and group, the code fetches the relevant datasets and 
# divides the number of affected patients by the total patient count

# Loop through "_patients" dfs and use them as a denominator for the quality indicator
for (canton in cantons) {
  for (group in groups) {
    df_name <- paste(canton, group, sep = "_")
    df_name_patients <- paste(canton, group, "patients", sep = "_")
    
    # Check if both dataframes exist
    if (exists(df_name) && exists(df_name_patients)) {
      
      df <- get(df_name) # Retrieval of the main dataframe
      df_patients <- get(df_name_patients)
      
      # Display which df is being processed for transparency
      cat(paste0("Processing: ", df_name, "\n"))
      
      # RATIO CALCULATION
      # For each year, compute the ratio of values in df to df_patients
      for (col_name in colnames(df)[-1]) {
        if (col_name %in% colnames(df_patients)) {
          dividend <- df[[col_name]][1]
          divisor <- df_patients[[col_name]][1]
          
          # Display calculations being performed for transparency
          cat(paste0("Column: ", col_name, ", Dividend: ", dividend, ", Divisor: ", divisor, "\n"))
          
          # If the divisor is NA or zero, the quotient is set as NA
          if (is.na(divisor) || divisor == 0) {
            quotient <- NA
          } else {
            quotient <- round(dividend / divisor, 4)  # Quotient rounded to 4 decimal places
          }
          
          df[[col_name]][1] <- quotient
          
          # Display resultant value
          cat(paste0("Resultant Quotient: ", df[[col_name]][1], "\n"))
        }
      }
      
      # Update the main dataframe in the global environment
      assign(df_name, df, envir = .GlobalEnv)
    }
  }
}

# Clean up by removing the patient data frames from the environment after processing all groups for all cantons
for (df_name in df_names) {
  df_name_patients <- paste0(df_name, "_patients")
  if (exists(df_name_patients)) {
    rm(list=df_name_patients, envir=.GlobalEnv)
  }
}


# --------------------------------------------------
# iv) Consolidate data by service group into two wide dataframes
# --------------------------------------------------
# The goal is to gather the data for each group across all cantons and bind them row-wise
# Each resultant dataframe will contain all cantons' data for a particular group (Brustkrebs & Dammriss)

# Define consolidated df names based on service groups
df_names <- c("Brustkrebs_Resekt", "Geburt_Dammriss")

# Loop through each service group
for (group in groups) {
  
  new_df <- data.frame()  # Initialize an empty df for the group
  
  # Extract and bind data for each canton
  for (canton in sort(cantons)) {  # Ensure cantons are processed alphabetically
    df_name <- paste(canton, group, sep = "_")
    
    # Attempt to fetch the df and process; skip if not found
    tryCatch({
      df <- get(df_name, envir = .GlobalEnv)
      
      # Rename the first column to "canton" and update its value
      colnames(df)[1] = "canton"
      df$canton[1] = canton
      
      # Row-bind to the consolidating group df
      new_df <- rbind(new_df, df)
      
      # Clean up by removing the individual cantonxgroup dfs
      rm(list = df_name, envir = .GlobalEnv)
    }, error = function(e) {})
  }
  
  # Store the consolidated df in the global environment
  assign(df_names[which(groups == group)], new_df, envir = .GlobalEnv)
}


# -------------------------------------
#v) Export Quality Indicator tables
# -------------------------------------

# Create new folder if necessary
output_dir <- "/Users/Nadine/Desktop/Thesis/05_Descriptive Analyis All/Quality Indicators/QI Tables"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define function to save tables as PDF in percetage
save_table_to_pdf <- function(df, filename) {
  
  # Convert to percentage and replace NA values with blank character
  df_for_output <- df
  df_for_output[,-1] <- lapply(df_for_output[,-1], function(col) {
    sapply(col, function(x) {
      if (is.na(x)) {
        return("")  # Return empty string for NA values
      }
      pct <- x * 100 # Convert to percentage
      return(as.character(round(pct))) # Round and return
    })
  })
  
  # Create table without row names
  tg <- tableGrob(df_for_output, rows = NULL)
  
  # Save to PDF with annotation
  pdf(filename, width = 11, height = 8.5)  # Landscape for width optimization
  grid.draw(tg)
  grid.text("All values are in %", x = 0.7, y = 0.05, just = "left", gp = gpar(fontsize = 10))
  dev.off()
}


# Execute function to xport consolidated data frames to PDFs
for (name in df_names) {
  df_export <- get(name, envir = .GlobalEnv)
  
  # Define filename and save
  filepath <- file.path(output_dir, paste0(name, ".pdf"))
  save_table_to_pdf(df_export, filepath)
}


# Export consolidated data frames to Excel
write_xlsx(list(Brustkrebs_Resekt = get("Brustkrebs_Resekt", envir = .GlobalEnv), 
                Geburt_Dammriss = get("Geburt_Dammriss", envir = .GlobalEnv)), 
           path = file.path(output_dir, "QI per canton and year.xlsx"))



# -------------------------------------
#vi) Merge separate wide dfs to one long df for future for easier future manipulation
# -------------------------------------

# Melt the data frames to long format

# Define the melting function
melt_data <- function(df, value_name) {
  reshape2::melt(df, id.vars = "canton", variable.name = "year", value.name = value_name) %>%
    mutate(year = as.numeric(gsub("X", "", year)))
}

# Melt and merge
indicators_long_all <- Reduce(
  function(df1, df2) merge(df1, df2, by = c("canton", "year"), all.x = TRUE), 
  list(
    melt_data(Brustkrebs_Resekt, "brustkrebs_resekt"), 
    melt_data(Geburt_Dammriss, "geburt_dammriss")
  )
)

# Store the wide df in a list in case they are needed later
indicators_wide_all <- list(
  Brustkrebs_Resekt = get("Brustkrebs_Resekt"), 
  Geburt_Dammriss = get("Geburt_Dammriss")
)


# -------------------------------------
#vii) Filter out non-informative cantons (almost exclusively missing values)
# -------------------------------------

# Filter out those only with a combination of NA or 0 for all years
# Then filter out those cantons where all values are NA or 0 for both groups over all years
filtered_cantons_df <- indicators_long_all %>%
  group_by(canton) %>%
  summarize(all_na_or_zero = all(is.na(brustkrebs_resekt) | brustkrebs_resekt == 0) & 
              all(is.na(geburt_dammriss) | geburt_dammriss == 0))

# Print cantons being removed
removed_cantons <- filtered_cantons_df$canton[filtered_cantons_df$all_na_or_zero]
cat("Cantons being removed:\n")
print(removed_cantons)

# Add to removed cantons OW, UR & GL, where pre-2012 data is missing
removed_cantons <- union(removed_cantons, c("OW", "UR", "GL"))
cat("Updated Cantons being removed:\n")
print(removed_cantons)

# Filter the main data frame to exclude the removed cantons
indicators_long_all <- indicators_long_all %>% 
  filter(!canton %in% removed_cantons)

# Adjust the 'cantons' vector to remove the filtered-out cantons
cantons <- setdiff(cantons, removed_cantons)

# Clean up global environment before moving to descriptive stats
rm(list = setdiff(ls(), c("indicators_long_all", "indicators_wide_all", "cantons", "groups", "save_table_to_pdf")), envir = .GlobalEnv)



# -------------------------------------
# DESCRIPTIVE STATS
# -------------------------------------


# -------------------------------------
#i) Add reform start year as information to the long df by matching it with the respective canton
# -------------------------------------
# Define start years & a df to check that allocation worked as intended
starts <- c(2012, 2012, 2012, 2012, 2012, 2015, 2020, 2014, 2015, 2012, 2016, 2017, 2014, 2013, 2012, 2012, 2012, 2016, 2015, 2023, 2012, 2012)

canton_to_start_df <- data.frame(canton = cantons, start = starts)
print(canton_to_start_df)

# Assign start dates to cantons in main df
indicators_long_all <- indicators_long_all %>%
  mutate(start = starts[match(canton, cantons)])


# -------------------------------------
#ii) Create tables with descriptive stats 
# -------------------------------------

# Create a new dataframe just for this purpose excluding cantons that started the reform after 2019
indicators_filtered <- indicators_long_all %>% filter(start <= 2019)

# Define function containing logic to calculate the relevant metrics on the proportion indicators
compute_metrics <- function(data, variable) {
  data %>%
    group_by(canton, start) %>%
    summarise(
      # Pre- and Post-reform mean
      mean_pre = mean(if_else(year < first(start), !!sym(variable), NA_real_), na.rm = TRUE),
      mean_post = mean(if_else(year >= first(start), !!sym(variable), NA_real_), na.rm = TRUE),
      
      # Pre- and Post-reform standard deviation
      sd_pre = sd(if_else(year < first(start), !!sym(variable), NA_real_), na.rm = TRUE),
      sd_post = sd(if_else(year >= first(start), !!sym(variable), NA_real_), na.rm = TRUE),
      
      # In which year was the max / min indicators?
      max_year = if (all(is.na(!!sym(variable)))) NA_real_ else year[which.max(!!sym(variable))],
      min_year = if (all(is.na(!!sym(variable)))) NA_real_ else year[which.min(!!sym(variable))]
    )
}

# Execute the function to compute metrics for each indicator and store in dfs
geburt_dammriss_metrics <- compute_metrics(indicators_filtered, "geburt_dammriss")
brustkrebs_resekt_metrics <- compute_metrics(indicators_filtered, "brustkrebs_resekt")



# -------------------------------------
#iii) Analyze descriptive stats
# -------------------------------------
# This is a very simple analysis if, for all cantons:
# for Breast-conserving mamma-resection: the mean indicator is higher post, the max indicators happened post-reform and the min indicators happened pre-reform
# for Perineal tear in vaginal births: the mean indicator is lower post, the max indicator happened pre-reform and the min indicator happened post-reform

# Define a function that evaluates various metrics for each canton, post-reform, and checks, in general:
# 1. If mean indicators post-reform follow the expected direction (higher/lower) compared to pre-reform
# 2. If the year of maximum indicators occurs after the reform start year
# 3. If the year of minimum indicators occurs before the reform start year

check_metrics <- function(data, variable_name, direction = "higher_post") {
  lower_post_cantons <- c()
  same_post_cantons <- c()
  max_year_problem_cantons <- c()
  min_year_problem_cantons <- c()
  
  # Create a new column for mean differences
  data$mean_difference <- ifelse(!is.na(data$mean_post) & !is.na(data$mean_pre), 
                                 data$mean_post - data$mean_pre, 
                                 NA_real_)
  
  # Loop through each canton and check
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    canton <- row$canton
    
    # Directional checks (i.e.  distinguishes between "higher = better", like for breast-conserving mamma-resections, and "lower = better", like for perineal tears in vaginal births)
    if (direction == "higher_post") {
      if (!is.na(row$mean_post) && !is.na(row$mean_pre) && row$mean_post <= row$mean_pre) {
        lower_post_cantons <- c(lower_post_cantons, canton)
      }
      if (!is.na(row$max_year) && !is.na(row$start) && row$max_year <= row$start) {
        max_year_problem_cantons <- c(max_year_problem_cantons, canton)
      }
      if (!is.na(row$min_year) && !is.na(row$start) && row$min_year >= row$start) {
        min_year_problem_cantons <- c(min_year_problem_cantons, canton)
      }
    } else if (direction == "lower_post") {
      if (!is.na(row$mean_post) && !is.na(row$mean_pre) && row$mean_post >= row$mean_pre) {
        lower_post_cantons <- c(lower_post_cantons, canton)
      }
      if (!is.na(row$max_year) && !is.na(row$start) && row$max_year >= row$start) {
        max_year_problem_cantons <- c(max_year_problem_cantons, canton)
      }
      
      if (!is.na(row$min_year) && !is.na(row$start) && row$min_year <= row$start) {
        min_year_problem_cantons <- c(min_year_problem_cantons, canton)
      }
    }
  }
  
  # Calculate the total number of non-NA cantons as a base to see how many cantons are being evaluated
  total_cantons <- sum(!is.na(data$mean_pre) & !is.na(data$mean_post))
  
  # Display results with commentary
  cat(sprintf("Results for %s:\n", variable_name))
  
  if (length(lower_post_cantons) > 0) {
    cat(sprintf("In these cantons (%s), the mean indicators did not follow the expected pattern of being %s post-reform.\n",
                paste(lower_post_cantons, collapse = ", "), 
                ifelse(direction == "higher_post", "higher", "lower")))
    cat(sprintf("That means %d out of %d cantons are unexpected for this mean post-reform check.\n", 
                length(lower_post_cantons), total_cantons))
  }
  
  if (length(max_year_problem_cantons) > 0) {
    cat(sprintf("In these cantons (%s), the year of %s indicators did not occur %s the reform, as expected.\n",
                paste(max_year_problem_cantons, collapse = ", "),
                ifelse(direction == "higher_post", "maximum", "maximum"),
                ifelse(direction == "higher_post", "after", "before")))
    cat(sprintf("That means %d out of %d cantons are unexpected for this %s year check.\n", 
                length(max_year_problem_cantons), total_cantons, "maximum"))
  }
  
  if (length(min_year_problem_cantons) > 0) {
    cat(sprintf("In these cantons (%s), the year of %s indicators did not occur %s the reform, as expected.\n",
                paste(min_year_problem_cantons, collapse = ", "),
                ifelse(direction == "higher_post", "minimum", "minimum"),
                ifelse(direction == "higher_post", "before", "after")))
    cat(sprintf("That means %d out of %d cantons are unexpected for this %s year check.\n", 
                length(min_year_problem_cantons), total_cantons, "minimum"))
  }
  
  
  return(data)
}
  

#Execute the checks for both service groups:

# For "brustkrebs_resekt_metrics", the expectation is higher post-reform values
brustkrebs_resekt_metrics_checked <- check_metrics(brustkrebs_resekt_metrics, "Breast-conserving mamma-resection", direction = "higher_post")

# For "geburt_dammriss_metrics", the expectation is lower post-reform values
geburt_dammriss_metrics_checked <- check_metrics(geburt_dammriss_metrics, "Perineal tear in vaginal births", direction = "lower_post")

# -------------------------------------
#iv) Export descriptive stats tables
# -------------------------------------

# Create new folder if necessary
output_dir <- "/Users/Nadine/Desktop/Thesis/05_Descriptive Analyis All/Quality Indicators/Stats Tables"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Change arrangement of columns for better presentation
clean_dataframe <- function(df) {
  df <- df %>%
    ungroup() %>%  
    relocate(mean_difference, .after = mean_post)
  return(df)
}

geburt_dammriss_metrics_checked <- clean_dataframe(geburt_dammriss_metrics_checked)
brustkrebs_resekt_metrics_checked <- clean_dataframe(brustkrebs_resekt_metrics_checked)

# Define modified save table function that includes conversion to %
save_table_to_pdf_percent <- function(df, filename) {
  
  # Replace NA and NaN values with blank character
  df_cleaned <- df %>%
    mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.), "", .)))
  
  # Convert specified columns to percentages
  df_as_percent <- df_cleaned %>%
    mutate(across(all_of(c("mean_pre", "mean_post", "mean_difference", "sd_pre", "sd_post")), 
                  ~ ifelse(. == "", ., sprintf("%.1f%%", as.numeric(.) * 100))))
  
  # Save the modified dataframe as a PDF
  pdf(file = filename, width = 20, height = 15)
  grid.table(df_as_percent, rows = NULL)
  dev.off()
}

# Save the data frames as a table in a PDF
save_table_to_pdf_percent(geburt_dammriss_metrics_checked, file.path(output_dir, "geburt_dammriss_metrics.pdf"))
save_table_to_pdf_percent(brustkrebs_resekt_metrics_checked, file.path(output_dir, "brustkrebs_resekt_metrics.pdf"))

# Save the data frames as Excel
write_xlsx(list(Geburt = geburt_dammriss_metrics_checked, Brustkrebs = brustkrebs_resekt_metrics_checked), 
           path = file.path(output_dir, "indicators stats.xlsx"))


# -------------------------------------
# BAR PLOTS
# -------------------------------------


# Set the directory path
output_dir <- "/Users/Nadine/Desktop/Thesis/05_Descriptive Analyis All/Quality Indicators/Bar Plots"

# Check if the directory exists, if not, create it
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

create_plot <- function(data, service_name, color, title) {
  # Divide mean_difference by 100
  plot_data <- data %>%
    arrange(desc(mean_difference)) %>%
    mutate(mean_difference = mean_difference * 100)
  
  bar_plot <- ggplot(plot_data, aes(x = reorder(canton, mean_difference), y = mean_difference)) +
    geom_bar(stat = "identity", fill = "grey70") +  # Neutral color for bars
    coord_flip() +
    theme_minimal() +
    labs(title = title,
         x = "Canton",
         y = "Average QI Gap (Post - Pre) in %") +
    theme(
      axis.title.y = element_text(size = 8, color = "black", face="plain"),  # y-axis title not in bold
      axis.title.x = element_text(size = 8, color = "black", face="plain"),  # x-axis title (Canton) not in bold
      axis.text.y = element_text(face="bold", color = "black"),              # x-axis text values (canton names) in bold
      title = element_text(size = 7.5, color = color)             # Main title remains in bold
    )
  # Save the plot
  ggsave(filename = file.path(output_dir, paste0("average_gap_plot_", service_name, ".png")), plot = bar_plot, width = 6, height = 4)
}

# Plot for Vaginal Births
create_plot(geburt_dammriss_metrics_checked, "Geburt", "darkblue", "Average Gap between Pre- and Post-Reform for % Perineal Tears during Vaginal Births")

# Plot for Mamma-Resections
create_plot(brustkrebs_resekt_metrics_checked, "Brustkrebs", "deeppink2", "Average Gap between Pre- and Post-Reform for % Breast-Conserving Mamma-Resections")



# -------------------------------------
# PANEL PLOTS
# -------------------------------------

# Create new folder if necessary
output_dir <- "/Users/Nadine/Desktop/Thesis/05_Descriptive Analyis All/Quality Indicators/Panel Plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define variable names
indicators_vars <- c("geburt_dammriss", "brustkrebs_resekt")

# Title mapping
titles <- c(
  geburt_dammriss = "% Perineal Tears during Vaginal Births",
  brustkrebs_resekt = "% Breast-Conserving Mamma-Resections"
)

# Generate and save plots
for (indicator in indicators_vars) {
  
  # Preprocess the data: Multiply the indicators by 100 for percentage and group by canton
  indicators_long_all_percent <- indicators_long_all %>%
    mutate(!!indicator := !!sym(indicator) * 100) %>%
    group_by(canton)
  
  # Generate the plot
  p <- ggplot(data = indicators_long_all_percent, aes(x = year, y = !!sym(indicator))) + 
    geom_line() +
    geom_vline(aes(xintercept = start), color = "red", linetype = "dashed") +
    facet_wrap(~ canton, scales = "free_x") + 
    theme_minimal() +
    labs(title = titles[indicator], x = "Year", y = NULL) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
    scale_x_continuous(breaks = seq(min(indicators_long_all_percent$year), max(indicators_long_all_percent$year), 4),
                       labels = seq(min(indicators_long_all_percent$year), max(indicators_long_all_percent$year), 4))
  
  # Save the plot to a PDF file
  filename <- file.path(output_dir, paste0(indicator, ".pdf"))
  ggsave(filename, plot = p, width = 10, height = 6)
}


## Export indicators_long_all for SCM analysis
output_dir <- "/Users/Nadine/Desktop/Thesis/02_Datasets/Qualitätsindikatoren Mamma-Resections & Births"
write_xlsx(list(indicators_compiled = indicators_long_all), 
           path = file.path(output_dir, "indicators_long_all.xlsx"))


# -------------------------------------
# STAGGERED PANEL PLOTS FOR QUALITY INDICATORS
# -------------------------------------

# Create new folder if necessary
output_dir <- "/Users/Nadine/Desktop/Thesis/05_Descriptive Analyis All/Quality Indicators/Panel Plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Reshape the data from wide to long format
indicators_long_all <- indicators_long_all %>%
  pivot_longer(
    cols = c(geburt_dammriss, brustkrebs_resekt),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  mutate(value = ifelse(variable == "geburt_dammriss", value * 10, value))

# Updated title mapping
titles <- c(
  geburt_dammriss = "% Perineal Tears during Vaginal Births",
  brustkrebs_resekt = "% Breast-Conserving Mamma-Resections"
)

# Custom function to format the secondary y-axis labels
format_percent <- function(x) {
  x = x * 100  # Convert the 0-10 scale to a percentage scale
  result <- ifelse(round(x) == x, sprintf("%d%%", as.integer(x)), sprintf("%.1f%%", x))
  return(result)
}

# Cantons to be placed in the appendix
appendix_cantons <- c("GE", "VD")

# Function to generate and save staggered panel plots with modifications for quality indicators
plot_quality_indicators <- function(data, title, pdf_name, included_cantons = NULL, include_vline = TRUE, appendix = FALSE) {
  # Filter the data based on included cantons, if provided
  data <- if (!is.null(included_cantons)) {
    data %>% filter(canton %in% included_cantons)
  } else {
    data
  }
  
  # Plot the data with color matching the line and staggered panel for cantons
  p <- ggplot(data = data, aes(x = year, y = value, color = variable)) +
    geom_line() +
    {if (include_vline) geom_vline(aes(xintercept = start, linetype = "Reform Introduction"), color = "black") else NULL} +
    facet_wrap(~ canton, scales = "free_x") +
    theme_minimal() +
    labs(
      title = title,
      x = "Year",
      y = "Indicator"
    ) + 
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), 4),
                       labels = seq(min(data$year), max(data$year), 4)) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 100, accuracy = 1),
      sec.axis = sec_axis(~.*0.1, name = "", 
                          labels = format_percent)
    ) +
    scale_color_manual(
      values = c("geburt_dammriss" = "darkblue", "brustkrebs_resekt" = "deeppink2"),
      labels = c("% Breast-Conserving Mamma-Resections","% Perineal Tears during Vaginal Births")
    ) +
    scale_linetype_manual(
      values = c("dashed"),
      labels = "Reform Introduction",
      name = NULL
    ) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = rel(0.9)),
      legend.title = element_text(size = rel(1)),
      axis.text.y.left = element_text(color = "deeppink2"),
      axis.text.y.right = element_text(color = "darkblue"),
      legend.box = "horizontal"
    ) +
    guides(
      color = guide_legend(title = NULL, order = 1),
      linetype = guide_legend(order = 2)
    )
  
  
  if (appendix) {
    p <- p + theme(
      strip.text = element_text(size = rel(1.1)),
      axis.title.x = element_text(vjust = -1)
    )
  }
  
  filename <- file.path(output_dir, paste0(pdf_name, ".pdf"))
  ggsave(filename, plot = p, width = 10, height = 6)
}


# Use the function for main cantons
plot_quality_indicators(
  data = indicators_long_all, 
  title = "Trends in Quality Indicators",
  pdf_name = "Quality_Indicators_Main",
  included_cantons = setdiff(unique(indicators_long_all$canton), appendix_cantons)
)

# Use the function for appendix cantons with different styling
plot_quality_indicators(
  data = indicators_long_all, 
  title = "Trends in Quality Indicators (continued)",
  pdf_name = "Quality_Indicators_Appendix",
  included_cantons = appendix_cantons,
  include_vline = FALSE, 
  appendix = TRUE
)

