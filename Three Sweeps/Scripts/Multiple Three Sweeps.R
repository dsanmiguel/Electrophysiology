rm(list=ls())
library(tidyverse)

rstudioapi::showDialog(title = "Select CSV Folder",
                       message = "Select Folder where CSV Data Files are located")
csvpath <- rstudioapi::selectDirectory()

# Select Experiment Identifiers file that should be a CSV file in directory above which contains four columns:
# ObsID, MouseID, CellID, Filename_IV
# for later merging with data output where Filename_IV contains names of your CSV files 
# without the .csv extension added to the name
# This will obviously be specific for each experiment so edit this accordingly
# to match the length of the number of CSV files you are trying to process in this directory
rstudioapi::showDialog(title = "Select Experiment Identifiers file",
                       message = "Select Experiment Identifiers file that corresponds to these CSV Data Files")
identifiers <- read_csv(rstudioapi::selectFile(caption = "Select Experiment Identifiers file that corresponds to this CSV Data File",
                                               filter = "CSV Files (*.csv)",
                                               existing = TRUE))

rstudioapi::showDialog(title = "Select Output Folder",
                       message = "Select Folder where you would like Output data to be saved.")
setwd(rstudioapi::selectDirectory())

dir.create("Three Sweeps Output")
setwd("Three Sweeps Output")

all_data <- NULL
finaldf <- NULL

for (mycsvfile in list.files(pattern = "csv", path = csvpath)) {
  # import data and get rid of top two rows since the way the data is output from abf to csv
  # these are not necessary as we will transform and fix column names later
  # and then this removes any empty columns that are only NAs 
  filename <- tools::file_path_sans_ext(basename(mycsvfile))
  this_identifier <- identifiers %>% filter(`Filename_Em and Rin` == filename)
  
  df <- read_csv(paste(csvpath, mycsvfile, sep = "/"), col_names = FALSE) %>% 
    slice(-c(1:2)) %>% 
    select(
      where(
        ~!all(is.na(.x))
      )
    )
  # count total number of sweeps and create vectors for new names of columns based on number of sweep
  # assumes first column 1 is Time data and remaining columns 2 and onward are Sweeps data
  sweeps_count <- df %>% select(2:last_col()) %>% ncol()
  old_names <- df %>% select(2:last_col()) %>% names() %>% as.vector()
  new_names <- seq(1:sweeps_count) %>% as.vector()
  
  # rename the columns based on Sweep number
  # make this a column called Sweep instead and the value the number of the Sweep
  # Group by Sweep so operations on run on each Sweep and not total data
  # Transmute renames columns, converts Time from seconds to milliseconds,
  # Fix column class type, and creates a new dV/dT column
  # Arrange by Sweep so data is sorted Sweep 1 to Sweep 2 and so on
  # Ungroup to add new total time elapsed column
  # Regroup data back by Sweep for future operations
  df <- df %>% 
    rename_with(~ new_names[which(old_names == .x)], .cols = old_names) %>% 
    pivot_longer(cols = 2:last_col(),
                 values_to = "Voltage (mV)",
                 names_to = "Sweep") %>% 
    group_by(Sweep) %>% 
    transmute(`Time (ms)` = as.numeric(X1)*1000,
              `Voltage (mV)` = as.numeric(`Voltage (mV)`),
              `dV/dT` = (`Voltage (mV)` - lag(`Voltage (mV)` )) / (`Time (ms)` - lag(`Time (ms)`)),
              Sweep = as.numeric(Sweep)) %>% 
    arrange(Sweep) 
  
  # remove variables we don't need later
  rm(sweeps_count, 
     old_names, 
     new_names)
  
  `Mean 1..2` <- df %>% 
    filter(between(`Time (ms)`, 0, 100)) %>% 
    summarise(`Mean from 0 to 100 ms` = mean(`Voltage (mV)`)) 
  
  `Mean 3..4` <- df %>% 
    filter(between(`Time (ms)`, 198.3, 215)) %>% 
    summarise(`Mean from 198.3 to 215 ms` = mean(`Voltage (mV)`)) 
  
  slopes1 <- df %>% 
    group_by(Sweep) %>% 
    filter(`Time (ms)` == 0 |
             `Time (ms)` == 100) %>% 
    transmute(Sweep,
              `Slope 0 to 100 ms` = (`Voltage (mV)` - lag(`Voltage (mV)`))/(`Time (ms)` - lag(`Time (ms)`))) %>% 
    drop_na()
  
  
  slopes2 <- df %>% 
    group_by(Sweep) %>% 
    filter(`Time (ms)` == 198.3 |
             `Time (ms)` == 215) %>% 
    transmute(Sweep,
              `Slope 198.3 to 215 ms` = (`Voltage (mV)` - lag(`Voltage (mV)`))/(`Time (ms)` - lag(`Time (ms)`))) %>% 
    drop_na()
  
  df2 <- tibble(Sweep = `Mean 1..2`$Sweep,
                `Slope 0 to 100 ms` = slopes1$`Slope 0 to 100 ms`,
                `Slope 198.3 to 215 ms` = slopes2$`Slope 198.3 to 215 ms`,
                `Mean from 0 to 100 ms` = `Mean 1..2`$`Mean from 0 to 100 ms`,
                `Mean from 198.3 to 215 ms` = `Mean 3..4`$`Mean from 198.3 to 215 ms`,
                `Rin (MegaOhms)` = (`Mean from 0 to 100 ms` - `Mean from 198.3 to 215 ms`)/50 * 1000) %>% 
    group_by(Sweep) %>% 
    filter((abs(`Slope 0 to 100 ms`) < 0.005 &
            abs(`Slope 198.3 to 215 ms`) < 0.05))
  
  
  finaldf <- this_identifier %>% 
    mutate(`Average Resting Em` = mean(df2$`Mean from 0 to 100 ms`),
           `Average R in` = mean(df2$`Rin (MegaOhms)`))
  
  all_data <- rbind(all_data, finaldf)
}

write_csv(all_data, "Computer Data Analysis Output Three Sweeps.csv")
