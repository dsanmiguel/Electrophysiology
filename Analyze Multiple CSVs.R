rm(list=ls())
library(tidyverse)

### This script was written by Daniel Anthony San Miguel Jr.,
### PhD Student at University of Austin as of 2023
### College of Pharmacy, Division of Pharmacology & Toxicology
### 
### If you or someone else plans to use or edit this script,please cite me.
### 
### The github repository that converts ABF to CSV is located at 
### https://github.com/swharden/AbfConvert
### 

### The Data is output as "Data Analysis output.csv" 
### into the parent folder of the CSV folder you selected

### This script assumes various set values throughout that of course can be changed
### Action potential peak voltage values that are above 0 set by the variable "voltage_cutoff" below

# choose folder where CSV files are located, please only have the data CSV files in this folder
setwd(rstudioapi::selectDirectory())


#### Import Identifier Data frame to be used later
#### The two dots in front indicate it is in the folder above the one
#### in the current working directory we are currently in (set above using setwd)
#### 
#### This will obviously be specific for each experiment so edit this accordingly
#### to match the length of the number of CSV files you are trying to process in this directory

# this assumes a file named Experiment Identifiers.csv is in the folder above the CSV folder you selected above
# Experiment Identifiers.csv should be a CSV file that contains four columns:
# ObsID, MouseID, CellID, Filename_IV
# for later merging with data output
identifiers <- read_csv("../Experiment Identifiers.csv")

# create minimum voltage peaks must pass to count as peaks (lower bound)
voltage_cutoff <- 0

all_data <- NULL
finaldf <- NULL

for (i in list.files(pattern = "csv")) {
  # import data and get rid of top two rows since the way the data is outputf rom abf to csv
  # these are not necessary as we will transform and fix column names later
  # and then this removes any empty columns that are only NAs 
  df <- read_csv(i, col_names = FALSE) %>% 
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
  df1 <- df %>% 
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
  rm(df, sweeps_count, old_names, new_names)
  
  # this then counts the number of peaks for each sweep that are above 0 as a baseline Voltage (mV)
  # by looking at the change in dV/dT from positive to negative as well as peaks at least 2/3 as
  # the max peak
  # If needed an additional filter for `Time (ms)` can be added
  
  sweeps_AP_count <- df1 %>% 
    filter(`Voltage (mV)` > 0) %>% 
    filter(lag(`dV/dT`) >= 0 &
             `dV/dT` < 0) %>% 
    filter((max(`Voltage (mV)`, na.rm = TRUE)/(`Voltage (mV)`)) < 1.5) %>% 
    select(Sweep) %>% 
    table() %>% 
    as.data.frame()
  
  # create NA variables which are overwritten later if they exist
  # If these NAs are not created then later parts will fail to run to
  # completion and create the data output file
  this_identifier <- NULL
  this_identifier$ObsID <- NA
  this_identifier$MouseID <- NA
  this_identifier$CellID <- NA
  this_identifier$Filename_IV <- NA
  
  `Sweep 9 Resting Em (mV)` <- NA
  `Sweep 9 -50 pA step Steady-State Em` <- NA
  `Sweep 9 Resistance_in (MegaOhms)` <- NA
  `Sweep # for 1st AP fired` <- NA
  `Rheobase (pA)` <- NA
  `Threshold Em (mV)` <- NA
  `Peak Voltage (mV)` <- NA
  `Halfwidth (ms)` <- NA
  `AP Amplitude (mV)` <- NA
  `Half Amplitude (mV)` <- NA
  `Halfwidth (ms)` <- NA
  max_AHP_1stAP_Voltage <- NA
  maxAHPdeltathreshold <- NA
  fAHP_1stAP <- NA
  fAHP_delta_threshold <- NA
  mAHP_1stAP <- NA
  mAHP_delta_threshold <- NA
  sAHP_1stAP <- NA
  sAHP_delta_threshold <- NA
  `Sweep # for max frequency and SFA` <- NA
  `Peak to peak Frequency_Max` <- NA
  `Interevent Interval_1` <- NA
  `Interevent Interval_last` <- NA
  `SFA (1st ISI/last ISI)` <- NA
  
  
  `Resting Em` <- df1 %>% 
    filter(between(`Time (ms)`, 0, 100) & Sweep == 9) %>% 
    summarise(mean_0_100 = mean(`Voltage (mV)`)) %>% 
    select(mean_0_100) %>% 
    unlist() %>% 
    as.numeric()
  
  `-50 pA step Steady-State Em` <- df1 %>% 
    filter(between(`Time (ms)`, 198.3, 215) & Sweep == 9) %>% 
    summarise(mean_198_215 = mean(`Voltage (mV)`)) %>% 
    select(mean_198_215) %>% 
    unlist() %>% 
    as.numeric()
  
  # this is using Ohm's law of R = V/I
  # where voltz is in millivolts and current (50) is in nanoamps 
  # therefore result is in megaOhms
  `Rin (MOhm)` <- ((`Resting Em` - `-50 pA step Steady-State Em`)/50)*1000
  
  # get the AP data for all the sweeps
  all_APs_all_sweeps <- df1 %>% 
    filter(`Voltage (mV)` > voltage_cutoff) %>% 
    arrange(Sweep) %>% 
    filter(lag(`dV/dT`) >= 0 &
             `dV/dT` < 0)
  
  # get filename without csv extension
  filename = tools::file_path_sans_ext(i)
  # match up filename to other identifiers in identifier data like ObsID, Mouse, and Cell
  this_identifier <- identifiers %>% filter(Filename_IV == filename)
  
  
  ########################################################################
  ########################################################################
  ############ IF LOOP BEGINS HERE FOR THOSE WITH ACTION POTENTIALS ########
  ########################################################################
  ########################################################################
  ########################################################################
  
  if (length(sweeps_AP_count) > 1) {
    
    sweeps_AP_count <- sweeps_AP_count %>% 
      transmute("Sweep" = Sweep,
                `AP Count` = Freq)
    
    # fix variable type for Sweep else error will occur later on
    sweeps_AP_count$Sweep <- as.character(sweeps_AP_count$Sweep)
    
    # if there are no Action Potentials in any Sweeps then all of these values below should be NA
    # get sweep with first AP
    `Sweep # for 1st AP fired` <- sweeps_AP_count %>% 
      slice(1) %>% 
      select(Sweep) %>% 
      unlist()  %>% 
      as.numeric()
    
    # create data frame to count number of APs for all sweeps 10 and onward including those with no APs
    # the subtracted 1 is so it doesn't include the first Sweep with an AP otherwise will be wrong
    sweeps_before_first_AP_count <- tibble(Sweep = c(10:(`Sweep # for 1st AP fired` - 1)),
                                           `AP Count` = 0)
    
    AP_count_all_sweeps <- rbind(sweeps_before_first_AP_count, sweeps_AP_count)
    
    # remove variables we don't need anymore
    rm(sweeps_before_first_AP_count, sweeps_AP_count)
    
    firstAP <- df1 %>% 
      filter(Sweep == `Sweep # for 1st AP fired`)
    
    firstAP_peak <- firstAP %>% 
      filter(`Voltage (mV)` > voltage_cutoff &
               lag(`dV/dT`) > 0 & 
               lead(`dV/dT`) < 0) %>% 
      slice(1)
    
    
    # get values of data from 15 ms before first peak and 20 ms after first peak
    Time1 <- (firstAP_peak[1,1] - 15) %>% as.numeric()
    Time2 <- (firstAP_peak[1,1] + 20) %>% as.numeric()
    
    firstAP_range <- firstAP %>% 
      filter(`Time (ms)` > Time1 &
               `Time (ms)` < Time2)
    
    
    `Threshold Em (mV)` <- firstAP_range %>% 
      filter(lead(`dV/dT` > 10)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(`Voltage (mV)`) %>% 
      unlist() %>% 
      as.numeric()
    
    
    `Threshold Time (ms)` <- firstAP_range %>% 
      filter(lead(`dV/dT` > 10)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(`Time (ms)`) %>% 
      unlist() %>% 
      as.numeric()
    
    
    `Peak Voltage (mV)` <- max(firstAP_range$`Voltage (mV)`)
    `AP Amplitude (mV)` <- `Peak Voltage (mV)` - `Threshold Em (mV)`
    `Half Amplitude (mV)` <- `AP Amplitude (mV)`/2
    
    # get lowest number of voltage (as in negative not small)
    max_AHP_1stAP_Voltage <- min(firstAP_range$`Voltage (mV)`)
    
    # find the Time that corresponds to the max_AHP_1stAP_Voltage
    max_AHP_1stAP_Time <- firstAP_range %>% 
      filter(`Voltage (mV)` == max_AHP_1stAP_Voltage) %>% 
      ungroup() %>% 
      select(`Time (ms)`) %>% 
      unlist() %>% 
      as.numeric()
    `HalfWidth Volts` <- `Threshold Em (mV)` - `Half Amplitude (mV)`
    
    
    # dV = V2 - V1
    # so V2 = dV + V1
    V2 <- `Threshold Em (mV)` + `Half Amplitude (mV)`
    
    # filter times greater than Threshold Time and then find which two voltage points that V2 lies between
    # slice for just the first two in case there are more than one
    half_ap_range <- firstAP_range %>% 
      filter(`Time (ms)` >= `Threshold Time (ms)` &
               V2 < lag(`Voltage (mV)`) & 
               V2 > lead(`Voltage (mV)`)) %>% 
      slice(1:2)
    
    # this finds formula of standard curve around point
    xs <- half_ap_range %>% 
      ungroup() %>% 
      select(`Time (ms)`) %>% 
      unlist()
    
    ys <- half_ap_range %>% 
      ungroup() %>% 
      select(`Voltage (mV)`) %>% 
      slice(1:2) %>% 
      unlist()
    
    linearfit <- lm(ys ~ xs)
    
    slope <- linearfit$coefficients["xs"] %>% as.numeric()
    y_intercept <- linearfit$coefficients["(Intercept)"] %>% as.numeric()
    
    # Rearrange and use standard curve to solve for x which is T2
    T2 <- (V2 - y_intercept)/slope
    
    # find Halfwidth by subtracting Threshold Time from T2
    `Halfwidth (ms)` <- (T2 - `Threshold Time (ms)`)
    
    # remove variables we don't need later
    rm(half_ap_range,
       slope, 
       y_intercept, 
       xs, 
       ys, 
       V2, 
       `HalfWidth Volts`, 
       T2, 
       Time1, 
       Time2, 
       linearfit)
    
    # create a dataframe of what rheobase value corresponds to what sweep
    # here we just said sweeps 10 through 20 with their corresponding values
    # from 50 to 550 with increasing 50 steps in between
    rheobase_df <- data.frame(Sweep = c(10:20), Rheobase = c(seq(50, 550, 50)))
    
    `Rheobase (pA)` <- rheobase_df %>% 
      filter(Sweep == `Sweep # for 1st AP fired`) %>% 
      select(Rheobase) %>% 
      unlist() %>% 
      as.numeric()
    
    first_peak_time <- firstAP_peak %>% 
      ungroup() %>% 
      arrange(desc(`Voltage (mV)`)) %>% 
      select(`Time (ms)`) %>% 
      slice(1) %>% 
      unlist() %>% 
      as.numeric()
    
    
    `fAHP_1stAP` <- df1 %>% filter(Sweep == `Sweep # for 1st AP fired` &
                                     `Time (ms)` >= `max_AHP_1stAP_Time` &
                                     `Time (ms)` <= (`max_AHP_1stAP_Time` + 5)) %>% 
      select(`Voltage (mV)`) %>% min()
    
    
    mAHP_1stAP <- df1 %>% 
      ungroup() %>% 
      filter(Sweep == `Sweep # for 1st AP fired` & 
               `Time (ms)` == as.character(`Threshold Time (ms)` + 10)) %>% 
      select(`Voltage (mV)`) %>% 
      unlist() %>% 
      as.numeric()
    
    sAHP_1stAP <- df1 %>% 
      ungroup() %>% 
      filter(Sweep == `Sweep # for 1st AP fired` & 
               `Time (ms)` == as.character(`Threshold Time (ms)` + 15)) %>% 
      select(`Voltage (mV)`) %>% 
      unlist() %>% 
      as.numeric()
    
    `Sweep # for max frequency and SFA` <- AP_count_all_sweeps %>% 
      arrange(desc(`AP Count`)) %>% 
      select(Sweep) %>% 
      slice(1) %>% 
      unlist() %>% 
      as.numeric()
    
    
    # this gets the Interevent Interval by subtracting current time from previous time
    # this gets Peak-to-Peak Frequency by taking inverse of the difference of the 
    # current Peak time with the last Peak time which gives you mHZ which then needs to be
    # multiplied by 1000 to get Hz
    # so this is simplified by just dividing 1000 by time difference to get Hz
    max_AP_sweep <- all_APs_all_sweeps %>% 
      filter(Sweep == `Sweep # for max frequency and SFA`) %>% 
      mutate(`Interevent Interval (ms)` = (`Time (ms)` - lag(`Time (ms)`)),
             `Peak-to-Peak Frequency (Hz)` = 1000/(`Time (ms)` - lag(`Time (ms)`)))
    
    `Peak to peak Frequency_Max` <- max_AP_sweep %>% 
      ungroup() %>% 
      select(`Peak-to-Peak Frequency (Hz)`) %>% 
      max(na.rm = TRUE)
    
    `Interevent Interval_1` <- max_AP_sweep %>% 
      ungroup() %>% 
      select(`Interevent Interval (ms)`) %>% 
      drop_na() %>% 
      head(1) %>% 
      unlist() %>% 
      as.numeric()
    
    `Interevent Interval_last` <- max_AP_sweep %>% 
      ungroup() %>% 
      select(`Interevent Interval (ms)`) %>% 
      drop_na() %>% 
      tail(1) %>% 
      unlist() %>% 
      as.numeric()
    
    if (max(AP_count_all_sweeps$`AP Count`) < 2) {
      `Interevent Interval_1` <- NA
      `Interevent Interval_last` <- NA
    }
    
    maxAHPdeltathreshold <- max_AHP_1stAP_Voltage - `Threshold Em (mV)`
    
    fAHP_delta_threshold <- fAHP_1stAP - `Threshold Em (mV)`
    
    mAHP_delta_threshold <- mAHP_1stAP- `Threshold Em (mV)`
    
    sAHP_delta_threshold <- sAHP_1stAP - `Threshold Em (mV)`
    
    `SFA (1st ISI/last ISI)` <- (`Interevent Interval_1`/`Interevent Interval_last`)
    
    wide_format_AP_count_all <- AP_count_all_sweeps %>% 
      pivot_wider(names_from = Sweep,
                  values_from = `AP Count`)
    
    colnames(wide_format_AP_count_all) <- paste("Sweep", colnames(wide_format_AP_count_all), "AP Count", sep = " ")
    
    finaldf <- wide_format_AP_count_all %>% 
      mutate(ObsID = this_identifier$ObsID,
             MouseID = this_identifier$MouseID,
             CellID = this_identifier$CellID,
             Filename_IV = this_identifier$Filename_IV,
             `Sweep 9 Resting Em (mV)` = `Resting Em`,
             `Sweep 9 -50 pA step Steady-State Em` = `-50 pA step Steady-State Em`,
             `Sweep 9 Resistance_in (MegaOhms)` = `Rin (MOhm)`,
             .before = `Sweep 10 AP Count`) %>% 
      mutate("Sweep # for 1st AP fired" = `Sweep # for 1st AP fired`,
             "Rheobase (pA)" = `Rheobase (pA)`,
             "Threshold Em (mV)" = `Threshold Em (mV)`,
             "Peak Voltage (mV)" = `Peak Voltage (mV)`,
             "AP Amplitude (mV)" = `AP Amplitude (mV)`,
             "Half Amplitude (mV)" = `Half Amplitude (mV)`,
             "HalfWidth (ms)" = `Halfwidth (ms)`,
             "max AHP 1stAP (mV)" = max_AHP_1stAP_Voltage,
             "max AHP delta Threshold"= maxAHPdeltathreshold,
             "fAHP_1stAP (mV)" = fAHP_1stAP,
             "fAHP delta Threshold" = fAHP_delta_threshold,
             "mAHP_1stAP (mV)" = mAHP_1stAP,
             "mAHP delta Threshold" = mAHP_delta_threshold,
             "sAHP_1stAP" = sAHP_1stAP,
             "sAHP delta Threshold" = sAHP_delta_threshold,
             "Sweep # for max frequency and SFA" = `Sweep # for max frequency and SFA`,
             "Peak to peak Frequency_Max" = `Peak to peak Frequency_Max`,
             "Interevent Interval_1" = `Interevent Interval_1`,
             "Interevent Interval_last" = `Interevent Interval_last`,
             "SFA (1st ISI/last ISI)" = `SFA (1st ISI/last ISI)`)
  } else {finaldf <- tibble(ObsID = this_identifier$ObsID,
                            MouseID = this_identifier$MouseID,
                            CellID = this_identifier$CellID,
                            Filename_IV = this_identifier$Filename_IV,
                            `Sweep 9 Resting Em (mV)` = `Resting Em`,
                            `Sweep 9 -50 pA step Steady-State Em` = `-50 pA step Steady-State Em`,
                            `Sweep 9 Resistance_in (MegaOhms)` = `Rin (MOhm)`,
                            "Sweep # for 1st AP fired" = `Sweep # for 1st AP fired`,
                            "Rheobase (pA)" = `Rheobase (pA)`,
                            "Threshold Em (mV)" = `Threshold Em (mV)`,
                            "Peak Voltage (mV)" = `Peak Voltage (mV)`,
                            "AP Amplitude (mV)" = `AP Amplitude (mV)`,
                            "Half Amplitude (mV)" = `Half Amplitude (mV)`,
                            "HalfWidth (ms)" = `Halfwidth (ms)`,
                            "max AHP 1stAP (mV)" = max_AHP_1stAP_Voltage,
                            "max AHP delta Threshold"= maxAHPdeltathreshold,
                            "fAHP_1stAP (mV)" = fAHP_1stAP,
                            "fAHP delta Threshold" = fAHP_delta_threshold,
                            "mAHP_1stAP (mV)" = mAHP_1stAP,
                            "mAHP delta Threshold" = mAHP_delta_threshold,
                            "sAHP_1stAP" = sAHP_1stAP,
                            "sAHP delta Threshold" = sAHP_delta_threshold,
                            "Sweep # for max frequency and SFA" = `Sweep # for max frequency and SFA`,
                            "Peak to peak Frequency_Max" = `Peak to peak Frequency_Max`,
                            "Interevent Interval_1" = `Interevent Interval_1`,
                            "Interevent Interval_last" = `Interevent Interval_last`,
                            "SFA (1st ISI/last ISI)" = `SFA (1st ISI/last ISI)`)
  }
  
  all_data <- plyr::rbind.fill(all_data, finaldf)
}

write_csv(all_data, "../Data Analysis output.csv")