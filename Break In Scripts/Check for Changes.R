rm(list=ls())
library(tidyverse)

rstudioapi::showDialog(title = "Select Human Data Analysis Output CSV file to compare with",
                       message = "Select Human Data Analysis Output CSV file to compare with")
humandf <- read_csv(rstudioapi::selectFile(caption = "Select Human Data Analysis Output CSV file to compare with.",
                                           filter = "CSV Files (*.csv)",
                                           existing = TRUE))

humandf$`SFA (1st ISI/last ISI)` <- as.numeric(humandf$`SFA (1st ISI/last ISI)`)

humandf <- humandf %>% rename(`fAHP_1stAP Voltage (mV)` = `fAHP_1stAP (mV)`)

rstudioapi::showDialog(title = "Select Computer Data Analysis Output CSV file to compare with",
                       message = "Select Computer Data Analysis Output CSV file to compare with")
computerdf <- read_csv(rstudioapi::selectFile(caption = "Select Computer Data Analysis Output CSV file to compare with.",
                                              filter = "CSV Files (*.csv)",
                                              existing = TRUE))
# verify columns are same
#humandf$CellID == computerdf$CellID
#sapply(humandf, class) == sapply(computerdf, class)
#which(is.na(humandf$`Peak to peak Frequency_Max`), arr.ind = TRUE)
#which(is.na(computerdf$`Peak to peak Frequency_Max`), arr.ind = TRUE)
#which(is.na(humandf$`Peak to peak Frequency_Max`), arr.ind = TRUE) == which(is.na(computerdf$`Peak to peak Frequency_Max`), arr.ind = TRUE)

rstudioapi::showDialog(title = "Select CSV Folder",
                       message = "Select Folder where CSV Data Files are located")
csvpath <- rstudioapi::selectDirectory()

rstudioapi::showDialog(title = "Select Output Folder",
                       message = "Select Folder where you would like Output data to be saved.")
setwd(rstudioapi::selectDirectory())

dir.create("Difference Output")
setwd("Difference Output")

differencedf <- humandf %>% 
  left_join(computerdf, by = c("ObsID","MouseID","CellID","Filename_IV")) %>% 
  transmute(ObsID, 
            MouseID, 
            CellID, 
            Filename_IV,
            `Sweep 9 Resting Em (mV) Difference` = `Sweep 9 Resting Em (mV).x` - `Sweep 9 Resting Em (mV).y`,
            `Sweep 9 -50 pA step Steady-State Em Difference` = `Sweep 9 -50 pA step Steady-State Em.x` - `Sweep 9 -50 pA step Steady-State Em.y`,
            `Sweep 9 Resistance_in (MegaOhms) Difference` = `Sweep 9 Resistance_in (MegaOhms).x` - `Sweep 9 Resistance_in (MegaOhms).y`,
            `Sweep 10 AP Count Difference` = `Sweep 10 AP Count.x` - `Sweep 10 AP Count.y`,
            `Sweep 11 AP Count Difference` = `Sweep 11 AP Count.x` - `Sweep 11 AP Count.y`,
            `Sweep 12 AP Count Difference` = `Sweep 12 AP Count.x` - `Sweep 12 AP Count.y`,
            `Sweep 13 AP Count Difference` = `Sweep 13 AP Count.x` - `Sweep 13 AP Count.y`,
            `Sweep 14 AP Count Difference` = `Sweep 14 AP Count.x` - `Sweep 14 AP Count.y`,
            `Sweep 15 AP Count Difference` = `Sweep 15 AP Count.x` - `Sweep 15 AP Count.y`,
            `Sweep 16 AP Count Difference` = `Sweep 16 AP Count.x` - `Sweep 16 AP Count.y`,
            `Sweep 17 AP Count Difference` = `Sweep 17 AP Count.x` - `Sweep 17 AP Count.y`,
            `Sweep 18 AP Count Difference` = `Sweep 18 AP Count.x` - `Sweep 18 AP Count.y`,
            `Sweep 19 AP Count Difference` = `Sweep 19 AP Count.x` - `Sweep 19 AP Count.y`,
            `Sweep 20 AP Count Difference` = `Sweep 20 AP Count.x` - `Sweep 20 AP Count.y`,
            `Sweep # for 1st AP fired Difference` = `Sweep # for 1st AP fired.x` - `Sweep # for 1st AP fired.y`,
            `Rheobase (pA) Difference` = `Rheobase (pA).x` - `Rheobase (pA).y`,
            `Threshold Em (mV) Difference` = `Threshold Em (mV).x` - `Threshold Em (mV).y`,
            `Peak Voltage (mV) Difference` = `Peak Voltage (mV).x` - `Peak Voltage (mV).y`,
            `AP Amplitude (mV) Difference` = `AP Amplitude (mV).x` - `AP Amplitude (mV).x`,
            `Half Amplitude (mV) Difference` = `Half Amplitude (mV).x` - `Half Amplitude (mV).y`,
            `HalfWidth (ms) Difference` = `HalfWidth (ms).x` - `HalfWidth (ms).y`,
            `max AHP 1stAP (mV) Difference` = `max AHP 1stAP (mV).x` - `max AHP 1stAP (mV).y`,
            `max AHP delta Threshold Difference` = `max AHP delta Threshold.x` - `max AHP delta Threshold.y`,
            `fAHP_1stAP (mV) Difference` = `fAHP_1stAP Voltage (mV).x` - `fAHP_1stAP Voltage (mV).y`,
            `fAHP delta Threshold Difference` = `fAHP delta Threshold.x` - `fAHP delta Threshold.y`,
            `mAHP_1stAP (mV) Difference` = `mAHP_1stAP (mV).x` - `mAHP_1stAP (mV).y`,
            `mAHP delta Threshold Difference` = `mAHP delta Threshold.x` - `mAHP delta Threshold.y`,
            `sAHP_1stAP Difference Difference` = `sAHP_1stAP.x` - `sAHP_1stAP.y`,
            `sAHP delta Threshold Difference` = `sAHP delta Threshold.x` - `sAHP delta Threshold.y`,
            `Peak to peak Frequency_Max Difference` = (`Peak to peak Frequency_Max.x` - `Peak to peak Frequency_Max.y`),
            `Interevent Interval_1 Difference` = `Interevent Interval_1.x` - `Interevent Interval_1.y`,
            `Interevent Interval_last Difference` = `Interevent Interval_last.x` - `Interevent Interval_last.y`,
            `SFA (1st ISI/last ISI) Difference` = `SFA (1st ISI/last ISI).x` - `SFA (1st ISI/last ISI).y`)

# check if the filenames are in the same order as original dataframes
#differencedf$Filename_IV == humandf$Filename_IV
#differencedf$Filename_IV == computerdf$Filename_IV

differencedf <- differencedf %>% 
  mutate_if(is.numeric, round) %>% 
  filter(if_all(c(`Sweep 9 Resting Em (mV) Difference`:`SFA (1st ISI/last ISI) Difference`), ~!is.na(.)))

write_csv(differencedf, "Differences Rounded.csv")

correct_AP_counts <- differencedf %>% filter(if_all(c(`Sweep 10 AP Count Difference`:`Sweep 20 AP Count Difference`), ~is.na(.) | . == 0))
incorrect_AP_counts <- differencedf %>% filter(if_any(c(`Sweep 10 AP Count Difference`:`Sweep 20 AP Count Difference`), ~ . != 0))

incorrect_AP_files <- incorrect_AP_counts$Filename_IV

differencedf %>%
  group_by(Filename_IV) %>% 
  filter(!(any(Filename_IV == correct_AP_counts$Filename_IV))) %>% 
  filter(!(any(Filename_IV == incorrect_AP_counts$Filename_IV)))

negligible_differences <- differencedf %>% 
  filter(if_all(c(`Sweep 9 Resting Em (mV) Difference`:`Sweep 9 Resistance_in (MegaOhms) Difference`,
                  `Sweep # for 1st AP fired Difference`:`SFA (1st ISI/last ISI) Difference`), 
                ~abs(.) < 1))

non_negligible_differences <- differencedf %>% 
  group_by(Filename_IV) %>% 
  filter(!(any(Filename_IV == negligible_differences$Filename_IV)))


non_negligible_differences_maybe_due_to_AP <- differencedf %>% 
  group_by(Filename_IV) %>% 
  filter(any(Filename_IV == incorrect_AP_files))

negligible_differences_ignoring_AP <- differencedf %>% 
  filter(if_all(c(`Sweep 9 Resting Em (mV) Difference`:`HalfWidth (ms) Difference`,
                  `Peak to peak Frequency_Max Difference`:`SFA (1st ISI/last ISI) Difference`), 
                ~ abs(.) < 1))

non_negligible_differences_ignoring_AP <- differencedf %>% 
  group_by(Filename_IV) %>% 
  filter(!(any(Filename_IV == negligible_differences_ignoring_AP$Filename_IV)))

non_AP_count_errors <- non_negligible_differences_ignoring_AP %>% 
  group_by(Filename_IV) %>% 
  filter(!any(Filename_IV == incorrect_AP_counts))


all_incorrect_file_sweeps <- differencedf %>% 
  filter(if_any(c(`Sweep 10 AP Count Difference`:`Sweep 20 AP Count Difference`), ~ . != 0)) %>% 
  select(`Filename_IV`, 
         ends_with("AP Count Difference")) %>% 
  select(where(~!all(is.na(.) | . == 0))) %>% # this gets rid of columns with only zeroes or only NAs or only zeroes and NAs
  pivot_longer(cols = names(select(. ,where(is.numeric))),
               names_to = "Sweep",
               values_to = "Count Difference") %>% 
  mutate(Sweep = parse_number(Sweep)) %>% 
  filter(`Count Difference` != 0)

human_count_file_sweeps <- humandf %>% 
  select(`Filename_IV`, 
         ends_with("AP Count")) %>% 
  select(where(~!all(is.na(.) | . == 0))) %>% # this gets rid of columns with only zeroes or only NAs or only zeroes and NAs
  pivot_longer(cols = names(select(. ,where(is.numeric))),
               names_to = "Sweep",
               values_to = "Count") %>% 
  mutate(Sweep = parse_number(Sweep)) %>% 
  filter(`Count` != 0)


# we actually dont need this below at all but it is left in case this information 
# is desired in a dataframe in the same format as human_count_file_sweeps
computer_incorrect_file_sweeps <- computerdf %>% 
  select(`Filename_IV`,
         ends_with("AP Count")) %>% 
  select(where(~!all(is.na(.) | . == 0))) %>% # this line gets rid of columns for Sweeps 10 and 11 because these were either all zeroes or zeroes and NAs
  pivot_longer(cols = names(select(. ,where(is.numeric))),
               names_to = "Sweep",
               values_to = "Count") %>% 
  mutate(Sweep = parse_number(Sweep)) %>% 
  filter(`Count` != 0)

dir.create("Difference Plots")

mycsvfile <- NULL
tmp_pl <- NULL
pdfname <- NULL
sweeps_AP_count <- NULL
df <- NULL
df1 <- NULL
all_APs_all_sweeps <- NULL
sweep_differences <- NULL
i <- NULL

for (i in unique(all_incorrect_file_sweeps$Filename_IV)) {
  mycsvfile <- paste(i, ".csv", sep = "")
  df <- read_csv(paste(csvpath, mycsvfile, sep = "/"), col_names = FALSE) %>% 
    slice(-c(1:2)) %>% 
    select(
      where(
        ~!all(is.na(.x))
      )
    )
  sweeps_count <- df %>% select(2:last_col()) %>% ncol()
  old_names <- df %>% select(2:last_col()) %>% names() %>% as.vector()
  new_names <- as.character(seq(1:sweeps_count)) %>% as.vector()
  df1 <- df %>% 
    rename_with(~ new_names[which(old_names == .x)], .cols = old_names) %>% 
    pivot_longer(cols = 2:last_col(),
                 values_to = "Voltage (mV)",
                 names_to = "Sweep") %>% 
    group_by(Sweep) %>% 
    transmute(`Time (ms)` = as.numeric(X1)*1000,
              `Voltage (mV)` = as.numeric(`Voltage (mV)`),
              `dV/dT` = (`Voltage (mV)` - lag(`Voltage (mV)` )) / (`Time (ms)` - lag(`Time (ms)`)),
              Sweep = as.factor(Sweep),
              `Normalised Voltage (mV)` = as.numeric(`Voltage (mV)` + 75)) %>% 
    arrange(Sweep) 
  
  # remove variables we don't need later
  rm(df, sweeps_count, old_names, new_names)
  
  # get the AP data for all the sweeps
  # this compares each peak to the points around it 
  # (specifically points 2.5 ms, 5 ms, and 10 ms before and after) 
  # and if any of these pass a certain height (15, 20, or 20, respectively), 
  # then they are considered an action potential
  
  sweeps1 <- df1 %>% 
    arrange(Sweep) %>% 
    filter(((`Normalised Voltage (mV)` - lag(`Normalised Voltage (mV)`, 25)) >= 15 &
              (`Normalised Voltage (mV)` - lead(`Normalised Voltage (mV)`, 25)) >= 15) |
             ((`Normalised Voltage (mV)` - lag(`Normalised Voltage (mV)`, 50)) >= 20 &
                (`Normalised Voltage (mV)` - lead(`Normalised Voltage (mV)`, 50)) >= 20) |
             ((`Normalised Voltage (mV)` - lag(`Normalised Voltage (mV)`, 100)) >= 20 &
                (`Normalised Voltage (mV)` - lead(`Normalised Voltage (mV)`, 100)) >= 20)) %>% 
    mutate(Filename_IV = i)
  
  sweeps2 <- df1 %>% 
    filter(lead(`dV/dT`) <= 0 &
             `dV/dT` > 0) %>% 
    mutate(Filename_IV = i)
  
  all_APs_all_sweeps <- inner_join(sweeps1, sweeps2)
  
  # get rid of split peaks where there are two points very close together
  
  split_peaks <- all_APs_all_sweeps %>% 
    group_by(Sweep) %>% 
    filter(abs(`Time (ms)` - lag(`Time (ms)`)) <= 3 |
             abs(`Time (ms)` - lead(`Time (ms)`)) <= 3) 
  
  tmpdf <- NULL
  tmptime <- NULL
  tmpsplitdf <- NULL
  splitdf <- NULL
  maxsplit <- NULL
  truepeaks <- NULL
  
  if (nrow(split_peaks) >= 1) {
    for (thisrow in 1:nrow(split_peaks)) {
      tmpdf = split_peaks[thisrow,]
      tmptime = tmpdf$`Time (ms)`
      tmpsplitdf = split_peaks %>% 
        filter(Sweep == tmpdf$Sweep & 
                 between(`Time (ms)`, (tmptime - 3), (tmptime + 3)))
      maxsplit = tmpsplitdf %>% slice_max(n = 1, order_by = `Voltage (mV)`) %>% head(1)
      splitdf = rbind(splitdf, tmpsplitdf)
      truepeaks = rbind(truepeaks, maxsplit) 
    }
    
    truepeaks <- truepeaks %>% distinct(.keep_all = TRUE)
    splitdf <- splitdf %>% distinct(.keep_all = TRUE)
    
    no_split_peaks <- anti_join(all_APs_all_sweeps, splitdf)
    
    all_APs_all_sweeps <- rbind(truepeaks, no_split_peaks) %>%
      group_by(Sweep) %>% 
      arrange(Sweep, `Time (ms)`)
  }
  
  sweeps_AP_count <- all_APs_all_sweeps %>% 
    select(Sweep) %>% 
    table() %>% 
    as.data.frame()
  
  sweep_differences <- all_incorrect_file_sweeps %>% 
    filter(Filename_IV == i) %>% 
    select(Sweep)
  
  computer_count <- NULL
  human_count <- NULL
  tmp_pl <- NULL
  pdfname <- NULL
  for (s in sweep_differences$Sweep) {
    computer_count <- length(all_APs_all_sweeps$`Voltage (mV)`[all_APs_all_sweeps$Sweep == s])
    human_count <- human_count_file_sweeps %>% 
      filter(Filename_IV == i &
               Sweep == s) %>% 
      select(Count) %>% 
      unlist() %>% 
      as.integer()
    
    tmp_pl <- df1 %>% 
      group_by(Sweep) %>% 
      filter(any(Sweep == s)) %>% 
      ggplot(aes(x = `Time (ms)` , 
                 y = `Voltage (mV)`),
             lab) +
      geom_line(size=0.2, 
                show.legend = FALSE) +
      geom_point(data = all_APs_all_sweeps %>% filter(Sweep == s),
                 size = 2,
                 show.legend = FALSE,
                 aes(x = `Time (ms)`,
                     y = `Voltage (mV)`,
                     color = "#FB8072")) +
      ggrepel::geom_text_repel(data = all_APs_all_sweeps %>% filter(Sweep == s),
                               size = 2,
                               show.legend = FALSE,
                               aes(x = `Time (ms)`,
                                   y = `Voltage (mV)`,
                                   label = `Voltage (mV)`)) +
      theme(legend.text = element_text(face = "bold"),
            legend.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 10, face = "bold", hjust = c(0,1,1), colour = c("black", "#F8766D", "black")),
            axis.title = element_text(size = 10, face = "bold"),
            plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
      labs(subtitle = c(paste("Sweep Number:", s), paste("Computer Count of Peaks:", computer_count), paste("\n", "Human Count of Peaks:", human_count))) +
      ggtitle(get("i"))
    pdfname <- paste(get("i"), get("s"), sep = "_Sweep_") %>% paste(".pdf", sep = "")
    ggsave(filename = pdfname, device = "pdf", plot = tmp_pl, path = "Difference Plots", width = 10, height = 6.5)
  }
}

pdftools::pdf_combine(input = list.files(path = "Difference Plots", full.names=TRUE, pattern=".pdf"),
                      output = "AP Count Differences Plots.pdf")

unlink("Difference Plots", recursive = TRUE)
