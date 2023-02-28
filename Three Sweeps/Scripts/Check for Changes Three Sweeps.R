rm(list=ls())
library(tidyverse)

setwd("~/Desktop/Lab data/Mangieri Lab/Data Analysis/Three Sweeps/Output")

humandf <- read_csv("Human Data Analysis Output Three Sweeps.csv") %>% 
           drop_na

computerdf <- read_csv("Computer Data Analysis Output Three Sweeps.csv") %>% 
              transmute(`Filename_Em and Rin`,
                        `Average Resting Em`,
                        `Average Rin` = `Average R in`)



# verify columns are same
# names(humandf) == names(computerdf)
# sapply(humandf, class) == sapply(computerdf, class)

differencedf <- humandf %>% 
  left_join(computerdf, by = c("Filename_Em and Rin")) %>% 
  transmute(`Filename_Em and Rin`,
            `Average Resting Em Difference` = `Average Resting Em.x` - `Average Resting Em.y`,
            `Average Rin Difference` = `Average Rin.x` - `Average Rin.y`)

# check if the filenames are in the same order as original dataframes
# differencedf$`Filename_Em and Rin` == humandf$`Filename_Em and Rin`
# differencedf$`Filename_Em and Rin` == computerdf$`Filename_Em and Rin`

#write_csv(differencedf, "Three Sweeps Differences.csv")

differencedf %>%
  filter(`Average Resting Em Difference` > 1)

differencedf %>%
  filter(`Average Resting Em Difference` < -1)

differencedf %>%
  filter(`Average Rin Difference` > 1)

differencedf %>%
  filter(`Average Rin Difference` < -1)

differences <- differencedf %>% 
               filter(`Filename_Em and Rin` == "22n10017" |
                      `Filename_Em and Rin` == "22n10024" |
                      `Filename_Em and Rin` == "22n08018" |
                      `Filename_Em and Rin` == "22n09017" |
                      `Filename_Em and Rin` == "22n11015")

humandifferences <- humandf %>% 
  filter(`Filename_Em and Rin` == "22n10017" |
         `Filename_Em and Rin` == "22n10024" |
         `Filename_Em and Rin` == "22n08018" |
         `Filename_Em and Rin` == "22n09017" |
         `Filename_Em and Rin` == "22n11015") %>% 
  mutate(`Data Collected By` = "Human")

computerdifferences <- computerdf %>% 
  filter(`Filename_Em and Rin` == "22n10017" |
         `Filename_Em and Rin` == "22n10024" |
         `Filename_Em and Rin` == "22n08018" |
         `Filename_Em and Rin` == "22n09017" |
         `Filename_Em and Rin` == "22n11015") %>% 
  mutate(`Data Collected By` = "Computer")


h <- left_join(humandifferences, differences, by = "Filename_Em and Rin")

differences2 <- differences %>%
                transmute(`Filename_Em and Rin`,
                          `Average Resting Em Difference` = (`Average Resting Em Difference`*-1),
                          `Average Rin Difference` = (`Average Rin Difference`*-1))

c <- left_join(computerdifferences, differences2, by = "Filename_Em and Rin")

majordifferences <- rbind(h, c) %>% mutate_if(is.numeric, round)

write_csv(majordifferences, "Three Sweeps Differences Rounded.csv")



setwd("~/Desktop/Lab data/Mangieri Lab/Data Analysis/Three Sweeps/CSVs")
dir.create("./Plots")

myfile <- NULL
tmp_pl <- NULL
pdfname <- NULL

for (i in majordifferences$`Filename_Em and Rin`) {
  myfile <- paste(i, ".csv", sep = "")
  df <- read_csv(myfile, col_names = FALSE) %>% 
    slice(-c(1:2)) %>% 
    select(
      where(
        ~!all(is.na(.x))
      )
    )
  sweeps_count <- df %>% select(2:last_col()) %>% ncol()
  old_names <- df %>% select(2:last_col()) %>% names() %>% as.vector()
  new_names <- seq(1:sweeps_count) %>% as.vector()
  df1 <- df %>% 
    rename_with(~ new_names[which(old_names == .x)], .cols = old_names) %>% 
    pivot_longer(cols = 2:last_col(),
                 values_to = "Voltage (mV)",
                 names_to = "Sweep") %>% 
    group_by(Sweep) %>% 
    transmute(`Time (ms)` = as.numeric(X1)*1000,
              `Voltage (mV)` = as.numeric(`Voltage (mV)`),
              Sweep = as.factor(Sweep)) %>% 
    arrange(Sweep) 
  
  # remove variables we don't need later
  rm(df, sweeps_count, old_names, new_names)
  
  tmp_pl <- df1 %>% 
    group_by(Sweep) %>% 
    ggplot(aes(x = `Time (ms)` , 
               y = `Voltage (mV)`,
               color = Sweep)) +
    geom_line(show.legend = TRUE) +
    theme(legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, face = "bold", hjust = c(0,1)),
          axis.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
    ggtitle(get("i"))
  pdfname <- paste(get("i"), ".pdf", sep = "")
  ggsave(filename = pdfname, plot = tmp_pl, path = "./Plots/", width = 10, height = 6.5)
}

pdftools::pdf_combine(input = list.files(path = "./Plots/", full.names=TRUE, pattern=".pdf"),
                      output = "~/Desktop/Lab data/Mangieri Lab/Data Analysis/Three Sweeps/Output/All Differences Three Sweeps Plots.pdf")
unlink("./Plots/", recursive = TRUE)
