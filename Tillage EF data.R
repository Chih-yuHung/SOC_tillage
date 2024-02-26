#Wrangle Tillage EF data, make wide format to long format
library(tidyverse)

setwd("C:/Users/HungC/OneDrive - EC-EC/2023/Fuel LCA/2. Soil Carbon/Tillage")
tillage <- read.csv("Tillage EF_data wrangle.csv")

# Use the gather function to reshape your data from wide to long format
tillage_long <- tillage %>%
  gather(key = "Year", value = "Value", -Province, -Practice) %>%
  na.omit()

# Extract the year from the "Year" column
tillage_long$Year <- gsub("X", "", tillage_long$Year)

# Convert Year column to numeric
tillage_long$Year <- as.numeric(tillage_long$Year)

# Group by Province, Practice, and Year, then calculate the average value
tillage_avg <- tillage_long %>%
  group_by(Province, Practice, Year) %>%
  summarize(Average = mean(Value, na.rm = TRUE))

#Arrange it to have the order I want (move total to the last one)
Practice <- unique(tillage_avg$Practice)
Practice_level <- c(Practice[-5],Practice[5])  
  
#Categorize them to source/sink
tillage_avg <- tillage_avg %>%
  mutate(Practice = factor(Practice, levels = Practice_level)) %>%
  mutate(`SinkSource` = case_when(
    Practice %in% Practice_level[c(3,5:6)]~ "Source",
    Practice %in% Practice_level[c(1:2,4)] ~ "Sink",
    TRUE ~ "Total")) %>%
    arrange(Practice, Year) %>%
    mutate(Average = Average*10^6) #Covert to kg CO2 ha-1

#Take average for recent 10 years (2012-2022); Practice separately 
tillage_avg_ten <- tillage_avg %>%
  filter(Year >= 2012 & Year <= 2022) %>%
  group_by(Province, Practice) %>%
  summarize(Average.ten = mean(Average, na.rm= T)) %>%
  mutate(Average.ten = round(Average.ten,1))

# Take average recent 10 years (2012-2022),  Maritime regions
tillage_avg_ten <- tillage_avg_ten %>%
  mutate(Province = ifelse(Province %in% c("NB", "NL", "NS", "PE"), "MT", Province)) %>%
  group_by(Province, Practice) %>%
  summarize(Average.ten = mean(Average.ten, na.rm = TRUE)) %>%
  mutate(Average.ten = round(Average.ten,1))

# Take average recent 10 years (2012-2022), Maritime regions regardless of practice
tillage_avg_all <- tillage_avg %>%
  filter(Year >= 2012 & Year <= 2022) %>%
  mutate(Province = ifelse(Province %in% c("NB", "NL", "NS", "PE"), "MT", Province)) %>%
  group_by(Province) %>%
  summarize(Average.all = mean(Average, na.rm= T)) %>%
  mutate(Average.all = round(Average.all,1))


#Group by source or sink
tillage_EF <- tillage_avg %>%
  group_by(SinkSource, Province, Year) %>%
  summarize(EF = mean(Average, na.rm = TRUE))

#Take average for 2012-2022; Practice combined
tillage_EF_ten <- tillage_EF %>%
  filter(Year >= 2012 & Year <= 2022) %>%
  group_by(Province, SinkSource) %>%
  summarize(EF.ten = mean(EF, na.rm= T)) %>%
  mutate(EF.ten = round(EF.ten,1))

# Take average for Maritime regions
tillage_EF_ten <- tillage_EF_ten %>%
  mutate(Province = ifelse(Province %in% c("NB", "NL", "NS", "PE"), "MT", Province)) %>%
  group_by(Province, SinkSource) %>%
  summarize(EF.ten = mean(EF.ten, na.rm = TRUE))  %>%
  mutate(EF.ten = round(EF.ten,1))


#Export files
library(xlsx)
excel_file <- "Tillage EF.xlsx"
# Write the data frames to different sheets in the Excel file
write.xlsx(as.data.frame(tillage_avg), file = excel_file, sheetName = "Avg_practices", row.names = FALSE)
write.xlsx(as.data.frame(tillage_avg_ten), file = excel_file, sheetName = "Avg_practices_ten", row.names = FALSE, append = TRUE)
write.xlsx(as.data.frame(tillage_EF), file = excel_file, sheetName = "Avg_sinksource", row.names = FALSE, append = TRUE)
write.xlsx(as.data.frame(tillage_EF_ten), file = excel_file, sheetName = "Avg_sinksource_ten", row.names = FALSE, append = TRUE)
