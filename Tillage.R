#Wrangle data
library(tidyverse)

tillage <- read.csv("Tillage EF_wide_wrangle.csv")

# Generate a sequence of years from 1990 to 2022
years <- seq(1990, 2022)

# Create an empty vector to store the column names
column_names <- character(length(years) * 2)

# Assign names for CO2e and Act.Area columns for each year
for (i in seq_along(years)) {
  column_names[(i - 1) * 2 + 1] <- paste0("CO2e_", years[i])
  column_names[(i - 1) * 2 + 2] <- paste0("Act.Area_", years[i])
}

# Assign the generated column names to the respective columns
colnames(tillage)[-(1:2)] <- column_names


tillage_long <- pivot_longer(tillage, 
                             cols = -c(Province, Practices), 
                             names_to = c(".value", "Year"), 
                             names_pattern = "(CO2e|Act\\.Area)_(\\d{4})")

# Group by Province, Practice, and Year, then calculate the average value
tillage_avg <- tillage_long %>%
  mutate(IEF = CO2e/Act.Area) %>%
  group_by(Province, Practices, Year) %>%
  summarize(Average = mean(IEF, na.rm = TRUE)*10^6) #convert to kg/ha/y

#Categorize them to source/sink
Practice <- unique(tillage_avg$Practices) #Arrange it to have the order I want (move total to the last one)
Practice_level <- c(Practice[-5],Practice[5])  
tillage_avg <- tillage_avg %>%
  mutate(Practices = factor(Practices, levels = Practice_level)) %>%
  mutate(`SinkSource` = case_when(
    Practices %in% Practice_level[c(3,5:6)]~ "Source",
    Practices %in% Practice_level[c(1:2,4)] ~ "Sink",
    TRUE ~ "Total")) %>%
  arrange(Practices, Year)


#Take average for recent 10 years (2012-2022); Practice separately 
tillage_avg_ten <- tillage_avg %>%
  filter(Year >= 2012 & Year <= 2022) %>%
  group_by(Province, Practices) %>%
  summarize(Average.ten = mean(Average, na.rm= T)) %>%
  mutate(Average.ten = round(Average.ten,1))

# Take average recent 10 years (2012-2022),  Maritime regions
tillage_region_ten <- tillage_long %>%
  mutate(Province = ifelse(Province %in% c("NB", "NL", "NS", "PE"), "MT", Province)) %>%
  na.omit()%>%
  filter(Year >= 2012 & Year <= 2022) %>%
  group_by(Province, Practices) %>%
  summarize(CO2e = sum(CO2e),
            Act.Area = sum(Act.Area),
            IEF = round(CO2e/Act.Area*10^6,1),
            .groups = 'drop') %>%
  mutate(Practices = factor(Practices, levels = Practice_level)) %>%
  arrange(Practices, Province)
  

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
write.xlsx(as.data.frame(tillage_region_ten), file = excel_file, sheetName = "Avg_region_ten", row.names = FALSE)
write.xlsx(as.data.frame(tillage_avg), file = excel_file, sheetName = "Avg_practices", row.names = FALSE, append =TRUE)
write.xlsx(as.data.frame(tillage_avg_ten), file = excel_file, sheetName = "Avg_practices_ten", row.names = FALSE, append = TRUE)
write.xlsx(as.data.frame(tillage_EF), file = excel_file, sheetName = "Avg_sinksource", row.names = FALSE, append = TRUE)
write.xlsx(as.data.frame(tillage_EF_ten), file = excel_file, sheetName = "Avg_sinksource_ten", row.names = FALSE, append = TRUE)


