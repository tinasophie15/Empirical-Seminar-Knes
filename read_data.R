# - WORKING DIRECTORY ---------------------------------------------------------
# set the working directory:
setwd("C:/Users/knesc/OneDrive/Documents/Uni/Master/2. Semester/ES/Allgemeines")

library(writexl)
library(readxl)
library(dplyr)

# - LOADING AND SAVING DATA ---------------------------------------------------
pretest <- read_excel("PreTest_wholeText.xlsx")
posttest <- read_excel("PostTest_wholeText.xlsx")
gamedata <- read_excel("game_data.xlsx")

# Merge all datasets on the common UserID
merged_data <- pretest |>
  inner_join(gamedata, by = "UID") |>
  inner_join(posttest, by = "UID")

df_clean <- merged_data %>%
  mutate(UID = as.character(UID)) %>%
  distinct(UID, .keep_all = TRUE)

# Check the result
cat("Number of matched users:", nrow(df_clean), "\n")
write_xlsx(df_clean, "merged_data.xlsx")
View(df_clean)