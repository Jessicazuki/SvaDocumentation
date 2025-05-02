# Load required libraries
library(tidyverse)

# Load the WVS Wave 7 dataset
data <- read.csv("WVS_Wave7.csv")

# Select only necessary columns (country code and 4 gender attitude questions)
data_selected <- data %>% select(Country = B_COUNTRY, D059, D060, D078, C001)

# Recode Likert responses:
# 1, 2 → Agree → 1
# 3, 4 (and 5 for C001) → Disagree or Neutral → 0
# NA / Don't know → keep as NA

recode_agree <- function(x) {
  ifelse(x %in% c(1, 2), 1,
         ifelse(x %in% c(3, 4, 5), 0, NA))
}

data_selected <- data_selected %>%
  mutate(
    D059_agree = recode_agree(D059),
    D060_agree = recode_agree(D060),
    D078_agree = recode_agree(D078),
    C001_agree = recode_agree(C001)
  )

# Aggregate by country and calculate percentage of agreement
country_agreement <- data_selected %>%
  group_by(Country) %>%
  summarise(
    Leaders_bias_pct = mean(D059_agree, na.rm = TRUE) * 100,
    Education_bias_pct = mean(D060_agree, na.rm = TRUE) * 100,
    Business_bias_pct = mean(D078_agree, na.rm = TRUE) * 100,
    Jobs_bias_pct = mean(C001_agree, na.rm = TRUE) * 100
  )

# View processed data
head(country_agreement)

# Export if needed
write.csv(country_agreement, "Gender_Bias_Country_Level_Agreement.csv", row.names = FALSE)


# Install required packages
# install.packages("fmsb")

library(fmsb)

# Prepare data
data <- data.frame(
  row.names = c("Max", "Min", "USA", "Sweden", "Spain", "Japan", "Pakistan", "Egypt", "Global_Avg"),
  Men_better_leaders = c(100, 0, 80, 56, 57, 66, 95, 92, mean(c(80,56,57,66,95,92))),
  Education_for_boys = c(100, 0, 10, 5, 6, 12, 80, 85, mean(c(10,5,6,12,80,85))),
  Men_better_execs = c(100, 0, 13, 12, 16, 23, 75, 78, mean(c(13,12,16,23,75,78))),
  Men_jobs_first = c(100, 0, 61, 22, 26, 30, 87, 88, mean(c(61,22,26,30,87,88)))
)

# Custom colors
colors_border <- c("#f57c6e", "#f2b56e", "#fbe79e", "#84c3b7", "#5AA1E3", "#B692C4", "#000000")
line_types <- c(1, 1, 1, 1, 1, 1, 2)
line_widths <- c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 3.5)

# Plot
par(mar = c(1, 2, 2, 2))
radarchart(data,
           axistype = 1,
           pcol = colors_border,
           plwd = line_widths,
           plty = line_types,
           cglcol = "#DDDDDD",
           cglty = 1,
           axislabcol = "#BBBBBB",
           caxislabels = seq(10, 100, 10),
           cglwd = 1.2,
           vlcex = 1.2,
           title = "Patterns of Internalized Misogyny by Country (with Global Average)"
)

legend("topright",
       legend = c("USA", "Sweden", "Spain", "Japan", "Pakistan", "Egypt", "Global Avg"),
       col = colors_border,
       lty = line_types,
       lwd = 2.5,
       bty = "n",
       cex = 0.9,
       title = "Country"
)

