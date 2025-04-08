# Preview the dataset

getwd()
setwd("/Users/jessicazhao/R scripts")
green_data <- read.csv("aus_green_growth.csv")
head(green_data)

install.packages("plotly")
install.packages("purrr")
library(purrr)

# Load libraries
library(dplyr)
library(ggplot2)
library(plotly)

# Load your dataset
data <- read.csv("aus_green_growth.csv")

# --- 1. Select Indicators ---

# CO₂ Productivity (production-based)
co2 <- data %>%
  filter(Measure == "Production-based CO2 productivity, GDP per unit of energy-related CO2 emissions") %>%
  select(Year = TIME_PERIOD, CO2Prod = OBS_VALUE)

# Renewable Energy Share
renew <- data %>%
  filter(Measure == "Renewable energy supply", Unit.of.measure == "Percentage of energy supply") %>%
  select(Year = TIME_PERIOD, RenewShare = OBS_VALUE)

# Energy Productivity = GDP / Energy use
energy <- data %>%
  filter(Measure == "Total energy supply", Unit.of.measure == "Tonnes of oil equivalent") %>%
  select(Year = TIME_PERIOD, Energy = OBS_VALUE, EMult = UNIT_MULT)

gdp <- data %>%
  filter(Measure == "Real GDP", Unit.of.measure == "US dollars, PPP converted", grepl("Constant", Price.base)) %>%
  select(Year = TIME_PERIOD, GDP = OBS_VALUE, GMult = UNIT_MULT)

# --- 2. Clean and Calculate Energy Productivity ---

# Apply multipliers
energy$Energy <- energy$Energy * 10^as.numeric(energy$EMult)
gdp$GDP <- gdp$GDP * 10^as.numeric(gdp$GMult)

# Merge and calculate
energy_prod <- inner_join(energy, gdp, by = "Year") %>%
  mutate(EnergyProd = GDP / Energy) %>%
  select(Year, EnergyProd)

# --- 3. Merge All Indicators ---

green_data <- reduce(list(co2, renew, energy_prod), full_join, by = "Year") %>%
  filter(!is.na(CO2Prod), !is.na(RenewShare), !is.na(EnergyProd))

# --- 4. Normalize Each Indicator ---

normalize <- function(x) (x - min(x)) / (max(x) - min(x))

green_data <- green_data %>%
  mutate(
    CO2Norm = normalize(CO2Prod),
    RenewNorm = normalize(RenewShare),
    EnergyNorm = normalize(EnergyProd),
    GGI = round((CO2Norm + RenewNorm + EnergyNorm) / 3, 3)
  )

# --- 5. Create Interactive Line Chart ---

p <- ggplot(green_data, aes(x = Year, y = GGI, text = paste0(
  "Year: ", Year, "<br>",
  "CO₂ Productivity: ", round(CO2Prod, 2), "<br>",
  "Renewable Share: ", round(RenewShare, 2), "%<br>",
  "Energy Productivity: ", round(EnergyProd, 4)
))) +
  geom_line(color = "#2E7D32", size = 1.3) +
  geom_point(color = "#66BB6A", size = 3) +
  labs(
    title = "Australia's Green Growth Index (GGI)",
    subtitle = "Blending CO₂ efficiency, renewable energy, and energy productivity",
    y = "Green Growth Index (0–1)", x = "Year"
  ) +
  theme_minimal(base_size = 14)

# Convert to Plotly for interactivity
ggplotly(p, tooltip = "text")


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)

# --- Load your data ---
data <- read.csv("aus_green_growth.csv")

# --- Extract indicators for GGI ---
co2 <- data %>%
  filter(Measure == "Production-based CO2 productivity, GDP per unit of energy-related CO2 emissions") %>%
  select(Year = TIME_PERIOD, CO2Prod = OBS_VALUE)

renew <- data %>%
  filter(Measure == "Renewable energy supply", Unit.of.measure == "Percentage of energy supply") %>%
  select(Year = TIME_PERIOD, RenewShare = OBS_VALUE)

energy <- data %>%
  filter(Measure == "Total energy supply", Unit.of.measure == "Tonnes of oil equivalent") %>%
  select(Year = TIME_PERIOD, Energy = OBS_VALUE, EMult = UNIT_MULT)

gdp <- data %>%
  filter(Measure == "Real GDP", Unit.of.measure == "US dollars, PPP converted", grepl("Constant", Price.base)) %>%
  select(Year = TIME_PERIOD, GDP = OBS_VALUE, GMult = UNIT_MULT)

# --- Unit conversion ---
energy$Energy <- energy$Energy * 10^as.numeric(energy$EMult)
gdp$GDP <- gdp$GDP * 10^as.numeric(gdp$GMult)

energy_prod <- inner_join(energy, gdp, by = "Year") %>%
  mutate(EnergyProd = GDP / Energy) %>%
  select(Year, EnergyProd)

# --- Merge and normalize for GGI ---
green_data <- reduce(list(co2, renew, energy_prod), full_join, by = "Year") %>%
  filter(!is.na(CO2Prod), !is.na(RenewShare), !is.na(EnergyProd))

normalize <- function(x) (x - min(x)) / (max(x) - min(x))

green_data <- green_data %>%
  mutate(
    CO2Norm = normalize(CO2Prod),
    RenewNorm = normalize(RenewShare),
    EnergyNorm = normalize(EnergyProd),
    GGI = round((CO2Norm + RenewNorm + EnergyNorm) / 3, 3)
  )

# --- Make it BEAUTIFUL with ggplot + Plotly ---
gg <- ggplot(green_data, aes(x = Year, y = GGI)) +
  geom_line(color = "#00695C", size = 1.5) +
  geom_point(aes(text = paste0(
    "<b>Year:</b> ", Year, "<br>",
    "<b>GGI:</b> ", GGI, "<br>",
    "<b>CO₂ Productivity:</b> ", round(CO2Prod, 2), "<br>",
    "<b>Renewable Energy Share:</b> ", round(RenewShare, 2), "%<br>",
    "<b>Energy Productivity:</b> ", round(EnergyProd, 4)
  )),
  fill = "#26A69A", color = "white", shape = 21, size = 5, stroke = 1.2) +
  labs(
    title = " Australia's Green Growth Index (GGI)",
    subtitle = "Composite of CO₂ efficiency, renewable share & energy productivity",
    x = "Year",
    y = "Green Growth Index (0–1)",
    caption = "Source: OECD Green Growth Indicators"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.05, 0.1))) +
  theme_minimal(base_family = "Helvetica", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#004D40"),
    plot.subtitle = element_text(size = 14, color = "#555"),
    plot.caption = element_text(size = 10, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90")
  )

# Convert to interactive plot
ggplotly(gg, tooltip = "text")

