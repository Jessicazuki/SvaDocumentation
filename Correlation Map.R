# Load necessary library for data manipulation and plotting
library(dplyr)
library(ggplot2)

# Read the dataset (assumes 'aus_green_growth.csv' is in working directory)
data <- read.csv("aus_green_growth.csv")

# Filter data for the two indicators:
# - CO2 Productivity (Production-based, GDP per unit CO2)
# - Total energy supply (to derive energy intensity per GDP)
co2_prod <- data %>%
  filter(Measure == "Production-based CO2 productivity, GDP per unit of energy-related CO2 emissions") %>%
  select(Year = TIME_PERIOD, CO2Prod = OBS_VALUE)

energy <- data %>%
  filter(Measure == "Total energy supply", Unit.of.measure == "Tonnes of oil equivalent") %>%
  select(Year = TIME_PERIOD, Energy = OBS_VALUE, EnergyUnitMult = UNIT_MULT)

gdp <- data %>%
  filter(Measure == "Real GDP", Unit.of.measure == "US dollars, PPP converted", grepl("Constant prices", Price.base)) %>%
  select(Year = TIME_PERIOD, GDP = OBS_VALUE, GDPUnitMult = UNIT_MULT)

# Merge energy and GDP to compute energy intensity (energy per GDP)
energy_gdp <- inner_join(energy, gdp, by="Year")
# Apply unit multipliers (e.g., 6 indicates values are in millions)
energy_gdp <- energy_gdp %>%
  mutate(EnergyTJ = Energy * 10^EnergyUnitMult,   # total energy in base units (e.g., toe)
         GDPUSD   = GDP * 10^GDPUnitMult)         # GDP in USD
energy_gdp <- energy_gdp %>%
  mutate(EnergyIntensity = EnergyTJ / GDPUSD) %>%  # energy per USD
  select(Year, EnergyIntensity)

# Join with CO2 productivity data
df_corr <- inner_join(co2_prod, energy_gdp, by="Year")
df_corr <- df_corr %>% filter(Year <= 2022)  # exclude 2023 if CO2Prod not available

# Plot a scatter with a trendline
ggplot(df_corr, aes(x = EnergyIntensity * 1000, y = CO2Prod)) +  # scale: per 1000 USD for readability
  geom_point(color="steelblue", size=3) +
  geom_smooth(method="lm", se=FALSE, color="darkblue", linetype="dashed") +  # linear trend line
  geom_text(aes(label = Year), vjust = -1, color="steelblue") +             # label points by year
  labs(title = "CO₂ Productivity vs Energy Intensity (Australia, 2017–2022)",
       x = "Energy intensity (tonnes of oil eq. per $1000 GDP)",
       y = "CO₂ productivity (GDP per tonne CO₂, thousand US$)") +
  theme_minimal(base_size = 13) +    # clean theme with larger text
  theme(plot.title = element_text(face="bold"),
        axis.title = element_text(face="bold"))



# Calculate annual GDP growth (%) from Real GDP
gdp_series <- gdp %>% arrange(Year) %>%
  mutate(GDP_growth = (GDPUSD / lag(GDPUSD) - 1) * 100)

# Prepare renewable energy share data and merge with GDP growth
renew <- data %>%
  filter(Measure == "Renewable energy supply", Unit.of.measure == "Percentage of energy supply") %>%
  select(Year = TIME_PERIOD, RenewShare = OBS_VALUE)
# Use GDP growth from 2018 onward (since 2017 has no prior year for growth rate)
growth <- gdp_series %>%
  filter(!is.na(GDP_growth)) %>%
  select(Year, GDP_growth)

df_line <- inner_join(renew, growth, by="Year") %>%
  filter(Year >= 2018)

# Plot both series on the same chart
ggplot(df_line, aes(x = Year)) +
  geom_line(aes(y = RenewShare, color="Renewable Share"), size=1.2) +
  geom_point(aes(y = RenewShare, color="Renewable Share"), size=3) +
  geom_line(aes(y = GDP_growth, color="GDP Growth"), size=1.2) +
  geom_point(aes(y = GDP_growth, color="GDP Growth"), shape=15, size=3) +
  geom_hline(yintercept = 0, linetype="dotted", color="gray40") +        # reference line at 0% growth
  scale_color_manual(name="Indicator",
                     values=c("Renewable Share"="forestgreen", "GDP Growth"="orange")) +
  scale_x_continuous(breaks = 2018:2023) +
  labs(title = "Renewable Energy Share vs GDP Growth (Australia, 2018–2022)",
       x = "Year", y = "Percentage (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        legend.position = "top")

