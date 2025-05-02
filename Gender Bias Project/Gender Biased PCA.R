# Install packages if needed
# install.packages(c("tidyverse", "FactoMineR", "factoextra", "ggrepel"))

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggrepel)

# Load gender bias data
data <- read.csv("~/R scripts/Gender_Bias_Country_Level_Agreement.csv")

# Prepare mapping from country code to country name (example below)
country_mapping <- data.frame(
  Country = c(818, 586, 840, 752, 156, 364, 804),  # Add all codes here (I will help generate full version later)
  Country_Name = c("Egypt", "Pakistan", "United States", "Sweden", "China", "India", "Ukraine")
)

# Merge to get country names
data <- left_join(data, country_mapping, by = c("Country" = "Country"))

# Replace Country column with proper country names
pca_data <- data %>% select(-Country, -Country_Name)
country_names <- data$Country_Name

# Handle missing values
pca_data_filled <- pca_data %>% mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Run PCA
res.pca <- PCA(pca_data_filled, graph = FALSE)
pca_coords <- as.data.frame(res.pca$ind$coord[, 1:2])
colnames(pca_coords) <- c("Overall_Gender_Bias", "Bias_Type_Difference")
pca_coords$Country <- country_names

# Run KMeans
set.seed(123)
km_res <- kmeans(pca_coords[, 1:2], centers = 3, nstart = 25)
pca_coords$Cluster <- as.factor(km_res$cluster)

# Use uploaded colors
low_bias_color <- "#84c3b7"    # Soft green
medium_bias_color <- "#fbe79e" # Soft yellow
high_bias_color <- "#f57c6e"   # Soft red

# Plot with consistent style
p <- ggplot(pca_coords, aes(x = Overall_Gender_Bias, y = Bias_Type_Difference, color = Cluster)) +
  geom_point(size = 20, alpha = 0.7) +
  geom_text_repel(aes(label = Country), size = 4, family = "Helvetica", fontface = "bold", color = "#333333", max.overlaps = 100) +
  scale_color_manual(values = c(low_bias_color, medium_bias_color, high_bias_color),
                     labels = c("Low Bias", "Medium Bias", "High Bias")) +
  labs(title = "PCA Clustering of Countries by Gender Attitudes",
       x = "Overall Gender Bias (Higher → More Biased)",
       y = "Bias Type Difference (Higher → Leadership Bias)") +
  theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14, family = "Helvetica"),
    axis.title = element_text(size = 16, family = "Helvetica", face = "bold"),
    axis.text = element_text(size = 14, family = "Helvetica", color = "#444444"),
    plot.title = element_text(size = 20, family = "Helvetica", face = "bold", hjust = 0.5)
  )

# Show plot
print(p)

# Save as PNG
ggsave("PCA_Gender_Bias_Clusters_CountryNames.png", plot = p, width = 12, height = 8, dpi = 300)
getwd()
