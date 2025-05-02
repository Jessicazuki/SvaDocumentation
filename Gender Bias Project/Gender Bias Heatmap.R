# Install required packages
install.packages("ggplot2")
install.packages("reshape2")
install.packages("RColorBrewer")

library(ggplot2)
library(reshape2)
library(RColorBrewer)

# Prepare data
data <- data.frame(
  Country = c("USA", "Sweden", "Germany", "Spain", "Japan", "China", "India", "Nigeria", "Pakistan", "Egypt"),
  Men_better_leaders = c(80, 56, 60, 57, 66, 58, 82, 91, 95, 92),
  Education_for_boys = c(10, 5, 7, 6, 12, 21, 43, 75, 80, 85),
  Men_better_execs = c(13, 12, 15, 16, 23, 30, 41, 70, 75, 78),
  Men_jobs_first = c(61, 22, 25, 26, 30, 44, 65, 80, 87, 88)
)

# Reshape data
data_long <- melt(data, id.vars = "Country", variable.name = "Category", value.name = "Agreement")

# Define custom pastel color palette (similar to your radar palette)
my_palette <- colorRampPalette(c("#f57c6e", "#f2b56e", "#fbe79e", "#84c3b7", "#88d7da", "#b692c4"))(100)

# Plot heatmap
ggplot(data_long, aes(x = Category, y = Country, fill = Agreement)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_gradientn(colors = my_palette) +
  geom_text(aes(label = Agreement), color = "#333333", size = 5, fontface = "bold") +
  labs(title = "Heatmap of Sexist Attitudes by Country and Category",
       x = "Sexist Statement Category",
       y = "Country",
       fill = "% Agreement") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "bold", color = "#222222"),
        axis.text.y = element_text(face = "bold", color = "#222222"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"))
