# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(janitor)
library(ggrepel)

# Import dataset
data <- read_csv("Jan 2021 Data - Viz5 Gender Inequality and HIVAIDS.csv") %>%
  clean_names()

# Filter for 2018, Age 10-19
data_2018 <- data %>%
  filter(year == 2018, age == "Age 10-19") %>%
  select(
    country,
    region = unicef_region,
    sex,
    incidence_rate = estimated_incidence_rate_of_new_hiv_infection_per_1_000_uninfected_population
  )

# Pivot wide: Female vs Male incidence
data_2018_wide <- data_2018 %>%
  pivot_wider(names_from = sex, values_from = incidence_rate) %>%
  rename(FemaleIncidence = Female, MaleIncidence = Male)

# Identify countries to label (South Africa and Lesotho)
label_countries <- c("South Africa", "Lesotho")

# Scatterplot with enhancements
ggplot(data_2018_wide, aes(x = MaleIncidence, y = FemaleIncidence, color = region)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(
    data = subset(data_2018_wide, country %in% label_countries),
    aes(label = country),
    size = 4,
    fontface = "bold",
    box.padding = 0.3,
    point.padding = 0.2,
    show.legend = FALSE
  ) +
  labs(
    title = "Gender Disparities in Adolescent HIV Incidence in Africa (2018)",
    subtitle = "Scatterplot of female vs male incidence rates (ages 10–19) per 1,000 uninfected population.\nDashed line = parity (equal rates).",
    x = "Male HIV Incidence (per 1,000 uninfected adolescents)",
    y = "Female HIV Incidence (per 1,000 uninfected adolescents)",
    color = "UNICEF Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Save plot
ggsave("HIV_gender_disparities_scatterplot_enhanced_2018.png", width = 9, height = 7)

