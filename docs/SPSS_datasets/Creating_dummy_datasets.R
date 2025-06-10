setwd("C:/Users/lhill3/OneDrive - University of Edinburgh/Work_Directory/Teaching/2024_25/CAP_pathway_Intro_to_Stats/CAP_pathway_Intro_to_Stats/SPSS_datasets/")

# Load necessary libraries
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Generate participant ID numbers
ID_number <- 1:300

# Generate prosocial_sum scores (normally distributed around 5)
prosocial_sum <- round(rnorm(300, mean = 5, sd = 2))
prosocial_sum <- pmin(pmax(prosocial_sum, 0), 10)  # Constrain to 0-10 range

# Generate peer_probs_sum scores with a negative relation to prosocial_sum
peer_probs_sum <- round(10 - prosocial_sum + rnorm(300, mean = 0, sd = 1))
peer_probs_sum <- pmin(pmax(peer_probs_sum, 0), 10)  # Constrain to 0-10 range

# Generate Gest_Age_wks with skewed distribution
Gest_Age_wks <- ifelse(
  runif(300) < 0.7,
  sample(39:41, 300, replace = TRUE),
  round(rbeta(300, 2, 5) * 16 + 26)
)
Gest_Age_wks <- round(Gest_Age_wks + 0.3 * prosocial_sum - 0.2 * peer_probs_sum)
Gest_Age_wks <- pmin(pmax(Gest_Age_wks, 26), 42)  # Constrain to 26-42 range

# Generate Sex_at_birth variable, coded 1 or 2
Sex_at_birth <- sample(c(1, 2), 300, replace = TRUE)

# Generate p_confidence categories
p_confidence <- case_when(
  prosocial_sum >= 8 ~ sample(c(2, 3), 300, replace = TRUE, prob = c(0.3, 0.7)),
  prosocial_sum <= 3 ~ sample(c(1, 2), 300, replace = TRUE, prob = c(0.7, 0.3)),
  TRUE              ~ sample(c(1, 2, 3), 300, replace = TRUE, prob = c(0.2, 0.6, 0.2))
)

# Generate Ethnicity categories
Ethnicity <- sample(1:5, 300, replace = TRUE, prob = c(0.65, 0.2, 0.1, 0.04, 0.01))

# Combine into a dataframe
df <- data.frame(
  ID_number = ID_number,
  prosocial_sum = prosocial_sum,
  peer_probs_sum = peer_probs_sum,
  Gest_Age_wks = Gest_Age_wks,
  Sex_at_birth = Sex_at_birth,
  p_confidence = p_confidence,
  Ethnicity = Ethnicity
)

# Write the dataframe to a CSV file
write.csv(df, "session_1_data.csv", row.names = FALSE)

