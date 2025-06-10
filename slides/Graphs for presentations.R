library(ggplot2)
library(dplyr)
library(gridExtra)

# Sample sizes
n <- 1000

# Generate data
set.seed(123)
scores_sd10 <- rnorm(n, mean = 50, sd = 10)
scores_sd20 <- rnorm(n, mean = 50, sd = 20)

# Create data frames
data_sd10 <- data.frame(score = pmax(pmin(round(scores_sd10), 100), 0))
data_sd20 <- data.frame(score = pmax(pmin(round(scores_sd20), 100), 0))

# Plot for SD = 10
plot_sd10 <- ggplot(data_sd10, aes(x = score)) +
  geom_bar() +
  labs(title = "(Mean = 50, SD = 10)", x = "Test Score", y = "Number of Pupils") +
  theme_minimal() +
  xlim(0, 100) +
  geom_vline(xintercept = mean(data_sd10$score), color = "red", linetype = "dashed")

# Plot for SD = 20
plot_sd20 <- ggplot(data_sd20, aes(x = score)) +
  geom_bar() +
  labs(title = "(Mean = 50, SD = 20)", x = "Test Score", y = "Number of Pupils") +
  theme_minimal() +
  xlim(0, 100) +
  geom_vline(xintercept = mean(data_sd20$score), color = "red", linetype = "dashed")

# Save plots side-by-side in a .png file
grid.arrange(plot_sd10, plot_sd20, ncol = 2)

### 

library(flextable)

# Sample sizes for each school
n <- 40

# Generate data with specified means and standard deviations
set.seed(123)
scores_A <- rnorm(n, mean = 50, sd = 10)
scores_B <- rnorm(n, mean = 60, sd = 10)
scores_C <- rnorm(n, mean = 70, sd = 30)
scores_D <- rnorm(n, mean = 70, sd = 10)

# Create data frame
data <- data.frame(
  score = c(scores_A, scores_B, scores_C, scores_D),
  school = factor(rep(c("A", "B", "C", "D"), each = n))
)

# Clamp scores to [0, 100]
data$score <- pmax(pmin(round(data$score), 100), 0)

# Calculate means and confidence intervals, to 2 dp
summary_data <- data %>%
  group_by(school) %>%
  summarise(
    mean_score = round(mean(score), 2),
    sd = round(sd(score), 2),
    ci_low = round(mean_score - (sd / sqrt(n)) * qt(0.975, n - 1), 2),
    ci_high = round(mean_score + (sd / sqrt(n)) * qt(0.975, n - 1), 2)
  )

# Create APA-style table using flextable
flex_table <- summary_data %>%
  flextable() %>%
  set_caption("Descriptive statistics for test scores by school") %>%
  set_header_labels(
    school = "School",
    mean_score = "Mean",
    sd = "SD",
    ci_low = "95% CI Low",
    ci_high = "95% CI High"
  ) %>%
  theme_booktabs()

# Print the table to view it
flex_table
