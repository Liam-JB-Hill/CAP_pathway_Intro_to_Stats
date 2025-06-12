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


# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate random data
intrinsic_motivation <- runif(100, 0, 100)
error_term <- rnorm(100, mean = 0, sd = 5)
MVPA_days_winter <- 0.5 * intrinsic_motivation + error_term

# Create a data frame
data <- data.frame(intrinsic_motivation, MVPA_days_winter)

# Create scatter plot with regression line
ggplot(data, aes(x = intrinsic_motivation, y = MVPA_days_winter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Intrinsic Motivation (0-100)",
       y = "Days of MVPA during Winter (0-90)",
       title = "Scatter Plot with Regression Line") +
  theme_minimal()


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Sample A: Large effect size
n_A <- 30
mean_control_A <- 70
mean_intervention_A <- 74.3
sd_A <- 10
control_scores_A <- rnorm(n_A, mean_control_A, sd_A)
intervention_scores_A <- rnorm(n_A, mean_intervention_A, sd_A)

# Calculate effect size for Sample A
effect_size_A <- mean(intervention_scores_A) - mean(control_scores_A)

# Run t-test for Sample A
t_test_A <- t.test(intervention_scores_A, control_scores_A, var.equal = TRUE)

# Sample B: Small effect size
n_B <- 200
mean_control_B <- 70
mean_intervention_B <- 72
sd_B <- 10
control_scores_B <- rnorm(n_B, mean_control_B, sd_B)
intervention_scores_B <- rnorm(n_B, mean_intervention_B, sd_B)

# Calculate effect size for Sample B
effect_size_B <- mean(intervention_scores_B) - mean(control_scores_B)

# Run t-test for Sample B
t_test_B <- t.test(intervention_scores_B, control_scores_B, var.equal = TRUE)

# Create data frames for plotting
data_A <- data.frame(
  Group = rep(c("Control", "Intervention"), each = n_A),
  Score = c(control_scores_A, intervention_scores_A)
)

data_B <- data.frame(
  Group = rep(c("Control", "Intervention"), each = n_B),
  Score = c(control_scores_B, intervention_scores_B)
)

# Plot Sample A
ggplot(data_A, aes(x = Group, y = Score, fill = Group)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.5) +
  geom_errorbar(stat = "summary", fun.data = "mean_cl_normal", width = 0.2) +
  labs(title = "Sample A: Large Effect Size",
       subtitle = paste("n =", n_A, "| p-value:", round(t_test_A$p.value, 3), 
                        "| Effect size:", round(effect_size_A, 2)),
       y = "Test Score") +
  theme_minimal()+
  ylim(0, 90)

# Plot Sample B
ggplot(data_B, aes(x = Group, y = Score, fill = Group)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.5) +
  geom_errorbar(stat = "summary", fun.data = "mean_cl_normal", width = 0.2) +
  labs(title = "Sample B: Small Effect Size",
       subtitle = paste("n =", n_B, "| p-value:", round(t_test_B$p.value, 3), 
                        "| Effect size:", round(effect_size_B, 2)),
       y = "Test Score") +
  theme_minimal()+
  ylim(0, 90)



# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Parameters
population_mean_A <- 10
population_mean_B <- 15
n_samples <- 1000  # Number of samples

# Sampling distribution 1: Method A
method_A_samples <- rnorm(n_samples, mean = population_mean_A, sd = 5)

# Sampling distribution 2: Method B
method_B_samples <- rnorm(n_samples, mean = population_mean_B, sd = 10)

# Create data frames for plotting
data_A <- data.frame(Benefit = method_A_samples, Method = "Training Method A (SD = 2)")
data_B <- data.frame(Benefit = method_B_samples, Method = "Training Method B (SD = 10)")

# Combine data
data_combined <- rbind(data_A, data_B)

# Plot the distributions
ggplot(data_combined, aes(x = Benefit, fill = Method)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
  labs(title = "1000 samples of WM training benefits",
       x = "mean Working Memory Benefit (%)",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ Method)+
  theme(legend.position = "none")


# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Parameters
n_observations <- 1000
sample_mean <- 15
sample_sd <- 10

# Generate sample data
sample_data <- rnorm(n_observations, mean = sample_mean, sd = sample_sd)

# Create data frame for plotting
data_sample <- data.frame(Benefit = sample_data)

# Plot the sampling distribution
ggplot(data_sample, aes(x = Benefit)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = sample_mean, linetype = "dotted", size = 1, color = "red") +
  annotate("text", x = sample_mean, y = 60, label = "True Population Mean", color = "black", vjust = 1) +
  labs(title = "",
       x = "Working Memory Benefit (%)",
       y = "Frequency") +
  theme_minimal()

###

# Set seed for reproducibility
set.seed(42)

# Constants
n_A <- 30
mean_A <- 5
sd_A <- 2

n_B <- 30
mean_B <- 5
sd_B <- 5

# Generate data for Class A and adjust to exact mean
class_A <- rnorm(n_A, mean_A, sd_A)
class_A <- class_A - mean(class_A) + mean_A

# Generate data for Class B and adjust to exact mean
class_B <- rnorm(n_B, mean_B, sd_B)
class_B <- class_B - mean(class_B) + mean_B

# Create a combined data frame for easier plotting
data <- data.frame(
  Improvement = c(class_A, class_B),
  Class = factor(rep(c("A", "B"), each = 30))
)

# Load necessary library
library(ggplot2)

# Plot
ggplot(data, aes(x = Class, y = Improvement, color = Class)) +
  geom_jitter(width = 0.2, size = 2) + # Scatterplot with some jitter
  stat_summary(fun = mean, geom = "point", size = 4, shape = 18, color = "black") + # Mean points
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") + # CI
  labs(
    title = "Improvement in Concentration Time",
    x = "Class",
    y = "Improvement (minutes)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add outline
    legend.position = "none"
  )

##

# Set seed for reproducibility
set.seed(42)

# Constants for Classes
mean_val <- 5
sd_A <- 2
sd_B <- 5

# Generate data
class_A <- rnorm(30, mean_val, sd_A)
class_A <- class_A - mean(class_A) + mean_val

class_B <- rnorm(30, mean_val, sd_B)
class_B <- class_B - mean(class_B) + mean_val

class_C <- rnorm(5, mean_val, sd_A)
class_C <- class_C - mean(class_C) + mean_val

class_D <- rnorm(60, mean_val, sd_A)
class_D <- class_D - mean(class_D) + mean_val

# Combine data
data <- data.frame(
  Improvement = c(class_A, class_B, class_C, class_D),
  Class = factor(rep(c("A", "B", "C (n=5)", "D (n=60)"), times = c(30, 30, 5, 60))) # Corrected n for Class C
)

# Load necessary library
library(ggplot2)

# Plot
ggplot(data, aes(x = Class, y = Improvement, color = Class)) +
  geom_jitter(width = 0.2, size = 2) + # Scatterplot with some jitter
  stat_summary(fun = mean, geom = "point", size = 4, shape = 18, color = "black") + # Mean points
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") + # 95% CI for A, B, C, D
  labs(
    title = "Improvement in Concentration Time by Class",
    x = "Class",
    y = "Improvement (minutes)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add outline
    legend.position = "none"
  )

# Plot
ggplot(data, aes(x = Class, y = Improvement, color = Class)) +
  geom_jitter(width = 0.2, size = 2) + # Scatterplot with some jitter
  stat_summary(fun = mean, geom = "point", size = 4, shape = 18, color = "black") + # Mean points
  stat_summary(
    fun.data = mean_cl_normal, 
    geom = "errorbar", 
    width = 0.2, 
    color = "black",
    fun.args = list(conf.int = 0.99) # 99% CI for all classes
  ) +
  labs(
    title = "Improvement in Concentration Time by Class",
    x = "Class",
    y = "Improvement (minutes)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add outline
    legend.position = "none"
  )
