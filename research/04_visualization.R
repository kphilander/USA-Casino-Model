# =============================================================================
# Distance Decay Estimation: Visualization
# =============================================================================
# This script creates figures for the research paper.
#
# Author: Kahlil Philander
# Date: February 2026
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)  # For combining plots

# Set theme
theme_set(theme_minimal(base_size = 12))

# Create figures directory
if (!dir.exists("figures")) dir.create("figures")

# =============================================================================
# 1. LOAD RESULTS
# =============================================================================

results <- readRDS("results/model_estimates.rds")
comparison <- readRDS("results/model_comparison.rds")
revenue_data <- bind_rows(
  read.csv("data/pa_revenue_2022.csv"),
  read.csv("data/oh_revenue_2022.csv")
) %>% filter(!is.na(total_revenue_2022))

# =============================================================================
# 2. FIGURE 1: DISTANCE DECAY CURVES
# =============================================================================

# Distance range for plotting
d <- seq(0.1, 150, by = 0.5)

# Calculate decay values for each model
decay_curves <- tibble(distance = d) %>%
  mutate(
    Exponential = if (!is.null(results$exponential))
      exp(-results$exponential$params["beta"] * distance) else NA,

    Power = if (!is.null(results$power))
      (distance + 0.1)^(-results$power$params["beta"]) else NA,

    Gaussian = if (!is.null(results$gaussian))
      exp(-results$gaussian$params["beta"] * distance^2) else NA,

    `Log-logistic` = if (!is.null(results$loglogistic))
      1 / (1 + (distance / results$loglogistic$params["alpha"])^results$loglogistic$params["beta"]) else NA,

    Combined = if (!is.null(results$combined))
      (distance + 0.1)^(-results$combined$params["alpha"]) *
      exp(-results$combined$params["beta"] * distance) else NA
  ) %>%
  pivot_longer(-distance, names_to = "Model", values_to = "decay_weight") %>%
  filter(!is.na(decay_weight))

# Normalize to 0-1 scale for comparability
decay_curves <- decay_curves %>%
  group_by(Model) %>%
  mutate(decay_normalized = decay_weight / max(decay_weight)) %>%
  ungroup()

# Plot
fig1 <- ggplot(decay_curves, aes(x = distance, y = decay_normalized, color = Model)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Figure 1: Estimated Distance Decay Functions",
    subtitle = "Normalized to equal weight at minimum distance",
    x = "Distance (miles)",
    y = "Relative Demand Weight",
    color = "Model"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 150, 25)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2))

ggsave("figures/fig1_decay_curves.png", fig1, width = 10, height = 6, dpi = 300)
ggsave("figures/fig1_decay_curves.pdf", fig1, width = 10, height = 6)

cat("Saved Figure 1: Distance decay curves\n")

# =============================================================================
# 3. FIGURE 2: PREDICTED VS ACTUAL REVENUE
# =============================================================================

# Get predictions from best model
best_model_name <- comparison$model[1]

# [This would need the full prediction function - simplified version here]
# For now, create placeholder

fig2_data <- revenue_data %>%
  mutate(
    revenue_millions = total_revenue_2022 / 1e6,
    state_label = ifelse(state == "PA", "Pennsylvania", "Ohio")
  )

fig2 <- ggplot(fig2_data, aes(x = reorder(name, -revenue_millions), y = revenue_millions, fill = state_label)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Pennsylvania" = "#1f77b4", "Ohio" = "#ff7f0e")) +
  labs(
    title = "Figure 2: Observed Casino Revenues (2022)",
    x = NULL,
    y = "Revenue ($ millions)",
    fill = NULL
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave("figures/fig2_observed_revenue.png", fig2, width = 10, height = 8, dpi = 300)
ggsave("figures/fig2_observed_revenue.pdf", fig2, width = 10, height = 8)

cat("Saved Figure 2: Observed revenue\n")

# =============================================================================
# 4. FIGURE 3: MODEL FIT COMPARISON
# =============================================================================

fig3 <- ggplot(comparison, aes(x = reorder(model, -sse), y = sse)) +
  geom_col(fill = "#2c7fb8") +
  geom_text(aes(label = round(r_squared, 3)), vjust = -0.5) +
  labs(
    title = "Figure 3: Model Fit Comparison",
    subtitle = "Sum of Squared Errors (lower is better); R² shown above bars",
    x = "Model",
    y = "SSE"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/fig3_model_comparison.png", fig3, width = 8, height = 6, dpi = 300)
ggsave("figures/fig3_model_comparison.pdf", fig3, width = 8, height = 6)

cat("Saved Figure 3: Model comparison\n")

# =============================================================================
# 5. FIGURE 4: DECAY AT KEY DISTANCES
# =============================================================================

# Show decay values at common distances
key_distances <- c(5, 10, 25, 50, 75, 100)

decay_at_distances <- decay_curves %>%
  filter(distance %in% key_distances) %>%
  mutate(distance_label = paste0(distance, " mi"))

fig4 <- ggplot(decay_at_distances,
               aes(x = factor(distance), y = decay_normalized, fill = Model)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Figure 4: Demand Weight at Key Distances",
    subtitle = "Relative to maximum (at ~0 miles)",
    x = "Distance (miles)",
    y = "Relative Demand Weight",
    fill = "Model"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave("figures/fig4_decay_at_distances.png", fig4, width = 10, height = 6, dpi = 300)
ggsave("figures/fig4_decay_at_distances.pdf", fig4, width = 10, height = 6)

cat("Saved Figure 4: Decay at key distances\n")

# =============================================================================
# 6. TABLE: PARAMETER ESTIMATES
# =============================================================================

# Create formatted parameter table
param_table <- tibble(
  Model = c("Exponential", "Power", "Gaussian", "Log-logistic", "Combined"),
  `Parameter 1` = c(
    sprintf("β = %.4f", results$exponential$params["beta"]),
    sprintf("β = %.4f", results$power$params["beta"]),
    sprintf("β = %.6f", results$gaussian$params["beta"]),
    sprintf("α = %.2f", results$loglogistic$params["alpha"]),
    sprintf("α = %.4f", results$combined$params["alpha"])
  ),
  `Parameter 2` = c(
    "—",
    "—",
    "—",
    sprintf("β = %.4f", results$loglogistic$params["beta"]),
    sprintf("β = %.4f", results$combined$params["beta"])
  ),
  SSE = round(comparison$sse, 6),
  `R²` = round(comparison$r_squared, 4),
  AIC = round(comparison$aic, 2),
  BIC = round(comparison$bic, 2)
)

write.csv(param_table, "figures/table_parameters.csv", row.names = FALSE)
cat("\nSaved parameter table\n")

# =============================================================================
# 7. SUMMARY
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("VISUALIZATION COMPLETE\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("\nFigures saved to research/figures/:\n")
cat("  - fig1_decay_curves.png/pdf\n")
cat("  - fig2_observed_revenue.png/pdf\n")
cat("  - fig3_model_comparison.png/pdf\n")
cat("  - fig4_decay_at_distances.png/pdf\n")
cat("  - table_parameters.csv\n")
