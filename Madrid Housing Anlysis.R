# =========================================================
# PROJECT: INTRODUCTION TO R (Fasih Ahmad)
# DATASET: Madrid Housing Market
# STORIES: "Sunlight Premium" & "Diminishing Returns"
# =========================================================

# 1. Load Libraries
library(tidyverse)
library(ggplot2)
library(readr)
update.packages("ggplot2")

# Check ggplot2 version (should be 3.4.0 or higher)
cat("ggplot2 version:", as.character(packageVersion("ggplot2")), "\n")
if (packageVersion("ggplot2") < "3.4.0") {
  cat("WARNING: ggplot2 is outdated. Run: update.packages('ggplot2')\n")
}

# 2. Import Data
data <- read_csv("houses_Madrid.csv", show_col_types = FALSE)

# 3. Data Management
# Clean data - DON'T use drop_na() as it removes too much
clean_data <- data %>%
  filter(operation == "sale") %>%
  filter(!is.na(buy_price), !is.na(sq_mt_built), !is.na(is_orientation_south)) %>%
  filter(buy_price > 0, sq_mt_built > 0) %>%
  mutate(
    Orientation = if_else(is_orientation_south, "South Facing", "Other"),
    price_per_sqm = buy_price / sq_mt_built
  )

# Summary Statistics
cat("\n--- Data Summary ---\n")
cat("Total properties:", nrow(clean_data), "\n")
summary(clean_data %>% select(buy_price, sq_mt_built, n_rooms, Orientation))

cat("\n--- Average Prices by Orientation ---\n")
clean_data %>%
  group_by(Orientation) %>%
  summarise(
    count = n(),
    avg_price = mean(buy_price),
    median_price = median(buy_price)
  ) %>%
  print()

# ===========================================
# 4. Data Visualization (5 Graphs)
# ===========================================

# Graph 1: Distribution of House Prices
ggplot(clean_data, aes(x = buy_price)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  xlim(0, 2e6) +
  labs(
    title = "Distribution of House Prices in Madrid",
    x = "Price (€)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)


# Graph 2: The Sunlight Premium (Boxplot)
ggplot(clean_data, aes(x = Orientation, y = buy_price, fill = Orientation)) +
  geom_boxplot(alpha = 0.8) +
  ylim(0, 1.5e6) +
  scale_fill_manual(values = c("Other" = "grey70", "South Facing" = "gold2")) +
  labs(
    title = "The Sunlight Premium",
    subtitle = "South-facing apartments command higher prices",
    x = NULL,
    y = "Price (€)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


# Graph 3: Diminishing Returns (Size vs Price)
 ggplot(clean_data, aes(x = sq_mt_built, y = buy_price)) +
  geom_point(alpha = 0.3, color = "darkgreen", size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "red", se = TRUE) +
  xlim(0, 600) +
  ylim(0, 3e6) +
  labs(
    title = "Diminishing Returns with Size",
    subtitle = "Price increases with size but at a decreasing rate",
    x = "Size (square meters)",
    y = "Price (€)"
  ) +
  theme_minimal(base_size = 12)



# Graph 4: Average Price by Number of Rooms
p4 = clean_data %>%
  filter(!is.na(n_rooms), n_rooms > 0, n_rooms <= 6) %>%
  ggplot(aes(x = as.factor(n_rooms), y = buy_price)) +
  stat_summary(fun = "mean", geom = "bar", fill = "coral", alpha = 0.8) +
  labs(
    title = "Average Price by Number of Rooms",
    subtitle = "How room count affects property value",
    x = "Number of Rooms",
    y = "Average Price (€)"
  ) +
  theme_minimal(base_size = 12)

print(p4)

# Graph 5: Multilinear Regression Visualization
model_main = lm(buy_price ~ sq_mt_built + Orientation, data = clean_data)

clean_data_pred = clean_data %>%
  mutate(
    fitted = predict(model_main),
    resid = residuals(model_main)
  )

ggplot(clean_data_pred, aes(x = sq_mt_built, y = buy_price, color = Orientation)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_line(aes(y = fitted), size = 1.2) +
  scale_color_manual(values = c("Other" = "grey50", "South Facing" = "gold3")) +
  labs(
    title = "Price vs Size by Orientation",
    subtitle = "Linear regression controlling for orientation",
    x = "Size (square meters)",
    y = "Price (€)",
    color = "Orientation"
  ) +
  theme_minimal(base_size = 12)

# Bonus Graph: Residual Plot
 ggplot(clean_data_pred, aes(x = sq_mt_built, y = resid)) +
  geom_point(alpha = 0.4, color = "purple", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residual Plot",
    subtitle = "Checking model assumptions",
    x = "Size (square meters)",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 12)


# ===========================================
# 5. Data Modeling
# ===========================================

# Model 1: Simple Linear Regression (Size only)
cat("\n=== Model 1: Simple Linear Regression (Size Only) ===\n")
simple_model = lm(buy_price ~ sq_mt_built, data = clean_data)
summary(simple_model)

# Model 2: Multiple Linear Regression (Size + Orientation + Rooms)
# Filter out NA rooms for this model only
cat("\n=== Model 2: Multiple Linear Regression ===\n")
clean_data_rooms = clean_data %>% filter(!is.na(n_rooms))
multi_model = lm(buy_price ~ sq_mt_built + is_orientation_south + n_rooms, 
                  data = clean_data_rooms)
summary(multi_model)

# Model 3: The Sunlight Premium (Size + Orientation)
cat("\n=== Model 3: The Sunlight Premium ===\n")
sunlight_model = lm(buy_price ~ sq_mt_built + is_orientation_south, 
                     data = clean_data)
summary(sunlight_model)

# Model 4: Quadratic Model (Diminishing Returns)
cat("\n=== Model 4: Diminishing Returns (Quadratic) ===\n")
quadratic_model = lm(buy_price ~ sq_mt_built + I(sq_mt_built^2), 
                      data = clean_data)
summary(quadratic_model)

# Model 5: Interaction Effect (Size * Orientation)
cat("\n=== Model 5: Interaction Effect ===\n")
interaction_model = lm(buy_price ~ sq_mt_built * Orientation, 
                        data = clean_data)
summary(interaction_model)

# Model Comparison Table
comparison = data.frame(
  Model = c("Simple (Size)", "Multiple (Size+Orient+Rooms)", 
            "Sunlight Premium", "Quadratic", "Interaction"),
  R_squared = round(c(
    summary(simple_model)$r.squared,
    summary(multi_model)$r.squared,
    summary(sunlight_model)$r.squared,
    summary(quadratic_model)$r.squared,
    summary(interaction_model)$r.squared
  ), 4),
  Adj_R_squared = round(c(
    summary(simple_model)$adj.r.squared,
    summary(multi_model)$adj.r.squared,
    summary(sunlight_model)$adj.r.squared,
    summary(quadratic_model)$adj.r.squared,
    summary(interaction_model)$adj.r.squared
  ), 4)
)

print(comparison)

# Key Findings
cat("\n=== Key Findings ===\n")
best_model_idx = which.max(comparison$Adj_R_squared)
cat("1. Best Model (by Adj R²):", comparison$Model[best_model_idx], 
    "with Adj R² =", comparison$Adj_R_squared[best_model_idx], "\n")

price_diff = mean(clean_data$buy_price[clean_data$Orientation == "South Facing"]) -
  mean(clean_data$buy_price[clean_data$Orientation == "Other"])
cat("2. Sunlight Premium: South-facing apartments cost €", 
    format(round(price_diff, 0), big.mark = ","), " more on average\n", sep = "")

cat("3. Sample size:", nrow(clean_data), "properties\n")
cat("4. Price range: €", format(min(clean_data$buy_price), big.mark = ","), 
    " to €", format(max(clean_data$buy_price), big.mark = ","), "\n", sep = "")

