
## ------------------------------------------------------

# User-level aggregation - Aggregate events to user level
user_metrics <- active %>%
  group_by(user_id, group) %>%
  summarise(
    finished_onboarding = any(event == "finished_onboarding_questionnaire"),
    paid_user = any(event == "paid"),
    add_temperature = any(event == "add_temperature"),
    week4 = any(add_temp_early)
  ) %>%
  ungroup()

# Conversion Metrics

# 1) Summarise (robust to NA)
conversion_summary <- user_metrics %>%
  group_by(group) %>%
  summarise(
    n_users = n(),
    onboarding_rate       = mean(as.numeric(finished_onboarding), na.rm = TRUE),
    paid_rate             = mean(as.numeric(paid_user),           na.rm = TRUE),
    week4 = mean(
      if_else(paid_user == TRUE, as.numeric(week4), NA_real_),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# 2) Long format (EXPLICITLY exclude n_users)
plot_data <- conversion_summary %>%
  pivot_longer(
    cols = c(onboarding_rate, paid_rate, week4),
    names_to = "metric",
    values_to = "rate"
  ) %>%
  mutate(
    group = factor(group, levels = c("control", "variant")),
    metric = factor(
      metric,
      levels = c("onboarding_rate", "paid_rate", "week4"),
      labels = c("Finished Onboarding", "Conversion Rate", "Week 4 Acitivity")
    )
  )

ggplot(plot_data, aes(x = metric, y = rate, fill = group)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = percent(rate, accuracy = 0.1)),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 5.5
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(plot_data$rate) * 1.15)
  ) +
  labs(
    title = "Conversion Metrics by Experimental Group",
    x = NULL,
    y = "Rate",
    fill = "Group"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold")
  )+ scale_fill_manual(
    values = c(
      "control" = "grey75",
      "variant" = "#2C7FB8"
    )
  )

ggsave(
  filename = "./results/A1_2_conversion_metrics_by_group.pdf",
  plot = last_plot(),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)


## ------------------------------------------------------
user_metrics$variant <- as.integer(user_metrics$group == "variant")

user_metrics_paid <- user_metrics %>% filter(paid_user==TRUE) 

library(modelsummary)
library(sandwich)

# -----------------------------
# 1) Setup
# -----------------------------

# -----------------------------
# 2) Run regressions explicitly
# -----------------------------
m_paid  <- lm(paid_user ~ variant, data = user_metrics)
m_week4 <- lm(week4     ~ variant, data = user_metrics_paid)

vc_paid  <- vcovHC(m_paid,  type = "HC1")
vc_week4 <- vcovHC(m_week4, type = "HC1")

# -----------------------------
# 3) Control means explicitly
# -----------------------------
cm_paid  <- mean(as.integer(user_metrics$paid_user)[user_metrics$variant == 0],  na.rm = TRUE)
cm_week4 <- mean(as.integer(user_metrics_paid$week4)[user_metrics_paid$variant == 0],      na.rm = TRUE)

# -----------------------------
# 4) Treatment effects + robust SEs explicitly
# -----------------------------
te_paid  <- unname(coef(m_paid)["variant"])
te_week4 <- unname(coef(m_week4)["variant"])

se_paid  <- sqrt(vc_paid["variant", "variant"])
se_week4 <- sqrt(vc_week4["variant", "variant"])

# -----------------------------
# 5) Percent change + CI explicitly
# -----------------------------
z <- 1.96

pct_paid      <- 100 * te_paid  / cm_paid
pct_week4     <- 100 * te_week4 / cm_week4

pct_low_paid  <- 100 * (te_paid  - z * se_paid)  / cm_paid
pct_high_paid <- 100 * (te_paid  + z * se_paid)  / cm_paid

pct_low_week4  <- 100 * (te_week4 - z * se_week4) / cm_week4
pct_high_week4 <- 100 * (te_week4 + z * se_week4) / cm_week4

pct_ci_paid  <- sprintf("[%.2f, %.2f]", pct_low_paid,  pct_high_paid)
pct_ci_week4 <- sprintf("[%.2f, %.2f]", pct_low_week4, pct_high_week4)

# -----------------------------
# 6) Add-rows table for modelsummary
# -----------------------------
add_rows <- data.frame(
  term = c("Control mean", "Percent change vs control", "95% CI (percent change)"),
  paid_user = c(sprintf("%.3f", cm_paid),  sprintf("%.2f", pct_paid),  pct_ci_paid),
  week4     = c(sprintf("%.3f", cm_week4), sprintf("%.2f", pct_week4), pct_ci_week4),
  check.names = FALSE
)

# -----------------------------
# 7) Export table
# -----------------------------
modelsummary(
  list(    "Conversion Rate" = m_paid,
           "Active in Week 4" = m_week4),
  vcov      = list(paid_user = vc_paid, week4 = vc_week4),
  coef_map  = c("variant" = "Treatment", "(Intercept)" = "Constant"),
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*" = .10, "**" = .05, "***" = .01),
  add_rows  = add_rows,
  gof_map   = c("nobs"),
  output    = "./results/A1_2_table.docx"
)

