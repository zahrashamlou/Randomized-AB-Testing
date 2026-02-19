


pattern <- active %>% filter(event== "add_temperature")

pattern <- pattern %>%
  mutate(week = floor(days_between / 7)+1)

end_2025 <- as.POSIXct("2025-12-31 23:59:59", tz = "UTC")
pattern <- pattern %>%
  mutate(
    weeks_left_2025 = floor(
      as.numeric(difftime(end_2025, first_paid, units = "days")) / 7
    )
  )


user_week_panel <- pattern %>%
  distinct(user_id, week, weeks_left_2025) %>%   # keep it
  mutate(has_obs = TRUE) %>%
  group_by(user_id) %>%
  complete(week = seq(1, max(weeks_left_2025, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(has_obs = replace_na(has_obs, FALSE)) %>% 
  left_join(assignments, by="user_id")


weekly_share <- user_week_panel %>%
  filter(week<=30, week>=1) %>% 
  group_by(group, week) %>%
  summarise(
    share_active = mean(has_obs),
    n_users = n(),
    .groups = "drop"
  ) %>% 
  mutate(share_active = share_active*100)

ggplot(weekly_share, aes(x = week, y = share_active, color = group)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(30, 100)) +
  labs(
    x = "Week since first payment",
    y = "Share of users active",
    color = "Group"
  ) +
  theme_light()

ggsave(
  filename = "./results/A1_3_dynamic_pattern.pdf",
  plot = last_plot(),
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)
