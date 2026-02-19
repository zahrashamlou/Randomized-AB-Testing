# Check missing
summary(events)
str(events)
summary(assignments)
str(assignments)


# Unique user counts
cat("Number of unique users:\n")
cat("  Events table:      ", length(unique(events$user_id)), "\n")
cat("  Assignments table: ", length(unique(assignments$user_id)), "\n\n")

# Users only in one table
cat("Number of users only in one table:\n")
cat("  Only in Events:      ", length(setdiff(unique(events$user_id), unique(assignments$user_id))), "\n")
cat("  Only in Assignments: ", length(setdiff(unique(assignments$user_id), unique(events$user_id))), "\n\n")

# Number of event rows that the user does not appear in assignment tab
cat("Event rows from non-experiment users: ", sum(!events$user_id %in% assignments$user_id), "\n\n")

# Inspect non-experiment events
events %>%
  filter(!user_id %in% assignments$user_id) %>%
  select(user_id, event, group, converted) %>%
  count(event)

# Inspect group column for users were not involved in the exp
table(events$group[!events$user_id %in% assignments$user_id])



## ------------------------------------------------------
# Balance Check - Compare the number of users in Control VS. Treatment group
# --- 1. All assigned users ---
cat("All assigned users (assignment table):\n")
assignments %>%
  group_by(group) %>%
  summarise(
    n_users = n_distinct(user_id)
  ) %>%
  mutate(
    share = n_users / sum(n_users) * 100
  ) %>%
  print()
cat("\n")

# --- 2. Assigned users who had at least one event ---
cat("Assigned users who had events (experiment participants):\n")
events %>%
  inner_join(assignments, by = "user_id") %>%
  group_by(group.y) %>%
  summarise(
    n_users = n_distinct(user_id)
  ) %>%
  mutate(
    share = n_users / sum(n_users) * 100
  ) %>%
  print()


## ------------------------------------------------------
# active is your data.frame / tibble
first_signup <- active %>%
  filter(event == "start_signup_flow") %>%
  mutate(time = as.POSIXct(time)) %>%          # safe if already POSIXct
  arrange(user_id, time) %>%
  group_by(user_id) %>%
  slice_head(n = 1) %>%                        # keep first start_signup_flow per user
  ungroup()

trend_df <- first_signup %>%
  mutate(month = floor_date(time, unit = "month")) %>%
  count(group, month, name = "n_users") %>%    # now each row is one user
  arrange(month, group)

ggplot(trend_df, aes(x = month, y = n_users, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_datetime(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(
    x = "Month",
    y = "Users (first start_signup_flow only)",
    color = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# # 1. Keep first start_signup_flow per user
# first_signup <- active %>%
#   filter(event == "start_signup_flow") %>%
#   mutate(time = as.POSIXct(time)) %>%
#   arrange(user_id, time) %>%
#   group_by(user_id) %>%
#   slice_head(n = 1) %>%
#   ungroup()
# 
# # 2. Monthly average conversion rate by group
# trend_df <- first_signup %>%
#   mutate(month = floor_date(time, unit = "month")) %>%
#   group_by(group, month) %>%
#   summarise(
#     avg_converted = mean(converted, na.rm = TRUE),
#     n_users = n(),                    # optional: for diagnostics
#     .groups = "drop"
#   ) %>%
#   arrange(month, group)
# 
# # 3. Plot
# ggplot(trend_df, aes(x = month, y = avg_converted, color = group, group = group)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   scale_x_datetime(date_labels = "%Y-%m", date_breaks = "1 month") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = "Month",
#     y = "Average conversion rate (first start_signup_flow)",
#     color = "Group"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
