

# Load CSVs
events <- read.csv("./data/events-1-.csv", header = TRUE)
assignments <- read.csv("./data/assignments-1-.csv", header = TRUE)

## ------------------------------------------------------
# Data construction

events$time <- mdy_hm(events$timestamp, tz = "UTC")

# Variable construction
events <- events %>%
  mutate(time = as.POSIXct(time, tz = "UTC"))  # adjust tz/format if needed

days_addtemp_to_paid <- events %>%
  filter(event %in% c("add_temperature")) %>%
  group_by(user_id) %>%
  summarise(
    first_paid     = suppressWarnings(min(time[event == "add_temperature"],     na.rm = TRUE)),
    .groups = "drop"
  )

events <- events %>%
  left_join(days_addtemp_to_paid %>% select(user_id, first_paid), by = "user_id") %>% 
  mutate(days_between = as.numeric(difftime(time, first_paid, units = "days")))

events <- events %>%
  mutate(add_temp_early = if_else( days_between>=22 & days_between<28 , TRUE, FALSE, missing = FALSE))

# rm(days_addtemp_to_paid)


## ------------------------------------------------------
# Sample definition - Keep all the assigned users with at least one event as the sample
# Keep assigned users with at least one event as the sample
active <- events %>%
  inner_join(assignments %>% filter(!is.na(group)), by = "user_id")  # only assigned users with at least one event

# Remove columns and rename group.y
active <- active %>%  select(-group.x)  
active <- active %>%  rename(group = group.y)

# Count unique users per group
active %>%
  group_by(group) %>%
  summarise(n_users = n_distinct(user_id), .groups = "drop") %>%
  mutate(share = n_users / sum(n_users) * 100) %>%
  print()