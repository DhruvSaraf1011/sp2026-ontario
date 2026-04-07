library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(ggrepel)
library(scales)

##############################################################################################################################

flight_schedule_data <- read_excel(
  "/Users/lindsayeisenman/Downloads/senior year semester 2/DS capstone/Flight Schedule Jan 2024-Jan 2026.xlsx"
)

##############################################################################################################################

#showing the total number of flights over the past 2 years 
#broken out between arrivals, departures, and total


library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(scales)

# -------------------------------
# 1. ARRIVALS
# -------------------------------
arrivals <- flight_schedule_data %>%
  filter(!is.na(OA_STA)) %>%
  mutate(date = as.Date(OA_STA)) %>%
  filter(date >= as.Date("2024-01-01")) %>%
  group_by(date) %>%
  summarise(flights = n(), .groups = "drop")

arr_extreme <- arrivals %>%
  slice_max(flights, n = 2) %>%
  bind_rows(arrivals %>% slice_min(flights, n = 2))

p_arrivals <- ggplot(arrivals, aes(date, flights)) +
  geom_line(color = "steelblue") +
  geom_point(data = arr_extreme, color = "firebrick", size = 3) +
  geom_label_repel(
    data = arr_extreme,
    aes(label = paste0(format(date, "%b %d, %Y"), "\n", comma(flights))),
    size = 3,
    fill = "white",
    color = "firebrick"
  ) +
  labs(title = "Arrivals Per Day (Since 2024)",
       x = "Date", y = "Flights") +
  theme_minimal()

# -------------------------------
# 2. DEPARTURES
# -------------------------------
departures <- flight_schedule_data %>%
  filter(!is.na(OD_STD)) %>%
  mutate(date = as.Date(OD_STD)) %>%
  filter(date >= as.Date("2024-01-01")) %>%
  group_by(date) %>%
  summarise(flights = n(), .groups = "drop")

dep_extreme <- departures %>%
  slice_max(flights, n = 2) %>%
  bind_rows(departures %>% slice_min(flights, n = 2))

p_departures <- ggplot(departures, aes(date, flights)) +
  geom_line(color = "darkgreen") +
  geom_point(data = dep_extreme, color = "firebrick", size = 3) +
  geom_label_repel(
    data = dep_extreme,
    aes(label = paste0(format(date, "%b %d, %Y"), "\n", comma(flights))),
    size = 3,
    fill = "white",
    color = "firebrick"
  ) +
  labs(title = "Departures Per Day (Since 2024)",
       x = "Date", y = "Flights") +
  theme_minimal()

# -------------------------------
# 3. TOTAL
# -------------------------------
total <- full_join(arrivals, departures, by = "date", suffix = c("_arr", "_dep")) %>%
  replace_na(list(flights_arr = 0, flights_dep = 0)) %>%
  mutate(flights = flights_arr + flights_dep)

tot_extreme <- total %>%
  slice_max(flights, n = 2) %>%
  bind_rows(total %>% slice_min(flights, n = 2))

p_total <- ggplot(total, aes(date, flights)) +
  geom_line(color = "purple") +
  geom_point(data = tot_extreme, color = "firebrick", size = 3) +
  geom_label_repel(
    data = tot_extreme,
    aes(label = paste0(format(date, "%b %d, %Y"), "\n", comma(flights))),
    size = 3,
    fill = "white",
    color = "firebrick"
  ) +
  labs(title = "Total Flights Per Day (Since 2024)",
       x = "Date", y = "Flights") +
  theme_minimal()

# -------------------------------
# Print plots
# -------------------------------
p_arrivals
p_departures
p_total


##############################################################################################################################

#number of flights per airline

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# -------------------------------
# 1. Filter from 2024 onward
# -------------------------------
filtered_data <- flight_schedule_data %>%
  filter(flight_date >= as.Date("2024-01-01"))

# -------------------------------
# 2. Calculate daily arrivals
# -------------------------------
arrivals_daily <- filtered_data %>%
  filter(!is.na(OA_STA), !is.na(RAL_CODE_ARR)) %>%   # drop NA airlines
  group_by(flight_date, RAL_CODE_ARR) %>%
  summarise(flights = n(), .groups = "drop") %>%
  mutate(type = "Arrivals")

# -------------------------------
# 3. Calculate daily departures
# -------------------------------
departures_daily <- filtered_data %>%
  filter(!is.na(OD_STD), !is.na(RAL_CODE_ARR)) %>%  # drop NA airlines
  group_by(flight_date, RAL_CODE_ARR) %>%
  summarise(flights = n(), .groups = "drop") %>%
  mutate(type = "Departures")

# -------------------------------
# 4. Combine arrivals and departures
# -------------------------------
combined_daily <- bind_rows(arrivals_daily, departures_daily)

# -------------------------------
# 5. Compute average per day per airline
# -------------------------------
avg_daily <- combined_daily %>%
  group_by(RAL_CODE_ARR, type) %>%
  summarise(avg_flights = mean(flights), .groups = "drop")

# -------------------------------
# 6. Plot stacked bar chart
# -------------------------------
ggplot(avg_daily, aes(x = reorder(RAL_CODE_ARR, avg_flights), y = avg_flights, fill = type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Arrivals" = "steelblue", "Departures" = "darkgreen")) +
  labs(title = "Average Flights per Day by Airline (since 2024)",
       x = "Airline",
       y = "Average Flights per Day",
       fill = "Flight Type") +
  theme_minimal()


##############################################################################################################################

#compares the number of flights per weekday

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# 1. Filter dataset from 2024 onward
filtered_data <- flight_schedule_data %>%
  filter(flight_date >= as.Date("2024-01-01"))

# 2. Daily total arrivals
arrivals_daily <- filtered_data %>%
  filter(!is.na(OA_STA)) %>%                # make sure STA exists
  group_by(flight_date) %>%
  summarise(total_arrivals = n(), .groups = "drop") %>%
  mutate(type = "Arrivals")

# 3. Daily total departures
departures_daily <- filtered_data %>%
  filter(!is.na(OD_STD)) %>%                # make sure STD exists
  group_by(flight_date) %>%
  summarise(total_departures = n(), .groups = "drop") %>%
  mutate(type = "Departures")

# 4. Combine arrivals and departures in long format
combined_daily <- bind_rows(
  arrivals_daily %>% rename(flights = total_arrivals),
  departures_daily %>% rename(flights = total_departures)
)

# 5. Extract weekday
combined_daily <- combined_daily %>%
  mutate(weekday = wday(flight_date, label = TRUE, week_start = 1))  # Mon = 1

# 6. Average flights per weekday
avg_weekday <- combined_daily %>%
  group_by(weekday, type) %>%
  summarise(avg_flights = mean(flights), .groups = "drop")

# 7. Plot
ggplot(avg_weekday, aes(x = weekday, y = avg_flights, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Arrivals" = "steelblue", "Departures" = "darkgreen")) +
  labs(title = "Average Flights per Weekday (Since 2024)",
       x = "Day of the Week",
       y = "Average Number of Flights",
       fill = "Flight Type") +
  theme_minimal() +
  theme(text = element_text(size = 12))


##############################################################################################################################


