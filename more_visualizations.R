library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)

##############################################################################################################################

flight_schedule_data <- read_excel(
  "/Users/lindsayeisenman/Downloads/senior year semester 2/DS capstone/Flight Schedule Jan 2024-Jan 2026.xlsx"
)


flight_schedule_data %>%
  filter(!is.na(OD_STD), !is.na(OD_PAX_TOTAL)) %>% #removes rows where the scheduled dep time or passenger count is missing
  mutate(
    parking_entry = OD_STD - hours(2), #shifts the departure time back 2 hours (when they arrive @ parking lot)
    hour = hour(parking_entry),
    weekday = wday(parking_entry, label = TRUE)
  ) %>%
  group_by(weekday, hour) %>% #for every uniqure weekday and hour, adds up all departing passengers in the 2 yr dataset
  summarise(pax = sum(OD_PAX_TOTAL, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = hour, y = weekday, fill = pax)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "firebrick", labels = scales::comma) +
  labs(title = "Parking Demand Heatmap: Hour × Day of Week",
       subtitle = "Cumulative departing passengers across Jan 2024–Jan 2026 (2-year total)",
       x = "Hour of Day", y = NULL, fill = "Est. Passengers") +
  theme_minimal()


##############################################################################################################################


flight_schedule_data %>%
  filter(!is.na(OD_STD)) %>% #drops flights with no scheduled departure time
  mutate(month = floor_date(OD_STD - hours(2), "month")) %>% #shifts departing time back 2 hours
    #rounds the timestamp down to the first of the month
    #every flight gets assigned to a calendar month when the passengers would have parked, not when the flight departed
  group_by(month) %>%
  summarise(pax = sum(OD_PAX_TOTAL, na.rm = TRUE)) %>% #adds up all departing passengers in the 2yr dataset
  ggplot(aes(month, pax)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(title = "Monthly Parking Demand Trend (Jan 2024 – Jan 2026)",
       x = NULL, y = "Est. Departing Passengers (Aggregated)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#jan 2026 drop is likely just the dataset not being finished


##############################################################################################################################


parking_by_hour <- flight_schedule_data %>%
  filter(!is.na(OD_STD), !is.na(OD_PAX_TOTAL)) %>%
  mutate(
    parking_entry = OD_STD - hours(2), #estimated departing passengers arrive 2hours before takeoff
    arrival_hour  = hour(parking_entry)
  ) %>%
  group_by(arrival_hour) %>%
  summarise(
    departing_pax = sum(OD_PAX_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )


peak_threshold <- quantile(parking_by_hour$departing_pax, 0.75)
  #calculates the 75% percentile of passenger volumes across all 24 hours (this is the threshold)
  #anything above this is considered "peak" hour
parking_by_hour %>%
  mutate(is_peak = departing_pax >= peak_threshold) %>% #adds a true or false
  ggplot(aes(arrival_hour, departing_pax, fill = is_peak)) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "firebrick"),
                    labels = c("Normal", "Peak")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Peak Parking Pressure Windows",
       subtitle = "Red bars = top 25% of demand hours",
       x = "Hour of Day", y = "Est. Passenger Volume (Aggregated)", fill = NULL) +
  theme_minimal()




##############################################################################################################################


flight_schedule_data %>%
  filter(!is.na(OD_STD), !is.na(OD_ATD)) %>%
  mutate(delay_min = as.numeric(difftime(OD_ATD, OD_STD, units = "mins"))) %>% 
    #calculates the delay of each flight by subtracting scheduled from actual departure time in minutes
    #positive = late, negative = early
  filter(delay_min > -30, delay_min < 300) %>%  # remove extreme outliers
  ggplot(aes(x = delay_min)) +
  geom_histogram(binwidth = 15, fill = "steelblue", color = "white") + #each bin represents 15 minutes 
  geom_vline(xintercept = 0, color = "firebrick", linetype = "dashed") + #line between early and late
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Departure Delays",
       subtitle = "Delays extend how long cars stay in the lot",
       x = "Delay (minutes)", y = "Number of Flights") +
  theme_minimal()


flight_schedule_data %>%
  filter(!is.na(OA_STA), !is.na(OA_ATA)) %>%
  mutate(delay_min = as.numeric(difftime(OA_ATA, OA_STA, units = "mins"))) %>%
  #calculates the delay of each flight by subtracting scheduled from actual arrival time in minutes
  #positive = late, negative = early
  filter(delay_min > -30, delay_min < 300) %>%
  ggplot(aes(x = delay_min)) +
  geom_histogram(binwidth = 15, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, color = "firebrick", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Arrival Delays",
       subtitle = "Arrival delays extend how long pickup drivers occupy parking spaces",
       x = "Delay (minutes)", y = "Number of Flights") +
  theme_minimal()

##############################################################################################################################


library(ggrepel)

stress_labels <- parking_by_hour %>%
  filter(departing_pax >= quantile(departing_pax, 0.85))
  #creates a table containing just the top 15% of hours by passenger volume (labeled)
ggplot(parking_by_hour, aes(arrival_hour, departing_pax)) +
  geom_area(fill = "steelblue", alpha = 0.3) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_label_repel(data = stress_labels,
                   aes(label = paste0(arrival_hour, ":00\n", scales::comma(departing_pax), " pax")),
                   size = 3, fill = "white", color = "firebrick") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "High-Stress Parking Windows",
       subtitle = "Top 15% of demand hours — cumulative volume Jan 2024–2026",
       x = "Hour of Day", y = "Est. Passenger Volume") +
  theme_minimal()


##############################################################################################################################


library(broom)

# Build hourly feature table
reg_data <- flight_schedule_data %>%
  filter(!is.na(OD_STD)) %>%
  mutate(
    hour = hour(OD_STD - hours(2)), #shifts departure time back 2 hours to get estimated parking arrival
    weekday = wday(OD_STD, label = FALSE),
    is_weekend = weekday %in% c(1, 7)
  ) %>%
  group_by(hour, is_weekend) %>%
  summarise(
    total_pax = sum(OD_PAX_TOTAL, na.rm = TRUE),
    flight_count = n(),
    .groups = "drop"
  )

model <- lm(total_pax ~ hour + I(hour^2) + is_weekend + flight_count, data = reg_data)
  #can we predict total passenger volume from:
    #hour - linear time of day effect
    #curved/non-linear time of day
    #whether being a weekend changes demand
    #whether more flights = more parking pressure
summary(model)
tidy(model)  # clean output table

summary(model)$r.squared
