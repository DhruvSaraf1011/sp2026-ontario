# flight schedule jan 2024-jan 2026 cleaned data - script
#contains 3 visualizations: departing, arriving, net

install.packages("readxl")

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


#converts flight times to estimated parking usage times
parking_arrivals <- flight_schedule_data %>%
  mutate(
    parking_arrival_time = OD_STD - hours(2), #assume passangers park 2 hours before their flight
    parking_departure_time = OA_ATA + minutes(45) #assume passangers leave parking 45 minutes after landing
  ) %>%
  select(parking_arrival_time, parking_departure_time, 
         OD_PAX_TOTAL, OA_PAX_TOTAL)


parking_by_hour <- parking_arrivals %>%
  mutate(
    arrival_hour = hour(parking_arrival_time),
    departure_hour = hour(parking_departure_time)
  ) %>%
  group_by(arrival_hour) %>%
  summarise(
    departing_pax = sum(OD_PAX_TOTAL, na.rm = TRUE)
  )


ggplot(parking_by_hour, aes(arrival_hour, departing_pax)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Estimated Parking Demand Pressure by Time of Day",
    subtitle = "Based on departing passengers arriving ~2h before flight",
    x = "Hour of Day", #hour of the day on a 24 hour scale 
    y = "Estimated Passenger Volume" #total departing passengers arriving at parking
  ) +
  theme_minimal()


#answers the question: At what time of day do the most departing passengers likely arrive at airport parking?
#groups the entire two year dataset and adds up all departing passengers based on the arrival hour


##############################################################################################################################

#arriving passengers - when parking spaces free up

parking_arrivals_only <- flight_schedule_data %>%
  mutate(
    parking_departure_time = OA_ATA + minutes(30) # assume passengers leave 30 min after landing
  ) %>%
  select(parking_departure_time, OA_PAX_TOTAL)

parking_exits_by_hour <- parking_arrivals_only %>%
  mutate(
    departure_hour = hour(parking_departure_time)
  ) %>%
  group_by(departure_hour) %>%
  summarise(
    arriving_pax_leaving = sum(OA_PAX_TOTAL, na.rm = TRUE)
  )

ggplot(parking_exits_by_hour, aes(departure_hour, arriving_pax_leaving)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Estimated Parking Spaces Freed by Time of Day",
    subtitle = "Based on arriving passengers leaving ~30 min after landing",
    x = "Hour of Day",
    y = "Estimated Arriving Passenger Volume"
  ) +
  theme_minimal()


##############################################################################################################################


#net parking occupancy by hour - takes into account both departing and arriving passengers

parking_flow <- flight_schedule_data %>%
  mutate(
    parking_entry_time = OD_STD - hours(2),
    parking_exit_time  = OA_ATA + minutes(30)
  ) %>%
  select(parking_entry_time, parking_exit_time,
         OD_PAX_TOTAL, OA_PAX_TOTAL)

# Create hourly bins
parking_entries <- parking_flow %>%
  mutate(hour = hour(parking_entry_time)) %>%
  group_by(hour) %>%
  summarise(entries = sum(OD_PAX_TOTAL, na.rm = TRUE))

parking_exits <- parking_flow %>%
  mutate(hour = hour(parking_exit_time)) %>%
  group_by(hour) %>%
  summarise(exits = sum(OA_PAX_TOTAL, na.rm = TRUE))

# Combine
parking_net <- full_join(parking_entries, parking_exits, by = "hour") %>%
  replace_na(list(entries = 0, exits = 0)) %>%
  arrange(hour) %>%
  mutate(
    net_flow = entries - exits,
    cumulative_occupancy = cumsum(net_flow)
  )

ggplot(parking_net, aes(hour, cumulative_occupancy)) +
  geom_line(color = "firebrick", linewidth = 1.3) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Estimated Net Parking Occupancy by Hour of Day",
    subtitle = "Departing arrivals (+) minus arriving departures (-)",
    x = "Hour of Day",
    y = "Estimated Occupied Spaces"
  ) +
  theme_minimal()


#this assumes EVERYONE is parking, which is not the case
#graph purely shows the number of passengers departing/arriving at ONT


##############################################################################################################################

flight_schedule_data %>%
  mutate(
    parking_entry_time = OD_STD - hours(2),
    weekday = wday(parking_entry_time, label = TRUE)
  ) %>%
  group_by(weekday) %>%
  summarise(pax = sum(OD_PAX_TOTAL, na.rm = TRUE)) %>%
  ggplot(aes(weekday, pax)) +
  geom_col(fill = "steelblue")




