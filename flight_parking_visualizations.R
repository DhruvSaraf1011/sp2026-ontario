library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(scales)
library(ggrepel)

##############################################################################################################################


flight_parking <- read.csv("merged_flight_parking.csv")
flight_parking

flight_parking <- flight_parking %>%
  mutate(datetime = as.POSIXct(paste(date, hour), format = "%Y-%m-%d %H"))
flight_parking

##############################################################################################################################

#average revenue averaged by hour

df_hourly <- flight_parking %>%
  filter(!is.na(date), !is.na(hour)) %>%
  group_by(hour) %>%
  summarise(mean_revenue = mean(avg_revenue, na.rm = TRUE))

top2_max <- df_hourly %>%
  slice_max(order_by = mean_revenue, n = 2)

top2_min <- df_hourly %>%
  slice_min(order_by = mean_revenue, n = 2)

extremes <- bind_rows(top2_max, top2_min)

ggplot(df_hourly, aes(x = hour, y = mean_revenue)) +
  geom_label_repel(
    data = extremes,
    aes(label = paste0("Hour ", hour, "\n", dollar(mean_revenue))),
    size = 3,
    fill = "white",
    color = "firebrick" 
  ) +
  geom_line(color = "steelblue", linewidth = 1.25) +  
  scale_y_continuous(limits = c(35, 60),
                     labels = dollar_format()) + 
  labs(title = "Average Parking Revenue per Hour",
       subtitle = "Averaged across 2 years worth of data",
       x = "Hour of Day",
       y = "Average Revenue") +
  theme_minimal()

##############################################################################################################################

#same thing just with the average number of departure flights per hour on a dual axis

df_hourly <- flight_parking %>%
  filter(!is.na(date), !is.na(hour)) %>%
  group_by(hour) %>%
  summarise(
    mean_revenue = mean(avg_revenue, na.rm = TRUE),
    mean_departures = mean(actual_departures, na.rm = TRUE),
    .groups = "drop"
  )
df_hourly

scale_factor <- max(df_hourly$mean_revenue, na.rm = TRUE) /
  max(df_hourly$mean_departures, na.rm = TRUE)

ggplot(df_hourly, aes(x = hour)) +
  geom_line(aes(y = mean_revenue, color = "Average Revenue"),
            linewidth = 1) +
  geom_line(aes(y = mean_departures * scale_factor, color = "Average Departures"),
            linewidth = 1) +
  scale_color_manual(
    values = c(
      "Average Revenue" = "steelblue",
      "Average Departures" = "firebrick"),
    name = NULL) +
  scale_y_continuous(
    name = "Average Revenue",
    breaks = seq(0, 60, by = 10),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Average Departures",
      breaks = pretty(df_hourly$mean_departures, n = 10))) +
  labs(title = "Revenue vs Departures by Hour",
    x = "Hour of Day") +
  theme_minimal() +
  theme(legend.position = "top")


##############################################################################################################################

#same thing but with arrivals

df_hourly <- flight_parking %>%
  filter(!is.na(date), !is.na(hour)) %>%
  group_by(hour) %>%
  summarise(
    mean_revenue = mean(avg_revenue, na.rm = TRUE),
    mean_arrivals = mean(actual_arrivals, na.rm = TRUE),
    .groups = "drop"
  )
df_hourly

scale_factor <- max(df_hourly$mean_revenue, na.rm = TRUE) /
  max(df_hourly$mean_arrivals, na.rm = TRUE)

ggplot(df_hourly, aes(x = hour)) +
  geom_line(aes(y = mean_revenue, color = "Average Revenue"),
            linewidth = 1) +
  geom_line(aes(y = mean_arrivals * scale_factor, color = "Average Arrivals"),
            linewidth = 1) +
  scale_color_manual(
    values = c(
      "Average Revenue" = "steelblue",
      "Average Arrivals" = "firebrick"),
    name = NULL) +
  scale_y_continuous(
    name = "Average Revenue",
    breaks = seq(0, 60, by = 10),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Average Arrivals",
      breaks = pretty(df_hourly$mean_arrivals, n = 6))) +
  labs(title = "Revenue vs Arrivals by Hour",
       x = "Hour of Day") +
  theme_minimal() +
  theme(legend.position = "top")


##############################################################################################################################

#compares the number of departing flights per top 5 airlines and parking revenue per hour

carrier_df <- flight_parking %>%
  filter(!is.na(date), !is.na(hour)) %>%
  pivot_longer(
    cols      = starts_with("flights_"),
    names_to  = "carrier",
    values_to = "num_flights"
  ) %>%
  mutate(carrier = str_remove(carrier, "flights_")) %>%
  select(date, hour, avg_revenue, carrier, num_flights)
carrier_df

#top 5 carriers
top5_carriers <- carrier_df %>%
  group_by(carrier) %>%
  summarise(total_flights = sum(num_flights, na.rm = TRUE)) %>%
  slice_max(total_flights, n = 5) %>%
  pull(carrier)

#filter to top 5
carrier_df_top5 <- carrier_df %>%
  filter(carrier %in% top5_carriers)

#summarize
plot_data <- carrier_df_top5 %>%
  group_by(date, hour, carrier) %>%
  summarise(num_flights = sum(num_flights), .groups = "drop") %>%
  group_by(hour, carrier) %>%
  summarise(avg_flights = mean(num_flights), .groups = "drop")

plot_data <- plot_data %>%
  mutate(carrier = factor(carrier, levels = rev(top5_carriers)))


#plot_data <- carrier_df %>%
  #group_by(date, hour, carrier) %>%
  #summarise(num_flights = sum(num_flights), .groups = "drop") %>%
  #group_by(hour, carrier) %>%
  #summarise(avg_flights = mean(num_flights), .groups = "drop")

hourly_revenue <- carrier_df_top5 %>%
  group_by(date, hour) %>%
  summarise(avg_revenue = first(avg_revenue), .groups = "drop") %>%
  group_by(hour) %>%
  summarise(avg_revenue = mean(avg_revenue), .groups = "drop")


library(patchwork)

p1 <- ggplot(plot_data, aes(x = factor(hour, levels = 0:23))) +
  geom_col(aes(y = avg_flights, fill = carrier)) +
  labs(title = "Avg Departing Flights by Hour", 
       subtitle = "Top 5 Carriers", x = NULL, y = "Flights",
       fill = "Airline") +
  theme_minimal()

p2 <- ggplot(hourly_revenue, aes(x = factor(hour, levels = 0:23))) +
  geom_line(aes(y = avg_revenue, group = 1), color = "#E24B4A") +
  geom_point(aes(y = avg_revenue), color = "#E24B4A") +
  labs(title = "Avg Parking Revenue by Hour", 
       x = "Hour", y = "Revenue ($)") +
  theme_minimal()

p1 / p2

##############################################################################################################################

#average parking duration over the course of the day

df_duration <- flight_parking %>%
  filter(!is.na(date), !is.na(hour)) %>%
  group_by(hour) %>%
  summarise(mean_duration = mean(avg_duration, na.rm = TRUE) / 60)
df_duration

ggplot(df_duration, aes(x = hour, y = mean_duration)) +
  geom_line(color = "steelblue", linewidth = 1.25) +  
  labs(title = "Average Parking Duration per Hour",
       subtitle = "Averaged across 2 years worth of data",
       x = "Hour of Day Parking Started",
       y = "Average Duration (hours)") +
  theme_minimal()

##############################################################################################################################

#trying to visualize duration per carrier 

carrier_df <- flight_parking %>%
  filter(!is.na(date), !is.na(hour)) %>%
  pivot_longer(
    cols      = starts_with("flights_"),
    names_to  = "carrier",
    values_to = "num_flights"
  ) %>%
  mutate(carrier = str_remove(carrier, "flights_")) %>%
  select(date, hour, avg_duration, carrier, num_flights)
carrier_df





















