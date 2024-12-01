# Set working directory

setwd(".\\projects\\cyclistic_data")

install.packages("tidyverse")

library(tidyverse)

csv_list <- list.files("unclean_data", pattern ="*.csv", full.names = TRUE)

bike_trips_df <- map_dfr(csv_list, read_csv)

# Remove the list to free up environment space

rm(csv_list)

# Remove columns that are unnecessary for out analysis

bike_trips_df <- bike_trips_df %>% 
  select(-ride_id, -start_station_id, -end_station_id, -start_lat, -start_lng, -end_lat, -end_lng)

# here I use glimpse to quickly check if everything went smoothly

glimpse(bike_trips_df)

# Now to clean the data
# Here I pipped it so it will remove all duplicates and rows with NA values

bike_trips_df <- bike_trips_df %>% 
  distinct() %>% 
  drop_na()

# Calculate the ride length (mins & seconds), day of the week, month, day, and season

bike_trips_df <- bike_trips_df %>% 
  mutate(
    
    # Calculate the ride length in seconds and then split it into minutes and seconds
    
    ride_length = as.numeric(difftime(ended_at, started_at, units = "secs")),
    ride_length_minutes = floor(ride_length / 60),
    ride_length_seconds = ride_length %% 60,
    
    day_of_week = wday(started_at, label = TRUE, abbr = TRUE),
    
    month = month(started_at, label = TRUE, abbr = TRUE),
    
    day = day(started_at),
    
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      month %in% c("Mar", "Apr", "May") ~ "Spring",
      month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      month %in% c("Sep", "Oct", "Nov") ~ "Fall"
    )
  )

# Calculate total rides, average monthly ride length, average daily ride length, total daily ride count

monthly_ride_count <- bike_trips_df %>%
  group_by(month, member_casual) %>%
  summarise(total_rides = n())

monthly_ride_length <- bike_trips_df %>%
  group_by(month, member_casual) %>%
  summarise(avg_ride_length = mean(ride_length_minutes, na.rm = TRUE))

daily_ride_length <- bike_trips_df %>%
  group_by(day_of_week, member_casual) %>%
  summarise(avg_ride_length = mean(ride_length_minutes, na.rm = TRUE))

daily_ride_count <- bike_trips_df %>%
  group_by(day_of_week, member_casual) %>%
  summarise(total_rides = n())

# Load scales
library(scales)

# LINE CHART OF TOTAL RIDES BY MONTH & MEMBERSHIP TYPE
ggplot(monthly_ride_count, aes(x = month, y = total_rides, color = member_casual, group = member_casual)) +
  geom_line(linewidth = 1) +
  labs(title = "Total Rides by Month & Membership type",
       x = "Month",
       y = "Total Rides") +
  theme_minimal() +
  scale_y_continuous(labels = label_number(scale_cut = scales::cut_short_scale())) +
  theme(legend.title = element_blank())

# BAR CHART OF TOTAL RIDES PER DAY OF THE WEEK & MEMBER TYPE
ggplot(daily_ride_count, aes(x = day_of_week, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Rides by Day of the Week & Membership Type",
       x = "Day of the Week",
       y = "Total Rides") +
  scale_y_continuous(labels = label_number(scale_cut = scales::cut_short_scale())) +
  theme_minimal()

# LINE CHART OF AVERAGE RIDE LENGTH BY WEEK DAY & MEMBERSHIP TYPE 
ggplot(daily_ride_length, aes(x = day_of_week, y = avg_ride_length, color = member_casual, group = member_casual)) +
  geom_line(linewidth = 1) +
  labs(title = "Average Ride Length by Week Day & Membership Type",
       x = "Day of the Week",
       y = "Average Ride Length (Minutes)") +
  theme_minimal() +
  theme(legend.title = element_blank())

# LINE CHART OF AVERAGE RIDE LENGTH & MONTH & MEMBERSHIP TYPE 
ggplot(monthly_ride_length, aes(x = month, y = avg_ride_length, color = member_casual, group = member_casual)) +
  geom_line(linewidth = 1) +
  labs(title = "Average Ride Length by Month & Membership type",
       x = "Month",
       y = "Average Ride Length (Minutes)") +
  theme_minimal() +
  theme(legend.title = element_blank())

# BAR CHART OF TOTAL RIDES BY SEASON & MEMBERSHIP TYPE
ggplot(bike_trips_df, aes(x = season, fill = member_casual)) +
  geom_bar(position = "stack") +
  labs(title = "Ride Frequency by Season & Membership Type",
       x = "Season",
       y = "Count of Rides") +
  theme_minimal() +
  scale_y_continuous(labels = label_number(scale_cut = scales::cut_short_scale())) +
  theme(legend.title = element_blank())






