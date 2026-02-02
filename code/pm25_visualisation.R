# load required libraries
library(tidyverse)
library(lubridate)
library(GGally)
library(viridis)
library(tidyr)

# 1.Load PM2.5 data
pm25<-read.csv("UK_AIR_Sheffiled_PM2.5_data.csv",
               skip=10,
               stringsAsFactors = FALSE
)

# Check basic structure of the dataset
head(pm25)
names(pm25)
str(pm25)

# Clean PM2.5 values 
pm25_dg<-pm25$PM2.5.particulate.matter..Hourly.measured..1

# Convert PM2.5 column to numeric
pm25_dg<-as.numeric(pm25_dg)

# summary and missing values
summary(pm25_dg)
sum(is.na(pm25_dg))

# Create datetime variable
pm25$datetime <- as.POSIXct(
  paste(pm25$Date, pm25$Time),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "GMT"
)

# Create cleaned Pm2.5 dataset
pm25_clean <- data.frame(
  datetime = pm25$datetime,
  pm25 = pm25_dg
)

pm25_clean <- na.omit(pm25_clean)
summary(pm25_clean)

# 2.Load and clean weather data
weather<-read.csv("OpenMeteo_Weather_Variables.csv",
                  skip=3,
                  stringsAsFactors = FALSE
)
head(weather)
str(weather)
names(weather)

# Convert time column to POSIXct format
weather$time<-as.POSIXct(weather$time,
                         format="%Y-%m-%dT%H:%M",
                         tz="GMT"
)

# Select and rename relevant weather variables
weather_clean<-data.frame(datetime=weather$time,
                          temperature=weather$temperature_2m...C.,
                          humidity=weather$relative_humidity_2m....,
                          wind_speed=weather$wind_speed_10m..km.h.)

weather_clean<-na.omit(weather_clean)
summary(weather_clean)

# 3.Merge PM2.5 and weather data
final_data<-merge(pm25_clean,weather_clean,by="datetime")

str(final_data)
summary(final_data)
head(final_data)


# Create additional time-based variables
final_data <- final_data %>%
  mutate(
    hour = as.numeric(format(datetime, "%H")),
    month = as.numeric(format(datetime, "%m")),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    )
  ) %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))
         )

# 4. Figure 1: Temporal variation of PM2.5
fig1 <- ggplot(final_data, aes(x = datetime, y = pm25)) +
  geom_line(color = "steelblue", alpha = 0.4) +
  geom_smooth(method ="loess", span = 0.3, color = "black") +
  labs(title = "Figure 1: Temporal variation of PM2.5 concentration",
       x = "Date", 
       y = "PM2.5 (µg/m³)") +
  theme_minimal()

print(fig1)

# 5. Figure 2: Seasonal and diurnal heatmap
df_heatmap <- final_data %>%
  group_by(season,hour ) %>%
  summarise(mean_pm = mean(pm25, na.rm = TRUE), .groups = "drop")

fig2 <- ggplot(df_heatmap, aes(x = hour, y = season, fill = mean_pm)) +
  geom_tile() +
  scale_fill_viridis(option = "magma", name = "Avg PM2.5") +
  labs(title = "Figure 2: PM2.5 by Season and Hour",
       x = "Hour of Day", 
       y = "Season") +
  scale_x_continuous(breaks=seq(0,23,by=3))+
  theme_minimal()

print(fig2)

# 6. Figure 3: scatter of meteorological factors
df_long <- final_data %>%
  pivot_longer(cols = c(temperature, humidity, wind_speed), 
               names_to = "weather_var", values_to = "value")

fig3 <- ggplot(df_long, aes(x = value, y = pm25)) +
  geom_point(alpha = 0.05, color = "darkgrey") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  facet_wrap(~weather_var, scales = "free_x") +
  labs(title = "Figure 3: PM2.5 and key meteorological variables",
       x = "Value", 
       y = "PM2.5 (µg/m³)") +
  theme_minimal()

print(fig3)

# 7. Figure 4: Parallel coordinates for high pollution events
high_pollution <- final_data %>% filter(pm25 > 20) 

fig4 <- ggparcoord(high_pollution, 
                   columns = c(3,4,5), 
                   groupColumn = "season", 
                   scale = "uniminmax", 
                   alphaLines = 0.1) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Figure 4: Weather Profiles of High Pollution Events",
       x = "Factors", 
       y = "Normalized Scale") +
  theme_minimal()

print(fig4)

ggsave("fig1_time_series.png", fig1)
ggsave("fig2_heatmap.png", fig2)
ggsave("fig3_scatter.png", fig3)
ggsave("fig4_parallel_coordinates.png", fig4)
