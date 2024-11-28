library(easyclimate)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)

# -- define coords
coords <- 
  read_excel("/Users/Valentin/Travail/Outils/Estimation/data/excel/meteo/Coord.xlsx")
coord.pt <- data.frame(lon = coords$xcoord_2[1], lat = coords$ycoord_2[1])
# test avec station météo d'arbois :
coord.pt <- data.frame(lon = 5.74, lat = 46.8958333)
# coordonnées du dispositif N°1 - Bois des Brosses
coord.pt <- data.frame(lat = 46.79674, lon = 3.44818)


# period
first_date = "1993-01-01"
# end_date = format(Sys.time(), "%Y-%m-%d")
end_date = "2022-12-31"
chosen_period = paste(first_date,":",end_date, sep = "")

##### activer si besoin (temps de téléchargement conséquent) #####
precipitation <- get_daily_climate(
  coords = coord.pt,
  period = chosen_period,
  climatic_var = "Prcp",
  output = "df",
  version = 4,
  check_connection = TRUE
)

temperature <- get_daily_climate(
  coords = coord.pt,
  period = chosen_period,
  climatic_var = "Tmax",
  output = "df",
  version = 4,
  check_connection = TRUE
)
#####

plot_table <- left_join(precipitation, temperature) %>% 
  mutate(
    year = as.numeric( str_sub(date, 1, 4) ),
    month_num = as.numeric( str_sub(date, 6, 7) ),
    month = month( as.Date(date, format = "%Y-%m-%d"), label = TRUE, abbr = FALSE )
  ) %>% 
  group_by(year) %>% 
  summarise(
    prec_mm = sum(Prcp),
    T_max = mean(Tmax)
  )

ggplot(data = plot_table, mapping = aes(x = year, y = prec_mm, group = 1)) + 
  geom_bar(stat = "identity", color="blue", fill="blue", width = 0.5) + 
  geom_line(mapping = aes(y = T_max * 100 / 3), color="red", size = 1.5) + # Scale data to match desired scale
  scale_y_continuous("Precipitation [mm]", 
                     sec.axis = sec_axis(~ .  * 3 / 100, name = "Temperature max [°C]") # Reverse transformation to match data
  ) + 
  # vertical line
  geom_vline(
    xintercept = seq(from = 1993, to = 2023, by = 5) - 0.5,
    colour = "black",
    linetype = 2,
    size = 0.5
  ) + 
  # scale_x_continuous(
  #   # limits = c(0, 400),
  #   expand = c(0, 0),
  #   breaks = seq(from = 1992, to = 2022),
  #   label = seq(from = 1993, to = 2022)
  # ) +
  # scale_x_date(limits = c(as.Date("1993-01-01", format = "%Y-%m-%d"), NA)) + 
  scale_x_continuous(breaks = seq(1993,2022)) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5))


# total des précipitations :
df <- precipitation %>% 
  mutate(
    year = as.numeric( str_sub(date, 1, 4) ),
    month_num = as.numeric( str_sub(date, 6, 7) ),
    month = month( as.Date(date, format = "%Y-%m-%d"), label = TRUE, abbr = FALSE )
  ) %>% 
  group_by(year) %>% 
  mutate(Prcp_by_year = sum(Prcp)) %>% 
  group_by(year, month_num, month) %>% 
  summarise(Prcp_by_month = sum(Prcp)) %>% 
  ungroup()

ggplot(df) +
  geom_line(mapping = aes(x = month, y = Prcp_by_month, group = year, color = year)) +
  # facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


df2 <- df %>% 
  group_by(year) %>% 
  summarise(
    Prcp_mean = mean(Prcp_by_month), 
    Prcp_min = min(Prcp_by_month),
    Prcp_max = max(Prcp_by_month),
    Prcp_sum = sum(Prcp_by_month)
  ) %>% 
  ungroup()

Prcp_total_mean <- mean(df2$Prcp_sum)

df3 <- df %>% 
  group_by(month) %>% 
  summarise(
    Prcp_mean = mean(Prcp_by_month), 
    Prcp_min = min(Prcp_by_month),
    Prcp_max = max(Prcp_by_month),
    Prcp_sum = sum(Prcp_by_month)
  )