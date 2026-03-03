library(GSODR)
library(dplyr)
library(tidyr)
library(ggplot2)

#FIG 1a, seasonal temperature variation
#find stations
nearest_stations(LAT = 20, LON = -100, distance = 1000)
#766250-99999       QUERETARO INTERCONTINENTAL
#use higher lat? 820980-99999 MACAPA / ALBERTO ALCOLUMBRE
nearest_stations(LAT = 40, LON = -100, distance = 100)
#724655-93990   HILL CITY MUNICIPAL ARPT
#lower lat? 711400-99999       BRANDON MUNI 

#retrieve data
trop.p <- get_GSOD(years = c(1980:1989), station = "766250-99999")
temp.p <- get_GSOD(years = c(1980:1989), station = "724655-93990")
trop.r <- get_GSOD(years = c(2015:2024), station = "766250-99999")
temp.r <- get_GSOD(years = c(2015:2024), station = "724655-93990")

#combine
tdat<- rbind(trop.p[,c("CTRY", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             temp.p[,c("CTRY", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             trop.r[,c("CTRY", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             temp.r[,c("CTRY", "YEAR", "YDAY", "TEMP","MIN","MAX")] )

#mean across years
tmean <- tdat %>%
  filter( YDAY>30 ) %>%
  mutate(period = ifelse(YEAR <= 2000, "1980_1984", "2020_2024")) %>%
  group_by(CTRY, period, YDAY) %>%
  summarise(
    tmean_mean = mean(TEMP, na.rm = TRUE),
    tmin_mean = mean(MIN, na.rm = TRUE),
    tmax_mean = mean(MAX, na.rm = TRUE),
    .groups   = "drop"
  )

#adjust doy to plot min and max
tmean_min<- tmean[,c("CTRY", "period",  "YDAY", "tmin_mean","tmean_mean")]
tmean_min$YDAY<- tmean_min$YDAY -0.5
names(tmean_min)[4]<- "temp"
tmean_max<- tmean[,c("CTRY", "period",  "YDAY", "tmax_mean","tmean_mean")]
names(tmean_max)[4]<- "temp"
tmean_l<- rbind(tmean_min, tmean_max)
tmean_l<- tmean_l[order(tmean_l$YDAY),]

#plot
fig1a= ggplot(tmean_l, aes(x=YDAY, colour=CTRY, lty=period)) + 
  geom_line(aes(y=temp),alpha=0.5)+
  geom_smooth(aes(y=tmean_mean), method="loess", se=FALSE)+
  scale_color_manual(values=c("darkorange","cornflowerblue"))+
  theme_classic()+
  ylab("Temperature (C)")+
  xlab("Day of year")

#time period issue?

#-------------------------
#FIG 1b, TPCs

# Schematic TPCs (as before)
tpc_schematic <- tibble::tribble(
  ~temp, ~perf, ~climate,
  0, 0.00, "Temperate",
  5, 0.20, "Temperate",
  10, 0.50, "Temperate",
  15, 0.80, "Temperate",
  20, 1.00, "Temperate",
  25, 0.80, "Temperate",
  30, 0.40, "Temperate",
  35, 0.10, "Temperate",
  15, 0.00, "Tropical",
  20, 0.30, "Tropical",
  25, 0.80, "Tropical",
  28, 1.00, "Tropical",
  31, 0.80, "Tropical",
  34, 0.30, "Tropical",
  37, 0.00, "Tropical"
)

# Schematic horizontal lines:
# y_past_mean, y_future_mean, and a y-range for body temperatures
horiz <- tibble::tribble(
  ~climate,    ~y_past, ~y_future, ~y_body_low, ~y_body_high,
  "Temperate",   0.25,     0.35,       0.45,        0.75,
  "Tropical",    0.25,     0.35,       0.55,        0.85
)

# Choose x positions for the horizontal lines (schematic only)
x_past_low   <- 8
x_past_high  <- 12
x_fut_low    <- 8
x_fut_high   <- 12
x_body_low   <- 18
x_body_high  <- 32

ggplot(tpc_schematic, aes(x = temp, y = perf, color = climate)) +
  geom_smooth(se = FALSE, span = 0.5, size = 1.1) +
  # Past mean horizontal line
  geom_segment(
    data = horiz,
    aes(x = x_past_low, xend = x_past_high,
        y = y_past, yend = y_past, color = climate),
    inherit.aes = FALSE, linetype = "dashed"
  ) +
  # Future mean horizontal line
  geom_segment(
    data = horiz,
    aes(x = x_fut_low, xend = x_fut_high,
        y = y_future, yend = y_future, color = climate),
    inherit.aes = FALSE, linetype = "solid"
  ) +
  # Body temperature range as a thick horizontal bar
  geom_segment(
    data = horiz,
    aes(x = x_body_low, xend = x_body_high,
        y = y_body_low, yend = y_body_low, color = climate),
    inherit.aes = FALSE, size = 3, alpha = 0.4
  ) +
  geom_segment(
    data = horiz,
    aes(x = x_body_low, xend = x_body_high,
        y = y_body_high, yend = y_body_high, color = climate),
    inherit.aes = FALSE, size = 3, alpha = 0.4
  ) +
  scale_color_manual(values = c("Temperate" = "steelblue",
                                "Tropical"  = "firebrick")) +
  scale_x_continuous("Temperature (°C)", limits = c(0, 40)) +
  scale_y_continuous("Relative performance", limits = c(0, 1.05)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")



