library(GSODR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(TrenchR)

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
    tmean_mean = mean((MIN+MAX)/2, na.rm = TRUE), #CHECK MEAN
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
  #geom_smooth(aes(y=temp), method="loess", se=FALSE)+
  geom_smooth(aes(y=tmean_mean), method="loess", se=FALSE)+
  scale_color_manual(values=c("darkorange","cornflowerblue"))+
  theme_classic()+
  ylab("Temperature (C)")+
  xlab("Day of year")

#mean annual temperatures
tann <- tmean %>%
  group_by(CTRY, period) %>%
  summarise(
    tmean_mean = mean(tmean_mean), 
    tmin_mean = mean(tmin_mean),
    tmax_mean = mean(tmax_mean),
    tmean_sd= sd(tmean_mean),
    .groups   = "drop"
  )

tann<- as.data.frame(tann)
tann$loc<- ifelse(tann$CTRY=="MX", "trop", "temp")

#-------------------------
#FIG 1b, TPCs

temps=1:40

temp.tpc<- TPC(temps, 18, 5, 28)*0.8 #scale to preserve area
trop.tpc<- TPC(temps, 28, 20, 32)

tpcs<- as.data.frame(rbind( cbind(loc="temp", temp=temps, perf=temp.tpc), cbind(loc="trop", temp=temps, perf=trop.tpc) ))
tpcs$temp= as.numeric(tpcs$temp)
tpcs$perf= as.numeric(tpcs$perf)

#plot
fig1b= ggplot(tpcs, aes(x=temp, y= perf, color=loc)) + 
  geom_line(linewidth=2)+
  geom_point(
    data = tann,
    mapping = aes(x = tmean_mean, y = 0, pch=period), size=3 )+  #+ inherit.aes = FALSE
  scale_color_manual(values=c("darkorange","cornflowerblue"))+
  theme_classic()+
  ylab("Performance")+
  xlab("Body temperature (C)")

#geom_segment
