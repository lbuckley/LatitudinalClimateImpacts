library(GSODR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(TrenchR)
library(patchwork)
library(mgcv)
library(purrr)

#FIG 1a, seasonal temperature variation
#find stations
nearest_stations(LAT = 21.4, LON = -101.9, distance = 1500)
#763930-99999                                MONTERREY  N.L. ***USE 25.733 -100.300   515.0

#other options:
#768056-99999 GENERAL JUAN N ALVAREZ INTL / ACAPULCO INTL
#767270-99999                               ZACATEPEC MOR
#766750-99999                                      TOLUCA  MEX.
#765710-99999 JESUS TERAN INTL / AGUASCALIENTES INTL 21.705 -102.318  1862.9
#821110-99999     EDUARDO GOMES INTL  -3.039 -60.050    80.5 

nearest_stations(LAT = 53.5, LON = -112.1, distance = 100)
# 711210-99999           EDMONTON/NAMAO(MIL) ***USE 53.667 -113.467   688.0  

#other options:
#724655-93990   HILL CITY MUNICIPAL ARPT
#lower lat? 711400-99999       BRANDON MUNI 
#724695-23036       BUCKLEY AIR FORCE BASE
#719595-99999 TUKTOYAKTUK / JAMES GRUBEN 
#719570-99999          INUVIK MIKE ZUBKO

#retrieve data
trop.p <- get_GSOD(years = c(1982:1984), station = "821110-99999")
strop.p <- get_GSOD(years = c(1982:1984), station = "763930-99999")
temp.p <- get_GSOD(years = c(1982:1984), station = "711210-99999")

trop.r <- get_GSOD(years = c(2022:2024), station = "821110-99999")
strop.r <- get_GSOD(years = c(2022:2024), station = "763930-99999")
temp.r <- get_GSOD(years = c(2022:2024), station = "711210-99999")

#combine
tdat<- rbind(trop.p[,c("CTRY", "LATITUDE", "LONGITUDE", "ELEVATION", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             strop.p[,c("CTRY", "LATITUDE", "LONGITUDE", "ELEVATION", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             temp.p[,c("CTRY", "LATITUDE", "LONGITUDE", "ELEVATION", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             trop.r[,c("CTRY", "LATITUDE", "LONGITUDE", "ELEVATION", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             strop.r[,c("CTRY", "LATITUDE", "LONGITUDE", "ELEVATION", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             temp.r[,c("CTRY", "LATITUDE", "LONGITUDE", "ELEVATION", "YEAR", "YDAY", "TEMP","MIN","MAX")] )

lats<- c(-3.039, 25.733, 53.667)
locs<- c("tropical","subtropical","temperate")
loc_sh<- c("trop","strop","temp")

tdat$loc<- locs[match(tdat$LATITUDE, lats)]

#write out
#write.csv(tdat, "data/tempdat_3locs.csv")
tdat<- read.csv("data/tempdat_3locs.csv")
#change names
tdat$loc<- locs[match(tdat$loc, loc_sh)]

#mean across years
tmean <- tdat %>%
  filter( YDAY>30 ) %>%
  mutate(period = ifelse(YEAR <= 2000, "1982-1984", "2022-2024")) %>%
  group_by(loc, CTRY, period, YDAY) %>%
  summarise(
    tmean_mean = mean((MIN+MAX)/2, na.rm = TRUE), #CHECK MEAN
    tmin_mean = mean(MIN, na.rm = TRUE),
    tmax_mean = mean(MAX, na.rm = TRUE),
    .groups   = "drop"
  )

#adjust doy to plot min and max
tmean_min<- tmean[,c("CTRY", "loc", "period",  "YDAY", "tmin_mean","tmean_mean")]
tmean_min$YDAY<- tmean_min$YDAY -0.5
names(tmean_min)[5]<- "temp"
tmean_max<- tmean[,c("CTRY", "loc", "period",  "YDAY", "tmax_mean","tmean_mean")]
names(tmean_max)[5]<- "temp"
tmean_l<- rbind(tmean_min, tmean_max)
tmean_l<- tmean_l[order(tmean_l$YDAY),]

#----
#plot potential for phenological shift day 200

# Step 1: Fit a GAM per loc/period and predict over all YDAYs
yday_seq <- data.frame(YDAY = 1:365)

smoothed <- tmean %>%
  group_by(loc, CTRY, period) %>%
  group_map(~ {
    fit <- gam(tmean_mean ~ s(YDAY, k = 4), data = .x)
    yday_seq %>%
      mutate(
        tmean_smooth = predict(fit, newdata = yday_seq),
        loc    = .y$loc,
        CTRY   = .y$CTRY,
        period = .y$period
      )
  }) %>%
  bind_rows()

# Step 2: Extract smoothed temp at YDAY 200 in 1982-1984 per loc
target_temps <- smoothed %>%
  filter(YDAY == 200, period == "1982-1984") %>%
  select(loc, CTRY, target_temp = tmean_smooth)

# Step 3: Find YDAY in 2022-2024 closest to that smoothed temp
matched_days <- smoothed %>%
  filter(period == "2022-2024", YDAY < 200) %>%
  left_join(target_temps, by = c("loc", "CTRY")) %>%
  mutate(diff = abs(tmean_smooth - target_temp)) %>%
  group_by(loc, CTRY) %>%
  slice_min(diff, n = 1) %>%
  select(loc, CTRY, matched_YDAY = YDAY, matched_tmean = tmean_smooth)

# Step 4: Assemble final data frame
tmean_wide <- target_temps %>%
  left_join(
    smoothed %>%
      filter(YDAY == 200, period == "2022-2024") %>%
      select(loc, CTRY, tmean_smooth_2022_YDAY200 = tmean_smooth),
    by = c("loc", "CTRY")
  ) %>%
  left_join(matched_days, by = c("loc", "CTRY")) %>%
  rename(tmean_smooth_1982_YDAY200 = target_temp)

#----
tmean_l$loc<- factor(tmean_l$loc, ordered=T, levels=c("temperate", "subtropical", "tropical") )

#plot
fig1a= ggplot(tmean_l, aes(x=YDAY, colour=loc)) + 
  geom_line(aes(y=temp, lty=period),alpha=0.4)+
  # smooth lines
  #geom_line(data = smoothed, aes(x = YDAY, y = tmean_smooth, lty=period), linewidth = 1) +
  geom_smooth(aes(y=tmean_mean, lty=period), method="loess", se=FALSE, show.legend = FALSE)+
  scale_color_manual(values=c("#0868ac", "#43a2ca","#7bccc4"))+ 
  theme_classic(base_size = 14)+
  ylab("Temperature (°C)")+
  xlab("Day of year")+
  ylim(-20,40)+
  theme(legend.position = c(0.55,0.3), legend.background = element_rect(fill = "transparent", color = NA),axis.label = element_text(size = 16))+
  labs(lty = "Period", color="Region") +guides(color="none")+
  #vertical lines for shift at day 200
  geom_segment(data = tmean_wide, aes(x = 200, y = tmean_smooth_1982_YDAY200, xend = 200, yend = tmean_smooth_2022_YDAY200), linewidth=0.7)+
  # Horizontal arrow: 
  geom_segment(
    data = tmean_wide,
    aes(
      x    = 200,
      xend = matched_YDAY,
      y    = matched_tmean,
      yend = matched_tmean,
      color = loc
    ),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth=1.5, show.legend = FALSE)+
  xlim(33,333)

#-------------------------
#FIG 1b, TPCs

#mean annual temperatures
tann <- tmean %>%
  #growing season
  filter( YDAY %in% 121:304) %>% #150:250 OR 121:304
  group_by(CTRY, loc, period) %>%
  summarise(
    tmean_sd= sd(tmean_mean),
    tmean_mean = mean(tmean_mean), 
    tmin_mean = mean(tmin_mean),
    tmax_mean = mean(tmax_mean),
    .groups   = "drop"
  )

tann<- as.data.frame(tann)

temps=seq(1, 40, 0.25)

temp.tpc<- TPC(temps, 20, 6, 30)*0.6 #scale to preserve area
strop.tpc<- TPC(temps, 28, 13, 32)*0.8
trop.tpc<- TPC(temps, 28, 20, 32)

tpcs<- as.data.frame(rbind( cbind(loc="temperate", temp=temps, perf=temp.tpc), cbind(loc="subtropical", temp=temps, perf=strop.tpc), cbind(loc="tropical", temp=temps, perf=trop.tpc) ))
tpcs$temp= as.numeric(tpcs$temp)
tpcs$perf= as.numeric(tpcs$perf)

#adjust temperature height
tann$y<- c(0.05, 0.1, 0.05, 0.1, 0.05, 0.1)
tann$perf<- NA 
tann$perf[which(tann$loc=="temperate")]<- TPC(tann$tmean_mean[which(tann$loc=="temperate")], 20, 6, 30)*0.6
tann$perf[which(tann$loc=="subtropical")]<- TPC(tann$tmean_mean[which(tann$loc=="subtropical")], 28, 13, 32)*0.8
tann$perf[which(tann$loc=="tropical")]<- TPC(tann$tmean_mean[which(tann$loc=="tropical")], 28, 20, 32)

#make temp per group
tmean$group= paste(tmean$loc, tmean$period, sep="_")

tpcs$loc <- factor(tpcs$loc, ordered=T, levels=c("temperate", "subtropical", "tropical") )
tmean$loc <- factor(tmean$loc, ordered=T, levels=c("temperate", "subtropical", "tropical") )
tann$loc <- factor(tann$loc, ordered=T, levels=c("temperate", "subtropical", "tropical") )

#plot
fig1b= ggplot(data=tpcs, aes(color=loc)) + 
  #add temperature distributions
  #may through october
  geom_density(data=tmean[tmean$YDAY %in% c(121:304),], aes(x=tmean_mean, y = after_stat(scaled), fill=loc, lty=period), adjust=2, alpha=0.4 )+
  geom_line(data=tpcs, aes(x=temp, y= perf), color="black", linewidth=1, alpha=1)+
  geom_point(
    data = tann,
    mapping = aes(x = tmean_mean, y = y, pch=period), size=3 )+  #+ inherit.aes = FALSE
  scale_color_manual(values=c("#0868ac", "#43a2ca","#7bccc4"))+
  scale_fill_manual(values=c("#0868ac", "#43a2ca","#7bccc4"))+
  facet_wrap(.~loc)+
  theme_classic(base_size = 14)+
  ylab("Relative performance")+
  xlab("Body temperature (°C)")+
  geom_segment(data = tann, aes(x = tmean_mean-tmean_sd, y = y, xend = tmean_mean+tmean_sd, yend = y), linewidth=1)+
  #add vertical lines
  geom_segment(data = tann, aes(x = tmean_mean, y = y, xend = tmean_mean, yend = perf), linewidth=0.5, lty="dashed")+
  theme(legend.position = c(0.7, 0.7), axis.label = element_text(size = 16))+
  labs(pch = "Period", lty="Period", colour="Region", fill="Region") +
  guides(colour="none", fill="none")

#-------------------------
#FIG 1c, metabolism

# --- Parameters from Gillooly et al. 2001 ---
k  <- 8.617e-5   # Boltzmann's constant (eV/K)
E  <- 0.65       # Activation energy (eV); Gillooly range ~0.6–0.7 eV
B0 <- 1          # Normalization constant (arbitrary units; mass held constant)
m  <- 1          # Body mass (g); held constant to isolate temperature effect

temp_K <- temps + 273.15

# Metabolic rate: Boltzmann-Arrhenius factor (mass fixed at m=1 so m^0.75 = 1)
mr<- function(temp_K) B0 * m^(3/4) * exp(-E / (k * temp_K))

df <- data.frame(temp_C = temps, B = mr(temp_K) )

#metabolic rate corresponding to temps
tann$mr= mr(tann$tmean_mean + 273.15)

# Pivot to wide so each CTRY has both periods on one row
tann_wide <- tann %>%
  select(loc, period, tmean_mean, mr) %>% 
  pivot_wider(
    id_cols     = loc,
    names_from  = period,
    values_from = c(tmean_mean, mr)
  )
  
colnames(tann_wide)<- gsub("-","_", colnames(tann_wide) )

# --- Plot ---
fig1c<- ggplot(df, aes(x = temp_C, y = B)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values=c("#0868ac", "#43a2ca","#7bccc4"))+ 
  theme_classic(base_size = 14)+
  ylab("Relative metabolic rate")+
  xlab("Body temperature (°C)")+
  geom_point(
    data = tann,
    mapping = aes(x = tmean_mean, y = mr, pch=period, color=loc), size=3)+
  # Horizontal arrow: from tmean_mean 1980 to tmean_mean 2020, at height mr1_1980
  geom_segment(
    data = tann_wide,
    aes(
      x    = tmean_mean_1982_1984,
      xend = tmean_mean_2022_2024,
      y    = mr_1982_1984,
      yend = mr_1982_1984,
      color = loc
    ),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed") , show.legend = FALSE) +
  # Vertical line: from mr1_1980 down to mr1_2020, at x = tmean_mean_2020
  geom_segment(
    data = tann_wide,
    aes(
      x    = tmean_mean_2022_2024,
      xend = tmean_mean_2022_2024,
      y    = mr_1982_1984,
      yend = mr_2022_2024,
      color = loc
    ),
    linetype = "dashed"
  ) +
  theme(legend.position = c(0.3,0.7))+
  labs(pch = "Period", colour="Region", lty="none") +
  theme(axis.text.y = element_blank(),axis.label = element_text(size = 16))
  
#save figure
design <- "AACC
           BBBB"

pdf("figures/Fig_1.pdf", height = 10, width = 10)
fig1a +fig1b +fig1c + plot_annotation(tag_levels = 'A')+
  plot_layout(design=design)
dev.off()
