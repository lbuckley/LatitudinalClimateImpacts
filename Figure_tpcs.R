library(TrenchR)
library(GSODR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(TrenchR)
library(patchwork)
library(mgcv)
library(purrr)

#load TPCs
tpcs<- read.csv("data/DeutschTPC.csv")

#Kingsolver et al species: 
#a) Clavigralla shadabi, 6·45°N latitude. (b) Bemisia argentifolia, 25·47°N latitude. (c) Brevicoryne brassicae, 38·93°N latitude. (d) Muscidifurax raptorellus, 40·82°N latitude.

specs<- c("Clavigralla.shadabi", "Bemisia.argentifolia", "Brevicoryne.brassicae", "Muscidifurax.raptorellus")
inds<- match(specs, tpcs$Species)

#selected species
tpcs<- tpcs[inds,]
tpcs$IND<- 1:nrow(tpcs)
temps= 1:50

  tpcs$tpc_curve <- mapply(function(topt, ctmin, ctmax) {
    TPC(temps, topt, ctmin, ctmax)
  }, tpcs$Topt, tpcs$Ctmin, tpcs$CTmax, SIMPLIFY = FALSE)
  
  tpcs$temps <- rep(list(temps), nrow(tpcs))
  
  tpc_long <- tpcs %>%
    select(-Topt, -Ctmin, -CTmax) %>%  # drop if not needed downstream, or keep them
    unnest(cols = c(temps, tpc_curve))
  
  #plot
  ggplot(tpc_long, aes(x=temps, y=tpc_curve, colour=Species)) +geom_line()
  
  # for(i in 1:nrow(tpcs) ){
  #   stat<- nearest_stations(LAT = tpcs[i,"Lat"], LON = tpcs[i,"Long"], distance = 100)
  #   tpcs[i,"Station"]<- stat[1,STNID]
  # }
  
  tpcs$station<- c("653440-99999", "722026-12826", "724450-03945", "725510-14939")
  
  #extract climate data
  for(i in 1:4){
  temp.p <- get_GSOD(years = c(1982:1984), station = tpcs$station[i])
  temp.r <- get_GSOD(years = c(2022:2024), station = tpcs$station[i])
  
  ts<- rbind(temp.p[,c("CTRY", "YEAR", "YDAY", "TEMP","MIN","MAX")],
             temp.r[,c("CTRY", "YEAR", "YDAY", "TEMP","MIN","MAX")])
  ts$IND<- i
  
  #combine
  if(i==1) tdat<- ts
  if(i>1) tdat<- rbind(ts, tdat)
 
  } #end loop data
  
  tdat$period = ifelse(tdat$YEAR <= 2000, "1982-1984", "2022-2024")
  
  tdat$loc<- tdat[tdat$ind,"Lat"]
  
  tdat$MEAN= (tdat$MIN + tdat$MAX)/2
  
  #save temp data
  write.csv(tdat, "data/tempdat.csv")
  
  #restrict may to oct
  tdat.gs<- tdat[tdat$YDAY %in% c(121:304),]
  
  # plot distribution by period
  fig1<- ggplot(tdat.gs)+
    geom_density(alpha=0.5, aes(x=MEAN,y = after_stat(scaled), color=period, fill=period))+
    geom_line(data=tpc_long, aes(x=temps, y=tpc_curve, colour=Species))+
    facet_wrap(.~loc)
  #[tpc_long$Species=="Clavigralla.shadabi",]
  
  #==============================
  #mean across years
  tmean <- tdat %>%
    filter( YDAY>30 ) %>%
    group_by(CTRY, loc, period, YDAY) %>%
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
  #plot
  fig1a= ggplot(tmean_l, aes(x=YDAY, colour=loc)) + 
    geom_line(aes(y=temp, lty=period),alpha=0.5)+
    # smooth lines
    #geom_line(data = smoothed, aes(x = YDAY, y = tmean_smooth, lty=period), linewidth = 1) +
    geom_smooth(aes(y=tmean_mean, lty=period), method="loess", se=FALSE, show.legend = FALSE)+
    scale_color_manual(values=c("darkorange","cornflowerblue"))+
    theme_classic(base_size = 14)+
    ylab("Temperature (°C)")+
    xlab("Day of year")+
    ylim(-20,40)+
    theme(legend.position = c(0.55,0.2), legend.background = element_rect(fill = "transparent", color = NA),axis.label = element_text(size = 16))+
    labs(lty = "Period") +guides(color="none")+
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
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth=0.7, show.legend = FALSE)+
    xlim(30,333)
  
  #-------------------------
  #FIG 1b, TPCs
  
  #mean annual temperatures
  tann <- tmean %>%
    #growing season
    filter( YDAY %in% 150:250) %>%
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
  
  temp.tpc<- TPC(temps, 20, 7, 30)*0.8 #scale to preserve area
  trop.tpc<- TPC(temps, 28, 20, 32)
  
  tpcs<- as.data.frame(rbind( cbind(loc="temp", temp=temps, perf=temp.tpc), cbind(loc="trop", temp=temps, perf=trop.tpc) ))
  tpcs$temp= as.numeric(tpcs$temp)
  tpcs$perf= as.numeric(tpcs$perf)
  
  #adjust temperature height
  tann$y<- c(0.05, 0.1, 0.05, 0.1)
  tann$perf<- TPC(tann$tmean_mean, 20, 7, 30)*0.8
  tann$perf[which(tann$loc=="trop")]<- TPC(tann$tmean_mean[which(tann$loc=="trop")], 28, 20, 32)
  
  #plot
  fig1b= ggplot(tpcs, aes(x=temp, y= perf, color=loc)) + 
    geom_line(linewidth=1)+
    geom_point(
      data = tann,
      mapping = aes(x = tmean_mean, y = y, pch=period), size=3 )+  #+ inherit.aes = FALSE
    scale_color_manual(values=c("darkorange","cornflowerblue"))+
    theme_classic(base_size = 14)+
    ylab("Relative performance")+
    xlab("Body temperature (°C)")+
    geom_segment(data = tann, aes(x = tmean_mean-tmean_sd, y = y, xend = tmean_mean+tmean_sd, yend = y), linewidth=1)+
    #add vertical lines
    geom_segment(data = tann, aes(x = tmean_mean, y = y, xend = tmean_mean, yend = perf), linewidth=0.5, lty="dashed")+
    theme(legend.position = "none",axis.label = element_text(size = 16))+
    labs(pch = "Period", colour="Region") 
  
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
  #update labels
  tann$loc= ifelse(tann$loc=="temp", "temperate", "sub-tropical")
  
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
    scale_color_manual(values=c("cornflowerblue","darkorange"))+
    theme_classic(base_size = 14)+
    ylab("Realtive metabolic rate")+
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
  pdf("figures/Fig_1.pdf", height = 4, width = 10)
  fig1a +fig1b +fig1c + plot_annotation(tag_levels = 'A')
  dev.off()
  
  
  