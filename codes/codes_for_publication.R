### codes to conduct analysis for "The diverging role of increasing wildfire 
### smoke to ambient PM2.5 exposure disparity in California (2006 to 2018)" project
### written and prepared by Chen Chen and Jenny Nguyen
library(data.table)
library(sf)
library(magick)
library(cowplot)
library(ggplot2)
library(RColorBrewer)

indir1 <- "" ## main directory, containing subfolders results and figure
indir2 <- "plot.data" ## directory for plotting data

hpi_vbs <- c("employed", "abovepoverty", "bachelorsed", "highschoool",
             "white_pct", "black_pct", "asian_pct", "latino_pct", "nativeAm_pct", "PacificIsl_pct",
             "income_median" 
)
pop_vbs <- c("pop25_64", "pop_total", "pop25_up", "pop15_17", 
             "pop_total", "pop_total", "pop_total", "pop_total", "pop_total", "pop_total",
             "pop_total"
)
names(pop_vbs) <- hpi_vbs

### data read in
#######
hpi_com <- fread(file.path(indir1, "data", "ct_acs5y_pm06to19_complete.csv")) ## complete data in wide format
pm.all <- fread(file.path(indir1, "data", "pm25_ct_2006to2019_by_year.csv")) ## exposure data in long format
pm.all.com <- pm.all[pm.all$geoid %in% hpi_com$CensusTract, ]
names(pm.all.com)[1] <- "GEOID"
hpi_com$CensusTract <- paste0("0", hpi_com$CensusTract)
#######

### Analysis
## Rank-rank correlations and Figure 2, Figure S4 and eTable 2
#######
rank.plot.process <- function(y1, y2, input, exposure) { ## plot with aggregation
  ## transformation of data to long format
  data <- merge(input[year==y1, c("year", "GEOID", exposure), with=FALSE], input[year==y2, c("GEOID", exposure), with=FALSE], by="GEOID")
  data <- as.data.frame(data)
  names(data)[3:4] <- paste0("pm_", c(y1, y2))
  ## aggregated for each percentile
  data$rp <- round(100*rank(data[, paste0("pm_", y1)])/nrow(data), digits = 0)
  data$rp_new <- 100*rank(data[, paste0("pm_", y2)])/nrow(data)
  data$rp_new_avg <- sapply(1:nrow(data), function(a) {
    mean(data[data$rp==data$rp[a], "rp_new"])
  })
  return(data)
}


## Figure 2
data <- rank.plot.process(2006, 2018, pm.all.com, "pm25_avg")
p1 <- ggplot(data, aes(x=rp, y = rp_new_avg)) +
  geom_point(size= 1) +
  xlim(0, 100) +
  ylim(0, 100) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.7, col="red") + 
  labs(title=paste("Total PM", "correlation is",
                   round(cor(data[, paste0("pm_", 2006)], data[, paste0("pm_", 2018)], 
                             method = "spearman")*100, digits = 1), "%"), 
       x=paste("Rank percentile in", 2006),
       y=paste("Mean rank percentile in", 2018)) +
  theme_bw() + 
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5))

data <- rank.plot.process(2006, 2018, pm.all.com, "pm25wf_avg")
p2 <- ggplot(data, aes(x=rp, y = rp_new_avg)) +
  geom_point(size= 1) +
  xlim(0, 100) +
  ylim(0, 100) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.7, col="red") + 
  labs(title=paste("WF PM", "correlation is",
                   round(cor(data[, paste0("pm_", 2006)], data[, paste0("pm_", 2018)], 
                             method = "spearman")*100, digits = 1), "%"), 
       x=paste("Rank percentile in", 2006),
       y=paste("Mean rank percentile in", 2018)) +
  theme_bw() + 
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5))

data <- rank.plot.process(2006, 2018, pm.all.com, "pm25_nowf")
p3 <- ggplot(data, aes(x=rp, y = rp_new_avg)) +
  geom_point(size= 1) +
  xlim(0, 100) +
  ylim(0, 100) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.7, col="red") + 
  labs(title=paste("NWF PM", "correlation is",
                   round(cor(data[, paste0("pm_", 2006)], data[, paste0("pm_", 2018)], 
                             method = "spearman")*100, digits = 1), "%"), 
       x=paste("Rank percentile in", 2006),
       y=paste("Mean rank percentile in", 2018)) +
  theme_bw() + 
  theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5))

tiff(file.path(indir1, "figures", paste0("figure2_rank_rank_average_06to18.tiff")),
     width=6,height=4,units="in", res = 500, bg="transparent",
     pointsize=9, family="sans", compression = "zip")
# png(file.path(indir1, "figures", paste0("figure2_rank_rank_average_06to18.png")),
#     width=6,height=4,units="in", res = 600, bg="transparent",
#     pointsize=9, family="sans")
plot_grid(p1, p3, p2, labels = "AUTO", nrow=2)
dev.off()



## Figure S4
for (yr in 2007:2017) {
  data <- rank.plot.process(2006, yr, pm.all.com, "pm25_avg")
  p1 <- ggplot(data, aes(x=rp, y = rp_new_avg)) +
    geom_point(size= 1) +
    xlim(0, 100) +
    ylim(0, 100) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.7, col="red") + 
    labs(title=paste("Total PM", "correlation is",
                     round(cor(data[, paste0("pm_", 2006)], data[, paste0("pm_", yr)], 
                               method = "spearman")*100, digits = 1), "%"), 
         x=paste("Rank percentile in", 2006),
         y=paste("Mean rank percentile in", yr)) +
    theme_bw() + 
    theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5))
  
  data <- rank.plot.process(2006, yr, pm.all.com, "pm25wf_avg")
  p2 <- ggplot(data, aes(x=rp, y = rp_new_avg)) +
    geom_point(size= 1) +
    xlim(0, 100) +
    ylim(0, 100) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.7, col="red") + 
    labs(title=paste("WF PM", "correlation is",
                     round(cor(data[, paste0("pm_", 2006)], data[, paste0("pm_", yr)], 
                               method = "spearman")*100, digits = 1), "%"), 
         x=paste("Rank percentile in", 2006),
         y=paste("Mean rank percentile in", yr)) +
    theme_bw() + 
    theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5))
  
  data <- rank.plot.process(2006, yr, pm.all.com, "pm25_nowf")
  p3 <- ggplot(data, aes(x=rp, y = rp_new_avg)) +
    geom_point(size= 1) +
    xlim(0, 100) +
    ylim(0, 100) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.7, col="red") + 
    labs(title=paste("NWF PM", "correlation is",
                     round(cor(data[, paste0("pm_", 2006)], data[, paste0("pm_", yr)], 
                               method = "spearman")*100, digits = 1), "%"), 
         x=paste("Rank percentile in", 2006),
         y=paste("Mean rank percentile in", yr)) +
    theme_bw() + 
    theme(text = element_text(size=8), plot.title = element_text(hjust = 0.5))
  
  tiff(file.path(indir1, "figures", paste0("FigureS4_2006_", yr, "_rank_rank.tiff")),
       width=6,height=4,units="in", res = 500, bg="transparent",
       pointsize=9, family="sans", compression = "zip")
  print(plot_grid(p1, p3, p2, labels = "AUTO", nrow=2))
  dev.off()
}



## eTable 2
rank.id <- function(y1, input, id, exposure, percentile, direction) { ## plot with aggregation
  ## transformation of data to long format
  data <- input[year==y1, c("year", id, exposure), with=FALSE]
  data <- as.data.frame(data)
  ## aggregated for each percentile
  data$rp <- 100*rank(data[, exposure])/nrow(data)
  if (direction=="lower") {
    return(data[data$rp<=percentile, id])
  } else if (direction=="higher") {
    return(data[data$rp>=percentile, id])
  }
}
foo1 <- data.frame(sesyears=rep(c("2006", "2018"), each=3), exyears=rep(c("06to08", "16to18"), each=3),
                   exposure=rep(c("pm25_avg", "pm25_nowf","pm25wf_avg"), times=2))
foo2 <- data.frame(sesyears=rep(c("2006", "2018"), each=3), exyears=rep(c("2006", "2018"), each=3),
                   exposure=rep(c("pm25_avg", "pm25_nowf","pm25wf_avg"), times=2))
foo <- rbind(foo1, foo2)
bar <- cbind(foo[rep(seq_len(nrow(foo)), each=2), ], 
             threshold=c(10, 90), relative=c("lower", "higher"))

for (i in 1:nrow(bar)) {
  ids <- rank.id(bar$exyears[i], pm.all.com, "GEOID", bar$exposure[i], 
                 bar$threshold[i], bar$relative[i])
  ids <- paste0("0", ids)
  bar[i, paste0(hpi_vbs, "_mean")] <- sapply(paste(hpi_vbs, bar$sesyears[i], sep="."), 
                                             function(a) {
                                               round(mean(hpi_com[hpi_com$CensusTract %in% ids, ][[a]], na.rm=TRUE), digits=2)
                                             })
}
cat("Distribution of indicators for communities least and most exposed", "\n")
print(bar)
#######

## Population weighted averages
#######
## Annual total $PM_{2.5}$ concentrations
indicators <- hpi_vbs
pop <- pop_vbs
print(pop)
years <- c("06to18", "06to17", "06to08", "16to18", 2006:2018)

nms <- c("avg", "popw_avg", paste(rep(indicators, each=4), c("yes", "no", "dif", "rel_dif"), sep="_"))
wt_summary <- data.frame(year=years,
                         setNames(replicate(length(nms), NA, simplify = F),
                                  nm=nms))
for (i in 1:nrow(wt_summary)) {
  exp_ <- paste0("pm25_avg.", years[i])
  if (years[i]=="06to08") {
    pop.yr <- 2006
  } else if (years[i]=="16to18") {
    pop.yr <- 2018
  } else if (years[i] %in% c("06to18", "06to17")) {
    pop.yr <- 2011
  } else {
    pop.yr <- years[i]
  }
  wt_summary[i, "avg"] <- mean(hpi_com[[exp_]])
  wt_summary[i, "popw_avg"] <- sum(hpi_com[[exp_]] * hpi_com[[paste0("pop_total.", pop.yr)]])/sum(hpi_com[[paste0("pop_total.", pop.yr)]])
  for (ind_ in indicators) {
    pop_use <- paste(pop[ind_], pop.yr, sep=".")
    wt_summary[i, paste0(ind_, "_yes")] <- sum(hpi_com[[exp_]] * (hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])/sum((hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])
    wt_summary[i, paste0(ind_, "_no")] <- sum(hpi_com[[exp_]]* (1 - hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])/sum((1 - hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])
    if (ind_ %in% c("white_pct", "black_pct", "asian_pct", "latino_pct", "nativeAm_pct", "PacificIsl_pct")) {
      wt_summary[i, paste0(ind_, "_dif")] <- wt_summary[i, paste0(ind_, "_yes")] - wt_summary[i, paste0(ind_, "_no")] 
    } else {
      wt_summary[i, paste0(ind_, "_dif")] <- wt_summary[i, paste0(ind_, "_no")] - wt_summary[i, paste0(ind_, "_yes")] 
    }
  }
}
wt_summary <- rbind(wt_summary, cbind(year="18-06", wt_summary[wt_summary$year==2018, -1] - wt_summary[wt_summary$year==2006, -1] ))
wt_summary <- rbind(wt_summary, cbind(year="17-06", wt_summary[wt_summary$year==2017, -1] - wt_summary[wt_summary$year==2006, -1] ))
wt_summary <- rbind(wt_summary, cbind(year="18-06_3y", wt_summary[wt_summary$year=="16to18", -1] - wt_summary[wt_summary$year=="06to08", -1] ))
for (i in 1:nrow(wt_summary)) {
  for (ind_ in indicators) {
    pop_use <- pop[ind_]
    wt_summary[i, paste0(ind_, "_rel_dif")] <- 100 * wt_summary[i, paste0(ind_, "_dif")] / wt_summary[i, paste0(ind_, "_no")] 
  }
}
wt_summary_2 <- round(wt_summary[, 2:ncol(wt_summary)], digits=2)
print(cbind(wt_summary[, 1], wt_summary_2))
write.csv(wt_summary, file.path(indir1, "results", "ct_indicator_wtavg.csv"), row.names = FALSE)

## Annual wildfire specific $PM_{2.5}$ concentrations
nms <- c("avg", "popw_avg", paste(rep(indicators, each=4), c("yes", "no", "dif", "rel_dif"), sep="_"))
wt_summary <- data.frame(year=years,
                         setNames(replicate(length(nms), NA, simplify = F),
                                  nm=nms))
for (i in 1:nrow(wt_summary)) {
  exp_ <- paste0("pm25wf_avg.", years[i])
  if (years[i]=="06to08") {
    pop.yr <- 2006
  } else if (years[i]=="16to18") {
    pop.yr <- 2018
  } else if (years[i] %in% c("06to18", "06to17")) {
    pop.yr <- 2011
  } else {
    pop.yr <- years[i]
  }
  wt_summary[i, "avg"] <- mean(hpi_com[[exp_]])
  wt_summary[i, "popw_avg"] <- sum(hpi_com[[exp_]] * hpi_com[[paste0("pop_total.", pop.yr)]])/sum(hpi_com[[paste0("pop_total.", pop.yr)]])
  for (ind_ in indicators) {
    pop_use <- paste(pop[ind_], pop.yr, sep=".")
    wt_summary[i, paste0(ind_, "_yes")] <- sum(hpi_com[[exp_]] * (hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])/sum((hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])
    wt_summary[i, paste0(ind_, "_no")] <- sum(hpi_com[[exp_]]* (1 - hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])/sum((1 - hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])
    if (ind_ %in% c("white_pct", "black_pct", "asian_pct", "latino_pct", "nativeAm_pct", "PacificIsl_pct")) {
      wt_summary[i, paste0(ind_, "_dif")] <- wt_summary[i, paste0(ind_, "_yes")] - wt_summary[i, paste0(ind_, "_no")] 
    } else {
      wt_summary[i, paste0(ind_, "_dif")] <- wt_summary[i, paste0(ind_, "_no")] - wt_summary[i, paste0(ind_, "_yes")] 
    }
  }
}
wt_summary <- rbind(wt_summary, cbind(year="18-06", wt_summary[wt_summary$year==2018, -1] - wt_summary[wt_summary$year==2006, -1] ))
wt_summary <- rbind(wt_summary, cbind(year="17-06", wt_summary[wt_summary$year==2017, -1] - wt_summary[wt_summary$year==2006, -1] ))
wt_summary <- rbind(wt_summary, cbind(year="18-06_3y", wt_summary[wt_summary$year=="16to18", -1] - wt_summary[wt_summary$year=="06to08", -1] ))
for (i in 1:nrow(wt_summary)) {
  for (ind_ in indicators) {
    pop_use <- pop[ind_]
    wt_summary[i, paste0(ind_, "_rel_dif")] <- 100 * wt_summary[i, paste0(ind_, "_dif")] / wt_summary[i, paste0(ind_, "_no")] 
  }
}

wt_summary_2 <- round(wt_summary[, 2:ncol(wt_summary)], digits=2)
print(cbind(wt_summary[, 1], wt_summary_2))
write.csv(wt_summary, file.path(indir1, "results", "ct_indicator_wildfire_wtavg.csv"), row.names = FALSE)

## Annual non-wildfire specific $PM_{2.5}$ concentrations
nms <- c("avg", "popw_avg", paste(rep(indicators, each=4), c("yes", "no", "dif", "rel_dif"), sep="_"))
wt_summary <- data.frame(year=years,
                         setNames(replicate(length(nms), NA, simplify = F),
                                  nm=nms))
for (i in 1:nrow(wt_summary)) {
  exp_ <- paste0("pm25_nowf.", years[i])
  if (years[i]=="06to08") {
    pop.yr <- 2006
  } else if (years[i]=="16to18") {
    pop.yr <- 2018
  } else if (years[i] %in% c("06to18", "06to17")) {
    pop.yr <- 2011
  } else {
    pop.yr <- years[i]
  }
  wt_summary[i, "avg"] <- mean(hpi_com[[exp_]])
  wt_summary[i, "popw_avg"] <- sum(hpi_com[[exp_]] * hpi_com[[paste0("pop_total.", pop.yr)]])/sum(hpi_com[[paste0("pop_total.", pop.yr)]])
  for (ind_ in indicators) {
    pop_use <- paste(pop[ind_], pop.yr, sep=".")
    wt_summary[i, paste0(ind_, "_yes")] <- sum(hpi_com[[exp_]] * (hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])/sum((hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])
    wt_summary[i, paste0(ind_, "_no")] <- sum(hpi_com[[exp_]]* (1 - hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])/sum((1 - hpi_com[[paste(ind_, pop.yr, sep=".")]]/100) * hpi_com[[pop_use]])
    if (ind_ %in% c("white_pct", "black_pct", "asian_pct", "latino_pct", "nativeAm_pct", "PacificIsl_pct")) {
      wt_summary[i, paste0(ind_, "_dif")] <- wt_summary[i, paste0(ind_, "_yes")] - wt_summary[i, paste0(ind_, "_no")] 
    } else {
      wt_summary[i, paste0(ind_, "_dif")] <- wt_summary[i, paste0(ind_, "_no")] - wt_summary[i, paste0(ind_, "_yes")] 
    }
  }
}
wt_summary <- rbind(wt_summary, cbind(year="18-06", wt_summary[wt_summary$year==2018, -1] - wt_summary[wt_summary$year==2006, -1] ))
wt_summary <- rbind(wt_summary, cbind(year="17-06", wt_summary[wt_summary$year==2017, -1] - wt_summary[wt_summary$year==2006, -1] ))
wt_summary <- rbind(wt_summary, cbind(year="18-06_3y", wt_summary[wt_summary$year=="16to18", -1] - wt_summary[wt_summary$year=="06to08", -1] ))
for (i in 1:nrow(wt_summary)) {
  for (ind_ in indicators) {
    pop_use <- pop[ind_]
    wt_summary[i, paste0(ind_, "_rel_dif")] <- 100 * wt_summary[i, paste0(ind_, "_dif")] / wt_summary[i, paste0(ind_, "_no")] 
  }
}

wt_summary_2 <- round(wt_summary[, 2:ncol(wt_summary)], digits=2)
print(cbind(wt_summary[, 1], wt_summary_2))
write.csv(wt_summary, file.path(indir1, "results", "ct_indicator_nonwildfire_wtavg.csv"), row.names = FALSE)

#######

### Figures
## Figure 1 and Figure S2 and Figure S3
#######
## county contour from the census tiger product
county.sp <- read_sf(file.path(indir2, "contours", "tl_2010_us_county10",
                               "tl_2010_us_county10.shp"), stringsAsFactors = FALSE, as_tibble = FALSE)
county.sp <- county.sp[county.sp$STATEFP10=="06", ]

ct.sp <- read_sf(file.path(indir2, "contours", "tl_2010_06_tract10",
                           "tl_2010_06_tract10.shp"), stringsAsFactors = FALSE, as_tibble = FALSE)

sp.dt <- merge(ct.sp, hpi_com, by.x="GEOID10", by.y="CensusTract", all.x=TRUE)

years <- c("06to18", 2006, 2018, "06to17", 2017, "06to08", "16to18", "18minus06", "18minus06_3y", "17minus06")
events <- c(paste0("pm25_avg.", years), paste0("pm25wf_avg.", years), paste0("pm25_nowf.", years))
events.nm <- c(expression(atop("Average 2006-2018", "total PM"[2.5]~"("*mu*"g/"*m^3*")  ")),
               expression(atop("Average 2006", "total PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2018", "total PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2017", "total PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Average 2017", "total PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2008", "total PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Average 2016-2018", "total PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Avg dif (2018-2006)", "total PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (3y_2018-2006)", "total PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (2017-2006)", "total PM"[2.5]~"("*mu*"g/"*m^3*")")),
               
               expression(atop("Average 2006-2018", "WF PM"[2.5]~"("*mu*"g/"*m^3*")  ")),
               expression(atop("Average 2006", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2018", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2017", "WF PM"[2.5]~"("*mu*"g/"*m^3*")        ")),
               expression(atop("Average 2017", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2008", "WF PM"[2.5]~"("*mu*"g/"*m^3*")        ")),
               expression(atop("Average 2016-2018", "WF PM"[2.5]~"("*mu*"g/"*m^3*")        ")),
               expression(atop("Avg dif (2018-2006)", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (3y_2018-2006)", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (2017-2006)", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               
               expression(atop("Average 2006-2018", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")  ")),
               expression(atop("Average 2006", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2018", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2017", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Average 2017", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2008", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Average 2016-2018", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Avg dif (2018-2006)", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (3y_2018-2006)", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (2017-2006)", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")"))
)

i <- 8
p1 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[i]))), color = "lightgrey", size = 0.00001) +
  scale_fill_gradientn(
    colors = c("#2166AC", "#F7F7F7", "#B2182B"), na.value = "gray80",
    rescaler = ~ scales::rescale_mid(.x, mid = 0),
    limits = c(-6, 6)) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[i])) +
  labs(x="", y="") +
  theme(text = element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[i+10]))), color = "lightgrey", size = 0.00001) +
  scale_fill_gradientn(
    colors = c("#2166AC", "#F7F7F7", "#B2182B"), na.value = "gray80",
    rescaler = ~ scales::rescale_mid(.x, mid = 0),
    limits = c(-6, 6)) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[i+10])) +
  labs(x="", y="") +
  theme(text = element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[i+20]))), color = "lightgrey", size = 0.00001) +
  scale_fill_gradientn(
    colors = c("#2166AC", "#F7F7F7", "#B2182B"), na.value = "gray80",
    rescaler = ~ scales::rescale_mid(.x, mid = 0),
    limits = c(-6, 6)) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[i+20])) +
  labs(x="", y="") +
  theme(text = element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


tiff(file.path(indir1, "figures", "figureS2_ct_complete_data_all_pm25_dif_0618.tiff"),
     width=8.7,height=8.7,units="in", res = 500, bg="transparent",
     pointsize=12, family="sans", compression = "zip")
# png(file.path(indir1, "figures", paste0("figureS2_ct_complete_data_all_pm25_dif_0618.png")),
#     width=8.7, height=8.7,units="in", res = 600, bg="transparent",
#     pointsize=12, family="sans")
print(plot_grid(p1, p3, p2, labels="AUTO", ncol = 2))
dev.off()



p1 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[2]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1
                       # ,limits = c(0, 20)
                       ) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[2])) +
  labs(x="", y="") +
  theme(text = element_text(size=8),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.92, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[2+10]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1
                       # ,limits = c(0, 8)
                       ) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[2+10])) +
  labs(x="", y="") +
  theme(text = element_text(size=8),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[2+20]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1
                       # ,limits = c(0, 20)
                       ) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[2+20])) +
  labs(x="", y="") +
  theme(text = element_text(size=8),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p4 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[3]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1
                       # ,limits = c(0, 20)
                       ) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[3])) +
  labs(x="", y="") +
  theme(text = element_text(size=8),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.92, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p5 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[3+10]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1
                       # ,limits = c(0, 8)
                       ) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[3+10])) +
  labs(x="", y="") +
  theme(text = element_text(size=8),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p6 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[3+20]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1
                       # ,limits = c(0, 20)
                       ) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[3+20])) +
  labs(x="", y="") +
  theme(text = element_text(size=8),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


tiff(file.path(indir1, "figures", "figureS3_ct_complete_data_all_pm25_06_18.tiff"),
     width=10,height=8,units="in", res = 500, bg="transparent",
     pointsize=10, family="sans", compression = "zip")
# png(file.path(indir1, "figures", paste0("figureS3_ct_complete_data_all_pm25_06_18.png")),
#     width=10,height=8,units="in", res = 600, bg="transparent",
#     pointsize=10, family="sans")
print(plot_grid(p1, p3, p2, p4, p6, p5, labels="AUTO", nrow = 2, byrow = T))
dev.off()


i <- 1
p1 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[i]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1,
                       limits = c(0, 20)) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[i])) +
  labs(x="", y="") +
  theme(text = element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p2 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[i+10]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1,
                       limits = c(0, 3)) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[i+10])) +
  labs(x="", y="") +
  theme(text = element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3 <- ggplot(sp.dt) +
  geom_sf(aes(fill = eval(as.name(events[i+20]))), color = "lightgrey", size = 0.00001) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1,
                       limits = c(0, 20)) +
  guides(fill = guide_colourbar(
    position = "inside",
    barheight = unit(1 , "in" ),
    title = events.nm[i+20])) +
  labs(x="", y="") +
  theme(text = element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.justification.inside = c(0.9, 0.95),
        legend.text=element_text(size=rel(0.8)),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


tiff(file.path(indir1, "figures", "fiugre1_ct_complete_data_all_pm25_avg_0618.tiff"),
     width=8.7,height=8.7,units="in", res = 500, bg="transparent",
     pointsize=12, family="sans", compression = "zip")
# png(file.path(indir1, "figures", paste0("figure1_ct_complete_data_all_pm25_avg_0618.png")),
#     width=8.7, height=8.7,units="in", res = 600, bg="transparent",
#     pointsize=12, family="sans")
print(plot_grid(p1, p3, p2, labels="AUTO", ncol = 2))
dev.off()


#######

## Figure 3 and Figure 4, and eTable 1
#######
total.wtavg <- fread(file.path(indir1, "results", 
                               "ct_indicator_wtavg.csv"))
total.wtavg <- total.wtavg[5:17, ]
total.wtavg$year <- as.numeric(total.wtavg$year)

wild.wtavg <- fread(file.path(indir1, "results", 
                              "ct_indicator_wildfire_wtavg.csv"))
wild.wtavg <- wild.wtavg[5:17, ]
wild.wtavg$year <- as.numeric(wild.wtavg$year)

nonwild.wtavg <- fread(file.path(indir1, "results",
                                 "ct_indicator_nonwildfire_wtavg.csv"))
nonwild.wtavg <- nonwild.wtavg[5:17, ]
nonwild.wtavg$year <- as.numeric(nonwild.wtavg$year)

## eTable 2
loc.rm <- grep("_rel", names(total.wtavg))
total.wtavg[, loc.rm] <- NULL
total.wtavg$exposure <- "total pm2.5"
foo <- total.wtavg

loc.rm <- grep("_rel", names(nonwild.wtavg))
nonwild.wtavg[, loc.rm] <- NULL
nonwild.wtavg$exposure <- "NWF pm2.5"
foo <- rbind(foo, nonwild.wtavg)

loc.rm <- grep("_rel", names(wild.wtavg))
wild.wtavg[, loc.rm] <- NULL
wild.wtavg$exposure <- "WF pm2.5"
foo <- rbind(foo, wild.wtavg)
write.csv(foo, file.path(indir1, "results", "etable1.csv"), row.names = FALSE)

## arrange colors
legend_colors_ses <- c("Unemployed - Employed"="#A6CEE3", 
                       "Below - Above poverty"="#1F78B4", 
                       "Lower - Higher Income"="#B2DF8A", 
                       "W/O - With College Educational Attainment"="#33A02C", 
                       "W/O - With High School Enrollment"="#FB9A99")
legend_colors_race <- c("White"="#1B9E77", "Black"="#D95F02", "Asian"="#7570B3", 
                        "Hispanic"="#E7298A", "Native American"="#66A61E", 
                        "Pacific Islander"="#E6AB02")

## combined ses figure, figure 3
p1 <- ggplot(total.wtavg, aes(x=year)) +
  geom_hline(yintercept=0, linewidth=1) +
  geom_line(aes(y=employed_dif, color="Unemployed - Employed"), linewidth=1.5) +
  geom_line(aes(y=abovepoverty_dif, color="Below - Above poverty"), linewidth=1.5) +
  geom_line(aes(y=income_median_dif, color="Lower - Higher Income"), linewidth=1.5) +
  geom_line(aes(y=bachelorsed_dif, color="W/O - With College Educational Attainment"), linewidth=1.5) +
  geom_line(aes(y=highschoool_dif, color="W/O - With High School Enrollment"), linewidth=1.5) +
  xlab("") +
  ylab(expression(atop("Absolute difference in average", "total PM"[2.5]~"("~mu*"g/"*m^3~")"))) +
  labs(color="") +
  scale_color_manual(values=legend_colors_ses) +
  scale_x_continuous(breaks=seq(2006, 2018, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position="none",
        axis.title.y = element_text(vjust = +1),
        axis.text.x = element_text(angle = 45, hjust=1))

p2 <- ggplot(wild.wtavg, aes(x=year)) +
  geom_hline(yintercept=0, linewidth=1) +
  geom_line(aes(y=employed_dif, color="Unemployed - Employed"), linewidth=1.5) +
  geom_line(aes(y=abovepoverty_dif, color="Below - Above poverty"), linewidth=1.5) +
  geom_line(aes(y=income_median_dif, color="Lower - Higher Income"), linewidth=1.5) +
  geom_line(aes(y=bachelorsed_dif, color="W/O - With College Educational Attainment"), linewidth=1.5) +
  geom_line(aes(y=highschoool_dif, color="W/O - With High School Enrollment"), linewidth=1.5) +
  xlab("") +
  ylab(expression(atop("Absolute difference in average","WF PM"[2.5]~"("~mu*"g/"*m^3~")"))) +
  labs(color="") +
  scale_color_manual(values=legend_colors_ses) +
  scale_x_continuous(breaks=seq(2006, 2018, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position="none",
        axis.title.y = element_text(vjust = +1),
        axis.text.x = element_text(angle = 45, hjust=1))

p3 <- ggplot(nonwild.wtavg, aes(x=year)) +
  geom_hline(yintercept=0, linewidth=1) +
  geom_line(aes(y=employed_dif, color="Unemployed - Employed"), linewidth=1.5) +
  geom_line(aes(y=abovepoverty_dif, color="Below - Above poverty"), linewidth=1.5) +
  geom_line(aes(y=income_median_dif, color="Lower - Higher Income"), linewidth=1.5) +
  geom_line(aes(y=bachelorsed_dif, color="W/O - With College Educational Attainment"), linewidth=1.5) +
  geom_line(aes(y=highschoool_dif, color="W/O - With High School Enrollment"), linewidth=1.5) +
  xlab("") +
  ylab(expression(atop("Absolute difference in average", "NWF PM"[2.5]~"("~mu*"g/"*m^3~")"))) +
  labs(color="") +
  scale_color_manual(values=legend_colors_ses) +
  scale_x_continuous(breaks=seq(2006, 2018, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14), legend.text = element_text(size = 14),
        legend.position="none",
        axis.title.y = element_text(vjust = +1),
        axis.text.x = element_text(angle = 45, hjust=1))

legend_p3 <- get_plot_component(
  ggplot(nonwild.wtavg, aes(x=year)) +
    geom_line(aes(y=employed_dif, color="Unemployed - Employed"), linewidth=1.5) +
    geom_line(aes(y=abovepoverty_dif, color="Below - Above poverty"), linewidth=1.5) +
    geom_line(aes(y=income_median_dif, color="Lower - Higher Income"), linewidth=1.5) +
    geom_line(aes(y=bachelorsed_dif, color="W/O - With College Educational Attainment"), linewidth=1.5) +
    geom_line(aes(y=highschoool_dif, color="W/O - With High School Enrollment"), linewidth=1.5) +
    scale_color_manual(values=legend_colors_ses) +
    scale_x_continuous(breaks=seq(2006, 2018, 1)) +
    labs(color="") +
    theme_bw() +
    theme(text = element_text(size = 14), legend.text = element_text(size = 12),
          legend.position="right",
          axis.title.y = element_text(vjust = +1),
          axis.text.x = element_text(angle = 45, hjust=1)), "guide-box", return_all = TRUE)[[1]]

tiff(file.path(indir1, "figures/figure3_combined_wtavg_ses.tiff"),
     width=8,height=8,units="in", res = 500, bg="transparent",
     pointsize=12, family="sans", compression = "zip")
# png(filename = file.path(indir1, "figures/figure3_combined_wtavg_ses.png"),
#     width=8, height=8, units="in", res = 600, bg="white",
#     pointsize=12, family="sans")
p_up <- plot_grid(p1, p3, labels="AUTO", nrow = 1)
p_bottom <- plot_grid(p2, legend_p3, labels=c("C", ""), nrow = 1, rel_widths = c(1, 1))
plot_grid(p_up, p_bottom, nrow=2)
dev.off()

## combined race/ethnicity figure, figure 4
p4 <- ggplot(total.wtavg, aes(x=year)) +
  geom_hline(yintercept=0, linewidth=1) +
  geom_line(aes(y=white_pct_dif, color="White"), linewidth=1.5) +
  geom_line(aes(y=black_pct_dif, color="Black"), linewidth=1.5) +
  geom_line(aes(y=asian_pct_dif, color="Asian"), linewidth=1.5) +
  geom_line(aes(y=latino_pct_dif, color="Hispanic"), linewidth=1.5) +
  geom_line(aes(y=nativeAm_pct_dif, color="Native American"), linewidth=1.5) +
  geom_line(aes(y=PacificIsl_pct_dif, color="Pacific Islander"), linewidth=1.5) +
  xlab("") +
  ylab(expression(atop("Absolute difference in average", "total PM"[2.5]~"("~mu*"g/"*m^3~")"))) +
  labs(color="") +
  scale_color_manual(values=legend_colors_race) +
  scale_x_continuous(breaks=seq(2006, 2018, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14), 
        legend.position="none",
        axis.title.y = element_text(vjust = +1), 
        axis.text.x = element_text(angle = 45, hjust=1))

p5 <- ggplot(wild.wtavg, aes(x=year)) +
  geom_hline(yintercept=0, linewidth=1) +
  geom_line(aes(y=white_pct_dif, color="White"), linewidth=1.5) +
  geom_line(aes(y=black_pct_dif, color="Black"), linewidth=1.5) +
  geom_line(aes(y=asian_pct_dif, color="Asian"), linewidth=1.5) +
  geom_line(aes(y=latino_pct_dif, color="Hispanic"), linewidth=1.5) +
  geom_line(aes(y=nativeAm_pct_dif, color="Native American"), linewidth=1.5) +
  geom_line(aes(y=PacificIsl_pct_dif, color="Pacific Islander"), linewidth=1.5) +
  xlab("") +
  ylab(expression(atop("Absolute difference in average","WF PM"[2.5]~"("~mu*"g/"*m^3~")"))) +
  labs(color="") +
  scale_color_manual(values=legend_colors_race) +
  scale_x_continuous(breaks=seq(2006, 2018, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14), 
        legend.position="none",
        axis.title.y = element_text(vjust = +1), 
        axis.text.x = element_text(angle = 45, hjust=1))

p6 <- ggplot(nonwild.wtavg, aes(x=year)) +
  geom_hline(yintercept=0, linewidth=1) +
  geom_line(aes(y=white_pct_dif, color="White"), linewidth=1.5) +
  geom_line(aes(y=black_pct_dif, color="Black"), linewidth=1.5) +
  geom_line(aes(y=asian_pct_dif, color="Asian"), linewidth=1.5) +
  geom_line(aes(y=latino_pct_dif, color="Hispanic"), linewidth=1.5) +
  geom_line(aes(y=nativeAm_pct_dif, color="Native American"), linewidth=1.5) +
  geom_line(aes(y=PacificIsl_pct_dif, color="Pacific Islander"), linewidth=1.5) +
  xlab("") +
  ylab(expression(atop("Absolute difference in average", "NWF PM"[2.5]~"("~mu*"g/"*m^3~")"))) +
  labs(color="") +
  scale_color_manual(values=legend_colors_race) +
  scale_x_continuous(breaks=seq(2006, 2018, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14), legend.text = element_text(size = 14),
        legend.position="none",
        axis.title.y = element_text(vjust = +1), 
        axis.text.x = element_text(angle = 45, hjust=1))

legend_p6 <- get_plot_component(
  ggplot(nonwild.wtavg, aes(x=year)) +
    geom_line(aes(y=white_pct_dif, color="White"), linewidth=1.5) +
    geom_line(aes(y=black_pct_dif, color="Black"), linewidth=1.5) +
    geom_line(aes(y=asian_pct_dif, color="Asian"), linewidth=1.5) +
    geom_line(aes(y=latino_pct_dif, color="Hispanic"), linewidth=1.5) +
    geom_line(aes(y=nativeAm_pct_dif, color="Native American"), linewidth=1.5) +
    geom_line(aes(y=PacificIsl_pct_dif, color="Pacific Islander"), linewidth=1.5) +
    scale_color_manual(values=legend_colors_race) +
    scale_x_continuous(breaks=seq(2006, 2018, 1)) +
    labs(color="") +
    theme_bw() +
    theme(text = element_text(size = 14), legend.text = element_text(size = 14),
          legend.position="right",
          axis.title.y = element_text(vjust = +1), 
          axis.text.x = element_text(angle = 45, hjust=1)), "guide-box", return_all = TRUE)[[1]]

tiff(file.path(indir1, "figures/figure4_combined_wtavg_race.tiff"),
     width=8,height=8,units="in", res = 550, bg="transparent",
     pointsize=12, family="sans", compression = "zip")
# png(filename = file.path(indir1, "figures/figure4_combined_wtavg_race.png"),
#     width=8, height=8, units="in", res = 600, bg="white",
#     pointsize=12, family="sans")
p_uprace <- plot_grid(p4, p6, labels="AUTO", nrow = 1)
p_bottomrace <- plot_grid(p5, legend_p6, labels=c("C", ""), nrow = 1, rel_widths = c(1, 1))
plot_grid(p_uprace, p_bottomrace, nrow=2)
dev.off()
#######

## Figure S1
#######
temp <- pm.all.com[pm.all.com$year %in% c(2006:2018),]

p1 <- ggplot(temp, aes(x=year, y=pm25_avg)) + 
  geom_boxplot() +
  labs(
    x="Year", y=expression("Average total PM"[2.5]~"("~mu*"g/"*m^3~")")) +
  theme_bw() +
  theme(text = element_text(size=9), plot.title = element_text(hjust = 0.5))

p2 <- ggplot(temp, aes(x=year, y=pm25wf_avg)) + 
  geom_boxplot() +
  labs(x="Year", y=expression("Average wildfire PM"[2.5]~"("~mu*"g/"*m^3~")")) +
  theme_bw() +
  theme(text = element_text(size=9), plot.title = element_text(hjust = 0.5))

p3 <- ggplot(temp, aes(x=year, y=pm25_nowf)) + 
  geom_boxplot() +
  labs(x="Year", y=expression("Average non-wildfire PM"[2.5]~"("~mu*"g/"*m^3~")")) +
  theme_bw() +
  theme(text = element_text(size=9), plot.title = element_text(hjust = 0.5))

tiff(file.path(indir1, "figures/figureS1_ct_complete_data_Average_all_pm25_by_year.png.tiff"),
     width=8,height=6, units="in", res = 500, bg="transparent",
     pointsize=12, family="sans", compression = "zip")
# png(file.path(indir1, "figures", paste0("figureS1_ct_complete_data_Average_all_pm25_by_year.png")),
#     width=8,height=6,units="in", res = 600, bg="transparent",
#     pointsize=12, family="sans")
print(plot_grid(p1, p3, p2, labels="AUTO", ncol = 2))
dev.off()
#######
