### codes to conduct analysis for "The diverging role of increasing wildfire 
### smoke to ambient PM2.5 exposure disparity in California (2006 to 2018)" project
### written and prepared by Chen Chen and Jenny Nguyen
library(data.table)
library(rgdal)
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

### data read in
#######
hpi_com <- fread(file.path(indir1, "data", "ct_acs5y_pm06to19_complete.csv")) ## complete data in wide format
pm.all <- fread(file.path(indir1, "data", "pm25_ct_2006to2019_by_year.csv")) ## exposure data in long format
pm.all.com <- pm.all[pm.all$geoid %in% hpi_com$CensusTract, ]
names(pm.all.com)[1] <- "GEOID"
hpi_com$CensusTract <- paste0("0", hpi_com$CensusTract)
#######

### Analysis
## Rank-rank correlations and Figure 2 and eFigure 2
#######
rank.plot.f2 <- function(y1, y2, input, exposure, nm) { ## plot with aggregation
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
  plot(data$rp, data$rp_new_avg, pch=19,
       main=paste(nm, "correlation is",
                  round(cor(data[, paste0("pm_", y1)], data[, paste0("pm_", y2)], 
                            method = "spearman")*100, digits = 1), "%"),
       xlab=paste("Rank percentile in", y1), ylab=paste("Mean rank percentile in", y2), xlim=c(0,100), ylim=c(0, 100), cex.main=2, cex.lab=1.5)
  abline(0, 1, lwd=2, col="red")
}


png(file.path(indir1, "figures", paste0("rank_rank_Average_total_pm25.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
rank.plot.f2(2006, 2018, pm.all.com, "pm25_avg", "Total PM")
dev.off()

cat("Relationship between wildfire PM2.5 rank in 2006 and mean PM2.5 rank in 2018", "\n")
png(file.path(indir1, "figures", paste0("rank_rank_Average_WF_pm25.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
rank.plot.f2(2006, 2018, pm.all.com, "pm25wf_avg", "WF PM")
dev.off()

cat("Relationship between non-wf PM2.5 rank in 2006 and mean PM2.5 rank in 2018", "\n")
png(file.path(indir1, "figures", paste0("rank_rank_Average_non_WF_pm25.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
rank.plot.f2(2006, 2018, pm.all.com, "pm25_nowf", "NWF PM")
dev.off()

## Figure 2
p1 <- image_ggplot(image_read(file.path(indir1, "figures", "rank_rank_Average_total_pm25.png")))
p2 <- image_ggplot(image_read(file.path(indir1, "figures", "rank_rank_Average_WF_pm25.png")))
p3 <- image_ggplot(image_read(file.path(indir1, "figures", "rank_rank_Average_non_WF_pm25.png")))
png(file.path(indir1, "figures", paste0("rank_rank_Average.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
plot_grid(p1, p3, p2, labels = "AUTO", nrow=2)
dev.off()


cat("Relationship between PM2.5 rank in 06to08 and mean PM2.5 rank in 16to18", "\n")
png(file.path(indir1, "figures", paste0("rank_rank_3y_Average_total_pm25.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
rank.plot.f2("06to08", "16to18", pm.all.com, "pm25_avg", "Total PM")
dev.off()

cat("Relationship between wildfire PM2.5 rank in 06to08 and mean PM2.5 rank in 16to18", "\n")
png(file.path(indir1, "figures", paste0("rank_rank_3y_Average_WF_pm25.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
rank.plot.f2("06to08", "16to18", pm.all.com, "pm25wf_avg", "WF PM")
dev.off()

cat("Relationship between non-wf PM2.5 rank in 06to08 and mean PM2.5 rank in 16to18", "\n")
png(file.path(indir1, "figures", paste0("rank_rank_3y_Average_non_WF_pm25.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
rank.plot.f2("06to08", "16to18", pm.all.com, "pm25_nowf", "NWF PM")
dev.off()

## eFigure 2
par(mfrow=c(1, 1))
p1 <- image_ggplot(image_read(file.path(indir1, "figures", "rank_rank_3y_Average_total_pm25.png")))
p2 <- image_ggplot(image_read(file.path(indir1, "figures", "rank_rank_3y_Average_WF_pm25.png")))
p3 <- image_ggplot(image_read(file.path(indir1, "figures", "rank_rank_3y_Average_non_WF_pm25.png")))
png(file.path(indir1, "figures", paste0("rank_rank_3y_Average.png")),
    width=6,height=4,units="in", res = 600, bg="transparent",
    pointsize=12, family="sans")
plot_grid(p1, p3, p2, labels = "AUTO", nrow=2)
dev.off()
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
## Figure 1 and eFigure 3
#######
## county contour from the census tiger product
county.sp <- readOGR(file.path(indir2, "contours", "tl_2010_us_county10",
                               "tl_2010_us_county10.shp"), stringsAsFactors = FALSE)
county.sp <- county.sp[county.sp$STATEFP10=="06", ]
county.sp@data$id <- rownames(county.sp@data)
county.dt <- fortify(county.sp, region = "id")

ct.sp <- readOGR(file.path(indir2, "contours", "tl_2010_06_tract10",
                           "tl_2010_06_tract10.shp"), stringsAsFactors = FALSE)

## census tract contour from the census tiger product
## transform the spatialpolygon to data.frame
ct.sp@data$id <- rownames(ct.sp@data)
ct.dt <- fortify(ct.sp, region = "id")
bar <- merge(ct.dt, ct.sp@data[, c("GEOID10", "id")], by = "id")
## merge exposure and plotting polygons
foo <- merge(bar, hpi_com, by.x="GEOID10", by.y="CensusTract", all.x=TRUE)
foo <- foo[order(foo$order), ]

years <- c("06to18", 2006, 2018, "06to17", 2017, "06to08", "16to18", "18minus06", "18minus06_3y", "17minus06")
events <- c(paste0("pm25_avg.", years), paste0("pm25wf_avg.", years), paste0("pm25_nowf.", years))
events.nm <- c(expression(atop("Average 2006-2018", "PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Average 2006", "PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2018", "PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2017", "PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Average 2017", "PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2008", "PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Average 2016-2018", "PM"[2.5]~"("*mu*"g/"*m^3*")          ")),
               expression(atop("Avg dif (2018-2006)", "PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (3y_2018-2006)", "PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (2017-2006)", "PM"[2.5]~"("*mu*"g/"*m^3*")")),
               
               expression(atop("Average 2006-2018", "WF PM"[2.5]~"("*mu*"g/"*m^3*")     ")),
               expression(atop("Average 2006", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2018", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2017", "WF PM"[2.5]~"("*mu*"g/"*m^3*")        ")),
               expression(atop("Average 2017", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Average 2006-2008", "WF PM"[2.5]~"("*mu*"g/"*m^3*")        ")),
               expression(atop("Average 2016-2018", "WF PM"[2.5]~"("*mu*"g/"*m^3*")        ")),
               expression(atop("Avg dif (2018-2006)", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (3y_2018-2006)", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               expression(atop("Avg dif (2017-2006)", "WF PM"[2.5]~"("*mu*"g/"*m^3*")")),
               
               expression(atop("Average 2006-2018", "NWF PM"[2.5]~"("*mu*"g/"*m^3*")     ")),
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
for (i in 1:length(years)) {
  
  if (i %in% 8:10) {
    p1 <- ggplot(foo, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = eval(as.name(events[i])))) +
      coord_map() +
      scale_fill_continuous_divergingx(palette = 'RdBu', na.value = "gray80", mid = 0, rev=TRUE) +
      guides(fill = guide_colourbar(
        barheight = unit(1 , "in" ),
        title = events.nm[i])) +
      labs(x="", y="") +
      geom_path(data=county.dt, aes(long, lat, group=group), size=0.05, col="grey") + ## county contour
      theme(text = element_text(size=16),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.position=c(.8,.8),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_rect(fill="transparent"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    p2 <- ggplot(foo, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = eval(as.name(events[i+10])))) +
      coord_map() +
      scale_fill_continuous_divergingx(palette = 'RdBu', na.value = "gray80", mid = 0, rev=TRUE) +
      guides(fill = guide_colourbar(
        barheight = unit(1 , "in" ),
        title = events.nm[i+10])) +
      labs(x="", y="") +
      geom_path(data=county.dt, aes(long, lat, group=group), size=0.05, col="grey") + ## county contour
      theme(text = element_text(size=16),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.position=c(.8,.8),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_rect(fill="transparent"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    p3 <- ggplot(foo, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = eval(as.name(events[i+20])))) +
      coord_map() +
      scale_fill_continuous_divergingx(palette = 'RdBu', na.value = "gray80", mid = 0, rev=TRUE) +
      guides(fill = guide_colourbar(
        barheight = unit(1 , "in" ),
        title = events.nm[i+20])) +
      labs(x="", y="") +
      geom_path(data=county.dt, aes(long, lat, group=group), size=0.05, col="grey") + ## county contour
      theme(text = element_text(size=16),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.position=c(.8,.8),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_rect(fill="transparent"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  } else {
    p1 <- ggplot(foo, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = eval(as.name(events[i])))) +
      coord_map() +
      scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1) +
      guides(fill = guide_colourbar(
        barheight = unit(1 , "in" ),
        title = events.nm[i])) +
      labs(x="", y="") +
      geom_path(data=county.dt, aes(long, lat, group=group), size=0.05, col="grey") + ## county contour
      theme(text = element_text(size=16),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.position=c(.8,.8),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_rect(fill="transparent"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    p2 <- ggplot(foo, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = eval(as.name(events[i+10])))) +
      coord_map() +
      scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1) +
      guides(fill = guide_colourbar(
        barheight = unit(1 , "in" ),
        title = events.nm[i+10])) +
      labs(x="", y="") +
      geom_path(data=county.dt, aes(long, lat, group=group), size=0.05, col="grey") + ## county contour
      theme(text = element_text(size=16),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.position=c(.8,.8),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_rect(fill="transparent"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    p3 <- ggplot(foo, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = eval(as.name(events[i+20])))) +
      coord_map() +
      scale_fill_distiller(palette = "YlOrRd", na.value = "gray80", direction = 1) +
      guides(fill = guide_colourbar(
        barheight = unit(1 , "in" ),
        title = events.nm[i+20])) +
      labs(x="", y="") +
      geom_path(data=county.dt, aes(long, lat, group=group), size=0.05, col="grey") + ## county contour
      theme(text = element_text(size=16),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.position=c(.8,.8),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_rect(fill="transparent"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  }
  
  png(file.path(indir1, "figures", paste0("ct_complete_data_Average_pm25_", years[i],".png")),
      width=5,height=5,units="in", res = 600, bg="transparent",
      pointsize=12, family="sans")
  print(p1)
  dev.off()
  
  png(file.path(indir1, "figures", paste0("ct_complete_data_Average_wildfire_pm25_", years[i],".png")),
      width=5,height=5,units="in", res = 600, bg="transparent", 
      pointsize=12, family="sans")
  print(p2)
  dev.off()
  
  png(file.path(indir1, "figures", paste0("ct_complete_data_Average_non_wildfire_pm25_", years[i],".png")),
      width=5,height=5,units="in", res = 600, bg="transparent", 
      pointsize=12, family="sans")
  print(p3)
  dev.off()
  
  png(file.path(indir1, "figures", paste0("ct_complete_data_all_pm25_", years[i],".png")),
      width=10,height=10,units="in", res = 600, bg="transparent",
      pointsize=12, family="sans")
  print(plot_grid(p1, p3, p2, labels="AUTO", ncol = 2))
  dev.off()
}
#######

## Figure 3 and Figure 4
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

legend_colors_ses <- c("Unemployment"="#A6CEE3", "Poverty"="#1F78B4", "Low Income"="#B2DF8A", "Low College Educational Attainment"="#33A02C", "Low High School Enrollment"="#FB9A99")
legend_colors_race <- c("White"="#1B9E77", "Black"="#D95F02", "Asian"="#7570B3", "Hispanic"="#E7298A", "Native American"="#66A61E", "Pacific Islander"="#E6AB02")

## combined ses figure
p1 <- ggplot(total.wtavg, aes(x=year)) +
  geom_hline(yintercept=0, linewidth=1) +
  geom_line(aes(y=employed_dif, color="Unemployment"), linewidth=1.5) +
  geom_line(aes(y=abovepoverty_dif, color="Poverty"), linewidth=1.5) +
  geom_line(aes(y=income_median_dif, color="Low Income"), linewidth=1.5) +
  geom_line(aes(y=bachelorsed_dif, color="Low College Educational Attainment"), linewidth=1.5) +
  geom_line(aes(y=highschoool_dif, color="Low High School Enrollment"), linewidth=1.5) +
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
  geom_line(aes(y=employed_dif, color="Unemployment"), linewidth=1.5) +
  geom_line(aes(y=abovepoverty_dif, color="Poverty"), linewidth=1.5) +
  geom_line(aes(y=income_median_dif, color="Low Income"), linewidth=1.5) +
  geom_line(aes(y=bachelorsed_dif, color="Low College Educational Attainment"), linewidth=1.5) +
  geom_line(aes(y=highschoool_dif, color="Low High School Enrollment"), linewidth=1.5) +
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
  geom_line(aes(y=employed_dif, color="Unemployment"), linewidth=1.5) +
  geom_line(aes(y=abovepoverty_dif, color="Poverty"), linewidth=1.5) +
  geom_line(aes(y=income_median_dif, color="Low Income"), linewidth=1.5) +
  geom_line(aes(y=bachelorsed_dif, color="Low College Educational Attainment"), linewidth=1.5) +
  geom_line(aes(y=highschoool_dif, color="Low High School Enrollment"), linewidth=1.5) +
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

legend_p3 <- get_legend(
  ggplot(nonwild.wtavg, aes(x=year)) +
    geom_line(aes(y=employed_dif, color="Unemployment"), linewidth=1.5) +
    geom_line(aes(y=abovepoverty_dif, color="Poverty"), linewidth=1.5) +
    geom_line(aes(y=income_median_dif, color="Low Income"), linewidth=1.5) +
    geom_line(aes(y=bachelorsed_dif, color="Low College Educational Attainment"), linewidth=1.5) +
    geom_line(aes(y=highschoool_dif, color="Low High School Enrollment"), linewidth=1.5) +
    scale_color_manual(values=legend_colors_ses) +
    scale_x_continuous(breaks=seq(2006, 2018, 1)) +
    labs(color="") +
    theme_bw() +
    theme(text = element_text(size = 14), legend.text = element_text(size = 14),
          legend.position="right",
          axis.title.y = element_text(vjust = +1), 
          axis.text.x = element_text(angle = 45, hjust=1))
)

png(filename = file.path(indir1, "figures/combined_wtavg_ses.png"),
    width=8, height=8, units="in", res = 600, bg="white",
    pointsize=12, family="sans")
p_up <- plot_grid(p1, p3, labels="AUTO", nrow = 1)
p_bottom <- plot_grid(p2, legend_p3, labels=c("C", ""), nrow = 1, rel_widths = c(1, 1))
plot_grid(p_up, p_bottom, nrow=2)
dev.off()

## combined race/ethnicity figure
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

legend_p6 <- get_legend(
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
          axis.text.x = element_text(angle = 45, hjust=1))
)


png(filename = file.path(indir1, "figures/combined_wtavg_race.png"),
    width=8, height=8, units="in", res = 600, bg="white",
    pointsize=12, family="sans")
p_uprace <- plot_grid(p4, p6, labels="AUTO", nrow = 1)
p_bottomrace <- plot_grid(p5, legend_p6, labels=c("C", ""), nrow = 1, rel_widths = c(1, 1))
plot_grid(p_uprace, p_bottomrace, nrow=2)
dev.off()
#######

## eFigure 1
#######
temp <- pm.all.com[pm.all.com$year %in% c(2006:2018),]

png(file.path(indir1, "figures", paste0("ct_complete_data_Average_total_pm25_by_year.png")),
      width=6,height=4,units="in", res = 600, bg="transparent",
      pointsize=12, family="sans")
ggplot(temp, aes(x=year, y=pm25_avg)) + 
  geom_boxplot() +
  labs(
    x="Year", y=expression("Average total PM"[2.5]~"("~mu*"g/"*m^3~")")) +
  theme_bw() +
  theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5))
dev.off()

png(file.path(indir1, "figures", paste0("ct_complete_data_Average_wildfire_pm25_by_year.png")),
      width=6,height=4,units="in", res = 600, bg="transparent",
      pointsize=12, family="sans")
ggplot(temp, aes(x=year, y=pm25wf_avg)) + 
  geom_boxplot() +
  labs(x="Year", y=expression("Average wildfire PM"[2.5]~"("~mu*"g/"*m^3~")")) +
  theme_bw() +
  theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5))
dev.off()

png(file.path(indir1, "figures", paste0("ct_complete_data_Average_non-wf_pm25_by_year.png")),
      width=6,height=4,units="in", res = 600, bg="transparent",
      pointsize=12, family="sans")
ggplot(temp, aes(x=year, y=pm25_nowf)) + 
  geom_boxplot() +
  labs(x="Year", y=expression("Average non-wildfire PM"[2.5]~"("~mu*"g/"*m^3~")")) +
  theme_bw() +
  theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5))
dev.off()
#######
