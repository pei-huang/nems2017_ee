# This file is used to process data for the building energy efficiency scenario
## Author: Pei Huang, 05/09/2019

# required packages------------------------------------
library(data.table)
library(ggplot2)
library(foreign)
library(stringr)
library(maps)
library(viridis)
library(ggrepel)
library(ggsci)
library(Hmisc)
library(grid)
library(gridExtra)

# set working directory------------------------------------
base_path <- "~/Dropbox (Yale_FES)/NEMS Docs/Scenario-EE/NEMS_Output"
setwd(base_path)

# set up scenario names
scenario <- c("s00", "s01", "s02", "s03", "s04", "s05", "s06", "s07", "s08")
scenario_main <- paste0("s", str_pad(c(0:4), 2, "left", pad = "0"))
scenario_sens <- paste0("s", str_pad(c(5:8), 2, "left", pad = "0"))

## EE scenarios
scenario_name_1 <- c("Reference",
                     "Intermediate EE",
                     "Optimistic EE",
                     "Reference",
                     "Optimistic EE",
                     "Optimistic EE",
                     "Optimistic EE",
                     "Reference",
                     "Optimistic EE")
scenario_name_1 <- factor(scenario_name_1,
                          levels = unique(scenario_name_1))

## carbon pricing scenarios
scenario_name_2 <- c("No",
                     "No",
                     "No",
                     "Yes",
                     "Yes",
                     "No",
                     "No",
                     "No",
                     "No")
scenario_name_2 <- factor(scenario_name_2,
                          levels = unique(scenario_name_2))

## sensitivity scenarios
scenario_name_3 <- c("Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment & Shell",
                     "Equipment",
                     "Shell",
                     "Low Renewables Cost",
                     "Low Renewables Cost")
scenario_name_3 <- factor(scenario_name_3,
                          levels = unique(scenario_name_3))


# NEMS output data------------------------------------------------------------------
filenames_model <- list.files(path=paste0(base_path, "/NEMS_Results"), pattern="ref_no_cpp_+.*csv")
data_model_all <- list()
for (k in 1:length(filenames_model)){
  ## read data
  data_scen <- fread(paste0(base_path, "/NEMS_Results/", filenames_model[k]),
                     strip.white = TRUE,
                     stringsAsFactors = FALSE,
                     na.strings = c("NA","NaN", ""),
                     fill = TRUE)
  
  ## process data into long format
  data_scen <- data_scen[, -c(1, dim(data_scen)[2], dim(data_scen)[2]-1), with = FALSE]
  col <- names(data_scen)[grepl("20", names(data_scen))]
  data_scen[, (col) := lapply(.SD, as.numeric), .SDcols = col]
  data_scen <- melt(data_scen, id.vars=names(data_scen)[!grepl("20", names(data_scen))], variable.name="Year")
  data_scen[, Year := as.numeric(as.character(Year))]
  
  data_scen[, RegionNum := as.integer(RegionNum)]
  
  data_scen[, `:=`(Scen = scenario[k],
                   Scenario.1 = scenario_name_1[k],
                   Scenario.2 = scenario_name_2[k],
                   Scenario.3 = scenario_name_3[k])]
  
  data_model_all[[k]] <- data_scen
}
data_model_all <- rbindlist(data_model_all)

## update data to reference before 2016
data_model_ref <- data_model_all[Scen == "s00"]
data_model_ref <- data_model_ref[, `:=`(Scen = NULL, Scenario.1 = NULL, Scenario.2 = NULL, Scenario.3 = NULL)]
setnames(data_model_ref, old = c("value"), new = c("refval"))

data_model_all <- merge(data_model_all, data_model_ref,
                        by = c("Datekey", "TableNumber", "RowNum", "RegionNum",
                               "VarName", "GLabel", "Gunits", "RowFmt",
                               "DaType", "SubDat", "Sector", "SubSec",
                               "Source", "SubSrc", "Geogr", "Year"))
data_model_all[Year <= 2016, value := refval]
data_model_all[, refval := NULL]

# ## dataset for excluding the carbon pricing scenario
# data_model_all_ctax <- data.table(data_model_all)
# data_model_all <- data_model_all[Scen != "ee3"]

data_model_main <- data_model_all[Scen %in% scenario_main]
data_model_sens <- data_model_all[Scen %in% scenario_sens]


# NEMS CO2 emissions by fuel data (industrial) ------------------------------------------------------------------
filenames_co2 <- list.files(path=paste0(base_path, "/NEMS_Results"), pattern="demand_co2_+.*csv")
data_co2_sec_all <- list()
for (k in 1:length(filenames_co2)){
  ## read residential data
  data_co2_scen_res <- fread(paste0(base_path, "/NEMS_Results/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 6,
                             nrows = 11)
  
  ## process data into long format
  data_co2_scen_res <- data_co2_scen_res[-c(1:2), -c(2:3, dim(data_co2_scen_res)[2]), with = FALSE]
  names(data_co2_scen_res)[1] <- c("Fuel")
  data_co2_scen_res[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_res <- data_co2_scen_res[Fuel %in% c("Oil Subtotal",
                                                     "Natural Gas",
                                                     "Electricity",
                                                     "Total Residential")]
  data_co2_scen_res[Fuel == "Total Residential", Fuel := "Total"]
  data_co2_scen_res[, Sector := "Residential"]
  
  ## read commercial data
  data_co2_scen_com <- fread(paste0(base_path, "/NEMS_Results/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 34,
                             nrows = 13)
  
  ## process data into long format
  data_co2_scen_com <- data_co2_scen_com[-c(1:2), -c(2:3, dim(data_co2_scen_com)[2]), with = FALSE]
  names(data_co2_scen_com)[1] <- c("Fuel")
  data_co2_scen_com[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_com <- data_co2_scen_com[Fuel %in% c("Oil Subtotal",
                                                     "Coal",
                                                     "Natural Gas",
                                                     "Electricity",
                                                     "Total Commercial")]
  data_co2_scen_com[Fuel == "Total Commercial", Fuel := "Total"]
  data_co2_scen_com[, Sector := "Commercial"]
  
  ## read industrial data
  data_co2_scen_ind <- fread(paste0(base_path, "/NEMS_Results/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 66,
                             nrows = 24)
  
  ## process data into long format
  data_co2_scen_ind <- data_co2_scen_ind[-c(1:2), -c(2:3, dim(data_co2_scen_ind)[2]), with = FALSE]
  names(data_co2_scen_ind)[1] <- c("Fuel")
  data_co2_scen_ind[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_ind <- data_co2_scen_ind[Fuel %in% c("Oil Subtotal",
                                                     "Coal Subtotal",
                                                     "Natural Gas Subtotal",
                                                     "Electricity",
                                                     "Total Industrial")]
  data_co2_scen_ind[Fuel == "Total Industrial", Fuel := "Total"]
  data_co2_scen_ind[, Sector := "Industrial"]
  
  ## read transportation data
  data_co2_scen_trn <- fread(paste0(base_path, "/NEMS_Results/", filenames_co2[k]),
                             strip.white = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = c("NA","NaN", ""),
                             fill = TRUE,
                             header = TRUE,
                             skip = 120,
                             nrows = 17)
  
  ## process data into long format
  data_co2_scen_trn <- data_co2_scen_trn[-c(1:2), -c(2:3, dim(data_co2_scen_trn)[2]), with = FALSE]
  names(data_co2_scen_trn)[1] <- c("Fuel")
  data_co2_scen_trn[, Fuel := trimws(Fuel, which = c("both"), whitespace = " ")]
  data_co2_scen_trn <- data_co2_scen_trn[Fuel %in% c("Oil Subtotal",
                                                     "Natural Gas",
                                                     "Electricity",
                                                     "Total Transportation")]
  data_co2_scen_trn[Fuel == "Total Transportation", Fuel := "Total"]
  data_co2_scen_trn[, Sector := "Transportation"]
  
  ## merge sector data
  data_co2_scen <- rbind(data_co2_scen_res, data_co2_scen_com, data_co2_scen_ind, data_co2_scen_trn)
  
  data_co2_scen <- melt(data_co2_scen, id.vars = c("Sector", "Fuel"), variable.name = "Year", variable.factor = FALSE)
  data_co2_scen[, Year := as.integer(Year)]
  data_co2_scen <- data_co2_scen[Year >= 2013]
  
  data_co2_scen[, `:=`(Scen = scenario[k],
                       Scenario.1 = scenario_name_1[k],
                       Scenario.2 = scenario_name_2[k],
                       Scenario.3 = scenario_name_3[k])]
  
  data_co2_sec_all[[k]] <- data_co2_scen
}
data_co2_sec_all <- rbindlist(data_co2_sec_all)

## update data to reference before 2016
data_co2_ind_ref <- data_co2_sec_all[Scen == "s00"]
data_co2_ind_ref <- data_co2_ind_ref[, `:=`(Scen = NULL, Scenario.1 = NULL, Scenario.2 = NULL, Scenario.3 = NULL)]
setnames(data_co2_ind_ref, old = c("value"), new = c("refval"))

data_co2_sec_all <- merge(data_co2_sec_all, data_co2_ind_ref,
                           by = c("Sector", "Fuel", "Year"))
data_co2_sec_all[Year <= 2016, value := refval]
data_co2_sec_all[, refval := NULL]


# plot: energy consumption and co2 emissions total--------------------------------------------------------------------------------------
data_consum_all <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector == "all sectors"
                                   & Source %in% c("total")
                                   & Geogr == "united states", ]

data_co2_all <- data_model_main[TableNumber == 2
                                & DaType == "emissions"
                                & Geogr == "united states", ]

data_consum_co2 <- rbind(data_consum_all, data_co2_all)
data_consum_co2[, DaType := as.factor(DaType)]
levels(data_consum_co2$DaType) <- c("(A) Energy Consumption (quads)", "(B) Carbon Dioxide Emissions (MMT)")

plot_consum_co2 <- ggplot(data = data_consum_co2, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ DaType, scale = "free") +
  ylab("") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


pdf("fig_cons_co2.pdf", width = 5, height = 3)
print(plot_consum_co2)
dev.off()

png("fig_cons_co2.png", width = 5, height = 3, units = "in", res = 500)
print(plot_consum_co2)
dev.off()

## generate output data
data_consum_all_out <- data_consum_all[, .(Scen, Year, value)]
data_consum_all_out <- merge(data_consum_all_out,
                             data_consum_all_out[Scen == scenario[1]],
                             by = "Year")

data_consum_all_out <- data_consum_all_out[Scen.x != scenario[1]]
data_consum_all_out[, change := value.x - value.y]
data_consum_all_out[, change.pct := (value.x - value.y)/value.y]
data_consum_all_out <- dcast.data.table(data_consum_all_out, Year ~ Scen.x, value.var = c("change", "change.pct"))

fwrite(data_consum_all_out, "fig_cons_data.csv", row.names = FALSE)

## generate output data
data_co2_out <- data_co2_all[, .(Scen, Year, value)]
data_co2_out <- merge(data_co2_out,
                      data_co2_out[Scen == scenario[1]],
                      by = "Year")

data_co2_out <- data_co2_out[Scen.x != scenario[1]]
data_co2_out[, change := value.x - value.y]
data_co2_out[, change.pct := (value.x - value.y)/value.y]
data_co2_out <- dcast.data.table(data_co2_out, Year ~ Scen.x, value.var = c("change", "change.pct"))

fwrite(data_co2_out, "fig_co2_data.csv", row.names = FALSE)



# plot (sensitivity): energy consumption and co2 emissions--------------------------------------------------------------------------------------
data_consum_sens <- data_model_all[TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector == "all sectors"
                                   & Source %in% c("total")
                                   & Geogr == "united states", ]

data_co2_sens <- data_model_all[TableNumber == 2
                                & DaType == "emissions"
                                & Geogr == "united states", ]

data_consum_co2_sens <- rbind(data_consum_sens, data_co2_sens)
data_consum_co2_sens <- data_consum_co2_sens[Scen %in% scenario[c(1,3,8,9)]]

data_consum_co2_sens[, DaType := as.factor(DaType)]
levels(data_consum_co2_sens$DaType) <- c("(A) Energy Consumption (quads)", "(B) Carbon Dioxide Emissions (MMT)")

data_consum_co2_sens[, Scenario.3 := ifelse(Scenario.3 == "Low Renewables Cost", "Yes", "No")]

plot_consum_co2_sens <- ggplot(data = data_consum_co2_sens, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.3)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ DaType, scale = "free") +
  ylab("") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Low Renewables Cost", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("fig_cons_co2_sens.pdf", width = 5, height = 3)
print(plot_consum_co2_sens)
dev.off()

png("fig_cons_co2_sens.png", width = 5, height = 3, units = "in", res = 500)
print(plot_consum_co2_sens)
dev.off()


# plot: energy intensity aggregate-------------------------------------------------------------------------------------
data_int_aggr <- data_model_main[TableNumber == 2
                                 & DaType == "intensity"
                                 & Geogr == "united states", ]

plot_int_aggr <- ggplot(data = data_int_aggr, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Energy intensity (thousand Btu/2009$ of GDP)") +
  ggtitle("Energy Use : Energy Intensity") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_ene_int.pdf", width = 4, height = 4.5)
print(plot_int_aggr)
dev.off()

png("sm_fig_ene_int.png", width = 4, height = 4.5, units = "in", res = 500)
print(plot_int_aggr)
dev.off()


# plot: energy efficiency by sector-------------------------------------------------------------------------------------
data_eff <- data_model_main[TableNumber == 31
                            & SubDat == "energy efficiency"
                            & Sector %in% c("residential", "commercial", "industrial",
                                            "transportation", "electric power")
                            & Geogr == "united states", ]

plot_eff <- ggplot(data = data_eff, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Index") +
  ggtitle("Energy Efficiency by Sector") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: energy intensity: residential-------------------------------------------------------------------------------------
data_res_int <- data_model_main[TableNumber == 4
                                & DaType == "intensity"
                                & SubSec == "total energy"
                                & Geogr == "united states", ]

plot_res_int <- ggplot(data = data_res_int, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ GLabel, scale = "free") +
  ylab("A: MMBtu/household; B: thousand Btu/sq ft") +
  ggtitle("Residential Total Energy Intensity per household and square foot") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: energy intensity: commercial-------------------------------------------------------------------------------------
data_com_int <- data_model_main[TableNumber == 5
                                & DaType == "intensity"
                                & SubSrc == "total energy consumption"
                                & Geogr == "united states", ]

plot_com_int <- ggplot(data = data_com_int, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab(unique(data_com_int[, Gunits])) +
  ggtitle("Commercial Energy Intensity per square foot") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: energy consumption by sector --------------------------------------------------------------------------------------
data_consum_sec <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   # & Sector %in% c("all sectors", "commercial", "residential")
                                   & !(Sector %in% c("total energy", "all sectors"))
                                   & Source %in% c("total")
                                   & Geogr == "united states", ]

data_consum_sec <- data_consum_sec[Sector != "other"]

data_consum_sec[, Sector := capitalize(Sector)]

plot_consum_sec <- ggplot(data = data_consum_sec, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Energy Consumption by sector") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_cons_sec.pdf", width = 6, height = 5)
print(plot_consum_sec)
dev.off()

png("sm_fig_cons_sec.png", width = 6, height = 5, units = "in", res = 500)
print(plot_consum_sec)
dev.off()


# plot: energy consumption (delta) by sector-------------------------------------------------------------------------------------
data_consum_sec_delta <- data_consum_sec[Scen != "s01", .(Scen, Sector, Year, value)]

data_consum_sec_delta_s00 <- data_consum_sec_delta[Scen == "s00"]
data_consum_sec_delta_s00[, `:=`(Scen = NULL)]

data_consum_sec_delta <- merge(data_consum_sec_delta, data_consum_sec_delta_s00, by = c("Sector", "Year"))
data_consum_sec_delta[, delta := value.x - value.y]
data_consum_sec_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_consum_sec_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                        Scenario.2 = scenario_name_2[i],
                                                        Scenario.3 = scenario_name_3[i])]
}

plot_consum_sec_delta <- ggplot(data = data_consum_sec_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Difference in energy consumption (quads)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_consum_sec_delta.pdf", width = 6, height = 5)
print(plot_consum_sec_delta)
dev.off()

png("sm_fig_consum_sec_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_consum_sec_delta)
dev.off()

## check the differenes
data_consum_sec_check <- data_consum_sec_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Sector, delta)]
data_consum_sec_check <- dcast(data_consum_sec_check, Sector ~ Scen, value.var = "delta")


# plot: energy consumption (delta) by fuel for power ----------------------------------------------------------------------------------
data_consum_power <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector %in% c("electric power")
                                   & Source %in% c("natural gas",
                                                   "steam coal",
                                                   "nuclear",
                                                   "renewable energy")
                                   & Geogr == "united states", ]

data_consum_power[, Source := capitalize(Source)]

data_consum_power_delta <- data_consum_power[Scen != "s01", .(Scen, Source, Year, value)]
data_consum_power_delta_s00 <- data_consum_power_delta[Scen == "s00"]
data_consum_power_delta_s00[, `:=`(Scen = NULL)]

data_consum_power_delta <- merge(data_consum_power_delta, data_consum_power_delta_s00, by = c("Source", "Year"))
data_consum_power_delta[, delta := value.x - value.y]
data_consum_power_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_consum_power_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                          Scenario.2 = scenario_name_2[i],
                                                          Scenario.3 = scenario_name_3[i])]
}

plot_consum_power_delta <- ggplot(data = data_consum_power_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy consumption (quads)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_consum_power_delta.pdf", width = 6, height = 5)
print(plot_consum_power_delta)
dev.off()

png("sm_fig_consum_power_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_consum_power_delta)
dev.off()

## check the differenes
data_consum_power_check <- data_consum_power_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Source, delta)]
data_consum_power_check <- dcast(data_consum_power_check, Source ~ Scen, value.var = "delta")


# plot: energy consumption by fuel for industrial ----------------------------------------------------------------------------------
data_consum_ind <- data_model_main[TableNumber == 2
                                   & DaType == "consumption"
                                   & Sector %in% c("industrial")
                                   & Source %in% c("liquid fuels subtotal",
                                                   "natural gas subtotal",
                                                   "coal subtotal",
                                                   "renewable energy",
                                                   "electricity")
                                   & Geogr == "united states", ]

data_consum_ind[, Source := capitalize(Source)]

plot_consum_ind <- ggplot(data = data_consum_ind[Scen != "s01"], mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source, scales = "free") +
  ylab("Energy consumption (quads)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_consum_ind.pdf", width = 6, height = 5)
print(plot_consum_ind)
dev.off()

png("sm_fig_consum_ind.png", width = 6, height = 5, units = "in", res = 500)
print(plot_consum_ind)
dev.off()


# plot: energy consumption (delta) by fuel for industrial ----------------------------------------------------------------------------------
data_consum_ind_delta <- data_consum_ind[Scen != "s01", .(Scen, Source, Year, value)]
data_consum_ind_delta_s00 <- data_consum_ind_delta[Scen == "s00"]
data_consum_ind_delta_s00[, `:=`(Scen = NULL)]

data_consum_ind_delta <- merge(data_consum_ind_delta, data_consum_ind_delta_s00, by = c("Source", "Year"))
data_consum_ind_delta[, delta := value.x - value.y]
data_consum_ind_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_consum_ind_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                        Scenario.2 = scenario_name_2[i],
                                                        Scenario.3 = scenario_name_3[i])]
}

plot_consum_ind_delta <- ggplot(data = data_consum_ind_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy consumption (quads)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_consum_ind_delta.pdf", width = 6, height = 5)
print(plot_consum_ind_delta)
dev.off()

png("sm_fig_consum_ind_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_consum_ind_delta)
dev.off()

## check the differenes
data_consum_ind_check <- data_consum_ind_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Source, delta)]
data_consum_ind_check <- dcast(data_consum_ind_check, Source ~ Scen, value.var = "delta")


# plot: energy consumption (delta) by refinery for industrial ----------------------------------------------------------------------------------
data_consum_ind_refine <- data_model_main[TableNumber == 6
                                          & DaType == "consumption"
                                          & SubSec %in% c("industrial use excluding refining",
                                                          "refining use")
                                          & Source %in% c("total")
                                          & Geogr == "united states", ]
data_consum_ind_refine[, SubSec := capitalize(SubSec)]

data_consum_ind_refine_delta <- data_consum_ind_refine[Scen != "s01", .(Scen, SubSec, Year, value)]
data_consum_ind_refine_delta_s00 <- data_consum_ind_refine_delta[Scen == "s00"]
data_consum_ind_refine_delta_s00[, `:=`(Scen = NULL)]

data_consum_ind_refine_delta <- merge(data_consum_ind_refine_delta, data_consum_ind_refine_delta_s00, by = c("SubSec", "Year"))
data_consum_ind_refine_delta[, delta := value.x - value.y]
data_consum_ind_refine_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_consum_ind_refine_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                               Scenario.2 = scenario_name_2[i],
                                                               Scenario.3 = scenario_name_3[i])]
}

plot_consum_ind_refine_delta <- ggplot(data = data_consum_ind_refine_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ SubSec) +
  ylab("Difference in energy consumption (quads)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_consum_ind_refine_delta.pdf", width = 6, height = 5)
print(plot_consum_ind_refine_delta)
dev.off()

png("sm_fig_consum_ind_refine_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_consum_ind_refine_delta)
dev.off()

## check the differenes
data_consum_ind_refine_check <- data_consum_ind_refine_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, SubSec, delta)]
data_consum_ind_refine_check <- dcast(data_consum_ind_refine_check, SubSec ~ Scen, value.var = "delta")


# plot: energy consumption by fuels: total--------------------------------------------------------------------------------------
data_cons_fuel <- data_model_main[TableNumber == 2
                                  & DaType == "consumption"
                                  & Sector == "total energy"
                                  & Source %in% c("liquid fuels subtotal",
                                                  "natural gas subtotal",
                                                  "coal subtotal",
                                                  "nuclear",
                                                  "biofuels heat and coproducts",
                                                  "renewable energy")
                                  & Geogr == "united states", ]

data_cons_fuel[, Source := as.factor(Source)]
levels(data_cons_fuel$Source) <- c("Biofuels",
                                   "Coal",
                                   "Liquid Fuels",
                                   "Natural Gas",
                                   "Nuclear",
                                   "Renewables")

# data_cons_fuel_test <- data_cons_fuel[Year == 2050 & Scen %in% c("s00", "s01", "s02"), .(Scen, Source, value)]
# data_cons_fuel_test <- merge(data_cons_fuel_test,
#                              data_cons_fuel_test[Scen == scenario[1]],
#                              by = c("Source"))
# data_cons_fuel_test <- data_cons_fuel_test[Scen.x != scenario[1]]
# data_cons_fuel_test[, pct.change := (value.x - value.y)/value.y*100]


plot_cons_fuel <- ggplot(data = data_cons_fuel, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Energy Consumption by fuel type") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_cons_fuel.pdf", width = 6, height = 5)
print(plot_cons_fuel)
dev.off()

png("sm_fig_cons_fuel.png", width = 6, height = 5, units = "in", res = 500)
print(plot_cons_fuel)
dev.off()


# plot: energy consumption: HVAC and refrigeration--------------------------------------------------------------------------------------
data_consum_resd_hvac <- data_model_main[TableNumber == 4
                                         & Source %in% c("total energy use by end use")
                                         & SubSec %in% c("space heating", "space cooling", "refrigeration", "freezers")
                                         & Geogr == "united states", ]

data_consum_comm_hvac <- data_model_main[TableNumber == 5
                                         & Source %in% c("total energy use by end use")
                                         & SubSrc %in% c("space heating", "space cooling", "refrigeration", "ventilation")
                                         & Geogr == "united states", ]
data_consum_comm_hvac[, SubSec := SubSrc]

data_consum_hvac <- rbind(data_consum_resd_hvac, data_consum_comm_hvac)
data_consum_hvac <- data_consum_hvac[, .(value = sum(value)),
                                     by = .(Scen, Scenario.1, Scenario.2, Scenario.3, Year)]

plot_consum_hvac <- ggplot(data = data_consum_hvac, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Energy consumption (quads)") +
  ggtitle("Energy Consumption for HVAC and refrigeration") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: energy consumption by end use: residential-------------------------------------------------------------------------------------
data_res_con <- data_model_main[TableNumber == 4
                                & DaType == "consumption"
                                & SubSec != ""
                                & Source == "total energy use by end use"
                                & Geogr == "united states", ]

data_res_con <- data_res_con[SubSec != "other uses"]
data_res_con[, SubSec := capitalize(SubSec)]

plot_res_con <- ggplot(data = data_res_con, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ SubSec, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Residential : Total Energy Use by End Use") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_cons_res_use.pdf", width = 7, height = 7)
print(plot_res_con)
dev.off()

png("sm_fig_cons_res_use.png", width = 7, height = 7, units = "in", res = 500)
print(plot_res_con)
dev.off()


# plot: energy consumption by end use: commercial-------------------------------------------------------------------------------------
data_com_con <- data_model_main[TableNumber == 5
                                & DaType == "consumption"
                                & SubSrc != ""
                                & Source == "total energy use by end use"
                                & Geogr == "united states", ]

data_com_con <- data_com_con[SubSrc != "other uses"]
data_com_con[, SubSrc := capitalize(SubSrc)]

plot_com_con <- ggplot(data = data_com_con, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ SubSrc, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Commercial: Total Energy Use by End Use") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_cons_com_use.pdf", width = 6, height = 6)
print(plot_com_con)
dev.off()

png("sm_fig_cons_com_use.png", width = 6, height = 6, units = "in", res = 500)
print(plot_com_con)
dev.off()


# plot: energy consumption by fuel: residential-------------------------------------------------------------------------------------
data_res_fuel <- data_model_main[TableNumber == 4
                                 & RowNum %in% c(35, 44, 50, 57, 59, 60)
                                 & Geogr == "united states", ]

data_res_fuel[, Source := as.factor(Source)]
levels(data_res_fuel$Source) <- c("Distillate",
                                  "Electricity",
                                  "Wood",
                                  "Natural Gas",
                                  "Other Fuels",
                                  "Propane")

plot_res_fuel <- ggplot(data = data_res_fuel, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Residential : Total Energy Use by Fuel") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: energy consumption by fuel: commercial-------------------------------------------------------------------------------------
data_com_fuel <- data_model_main[TableNumber == 5
                                 & RowNum %in% c(28, 36, 42, 44, 45)
                                 & Geogr == "united states", ]

data_com_fuel[, Source := as.factor(Source)]
levels(data_com_fuel$Source) <- c("Biomass",
                                  "Distillate",
                                  "Natural Gas",
                                  "Other Fuels",
                                  "Electricity")

plot_com_fuel <- ggplot(data = data_com_fuel, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Commercial : Total Energy Use by Fuel") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: energy consumption by fuel: industrial-------------------------------------------------------------------------------------
data_ind_fuel <- data_model_main[TableNumber == 6
                                 & RowNum %in% c(101, 107, 111, 112, 113, 114)
                                 & Geogr == "united states", ]

data_ind_fuel[, Source := as.factor(Source)]
levels(data_ind_fuel$Source) <- c("Biofuels",
                                  "Coal",
                                  "Natural Gas",
                                  "Petroleum",
                                  "Electricity",
                                  "Renewables")

plot_ind_fuel <- ggplot(data = data_ind_fuel, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Commercial : Total Energy Use by Fuel") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


## plot: carbon emissions by equipment and shell------------------------------------------------------
data_co2_split <- data_model_all[TableNumber == 2
                                 & DaType == "emissions"
                                 & Geogr == "united states", ]
data_co2_split <- data_co2_split[Scen %in% c("s00", "s02", "s05", "s06")]

data_co2_split[, Scenario.Con := paste(Scenario.1, Scenario.3, sep = " - ")]

plot_co2_split <- ggplot(data = data_co2_split, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.Con)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab(unique(data_co2_split[,Gunits])) +
  ggtitle(unique(data_co2_split[, GLabel])) +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank()) +
  guides(colour = guide_legend(title="", byrow = TRUE, nrow = 3), linetype = FALSE) +
  scale_color_npg()


# plot (sensitivity): energy consumption by fuels: total--------------------------------------------------------------------------------------
data_cons_fuel_sens <- data_model_all[TableNumber == 2
                                      & DaType == "consumption"
                                      & Sector == "total energy"
                                      & Source %in% c("liquid fuels subtotal",
                                                      "natural gas subtotal",
                                                      "coal subtotal",
                                                      "nuclear",
                                                      "biofuels heat and coproducts",
                                                      "renewable energy")
                                      & Geogr == "united states", ]

data_cons_fuel_sens <- data_cons_fuel_sens[Scen %in% scenario[c(1,3,8,9)]]

data_cons_fuel_sens[, Source := as.factor(Source)]
levels(data_cons_fuel_sens$Source) <- c("Biofuels",
                                        "Coal",
                                        "Liquid Fuels",
                                        "Natural Gas",
                                        "Nuclear",
                                        "Renewables")

data_cons_fuel_sens[, Scenario.3 := ifelse(Scenario.3 == "Low Renewables Cost", "Yes", "No")]

plot_cons_fuel_sens <- ggplot(data = data_cons_fuel_sens, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.3)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source, scale = "free") +
  ylab("Energy consumption (quads)") +
  ggtitle("Energy Consumption by fuel type") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Low Renewables Cost", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_sens_cons_fuel.pdf", width = 6, height = 5)
print(plot_cons_fuel_sens)
dev.off()

png("sm_fig_sens_cons_fuel.png", width = 6, height = 5, units = "in", res = 500)
print(plot_cons_fuel_sens)
dev.off()


# plot: carbon intensity aggregate-------------------------------------------------------------------------------------
data_co2eff_aggr <- data_model_main[TableNumber == 2
                                    & Source == "ghg intensity"
                                    & Geogr == "united states", ]

plot_co2eff_aggr <- ggplot(data = data_co2eff_aggr, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  ylab("Carbon intensity (MMT CO2/million 2009$ GDP)") +
  ggtitle("Aggregate Carbon Intensity") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 2, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: carbon efficiency by sector-------------------------------------------------------------------------------------
data_co2eff <- data_model_main[TableNumber == 31
                               & SubDat == "carbon efficiency"
                               & Sector %in% c("residential", "commercial", "industrial",
                                               "transportation")
                               & Geogr == "united states", ]

plot_co2eff <- ggplot(data = data_co2eff, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Carbon efficiency index") +
  ggtitle("Carbon Efficiency by Sector") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: carbon emissions by end use: residential-------------------------------------------------------------------------------------
data_res_co2 <- data_model_main[TableNumber == 22
                                & Sector == "residential"
                                & Geogr == "united states", ]

plot_res_co2 <- ggplot(data = data_res_co2, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ SubSec, scale = "free") +
  ylab("Carbon emissions (MMT)") +
  ggtitle("Carbon Dioxide by End Use: Residential") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: carbon emissions by end use: commercial-------------------------------------------------------------------------------------
data_com_co2 <- data_model_main[TableNumber == 22
                                & Sector == "commercial"
                                & Geogr == "united states", ]

plot_com_co2 <- ggplot(data = data_com_co2, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ SubSec, scale = "free") +
  ylab("Carbon emissions (MMT)") +
  ggtitle("Carbon Dioxide by End Use: Commercial") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


# plot: carbon emissions by sector-------------------------------------------------------------------------------------
data_co2_sec <- data_model_main[TableNumber == 17
                                & Sector %in% c("residential",
                                                "commercial",
                                                "industrial",
                                                "transportation",
                                                "electric power")
                                & Source == ""
                                & Geogr == "united states", ]

data_co2_sec[, Sector := capitalize(Sector)]

# data_co2_sec <- data_co2_sec[Scen != "s01"]

plot_co2_sec <- ggplot(data = data_co2_sec, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Carbon emissions (MMT)") +
  ggtitle("Carbon Dioxide by Sector") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


pdf("sm_fig_co2_sec.pdf", width = 6, height = 5)
print(plot_co2_sec)
dev.off()

png("sm_fig_co2_sec.png", width = 6, height = 5, units = "in", res = 500)
print(plot_co2_sec)
dev.off()


# plot: carbon emissions (delta) by sector (including the power sector) ------------------------------------------------
data_co2_sec_delta <- data_co2_sec[Scen != "s01", .(Scen, Sector, Year, value)]
data_co2_sec_delta_s00 <- data_co2_sec_delta[Scen == "s00"]
data_co2_sec_delta_s00[, `:=`(Scen = NULL)]

data_co2_sec_delta <- merge(data_co2_sec_delta, data_co2_sec_delta_s00, by = c("Sector", "Year"))
data_co2_sec_delta[, delta := value.x - value.y]
data_co2_sec_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_co2_sec_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                     Scenario.2 = scenario_name_2[i],
                                                     Scenario.3 = scenario_name_3[i])]
}

plot_co2_sec_delta <- ggplot(data = data_co2_sec_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector) +
  ylab("Difference in CO2 emissions (MMT)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_co2_sec_delta.pdf", width = 6, height = 5)
print(plot_co2_sec_delta)
dev.off()

png("sm_fig_co2_sec_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_co2_sec_delta)
dev.off()

## check the differenes
data_co2_sec_check <- data_co2_sec_delta[Year == 2050, .(Scen, Sector, delta)]
data_co2_sec_check <- dcast(data_co2_sec_check, Sector ~ Scen, value.var = "delta")


# plot: carbon emissions by fuel in sectors-------------------------------------------------------------------------------------
for (j in 1:uniqueN(data_co2_sec_all$Sector)){
  sector_name <- unique(data_co2_sec_all$Sector)[j]
  
  data_co2_fuel_sec <- data_co2_sec_all[Scen != "s01" & Sector == sector_name & Fuel != "Total"]
  
  plot_co2_fuel_sec <- ggplot(data = data_co2_fuel_sec, mapping = aes(x = Year, y = value, group = Scen)) +
    geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
    geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
    facet_wrap(~ Fuel, scale = "free", nrow = 2) +
    ylab("Carbon emissions (MMT)") +
    theme(legend.position = 'bottom',
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-7,-7,-7,-7),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          legend.box = "vertical",
          legend.title=element_text(size=9),
          legend.text=element_text(size=8),
          axis.title=element_text(size=9),
          axis.text = element_text(size=8),
          plot.title = element_blank(),
          strip.text = element_text(size=8)) +
    guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
           linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
    scale_color_npg()
  
  pdf(paste0("sm_fig_co2_fuel_", sector_name, ".pdf"), width = 6, height = 5)
  print(plot_co2_fuel_sec)
  dev.off()
  
  png(paste0("sm_fig_co2_fuel_", sector_name, ".png"), width = 6, height = 5, units = "in", res = 500)
  print(plot_co2_fuel_sec)
  dev.off()
}


# plot: carbon emissions (delta) by fuel in sectors-------------------------------------------------------------------------------------
data_co2_fuel_sec_delta <- data_co2_sec_all[Scen != "s01"]

data_co2_fuel_sec_delta_s00 <- data_co2_fuel_sec_delta[Scen == "s00"]
data_co2_fuel_sec_delta_s00[, `:=`(Scen = NULL, Scenario.1 = NULL, Scenario.2 = NULL, Scenario.3 = NULL)]

data_co2_fuel_sec_delta <- merge(data_co2_fuel_sec_delta, data_co2_fuel_sec_delta_s00, by = c("Sector", "Fuel", "Year"))
data_co2_fuel_sec_delta[, delta := value.x - value.y]
data_co2_fuel_sec_delta[, `:=`(value.x = NULL, value.y = NULL)]


for (j in 1:uniqueN(data_co2_sec_all$Sector)){
  sector_name <- unique(data_co2_sec_all$Sector)[j]
  
  data_co2_fuel_sec_delta_sub <- data_co2_fuel_sec_delta[Sector == sector_name & Fuel != "Total"]
  
  plot_co2_fuel_sec_delta <- ggplot(data = data_co2_fuel_sec_delta_sub, mapping = aes(x = Year, y = delta, group = Scen)) +
    geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
    geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
    facet_wrap(~ Fuel, nrow = 2) +
    ylab("Difference in carbon emissions (MMT)") +
    theme(legend.position = 'bottom',
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-7,-7,-7,-7),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          legend.box = "vertical",
          legend.title=element_text(size=9),
          legend.text=element_text(size=8),
          axis.title=element_text(size=9),
          axis.text = element_text(size=8),
          plot.title = element_blank(),
          strip.text = element_text(size=8)) +
    guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
           linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
    scale_color_npg()
  
  pdf(paste0("sm_fig_co2_fuel_", sector_name, "_delta.pdf"), width = 6, height = 5)
  print(plot_co2_fuel_sec_delta)
  dev.off()
  
  png(paste0("sm_fig_co2_fuel_", sector_name, "_delta.png"), width = 6, height = 5, units = "in", res = 500)
  print(plot_co2_fuel_sec_delta)
  dev.off()
}


# plot: carbon emissions (delta) by sector (excluding the power sector) ------------------------------------------------
## emissions from electricity are accounted to each end-use sector
data_co2_total_sec_delta <- data_co2_fuel_sec_delta[Fuel == "Total"]

plot_co2_total_sec_delta <- ggplot(data = data_co2_total_sec_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector) +
  ylab("Difference in CO2 emissions (MMT)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_co2_sec_nopower_delta.pdf", width = 6, height = 5)
print(plot_co2_total_sec_delta)
dev.off()

png("sm_fig_co2_sec_nopower_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_co2_total_sec_delta)
dev.off()

## check the differenes
data_co2_sec_check <- data_co2_total_sec_delta[Year == 2050 & Scen != "s00", .(Scen, Sector, delta)]
data_co2_sec_check <- dcast(data_co2_sec_check, Sector ~ Scen, value.var = "delta")


# plot (sensitivity): carbon emissions by sector-------------------------------------------------------------------------------------
data_co2_sec_sens <- data_model_all[TableNumber == 17
                                    & Sector %in% c("residential",
                                                    "commercial",
                                                    "industrial",
                                                    "transportation",
                                                    "electric power")
                                    & Source == ""
                                    & Geogr == "united states", ]

data_co2_sec_sens[, Sector := capitalize(Sector)]

data_co2_sec_sens <- data_co2_sec_sens[Scen %in% scenario[c(1,3,8,9)]]

data_co2_sec_sens[, Scenario.3 := ifelse(Scenario.3 == "Low Renewables Cost", "Yes", "No")]

plot_co2_sec_sens <- ggplot(data = data_co2_sec_sens, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.3)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Carbon emissions (MMT)") +
  ggtitle("Carbon Dioxide by Sector") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_sens_co2_sec.pdf", width = 6, height = 5)
print(plot_co2_sec_sens)
dev.off()

png("sm_fig_sens_co2_sec.png", width = 6, height = 5, units = "in", res = 500)
print(plot_co2_sec_sens)
dev.off()


# plot: average energy price by sector-------------------------------------------------------------------------------------
data_price_sec <- data_model_main[TableNumber == 3
                                  & SubDat == "real"
                                  & Source == "sector average prices"
                                  & Geogr == "united states", ]
data_price_sec[, Sector := capitalize(Sector)]
# data_price_sec <- data_price_sec[Scen != "s01"]

plot_price_sec <- ggplot(data = data_price_sec, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Average energy prices (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()


pdf("sm_fig_price_sec.pdf", width = 6, height = 5)
print(plot_price_sec)
dev.off()

png("sm_fig_price_sec.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_sec)
dev.off()


# plot: average energy price (delta) by sector ---------------------------------------------------------------------------
data_price_sec_delta <- data_price_sec[Scen != "s01", .(Scen, Sector, Year, value)]
data_price_sec_delta_s00 <- data_price_sec_delta[Scen == "s00"]
data_price_sec_delta_s00[, `:=`(Scen = NULL)]

data_price_sec_delta <- merge(data_price_sec_delta, data_price_sec_delta_s00, by = c("Sector", "Year"))
data_price_sec_delta[, delta := value.x - value.y]
data_price_sec_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_price_sec_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                       Scenario.2 = scenario_name_2[i],
                                                       Scenario.3 = scenario_name_3[i])]
}

plot_price_sec_delta <- ggplot(data = data_price_sec_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Difference in average energy price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_price_sec_delta.pdf", width = 6, height = 5)
print(plot_price_sec_delta)
dev.off()

png("sm_fig_price_sec_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_sec_delta)
dev.off()

## check the differenes
data_price_sec_check <- data_price_sec_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Sector, delta)]
data_price_sec_check <- dcast(data_price_sec_check, Sector ~ Scen, value.var = "delta")


# plot: average fossil fuel price (delta) by sector ---------------------------------------------------------------------------
data_fossil_price_sec <- data_model_main[TableNumber == 3
                                         & SubDat == "real"
                                         & Source == "sector average fossil prices"
                                         & Geogr == "united states", ]
data_fossil_price_sec[, Sector := capitalize(Sector)]

data_fossil_price_sec_delta <- data_fossil_price_sec[Scen != "s01", .(Scen, Sector, Year, value)]
data_fossil_price_sec_delta_s00 <- data_fossil_price_sec_delta[Scen == "s00"]
data_fossil_price_sec_delta_s00[, `:=`(Scen = NULL)]

data_fossil_price_sec_delta <- merge(data_fossil_price_sec_delta, data_fossil_price_sec_delta_s00, by = c("Sector", "Year"))
data_fossil_price_sec_delta[, delta := value.x - value.y]
data_fossil_price_sec_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_fossil_price_sec_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                              Scenario.2 = scenario_name_2[i],
                                                              Scenario.3 = scenario_name_3[i])]
}

plot_fossil_price_sec_delta <- ggplot(data = data_fossil_price_sec_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector, scale = "free") +
  ylab("Difference in average fossil price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_fossil_price_sec_delta.pdf", width = 6, height = 5)
print(plot_fossil_price_sec_delta)
dev.off()

png("sm_fig_fossil_price_sec_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_fossil_price_sec_delta)
dev.off()

## check the differenes
data_fossil_price_sec_check <- data_fossil_price_sec_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Sector, delta)]
data_fossil_price_sec_check <- dcast(data_fossil_price_sec_check, Sector ~ Scen, value.var = "delta")


# plot: average energy price (delta) by fuel for power ---------------------------------------------------------------------------
data_price_power <- data_model_main[TableNumber == 3
                                  & SubDat == "real"
                                  & Sector == "electric power"
                                  & Source != "sector average fossil prices"
                                  & Geogr == "united states", ]
data_price_power[, Source := capitalize(Source)]

data_price_power_delta <- data_price_power[Scen != "s01", .(Scen, Source, Year, value)]
data_price_power_delta_s00 <- data_price_power_delta[Scen == "s00"]
data_price_power_delta_s00[, `:=`(Scen = NULL)]

data_price_power_delta <- merge(data_price_power_delta, data_price_power_delta_s00, by = c("Source", "Year"))
data_price_power_delta[, delta := value.x - value.y]
data_price_power_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_price_power_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                         Scenario.2 = scenario_name_2[i],
                                                         Scenario.3 = scenario_name_3[i])]
}

plot_price_power_delta <- ggplot(data = data_price_power_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_price_power_delta.pdf", width = 6, height = 5)
print(plot_price_power_delta)
dev.off()

png("sm_fig_price_power_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_power_delta)
dev.off()

## check the differenes
data_price_power_check <- data_price_power_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Source, delta)]
data_price_power_check <- dcast(data_price_power_check, Source ~ Scen, value.var = "delta")


# plot: average energy price by fuel for industrial ---------------------------------------------------------------------------
data_price_ind <- data_model_main[TableNumber == 3
                                  & SubDat == "real"
                                  & DaType == "price"
                                  & Sector == "industrial"
                                  & !(Source %in% c("sector average prices",
                                                    "sector average fossil prices",
                                                    "coal to liquids",
                                                    "residual fuel oil",
                                                    "distillate fuel oil",
                                                    "metallurgical coal"))
                                  & Geogr == "united states", ]
data_price_ind[Source == "other industrial coal", Source := "Coal"]

data_price_ind[, Source := capitalize(Source)]

plot_price_ind <- ggplot(data = data_price_ind[Scen != "s01"], mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source, scale = "free") +
  ylab("Energy price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_price_ind.pdf", width = 6, height = 5)
print(plot_price_ind)
dev.off()

png("sm_fig_price_ind.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_ind)
dev.off()


# plot: average energy price (delta) by fuel for industrial ----------------------------------------------------------------
data_price_ind_delta <- data_price_ind[Scen != "s01", .(Scen, Source, Year, value)]
data_price_ind_delta_s00 <- data_price_ind_delta[Scen == "s00"]
data_price_ind_delta_s00[, `:=`(Scen = NULL)]

data_price_ind_delta <- merge(data_price_ind_delta, data_price_ind_delta_s00, by = c("Source", "Year"))
data_price_ind_delta[, delta := value.x - value.y]
data_price_ind_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_price_ind_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                       Scenario.2 = scenario_name_2[i],
                                                       Scenario.3 = scenario_name_3[i])]
}

plot_price_ind_delta <- ggplot(data = data_price_ind_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_price_ind_delta.pdf", width = 6, height = 5)
print(plot_price_ind_delta)
dev.off()

png("sm_fig_price_ind_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_ind_delta)
dev.off()

## check the differenes
data_price_ind_check <- data_price_ind_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Source, delta)]
data_price_ind_check <- dcast(data_price_ind_check, Source ~ Scen, value.var = "delta")


# plot: average energy price (delta) by fuel for residential ---------------------------------------------------------------------------
data_price_res <- data_model_main[TableNumber == 3
                                  & SubDat == "real"
                                  & DaType == "price"
                                  & Sector == "residential"
                                  & !(Source %in% c("sector average prices",
                                                    "sector average fossil prices",
                                                    "coal to liquids",
                                                    "other industrial coal"))
                                  & Geogr == "united states", ]
data_price_res[, Source := capitalize(Source)]

data_price_res_delta <- data_price_res[Scen != "s01", .(Scen, Source, Year, value)]
data_price_res_delta_s00 <- data_price_res_delta[Scen == "s00"]
data_price_res_delta_s00[, `:=`(Scen = NULL)]

data_price_res_delta <- merge(data_price_res_delta, data_price_res_delta_s00, by = c("Source", "Year"))
data_price_res_delta[, delta := value.x - value.y]
data_price_res_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_price_res_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                       Scenario.2 = scenario_name_2[i],
                                                       Scenario.3 = scenario_name_3[i])]
}

plot_price_res_delta <- ggplot(data = data_price_res_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_price_res_delta.pdf", width = 6, height = 5)
print(plot_price_res_delta)
dev.off()

png("sm_fig_price_res_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_res_delta)
dev.off()

## check the differenes
data_price_res_check <- data_price_res_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Source, delta)]
data_price_res_check <- dcast(data_price_res_check, Source ~ Scen, value.var = "delta")


# plot: average energy price (delta) by fuel for commercial ---------------------------------------------------------------------------
data_price_com <- data_model_main[TableNumber == 3
                                  & SubDat == "real"
                                  & DaType == "price"
                                  & Sector == "commercial"
                                  & !(Source %in% c("sector average prices",
                                                    "sector average fossil prices",
                                                    "coal to liquids",
                                                    "other industrial coal"))
                                  & Geogr == "united states", ]
data_price_com[, Source := capitalize(Source)]

data_price_com_delta <- data_price_com[Scen != "s01", .(Scen, Source, Year, value)]
data_price_com_delta_s00 <- data_price_com_delta[Scen == "s00"]
data_price_com_delta_s00[, `:=`(Scen = NULL)]

data_price_com_delta <- merge(data_price_com_delta, data_price_com_delta_s00, by = c("Source", "Year"))
data_price_com_delta[, delta := value.x - value.y]
data_price_com_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_price_com_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                       Scenario.2 = scenario_name_2[i],
                                                       Scenario.3 = scenario_name_3[i])]
}

plot_price_com_delta <- ggplot(data = data_price_com_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_price_com_delta.pdf", width = 6, height = 5)
print(plot_price_com_delta)
dev.off()

png("sm_fig_price_com_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_com_delta)
dev.off()

## check the differenes
data_price_com_check <- data_price_com_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Source, delta)]
data_price_com_check <- dcast(data_price_com_check, Source ~ Scen, value.var = "delta")


# plot: average energy price (delta) by fuel for all ---------------------------------------------------------------------------
data_price_all <- data_model_main[TableNumber == 3
                                  & SubDat == "real"
                                  & DaType == "price"
                                  & Sector == "total energy"
                                  & !(Source %in% c("e85",
                                                    "motor gasoline",
                                                    "jet fuel",
                                                    "distillate fuel oil",
                                                    "coal to liquids",
                                                    "delivered average price",
                                                    "delivered fossil average price"))
                                  & Geogr == "united states", ]
data_price_all[, Source := capitalize(Source)]

data_price_all_delta <- data_price_all[Scen != "s01", .(Scen, Source, Year, value)]
data_price_all_delta_s00 <- data_price_all_delta[Scen == "s00"]
data_price_all_delta_s00[, `:=`(Scen = NULL)]

data_price_all_delta <- merge(data_price_all_delta, data_price_all_delta_s00, by = c("Source", "Year"))
data_price_all_delta[, delta := value.x - value.y]
data_price_all_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_price_all_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                       Scenario.2 = scenario_name_2[i],
                                                       Scenario.3 = scenario_name_3[i])]
}

plot_price_all_delta <- ggplot(data = data_price_all_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Source) +
  ylab("Difference in energy price (2016$/MMBtu)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_price_all_delta.pdf", width = 6, height = 5)
print(plot_price_all_delta)
dev.off()

png("sm_fig_price_all_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_price_all_delta)
dev.off()

## check the differenes
data_price_all_check <- data_price_all_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Source, delta)]
data_price_all_check <- dcast(data_price_all_check, Source ~ Scen, value.var = "delta")


# plot: average electricity price (delta) by sector ---------------------------------------------------------------------------
data_elec_price_sec <- data_model_main[TableNumber == 8
                                       & DaType == "price"
                                       & Gunits == "2016 cents/kWh"
                                       & Source == "end use"
                                       & Geogr == "united states", ]
data_elec_price_sec[, Sector := capitalize(Sector)]

data_elec_price_sec_delta <- data_elec_price_sec[Scen != "s01", .(Scen, Sector, Year, value)]
data_elec_price_sec_delta_s00 <- data_elec_price_sec_delta[Scen == "s00"]
data_elec_price_sec_delta_s00[, `:=`(Scen = NULL)]

data_elec_price_sec_delta <- merge(data_elec_price_sec_delta, data_elec_price_sec_delta_s00, by = c("Sector", "Year"))
data_elec_price_sec_delta[, delta := value.x - value.y]
data_elec_price_sec_delta[, `:=`(value.x = NULL, value.y = NULL)]

### add proper scenario names
for(i in 1:length(scenario)){
  data_elec_price_sec_delta[Scen == paste0("s0", i-1), `:=`(Scenario.1 = scenario_name_1[i],
                                                            Scenario.2 = scenario_name_2[i],
                                                            Scenario.3 = scenario_name_3[i])]
}

plot_elec_price_sec_delta <- ggplot(data = data_elec_price_sec_delta, mapping = aes(x = Year, y = delta, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ Sector) +
  ylab("Difference in electricity price (2016 cents/kWh)") +
  theme(legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.box = "vertical",
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        axis.title=element_text(size=9),
        axis.text = element_text(size=8),
        plot.title = element_blank(),
        strip.text = element_text(size=8)) +
  guides(colour = guide_legend(title="Scenario", byrow = TRUE, nrow = 1, order = 1),
         linetype = guide_legend(title="Carbon Pricing", byrow = TRUE, nrow = 1, order = 2)) +
  scale_color_npg()

pdf("sm_fig_elec_price_sec_delta.pdf", width = 6, height = 5)
print(plot_elec_price_sec_delta)
dev.off()

png("sm_fig_elec_price_sec_delta.png", width = 6, height = 5, units = "in", res = 500)
print(plot_elec_price_sec_delta)
dev.off()

## check the differenes
data_elec_price_sec_check <- data_elec_price_sec_delta[Year == 2050 & Scen %in% c("s02", "s04"), .(Scen, Sector, delta)]
data_elec_price_sec_check <- dcast(data_elec_price_sec_check, Sector ~ Scen, value.var = "delta")


# check results-----------------------------------------------------------
data_res_check <- data_model_main[TableNumber == 4
                                  & DaType == "consumption"
                                  & SubSec == "delivered energy"
                                  & Source == "electricity"
                                  & Geogr == "united states", .(Scen, Year, value)]
# data_res_check <- data_model_main[TableNumber == 4
#                                   & DaType == "consumption"
#                                   & SubSec == ""
#                                   & Source == "total energy use by end use"
#                                   & Geogr == "united states", .(Scen, Year, value)]

data_res_check <- data_res_check[Year == 2050]
data_res_check[Scen == "s02", value] - data_res_check[Scen == "s00", value] + 
  data_res_check[Scen == "s03", value] - data_res_check[Scen == "s00", value]
data_res_check[Scen == "s04", value] - data_res_check[Scen == "s00", value]

data_com_check <- data_model_main[TableNumber == 5
                                  & DaType == "consumption"
                                  & SubSrc == "delivered energy"
                                  & Source == "purchased electricity"
                                  & Geogr == "united states", .(Scen, Year, value)]
# data_com_check <- data_model_main[TableNumber == 5
#                                   & DaType == "consumption"
#                                   & SubSrc == ""
#                                   & Source == "total energy use by end use"
#                                   & Geogr == "united states", .(Scen, Year, value)]
data_com_check <- data_com_check[Year == 2050]
data_com_check[Scen == "s02", value] - data_com_check[Scen == "s00", value] + 
  data_com_check[Scen == "s03", value] - data_com_check[Scen == "s00", value]
data_com_check[Scen == "s04", value] - data_com_check[Scen == "s00", value]

data_ind_check <- data_model_main[TableNumber == 6
                                  & DaType == "consumption"
                                  & SubSec == ""
                                  & Source == "purchased electricity"
                                  & Geogr == "united states", .(Scen, Year, value)]
# data_ind_check <- data_model_main[TableNumber == 6
#                                   & DaType == "consumption"
#                                   & SubSec == ""
#                                   & Source == "total"
#                                   & Geogr == "united states", .(Scen, Year, value)]
data_ind_check <- data_ind_check[Year == 2050]
data_ind_check[Scen == "s02", value] - data_ind_check[Scen == "s00", value] + 
  data_ind_check[Scen == "s03", value] - data_ind_check[Scen == "s00", value]
data_ind_check[Scen == "s04", value] - data_ind_check[Scen == "s00", value]


data_co2_check <- data_model_main[TableNumber == 22
                                  & SubSec == ""
                                  & Source == ""
                                  & Geogr == "united states", .(Scen, Year, Sector, value)]
data_co2_check <- data_co2_check[Year == 2050]

data_co2_check[Scen == "s02" & Sector == "residential", value] - data_co2_check[Scen == "s00" & Sector == "residential", value] + 
  data_co2_check[Scen == "s03" & Sector == "residential", value] - data_co2_check[Scen == "s00" & Sector == "residential", value]
data_co2_check[Scen == "s04" & Sector == "residential", value] - data_co2_check[Scen == "s00" & Sector == "residential", value]

data_co2_check[Scen == "s02" & Sector == "commercial", value] - data_co2_check[Scen == "s00" & Sector == "commercial", value] + 
  data_co2_check[Scen == "s03" & Sector == "commercial", value] - data_co2_check[Scen == "s00" & Sector == "commercial", value]
data_co2_check[Scen == "s04" & Sector == "commercial", value] - data_co2_check[Scen == "s00" & Sector == "commercial", value]

data_co2_check[Scen == "s02" & Sector == "industrial", value] - data_co2_check[Scen == "s00" & Sector == "industrial", value] + 
  data_co2_check[Scen == "s03" & Sector == "industrial", value] - data_co2_check[Scen == "s00" & Sector == "industrial", value]
data_co2_check[Scen == "s04" & Sector == "industrial", value] - data_co2_check[Scen == "s00" & Sector == "industrial", value]

data_co2_check[Scen == "s02" & Sector == "transportation", value] - data_co2_check[Scen == "s00" & Sector == "transportation", value] + 
  data_co2_check[Scen == "s03" & Sector == "transportation", value] - data_co2_check[Scen == "s00" & Sector == "transportation", value]
data_co2_check[Scen == "s04" & Sector == "transportation", value] - data_co2_check[Scen == "s00" & Sector == "transportation", value]


