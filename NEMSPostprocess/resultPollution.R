# This file is used to post-process air pollution results
## Author: Pei Huang, 06/24/2019

# required packages------------------------------------
library(data.table)
library(ggplot2)
library(foreign)
library(stringr)
library(maps)
library(viridis)
library(mapproj)
library(ggrepel)
library(ggsci)    # scientific style color palettes
library(gridExtra)
library(rgdal)     # R wrapper around GDAL/OGR
library(scales)

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
                     na.strings = c("NA","NaN", ""),
                     fill = TRUE)
  
  ## process data into long format
  data_scen <- data_scen[, -c(1, dim(data_scen)[2], dim(data_scen)[2]-1), with = FALSE]
  col <- names(data_scen)[grepl("20", names(data_scen))]
  data_scen[, (col) := lapply(.SD, as.numeric), .SDcols = col]
  data_scen <- melt(data_scen, id.vars=names(data_scen)[!grepl("20", names(data_scen))], variable.name="Year")
  data_scen[, Year := as.numeric(as.character(Year))]
  
  data_scen[, RegionNum := as.integer(RegionNum)]
  
  data_scen[, `:=`(Scen = scenario[k])]
  
  data_model_all[[k]] <- data_scen
}
data_model_all <- rbindlist(data_model_all)

## update data to reference before 2016
data_model_ref <- data_model_all[Scen == "s00"]
data_model_ref <- data_model_ref[, `:=`(Scen = NULL)]
setnames(data_model_ref, old = c("value"), new = c("refval"))

data_model_all <- merge(data_model_all, data_model_ref,
                        by = c("Datekey", "TableNumber", "RowNum", "RegionNum",
                               "VarName", "GLabel", "Gunits", "RowFmt",
                               "DaType", "SubDat", "Sector", "SubSec",
                               "Source", "SubSrc", "Geogr", "Year"))
data_model_all[Year <= 2016, value := refval]
data_model_all[, refval := NULL]


# Emissions data from NEMS------------------------------------------------------------------
## data for mapping from NERC region to census region
mapping_nerc_census_raw <- read.csv("External_Data/mapping_nerc_census.csv", header = TRUE, stringsAsFactors = FALSE)
## fips mapping
mapping_fips_state_raw <- read.csv("External_Data/mapping_fips_state.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

## process nerc to census region mapping data weighted by electricity sales by sectors
mapping_nerc_census <- data.table(mapping_nerc_census_raw)
mapping_nerc_census <- melt(mapping_nerc_census, id.vars = names(mapping_nerc_census)[!grepl("CR", names(mapping_nerc_census))], variable.name = "RegionNum")
mapping_nerc_census[, NERC := as.numeric(gsub("NR","",mapping_nerc_census$NERC))]
mapping_nerc_census[, RegionNum := as.numeric(gsub("CR","",mapping_nerc_census$RegionNum))]

mapping_nerc_census <- mapping_nerc_census[, .(value = sum(value)), by = .(NERC,RegionNum)]
mapping_nerc_census[, proportion := value/sum(value), by = .(NERC)]

## SO2 and NOx for the power sector
data_plt_nems <- data_model_all[TableNumber == 62
                                & DaType == "emissions"
                                & SubDat %in% c("sulfur dioxide","nitrogen oxide")
                                & Geogr != "united states", c("Scen", "RegionNum","SubDat","Geogr","Year","value")]
setnames(data_plt_nems, old = "RegionNum", new = "NERC")

### merege the mapping data
data_plt_nems <- merge(data_plt_nems,
                       mapping_nerc_census[,c("RegionNum","NERC","proportion")],
                       by = "NERC",
                       all.x = T,
                       allow.cartesian = T)

### map from NERC to RegionNum
data_plt_nems <- data_plt_nems[, .(value = sum(value*proportion)), by = .(Scen,RegionNum,SubDat,Year)]
### convet from short ton to metric ton
data_plt_nems[, value := value*0.907185]

data_plt_nems[SubDat == "sulfur dioxide", SubDat := "SO2"]
data_plt_nems[SubDat == "nitrogen oxide", SubDat := "NOx"]

data_plt_nems[, Sector := "electric power"]

setnames(data_plt_nems, old = c("RegionNum", "SubDat", "Sector"), new = c("CensusID", "PLT", "SEC"))


# Emissions data from EPA NEI------------------------------------------------------------------
## Source: EPA 2014 data (https://www.epa.gov/air-emissions-inventories/2014-national-emissions-inventory-nei-data)
data_plt_nei_1 <- fread("External_Data/data_epa_nei_county_1.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
data_plt_nei_2 <- fread("External_Data/data_epa_nei_county_2.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
data_plt_nei_3 <- fread("External_Data/data_epa_nei_county_3.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
data_plt_nei_4 <- fread("External_Data/data_epa_nei_county_4.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

data_plt_nei <- rbind(data_plt_nei_1, data_plt_nei_2, data_plt_nei_3, data_plt_nei_4)

### add sector and pollutant columns
data_plt_nei[Sector %like% "Fuel Comb - Residential", SEC := "residential"]
data_plt_nei[Sector %like% "Fuel Comb - Comm/Institutional", SEC := "commercial"]
data_plt_nei[Sector %like% "Fuel Comb - Industrial Boilers", SEC := "industrial"]
data_plt_nei[Sector %like% "Fuel Comb - Electric Generation", SEC := "electric power"]
data_plt_nei[Sector %like% "Mobile", SEC := "transportation"]
data_plt_nei <- na.omit(data_plt_nei)

data_plt_nei[Pollutant %like% "Carbon Monoxide", PLT := "CO"]
data_plt_nei[Pollutant %like% "Sulfur Dioxide", PLT := "SO2"]
data_plt_nei[Pollutant %like% "Nitrogen Oxides", PLT := "NOx"]
data_plt_nei[Pollutant %like% "Volatile Organic Compounds", PLT := "VOC"]
data_plt_nei[Pollutant %like% "PM10", PLT := "PM10"]
data_plt_nei[Pollutant %like% "PM2.5", PLT := "PM2.5"]
data_plt_nei[Pollutant %like% "Ammonia", PLT := "NH3"]
data_plt_nei[Pollutant %like% "Lead", PLT := "Lead"]
data_plt_nei <- na.omit(data_plt_nei)

data_plt_nei[Sector %like% "non-Diesel", source := "gasoline"]
data_plt_nei[Sector %like% " Diesel", source := "diesel"]
data_plt_nei[Sector %like% "Locomotives", source := "diesel"]
data_plt_nei[Sector %like% "Commercial Marine Vessels", source := "diesel"]
data_plt_nei[Sector %like% "Aircraft", source := "jet fuel"]
data_plt_nei[Sector %like% "Gasoline", source := "gasoline"]

data_plt_nei[Sector %like% "Wood", source := "wood"]
data_plt_nei[Sector %like% "Natural Gas", source := "natural gas"]
data_plt_nei[Sector %like% "Oil", source := "oil"]
data_plt_nei[Sector %like% "Coal", source := "coal"]
# data_plt_nei[Sector %like% "Biomass", source := "biomass"]
# data_plt_nei[Sector %like% "Other", source := "other"]
data_plt_nei <- na.omit(data_plt_nei)

### add a column of long state name
mapping_state_census <- na.omit(unique(mapping_fips_state_raw[,c("StateShort","State","CensusID")]))
data_plt_nei <- merge(data_plt_nei, mapping_state_census, by.x = "Address", by.y = "StateShort")

### convert unit from short ton to million metric ton
data_plt_nei[, value := Emissions*0.907185/1e6]

### select relevant columns
data_plt_nei <- data_plt_nei[, .(CensusID, State, County, SEC, source, PLT, value)]


# Fule consumption data from NEMS------------------------------------------------------------------
### fuel consumption from various sectors from NEMS
data_cons_pwr <- data_model_all[TableNumber == 2
                                & Sector == "electric power"
                                & Source %in% c("liquid fuels subtotal", "natural gas", "steam coal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_pwr[Source == "liquid fuels subtotal", Source := "oil"]
data_cons_pwr[Source == "steam coal", Source := "coal"]

data_cons_rsd <- data_model_all[TableNumber == 2
                                & Sector == "residential"
                                & Source %in% c("liquid fuels subtotal", "natural gas", "renewable energy")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_rsd[Source == "liquid fuels subtotal", Source := "oil"]
data_cons_rsd[Source == "renewable energy", Source := "wood"]

data_cons_com <- data_model_all[TableNumber == 2
                                & Sector == "commercial"
                                & Source %in% c("liquid fuels subtotal", "natural gas", "coal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_com[Source == "liquid fuels subtotal", Source := "oil"]

data_cons_ind <- data_model_all[TableNumber == 2
                                & Sector == "industrial"
                                & Source %in% c("liquid fuels subtotal", "natural gas subtotal", "coal subtotal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_ind[Source == "liquid fuels subtotal", Source := "oil"]
data_cons_ind[Source == "natural gas subtotal", Source := "natural gas"]
data_cons_ind[Source == "coal subtotal", Source := "coal"]

data_cons_trn <- data_model_all[TableNumber == 2
                                & Sector == "transportation"
                                & Source %in% c("motor gasoline", "jet fuel", "liquid fuels subtotal")
                                & Geogr != "united states", c("Scen","RegionNum","Sector","Source","Year","value")]
data_cons_trn[Source == "liquid fuels subtotal", Source := "diesel"]
data_cons_trn[Source == "motor gasoline", Source := "gasoline"]

### merge data from various sectors
data_cons <- rbind(data_cons_pwr, data_cons_rsd, data_cons_com, data_cons_ind, data_cons_trn)

data_cons_2014 <- data_cons[which(data_cons$Year == 2014),]
data_cons_2014 <- data_cons_2014[value > 0]
data_cons <- merge(data_cons, data_cons_2014[,c("Scen","RegionNum","Sector","Source","value")],
                   by = c("Scen","Sector","Source","RegionNum"))
data_cons[, scale := value.x/value.y]
data_cons <- data_cons[,c("Scen","RegionNum","Sector","Source","Year","scale")]


# Emissions extrapolation------------------------------------------------------------------
data_nei_rgn <- data_plt_nei[, .(value = sum(value)),
                             by = .(CensusID, SEC, source, PLT)]

### merge the NEI pollutant data and the NEMS consumption data
data_merge_nei <- merge(data_nei_rgn,
                        data_cons,
                        by.x = c("source","CensusID","SEC"),
                        by.y = c("Source","RegionNum","Sector"),
                        allow.cartesian = TRUE)

### calculate the scaled emissions
data_merge_nei[, value := value*scale]
### sum over source
data_merge_nei <- data_merge_nei[, .(value = sum(value)), by = .(Scen,CensusID,SEC,PLT,Year)]

### remove SO2 and NOx from the electric power sector because it's directly from NEMS
data_merge_nei <- data_merge_nei[!(SEC == "electric power" & PLT %in% c("SO2", "NOx"))]

### assume 1% and 1.5% annual decrease of marginal emission of per unit of energy consumption
decay.len <- c(0, seq(0, (2050-2014), by = 1))
data_year_decay <- data.table(Year = c(2013:2050), Decay.1 = 1-decay.len*0.01, Decay.2= 1-decay.len*0.015)

data_merge_nei <- merge(data_merge_nei, data_year_decay, by = "Year")
data_merge_nei[, `:=`(value.decay.1 = value*Decay.1,
                      value.decay.2 = value*Decay.2)]
data_merge_nei[, `:=`(Decay.1 = NULL, Decay.2 = NULL)]

### rbind the SO2 and NOx from NEMS
data_plt_nems[, `:=`(value.decay.1 = value,
                     value.decay.2 = value)]
data_merge_rgn <- rbind(data_merge_nei, data_plt_nems)

### add region names
data_merge_rgn[data_merge_rgn$CensusID == 1, RNAME := "new england"]
data_merge_rgn[data_merge_rgn$CensusID == 2, RNAME := "middle atlantic"]
data_merge_rgn[data_merge_rgn$CensusID == 3, RNAME := "east north central"]
data_merge_rgn[data_merge_rgn$CensusID == 4, RNAME := "west north central"]
data_merge_rgn[data_merge_rgn$CensusID == 5, RNAME := "south atlantic"]
data_merge_rgn[data_merge_rgn$CensusID == 6, RNAME := "east south central"]
data_merge_rgn[data_merge_rgn$CensusID == 7, RNAME := "west south central"]
data_merge_rgn[data_merge_rgn$CensusID == 8, RNAME := "mountain"]
data_merge_rgn[data_merge_rgn$CensusID == 9, RNAME := "pacific"]

### add proper names for pollutants for printing
data_merge_rgn[data_merge_rgn$PLT == "CO", PLT.Name := "CO"]
data_merge_rgn[data_merge_rgn$PLT == "VOC", PLT.Name := "VOC"]
data_merge_rgn[data_merge_rgn$PLT == "PM10", PLT.Name := "PM[10]"]
data_merge_rgn[data_merge_rgn$PLT == "PM2.5", PLT.Name := "PM[2.5]"]
data_merge_rgn[data_merge_rgn$PLT == "NH3", PLT.Name := "NH[3]"]
data_merge_rgn[data_merge_rgn$PLT == "Lead", PLT.Name := "Lead"]
data_merge_rgn[data_merge_rgn$PLT == "NOx", PLT.Name := "NO[x]"]
data_merge_rgn[data_merge_rgn$PLT == "SO2", PLT.Name := "SO[2]"]

### reorder pollutants
data_merge_rgn[, PLT := factor(PLT, levels = c("PM10", "PM2.5", "NOx", "CO", "SO2", "VOC", "NH3", "Lead"))]
data_merge_rgn[, PLT.Name := factor(PLT.Name, levels = c("PM[10]", "PM[2.5]", "NO[x]", "CO", "SO[2]", "VOC", "NH[3]", "Lead"))]

# Health benefits: avoided human death-----------------------------------------------------------------------
## county names
county_fips <- fread("External_Data/AP3/fips_apeep_updated.csv")
county_fips <- county_fips[, .(fips)]

## facility names
tall1_fac <- fread("External_Data/AP3/tall_facilities_ap3.csv")
tall1_fac <- tall1_fac[, .(EIS, FIPS)]
tall1_fac[, id := seq_len(.N), by = .(EIS, FIPS)]
setnames(tall1_fac, old = 'FIPS', new = 'fips')

tall2_fac <- fread("External_Data/AP3/tall2_facilities_ap3.csv")
tall2_fac <- tall2_fac[, .(EIS, FIPS)]
tall2_fac[, id := seq_len(.N), by = .(EIS, FIPS)]
setnames(tall2_fac, old = 'FIPS', new = 'fips')

## county population data
data_county_pop <- fread("External_Data/AP3/pop_2014.csv")
data_county_pop <- apply(data_county_pop, 1, sum)
data_county_pop <- data.table(county_fips, pop = data_county_pop)

## county emission data
data_emi_area <- fread("External_Data/AP3/area_sources_2014.csv")
data_emi_area[, V6 := V6 + V7]
data_emi_area <- data_emi_area[, .(V1, V2, V4, V5, V6)]
data_emi_area <- cbind(county_fips, data_emi_area)
data_emi_area[, `:=`(Source = "area",
                     EIS = 9999999,
                     id = 1)]

data_emi_low <- fread("External_Data/AP3/low_2014.csv")
data_emi_low <- data_emi_low[, .(V1, V2, V4, V5, V6)]
data_emi_low <- cbind(county_fips, data_emi_low)
data_emi_low[, `:=`(Source = "low",
                    EIS = 9999999,
                    id = 1)]

data_emi_med <- fread("External_Data/AP3/medium_2014.csv")
data_emi_med <- data_emi_med[, .(V1, V2, V4, V5, V6)]
data_emi_med <- cbind(county_fips, data_emi_med)
data_emi_med[, `:=`(Source = "medium",
                    EIS = 9999999,
                    id = 1)]

data_emi_tall1 <- fread("External_Data/AP3/tall_2014.csv")
data_emi_tall1 <- data_emi_tall1[, .(V1, V2, V4, V5, V6)]
data_emi_tall1 <- cbind(tall1_fac, data_emi_tall1)
data_emi_tall1 <- na.omit(data_emi_tall1)
data_emi_tall1[, Source := "tall1"]

data_emi_tall2 <- fread("External_Data/AP3/tall2_2014.csv")
data_emi_tall2 <- data_emi_tall2[, .(V1, V2, V4, V5, V6)]
data_emi_tall2 <- cbind(tall2_fac, data_emi_tall2)
data_emi_tall2 <- na.omit(data_emi_tall2)
data_emi_tall2[, Source := "tall2"]

data_emi_ap3 <- rbind(data_emi_area, data_emi_low, data_emi_med, data_emi_tall1, data_emi_tall2)
data_emi_ap3 <- melt.data.table(data_emi_ap3, id.vars = c("fips", "EIS", "Source", "id"), variable.name = "PLT", value.name = "emission")
levels(data_emi_ap3$PLT) <- c("NH3", "NOx", "PM2.5", "SO2", "VOC")

## marginal damages for various sources ($ per short ton)
### the data is based on the Nick Muller's AP3 (2019)
### VSL is assumed $9,186,210
data_md_area <- fread("External_Data/AP3/md_A_2014_DR-Krewski_VRMR-9186210.csv")
data_md_area <- cbind(county_fips, data_md_area)
data_md_area[, `:=`(Source = "area",
                    EIS = 9999999,
                    id = 1)]

data_md_low <- fread("External_Data/AP3/md_L_2014_DR-Krewski_VRMR-9186210.csv")
data_md_low <- cbind(county_fips, data_md_low)
data_md_low[, `:=`(Source = "low",
                   EIS = 9999999,
                   id = 1)]

data_md_med <- fread("External_Data/AP3/md_M_2014_DR-Krewski_VRMR-9186210.csv")
data_md_med <- cbind(county_fips, data_md_med)
data_md_med[, `:=`(Source = "medium",
                   EIS = 9999999,
                   id = 1)]

data_md_tall1 <- fread("External_Data/AP3/md_T_2014_DR-Krewski_VRMR-9186210.csv")
data_md_tall1 <- cbind(tall1_fac, data_md_tall1)
data_md_tall1 <- na.omit(data_md_tall1)
data_md_tall1[, Source := "tall1"]

data_md_tall2 <- fread("External_Data/AP3/md_T2_2014_DR-Krewski_VRMR-9186210.csv")
data_md_tall2 <- cbind(tall2_fac, data_md_tall2)
data_md_tall2 <- na.omit(data_md_tall2)
data_md_tall2[, Source := "tall2"]

data_md <- rbind(data_md_area, data_md_low, data_md_med, data_md_tall1, data_md_tall2)
data_md <- melt.data.table(data_md, id.vars = c("fips", "EIS", "Source", "id"), variable.name = "PLT")
levels(data_md$PLT) <- c("NH3", "NOx", "PM2.5", "SO2", "VOC")

# ## calculate average across sources
# data_md <- data_md[, .(value = mean(value)), by = .(fips, PLT)]

## convert from $/short ton to $/metric ton
data_md[, value := value*1.10231]

## calculate the number of deaths by dividing it with VSL
data_md[, death := value/9186210]
data_md[, value := NULL]

# ## merge with population data
# data_md <- merge(data_md, data_county_pop, by = "fips")

# merge with emissions data
data_md <- merge(data_md, data_emi_ap3, by = c("fips", "EIS", "Source", "PLT", "id"))
# data_dup <- data_md[(duplicated(data_md[,c("fips", "EIS", "Source", "PLT", "id"), with = F])
#                           | duplicated(data_md[,c("fips", "EIS", "Source", "PLT", "id"), with = F], fromLast = TRUE)),]

## process the marginal damage data and map them into census regions level
data_md[, fips := str_pad(fips, 5, pad = "0")]
data_md[, StateID := as.integer(substr(fips, 1, 2))]

## fips to county mapping data
mapping_fips_state <- data.table(unique(mapping_fips_state_raw[,c("StateID","CensusID")]))
data_md <- merge(data_md, mapping_fips_state, by = "StateID", all.x = TRUE)
data_md <- data_md[, .(death = weighted.mean(death, emission)),
                   by = .(CensusID, PLT)]
# data_md <- data_md[, .(death = mean(death)),
#                    by = .(CensusID, PLT)]

## assume pm10 has the same MD of PM2.5
# data_md_pm10 <- data_md[PLT == "PM2.5"]
# data_md_pm10[, PLT := "PM10"]

# data_md <- rbind(data_md, data_md_pm10)

## merge marginal damage data with emissions data
data_mortality <- merge(data_merge_rgn, data_md, by = c("CensusID", "PLT"))
data_mortality[, total.death := as.integer(value*death*1e6)]
data_mortality[, total.death.1 := as.integer(value.decay.1*death*1e6)]
data_mortality[, total.death.2 := as.integer(value.decay.2*death*1e6)]

## sum over sectors and pollutants
data_mortality_base <- data_mortality[Scen %in% scenario[c(1:5)]]
data_mortality_base <- data_mortality_base[, .(value = sum(total.death),
                                               value.1 = sum(total.death.1),
                                               value.2 = sum(total.death.2)),
                                           by = .(Scen, CensusID, RNAME, Year)]

## calculate changes compared to the reference
data_mortality_base <- merge(data_mortality_base,
                             data_mortality_base[Scen == scenario[1]],
                             by = c("Year", "CensusID", "RNAME"))
data_mortality_base[, change := -(value.x - value.y)]
data_mortality_base[, change.1 := -(value.1.x - value.1.y)]
data_mortality_base[, change.2 := -(value.2.x - value.2.y)]

data_mortality_base <- data_mortality_base[Scen.x != scenario[1]]
data_mortality_base <- data_mortality_base[, `:=`(value.x = NULL, Scen.y = NULL, value.y = NULL,
                                                  value.1.x = NULL, value.1.y = NULL, value.2.x = NULL, value.2.y = NULL)]

## output results for report (from 2050)
data_mortality_output <- data_mortality_base[Year ==2050]
data_mortality_output <- data_mortality_output[, .(change = round(mean(change), 0),
                                                   change.1 = round(mean(change.1), 0),
                                                   change.2 = round(mean(change.2), 0)),
                                               by = .(CensusID, RNAME, Scen.x)]

## save data into a csv file
data_mortality_table <- dcast.data.table(data_mortality_output, CensusID + RNAME ~ Scen.x, value.var = "change")
data_mortality_table_sum <- t(apply(data_mortality_table[,-c(1:2)], 2, sum))
data_mortality_table_sum <- cbind(11, "united states", data_mortality_table_sum)
data_mortality_table <- rbind(data_mortality_table, data_mortality_table_sum, use.names=FALSE)
fwrite(data_mortality_table, "tab_mortality.csv", row.names = FALSE)

data_mortality_table_1 <- dcast.data.table(data_mortality_output, CensusID + RNAME ~ Scen.x, value.var = "change.1")
data_mortality_table_1_sum <- t(apply(data_mortality_table_1[,-c(1:2)], 2, sum))
data_mortality_table_1_sum <- cbind(11, "united states", data_mortality_table_1_sum)
data_mortality_table_1 <- rbind(data_mortality_table_1, data_mortality_table_1_sum, use.names=FALSE)
fwrite(data_mortality_table_1, "tab_mortality_sens_decay_1.csv", row.names = FALSE)

data_mortality_table_2 <- dcast.data.table(data_mortality_output, CensusID + RNAME ~ Scen.x, value.var = "change.2")
data_mortality_table_2_sum <- t(apply(data_mortality_table_2[,-c(1:2)], 2, sum))
data_mortality_table_2_sum <- cbind(11, "united states", data_mortality_table_2_sum)
data_mortality_table_2 <- rbind(data_mortality_table_2, data_mortality_table_2_sum, use.names=FALSE)
fwrite(data_mortality_table_2, "tab_mortality_sens_decay_1.5.csv", row.names = FALSE)


# calculate the saved deaths comparing "Low Renewables Cost" and "Optimistic EE & Low Renewables Cost"
## sum over sectors and pollutants
data_mortality_lrc <- data_mortality[Scen %in% scenario[c(8:9)]]
data_mortality_lrc <- data_mortality_lrc[, .(value = sum(total.death)),
                                         by = .(Scen, CensusID, RNAME, Year)]

## calculate changes compared to the reference
data_mortality_lrc <- merge(data_mortality_lrc,
                            data_mortality_lrc[Scen == scenario[8]],
                            by = c("Year", "CensusID", "RNAME"))
data_mortality_lrc[, change := -(value.x - value.y)]

data_mortality_lrc <- data_mortality_lrc[Scen.x != scenario[8]]
data_mortality_lrc <- data_mortality_lrc[, `:=`(value.x = NULL, Scen.y = NULL, value.y = NULL)]

## output results for report (year 2050)
data_mortality_output_lrc <- data_mortality_lrc[Year == 2050]
data_mortality_output_lrc <- data_mortality_output_lrc[, .(change = round(mean(change), 0)),
                                                       by = .(CensusID, RNAME, Scen.x)]

## save data into a csv file
data_mortality_table_lrc <- dcast.data.table(data_mortality_output_lrc, CensusID + RNAME ~ Scen.x, value.var = "change")
data_mortality_table_lrc_sum <- t(apply(data_mortality_table_lrc[,-c(1:2)], 2, sum))
data_mortality_table_lrc_sum <- cbind(11, "united states", data_mortality_table_lrc_sum)
data_mortality_table_lrc <- rbind(data_mortality_table_lrc, data_mortality_table_lrc_sum, use.names=FALSE)
fwrite(data_mortality_table_lrc, "tab_mortality_sens_LowRnwCst.csv", row.names = FALSE)


# Calculate the population proportion in the regions in Buonocore et al Nature Climate Change paper-----------------------------------
## compared to the census divisions of "Middle Atlantic" and "east north central"
## then applies to the total damages in the two regions
county_pop <- fread("External_Data/county_population2010_usa.csv", header = TRUE, strip.white = TRUE)
county_pop[, B_paper := as.factor(B_paper)]
fips_state <- data.table(mapping_fips_state_raw)

county_census_pop <- merge(fips_state[, .(StateID, CountyID, CensusID)], county_pop[, .(STATE, COUNTY, CENSUS2010POP, B_paper)], by.x = c("StateID", "CountyID"), by.y = c("STATE", "COUNTY"))
county_census_pop <- county_census_pop[CensusID %in% c(2, 3)]

## calculate the population proportion of the counties in Buonocore et al paper
census_bpaper_pop <- county_census_pop[, .(pop = sum(CENSUS2010POP)), by = .(B_paper)]
census_bpaper_pop[, prop := pop/sum(pop)]

data_mortality_bpaper <- data_mortality_output[CensusID %in% c(2, 3)]
data_mortality_bpaper <- data_mortality_bpaper[, .(change = sum(change)),
                                               by = Scen.x]
data_mortality_bpaper[, change := change * census_bpaper_pop[B_paper == 1, prop]]

### final results in billion dollars in the sub-regions
data_mortality_bpaper[, change.dollar := change * 7.58/1000]

## calculate climate benefits from CO2 emissions
data_co2 <- data_model_all[TableNumber == 2
                           & DaType == "emissions"
                           & Geogr != "united states",
                           .(Scen, RegionNum, Geogr, Year, value)]
data_co2 <- data_co2[Scen %in% scenario[c(1:5)] & Year %in% c(2050)]

data_co2_change <- merge(data_co2,
                         data_co2[Scen == scenario[1]],
                         by = c("Year", "RegionNum", "Geogr"))
data_co2_change[, change := -(value.x - value.y)]

data_co2_change <- data_co2_change[Scen.x != scenario[1]]
data_co2_change <- data_co2_change[, `:=`(value.x = NULL, Scen.y = NULL, value.y = NULL)]

data_co2_change <- data_co2_change[, .(change = mean(change)),
                                   by = .(RegionNum, Geogr, Scen.x)]

data_co2_change <- data_co2_change[RegionNum %in% c(2, 3)]
data_co2_change <- data_co2_change[, .(change = sum(change)),
                                   by = Scen.x]

data_co2_change[, change := change * census_bpaper_pop[B_paper == 1, prop]]

### final results in billion dollars in the sub-regions
#### assuming US$43/metric ton CO2 emissions (the same in Buonocore et al)
data_co2_change[, change.dollar := change * 43/1000]


# plot: pollution results over time--------------------------------------------------------------------------------------
data_merge_rgn_sub <- data_merge_rgn[Scen %in% scenario[c(1:5)]]
## add proper scenario names
for(i in 1:length(unique(data_merge_rgn$Scen))){
  data_merge_rgn_sub[Scen == unique(data_merge_rgn$Scen)[i], `:=`(Scenario.1 = scenario_name_1[i],
                                                                  Scenario.2 = scenario_name_2[i],
                                                                  Scenario.3 = scenario_name_3[i])]
}

data_plt_rgn <- data_merge_rgn_sub[, .(value = sum(value),
                                       value.1 = sum(value.decay.1),
                                       value.2 = sum(value.decay.2)),
                                   by = .(Scen, Scenario.1, Scenario.2, Scenario.3, Year, CensusID, RNAME, PLT, PLT.Name)]
data_plt_us_sec <- data_merge_rgn_sub[, .(value = sum(value),
                                          value.1 = sum(value.decay.1),
                                          value.2 = sum(value.decay.2)),
                                      by = .(Scen, Scenario.1, Scenario.2, Scenario.3, Year, SEC, PLT, PLT.Name)]
data_plt_us <- data_plt_us_sec[, .(value = sum(value),
                                   value.1 = sum(value.1),
                                   value.2 = sum(value.2)),
                               by = .(Scen, Scenario.1, Scenario.2, Scenario.3, Year, PLT, PLT.Name)]

## print US total
plot_us_total <- ggplot(data = data_plt_us, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
  ggtitle("all sectors: united states") +
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

pdf("fig_plt.pdf", width = 6, height = 6)
print(plot_us_total)
dev.off()

png("fig_plt.png", width = 6, height = 6, units = "in", res = 500)
print(plot_us_total)
dev.off()

### generate output data
data_plt_us_out <- data_plt_us[, .(Scen, Year, PLT, value)]
data_plt_us_out <- merge(data_plt_us_out,
                         data_plt_us_out[Scen == scenario[1]],
                         by = c("PLT", "Year"))

data_plt_us_out <- data_plt_us_out[Scen.x != scenario[1]]
data_plt_us_out[, change := value.x - value.y]
data_plt_us_out[, change.pct := (value.x - value.y)/value.y]
data_plt_us_out <- dcast.data.table(data_plt_us_out, PLT + Year ~ Scen.x, value.var = c("change", "change.pct"))

fwrite(data_plt_us_out, "fig_plt_data.csv", row.names = FALSE)


## print US total with a decreasing rate of marginal emission of energy consumption
plot_us_total_1 <- ggplot(data = data_plt_us, mapping = aes(x = Year, y = value.1, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
  ggtitle("all sectors: united states") +
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

pdf("sm_fig_sens_plt_decay_1.pdf", width = 6, height = 6)
print(plot_us_total_1)
dev.off()

png("sm_fig_sens_plt_decay_1.png", width = 6, height = 6, units = "in", res = 500)
print(plot_us_total_1)
dev.off()

## print US total with a decreasing rate of marginal emission of energy consumption
plot_us_total_2 <- ggplot(data = data_plt_us, mapping = aes(x = Year, y = value.2, group = Scen)) +
  geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
  geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
  facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
  ylab("Pollutant emissions (MMT)") +
  ggtitle("all sectors: united states") +
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

pdf("sm_fig_sens_plt_decay_1.5.pdf", width = 6, height = 6)
print(plot_us_total_2)
dev.off()

png("sm_fig_sens_plt_decay_1.5.png", width = 6, height = 6, units = "in", res = 500)
print(plot_us_total_2)
dev.off()


## print sector results (excluding transportation)
data_plt_us_sec <- data_plt_us_sec[SEC != "transportation"]
for(i in 1:length(unique(data_plt_us_sec[,SEC]))){
  sec_id <- unique(data_plt_us_sec[,SEC])[i]
  
  plot_sec <- ggplot(data = data_plt_us_sec[SEC == sec_id], mapping = aes(x = Year, y = value, group = Scen)) +
    geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
    geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
    facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
    ylab("Pollutant emissions (MMT)") +
    ggtitle(paste0(unique(data_plt_us_sec[SEC == sec_id, SEC]), ": united states")) +
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
  
  pdf(paste0("sm_fig_plt_sec_", sec_id,".pdf"), width = 6, height = 6)
  print(plot_sec)
  dev.off()
  
  png(paste0("sm_fig_plt_sec_", sec_id,".png"), width = 6, height = 6, units = "in", res = 500)
  print(plot_sec)
  dev.off()
}


#### print region results
for(i in 1:length(unique(data_plt_rgn[,CensusID]))){
  rgn_id <- unique(data_plt_rgn[,CensusID])[i]
  
  plot_rgn <- ggplot(data = data_plt_rgn[CensusID == rgn_id], mapping = aes(x = Year, y = value, group = Scen)) +
    geom_line(aes(color = Scenario.1, linetype = Scenario.2)) + 
    geom_vline(xintercept = 2016, linetype="dotted", color = "grey70", size = 0.7) +
    facet_wrap(~ PLT.Name, scale = "free", labeller = label_parsed) +
    ylab("Pollutant emissions (MMT)") +
    ggtitle(paste0("all sectors: ", unique(data_plt_rgn[CensusID == rgn_id, RNAME]))) +
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
  # print(plot_rgn)
}


# plot: pollution in census region level in map--------------------------------------------------------------------------------------
## prepare data for calculating pollution changes in a year
data_plt_census <- data_merge_rgn_sub[Year == 2050]
data_plt_census <- data_plt_census[, .(value = sum(value)),
                                   by = .(Scen, Year, CensusID, PLT, PLT.Name)]
data_plt_census <- merge(data_plt_census,
                         data_plt_census[Scen == scenario[1]],
                         by = c("Year", "CensusID", "PLT", "PLT.Name"))
data_plt_census[, change := value.x - value.y]
data_plt_census[, change.pct := (value.x - value.y)/value.y*100]

data_plt_census <- data_plt_census[Scen.x != scenario[1]]
data_plt_census <- data_plt_census[!is.na(change.pct)]

data_plt_census <- data_plt_census[, `:=`(value.x = NULL, Scen.y = NULL, value.y = NULL)]

mapping_census_state <- unique(data.table(mapping_fips_state_raw)[, .(CensusID, State)])

data_plt_census <- merge(data_plt_census,
                         mapping_census_state,
                         by = "CensusID",
                         allow.cartesian = TRUE)
data_plt_census[, `:=`(State = tolower(State))]

## merge with map polygon data
states <- data.table(map_data("state"))
data_plt_map <- merge(states,
                      data_plt_census,
                      by.x = c("region"),
                      by.y = c("State"),
                      sort = FALSE,
                      allow.cartesian=TRUE,
                      all.y = TRUE)
data_plt_map <- data_plt_map[!(region %in% c("alaska","hawaii"))]
data_plt_map <- data_plt_map[order(data_plt_map$order),]

### scale unit for lead from million metric tons to metric tons
data_plt_map[PLT == "Lead", change := change*1e6]

### add full scenario names
data_plt_map[Scen.x == scenario[2], Scenario.Label := scenario_name_1[2]]
data_plt_map[Scen.x == scenario[3], Scenario.Label := scenario_name_1[3]]
data_plt_map[Scen.x == scenario[4], Scenario.Label := "Carbon Pricing"]
data_plt_map[Scen.x == scenario[5], Scenario.Label := paste(scenario_name_1[3], "Carbon Pricing", sep = " & ")]

### prepare shapefile data for the US census divisions
#### First read in the shapefile, using the path to the shapefile and the shapefile name minus the extension as arguments
cd_shapefile <- readOGR("External_Data/census_division_shapefiles", "cb_2018_us_division_20m")

#### Next the shapefile has to be converted to a data.table for use in ggplot2
shapefile_df <- fortify(cd_shapefile)
shapefile_df <- data.table(shapefile_df)

#### Include only the contiguous 48 states
shapefile_df <- shapefile_df[(long > -125 & long < -65)
                             & (lat > 24 & lat < 50)]

for(i in 1:length(unique(data_plt_map[,PLT]))){
  plt_id <- unique(data_plt_map[,PLT.Name])[i]
  data_plt_map_sub <- data_plt_map[PLT.Name == plt_id]
  
  ### absolute change
  plot_map_abs <- ggplot() +
    geom_polygon(data = data_plt_map_sub,
                 aes(long, lat, group = group, fill = change),
                 color = NA) + # data containing modeling results
    coord_map("albers", at0 = 45.5, lat1 = 29.5) +
    # labs(title = parse(text = paste0(toupper(letters)[i], ":~", plt_id))) +
    labs(title = paste0(toupper(letters)[i], ": ", unique(data_plt_map_sub[,PLT]))) +
    geom_polygon(data = shapefile_df,
                 aes(x = long, y = lat, group = group),
                 color = "grey40",
                 size = 0.2,
                 fill = NA) + # add a layer of census divisions
    facet_wrap(~Scenario.Label, nrow = 1) +
    theme(legend.position = 'right',
          plot.title = element_text(size=8),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-2,-2,-2,-2),
          legend.key.size =  unit(0.1, "in"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          strip.text = element_text(size=6.5),
          legend.box = "vertical",
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + 
    scale_fill_binned(name = ifelse(plt_id == "Lead", "Ton", "MMT"),
                      type = "viridis", alpha = .5,
                      labels = scales::number_format(accuracy = 0.001))
  assign(paste0("plot_map_abs_", i), plot_map_abs)
  
  ### percentage change
  plot_map_pct <- ggplot() +
    geom_polygon(data = data_plt_map_sub,
                 aes(long, lat, group = group, fill = change.pct),
                 color = NA) + # data containing modeling results
    coord_map("albers", at0 = 45.5, lat1 = 29.5) +
    # labs(title = parse(text = paste0(toupper(letters)[i], ":~", plt_id))) +
    labs(title = paste0(toupper(letters)[i], ": ", unique(data_plt_map_sub[,PLT]))) +
    geom_polygon(data = shapefile_df,
                 aes(x = long, y = lat, group = group),
                 color = "grey40",
                 size = 0.2,
                 fill = NA) + # add a layer of census divisions
    facet_wrap(~Scenario.Label, nrow = 1) +
    theme(legend.position = 'right',
          plot.title = element_text(size=8),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-2,-2,-2,-2),
          legend.key.size =  unit(0.1, "in"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA),
          strip.text = element_text(size=6.5),
          legend.box = "vertical",
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_fill_binned(name = "Pct",
                      type = "viridis", alpha = .5,
                      labels = scales::number_format(accuracy = 0.1))
  assign(paste0("plot_map_pct_", i), plot_map_pct)
}

## output to PDF
pdf("sm_fig_plt_change_rgn.pdf", width = 6, height = 5)
grid.arrange(plot_map_abs_1, plot_map_abs_2, plot_map_abs_3, plot_map_abs_4, nrow=4)
grid.arrange(plot_map_abs_5, plot_map_abs_6, plot_map_abs_7, plot_map_abs_8, nrow=4)
dev.off()

pdf("sm_fig_plt_pctchange_rgn.pdf", width = 6, height = 5)
grid.arrange(plot_map_pct_1, plot_map_pct_2, plot_map_pct_3, plot_map_pct_4, nrow=4)
grid.arrange(plot_map_pct_5, plot_map_pct_6, plot_map_pct_7, plot_map_pct_8, nrow=4)
dev.off()

png("sm_fig_plt_change_rgn_1.png", width = 6, height = 5, units = "in", res = 500)
grid.arrange(plot_map_abs_1, plot_map_abs_2, plot_map_abs_3, plot_map_abs_4, nrow=4)
dev.off()

png("sm_fig_plt_change_rgn_2.png", width = 6, height = 5, units = "in", res = 500)
grid.arrange(plot_map_abs_5, plot_map_abs_6, plot_map_abs_7, plot_map_abs_8, nrow=4)
dev.off()

png("sm_fig_plt_pctchange_rgn_1.png", width = 6, height = 5, units = "in", res = 500)
grid.arrange(plot_map_pct_1, plot_map_pct_2, plot_map_pct_3, plot_map_pct_4, nrow=4)
dev.off()

png("sm_fig_plt_pctchange_rgn_2.png", width = 6, height = 5, units = "in", res = 500)
grid.arrange(plot_map_pct_5, plot_map_pct_6, plot_map_pct_7, plot_map_pct_8, nrow=4)
dev.off()

### generate output data
data_plt_rgn_out <- data_merge_rgn_sub[Year == 2050]
data_plt_rgn_out <- data_plt_rgn_out[, .(value = sum(value)),
                                     by = .(Scen, CensusID, RNAME, PLT)]

data_plt_rgn_out <- merge(data_plt_rgn_out,
                          data_plt_rgn_out[Scen == scenario[1]],
                          by = c("CensusID", "RNAME", "PLT"))

data_plt_rgn_out <- data_plt_rgn_out[Scen.x != scenario[1]]
data_plt_rgn_out[, change := value.x - value.y]
data_plt_rgn_out[, change.pct := (value.x - value.y)/value.y]
data_plt_rgn_out <- dcast.data.table(data_plt_rgn_out, PLT + CensusID + RNAME ~ Scen.x, value.var = c("change", "change.pct"))

fwrite(data_plt_rgn_out, "sm_fig_plt_change_rgn_data.csv", row.names = FALSE)


# plot: map of saved lives by census divisions------------------------------------------------------------------------------------
## outdoor mortality
data_mortality_outdoor <- data_mortality_base[, .(Outdoor = sum(change)), by = .(Year, Scen.x, CensusID)]
setnames(data_mortality_outdoor, old = "Scen.x", new = "Scen")

## indoor mortality
data_mortality_indoor <- fread("output_mortality_indoor.csv")
data_mortality_indoor <- data_mortality_indoor[Home == "mixed"]
data_mortality_indoor[, Home := NULL]

## merge indoor and outdoor mortality data
data_mortality_net <- merge(data_mortality_outdoor, data_mortality_indoor, by.x = c("Scen", "Year", "CensusID"), by.y = c("Scen", "Year", "RegionNum"))

data_mortality_net <- data_mortality_net[Year >= 2020]
data_mortality_net <- melt(data_mortality_net, id.vars = c("Scen", "Year", "CensusID"))

data_mortality_net[Scen == "s01", Scenario := scenario_name_1[2]]
data_mortality_net[Scen == "s02", Scenario := scenario_name_1[3]]
data_mortality_net[Scen == "s03", Scenario := "Carbon Pricing"]
data_mortality_net[Scen == "s04", Scenario := paste(scenario_name_1[3], "Carbon Pricing", sep = " & ")]

data_mortality_census <- data_mortality_net[, .(value = sum(value)), by = .(Scen, Scenario, Year, CensusID)]
data_mortality_census <- data_mortality_census[Year == 2050]

data_mortality_table_net <- dcast(data_mortality_census, CensusID ~ Scenario, value.var = "value")
fwrite(data_mortality_table_net, "data_mortality_table_net.csv")

data_mortality_census <- merge(data_mortality_census,
                               mapping_census_state,
                               by = "CensusID",
                               allow.cartesian = TRUE)
data_mortality_census[, `:=`(State = tolower(State))]

## merge with map polygon data
data_mortality_map <- merge(states,
                            data_mortality_census,
                            by.x = c("region"),
                            by.y = c("State"),
                            sort = FALSE,
                            allow.cartesian=TRUE,
                            all.y = TRUE)
data_mortality_map <- data_mortality_map[!(region %in% c("alaska","hawaii"))]
data_mortality_map <- data_mortality_map[order(data_mortality_map$order),]

### baseline
plot_map_mortality <- ggplot() +
  geom_polygon(data = data_mortality_map,
               aes(long, lat, group = group, fill = value),
               color = NA) + # data containing modeling results
  coord_map("albers", at0 = 45.5, lat1 = 29.5) +
  # labs(title = parse(text = paste0(toupper(letters)[i], ":~", plt_id))) +
  geom_polygon(data = shapefile_df,
               aes(x = long, y = lat, group = group),
               color = "grey40",
               size = 0.2,
               fill = NA) + # add a layer of census divisions
  facet_wrap(~Scenario) +
  theme(legend.position = 'right',
        plot.title = element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-2,-2,-2,-2),
        legend.key.size =  unit(0.2, "in"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        strip.text = element_text(size=8),
        legend.box = "vertical",
        legend.title=element_text(size=8),
        legend.text=element_text(size=7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_binned(type = "viridis", name = "Reduced\nPremature\nDeaths", alpha = .5, labels = comma)

pdf("fig_saved_lives_census.pdf", width = 6, height = 3.5)
print(plot_map_mortality)
dev.off()

png("fig_saved_lives_census.png", width = 6, height = 3.5, units = "in", res = 500)
print(plot_map_mortality)
dev.off()


# plot: indoor and outdoor health effect over time ------------------------------------------------------------------------------------
data_mortality_nation <- data_mortality_net[, .(value = sum(value)), by = .(Scen, Scenario, Year, variable)]

## plot in a bar chart
plot_mort_year <- ggplot(data = data_mortality_nation, mapping = aes(x = Year, y = value, group = Scen)) +
  geom_col(position="dodge", aes(fill = Scenario)) + 
  facet_wrap(~ variable) +
  ylab("Reduced Premature Deaths") +
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
  guides(fill = guide_legend(title="", byrow = TRUE, nrow = 2, order = 1)) +
  scale_fill_npg()

pdf("sm_fig_saved_lives_years.pdf", width = 6, height = 3.5)
print(plot_mort_year)
dev.off()

png("sm_fig_saved_lives_years.png", width = 6, height = 3.5, units = "in", res = 500)
print(plot_mort_year)
dev.off()

data_mortality_nation_2050 <- data_mortality_nation[Year == 2050]
data_mortality_nation_2050 <- dcast(data_mortality_nation_2050, variable ~ Scenario, value.var = "value")

data_mortality_nation_2050_sum <- t(apply(data_mortality_nation_2050[,-c(1)], 2, sum))
data_mortality_nation_2050_sum <- cbind("Net", data_mortality_nation_2050_sum)
data_mortality_nation_2050 <- rbind(data_mortality_nation_2050, data_mortality_nation_2050_sum, use.names=FALSE)


