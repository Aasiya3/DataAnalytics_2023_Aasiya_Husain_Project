library(labelled)
library(dplyr)


#loading weather data in CSV format
Weather<-read.csv("EMS Data/Weather Data/GHCND_NY_Central_Park_20080101_20161231.csv",header=TRUE)

#labelling different variables
var_label(Weather$AWND) <- "Average Daily Wind Speed (mi/h)"
var_label(Weather$FMTM) <- "Time of Fastest 1-minute wind (HHMM)"
var_label(Weather$PGTM) <- "Peak Gust Time (HHMM)"
var_label(Weather$PRCP) <- "Precipitation (in)"
var_label(Weather$SNOW) <- "Snowfall (in)"
var_label(Weather$SNWD) <- "Snow depth (in)"
var_label(Weather$TMAX) <- "Max Temp (F)"
var_label(Weather$TMIN) <- "Min Temp (F)"
var_label(Weather$WDF2) <- "Direction of fastest 2-minute wind (degrees)"
var_label(Weather$WDF5) <- "Direction of fastest 5-second wind (degrees)"
var_label(Weather$WSF2) <- "Fastest 2-minute wind speed (mph)"
var_label(Weather$WSF5) <- "Fastest 5-second wind speed (mph)"
var_label(Weather$WT01) <- "Fog"
var_label(Weather$WT02) <- "Heavy Fog"
var_label(Weather$WT03) <- "Thunder"
var_label(Weather$WT04) <- "Ice pellets"
var_label(Weather$WT05) <- "Hail"
var_label(Weather$WT06) <- "Glaze or rime"
var_label(Weather$WT07) <- "Dust or sand"
var_label(Weather$WT08) <- "Smoke or haze"
var_label(Weather$WT09) <- "Blowing or drifting snow"
var_label(Weather$WT11) <- "High or damaging winds"
var_label(Weather$WT13) <- "Mist"
var_label(Weather$WT14) <- "Drizzle"
var_label(Weather$WT16) <- "Rain"
var_label(Weather$WT17) <- "Freezing Rain"
var_label(Weather$WT18) <- "Snow"
var_label(Weather$WT19) <- "Unknown Source of Precipitation"
var_label(Weather$WT22) <- "Freezing Fog"

#replace blanks with zeros for binary variables
wts <- grep('^WT', names(Weather), value = TRUE) #creates a new vector of all the binary variables related to weather type
Weather[wts][is.na(Weather[wts])] <- 0 #replaces all of the "NA" values in those binary variables with 0s

#create date variable for merging
#EMS$date_m <- as.Date(EMS$incident_dt, "%m/%d/%Y")

#create date variable for merging with EMS_Brooklyn
EMS_Brooklyn$date_m <- as.Date(EMS_Brooklyn$incident_dt, "%m/%d/%Y")

#reformat date variable in weather data from character to date
Weather$date_m <- as.Date(Weather$DATE, "%m/%d/%Y")

#create month category
library(lubridate)
Weather$month <- floor_date(Weather$date_m, "month") #uses the floor_date command within dplyr

#create new dataframe that stores total precipitation by month
g <- Weather %>% #specifies the Weather data frame as the reference data frame
  group_by(month) %>%
  summarise(precip_total = sum(PRCP))

#plot that new dataframe by month and year
ggplot(g, aes(month, precip_total)) +
  geom_line() +
  xlab("Year") + ylab("Total Rainfall (inches)")

#merge data at the incident level
#merged_data <- left_join(EMS,Weather,by="date_m")


#merge data at the incident level with EMS_Brooklyn
merged_data <- left_join(EMS_Brooklyn,Weather,by="date_m")
