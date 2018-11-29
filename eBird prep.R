library(auk)
library(lubridate)
library(sp)
library(sf)
library(raster)

owd <- setwd("ebd/")

f_in <- "ebd_CA_relOct-2018.txt"

# output text file
f_out <- "ebd_filtered_grja.txt"

ebird_data <- f_in %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_species(species = c("Bank Swallow", "Barn Swallow")) %>% 
  
  # date: use standard ISO date format `"YYYY-MM-DD"`
  auk_date(date = c("2007-01-01", "2018-12-31")) %>%

  # 3. run filtering
  auk_filter(file = f_out, overwrite = TRUE) %>% 
  # 4. read text file into r data frame
  read_ebd()


#filter months (only retain months 4-5 and 8-9)
month_filter <- (month(ebird_data$observation_date) >= 4 & month(ebird_data$observation_date) <= 5) |
  (month(ebird_data$observation_date) >= 8 & month(ebird_data$observation_date) <= 9)  

ebird_data_month <- ebird_data[month_filter,]

#filter start time (Include 0400 to 0659 and 2000 to 2259)
time_filter <- (ebird_data_month$time_observations_started >= "04:00:00" & ebird_data_month$time_observations_started < "07:00:00") |
  (ebird_data_month$time_observations_started >= "20:00:00" & ebird_data_month$time_observations_started < "23:00:00")


ebird_data_month_time <- ebird_data_month[time_filter,]

#filter number of observations (currently >= 10)
ebird_data_month_time_obs <- ebird_data_month_time[ebird_data_month_time$observation_count >= 10,]
ebird_data_month_time_obs <- ebird_data_month_time_obs[ebird_data_month_time_obs$observation_count != "X",]



#####################################################
## some analysis
#####################################################
sp <- SpatialPointsDataFrame(ebird_data_month_time_obs[,c("longitude" , "latitude")], ebird_data_month_time_obs)

r <- raster(sp)
res(r) <- 0.01
r

nc <- rasterize(coordinates(sp), r, fun='count', background=0)
plot(nc)


setwd("../out")
writeRaster(nc,"tt.tif", overwrite = T)
