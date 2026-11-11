#Processing climate data for Australia from SILO
# https://www.longpaddock.qld.gov.au/silo/about/climate-variables/
# Water occurance from Landsat: https://developers.google.com/earth-engine/datasets/catalog/JRC_GSW1_4_GlobalSurfaceWater#description
# NDVI from MODIS: https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A1

#Data are in the "dataprep.7z" compressed file, which you need to extract to
#the data folder for this script to work.

#Setup
library(terra)
library(sf)
library(tmap)
library(tidyverse)

sf_use_s2(FALSE)
tmap_options(check.and.fix=TRUE)

#Read in data
rain00<-rast("data/prep_data/2000.daily_rain.nc")
rain_sum_00<-sum(rain00)
#plot(rain_sum_00)

rain20<-rast("data/prep_data/2020.daily_rain.nc")
rain_sum_20<-sum(rain20)
#plot(rain_sum)

maxtemp_00<-rast("data/prep_data/2000.max_temp.nc")
maxtemp_med_00<-median(maxtemp_00)
#plot(maxtemp_med_00)

maxtemp_20<-rast("data/prep_data/2020.max_temp.nc")
maxtemp_med_20<-median(maxtemp_20)
#plot(maxtemp_med_20)

mintemp_00<-rast("data/prep_data/2000.min_temp.nc")
mintemp_med_00<-median(mintemp_00)
#plot(mintemp_med_00)

mintemp_20<-rast("data/prep_data/2020.min_temp.nc")
mintemp_med_20<-median(mintemp_20)
#plot(mintemp_med_20)

ndvi_00<-rast("data/prep_data/Mean_NDVI_2000_Australia.tif")
#plot(ndvi_00)

ndvi_20<-rast("data/prep_data/Mean_NDVI_2020_Australia.tif")
#plot(ndvi_20)

water_00<-rast("data/prep_data/Water_Composite_2000_Australia.tif")
#plot(water_00)

water_20<-rast("data/prep_data/Water_Composite_2020_Australia.tif")
#plot(water_20)

pop_00<-rast("data/prep_data/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2000_15_min.tif")
#plot(pop_00)

pop_20<-rast("data/prep_data/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_15_min.tif")
#plot(pop_20)

dem<-rast("data/prep_data/aus_dem_databasin.tif")

aus<-as.polygons(mintemp_00) %>%
  st_as_sf() %>%
  summarise()

#Create hex grid and do zonal statistics
grid<-st_make_grid(aus,cellsize=1,square = FALSE) 
grid<-st_as_sf(grid) %>%
  mutate(grid_id=paste0("G",row_number()))

maxtemp_00z<-zonal(maxtemp_med_00,vect(grid),"median")
grid$maxtemp_00_med<-maxtemp_00z$median

##Filter out any cells that don't have data
grid1<-grid %>%
  filter(!is.na(maxtemp_00_med))

##Do the rest of the zonal stats
maxtemp_20z<-zonal(maxtemp_med_20,vect(grid1),"median")
mintempz_00z<-zonal(mintemp_med_00,vect(grid1),"median")
mintempz_20z<-zonal(mintemp_med_20,vect(grid1),"median")
rain_00z<-zonal(rain_sum_00,vect(grid1),"sum")
rain_20z<-zonal(rain_sum_20,vect(grid1),"sum")
ndvi_00z<-zonal(ndvi_00,vect(grid1),"median")
ndvi_20z<-zonal(ndvi_20,vect(grid1),"median")
water_00z<-zonal(water_00,vect(grid1),"mean")
water_20z<-zonal(water_20,vect(grid1),"mean")
pop_00z<-zonal(pop_00,vect(grid1),"sum")
pop_20z<-zonal(pop_20,vect(grid1),"sum")

demz<-zonal(dem,vect(grid1),"median")

grid1$maxtemp_20_med<-maxtemp_20z$median
grid1$mintemp_00_med<-mintempz_00z$median
grid1$mintemp_20_med<-mintempz_20z$median
grid1$rain_00_sum<-rain_00z$sum
grid1$rain_20_sum<-rain_20z$sum
grid1$ndvi_00_med<-ndvi_00z$NDVI
grid1$ndvi_20_med<-ndvi_20z$NDVI
grid1$water_00_pct<-water_00z$waterClass
grid1$water_20_pct<-water_20z$waterClass
grid1$pop_00<-pop_00z$gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2000_15_min
grid1$pop_20<-pop_20z$gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_15_min
grid1$elev_med<-demz$aus_dem_databasin

tm_shape(grid1)+tm_polygons("pop_20",style="jenks")

#Add animals
animals<-st_read("data/prep_data/inaturalist_aus.gpkg") 

tmap_mode("view")
tm_shape(grid1)+
  tm_polygons("maxtemp",style="jenks")+
  tm_shape(animals)+tm_dots("commonName")

grid_sum<-animals %>%
  st_join(grid1,join=st_within) %>%
  st_set_geometry(NULL) %>%
  count(grid_id,commonName) %>%
  pivot_wider(names_from=commonName,values_from=n,values_fill=0) 

grid2<-grid1 %>%
  left_join(grid_sum)
grid2[is.na(grid2)]<-0

tm_shape(grid2)+
  tm_polygons("Agile wallaby",style="jenks")

st_write(grid2,"data/aus_climate_inat.gpkg",delete_dsn=TRUE)
