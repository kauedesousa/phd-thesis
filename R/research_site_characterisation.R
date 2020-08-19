library("raster")
library("rgdal")
library("dismo")
library("sp")
library("tidyverse")

el <- stack("/Volumes/BioversityInt/rasters/GloElev_30as/GloElev_30as.asc")
temp <- stack("/Volumes/BioversityInt/rasters/worldclim2_0_30s/wc2.0_bio_30s_01.tif")
maxtemp <- stack("/Volumes/BioversityInt/rasters/worldclim2_0_30s/wc2.0_bio_30s_05.tif")
mintemp <- stack("/Volumes/BioversityInt/rasters/worldclim2_0_30s/wc2.0_bio_30s_06.tif")
prec <- stack("/Volumes/BioversityInt/rasters/worldclim2_0_30s/wc2.0_bio_30s_12.tif")
maxprec <- stack("/Volumes/BioversityInt/rasters/worldclim2_0_30s/wc2.0_bio_30s_13.tif")
minprec <- stack("/Volumes/BioversityInt/rasters/worldclim2_0_30s/wc2.0_bio_30s_14.tif")
eco <- readOGR("/Volumes/BioversityInt/shapefiles/tnc_terr_ecoregions", "tnc_terr_ecoregions")

dt1 <- read.csv("data/research_sites/anexo3_farm_ids.csv")
dt1$region <- "ca"

dt2 <- read.csv("data/tricot_data.csv")
dt2$region <- ifelse(dt2$crop == "wheat", "sa", ifelse(dt2$crop == "durumwheat", "ea", "ca"))

dt <- rbind(dt1[,c("lon","lat", "region")], dt2[,c("lon","lat", "region")])

coord <- dt[,c("lon","lat")]

coord <- gridSample(coord, temp, n = 1)

dt <- dt[rownames(dt) %in% rownames(coord), ]

table(dt$region)

dt$elevation <- as.vector(raster::extract(el, dt[,c("lon","lat")]))

dt$temp <- as.vector(raster::extract(temp, dt[,c("lon","lat")]))

dt$maxtemp <- as.vector(raster::extract(maxtemp, dt[,c("lon","lat")]))

dt$mintemp <- as.vector(raster::extract(mintemp, dt[,c("lon","lat")]))

dt$prec <- as.vector(raster::extract(prec, dt[,c("lon","lat")]))

dt$maxprec <- as.vector(raster::extract(maxprec, dt[,c("lon","lat")]))

dt$minprec <- as.vector(raster::extract(minprec, dt[,c("lon","lat")]))

coord <- dt[,c("lon","lat")]

proj <- proj4string(eco)

coord <- SpatialPoints(coord, proj4string = CRS(proj))

dt$ecoregion <- over(coord, eco)$ECO_NAME

dt <- as.data.frame(dt)

dt <- as_tibble(dt)

dt

ind <- getData('GADM', country=c('IND'), level=1)

table(over(coord, ind)$NAME_1)


dt %>% 
  group_by(region) %>% 
  summarise(mean = mean(elevation),
            min = min(elevation),
            max = max(elevation),
            sd = sd(elevation))

dt %>% 
  group_by(region) %>% 
  summarise(mean = mean(prec),
            min = min(prec),
            max = max(prec),
            sd = sd(prec))

dt %>% 
  group_by(region) %>% 
  summarise(mean = mean(temp),
            min = mean(mintemp),
            max = mean(maxtemp),
            sd = sd(temp))

table(dt$ecoregion, dt$region)

write_csv(dt, "data/research_sites_char.csv")
