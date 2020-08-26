library("raster")
library("rgdal")
library("dismo")
library("sp")
library("tidyverse")
library("climatrends")
library("nasapower")

el <- stack("/Volumes/BioversityInt/rasters/GloElev_30as/GloElev_30as.asc")
temp <- stack("/Volumes/BioversityInt/rasters/worldclim2_0_30s/wc2.0_bio_30s_01.tif")
eco <- readOGR("/Volumes/BioversityInt/shapefiles/tnc_terr_ecoregions", "tnc_terr_ecoregions")
r <- raster()


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

coord <- dt[,c("lon","lat")]

proj <- proj4string(eco)

coord <- SpatialPoints(coord, proj4string = CRS(proj))

dt$ecoregion <- over(coord, eco)$ECO_NAME

dt <- as.data.frame(dt)

# climatic data
coord <- dt[,c("lon","lat")]

coord <- gridSample(coord, r, n = 1)

ids <- rownames(coord)


temp <- temperature(coord, 
                    day.one = "2000-01-01",
                    last.day = "2019-12-01", 
                    timeseries = TRUE, 
                    intervals = 365)


rain <- rainfall(coord, 
                 day.one = "2000-01-01",
                 last.day = "2019-12-01", 
                 timeseries = TRUE, 
                 intervals = 365)

dt <- as_tibble(dt)

dt


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
