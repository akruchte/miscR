## failed attempt at cartogram code

                                        # get block by block data
#P001001 total population
library(tidycensus)
library(tidyverse)
library(cartogram)
library(sf)

pop <- "P001001"
pa_tract_data <- get_decennial(geography = "county", variables = pop, state = "PA", geometry = T) %>%
  select(county = NAME, county_population = value, geometry) %>%
  mutate(county = str_extract(county, "[A-Za-z]+")) %>%
  mutate(tract_data = map(county,
                          ~get_decennial(geography = "tract", county = ., state = "PA", variables = pop, geometry = T)))

pa_tract_data_full <- do.call(rbind, pa_tract_data$tract_data)


county_data <- select(pa_tract_data, -tract_data) %>%
  mutate(density = county_population / as.numeric(st_area(geometry)),
         weight = county_population) %>%
  st_transform(2271)
#  st_transform( "+proj=merc")

philly <- county_data %>% filter(county == "Philadelphia") %>% pull(geometry)
allegheny <- county_data %>% filter(county == "Allegheny") %>% pull(geometry)

county_data <- county_data %>%
  mutate(dist_philly = st_distance(st_centroid(geometry), st_centroid(philly)),
         dist_allegheny = st_distance(st_centroid(geometry), st_centroid(allegheny)))

closer_philly <- filter(county_data, dist_philly < dist_allegheny)
closer_allegheny <- filter(county_data, dist_allegheny < dist_philly)

ggplot(county_data) + geom_sf()

save(pa_tract_data_full, pa_tract_data, file = "pa_tract_data_full.rda")


county_data <- county_data %>% mutate(county = str_to_upper(county))

deaths <- preprocessed_data %>% filter(lat !=0, long != 0, !is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c('long', 'lat'))

death_sites <- deaths %>%
  select(county) %>%
  st_set_crs(4269) %>%
  st_transform(2271) %>%
  group_by(county) %>%
  summarize(geometry = st_combine(geometry))

county_data <- county_data %>% arrange(county)
death_sites <- death_sites %>% arrange(county)

data <- bind_cols(county_data, death_sites) %>%
  select(-county1) %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  mutate(geometry = st_combine(c(geometry, geometry1)), geometry1 = NULL) %>%
  mutate(geometry = st_collection_extract(geometry, "POLYGON")) %>%
  ungroup()

pop_density_cartogram <- cartogram_cont(data, "density")
ggplot(pop_density_cartogram) + geom_sf() + theme_void()

#compute death rate per densit
#deaths_agg %>% left_join(pop) %>% mutate(rate = n / pop) %>% left_join(county_data %>% mutate(county = str_to_upper(county))) %>% mutate(rate_per_density = rate / density) -> death_data
