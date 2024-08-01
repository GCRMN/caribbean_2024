# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(nngeo)

# 2. Coral reefs ----

## 2.1 Load data ----

data_regions <- read_sf("data/01_maps/02_clean/01_gcrmn-regions/gcrmn_regions.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

data_reefs <- read_sf("data/01_maps/01_raw/01_reefs/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

data_eez <- read_sf("data/01_maps/01_raw/02_eez/eez_v12.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

## 2.2 Filter reefs falling within Caribbean GCRMN region ----

data_reefs <- st_intersection(data_reefs, data_regions) %>%
  filter(region == "Caribbean")

## 2.3 Visual check ----

ggplot() +
  geom_sf(data = data_reefs, fill = "red")

# 3. EEZ ----

## 3.1 Filter EEZ with coral reefs ----

data_eez_intersects <- st_intersection(data_eez, data_reefs)

data_eez <- data_eez %>% 
  filter(TERRITORY1 %in% unique(data_eez_intersects$TERRITORY1))

## 3.2 Identify overclaimed territories and joint regimes ----

data_eez2 <- data_eez %>% 
  mutate(overclaimed = ifelse(str_detect(GEONAME, "Over"), TRUE, FALSE))

plot <- ggplot() +
  geom_sf(data = data_eez2, aes(fill = overclaimed)) +
  scale_fill_manual(breaks = c(TRUE, FALSE),
                     values = c("red", "white")) +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

ggsave("figs/06_additional/overclaimed-territories.png", height = 4)

data_eez2 <- data_eez %>% 
  mutate(joint_regime = ifelse(str_detect(GEONAME, "Joint"), TRUE, FALSE))

plot <- ggplot() +
  geom_sf(data = data_eez2, aes(fill = joint_regime)) +
  scale_fill_manual(breaks = c(TRUE, FALSE),
                    values = c("red", "white")) +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

ggsave("figs/06_additional/joint-regime-areas.png", height = 4)

rm(data_eez2, plot)

## 3.3 Remove non-caribbean EEZ ----

### 3.3.1 United States ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "United States" & GEONAME == "United States Exclusive Economic Zone") %>%
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 1) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "United States" & 
                         GEONAME == "United States Exclusive Economic Zone")), .) %>% 
  filter(GEONAME != "Joint regime area: United States / Russia")

### 3.3.2 Mexico ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Mexico") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Mexico")), .)
  
### 3.3.3 Colombia ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Colombia" & GEONAME == "Colombian Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Colombia" & 
                         GEONAME == "Colombian Exclusive Economic Zone")), .)

### 3.3.4 Nicaragua ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Nicaragua") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(TERRITORY1 != "Nicaragua"), .)

### 3.3.5 Costa Rica ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Costa Rica" & GEONAME == "Costa Rican Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Costa Rica" & 
                         GEONAME == "Costa Rican Exclusive Economic Zone")), .) %>% 
  filter(GEONAME != "Joint regime area: Costa Rica / Ecuador (Galapagos)")

### 3.3.6 Panama ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Panama") %>% 
  st_cast(., "POLYGON") %>% 
  st_intersection(., data_regions) %>% 
  filter(region == "Caribbean") %>% 
  bind_rows(data_eez %>% 
              filter(!(TERRITORY1 == "Panama")), .)

### 3.3.7 Honduras ----

data_eez <- data_eez %>% 
  filter(GEONAME == "Honduran Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(data_eez %>% 
              filter(GEONAME != "Honduran Exclusive Economic Zone"), .) %>% 
  filter(GEONAME != "Joint regime area: Honduras / United Kingdom (Cayman Islands)")

### 3.3.8 Dominican Republic ----

data_eez <- data_eez %>% 
  filter(TERRITORY1 == "Dominican Republic") %>% 
  filter(row_number() == 1) %>% 
  bind_rows(data_eez %>% 
              filter(TERRITORY1 != "Dominican Republic"), .)

### 3.3.9 Jamaica ----

data_eez_a <- data_eez %>% 
  filter(TERRITORY1 == "Jamaica") %>% 
  group_by(SOVEREIGN1, TERRITORY1) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  st_buffer(0.1) %>% 
  nngeo::st_remove_holes(.) %>% 
  mutate(TERRITORY1 = "Jamaica")
  
data_eez <- st_difference(data_eez_a, st_union(data_eez %>% filter(TERRITORY1 != "Jamaica"))) %>% 
  bind_rows(data_eez %>% 
              filter(TERRITORY1 != "Jamaica"), .)

rm(data_eez_a)

### 3.3.10 Visual check ----

ggplot() +
  geom_sf(data = data_eez) +
  geom_sf(data = data_reefs, col = "red") +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

### 3.3.11 Tag overclaimed territories ----

data_eez <- data_eez %>% 
  mutate(TERRITORY1 = ifelse(str_detect(GEONAME, "Over") == TRUE, GEONAME, TERRITORY1))

### 3.3.12 Remove useless columns ----

data_eez <- data_eez %>% 
  rename(country = SOVEREIGN1, territory = TERRITORY1) %>% 
  select(country, territory) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

### 3.3.13 Remove holes in EEZ ----

ggplot() +
  geom_sf(data = data_eez)

data_eez_a <- data_eez %>% 
  filter(territory == "Serrana Bank") %>% 
  nngeo::st_remove_holes(.)

data_eez_b <- data_eez %>% 
  filter(territory == "Quitasueño Bank") %>% 
  nngeo::st_remove_holes(.)

data_eez_c <- data_eez %>% 
  filter(territory == "Nicaragua")

data_eez <- data_eez %>% 
  filter(!(territory %in% c("Quitasueño Bank", "Serrana Bank", "Nicaragua"))) %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(., data_eez_a) %>% 
  bind_rows(., data_eez_b) %>% 
  bind_rows(., data_eez_c)

ggplot() +
  geom_sf(data = data_eez, fill = "lightblue")

## 3.4 Export the data ----

st_write(data_eez, "data/01_maps/02_clean/03_eez/caribbean_eez.shp", append = TRUE, delete_dsn = TRUE)

rm(data_eez_a, data_eez_b, data_eez_c)

## 3.5 Modified EEZ (non official EEZ, but needed for reporting) ----

### 3.5.1 Flower Garden Banks ----

data_fgb <- st_read("data/01_maps/01_raw/06_noaa/fgbnms_py/FGBNMS_py.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_buffer(0.3) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(country = "United States", territory = "Flower Garden Banks")

### 3.5.2 Florida ----

data_fk <- st_read("data/01_maps/01_raw/06_noaa/fknms_py2/fknms_py.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_buffer(0.3) %>% 
  summarise(geometry = st_union(geometry))

data_fk <- tibble(long = c(-80.1, -80), 
                       lat = c(25.5, 27.1)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  summarise() %>% 
  st_cast("LINESTRING") %>% 
  st_buffer(0.3) %>% 
  bind_rows(data_fk, .) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(country = "United States", territory = "Florida")

### 3.5.3 Combine Florida and FGB ----

data_us <- bind_rows(data_fk, data_fgb)

### 3.5.4 Visual check ----

ggplot() +
  geom_sf(data = data_us) +
  geom_sf(data = data_reefs, fill = "red", color = "red") +
  coord_sf(xlim = c(-95, -79), ylim = c(24, 29))

### 3.5.5 Add to EEZ data ----

data_eez <- data_eez %>% 
  filter(territory != "United States") %>% 
  bind_rows(., data_us)

rm(data_us, data_fgb, data_fk, data_fk_supp)

### 3.5.6 Merge Saint-Martin and Sint-Marteen ----

data_eez <- data_eez %>% 
  filter(territory %in% c("Collectivity of Saint Martin", "Sint-Maarten")) %>% 
  mutate(territory = "Sint-Marteen - Saint-Martin") %>% 
  group_by(territory) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(data_eez %>% filter(!(territory %in% c("Collectivity of Saint Martin", "Sint-Maarten"))), .)

### 3.5.7 Export the data ----

st_write(data_eez, "data/01_maps/02_clean/03_eez/caribbean_eez_sub.shp", append = TRUE, delete_dsn = TRUE)

## 3.6 Associate EEZ to reefs ----

data_reefs <- data_reefs %>% 
  select(-GRIDCODE) %>% 
  st_intersection(., data_eez) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  group_by(country, territory) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()

st_write(data_reefs, "data/01_maps/02_clean/02_reefs/reefs.shp", append = TRUE)

# 4. Land (Princeton) ----

## 4.1 List of shp to combine ----

list_shp <- list.files(path = "data/01_maps/01_raw/05_princeton",
                       pattern = ".shp$", full.names = TRUE, recursive = TRUE)

## 4.2 Combine shp ----

data_land <- map_dfr(list_shp, ~st_read(.)) %>% 
  rename(TERRITORY1 = NAME_ENGLI) %>% 
  st_transform(crs = 4326) %>% 
  select(TERRITORY1)

## 4.3 Correct issue for grouped territories ----

data_land <- data_land %>%
  filter(TERRITORY1 == "Bonaire, Saint Eustatius and Saba") %>% 
  st_cast(., "POLYGON") %>% 
  mutate(TERRITORY1 = case_when(row_number() == 1 ~ "Bonaire",
                                row_number() == 2 ~ "Bonaire",
                                row_number() == 3 ~ "Saint Eustatius",
                                row_number() == 4 ~ "Saba")) %>% 
  bind_rows(data_land, .) %>% 
  filter(TERRITORY1 != "Bonaire, Saint Eustatius and Saba") %>% 
  rename(territory = TERRITORY1) %>% 
  mutate(territory = str_replace_all(territory, c("Virgin Islands, U.S." = "United States Virgin Islands",
                                                  "Saint Eustatius" = "Sint-Eustatius",
                                                  "Saint-Martin" = "Sint-Marteen - Saint-Martin")))

## 4.4 Export the data ----

st_write(data_land, "data/01_maps/02_clean/05_princeton/land.shp", append = FALSE, delete_dsn = TRUE)
