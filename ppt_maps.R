library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidycensus)
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
library(plotly)
library(ggrepel)

setwd("/Users/lamhine/Library/CloudStorage/Box-Box/San Joaquin Valley PH/Outputs and Reports")
census_api_key("f77de7a3ed4802ce5f3a28b99ba1105b8bb39529", overwrite = F)

# used these tutorials: 
# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://www.emilhvitfeldt.com/post/2019-05-21-center-continuous-palettes-in-ggplot2/ 


#--------------------------- PREP GEOGRAPHIC DATA -----------------------------#

# PREP DATA FOR MAPPING
ca_df <- map_data("state") %>% subset(., region == "california") 
ca_fips <- as_tibble(county.fips) %>% 
  separate(polyname, ",", into = c("state", "county"), remove = T) %>% 
  filter(state == "california") %>% 
  rename(subregion = county)
ca_counties <- map_data("county") %>% 
  filter(region == "california") %>% 
  inner_join(., ca_fips, by = "subregion") %>% 
  select(-state)

# NOTE - SJVPHC does not include kern county
sjv_list <- c("calaveras", "fresno", "kings", "madera", "mariposa", "merced", 
              "san benito", "san joaquin", "stanislaus", "tulare", "tuolumne")
sjv_counties <- ca_counties %>% 
  filter(subregion %in% sjv_list)

# GET MAP CENTROIDS FOR LABELS
ca_cent <- map_data('county', 'california')
cnames <- ca_cent %>% group_by(subregion) %>%
  summarize_at(vars(long, lat), ~ mean(range(.))) %>% 
  filter(subregion %in% sjv_list) %>% 
  mutate(subregion = str_to_title(subregion))

# GET 2020 CENSUS COUNTY POPULATION COUNTS FOR SJV
sjv_pop <- get_decennial(geography = "county", variables = "P1_001N", year = 2020) %>% 
  filter(grepl("California", NAME)) %>% 
  mutate(GEOID = sub(".", "", GEOID)) %>% 
  filter(GEOID %in% unlist(sjv_counties %>% distinct(fips))) %>% 
  mutate(NAME = tolower(str_remove(NAME, " County, California"))) %>% 
  rename(population = value, 
         fips = GEOID, 
         geog = NAME) %>% 
  select(-variable)

zip_pops <- get_acs(geography = "zcta", year = 2019, state = 06, variables = "B01003_001")

#---------------------------- PREP SCREENING DATA -----------------------------#

hiv <- read_xlsx("SJV_Screenings_Master_file.xlsx", sheet = "HIV") %>% mutate(condition = "HIV")
hep <- read_xlsx("SJV_Screenings_Master_file.xlsx", sheet = "HepC_etc") %>% mutate(condition = "Hepatitis C")
brc <- read_xlsx("SJV_Screenings_Master_file.xlsx", sheet = "Breast_Cancer") %>% mutate(condition = "Breast Cancer")
cer <- read_xlsx("SJV_Screenings_Master_file.xlsx", sheet = "Cervial_Cancer") %>% mutate(condition = "Cervical Cancer")
col <- read_xlsx("SJV_Screenings_Master_file.xlsx", sheet = "Colorectal_Cancer") %>% mutate(condition = "Colorectal Cancer")

# COMBINE ALL FILES
all <- rbind(hiv, hep, brc, cer, col) %>% 
  select(-"Kern County")

# GET COUNTS AND PROPORTIONS OF ELIGIBLE BY COUNTY IN OPTUM
opt_elg <- all %>% 
  filter(grepl("Eligible", Number_Type),
         Datset == "Optum") %>% 
  select(-Race) %>% 
  mutate(across(3:15, as.numeric)) %>% 
  pivot_longer(cols = 3:15, names_to = "geog", values_to = "val") %>% 
  pivot_wider(names_from = Number_Type, values_from = val) %>% 
  mutate(avg_elg = rowMeans(select(.,c(Eligible_Pre_COVID, Eligible_Post_COVID))),
         geog = tolower(str_remove(geog, " County"))) %>% 
  filter(!geog %in% c("ca", "sjv")) %>% 
  left_join(., sjv_pop, by = "geog") %>% 
  mutate(pct_elg = avg_elg/population)


# GET RATES AND PRE/POST RATE CHANGES FOR ALL RACES FOR OPTUM AND AFC
all_rc <- all %>% 
  filter(Race == "All", grepl("Rate", Number_Type)) %>% 
  select(-Race) %>% 
  mutate(across(3:15, as.numeric)) %>% 
  pivot_longer(cols = 3:15, names_to = "geog", values_to = "val") %>% 
  pivot_wider(names_from = Number_Type, values_from = val) %>% 
  mutate(rate_change = Rate_Post_COVID - Rate_Pre_COVID, 
         geog = tolower(str_remove(geog, " County"))) %>% 
  filter(!geog %in% c("ca", "sjv")) 

#-------------------- CREATE DISEASE-SPECIFIC DATA FRAMES ---------------------#

all_rc <- left_join(all_rc, ca_fips, by = c("geog" = "subregion")) %>% select(-state)

# HIV - OPTUM
hiv_map_df <- all_rc %>% 
  filter(condition == "HIV" & Datset == "Optum") %>% 
  left_join(ca_counties, by = "fips")

# HEPATITIS C - OPTUM
hep_map_df <- all_rc %>% 
  filter(condition == "Hepatitis C" & Datset == "Optum") %>% 
  left_join(ca_counties, by = "fips")

# BREAST CANCER - OPTUM
brc_map_df <- all_rc %>% 
  filter(condition == "Breast Cancer" & Datset == "Optum") %>% 
  left_join(ca_counties, by = "fips")

# CERVICAL CANCER - OPTUM
cer_map_df <- all_rc %>% 
  filter(condition == "Cervical Cancer" & Datset == "Optum") %>% 
  left_join(ca_counties, by = "fips")

# COLORECTAL CANCER - OPTUM
col_map_df <- all_rc %>% 
  filter(condition == "Colorectal Cancer" & Datset == "Optum") %>% 
  left_join(ca_counties, by = "fips")


#-------------------------------- CREATE MAPS ---------------------------------#

# BASE LAYER
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "black", fill = "gray")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# HIV MAP
lim_hiv <- max(abs(hiv_map_df$rate_change)) * c(-1, 1)
hiv_map <- ca_base + 
  theme_nothing() + 
  geom_polygon(data = hiv_map_df, aes(fill = rate_change), color = "white") +
  scale_fill_distiller(type = "div", limit = lim_hiv) +
  geom_polygon(color = "black", fill = NA) + 
  geom_label_repel(data = cnames, aes(x = long, y = lat, label = subregion, group = 4), 
                  min.segment.length = 0, alpha = 0.75, color = "black",
                  fontface = "bold", xlim = c(-Inf,Inf), ylim = c(-Inf,Inf)) +
  coord_fixed(xlim = c(-123.25, -116.25),  ylim = c(34.5, 38.75), ratio = 1.3) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
  theme_bw() + 
  ditch_the_axes 
hiv_map

# HEPATITIS C MAP
lim_hep <- max(abs(hep_map_df$rate_change)) * c(-1, 1)
hep_map <- ca_base + 
  theme_nothing() + 
  geom_polygon(data = hep_map_df, aes(fill = rate_change), color = "white") +
  scale_fill_distiller(type = "div", limit = lim_hep) +
  geom_polygon(color = "black", fill = NA) + 
  geom_label_repel(data = cnames, aes(x = long, y = lat, label = subregion, group = 4), 
                   min.segment.length = 0, alpha = 0.75, color = "black",
                   fontface = "bold", xlim = c(-Inf,Inf), ylim = c(-Inf,Inf)) +
  coord_fixed(xlim = c(-123.25, -116.25),  ylim = c(34.5, 38.75), ratio = 1.3) +
  theme_bw() + 
  ditch_the_axes 
hep_map

# BREAST CANCER MAP
lim_brc <- max(abs(brc_map_df$rate_change)) * c(-1, 1)
brc_map <- ca_base + 
  theme_nothing() + 
  geom_polygon(data = brc_map_df, aes(fill = rate_change), color = "white") +
  scale_fill_distiller(type = "div", limit = lim_brc) +
  geom_polygon(color = "black", fill = NA) + 
  geom_label_repel(data = cnames, aes(x = long, y = lat, label = subregion, group = 4), 
                   min.segment.length = 0, alpha = 0.75, color = "black",
                   fontface = "bold", xlim = c(-Inf,Inf), ylim = c(-Inf,Inf)) +
  coord_fixed(xlim = c(-123.25, -116.25),  ylim = c(34.5, 38.75), ratio = 1.3) +
  theme_bw() + 
  ditch_the_axes 
brc_map

# CERVICAL CANCER MAP
lim_cer <- max(abs(cer_map_df$rate_change)) * c(-1, 1)
cer_map <- ca_base + 
  theme_nothing() + 
  geom_polygon(data = cer_map_df, aes(fill = rate_change), color = "white") +
  scale_fill_distiller(type = "div", limit = lim_cer) +
  geom_polygon(color = "black", fill = NA) + 
  geom_label_repel(data = cnames, aes(x = long, y = lat, label = subregion, group = 4), 
                   min.segment.length = 0, alpha = 0.75, color = "black",
                   fontface = "bold", xlim = c(-Inf,Inf), ylim = c(-Inf,Inf)) +
  coord_fixed(xlim = c(-123.25, -116.25),  ylim = c(34.5, 38.75), ratio = 1.3) +
  theme_bw() + 
  ditch_the_axes 
cer_map

# COLORECTAL CACNER MAP
lim_col <- max(abs(col_map_df$rate_change)) * c(-1, 1)
col_map <- ca_base + 
  theme_nothing() + 
  geom_polygon(data = col_map_df, aes(fill = rate_change), color = "white") +
  scale_fill_distiller(type = "div", limit = lim_col) +
  geom_polygon(color = "black", fill = NA) + 
  geom_label_repel(data = cnames, aes(x = long, y = lat, label = subregion, group = 4), 
                   min.segment.length = 0, alpha = 0.75, color = "black",
                   fontface = "bold", xlim = c(-Inf,Inf), ylim = c(-Inf,Inf)) +
  coord_fixed(xlim = c(-123.25, -116.25),  ylim = c(34.5, 38.75), ratio = 1.3) +
  theme_bw() + 
  ditch_the_axes 
col_map
