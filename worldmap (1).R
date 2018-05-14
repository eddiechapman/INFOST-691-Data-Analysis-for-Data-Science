library(tidyverse)
library(maps)

setwd('C:\\Users\\chapman4\\Downloads\\spl infographic')

# The library records w/place of publication in MARC Country Code format
books <- read.csv('spl.csv', header = TRUE, fileEncoding = 'UTF-8-BOM')


# ==== MARC Country Codes ======================================================
# Library records use MARC Country Code to mark publishing location. The 
# following steps import a list of country codes and their associated full names
# and deal with the issue of "depreciated" codes. 

# Load MARC Country code list w/corresponding full names
place.codes <- read.csv('place_codes.txt', 
                        header = TRUE, sep = '\t', fileEncoding = 'UTF-8-BOM')

# "Depreciated" codes (AKA countries no longer exist/renamed) are marked with a "-".
# Some library records use depreciated codes so I want to try to update some of them.
# Seperating the MARC Country Codes into current codes: 
current.codes <- place.codes %>%
  filter(!str_detect(place.code, '-')) %>%
  mutate(code.status = 'current')

# ...and depreciated codes. Removing the "-" so they match my records. 
depreciated.codes <- place.codes %>%
  filter(str_detect(place.code, "-")) %>%
  mutate(place.code = gsub("-", "", .$place.code),
         code.status = 'depreciated')

# Putting them back together with new column to distinguish. 
place.codes <- bind_rows(current.codes, depreciated.codes)


# ==== Cleaning location data ==================================================
# The following steps are used to figure out which library records  are not 
# matching up with the map region names. This includesreorganizing US states and
# Canadian provinces into single national counts.

# Making list of total region names used in world map
map.regions <- map %>%
  distinct(region) %>%
  mutate(map.status = 'in map') %>%
  select(region, map.status)

# A list of a US states 
us.regions <- data.frame(state.name, usa = 'T') %>%
  rename(region = state.name)

# A list of Canadian provinces 
can <- c('Alberta', 'British Columbia', 'Manitoba', 'New Brunswick', 
            'Newfoundland and Labrador', 'Northwest Territories', 
            'Nova Scotia', 'Nunavut', 'Ontario', 'Prince Edward Island', 
            'Quebec', 'Saskatchewan', 'Yukon')
can.regions <- data.frame(can, can = 'T') %>%
  rename(region = can)

# Counting the number of library records from each location. Adding the location 
# code's full name via the MARC Country Code list. Joining the lists of region 
# names (map, US, Canadian) to keep track of which records are being represented 
# in the map. 
books <- books %>%
  count(place.code) %>%
  left_join(place.codes, by = 'place.code') %>%
  mutate(place.name = as.character(place.name)) %>%
  select(region = place.name, n, code.status) %>%
  left_join(map.regions) %>%
  left_join(us.regions) %>%
  left_join(can.regions) %>%
  mutate()

# Subset "books" with US locations in order to sum the total so that a new "US" 
# location can be used instead of the individual states (which are not accepted 
# in the map)
us.books <- books %>%
  filter(usa == 'T')
us.total <- (sum(us.books$n) + 553)

# same as above, but with Canadian provinces. 
can.books <- books %>%
  filter(can.1 == 'T')
can.total <- (sum(can.books$n) + 784)

# Adding the nation totals for Canada & US
# Updating countries that had old/alternate region names in the 
# library / MARC records. 
books <- books %>%
  bind_rows(data.frame(region = 'Canada', n = can.total)) %>%
  bind_rows(data.frame(region = 'USA', n = us.total)) %>%
  bind_rows(data.frame(region = 'UK', n = 7940)) %>%
  bind_rows(data.frame(region = 'Russia', n = 207)) %>%
  bind_rows(data.frame(region = 'Myanmar', n = 2)) %>%
  bind_rows(data.frame(region = 'Serbia', n = 110)) 

# A list of which library records aren't being accepted in the map.   
unmapped.regions <- books %>%
  filter(!map.status %in% 'in map'
         & is.na(usa)
         & is.na(can.1)
         & !is.na(region)) %>%
  select(region, n, code.status) 

# ==== Plotting ================================================================

# Adding the the region book counts to the map for visualizing. 
map <- map_data('world') %>% 
  left_join(books)

# Custom theme (blank slate)
theme_bare <- theme(
  axis.line        = element_blank(),
  axis.text.x      = element_blank(),
  axis.text.y      = element_blank(),
  axis.ticks       = element_blank(),
  axis.title.x     = element_blank(),
  axis.title.y     = element_blank(),
  legend.text      = element_text(size = 7),
  legend.title     = element_text(size = 8),
  panel.background = element_blank(),
  panel.border     = element_blank())

# The plot! 
ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = log(n)),
               color = 'gray', size = 0.1) +
  ggtitle('UWM Special Collections Holdings By Country') +
  scale_fill_gradient(high = 'darkgreen', low = 'seashell', guide = 'colorbar') +
  coord_fixed(1.3) +
  theme_bare 