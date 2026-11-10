library(dplyr)
library(forcats)
library(sf)
library(strayr)

# State geography import
state_sf <- read_absmap("state2021", remove_year_suffix = TRUE) |>
  select(state_name) |>
  filter(state_name != "Other Territories")


# Place geography import
place_levels <- c("national_capital", "city", "town", "village", 
                   "hamlet", "suburb", "island", "farm", "region", "locality")

places <- read_sf("data-raw/gis_osm_places_free_1.shp") |>
  select(fclass, name, geometry) |>
  mutate(
    fclass = fct_relevel(fclass, place_levels)
  )


# Ensure both spatial datasets use the same CRS.
state_sf <- st_transform(state_sf, st_crs(places))


# Assign each place to its nearest state
nearest_state_id <- st_nearest_feature(places, state_sf)
places$state_name <- st_drop_geometry(state_sf)$state_name[nearest_state_id]
places$state_name <- state_name(places$state_name)
places$state_abbr <- state_abbr(places$state_name)

# Keep places sufficiently close to a state and west of 158°E
max_state_distance <- 100 * 1000  # metres
max_longitude <- 158              # degrees east

places_filtered <- places |>
  filter(
    lengths(
      st_is_within_distance(
        geometry,
        state_sf,
        dist = max_state_distance
      )
    ) > 0
  ) |>
  mutate(longitude = st_coordinates(geometry)[, 1]) |>
  filter(longitude < max_longitude) |>
  select(-longitude)


excluded_places <- anti_join(
  places,
  as.tibble(places_filtered),
  by = c("fclass", "name")
)

message(nrow(excluded_places), " places excluded")


places_in_australia <- places_filtered |> 
  arrange(fclass, state_abbr, name) |> 
  relocate(geometry, .after = last_col())

usethis::use_data(places_in_australia, overwrite = TRUE)
