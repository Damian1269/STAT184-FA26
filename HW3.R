library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(tibble)
library(ggplot2)

dir.create("HW3_output", showWarnings = FALSE)

# Problem 1
# Part A

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

p_a <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~drv) +
  labs(
    title = "Engine size and highway fuel economy by drive type",
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "drv"
  ) +
  theme_bw()

print(p_a)
ggsave("HW3_output/p_a.png", p_a, width = 9, height = 5, dpi = 300)

# There are three fitted lines because facet_wrap creates a separate panel
# for each drive type. geom_smooth fits a line using the data in each panel.
# The lines are blue because color = "blue" is set in the smooth layer.

# Part B
p_b_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(
    title = "Vehicle counts by drive type and number of cylinders",
    x = "drv",
    y = "count",
    fill = "factor(cyl)"
  ) +
  theme_bw()

print(p_b_count)
ggsave("HW3_output/p_b_count.png", p_b_count, width = 7, height = 5, dpi = 300)

p_b_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of cylinder groups within each drive type",
    x = "drv",
    y = "Proportion within drive type",
    fill = "factor(cyl)"
  ) +
  theme_bw()

print(p_b_prop)
ggsave("HW3_output/p_b_prop.png", p_b_prop, width = 7, height = 5, dpi = 300)

# The proportion plot makes each bar total 1. This shows the mix of
# cylinder groups within each drive type, not the number of vehicles.
# The original counts can differ even though the proportion bars have equal heights.

# Part C

gems_small <- gems %>%
  filter(carat <= 2.5)

n_original <- nrow(gems)
n_retained <- nrow(gems_small)
n_excluded <- n_original - n_retained

cat("\nProblem 1C record counts\n")
cat("Original diamonds records:", n_original, "\n")
cat("Retained in gems_small (carat <= 2.5):", n_retained, "\n")
cat("Excluded:", n_excluded, "\n")

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right",
                 color = "white") +
  coord_cartesian(xlim = c(0, 2.5)) +
  labs(
    title = "Diamond carat distribution (fine bins)",
    x = "Carat",
    y = "Count",
    subtitle = "Bin width = 0.01"
  ) +
  theme_bw()

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right",
                 color = "white") +
  coord_cartesian(xlim = c(0, 2.5)) +
  labs(
    title = "Diamond carat distribution (coarse bins)",
    x = "Carat",
    y = "Count",
    subtitle = "Bin width = 0.5"
  ) +
  theme_bw()

print(p_fine)
print(p_coarse)
ggsave("HW3_output/p_fine.png", p_fine, width = 8, height = 5, dpi = 300)
ggsave("HW3_output/p_coarse.png", p_coarse, width = 8, height = 5, dpi = 300)

carat_counts <- gems %>%
  filter(carat %in% c(0.99, 1.00)) %>%
  count(carat, name = "number_of_diamonds")

cat("\nOriginal diamonds recorded as exactly 0.99 and 1.00 carat:\n")
print(carat_counts)

# Narrower bins make small changes and peaks in the carat distribution easier
# to see. They can reveal clusters around particular recorded carat values.
# Wider bins summarize the overall shape but hide more fine detail.

# Problem 2

library_url <- "https://data.cityofchicago.org/api/v3/views/wa2i-tm5d/query.geojson?accessType=DOWNLOAD"

boundary_url <- "https://data.cityofchicago.org/api/v3/views/igwz-8jzy/query.geojson?accessType=DOWNLOAD"

libraries_raw <- tryCatch(
  st_read(library_url, quiet = TRUE),
  error = function(e) {
    message("Could not read the library URL directly. Download the library dataset as GeoJSON from https://data.cityofchicago.org/d/wa2i-tm5d and read it with st_read('your_file.geojson').")
    stop(e)
  }
)

areas_raw <- tryCatch(
  st_read(boundary_url, quiet = TRUE),
  error = function(e) {
    message("Could not read the boundary URL directly. Download GeoJSON from https://data.cityofchicago.org/d/igwz-8jzy and read it with st_read('your_file.geojson').")
    stop(e)
  }
)

st_write(libraries_raw, "HW3_output/libraries_raw.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(areas_raw, "HW3_output/community_areas_raw.gpkg", delete_dsn = TRUE, quiet = TRUE)

data_sources <- tibble(
  dataset = c("Chicago Public Library locations", "Chicago community area boundaries"),
  publisher = c("Chicago Public Library / City of Chicago", "City of Chicago"),
  url = c("https://data.cityofchicago.org/d/wa2i-tm5d",
          "https://data.cityofchicago.org/d/igwz-8jzy"),
  access_date = as.character(Sys.Date()),
  format = c("GeoJSON", "GeoJSON")
)
write_csv(data_sources, "HW3_output/data_sources.csv")

cat("\nLibrary data fields:\n")
print(names(libraries_raw))
cat("\nBoundary data fields:\n")
print(names(areas_raw))

target_crs <- 26916
libraries_projected <- st_transform(libraries_raw, target_crs)
areas_projected <- st_transform(areas_raw, target_crs)

name_field <- "branch_"

libraries_projected$site_name <- as.character(libraries_projected[[name_field]])
libraries_projected$site_id <- paste0("LIB", seq_len(nrow(libraries_projected)))

n_before <- nrow(libraries_projected)
empty_geom <- st_is_empty(libraries_projected)
missing_name <- is.na(libraries_projected$site_name) |
  trimws(libraries_projected$site_name) == ""
bad_geom <- is.na(empty_geom) | empty_geom
bad_coord <- !st_is_valid(libraries_projected)
remove_rows <- missing_name | bad_geom | bad_coord

cleaning_summary <- tibble(
  measure = c("Records before cleaning", "Missing/blank names", "Empty/invalid geometry flagged",
              "Records removed", "Records retained"),
  count = c(n_before, sum(missing_name, na.rm = TRUE),
            sum(bad_geom | bad_coord, na.rm = TRUE),
            sum(remove_rows, na.rm = TRUE), sum(!remove_rows, na.rm = TRUE))
)
print(cleaning_summary)
write_csv(cleaning_summary, "HW3_output/library_cleaning_summary.csv")

sites_m <- libraries_projected[!remove_rows, ] %>%
  select(site_id, site_name, geometry)

site_coords <- st_coordinates(sites_m)
coord_key <- paste(round(site_coords[, 1], 2), round(site_coords[, 2], 2), sep = "_")
duplicate_coordinate_records <- sum(duplicated(coord_key))
cat("\nRetained library sites:", nrow(sites_m), "\n")
cat("Records sharing coordinates with an earlier record (not removed):",
    duplicate_coordinate_records, "\n")
cat("Projected CRS: EPSG:", target_crs, "; units: metres\n", sep = "")

st_write(sites_m, "HW3_output/sites_m.gpkg", delete_dsn = TRUE, quiet = TRUE)

p_sites <- ggplot() +
  geom_sf(data = sites_m, color = "darkred", size = 1.4) +
  labs(
    title = "Chicago Public Library locations",
    subtitle = paste("Retained facilities:", nrow(sites_m)),
    caption = paste("Source: Chicago Data Portal; accessed", Sys.Date()),
    x = NULL, y = NULL
  ) +
  theme_bw()

print(p_sites)
ggsave("HW3_output/p_sites.png", p_sites, width = 8, height = 7, dpi = 300)

buffer_500 <- st_buffer(sites_m, dist = 500)
buffer_1000 <- st_buffer(sites_m, dist = 1000)

union_500 <- st_union(buffer_500)
union_1000 <- st_union(buffer_1000)

p_buffers <- ggplot() +
  geom_sf(data = st_as_sf(st_sfc(union_500, crs = target_crs)),
          fill = "skyblue", color = "steelblue", alpha = 0.35) +
  geom_sf(data = sites_m, color = "darkred", size = 1.3) +
  labs(
    title = "Chicago libraries and combined 500-metre buffers",
    caption = paste("Source: Chicago Data Portal; accessed", Sys.Date()),
    x = NULL, y = NULL
  ) +
  theme_bw()

print(p_buffers)
ggsave("HW3_output/p_buffers.png", p_buffers, width = 8, height = 7, dpi = 300)

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(union_500)),
                     as.numeric(st_area(union_1000))) / 1000000
)
print(buffer_summary)
write_csv(buffer_summary, "HW3_output/buffer_summary.csv")

if (buffer_summary$union_area_km2[2] < buffer_summary$union_area_km2[1]) {
  warning("The 1000-metre buffer area is unexpectedly smaller than the 500-metre area.")
}

st_write(st_as_sf(st_sfc(union_500, crs = target_crs)),
         "HW3_output/buffer_500_union.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(st_as_sf(st_sfc(union_1000, crs = target_crs)),
         "HW3_output/buffer_1000_union.gpkg", delete_dsn = TRUE, quiet = TRUE)

# Overlapping buffers should be combined so the same piece of land is not counted
# more than once. Straight-line buffers do not prove walking access because they
# do not account for streets, barriers, entrances, or travel routes. The buffer
# areas include land outside the study boundary, so the reported area is not limited
# to land inside Chicago's community-area polygons.

# Problem 3

area_fields <- names(areas_projected)
id_candidates <- c("area_numbe", "area_num_1", "area_num", "area_id", "community_area", "objectid", "OBJECTID")
name_candidates <- c("community", "area_name", "name", "community_area_name", "pri_neigh")

district_id_field <- id_candidates[id_candidates %in% area_fields][1]
district_name_field <- name_candidates[name_candidates %in% area_fields][1]

if (is.na(district_id_field) || is.na(district_name_field)) {
  stop(paste(
    "Could not automatically identify community-area ID/name fields.",
    "Run names(areas_raw), then set district_id_field and district_name_field",
    "to the correct field names."
  ))
}

districts <- areas_projected %>%
  mutate(
    district_id = as.character(.data[[district_id_field]]),
    district_name = as.character(.data[[district_name_field]])
  ) %>%
  select(district_id, district_name, geometry)

area_empty <- st_is_empty(districts)
area_valid <- st_is_valid(districts)
area_check <- tibble(
  check = c("Number of boundary records",
            "Missing/blank IDs",
            "Duplicate IDs",
            "Empty geometries",
            "Invalid geometries before repair"),
  count = c(nrow(districts),
            sum(is.na(districts$district_id) | districts$district_id == ""),
            sum(duplicated(districts$district_id)),
            sum(area_empty, na.rm = TRUE),
            sum(!area_valid, na.rm = TRUE))
)
print(area_check)
write_csv(area_check, "HW3_output/boundary_checks.csv")

districts <- st_make_valid(districts)
districts <- districts[
  !is.na(districts$district_id) &
    districts$district_id != "" &
    !st_is_empty(districts), ]

p_district_sites <- ggplot() +
  geom_sf(data = districts, fill = "white", color = "gray50") +
  geom_sf(data = sites_m, color = "red", size = 1.5) +
  labs(
    title = "Chicago libraries and community areas",
    caption = paste("Source: City of Chicago; accessed", Sys.Date())
  ) +
  theme_bw()

print(p_district_sites)

ggsave("HW3_output/p_district_sites.png",
       p_district_sites, width = 8, height = 7, dpi = 300)

matches <- st_intersects(sites_m, districts)
match_count <- lengths(matches)

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  n_area_matches = match_count
)
write_csv(site_match_audit, "HW3_output/site_match_audit.csv")

cat("\nSite-to-area matching audit:\n")
cat("Sites matching zero areas:", sum(match_count == 0), "\n")
cat("Sites matching exactly one area:", sum(match_count == 1), "\n")
cat("Sites matching multiple areas:", sum(match_count > 1), "\n")
cat("Sites not matching exactly one area:", sum(match_count != 1), "\n")

assigned_rows <- which(match_count == 1)
assigned_sites <- sites_m[assigned_rows, ]
assigned_area_index <- vapply(matches[assigned_rows], function(x) x[1], integer(1))

assigned_counts <- tibble(
  district_id = districts$district_id[assigned_area_index]
) %>%
  count(district_id, name = "n_sites")

district_counts <- districts %>%
  left_join(assigned_counts, by = "district_id") %>%
  mutate(
    n_sites = replace_na(n_sites, 0L),
    area_km2 = as.numeric(st_area(geometry)) / 1000000,
    sites_per_km2 = n_sites / area_km2
  ) %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2, geometry)
  print(st_drop_geometry(district_counts), n = Inf)

cat("\nAll area values positive:", all(district_counts$area_km2 > 0), "\n")
cat("Total assigned sites:", sum(district_counts$n_sites), "\n")
cat("Number of sites assigned exactly once:", sum(match_count == 1), "\n")
stopifnot(all(district_counts$area_km2 > 0))
stopifnot(sum(district_counts$n_sites) == sum(match_count == 1))

st_write(district_counts, "HW3_output/district_counts.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
write_csv(st_drop_geometry(district_counts), "HW3_output/district_counts.csv")

p_count <- ggplot(district_counts) +
  geom_sf(aes(fill = n_sites), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "lightyellow", high = "darkred",
    name = "Facilities\n(count)"
  ) +
  labs(
    title = "Chicago library counts by community area",
    caption = paste("Source: City of Chicago; accessed", Sys.Date(),
                    "| Sites not assigned exactly once:", sum(match_count != 1)),
    x = NULL, y = NULL
  ) +
  theme_bw()

print(p_count)
ggsave("HW3_output/p_count.png", p_count, width = 8, height = 7, dpi = 300)

p_density <- ggplot(district_counts) +
  geom_sf(aes(fill = sites_per_km2), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "lightyellow", high = "darkred",
    name = "Facilities per\nkm²"
  ) +
  labs(
    title = "Chicago library density by community area",
    caption = paste("Source: City of Chicago; accessed", Sys.Date(),
                    "| Sites not assigned exactly once:", sum(match_count != 1)),
    x = NULL, y = NULL
  ) +
  theme_bw()

print(p_density)
ggsave("HW3_output/p_density.png", p_density, width = 8, height = 7, dpi = 300)

highest_count <- max(district_counts$n_sites, na.rm = TRUE)
highest_density <- max(district_counts$sites_per_km2, na.rm = TRUE)

cat("\nArea(s) with the highest facility count:\n")
print(district_counts %>%
        st_drop_geometry() %>%
        filter(n_sites == highest_count))

cat("\nArea(s) with the highest facility density:\n")
print(district_counts %>%
        st_drop_geometry() %>%
        filter(abs(sites_per_km2 - highest_density) < 1e-10))

# The area(s) with the highest count have the most libraries in total, while the
# area(s) with the highest density have the most libraries per square kilometre.
# A zero count means no retained library point was assigned to that polygon; it
# does not prove residents have no access to a library outside the polygon.
# Density is not facilities per resident and says nothing by itself about service
# quality. Community-area boundaries and the inclusion of parks, water, or other
# unpopulated land can affect the area denominator and therefore density.