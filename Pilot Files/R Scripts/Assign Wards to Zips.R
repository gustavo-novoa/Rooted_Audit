# Assign ZIP Codes to Wards Based on Plurality of Area Overlap
# This script assigns exactly one zip code to each ward based on which zip code
# covers the largest portion of that ward's area

library(sf)
library(dplyr)

wd<-"~/Documents/GitHub/Rooted_Audit/Pilot Files"
setwd(wd)


assign_wards<-function(city_name, wards, zips){


# Transform to projected CRS for area calculations
wards_proj <- st_transform(wards, crs = 3435)
zips_proj <- st_transform(zips, crs = 3435)

cat("Processing", nrow(wards_proj), "wards and", nrow(zips_proj), "ZIP codes...\n\n")

# Function to find the zip code with plurality overlap for a given ward
assign_zip_to_ward <- function(ward_geom, ward_id, zips_sf) {
  intersections <- st_intersection(ward_geom, zips_sf)
  
  if (nrow(intersections) == 0) {
    return(data.frame(ward = ward_id, assigned_zip = NA, overlap_area = 0, overlap_pct = 0))
  }
  
  intersections$intersection_area <- st_area(intersections)
  
  max_overlap_idx <- which.max(intersections$intersection_area)
  max_overlap <- intersections[max_overlap_idx, ]
  
  # Percentage of ward area covered by the winning zip code
  ward_total_area <- st_area(ward_geom)
  overlap_pct <- as.numeric(max_overlap$intersection_area / ward_total_area * 100)
  
  return(data.frame(
    ward = ward_id,
    assigned_zip = max_overlap$zip,
    overlap_area = as.numeric(max_overlap$intersection_area),
    overlap_pct = overlap_pct
  ))
}

# Process each ward
results_list <- list()

for (i in 1:nrow(wards_proj)) {
  ward_id <- wards_proj$DISTRICT[i]
  ward_geom <- wards_proj[i, ]
  
  cat(sprintf("Processing Ward %s (%d of %d)...\n", ward_id, i, nrow(wards_proj)))
  
  result <- assign_zip_to_ward(ward_geom, ward_id, zips_proj)
  results_list[[i]] <- result
}

# Combine all results
ward_zip_assignments <- bind_rows(results_list) %>%
  arrange(ward)
ward_zip_assignments$DISTRICT<-ward_zip_assignments$ward

# Display results
cat("\n========================================\n")
cat("Ward to ZIP Code Assignments Complete!\n")
cat("========================================\n\n")

print(ward_zip_assignments)

cat("\nSummary Statistics:\n")
cat("-------------------\n")
cat(sprintf("Total wards: %d\n", nrow(ward_zip_assignments)))
cat(sprintf("Wards with ZIP assigned: %d\n", sum(!is.na(ward_zip_assignments$assigned_zip))))
cat(sprintf("Wards without ZIP: %d\n", sum(is.na(ward_zip_assignments$assigned_zip))))
cat(sprintf("\nAverage overlap percentage: %.2f%%\n", mean(ward_zip_assignments$overlap_pct, na.rm = TRUE)))
cat(sprintf("Median overlap percentage: %.2f%%\n", median(ward_zip_assignments$overlap_pct, na.rm = TRUE)))

# Note any ZIP codes assigned to multiple wards
cat("\nZIP codes assigned to more than one ward:\n")
zip_counts <- ward_zip_assignments %>%
  filter(!is.na(assigned_zip)) %>%
  count(assigned_zip, name = "num_wards") %>%
  filter(num_wards > 1) %>%
  arrange(desc(num_wards))
print(zip_counts)

# Create a merged spatial file with zip assignments
wards_with_zips <- wards %>%
  left_join(ward_zip_assignments %>% select(DISTRICT, assigned_zip, overlap_pct),
            by = "DISTRICT")

# Save as GeoJSON
output_geojson <- paste0("./Merged Spatial/", city_name, "_wards_with_zip_assignments.geojson")
st_write(wards_with_zips, output_geojson, delete_dsn = TRUE)
df<-sf::st_drop_geometry(wards_with_zips)
setwd(paste0(wd,"/Merged Lists"))
write.csv(df, paste0(city_name, "_zip_list.csv"))
cat(sprintf("Spatial data with assignments saved to: %s\n", output_geojson))

cat("\nDone!\n")

setwd(wd)

}

chicago_wards <-st_read("./City Council Maps/Illinois_Chicago_City_2025.geojson")
  chicago_zips<- st_read("./Zip Code Maps/Chicago_ZIP_Codes.geojson")
  
  
  assign_wards("chicago", chicago_wards, chicago_zips)
  

nashville_wards<- st_read("./City Council Maps/Tennessee_NashvilleDavidsonMetro_City_2022.geojson")
  nashville_zips<-st_read("./Zip Code Maps/Nashville_ZIP_Codes.geojson")
  
  assign_wards("nashville", nashville_wards, nashville_zips)
  
  
  