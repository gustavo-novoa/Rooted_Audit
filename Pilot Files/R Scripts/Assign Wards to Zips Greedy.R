
# Assign ZIP Codes to Wards Based on Plurality of Area Overlap
# This script assigns exactly one zip code to each ward based on which zip code
# covers the largest portion of that ward's area

library(sf)
library(dplyr)

wd<-"~/Documents/GitHub/Rooted_Audit/Pilot Files"
setwd(wd)



assign_wards <- function(city_name, wards, zips){
  
  library(sf)
  library(dplyr)
  
  # Transform to projected CRS for area calculations
  wards_proj <- st_transform(wards, crs = 3435)
  zips_proj  <- st_transform(zips, crs = 3435)
  
  cat("Processing", nrow(wards_proj), "wards and", nrow(zips_proj), "ZIP codes...\n\n")
  
  # --------------------------------------------
  # STEP 1: Compute ALL ward–zip intersections
  # --------------------------------------------
  all_intersections <- st_intersection(
    wards_proj %>% select(DISTRICT),
    zips_proj %>% select(zip)
  )
  
  if (nrow(all_intersections) == 0) {
    stop("No overlaps found between wards and ZIP codes.")
  }
  
  # Calculate overlap area
  all_intersections$overlap_area <- st_area(all_intersections)
  
  # Compute total ward area for percentage calc
  ward_areas <- wards_proj %>%
    mutate(ward_area = st_area(geometry)) %>%
    st_drop_geometry() %>%
    select(DISTRICT, ward_area)
  
  # Join total ward area
  all_intersections <- all_intersections %>%
    left_join(ward_areas, by = "DISTRICT") %>%
    mutate(overlap_pct = as.numeric(overlap_area / ward_area * 100)) %>%
    st_drop_geometry()
  
  # --------------------------------------------
  # STEP 2: Rank overlaps largest → smallest
  # --------------------------------------------
  ranked_overlaps <- all_intersections %>%
    arrange(desc(overlap_area))
  
  # --------------------------------------------
  # STEP 3: Greedy assignment (no zip repeats)
  # --------------------------------------------
  assigned_zips  <- c()
  assigned_wards <- c()
  results        <- list()
  
  for (i in 1:nrow(ranked_overlaps)) {
    
    row <- ranked_overlaps[i, ]
    
    if (!(row$DISTRICT %in% assigned_wards) &&
        !(row$zip %in% assigned_zips)) {
      
      results[[length(results) + 1]] <- data.frame(
        ward         = row$DISTRICT,
        assigned_zip = row$zip,
        overlap_area = as.numeric(row$overlap_area),
        overlap_pct  = row$overlap_pct
      )
      
      assigned_wards <- c(assigned_wards, row$DISTRICT)
      assigned_zips  <- c(assigned_zips, row$zip)
    }
  }
  
  ward_zip_assignments <- bind_rows(results)
  
  # --------------------------------------------
  # STEP 4: Ensure all wards appear (even if NA)
  # --------------------------------------------
  ward_zip_assignments <- wards_proj %>%
    st_drop_geometry() %>%
    select(DISTRICT) %>%
    left_join(ward_zip_assignments,
              by = c("DISTRICT" = "ward"))
  
  # --------------------------------------------
  # Reporting
  # --------------------------------------------
  cat("\n========================================\n")
  cat("Ward to ZIP Code Assignments Complete!\n")
  cat("========================================\n\n")
  
  print(ward_zip_assignments)
  
  cat("\nSummary Statistics:\n")
  cat("-------------------\n")
  cat(sprintf("Total wards: %d\n", nrow(ward_zip_assignments)))
  cat(sprintf("Wards with ZIP assigned: %d\n",
              sum(!is.na(ward_zip_assignments$assigned_zip))))
  cat(sprintf("Wards without ZIP: %d\n",
              sum(is.na(ward_zip_assignments$assigned_zip))))
  cat(sprintf("\nAverage overlap percentage: %.2f%%\n",
              mean(ward_zip_assignments$overlap_pct, na.rm = TRUE)))
  cat(sprintf("Median overlap percentage: %.2f%%\n",
              median(ward_zip_assignments$overlap_pct, na.rm = TRUE)))
  
  # Confirm no duplicates
  cat("\nZIP codes assigned to more than one ward:\n")
  zip_counts <- ward_zip_assignments %>%
    filter(!is.na(assigned_zip)) %>%
    count(assigned_zip, name = "num_wards") %>%
    filter(num_wards > 1)
  
  print(zip_counts)   # Should now always be empty
  
  # --------------------------------------------
  # Merge back to spatial
  # --------------------------------------------
  wards_with_zips <- wards %>%
    left_join(ward_zip_assignments %>%
                select(DISTRICT, assigned_zip, overlap_pct),
              by = "DISTRICT")
  
  output_geojson <- paste0("./Merged Spatial/",
                           city_name,
                           "_wards_with_zip_assignments.geojson")
  
  st_write(wards_with_zips, output_geojson, delete_dsn = TRUE)
  
  df <- sf::st_drop_geometry(wards_with_zips)
  setwd(paste0(wd, "/Merged Lists"))
  write.csv(df, paste0(city_name, "_zip_list.csv"))
  
  cat(sprintf("Spatial data with assignments saved to: %s\n",
              output_geojson))
  cat("\nDone!\n")
  
  setwd(wd)
}


chicago_wards <-st_read("./City Council Maps/Illinois_Chicago_City_2025.geojson")
chicago_zips<- st_read("./Zip Code Maps/Chicago_ZIP_Codes.geojson")


assign_wards("chicago", chicago_wards, chicago_zips)


nashville_wards<- st_read("./City Council Maps/Tennessee_NashvilleDavidsonMetro_City_2022.geojson")
nashville_zips<-st_read("./Zip Code Maps/Nashville_ZIP_Codes.geojson")

assign_wards("nashville", nashville_wards, nashville_zips)

