library("terra")
library("tidyterra")
library("dplyr")

if (Sys.info()["sysname"] == "Windows") {
  dir_lud11 <- "L:"
} else {
  dir_lud11 <- "/lud11"
}

recompute <- FALSE

dir_biomes <- file.path(dir_lud11, "lechartr", "sharing")
dir_disturbances <- file.path(
  dir_lud11, "poppman/data/rsm/dat/lud11/EUForestDisturbAtlas/annual"
)
dir_out <- file.path(
  dir_lud11, "poppman", "data", "rsm", "dat", "lud11", "disturbance_events"
)

dir.create(dir_out, showWarnings = FALSE)

f_disturbances <- list.files(
  dir_disturbances, pattern = ".tif$", full.names = TRUE
  )

f_biomes_t0 <- file.path(dir_biomes, "output_BIOME4_1951-1980.nc")
f_biomes_t1 <- file.path(dir_biomes, "output_BIOME4_1981_2010.nc")

# Load disturbance data---------------------------------------------------------
disturbances <- terra::rast(f_disturbances)

# Load and prepare biome data---------------------------------------------------
ext_d <- terra::ext(disturbances) %>%
  terra::project(
    from = terra::crs(disturbances),
    to = terra::crs(terra::rast(f_biomes_t0)[["biome"]])
    )

biomes_t0 <- terra::rast(f_biomes_t0)[["biome"]] %>%
  terra::crop(ext_d) %>%
  terra::project(disturbances, method = "near") %>%
  terra::resample(disturbances, method = "near")

biomes_t1 <- terra::rast(f_biomes_t1)[["biome"]] %>%
  terra::crop(ext_d) %>%
  terra::project(disturbances, method = "near") %>%
  terra::resample(disturbances, method = "near")

terra::NAflag(biomes_t0) <- -9999
terra::NAflag(biomes_t1) <- -9999

# Combine data------------------------------------------------------------------
biome_ids <- terra::unique(biomes_t0)

for (biome_id in biome_ids) {
  # Identify core and shift areas of the biome
  biome_t0 <- (biomes_t0 == biome_id)
  biome_t1 <- (biomes_t1 == biome_id)
  biome_core <- (biome_t0 & biome_t1)
  biome_shift <- (biome_t0 & !biome_core)
  
  # Sample disturbances in core and shift areas
  disturbances_core <- terra::mask(
    disturbances, mask = biome_shift, maskvalue = FALSE, updatevalue = NA
  )
  
  disturbances_shift <- terra::mask(
    disturbances, mask = biome_shift, maskvalue = TRUE, updatevalue = NA
  )
  
  f_out_core <- file.path(dir_out, paste0("FreqTabEU", biome_id, "Core.csv"))
  if (!file.exists(f_out_core) | recompute) {
    freq_core <- terra::freq(disturbances_core)
    write.csv(freq_core, file = f_out_core, row.names = FALSE)
  }
  
  f_out_shift <- file.path(dir_out, paste0("FreqTabEU", biome_id, "Shift.csv"))
  if (!file.exists(f_out_shift) | recompute) {
    freq_shift <- terra::freq(disturbances_shift)
    write.csv(freq_shift, file = f_out_shift, row.names = FALSE)
  }
}
