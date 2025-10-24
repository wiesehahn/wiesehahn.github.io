# pak::pak("USDAForestService/gdalraster")
library(gdalraster)
library(purrr)
library(fs)

# Set gdal configurations (for reproducability?)
# Multi-threaded (comment GDAL_NUM_THREADS to use default single threaded
set_config_option("GDAL_NUM_THREADS", "32")
set_config_option("GDAL_CACHEMAX", "8000")
set_config_option("OVERVIEWS", "IGNORE_EXISTING")

# get orthoimages from LGLN open data
urls <- c(
  "https://dop20-rgbi.s3.eu-de.cloud-object-storage.appdomain.cloud/324905842/2024-09-05/dop20rgbi_32_490_5842_2_ni_2024-09-05.tif",
  "https://dop20-rgbi.s3.eu-de.cloud-object-storage.appdomain.cloud/326085740/2022-05-09/dop20rgbi_32_608_5740_2_ni_2022-05-09.tif",
  "https://dop20-rgbi.s3.eu-de.cloud-object-storage.appdomain.cloud/326085732/2022-05-09/dop20rgbi_32_608_5732_2_ni_2022-05-09.tif",
  "https://dop20-rgbi.s3.eu-de.cloud-object-storage.appdomain.cloud/326125846/2024-09-21/dop20rgbi_32_612_5846_2_ni_2024-09-21.tif",
  "https://dop20-rgbi.s3.eu-de.cloud-object-storage.appdomain.cloud/326045852/2024-09-21/dop20rgbi_32_604_5852_2_ni_2024-09-21.tif"
)

dsns <- paste0("/vsicurl/", urls) # prefix for virtual file source



# Create a function to measure performance for each datasource and compression
raster_compression <- function(dsns, options) {
  results <- list()

  for (datasourcename in dsns) {
    for (option in options) {
      output_file <- file_temp(ext = ".tif")

      write_ds <- function() {
        createCopy(
          src_filename = datasourcename,
          dst_filename = output_file,
          format = "COG",
          options = option$setting
        )
      }

      # Get write time
      writetime <- system.time(write_ds())["elapsed"]

      # Get file size in MB
      data_size_uncompressed <- vsi_stat(datasourcename, "size") / (1024^2)
      data_size_compressed <- vsi_stat(output_file, "size") / (1024^2)

      # Get read time
      img <- new(GDALRaster, output_file)
      readtime <- system.time(read_ds(img))["elapsed"]

      # Store results
      results[[length(results) + 1]] <- list(
        datasource = basename(datasourcename),
        settings = option$naming,
        uncompressed_mb = data_size_uncompressed,
        compressed_mb = data_size_compressed,
        writetime_sec = writetime,
        readtime_sec = readtime
      )
    }
  }

  # Convert list to data frame
  results_df <- do.call(rbind, lapply(results, data.frame))
  return(results_df)
}



# create list of options
options <- list(
  # lossless
  list(setting = c("COMPRESS=NONE")),
  list(setting = c("COMPRESS=LZW", "PREDICTOR=YES")),
  list(setting = c("COMPRESS=LZW", "PREDICTOR=NO")),
  list(setting = c("COMPRESS=DEFLATE", "PREDICTOR=YES", "LEVEL=1")),
  list(setting = c("COMPRESS=DEFLATE", "PREDICTOR=YES", "LEVEL=6")),
  list(setting = c("COMPRESS=DEFLATE", "PREDICTOR=YES", "LEVEL=9")),
  list(setting = c("COMPRESS=DEFLATE", "PREDICTOR=NO", "LEVEL=1")),
  list(setting = c("COMPRESS=DEFLATE", "PREDICTOR=NO", "LEVEL=6")),
  list(setting = c("COMPRESS=DEFLATE", "PREDICTOR=NO", "LEVEL=9")),
  list(setting = c("COMPRESS=LZMA", "LEVEL=1")),
  list(setting = c("COMPRESS=LZMA", "LEVEL=9")),
  list(setting = c("COMPRESS=ZSTD", "PREDICTOR=YES", "LEVEL=1")),
  list(setting = c("COMPRESS=ZSTD", "PREDICTOR=YES", "LEVEL=9")),
  list(setting = c("COMPRESS=ZSTD", "PREDICTOR=YES", "LEVEL=22")),
  list(setting = c("COMPRESS=ZSTD", "PREDICTOR=NO", "LEVEL=1")),
  list(setting = c("COMPRESS=ZSTD", "PREDICTOR=NO", "LEVEL=9")),
  list(setting = c("COMPRESS=ZSTD", "PREDICTOR=NO", "LEVEL=22")),
  list(setting = c("COMPRESS=WEBP", "QUALITY=100")),
  list(setting = c("COMPRESS=LERC", "MAX_Z_ERROR=0")),
  list(setting = c("COMPRESS=LERC_DEFLATE", "LEVEL=1")),
  list(setting = c("COMPRESS=LERC_DEFLATE", "LEVEL=6")),
  list(setting = c("COMPRESS=LERC_DEFLATE", "LEVEL=9")),
  list(setting = c("COMPRESS=LERC_ZSTD", "LEVEL=1")),
  list(setting = c("COMPRESS=LERC_ZSTD", "LEVEL=9")),
  list(setting = c("COMPRESS=LERC_ZSTD", "LEVEL=22")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=YES", "JXL_EFFORT=1")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=YES", "JXL_EFFORT=5")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=YES", "JXL_EFFORT=9")),
  # lossy overviews
  list(setting = c("COMPRESS=WEBP", "QUALITY=100", "OVERVIEW_QUALITY=75")),
  # lossy
  list(setting = c("COMPRESS=WEBP", "QUALITY=75"))
  # only available in certain gdal builds
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=NO", "JXL_EFFORT=1")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=NO", "JXL_EFFORT=5")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=NO", "JXL_EFFORT=9")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=NO", "JXL_EFFORT=5", "JXL_DISTANCE=2"))
)

for (i in 1:length(options)) {
  options[[i]]$naming <- paste(options[[i]]$setting, collapse = ", ")
}


# apply function on list
benchmark_results <- raster_compression(dsns, options)



# plot
library(gt)
library(dplyr)

benchmark_results |>
  mutate(
    relative_size = compressed_mb / uncompressed_mb,
    relative_write_speed = uncompressed_mb / writetime_sec,
    relative_read_speed = compressed_mb / readtime_sec
  ) |>
  group_by(settings) |>
  summarise(across(c(compressed_mb, relative_size, writetime_sec, readtime_sec, relative_write_speed, relative_read_speed),
    list(mean = mean, sd = sd),
    .names = "{.col}_{.fn}"
  )) |>
  arrange(desc(relative_size_mean)) |>
  # Create the gt table
  gt() |>
  fmt_number(columns = ends_with(c("_mean", "_sd")), decimals = 0) |>
  fmt_number(columns = contains(c("time", "speed")), decimals = 1) |>
  fmt_number(
    columns = contains("size"),
    scale_by = 100,
    decimals = 0
  ) |>
  cols_merge_uncert(
    col_val = compressed_mb_mean,
    col_uncert = compressed_mb_sd
  ) |>
  cols_merge_uncert(
    col_val = relative_size_mean,
    col_uncert = relative_size_sd
  ) |>
  cols_merge_uncert(
    col_val = readtime_sec_mean,
    col_uncert = readtime_sec_sd
  ) |>
  cols_merge_uncert(
    col_val = writetime_sec_mean,
    col_uncert = writetime_sec_sd
  ) |>
  cols_merge_uncert(
    col_val = relative_write_speed_mean,
    col_uncert = relative_write_speed_sd
  ) |>
  cols_merge_uncert(
    col_val = relative_read_speed_mean,
    col_uncert = relative_read_speed_sd
  ) |>
  cols_label(
    settings = "Settings",
    compressed_mb_mean = "Absolute (MB)",
    relative_size_mean = "Relative (%)",
    writetime_sec_mean = "Write (s)",
    readtime_sec_mean = "Read (s)",
    relative_write_speed_mean = "Write (MB/s)",
    relative_read_speed_mean = "Read (MB/s)"
  ) |>
  tab_spanner(
    label = "Size",
    columns = c(compressed_mb_mean, relative_size_mean)
  ) |>
  tab_spanner(
    label = "Time",
    columns = c(writetime_sec_mean, readtime_sec_mean)
  ) |>
  tab_spanner(
    label = "Speed",
    columns = c(relative_write_speed_mean, relative_read_speed_mean)
  ) |>
  tab_header(
    title = "Comparison of Compression Performances",
    subtitle = "Size and processing time (mean ± standard deviation) depending on compression settings",
  ) |>
  tab_footnote(
    footnote = "WEBP compression with QUALITY < 100 implies lossy compression, all other tested algorithms are lossless compression",
    locations = cells_column_labels(columns = settings)
  ) |>
  tab_source_note(source_note = md(
    "Source: Original images were five uncompressed Orthophotos (2x2km) from LGLN. Original images were stored as uncompressed COGs (size: 539 ± 0 MB)."
  ))
