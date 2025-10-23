# pak::pak("USDAForestService/gdalraster")
library(gdalraster)
library(purrr)
library(fs)

# plot
library(gt)
library(dplyr)


# Sset gdal configurations (for reproducability?)
set_config_option("GDAL_NUM_THREADS", "16")
set_config_option("GDAL_CACHEMAX", "4000")
set_config_option("OVERVIEWS", "IGNORE_EXISTING")

urls <- c(
  "https://dgm.s3.eu-de.cloud-object-storage.appdomain.cloud/325735712/2016-04-04/dgm1_32_573_5712_1_ni_2016.tif",
  "https://dgm.s3.eu-de.cloud-object-storage.appdomain.cloud/326025743/2018-04-09/dgm1_32_602_5743_1_ni_2018.tif",
  "https://dgm.s3.eu-de.cloud-object-storage.appdomain.cloud/326065787/2019-02-27/dgm1_32_606_5787_1_ni_2019.tif",
  "https://dgm.s3.eu-de.cloud-object-storage.appdomain.cloud/326065850/2020-04-07/dgm1_32_606_5850_1_ni_2020.tif",
  "https://dgm.s3.eu-de.cloud-object-storage.appdomain.cloud/325045930/2017-02-15/dgm1_32_504_5930_1_ni_2017.tif")

dsns <- paste0("/vsicurl/", urls)  # prefix for virtual file source




# Create a function to measure performance for each datasource and compression
raster_compression <- function(dsns, options) {
  results <- list()
  
  for (datasourcename in dsns) {
    for (option in options) {
      
      output_file <- file_temp(ext = ".tif")
      
      write_ds <- function(){
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
  list(setting = c("COMPRESS=LERC", "MAX_Z_ERROR=0")),
  list(setting = c("COMPRESS=LERC_DEFLATE", "LEVEL=1")),
  list(setting = c("COMPRESS=LERC_DEFLATE", "LEVEL=6")),
  list(setting = c("COMPRESS=LERC_DEFLATE", "LEVEL=9")),
  list(setting = c("COMPRESS=LERC_ZSTD", "LEVEL=1")),
  list(setting = c("COMPRESS=LERC_ZSTD", "LEVEL=9")),
  list(setting = c("COMPRESS=LERC_ZSTD", "LEVEL=22"))
)

for (i in 1:length(options)) {
  options[[i]]$naming <- paste(options[[i]]$setting, collapse = ", ")
}


# apply function on list
benchmark_results <- raster_compression(dsns, options)


benchmark_results |> 
  mutate(relative_size = compressed_mb / uncompressed_mb) |> 
  group_by(settings) |> 
  summarise(across(c(compressed_mb, relative_size, writetime_sec, readtime_sec), 
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}")) |> 
  arrange(desc(relative_size_mean)) |>
  # Create the gt table
  gt() |> 
  fmt_number(columns = ends_with("_mean"), decimals = 2) |> 
  fmt_number(columns = ends_with("_sd"), decimals = 2) |>
  fmt_number(
    columns = contains("size"),
    scale_by = 100,
    decimals = 1
  ) |>
  cols_merge_uncert(
    col_val = compressed_mb_mean,
    col_uncert = compressed_mb_sd
  ) |>
  cols_merge_uncert(
    col_val = relative_size_mean,
    col_uncert = relative_size_sd
  )  |>
  cols_merge_uncert(
    col_val = readtime_sec_mean,
    col_uncert = readtime_sec_sd
  )  |>
  cols_merge_uncert(
    col_val = writetime_sec_mean,
    col_uncert = writetime_sec_sd
  )   |>
  cols_label(
    settings = "Settings",
    compressed_mb_mean = "Absolute Size (MB)",
    relative_size_mean = "Relative Size (%)",
    writetime_sec_mean = "Runtime Write (s)",
    readtime_sec_mean = "Runtime Read (s)"
  ) |>
  tab_header(
    title = "Comparison of Compression Performances",
    subtitle = "Size and processing time (mean ± standard deviation) depending on compression settings",
  ) |>
  tab_footnote(
    footnote = "All tested algorithms are lossless compression, WEBP which performes best with Orthoimages is not possible with 1-band data.",
    locations = cells_column_labels(columns = settings)
  )






# create list of options for RGB encoding
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
  # only available with certain gdal builds
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=YES", "JXL_EFFORT=1")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=YES", "JXL_EFFORT=5")),
  # list(setting = c("COMPRESS=JXL", "JXL_LOSSLESS=YES", "JXL_EFFORT=9")),
  # lossy
  list(setting = c("COMPRESS=WEBP", "QUALITY=100", "DISCARD_LSB=0,0,1")),
  list(setting = c("COMPRESS=WEBP", "QUALITY=100", "DISCARD_LSB=0,0,2")),
  list(setting = c("COMPRESS=WEBP", "QUALITY=100", "DISCARD_LSB=0,0,3"))
)

for (i in 1:length(options)) {
  options[[i]]$naming <- paste(options[[i]]$setting, collapse = ", ")
}



#### benchmark mapbox encoding

# Create a function to measure performance for each datasource and compression
raster_compression <- function(dsns, options) {
  results <- list()
  
  for (datasourcename in dsns) {
    
    
    # convert DTM to RGB encoded (Mapbox)
    mapbox_encoded <- "to_terrainrgb <- function(dtm) {
        startingvalue <- 10000
        precision <- 0.1
        rfactor <- 256*256 * precision
        gfactor <- 256 * precision
        r <- floor((startingvalue +dtm)*(1/precision) / 256 / 256)
        g <- floor((startingvalue +dtm - r*rfactor)*(1/precision) / 256)
        b <- floor((startingvalue +dtm - r*rfactor - g*gfactor)*(1/precision))
        return(c(r,g,b))
    }
    
    to_terrainrgb(ELEV)"
    
    out_file <- tempfile(fileext = ".tif")
    
    
    dtm_mapbox <- calc(expr = mapbox_encoded,
                       rasterfiles = datasourcename,
                       var.names = "ELEV",
                       dstfile = out_file,
                       dtName = "Byte",
                       out_band = 1:3,
                       nodata_value = 0)
    
    
    
    for (option in options) {
      
      output_file <- file_temp(ext = ".tif")
      
      write_ds <- function(){
        createCopy(
          src_filename = dtm_mapbox,
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
      )}
  }
  
  # Convert list to data frame
  results_df <- do.call(rbind, lapply(results, data.frame))
  return(results_df)
}

# apply function on list
benchmark_results_mapbox <- raster_compression(dsns, options)


benchmark_results_mapbox |> 
  mutate(relative_size = compressed_mb / uncompressed_mb) |> 
  group_by(settings) |> 
  summarise(across(c(compressed_mb, relative_size, writetime_sec, readtime_sec), 
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}")) |> 
  arrange(desc(relative_size_mean)) |>
  # Create the gt table
  gt() |> 
  fmt_number(columns = ends_with("_mean"), decimals = 2) |> 
  fmt_number(columns = ends_with("_sd"), decimals = 2) |>
  fmt_number(
    columns = contains("size"),
    scale_by = 100,
    decimals = 1
  ) |>
  cols_merge_uncert(
    col_val = compressed_mb_mean,
    col_uncert = compressed_mb_sd
  ) |>
  cols_merge_uncert(
    col_val = relative_size_mean,
    col_uncert = relative_size_sd
  )  |>
  cols_merge_uncert(
    col_val = readtime_sec_mean,
    col_uncert = readtime_sec_sd
  )  |>
  cols_merge_uncert(
    col_val = writetime_sec_mean,
    col_uncert = writetime_sec_sd
  )   |>
  cols_label(
    settings = "Settings",
    compressed_mb_mean = "Absolute Size (MB)",
    relative_size_mean = "Relative Size (%)",
    writetime_sec_mean = "Runtime Write (s)",
    readtime_sec_mean = "Runtime Read (s)"
  ) |>
  tab_header(
    title = "Comparison of Compression Performances",
    subtitle = "Size and processing time (mean ± standard deviation) depending on compression settings",
  ) |>
  tab_footnote(
    footnote = "All tested algorithms are lossless compression, except for DISCARD_LSB where the last 1/2/3 bits of the blue band are discarted.",
    locations = cells_column_labels(columns = settings)
  ) |> 
  tab_source_note(source_note = md(
    "Source: Original images were converted to RGB-encoded images first (Mapbox-encoding)"
  ))




#### benchmark terrarium encoding

# Create a function to measure performance for each datasource and compression
raster_compression <- function(dsns, options) {
  results <- list()
  
  for (datasourcename in dsns) {
    
    
    # convert DTM to RGB encoded (Terrarium)
    terrarium_encoded <- "to_terrainrgb <- function(dtm) {
        v <- dtm + 32768
        r <- floor(v/256)
        g <- floor(v %% 256)
        b <- floor((v - floor(v)) * 256)
        return(c(r,g,b))
        }
        to_terrainrgb(ELEV)"
    
    out_file <- tempfile(fileext = ".tif")
    
    dtm_terrarium <- calc(expr = terrarium_encoded,
                          rasterfiles = datasourcename,
                          var.names = "ELEV",
                          dstfile = out_file,
                          dtName = "Byte",
                          out_band = 1:3,
                          nodata_value = 0)
    
    
    
    for (option in options) {
      
      output_file <- file_temp(ext = ".tif")
      
      write_ds <- function(){
        createCopy(
          src_filename = dtm_terrarium,
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

# apply function on list
benchmark_results_terrarium <- raster_compression(dsns, options)



benchmark_results_terrarium |> 
  mutate(relative_size = compressed_mb / uncompressed_mb) |> 
  group_by(settings) |> 
  summarise(across(c(compressed_mb, relative_size, writetime_sec, readtime_sec), 
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}")) |> 
  arrange(desc(relative_size_mean)) |>
  # Create the gt table
  gt() |> 
  fmt_number(columns = ends_with("_mean"), decimals = 2) |> 
  fmt_number(columns = ends_with("_sd"), decimals = 2) |>
  fmt_number(
    columns = contains("size"),
    scale_by = 100,
    decimals = 1
  ) |>
  cols_merge_uncert(
    col_val = compressed_mb_mean,
    col_uncert = compressed_mb_sd
  ) |>
  cols_merge_uncert(
    col_val = relative_size_mean,
    col_uncert = relative_size_sd
  )  |>
  cols_merge_uncert(
    col_val = readtime_sec_mean,
    col_uncert = readtime_sec_sd
  )  |>
  cols_merge_uncert(
    col_val = writetime_sec_mean,
    col_uncert = writetime_sec_sd
  )   |>
  cols_label(
    settings = "Settings",
    compressed_mb_mean = "Absolute Size (MB)",
    relative_size_mean = "Relative Size (%)",
    writetime_sec_mean = "Runtime Write (s)",
    readtime_sec_mean = "Runtime Read (s)"
  ) |>
  tab_header(
    title = "Comparison of Compression Performances",
    subtitle = "Size and processing time (mean ± standard deviation) depending on compression settings",
  ) |>
  tab_footnote(
    footnote = "All tested algorithms are lossless compression, except for DISCARD_LSB where the last 1/2/3 bits of the blue band are discarted.",
    locations = cells_column_labels(columns = settings)
  ) |> 
  tab_source_note(source_note = md(
    "Source: Original images were converted to RGB-encoded images first (Terrarium-encoding)"
  ))

