## ---- echo = FALSE, include = FALSE, message = FALSE--------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(mpathsenser)

## ----copy data, results="hide"------------------------------------------------
# Get the temp folder
tempdir <- tempdir()

# Get a handle to the data files
path <- system.file("extdata", "example", package = "mpathsenser")

# Get a list of all the files that are to be copied
copy_list <- list.files(path, "carp-data", full.names = TRUE)

# Copy all data
file.copy(
  from = copy_list,
  to = tempdir,
  overwrite = TRUE,
  copy.mode = FALSE
)

## ----unzip--------------------------------------------------------------------
unzip_data(path = tempdir)

## ----fix and test JSONS-------------------------------------------------------
# Note that test_jsons returns the full path names
to_fix <- test_jsons(tempdir)
print(to_fix)

fix_jsons(path = NULL, to_fix)

## ----import data--------------------------------------------------------------
# Create a new database
db <- create_db(tempdir, "getstarted.db")

# Import the data
import(
  path = tempdir,
  db = db,
  batch_size = 12
)

## ---- fig.width=13, fig.height=8, fig.align='center', dpi=55------------------
sensors <- c(
  "Accelerometer", "Activity", "AppUsage", "Bluetooth", "Calendar",
  "Connectivity", "Device", "Gyroscope", "InstalledApps", "Light",
  "Location", "Memory", "Pedometer", "Screen", "Weather", "Wifi"
)
coverage(
  db = db,
  participant_id = "2784",
  sensor = sensors,
  relative = FALSE
)

## -----------------------------------------------------------------------------
close_db(db)

