# Tests for database.R

test_that("sensors-vec", {
  expect_vector(sensors, character(), size = 25)
})

test_that("create_db", {
  filename <- tempfile("create", fileext = ".db")

  db <- create_db(path = NULL, filename)
  DBI::dbDisconnect(db)
  expect_true(file.exists(filename))

  # Test overwrite argument
  expect_error({
    db <- create_db(path = NULL, filename, overwrite = TRUE)
    DBI::dbDisconnect(db)
  }, NA)
  expect_error({
    db <- create_db(path = NULL, filename, overwrite = FALSE)
    DBI::dbDisconnect(db)
  }, "Database .+?(?=\\.db)\\.db already exists\\. Use overwrite = TRUE to overwrite\\.",
  perl = TRUE)

  file.remove(filename)
})

test_that("open_db", {
  fake_db <- tempfile("foo", fileext = ".db")
  expect_error(open_db(NULL, fake_db), "There is no such file")

  # Create a new (non-mpathsenser db)
  db <- DBI::dbConnect(RSQLite::SQLite(), fake_db)
  DBI::dbExecute(db, "CREATE TABLE foo(bar INTEGER, PRIMARY KEY(bar));")
  DBI::dbDisconnect(db)
  expect_error(open_db(NULL, fake_db), "Sorry, this does not appear to be a mpathsenser database.")
  file.remove(fake_db)

  path <- system.file("testdata", package = "mpathsenser")
  db <- open_db(path, "test.db")
  expect_true(DBI::dbIsValid(db))
  DBI::dbDisconnect(db)
})

test_that("copy_db", {
  test_db_name <- tempfile("test", fileext = ".db")
  filename <- tempfile("copy", fileext = ".db")
  file.copy(from = system.file("testdata", "test.db", package = "mpathsenser"),
            to = test_db_name,
            overwrite = TRUE,
            copy.mode = FALSE)
  db <- open_db(NULL, test_db_name)

  new_db <- create_db(NULL, filename)
  copy_db(db, new_db, sensor = "All")
  expect_equal(get_nrows(db), get_nrows(new_db))
  close_db(new_db)
  file.remove(filename)

  # Create new db and copy to it
  copy_db(db, sensor = "Accelerometer", path = tempdir(), db_name = "copy.db")
  new_db <- open_db(tempdir(), "copy.db")
  true <- c(6L, rep(0L, 24))
  names(true) <- sensors
  expect_equal(get_nrows(new_db), true)

  expect_error(copy_db(db, sensor = "Accelerometer", path = tempdir(), db_name = "copy.db"),
               paste0("A file in .+ with the name copy\\.db already exists\\. Please choose ",
                      "a different name or path or remove the file\\."))
  close_db(new_db)
  file.remove(file.path(tempdir(), "copy.db"))

  DBI::dbDisconnect(db)
  expect_error(copy_db(db, sensor = "Accelerometer", path = tempdir(), db_name = "copy.db"),
               "Database connection is not valid")
  file.remove(test_db_name)
})

test_that("close_db", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  expect_error(close_db(db), NA)
  expect_false(DBI::dbIsValid(db))
  expect_error(close_db(db), NA) # Invalid db
  rm(db)
  expect_error(close_db(db), NA) # db does not exist
  db <- NULL
  expect_error(close_db(db), NA) # NULL db
})

test_that("index_db", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  expect_error(index_db(db), NA)
  expect_error(index_db(db), "index idx_accelerometer already exists")

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(filename)
})

test_that("add_study", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  data <- data.frame(study_id = "12345", data_format = "mpathsenser")
  expect_equal(add_study(db, data), 1)

  studies <- DBI::dbGetQuery(db, "SELECT * FROM Study")
  expect_equal(studies, data)
  expect_equal(add_study(db, data), 0)
  expect_equal(add_study(db, data.frame(foo = 1, bar = 2)), 0)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(filename)
})

test_that("add_participant", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  data <- data.frame(participant_id = "12345", study_id = "12345")
  DBI::dbExecute(db, "INSERT INTO Study VALUES('12345', 'mpathsenser')")
  expect_equal(add_participant(db, data), 1)
  participants <- DBI::dbGetQuery(db, "SELECT * FROM Participant")
  expect_equal(participants, data)
  expect_equal(add_participant(db, data), 0)
  expect_equal(add_participant(db, data.frame(foo = 1, bar = 2)), 0)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(filename)
})

test_that("add_processed_file", {
  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  data <- data.frame(file_name = "12345.json", participant_id = "12345", study_id = "12345")
  DBI::dbExecute(db, "INSERT INTO Study VALUES('12345', 'mpathsenser')")
  DBI::dbExecute(db, "INSERT INTO Participant VALUES('12345', '12345')")
  expect_equal(add_processed_files(db, data), 1)
  files <- DBI::dbGetQuery(db, "SELECT * FROM ProcessedFiles")
  expect_equal(files, data)
  expect_equal(add_processed_files(db, data), 0)
  expect_equal(add_processed_files(db, data.frame(foo = 1, bar = 2)), 0)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(filename)
})

test_that("clear_sensors_db", {
  path <- system.file("testdata", package = "mpathsenser")

  # Create db
  filename <- tempfile("foo", fileext = ".db")
  db <- create_db(NULL, filename)

  db <- import(path, db = db, recursive = FALSE)
  db <- open_db(NULL, filename)
  original <- get_nrows(db)
  res <- clear_sensors_db(db)
  expect_type(res, "list")
  expect_length(res, length(sensors))
  expect_equal(Reduce(`+`, res), sum(original))
  expect_equal(sum(get_nrows(db)), 0L)

  # Cleanup
  DBI::dbDisconnect(db)
  file.remove(filename)
})

test_that("get_processed_files", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  res <- get_processed_files(db)
  true <- data.frame(
    file_name = c("empty.json", "test.json"),
    participant_id = c("empty.json", "12345"),
    study_id = c("-1", "test-study")
  )
  expect_equal(res, true)
  DBI::dbDisconnect(db)
})

test_that("get_participants", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  res <- get_participants(db)
  res_lazy <- get_participants(db, lazy = TRUE)
  true <- data.frame(
    participant_id = c("empty.json", "12345"),
    study_id = c("-1", "test-study")
  )
  expect_equal(res, true)
  expect_s3_class(res_lazy, "tbl_SQLiteConnection")
  DBI::dbDisconnect(db)
})

test_that("get_study", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  res <- get_studies(db)
  res_lazy <- get_studies(db, lazy = TRUE)
  true <- data.frame(
    study_id = c("-1", "test-study"),
    data_format = c(NA, "carp")
  )
  expect_equal(res, true)
  expect_s3_class(res_lazy, "tbl_SQLiteConnection")
  DBI::dbDisconnect(db)
})

test_that("get_nrows", {
  db <- open_db(system.file("testdata", package = "mpathsenser"), "test.db")
  expect_vector(get_nrows(db), integer(), length(sensors))
  DBI::dbDisconnect(db)
})
