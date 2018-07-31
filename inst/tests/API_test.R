context("API")

test_that("console prov dir", {
      actual.path <- RDataTracker:::.ddg.set.path (".", NULL, FALSE)
      expect_true(startsWith (actual.path, 
          paste (getwd(), "prov_console", sep="/")))
      unlink (actual.path, recursive=TRUE)
    })

test_that("specified prov dir", {
      expect_warning (actual.path <- RDataTracker:::.ddg.set.path ("/x/y/z", NULL, FALSE))
      expect_true(startsWith (actual.path, "/x/y/z/prov_console"))
    })
