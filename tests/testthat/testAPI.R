context("API")

test_that("console prov dir", {
      console_dir <- tempdir()
      actual.path <- .ddg.set.path (console_dir, NULL, FALSE)
      expect_true(startsWith (actual.path, 
          paste (normalizePath(console_dir, winslash = "/"), "prov_console", sep="/")))
      unlink (actual.path, recursive=TRUE)
    })

