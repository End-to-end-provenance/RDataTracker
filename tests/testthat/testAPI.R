context("API")

test_that("console prov dir", {
      actual.path <- .ddg.set.path (".", NULL, FALSE)
      expect_true(startsWith (actual.path, 
          paste (getwd(), "prov_console", sep="/")))
      unlink (actual.path, recursive=TRUE)
    })

test_that("specified prov dir", {
	  actual.path <- .ddg.set.path ("/x/y/z", NULL, FALSE)
      expect_true(startsWith (actual.path, "/x/y/z/prov_console"))
    })
