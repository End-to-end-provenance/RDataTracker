context ("DDGStatement construction")

test_that("var uses", 
    {
      expect_equal(.ddg.find.var.uses (parse (text = "a <- b")),
          "b")
      expect_equal(.ddg.find.var.uses (parse (text = "a <- b + c")),
          c("b", "c"))
      expect_equal(.ddg.find.var.uses (parse (text = "if (x > 1 ) a <- b + c else d <- e + f")),
          c("x", "b", "c", "e", "f"))
      expect_equal(.ddg.find.var.uses (parse (text = "f <- function (a) return (b)")), character())
      expect_equal(.ddg.find.var.uses (parse (text = "a[[b]] <- 3")), c("a","b"))
      expect_equal(.ddg.find.var.uses (parse (text = "a[b] <- 'abc'")), c("a","b"))
      expect_equal(.ddg.find.var.uses (parse (text = "a <- c[d]")), c("c","d"))
    })

test_that ("vars set",
    {
      expect_equal(.ddg.find.simple.assign (parse (text = "a <- b")[[1]]), "a")
      expect_equal(.ddg.find.simple.assign (parse (text = "a -> b")[[1]]), "b")
      expect_equal(.ddg.find.simple.assign (parse (text = "a <<- b")[[1]]), "a")
      expect_equal(.ddg.find.simple.assign (parse (text = "a ->> b")[[1]]), "b")
      expect_equal(.ddg.find.simple.assign (parse (text = "a = b")[[1]]), "a")
      expect_equal(.ddg.find.simple.assign (parse (text = "a (b)")[[1]]), "")
      expect_equal(.ddg.find.simple.assign (parse (text = "a <<- b")[[1]], locals.only = TRUE), "")
    })

test_that ("vars possibly set",
    {
      expect_equal(.ddg.find.assign (parse (text = "a <- b")[[1]]), "a")
      expect_equal(.ddg.find.assign (parse (text = "a <- (b <- 2) * 3")[[1]]), c("a", "b"))
      expect_equal(.ddg.find.assign (parse (text = "f <- function () a <- 1")), "f")
      expect_equal(.ddg.find.assign (parse (text = "if (x > 1 ) a <- b + c else d <- e + f")),
          c("a", "d"))
    })

test_that ("functions called",
    {
      expect_equal(.ddg.find.calls (parse (text = "a <- b")[[1]]), list(c("<-"), c("a", "b"), NULL))
      expect_equal(.ddg.find.calls (parse (text = "a <- b()")[[1]]), list(c("<-", "b"), "a", NULL))
      expect_equal(.ddg.find.calls (parse (text = "a <- b() + c()")[[1]]), list(c("<-", "+", "b", "c"), "a", NULL))
      calls <- .ddg.find.calls (parse (text = "a <- utils::alarm()")[[1]])
      expect_equal(calls[[1]], c("<-","::"))
      expect_equal(calls[[2]], "a")
      ddg.fun <- c("alarm")
      ddg.lib <- c("utils")
      expected.lib.calls <- data.frame(ddg.fun, ddg.lib, stringsAsFactors=FALSE)
      expect_equal(calls[[3]], expected.lib.calls )
    })

