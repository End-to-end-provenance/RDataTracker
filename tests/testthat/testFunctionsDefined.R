test_that ("function globals set",
    {
      expect_equal (.ddg.find.globals.set (parse (text = "a <- b")[[1]]), character())
      expect_equal (.ddg.find.globals.set (parse (text = "function () { return (1)}")[[1]]), character())
      expect_equal (.ddg.find.globals.set (parse (text = "function () { a <- 1}")[[1]]), character())
      expect_equal (.ddg.find.globals.set (parse (text = "function () { a <<- 1}")[[1]]), "a")
      expect_equal (.ddg.find.globals.set (parse (text = "function () { a <<- 1; b <<- 2}")[[1]]), c("a", "b"))
    }
)