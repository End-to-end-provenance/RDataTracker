test_that ("function nonlocals set",
    {
      expect_equal (.ddg.find.nonlocals.set (parse (text = "a <- b")[[1]]), character())
      expect_equal (.ddg.find.nonlocals.set (parse (text = "function () { return (1)}")[[1]]), character())
      expect_equal (.ddg.find.nonlocals.set (parse (text = "function () { a <- 1}")[[1]]), character())
      expect_equal (.ddg.find.nonlocals.set (parse (text = "function () { a <<- 1}")[[1]]), "a")
      expect_equal (.ddg.find.nonlocals.set (parse (text = "function () { a <<- 1; b <<- 2}")[[1]]), c("a", "b"))
    }
)

test_that ("getting nonlocals",
    {
      .ddg.save.func.decl.info ("f", parse (text = "function () { a <<- 1; b <<- 2}")[[1]])
      expect_equal (.ddg.lookup.nonlocals.set ("f"), c ("a", "b"))
    })

test_that ("function nonlocals used",
    {
      expect_equal (.ddg.find.nonlocals.used (parse (text = "function () return (a)")[[1]]), "a")
      expect_equal (.ddg.find.nonlocals.used (parse (text = "function (a) return (a)")[[1]]), character())
      expect_equal (.ddg.find.nonlocals.used (parse (text = "function (a) { b <- a + 1; return (b) }")[[1]]), character())
      expect_equal (.ddg.find.nonlocals.used (parse (text = "function (a) { b <- c + 1; return (c) }")[[1]]), "c")
      expect_equal (.ddg.find.nonlocals.used (parse (text = "function (a) { b <- c + 1; return (d) }")[[1]]), c ("c", "d"))
      expect_equal (.ddg.find.nonlocals.used (parse (text = "function (a) { if (TRUE) return (c) else return(d) }")[[1]]), c ("c", "d"))
    })