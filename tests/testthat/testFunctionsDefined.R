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
      expect_equal (.ddg.get.nonlocals.set ("f"), c ("a", "b"))
    })