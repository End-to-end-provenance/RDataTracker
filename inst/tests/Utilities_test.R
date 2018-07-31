context ("Utilities")

test_that ("format time",
    {
      expect_true(startsWith (RDataTracker:::.ddg.format.time ("2018-07-31 16:46:30"), 
          "2018-07-31T16.46.30"))
    })

test_that ("remove tabs and eols",
    {
      expect_equal (RDataTracker:::.ddg.remove.tab.and.eol.chars ("abc"), "abc")
      expect_equal (RDataTracker:::.ddg.remove.tab.and.eol.chars ("	abc"), " abc")
      expect_equal (RDataTracker:::.ddg.remove.tab.and.eol.chars ("a	bc"), "a bc")
      expect_equal (RDataTracker:::.ddg.remove.tab.and.eol.chars ("abc	"), "abc ")
      expect_equal (RDataTracker:::.ddg.remove.tab.and.eol.chars ("	a	bc	"), " a bc ")
    })
        