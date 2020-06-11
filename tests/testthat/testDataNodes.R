test_that ("simple node values",
    {
      .ddg.set("ddg.snapshot.size", 0)
      expect_equal (.ddg.get.node.val (1), "1")
      expect_equal (.ddg.get.node.val ("a"), "\"a\"")
      expect_equal (.ddg.get.node.val (NULL), "NULL")
      expect_equal (.ddg.get.node.val (NA), "NA")
      expect_equal (.ddg.get.node.val (TRUE), "TRUE")

      .ddg.set("ddg.snapshot.size", 100)
      expect_equal (.ddg.get.node.val (1), "1")
      expect_equal (.ddg.get.node.val ("a"), "\"a\"")
      expect_equal (.ddg.get.node.val (NULL), "NULL")
      expect_equal (.ddg.get.node.val (NA), "NA")
      expect_equal (.ddg.get.node.val (TRUE), "TRUE")
    }
)

test_that ("small data structures - no snapshots",
    {
      .ddg.set("ddg.snapshot.size", 0)
      expect_equal (.ddg.get.node.val (c(1,2,3)), "1 2 3")
      expect_equal (.ddg.get.node.val (c("a", "b", "c")), "\"a\" \"b\" \"c\"")
      expect_equal (.ddg.get.node.val (c(TRUE, FALSE, TRUE)), " TRUE FALSE  TRUE")
      expect_equal (.ddg.get.node.val (list (1, "a", TRUE)), "\"1\"    \"a\"    \"TRUE\"")
      expect_equal (.ddg.get.node.val (array (c (1,2,3))), "[1] 1 2 3")
    }
)

test_that ("small data structures - with snapshots",
    {
      .ddg.set("ddg.snapshot.size", 10)
      expect_equal (.ddg.get.node.val (c(1,2,3)), "1 2 3")
      expect_equal (.ddg.get.node.val (c("a", "b", "c")), "\"a\" \"b\" \"c\"")
      expect_equal (.ddg.get.node.val (c(TRUE, FALSE, TRUE)), " TRUE FALSE  TRUE")
      expect_equal (.ddg.get.node.val (list (1, "a", TRUE)), "\"1\"    \"a\"    \"TRUE\"")
      expect_equal (.ddg.get.node.val (array (c (1,2,3))), "[1] 1 2 3")
    }
)

test_that ("large data structures - no snapshots",
    {
      .ddg.set("ddg.snapshot.size", 0)
      expect_equal (.ddg.get.node.val ("Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this. But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth."), 
          "\"Four score and seven years ago our fathers brought forth on this continent, a n...")

      char.vector <- c ("a", "b", "c")
      int.vector <- c (1, 2, 3)
      bool.vector <- c (TRUE, FALSE, TRUE)
      df <- data.frame (char.vector, int.vector, bool.vector)
      expect_equal (.ddg.get.node.val (df), "Row 1           a          1        TRUE")
      expect_equal (.ddg.get.node.val (matrix(data=c(1,2,3,4,5,6), nrow=3, ncol=2)), "[1,]    1    4")
      expect_equal (.ddg.get.node.val (x.array <- array(data=c(1,2,3,4,5,6,7,8), dim=c(2,2,2))), "[1,]    1    3")
      list2 <- list(int.vector, char.vector, bool.vector, df)
      expect_equal (.ddg.get.node.val (list2), "\"1\"          \"2\"          \"3\"          \"a\"          \"b\"          \"c\"            ...")

      n <- c("foo", "bar", "bat")
      z <- c("hello", "world,","R","is","fun",".", "Watch out", "for","the R", "Inferno")
      complex.array <- array(NA,c(2,length(n),length(z)), dimnames=list(c("1998","1999"),n,z))
      expect_equal (.ddg.get.node.val (complex.array), "[1,]   NA   NA   NA")
   }
)

test_that ("large data structures - with snapshots",
    {
      .ddg.set("ddg.snapshot.size",  10)
      expect_equal (.ddg.get.node.val ("Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this. But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth."), 
          NULL)
      
      char.vector <- c ("a", "b", "c")
      int.vector <- c (1, 2, 3)
      bool.vector <- c (TRUE, FALSE, TRUE)
      df <- data.frame (char.vector, int.vector, bool.vector)
      expect_equal (.ddg.get.node.val (df), NULL)
      m <- matrix(data=c(1,2,3,4,5,6), nrow=3, ncol=2)
      expect_equal (.ddg.get.node.val (m), NULL)
      expect_equal (.ddg.get.node.val (x.array <- array(data=c(1,2,3,4,5,6,7,8), dim=c(2,2,2))), NULL)
      list1 <- list(10, "abc", TRUE, NA, NULL)
      list2 <- list(int.vector, char.vector, bool.vector, m, df)
      expect_equal (.ddg.get.node.val (list2), NULL)
      
      list3 <- list(list1, list2)
      expect_equal (.ddg.get.node.val (list3), NULL)
    }
)

