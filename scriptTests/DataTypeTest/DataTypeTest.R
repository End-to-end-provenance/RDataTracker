# Tests for various R data types.

# Modified by Emery Boose 29-May-2016, Elizabeth Fong 19-Jul-2018

x.number <- 10

x.string <- "one two three"

x.long.string <- " Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this. But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth."

x.logical <- TRUE

x.na <- NA

x.null <- NULL

x.int0 <- integer(0)

x.chr0 <- character(0)

x.log0 <- logical(0)

x.posixct <- as.POSIXct("080406 10:11", format = "%y%m%d %H:%M")

x.factor <- factor(c("red","green","blue","red","green","red"))

# vectors
x.vector.number <- c(1,2,3)
x.vector.string <- c("one", "two", "three")
x.vector.logical <- c(TRUE, FALSE, TRUE)

# Surprisingly, is.vector returns false for the following (as do all other is. 
# functions except for is.object).  Length returns 3.
# What should we record for valtype?
x.vector.posixct <- rep(x.posixct, 3)

# matrix, array
x.matrix <- matrix(data=c(1,2,3,4,5,6), nrow=3, ncol=2)
x.array <- array(data=c(1,2,3,4,5,6,7,8), dim=c(2,2,2))

# data frame
x.data.frame1 <- data.frame(x.vector.number, x.vector.string, x.vector.logical)
x.data.frame2 <- data.frame(x.vector.logical, x.vector.posixct, x.vector.number, x.vector.posixct)

# list
x.list1 <- list(x.number, x.string, x.logical, x.na, x.null)
x.list2 <- list(x.vector.number, x.vector.string, x.vector.logical, x.matrix, x.data.frame1)
x.list3 <- list(x.list1, x.list2)

