# It assumes the caller has already found the function text.  When integrated into RDataTracker, it is much more likely that we would have the parsed function.
# It does not add the ddg.return.value calls yet.


# Function with no parameters and no return value
# f1 <- function () { 
#  a <-1
#  b <- 2 
# }
# 
# # Single parameter and return value.
# # Function has a single statement.
# f2 <- function (x) {
# #  print(paste("In f2: x =", x))
#   return (x + 1)
# }

f2a <- function(x) {
  if (x > 0) {
    dev.off()
  }
}

f2a(5)
 
# Two return calls in different branches of an if
#f3 <- function (x) {
# if (x > 0) return (x + 1)
# else return (x - 1)
#}
# 
# # Function appears on a single line
# f4 <- function () return (1)
# 
# # Return value combines arguments
# f5 <- function (x, y) {
#   return (x + y)
# }
# 
# # Use of "return" as part of a variable name
# f6 <- function () {
#   returned <- TRUE
# }
# 
# # Use of "return" to not mean the return function
# f7 <- function () {
#   x <- "return"
# }
# 
# # More playing with "return"
# f8 <- function () {
#   returned <- TRUE
#   return (returned)
# }
# 
# # Nested return call.  Kind of silly...  Less important if this works
# # since it is bad R!
# f9 <- function(x) {  
#   return(2*return(x^2)) 
# }
# 
# # Returns null if the condition is false
# f10 <- function(x) {
#   if (x > 0) return (x + 1)
# }
# 
# # A function that is not assigned to a variable
# x <- 2:6
# lapply (x, function(num) {return(num+1)})

# f1_val <- f1(); stopifnot(f1_val == 2)
# f2_val <- f2(3); stopifnot(f2_val == 4)
#f3_val_pos <- f3(3); print(paste("f3_val_pos =", f3_val_pos)); stopifnot (f3_val_pos == 4)
#f3_val_neg <- f3(-3); stopifnot (f3_val_neg == -4)
# f4_val <- f4(); stopifnot(f4_val == 1)
# f5_val <- f5(1, 2); stopifnot(f5_val == 3)
# f6_val <- f6(); stopifnot(f6_val == TRUE)
# f7_val <- f7(); stopifnot(f7_val == "return")
# f8_val <- f8(); stopifnot(f8_val == TRUE)
# # print ("f9 produces an error of mismatched start-finish nodes.")
# # print ("This is due to a bug in the function annotation code.")
# # print ("The inner return call should have a call to ddg.return.value.")
# f9_val <- f9(3); stopifnot(f9_val == 9)
# f10_val_pos <- f10(1); stopifnot(f10_val_pos == 2)
# f10_val_neg <- f10(-1); stopifnot(is.null(f10_val_neg))
# 
# # Nested function
# outer <- function() {
#   inner <- function() {
#     a <- 1
#   }
#   
#   b <- 2
#   inner()
# }
# 
# outer()
# 
# # Defining a special operator
# `%test%` = function (a, b) a * b
# 2 %test% 4

