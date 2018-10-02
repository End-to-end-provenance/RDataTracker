# Assignment Operators Test
# Tests the '=', '<-' and '<<-' operators
#
# @author Elizabeth Fong
# @version December 2016
#
# edited December 11 2017: commented out nested function tests


# --- THE '=' OPERATOR ------------------------------------------------- #

# new variable in global environment
a = 1
stopifnot( a == 1 )


# in a function environment
fn1 <- function()
{
  b = 2
  stopifnot( b == 2 )
}

fn1()
stopifnot( ! exists("b") )


# nested function environment
#fn2 <- function()
#{
#  d = 3
#  
#  fn <- function()
#  {
#    d = 4
#  }
#  
#  fn()
#  stopifnot( d == 3 )
#}
#
#fn2()
#stopifnot( ! exists("d") )


# in a new environment
env1 <- new.env()
env1$e = 5

stopifnot( env1$e == 5 )
stopifnot( ! exists("e") )


# in a nested environment
env2 <- new.env()
env2$env <- new.env( parent = env2 )
env2$env$f = 6

stopifnot( env2$env$f == 6 )
stopifnot( ! exists("env2$f") )
stopifnot( ! exists("f") )


# --- THE '<-' OPERATOR ------------------------------------------------ #

# new variable in global environment
g <- 7
stopifnot( g == 7 )


# in a function environment
fn3 <- function()
{
  h <- 8
  stopifnot( h == 8 )
}

fn3()
stopifnot( ! exists("h") )


# nested function environment
#fn4 <- function()
#{
#  i <- 9
#  
#  fn <- function()
#  {
#    i <- 10
#  }
#  
#  fn()
#  stopifnot( i == 9 )
#}

#fn4()
#stopifnot( ! exists("i") )


# in a new environment
env3 <- new.env()
env3$j <- 11

stopifnot( env3$j == 11 )
stopifnot( ! exists("j") )


# in a nested environment
env4 <- new.env()
env4$env <- new.env( parent = env4 )
env4$env$k <- 12

stopifnot( env4$env$k == 12 )
stopifnot( ! exists("env4$k") )
stopifnot( ! exists("k") )


# --- THE '<<-' OPERATOR ----------------------------------------------- #s

# assignment in the global environment WILL FAIL!

# in a function
# assignment statement is not the last statement (tests .ddg.parse.commands)
fn5 <- function()
{
  l <<- 13
  stopifnot( l == 13 )
}

fn5()
stopifnot( l == 13 )


# in a function
# assignment statement is the last statement (tests ddg.return.value)
fn6 <- function()
{
  m <<- 14
}

fn6()
stopifnot( m == 14 )


# nested function environment - overriding exising variable
# assignment statement in nested function is not the last statement 
# tests .ddg.parse.commands
# DOES NOT WORK!!!
#fn7 <- function()
#{
#  n <- 15
#  
#  fn <- function()
#  {
#    n <<- 16
#    stopifnot( n == 16 )
#  }
#  
#  fn()
#  stopifnot( n == 16 )
#}

#fn7()
#stopifnot( ! exists("n") )


# nested function environment - overriding existing variable
# assignment statement in nested function is the last statement
# tests ddg.return.value
#fn8 <- function()
#{
#  o <- 17
#  
#  fn <- function()
#  {
#    o <<- 18
#  }
#  
#  fn()
#  stopifnot( o == 18 )
#}
#
#fn8()
#stopifnot( ! exists("o") )


# nested function environment - no existing variable
# assignment statement in nested function is not the last statement
# tests .ddg.parse.commands
# DOES NOT WORK!!!
#fn9 <- function()
#{
#  fn <- function()
#  {
#    p <<- 19
#    stopifnot( p == 19 )
#  }
#
#  fn()
#  stopifnot( p == 19 )
#}
#
#fn9()
#stopifnot( p == 19 )


# nested function environment - no existing variable
# assignment statement in nested function is the last statement
# tests ddg.return.value
#fn10 <- function()
#{
#  fn <- function()
#  {
#    r <<- 20
#  }
#  
#  fn()
#  stopifnot( r == 20 )
#}
#
#fn10()
#stopifnot( r == 20 )
