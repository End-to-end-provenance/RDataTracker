# Assignment Operator Test
# Tests the '=', '<-' and '<<-' operators
#
# @author Elizabeth Fong
# @version September 2016


# --- THE '=' OPERATOR ------------------------------------------------- #

# new variable in global environment
a = 1
stopifnot( a == 1 )


# reassignment to existing variable in global environment
a = 2
stopifnot( a == 2 )


# in a function environment
fn1 <- function()
{
  a = 3
  stopifnot( a == 3 )
}

fn1()
stopifnot( a == 2 )


# nested function environment
fn2 <- function()
{
  a = 4
  
  fn <- function()
  {
    a = 5
  }
  
  fn()
  stopifnot( a == 4 )
}

fn2()
stopifnot( a == 2 )


# in a new environment
env1 <- new.env()
env1$a = 6

stopifnot( env1$a == 6 )
stopifnot( a == 2 )


# in a nested environment
env1$env <- new.env( parent = env1 )
env1$env$a = 7

stopifnot( env1$env$a == 7 )
stopifnot( env1$a == 6 )
stopifnot( a == 2 )


# --- THE '<-' OPERATOR ------------------------------------------------ #

# new variable in global environment
b <- 1
stopifnot( b == 1 )

# reassignment to existing variable in global environment
b <- 2
stopifnot( b == 2 )


# in a function environment
fn3 <- function()
{
  b <- 3
  stopifnot( b == 3 )
}

fn3()
stopifnot( b == 2 )


# nested function environment
fn4 <- function()
{
  b <- 4
  
  fn <- function()
  {
    b <- 5
  }
  
  fn()
  stopifnot( b == 4 )
}

fn4()
stopifnot( b == 2 )


# in a new environment
env2 <- new.env()
env2$b <- 6

stopifnot( env2$b == 6 )
stopifnot( b == 2 )


# in a nested environment
env2$env <- new.env( parent = env2 )
env2$env$b <- 7

stopifnot( env2$env$b == 7 )
stopifnot( env2$b == 6 )
stopifnot( b == 2 )


# --- THE '<<-' OPERATOR ----------------------------------------------- #s

# assignment in the global environment WILL FAIL!

# in a function environment

fn5 <- function()
{
  d <<- 1
}

fn5()
stopifnot( d == 1 )


# nested function environment - overriding exising variable
fn6 <- function()
{
  e <- 0
  
  fn <- function()
  {
    e <<- 1
  }
  
  fn()
  stopifnot( e == 1 )
}

fn6()
stopifnot( ! exists("e") )


# nested function environment - no existing variable
fn7 <- function()
{
  fn <- function()
  {
    f <<- 3
  }
  
  fn()
}

fn7()
stopifnot( f == 3 )


# in a new environment - DOES NOT WORK!
#env3 <- new.env()
#env3$f <<- 4    # Error: object 'env3' not found
#
#stopifnot( f == 4 )
#stopifnot( ! exists("env3$f") )
#
#
# in a nested environment - DOES NOT WORK!
#env3$env <- new.env( parent = env3 )
#env3$env$f <<- 5    # Error: object 'env3' not found
#
#stopifnot( ! exists("env2$env$f") )
#stopifnot( ! exists("env$f") )
#stopifnot( f == 5 )
