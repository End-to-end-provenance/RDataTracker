# Assign Function Test
# Tests the 'assign()' function
#
# @author Elizabeth Fong
# @version September 2016


# --- The assign() function -------------------------------------------- #

# new variable in global environment
assign( "a" , 0 , inherits = FALSE )
stopifnot( a == 0 )

assign( "b" , 0 , inherits = TRUE )
stopifnot( b == 0 )


# reassignment to existing variable in global environment
assign( "a" , 1 , inherits = FALSE )
stopifnot( a == 1 )

assign( "b" , 1 , inherits = TRUE )
stopifnot( b == 1 )


# in a function environment: inherits = FALSE 
fn1 <- function()
{
  assign( "d" , 0 , inherits = FALSE )
  stopifnot( d == 0 )
}

fn1()
stopifnot( ! exists("d") )


# in a function environment: inherits = TRUE
fn2 <- function()
{
  assign( "e" , 1 , inherits = TRUE )
  stopifnot( e == 1 )
}

fn2()
stopifnot( e == 1 )


# nested function environment: inherits = FALSE
fn3 <- function()
{
  f <- 0
  
  fn <- function()
  {
    assign( "f" , 1 , inherits = FALSE )
    stopifnot( f == 1 )
  }
  
  fn()
  stopifnot( f == 0 )
}

fn3()
stopifnot( ! exists("f") )


# nested function environment: inherits = TRUE, existing variable
fn4 <- function()
{
  g <- 0
  
  fn <- function()
  {
    assign( "g" , 1 , inherits = TRUE )
  }
  
  fn()
  stopifnot( g == 1 )
}

fn4()
stopifnot( ! exists("g") )


# nested function environment: inherits = TRUE, no existing variable
fn5 <- function()
{
  fn <- function()
  {
    assign( "h" , 0 , inherits = TRUE )
  }
  
  fn()
}

fn5()
stopifnot( h == 0 )


# in a new environment: inherits = FALSE
env1 <- new.env()
assign( "i" , 1 , envir = env1 , inherits = FALSE )

stopifnot( env1$i == 1 )
stopifnot( ! exists("i") )


# in an environment: inherits = TRUE
assign( "j" , 2 , envir = env1 , inherits = TRUE )

stopifnot( ! exists("env1$j") )
stopifnot( j == 2 )


# in a nested environment: inherits = FALSE
env1$env <- new.env( parent = env1 )
assign( "k" , 3 , envir = env1$env , inherits = FALSE )

stopifnot( env1$env$k == 3 )
stopifnot( ! exists("env1$k") )
stopifnot( ! exists("k") )


# in a nested environment: inherits = TRUE, existing variable
assign( "i" , 4 , envir = env1$env , inherits = TRUE )

stopifnot( ! exists("env1$env$i") )
stopifnot( env1$i == 4 )
stopifnot( ! exists("i") )


# in a nested environment: inherits = TRUE, no existing variable
assign( "l" , 5 , envir = env1$env , inherits = TRUE )

stopifnot( ! exists("env1$env$l") )
stopifnot( ! exists("env1$l") )
stopifnot( l == 5 )
