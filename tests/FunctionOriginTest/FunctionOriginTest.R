# FunctionOriginTest
#
# @author: Elizabeth Fong
# @version: January 2018


# calling functions from base
stopifnot( is.double(10) )


# explicit direct function calling from unloaded library
stopifnot( identical(stringr::str_to_upper("apple"), "APPLE") )
stopifnot( identical(stringr:::str_to_lower("BAGEL"), "bagel") )

stopifnot( as.numeric(utils::as.roman(stringi::stri_reverse(stringi::stri_join("1","2")))) == 21 )


# higher order function with variable names
a <- c(1:10)
stopifnot( identical(mapply(`+`, a, 1), as.double(c(2:11))) )

a <- c("a","b")
identical(stringi::stri_flatten(sapply(a,stringr::str_to_upper)),"AB")


# function calls without parameters
vector()


# parameter name is ""
a <- data.frame( c(1:3) , c(11:13) )
a[2,]


# not a call
1
a


# loading a library before using a function
library(stringi)
stopifnot( identical(stri_join("ab","cde"), "abcde") )


# multiple function calls from a single library
stopifnot( as.numeric(
  stri_flatten(stri_sort(stri_reverse(stri_trim(
    c(" 12 "," 1 "," 145 ")
  ))))
) == 121541)


# repeated function calls
stopifnot( identical(stri_reverse(stri_reverse(("cat"))), "cat") )


# multiple function calls from multiple libraries
library(stringr)
stopifnot( as.numeric(
  stri_flatten( stri_sort( stri_trim( stri_reverse( 
    str_to_lower( str_to_upper(
      c(" 12 "," 1 "," 145 ")
    ))
  ))))
) == 121541 )


# redefining an existing function
stopifnot( identical(format(as.person("a")), "a") )

as.person <- function(obj)
{
  return(5)
}

stopifnot( identical(as.person("a"), 5) )


# explicit return in a control block as the last statement of a function
fn <- function( a )
{
  stopifnot( is.double(a) )
  
  if( a < 10 )
    return( as.roman(a) )
  else
    return( is.leaf(a) )
}

stopifnot( identical(as.double(fn(1)),1) )
stopifnot( ! fn(11) )


# implicit return in a control block as the last statement of a function
fn1 <- function( b )
{
  stopifnot( is.double(b) )
  
  if( b < 10 )
    as.roman(b)
  else
    is.leaf(b)
}

stopifnot( identical(as.double(fn1(1)),1) )
stopifnot( ! fn1(11) )


# implicit return as the last statement of a function
fn2 <- function( d )
{
  stopifnot( is.double(d) )
  as.roman(d)
}

stopifnot( identical(as.double(fn2(15)),15) )


# explicit return as the last statement of a function
fn3 <- function( e )
{
  stopifnot( is.double(e) )
  return( as.roman(e) )
}

stopifnot( identical(as.double(fn3(15)),15) )


# statement in a loop block of a function
fn4 <- function( f )
{
  stopifnot( is.double(f) )
  
  for( i in 1:10 )
  {
    as.roman(f+i)
  }
}

stopifnot( is.null(fn4(20)) )


# ERROR
as.roman(z)