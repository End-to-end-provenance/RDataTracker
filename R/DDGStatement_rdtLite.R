# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

########################## DDGStatement.R ############################


#' .ddg.construct.DDGStatement creates a DDGStatement.
#' @param expr - the parsed expression
#' @param pos - the DDGStatementPos object for this statement
#' @param script.name - the name of the script the statement is from
#' @param script.num - the script number used to find the script in the sourced script table
#' @param parseData - the object created by the parser that gives us source position information
#' @return a DDG statement
#' @noRd

.ddg.construct.DDGStatement <- function (expr, pos, script.name, script.num, parseData) {
  #print(paste(".ddg.construct.DDGStatement: expr =", expr))
  # Surprisingly, if a statement is just a number, like 1 (which could be the last 
  # statement in a function, for example), the parser returns a number, rather 
  # than a parse tree!
  if (is.numeric(expr)) expr <- parse(text=expr)
  
  return (methods::new (Class = "DDGStatement", parsed = expr, pos, script.num))
}
