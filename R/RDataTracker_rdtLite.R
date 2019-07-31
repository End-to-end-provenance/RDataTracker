# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017, 2018.

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

######################### RDataTracker.R #########################


#' .ddg.link.function.returns.
#' @param command input command.
#' @return nothing
#' @noRd

.ddg.link.function.returns <- function (command) {}

#' .ddg.markdown takes a Rmd file and extracts the R code and text through
#' the purl function in the knitr library and executes the R code to 
#' collect provenance.
#' @param r.script.path the path of the original Rmd file
#' @param output.path the path of the generated R script
#' @return the path to the original Rmd file
#' @noRd

.ddg.markdown <- function(r.script.path, output.path){
  
  #generates R script file from markdown file.
  r.file <- knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)
  # print(paste("purl output in ", r.file))
  
  # Generate the formatted output
  # print(paste("Rendering ", r.script.path))
  tryCatch (
    rmarkdown::render(r.script.path, quiet=TRUE),
    error = function (e) {}
  )
  
  #moves file to ddg directory
  file.rename(from = r.file, to = output.path)
  return (r.script.path)
}
