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
#' the purl function in the knitr library. It then annotates the R script
#' to insert start and finish nodes based on the chunks the user already
#' created. If eval = false, then the chunk will not be added to the DDG. If
#' the user has a name for the chunk, then that name will be used, else a chunk
#' name "ddg.chunk_1" and higher numbers will be generated.
#' Important: If in a code chunk, there is an empty line followed by "#' ----"
#' or "#''", then an extra finish node will be inserted, causing an error.
#' @param r.script.path the path of the original Rmd file
#' @param output.path the path of the generated R script
#' @return the path to the original Rmd file
#' @noRd

.ddg.markdown <- function(r.script.path, output.path){
  
  #generates R script file from markdown file
  knitr::purl(r.script.path, documentation = 2L, quiet = TRUE)
  
  #moves file to ddg directory
  file.rename(from = paste(getwd(), "/", 
          basename(tools::file_path_sans_ext(r.script.path)), 
          ".R", sep = ""), 
      to = output.path)
  return (r.script.path)
}
