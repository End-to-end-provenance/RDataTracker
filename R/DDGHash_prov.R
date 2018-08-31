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

########################### DDGHash.R ###########################


#' .ddg.set.hash sets the hash and rw fields for a data node 
#' @param dname data node name.
#' @param dnum the id of the node to set
#' @param dloc path and name of original file.
#' @param dvalue data node value.
#' @param dtime timestamp of original file.
#' @return nothing 
#' @noRd

.ddg.set.hash <- function (dname, dnum, dloc, dvalue, dtime) {
  
  ddg.data.nodes <- .ddg.data.node.table()
  dhash <- .ddg.calculate.hash(dname)
  ddg.data.nodes$ddg.hash[dnum] <- dhash
  
  # EF EDITS
  #drw <- .ddg.calculate.rw(dname)
  #ddg.data.nodes$ddg.rw[dnum] <- drw
  
  .ddg.set("ddg.data.nodes", ddg.data.nodes)
}
