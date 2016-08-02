# An S4 class to maintain information about a command
# 
# Author: blerner
###############################################################################


setClass("Command",
    slots = list(
        text = "character",
        parsed = "expression",
        quoted = "character",
        abbrev = "character"))
    
