# I don't think I need any of this!!!

#.ddg.init.connection.table <- function() {
#  size <- 20
#  
#  # TODO:  Not sure that I need mode and isopen.
#  .ddg.set(".ddg.connections", data.frame(
#          conn = numeric(size),
#          source = character(size),
#          mode = character(size),
#          isopen = logical(size),
#          stringsAsFactors=FALSE)) 
#  .ddg.set(".ddg.num.connections", 0)
#}
#
#.ddg.add.connection <- function(conn, source, mode="") {
#  print(paste("Adding a connection:", conn, source))
#  # If the table is full, make it bigger.
#  ddg.connections <- .ddg.get(".ddg.connections")
#  ddg.num.connections <- .ddg.get(".ddg.num.connections") + 1
#  
#  if (nrow(ddg.connections) < ddg.num.connections) {
#    size = 20
#    new.rows <- data.frame(
#        conn = numeric(size),
#        source = character(size),
#        mode = character(size),
#        isopen = logical(size),
#        stringsAsFactors=FALSE)
#    .ddg.add.rows(".ddg.connections", new.rows)
#    ddg.connections <- .ddg.get(".ddg.connections")    
#  }
#  
#  ddg.connections$conn[ddg.num.connections] <- conn[1]
#  ddg.connections$source[ddg.num.connections] <- source
#  if (isOpen (conn)) {
#    ddg.connections$isopen[ddg.num.connections] <- TRUE
#    ddg.connections$mode[ddg.num.connections] <- 
#        if (isOpen (conn, "read")) {
#          if (isOpen (conn, "write")) "rw"
#          else "r"
#        }
#        else if (isOpen (conn, "write")) "w"
#  }
#  else {
#    ddg.connections$isopen[ddg.num.connections] <- FALSE
#    ddg.connections$mode[ddg.num.connections] <- NA
#  }
#
#  
#  .ddg.set(".ddg.connections", ddg.connections) 
#  .ddg.set(".ddg.num.connections", ddg.num.connections)
#  print(paste(".ddg.connections =", ddg.connections))
#  print(paste(".ddg.num.connections =", ddg.num.connections))
#}
#
#.ddg.get.connected.file <- function (conn) {
#  ddg.connections <- .ddg.get(".ddg.connections")
#  print(ddg.connections)
#  
#  # TODO: Get the last connection with the matching id
#  matches <- ddg.connections [ which (ddg.connections$conn == conn[1]), ]
#  matches <- matches [nrow(matches), ]
#  
#  # Return its source value
#  print(paste(".ddg.get.connected.file returning", matches$source))
#  return (matches$source)
#}
