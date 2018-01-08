.ddg.init.connection.table <- function() {
  size <- 20
  
  .ddg.set(".ddg.connections", data.frame(
          conn = numeric(size),
          source = character(size),
          isopen = logical(size),
          stringsAsFactors=FALSE)) 
  .ddg.set(".ddg.num.connections", 0)
}

.ddg.add.connection <- function(conn, source) {
  # If the table is full, make it bigger.
  ddg.connections <- .ddg.get(".ddg.connections")
  ddg.num.connections <- .ddg.get(".ddg.num.connections") + 1
  
  if (nrow(ddg.connections) < ddg.num.connections) {
    size = 20
    new.rows <- data.frame(
        conn = numeric(size),
        source = character(size),
        isopen = logical(size),
        stringsAsFactors=FALSE)
    .ddg.add.rows(".ddg.connections", new.rows)
    ddg.connections <- .ddg.get(".ddg.connections")    
  }
  
  ddg.connections$conn[ddg.num.connections] <- conn[1]
  ddg.connections$source[ddg.num.connections] <- source
  ddg.connections$isopen[ddg.num.connections] <- TRUE
  
  .ddg.set(".ddg.connections", ddg.connections) 
  .ddg.set(".ddg.num.connections", ddg.num.connections)
  print(paste(".ddg.connections =", ddg.connections))
  print(paste(".ddg.num.connections =", ddg.num.connections))
}
