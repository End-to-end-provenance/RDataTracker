chunk.source <- function(file){
  in_chunk <- FALSE
  prov_active <- FALSE
  backticks <- "```"
  lines <- readLines(file, warn = FALSE)
  cur_chunk <- c()
  
  for(line in lines){
    if(grepl(backticks,line, fixed = TRUE)){
      if(in_chunk){
        exprs <- parse(text = cur_chunk)
        if(prov_active){
          .ddg.parse.commands(exprs)
        }else{
          .ddg.evaluate.commands(exprs)
        }
        in_chunk = FALSE
        prov_active = FALSE
        cur_chunk <- c()
      }else{
        in_chunk = TRUE
        header = substr(line,5,nchar(line)-1) 
        if(grepl("provenance = TRUE", header, fixed = TRUE)){
          prov_active <- TRUE
        }
      }
    }else if(in_chunk){
      cur_chunk <- c(cur_chunk, line)
    }
  }
}