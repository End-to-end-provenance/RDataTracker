#' @export
prov.capture <- function(r.script.path=NULL, enable.console = TRUE, annotate.inside.functions = FALSE, first.loop = 1, max.loops = 1){
  # Initiate ddg.
  ddg.init(r.script.path, NULL, FALSE, enable.console, annotate.inside.functions, first.loop, max.loops, 0)

  # Set .ddg.is.sourced to TRUE if script provided.
  if (!is.null(r.script.path)) .ddg.set(".ddg.is.sourced", TRUE)

  # Save debug files to debug directory.
  .ddg.set("ddg.save.debug", FALSE)

  # If an R error is generated, get the error message and close
  # the DDG.
  if (!is.null(r.script.path)) ddg.source(
       .ddg.get("ddg.r.script.path"),
        ddgdir = NULL,
        ignore.ddg.calls = FALSE,
        ignore.init = TRUE,
        force.console = FALSE)
  else stop("r.script.path cannot be NULL")
  invisible()
}

prov.json <- function(){
  ddg.json <- .ddg.json.current()
}
