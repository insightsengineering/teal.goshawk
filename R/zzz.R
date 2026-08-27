.onLoad <- function(libname, pkgname) {
  teal.logger::register_logger(namespace = "teal.goshawk")
  teal.logger::register_handlers("teal.goshawk")
}

select_decorators <- getFromNamespace("select_decorators", "teal")
