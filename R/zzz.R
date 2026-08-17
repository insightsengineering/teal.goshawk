.onLoad <- function(libname, pkgname) {
  teal.logger::register_logger(namespace = "teal.goshawk")
  teal.logger::register_handlers("teal.goshawk")
}
