.onLoad <- function(libname, pkgname) {
  teal.logger::register_logger(namespace = "teal.goshawk")
  teal.logger::register_handlers("teal.goshawk")

  if (getRversion() < "4.4") {
    assign("%||%", rlang::`%||%`, envir = getNamespace(pkgname))
  }
}
