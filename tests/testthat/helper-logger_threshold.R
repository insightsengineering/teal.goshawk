local_log_threshold <- function(level = "WARN", envir = parent.frame()) {
  if (requireNamespace("logger", quietly = TRUE)) {
    threshold <- logger::log_threshold(namespace = "teal.goshawk")
    logger::log_threshold(level, namespace = "teal.goshawk")
    withr::defer(logger::log_threshold(threshold, namespace = "teal.goshawk"), envir = envir)
  }
}
