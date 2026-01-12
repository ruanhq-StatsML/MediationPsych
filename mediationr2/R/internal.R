.check_optional_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing required suggested package(s): %s. Please install them to use this function.",
        paste(shQuote(missing), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
