

assert_positive_numeric <- function(x, name) { # nocov start
  if (!is.numeric(x)) cli::cli_abort("{.arg {name}} has to be a numeric vector.")
  if (any(x <= 0, na.rm = TRUE)) cli::cli_abort("There are negative values in {.arg {name}}. Review your data.")
} # nocov end


assert_same_length <- function(..., names) { # nocov start
  args <- list(...)
  lengths <- sapply(args, length)
  if (length(unique(lengths)) != 1) {
    cli::cli_abort("The following arguments must have the same length: {.arg {names}}.")
  }
} # nocov end

assert_logical <- function(x, name) { # nocov start
  if (!is.logical(x)) cli::cli_abort("{.arg {name}} has to be a logical vector.")
} # nocov end

assert_scalar_numeric <- function(x, name) { # nocov start
  if (!is.numeric(x) || length(x) != 1) cli::cli_abort("{.arg {name}} has to be a single numeric value.")
} # nocov end

assert_greater_than <- function(x, threshold, name) { # nocov start
  if (any(x <= threshold, na.rm = TRUE)) cli::cli_abort("All values in {.arg {name}} must be greater than {threshold}.")
} # nocov end

assert_numeric_interval <- function(arg, minn, maxx, ref) { # nocov start

    if (!is.numeric(arg) || min(arg) < minn || max(arg) > maxx) {
        cli::cli_abort(
            "{.arg {ref}} must be a single numeric value between {minn} and {maxx}.",
            .frame = parent.frame()
        )
    }
} # nocov end