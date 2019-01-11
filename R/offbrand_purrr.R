
# off-brand purrr analogues
map <- function (.x, .f, ...) {
  .f <- as_closure(.f)
  Map(.f, .x, ...)
}

map2 <- function (.x, .y, .f, ...) {
  stopifnot(length(.x) == length(.y))
  .f <- as_closure(.f)
  out <- Map(.f, .x, .y, ...)
  set_names(out, names(.x))
}

imap <- function (.x, .f, ...) {
  if (is.null(names(.x)))
    map2(.x, seq_along(.x), .f, ...)
  else
    map2(.x, names(.x), .f, ...)
}

map_lgl <- function (.x, .f, ...) {
  res <- map(.x, .f, ...)
  if (length(res) == 0)
    lgl()
  else
    res %>%
    unlist() %>%
    as_logical()
}

map_dbl <- function (.x, .f, ...) {
  res <- map(.x, .f, ...)
  if (length(res) == 0)
    dbl()
  else
    res %>%
    unlist() %>%
    as_logical()
}

reduce <- function (.x, .f, ..., .init) {
  .f <- as_closure(.f)
  Reduce(.f, .x, init = .init)
}

walk <- function(.x, .f, ...) {
  .f <- as_closure(.f)
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  invisible(.x)
}


keep <- function (.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    woo <- .p
  } else {
    woo <- map_lgl(.x, .p, ...)
  }
  .x[!is.na(woo) & woo]
}




