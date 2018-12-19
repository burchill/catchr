# Recursively moves through AST
check_nodes <- function(x, nms) {
  if (is_symbol(x) && deparse(x) %in% nms)
    signal(paste0("Reserved symbol in arguments: ", deparse(x)),
           "passer", val=deparse(x))
  if (is_call(x))
    call_args(x) %>%
    map(~check_nodes(., nms))
}

# Checks all the symbols in the AST
find_used_symbols <- function(x, nms) {
  expr <- enexpr(x)
  symbol_list <- NULL

  handler <- function(cond)
    symbol_list <<- append(symbol_list, cond$val)

  withCallingHandlers(
    check_nodes(expr, nms),
    passer = handler
  )
  return(unique(symbol_list))
}

# checks the kwargs for special symbols defined elsewhere
warn_of_specials <- function(qs, names_to_check) {
  bad_boys <- qs %>%
    keep(~quo_is_call(.) | quo_is_symbol(.)) %>%
    map(function(q) {
      l <- find_used_symbols(!!get_expr(q), names_to_check)
      if (!is.null(l))
        env_has(env=get_env(q), nms=l, inherit = T) %>%
        keep(~.==T) %>% names()
    }) %>%
    unlist() %>%
    unique()
  if (length(bad_boys) > 0)
    warning("`", paste(bad_boys,collapse = "`,`"),
            "` have special meaning in these arguments, but seem to already be defined elsewhere.  These previous definitions will not be used in determining condition behavior.",
            immediate. = TRUE, call. = FALSE)
  invisible()
}

# Can't require more than one argument passed into it
has_handler_args <- function(fn) {
  args <- fn_fmls(fn) %>%
  {Map(is_missing, .)} # purrr can't iterate over pairlist
  needed <- args %>% keep(~.) %>% length()
  supplied <- args %>% keep(~!.) %>% length()
  return(needed == 1 || (needed == 0 && supplied > 0))
}

# checks to see if one of the elements in an argument meets criteria
classify_el <- function(el, nono_words) {
  if (is_function(el) && !has_handler_args(el))
    abort("All functions supplied must take one argument", fn = el)
  else if (is_string(el) && !(el %in% nono_words))
    abort("All unquoted expressions and strings supplied must be one of the options", string = el)
  else if (!is_string(el) && !is_function(el))
    abort("Arguments supplied must evaluate to strings, unquoted expressions, or functions", arg=el)
}

# checks arguments to see if they meet criteria
classify_arg <- function(arg, nono_words) {
  if (length(arg) > 1) {
    if (!is_list(arg) && !is_bare_character(arg))
      abort(paste0("`", arg, "` has an invalid type: ", typeof(arg)), val=arg)
    walk(arg, ~classify_el(., nono_words))
  } else
    classify_el(arg, nono_words)
  invisible(arg)
}

clean_cond_input <- function(..., spec_names) {
  akw <- zplyr::args_and_kwargs(...)
  v <- as_environment(
    set_names(spec_names, spec_names),
    parent = caller_env())
  warn_of_specials(akw$kwargs, spec_names)
  kwargs <- akw$kwargs %>%
    map(~eval_tidy(set_env(., v))) %>%
    map(~classify_arg(., spec_names))
  args <- akw$args  %>%
    map(~eval_tidy(set_env(., v))) %>%
    walk(function(arg) {
      if (!is_bare_string(arg))
        abort(paste0("Unnamed arguments (i.e., `", arg, "`) must be strings/unquoted expressions."), arg=arg)
      if (!(arg %in% spec_names))
        abort("All unquoted expressions and strings supplied must be one of the options", string = arg)
    })
  return(list(args = args, kwargs = kwargs))
}

clean_cond_input(
  error = exit,
  warning = c(display, muffle),
  message = c(collect, warning, muffle),
  spec_names = c("exit", "sup", "display", "muffle", "collect"))
