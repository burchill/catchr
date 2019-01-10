
# Recursively moves through AST (kinda)
check_nodes <- function(x, nms) {
  if (is_symbol(x) && deparse(x) %in% nms)
    signal(paste0("Reserved symbol in arguments: ", deparse(x)),
           "passer", val=deparse(x))
  #               e.g. `beepr::beep` shouldn't ruffle feathers
  if (is_call(x) && !(call_name(x) %in% c("::", ":::")))
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

#' Check expressions for special terms
#'
#' To-do: write doc
#'
#' @param qs A list of quosures.
#' @param names_to_check A character vector of reserved special terms to check the expressions for.
#' @export
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
    warning("`", paste(bad_boys,collapse = "`, `"),
            "` have special meaning in these arguments, but seem to already be defined elsewhere.  These previous definitions may be masked when determining condition behavior.",
            immediate. = TRUE, call. = FALSE)
  invisible()
}


#' Make sure a function can be a handler
#'
#' This makes sure that a given function doesn't *require* more than one argument to be passed into it, and takes in at least one argument (which is what a \link[base:conditions]{handler} needs).
#'
#' @param fn A function
#' @export
has_handler_args <- function(fn) {
  args <- Map(is_missing, fn_fmls(fn)) # purrr can't iterate over pairlist
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
  if (length(arg) > 1 || is_list(arg)) {
    if (!is_list(arg) && !is_bare_character(arg))
      abort(paste0("`", arg, "` has an invalid type: ", typeof(arg)), val=arg)
    walk(arg, ~classify_el(., nono_words))
  } else
    classify_el(arg, nono_words)
  invisible(arg)
}

# Generates a new catchr data mask
make_catchr_mask <- function(nms = special_terms) {
  as_list(nms) %>%
    set_names(nms) %>%
    as_data_mask()
}

# the problem is whether to make a lot of environments or just one
clean_input <- function(qs, spec_names = NULL) {
  if (is.null(spec_names)) {
    mask <- make_catchr_mask()
    spec_names <- special_terms
  } else {
    mask <- make_catchr_mask(spec_names)
  }

  res <- qs %>%
    map(~eval_tidy(., data = mask)) %>%
    map(~classify_arg(., spec_names)) %>%
    add_back_arg_pos(qs)

  env_unbind(parent.env(mask), env_names(parent.env(mask)))
  res
}


# Checks to see if input is safe and puts it into right format
# Internal
clean_cond_input <- function(..., spec_names) {
  akw <- args_and_kwargs(...)

  warn_of_specials(akw$kwargs, spec_names)

  kwargs <- clean_input(akw$kwargs, spec_names)

  args <- akw$args %>%
    map(get_expr) %>%
    walk(~if (!is_string(.) && !is_symbol(.))
      abort("Unnamed args must be unquoted names or strings", arg=.)) %>%
    as.character() %>%
    add_back_arg_pos(akw$args)

  # Check args
  walk(args,
       function(arg)
         if (arg %in% names(kwargs))
           abort(paste0("'", arg, "' is both an unnamed and named argument")))
  return(list(args = args, kwargs = kwargs))
}

# Turns the unnamed arguments into the defaults
give_default <- function(args, default_plan = NULL) {
  if (is.null(default_plan))
    default_plan = getOption("catchr.default_plan", catchr.default_plan)
  map(args, ~default_plan) %>%
    set_names(args)
}
