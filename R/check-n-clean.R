
# Recursively moves through AST (kinda)
check_nodes <- function(x, nms, check_calls = F, depth = NA) {
  depth = depth - 1
  if (!is.na(depth) && depth < 0)
    return(NULL)

  if (check_calls) {
    if (is_call(x) && call_name(x) %in% nms)
      withRestarts(
        signal("Reserved symbol found", .subclass="passer", val=deparse(x)),
        get_back_to_work = function() NULL)
  } else {
    if (is_symbol(x) && deparse(x) %in% nms)
      withRestarts(
        signal("Reserved symbol found", .subclass="passer", val=deparse(x)),
        get_back_to_work = function() NULL)
  }

  # e.g. `beepr::beep` shouldn't ruffle feathers
  if (is_call(x) && !(call_name(x) %in% c("::", ":::")))
    call_args(x) %>%
    map(~check_nodes(., nms, check_calls, depth))
}

# Checks all the symbols in the AST
find_used_symbols <- function(x, nms, check_calls = F, depth = NA) {
  expr <- enexpr(x)
  symbol_list <- NULL

  handler <- function(cond) {
    symbol_list <<- append(symbol_list, cond$val)
    invokeRestart("get_back_to_work")
  }

  withCallingHandlers(
    check_nodes(expr, nms, check_calls, depth),
    passer = handler
  )
  return(unique(symbol_list))
}

# Gets the specials. qs is a list of quosures
get_used_specials <- function(qs, names_to_check, ...) {
  qs %>%
    keep(~quo_is_call(.) | quo_is_symbol(.)) %>%
    map(function(q) {
      l <- find_used_symbols(!!get_expr(q), names_to_check, ...)
      if (!is.null(l))
        env_has(env = get_env(q), nms=l, inherit = T) %>%
        keep(~.==T) %>% names()
    }) %>%
    unlist() %>%
    unique()
}

# Basically to check for `user_exit`
check_for_calls <- function(qs, names_to_check, message, ...) {
  l <- qs %>%
    keep(~quo_is_call(.) | quo_is_symbol(.)) %>%
    map(function(q)
      find_used_symbols(!!get_expr(q), names_to_check, check_calls = T, ...)) %>%
    unlist()

  if (length(l) > 0) warning(paste0(message, "`", l[[1]], "`"),
                             immediate. = TRUE, call. = FALSE)
  invisible(NULL)
}

# The warning was bulky so I moved it here
warn_of_specials <- function(x) {
  if (length(x) > 0) {
    agreement <- ""
    verb <- "have"
    if (length(x) == 1) {
      agreement <- "s"
      verb <- "has"
    }
    agreement <- ifelse(length(x)==1, "has", "have")
    warning("`", paste(x, collapse = "`, `"),
            "` ", verb, " special meaning as catchr input, but seem", agreement, " to already be defined elsewhere.  These previous definitions may be masked when determining condition behavior.",
            immediate. = TRUE, call. = FALSE)
  }
}


# used to get some form of "helpful-ish" name for something passed in.
# need to use splicing to pass stuff in
approx_arg_name <- function(x, len = 25) {
  v <- get_expr(enquo(x)) %>% expr_deparse(999) %>% paste(collapse = "")
  add_ellipses(v, len)
}




#' Make sure a function can be a handler
#'
#' This makes sure that a given function doesn't *require* more than one argument to be passed into it, and takes in at least one argument (which is what a \link[base:conditions]{handler} needs).
#'
#' @param fn A function that is a candidate for being a handler
#' @export
has_handler_args <- function(fn) {
  args <- Map(is_missing, fn_fmls(args(fn))) # purrr can't iterate over pairlist
  needed <- args %>% keep(~.) %>% length()
  supplied <- args %>% keep(~!.) %>% length()
  has_dots <- "..." %in% fn_fmls_names(args(fn))
  return(needed == 1 || (needed == 0 && supplied > 0) || has_dots)
}

# checks to see if one of the elements in an argument meets criteria
classify_el <- function(el, nono_words) {
  el_expr <- approx_arg_name(!!el)
  if (is_function(el) && !has_handler_args(el))
    abort(paste0(el_expr, " must take at least one argument to be in a catchr plan"), fn = el)
  else if (is_string(el) && !(el %in% nono_words))
    abort(paste0(el_expr, " is not one of catchr's special reserved terms"), string = el)
  else if (!is_string(el) && !is_function(el))
    abort(paste0(el_expr, " must be a string, unquoted expression, or function, but is type '", typeof(el), "'"), arg=el)
}

# checks arguments to see if they meet criteria
classify_arg <- function(arg, nono_words) {
  arg_expr <- approx_arg_name(!!arg)
  if (length(arg) > 1 || is_list(arg)) {
    if (!is_list(arg) && !is_bare_character(arg))
      abort(paste0("Input `", arg_expr, "` has an invalid type: '", typeof(arg), "'"), val=arg)
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

# The internal version
clean_input <- function(qs, spec_names = NULL) {
  if (is.null(spec_names))
    spec_names <- special_terms

  mask <- make_catchr_mask(spec_names)
  res <- qs %>%
    map(~eval_tidy(., data = mask)) %>%
    map(~classify_arg(., spec_names)) %>%
    add_back_arg_pos(qs)

  env_unbind(parent.env(mask), env_names(parent.env(mask)))

  res
}

# Checks to see if input is safe and puts it into right format
# Internal
check_and_clean_input <- function(..., spec_names) {
  akw <- args_and_kwargs(...)
  if (getOption("catchr.warn_about_terms", FALSE))
    warn_of_specials(get_used_specials(akw$kwargs, spec_names))

  check_for_calls(akw$kwargs, c("user_exit", "user_display"), "`user_exit/user_display` is being called in the input to a plan at a very shallow level, possibly meaning that it is not in a function. Remember that these functions need to be IN a function or passed in AS a function, not a call. The call in question: ", depth=2)

  kwargs <- clean_input(akw$kwargs, spec_names)

  args <- unnamed_args_to_strings(akw$args)

  check_for_duplicates(args, names(kwargs))

  # Check args for duplicated names
  walk(args, function(arg)
    if (arg %in% names(kwargs))
      abort(paste0("'", arg, "' is both an unnamed and named argument")))
  return(list(args = args, kwargs = kwargs))
}

# throws error if there are duplicates
check_for_duplicates <- function(l, ...) {
  l <- append(l, list(...))
  dupes <- l[duplicated(l)] %>% unique()

  if (length(dupes) > 0)
    abort(paste0("Conditions cannot have multiple plans: ",
                 paste0("'", dupes, "'", collapse = ",")))
  NULL
}

unnamed_args_to_strings <- function(x) {
  x %>%
    map(get_expr) %>%
    walk(~if (!is_string(.) && !is_symbol(.))
      abort("Unnamed args must be unquoted names or strings", arg=.)) %>%
    as.character() %>%
    add_back_arg_pos(x)
}
