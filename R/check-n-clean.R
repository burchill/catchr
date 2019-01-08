
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

# Checks to see if input is safe and puts it into right format
# Internal
clean_cond_input <- function(..., spec_names) {
  akw <- args_and_kwargs(...)
  v <- as_environment(
    set_names(spec_names, spec_names),
    parent = caller_env())

  warn_of_specials(akw$kwargs, spec_names)

  kwargs <- akw$kwargs %>%
    map(~eval_tidy(set_env(., v))) %>%
    map(~classify_arg(., spec_names)) %>%
    add_back_arg_pos(akw$kwargs)

  args <- akw$args %>%
    map(get_expr) %>%
    walk(~if (!is_string(.) && !is_symbol(.))
      abort("Unnamed args must be unquoted names or strings", arg=.)) %>%
    as.character() %>%
    add_back_arg_pos(akw$args)

  # Unbind the special names from v
  env_unbind(v, spec_names)

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
    default_plan = getOption("default.catchr.plan", default_catchr_plan)
  map(args, ~default_plan) %>%
    set_names(args)
}
