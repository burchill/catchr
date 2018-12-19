


exit_call_fn <- function(cond) {
  stop(cnd(".rlang_exit_calling_condition",
           message="Internal exit calling error",
           orig_cond = cond))
}

rlang_internal_handler <- function(cond) {
  cnd_signal(cond$orig_cond)
}

mark_orig_conditions <- calling(function(cond) {
  if (!inherits(cond, "error")) {
    class(cond) <- c(class(cond), ".rlang_checked_cond")
    cnd_signal(cond)
    cnd_muffle(cond)
  }
})

remove_checked_cond_class <- function(cond) {
  if (inherits(cond, ".rlang_checked_cond")) {
    classes <- class(cond)
    class(cond) <- classes[classes!=".rlang_checked_cond"]
  }
  cond
}
# dependent on the fact that handlers are only passed one, unnamed argument: the conditoins
mod_handlers_to_remove <- function(fn) {
  first_arg <- fn_fmls_syms(fn)[[1]]
  body <- substitute(fn(remove_checked_cond_class(first_arg)))
  new_func <- function(x) x
  formals(new_func) <- fn_fmls(fn)[1]
  body(new_func) <- body
  new_func
}


mod_handlers_to_remove <- function(fn) {
  first_arg <- fn_fmls_syms(fn)[[1]]
  body <- substitute(fn(remove_checked_cond_class(first_arg)),
                     list("fn"=fn, "first_arg"=first_arg))
  new_func <- function(x) x
  formals(new_func) <- fn_fmls(fn)[1]
  body(new_func) <- body
  new_func
}




with_both_handlers <- function (.expr, ...)
{
  handlers <- rlang:::map(list2(...), as_function)

  nms <- names2(handlers)
  nms <- ifelse(nms=="condition", ".rlang_checked_cond", nms)
  handlers <- set_names(handlers, nms)

  if (any(nms == ""))
    abort("All handlers must be named arguments")
  if (length(unique(nms)) != length(nms))
    abort("Each handler argument must have a unique name")

  fake_calling_fns <- rep(list(exit_call_fn),
                          length(handlers))

  is_calling <- rlang:::map_lgl(handlers, inherits, "calling")
  exiting <- handlers[!is_calling]

  # calling <- new_list(length(handlers), nms)
  # for (i in 1:length(handlers)) {
  #   print(handlers[[i]])
  #   print("------------")
  #   if (is_calling[[i]] == T)
  #     calling[[i]] <- mod_handlers_to_remove(handlers[[i]])
  #   else
  #     calling[[i]] <- fake_calling_fns
  # }
  # print(calling)

  calling <- ifelse(is_calling==T,
                    rlang:::map(handlers, mod_handlers_to_remove),
                    fake_calling_fns)


  names(calling) <- nms
  # add in the checker and turn them all to calling functions
  # calling <- append(list(condition = calling(mark_orig_conditions)), calling)
               # rlang:::map(calling, function(x) calling(x)))

  calling <- c(condition = calling(mark_orig_conditions),
                    rlang:::map(calling, function(x) calling(x)))

  expr <- quote(.expr)
  expr <- expr(
    tryCatch(
      tryCatch(
        withCallingHandlers(!!expr, !!!calling),
        .rlang_exit_calling_condition = rlang_internal_handler
      ), !!!exiting)
  )
  # print(calling)
  # I've been using the following to test my code in place of the `.Call` function
  eval_tidy(expr)
  # .Call(rlang_eval, expr, environment())
}



with_both_handlers(warn("Bottom warning"),
                   warning = exiting(function(x)
                     print(paste0("you had a warning: ", x$message))),
                   condition = calling(function(x) {
                     print("There was a condition but I squashed it")
                     cnd_muffle(x)}),
                   error = calling(function(x) print("C")))

with_both_handlers(warn("Bottom warning"),
                   condition = calling(function(x) {
                     print("This muffles any warnings before they can be exited")
                     print(paste0("This is the behavior one would both want and expect,",
                                  " given that handlers get checked in order."))
                     cnd_muffle(x)
                   }),
                   warning = exiting(function(x)
                     print(paste0("you had a warning: ", x$message))))

#' This is what I did at a conceptual level:
#' Since `withCallingHandlers` only accepts calling handlers, I turn everything into 'calling handlers'. However, for the handlers that I pass into `withCallingHandlers`, I replace the `exiting` handlers with a function that throws a 'unique' condition with type: '.rlang_exit_calling_condition', which contains the originally thrown condition as data. The unique condition then halts `withCallingHandlers`. However, `withCallingHandlers` is wrapped by a `tryCatch` function that catches this unique condition, extracts the original condition, and signals that to a higher `tryCatch` function that contains all of the "real" `exiting` handlers. They then do whatever they're supposed to.
#' What I did in practice is a bit more complicated. Although my code **IS** built on the assumption that the only code that will ever throw a '.rlang_exit_calling_condition' condition will be `rlang`, the way I've described the process so far would still run into problems when someone tries to catch "general" conditions (e.g., a handler like `condition = calling(print)`), which would catch the '.rlang_exit_calling_condition' condition and possibly prevent it from halting `withCallingHandlers`. I feel like catching general conditions like this is not so uncommon, so this would definitely be an issue.
#' My work-around was to modify the supplied calling handlers and automatically add a specific handler to the front of the list (I believe they checked in order). The new handler is a general condition handler, and takes every condition that is raised from `.expr`, gives it a custom type of '.rlang_checked_cond', muffles the original condition and signals the one with the additional type. Any *supplied* general condition handlers are change to catch '.rlang_checked_cond' conditions, and I modify all the supplied functions so that they remove the '.rlang_checked_cond' class before they process the condition.
#'








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

# Checks to see if input is safe and puts it into right format
clean_cond_input <- function(..., spec_names) {
  akw <- zplyr::args_and_kwargs(...)
  v <- as_environment(
    set_names(spec_names, spec_names),
    parent = caller_env())
  warn_of_specials(akw$kwargs, spec_names)
  kwargs <- akw$kwargs %>%
    map(~eval_tidy(set_env(., v))) %>%
    map(~classify_arg(., spec_names))
  args <- akw$args %>%
    map(get_expr) %>%
    walk(~if (!is_string(.) && !is_symbol(.))
      abort("Unnamed args must be unquoted names or strings", arg=.)) %>%
    as.character()
    # map(~eval_tidy(set_env(., v))) %>%
    # walk(function(arg) {
    #   if (!is_bare_string(arg))
    #     abort(paste0("Unnamed arguments (i.e., `", arg, "`) must be strings/unquoted expressions."), arg=arg)
    #   if (!(arg %in% spec_names))
    #     abort("All unquoted expressions and strings supplied must be one of the options", string = arg)
    # })
  walk(args,
       function(arg)
         if (arg %in% names(kwargs))
           abort(paste0("'", arg, "' is both an unnamed and named argument")))
  return(list(args = args, kwargs = kwargs))
}

clean_cond_input(
  error = exit,
  warning = c(display, muffle),
  message = c(collect, warning, muffle),
  spec_names = c("exit", "sup", "display", "muffle", "collect"))


















