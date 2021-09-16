# Here are our import options, as of now:

# ----- What I'm going with right now
#' @import rlang
#' @importFrom purrr %>% as_mapper detect_index imap keep lift_dl map map2 map_chr map_dbl map_if map_lgl reduce walk


# ---Lots of warnings -----------------------
# @import rlang purrr

# ---Fine, but loads purrr ------------------------
# @importFrom rlang abort as_closure as_data_mask as_list call_args call_name calling cnd_signal current_env enexpr enexprs enquo env_has env_names env_unbind eval_tidy exiting expr expr_deparse fn_fmls fn_fmls_names get_env get_expr is_bare_character is_call is_callable is_false is_function is_installed is_list is_missing is_null is_string is_symbol is_true is_vector quo_is_null set_names signal with_handlers
# @importFrom rlang child_env quo_is_call quo_is_symbol is_logical as_logical as_double dbl lgl
# @importFrom purrr imap map map_dbl map_lgl reduce walk
# @importFrom purrr keep map2 %>%

# ----Needs the crappy, off-brand purrr functions ----------------
# @importFrom magrittr %>%
# @importFrom rlang abort as_closure as_data_mask as_list call_args call_name calling cnd_signal current_env enexpr enexprs enquo env_has env_names env_unbind eval_tidy exiting expr expr_deparse fn_fmls fn_fmls_names get_env get_expr is_bare_character is_call is_callable is_function is_installed is_list is_missing is_string is_symbol is_vector quo_is_null set_names signal with_handlers
# @importFrom rlang child_env quo_is_call quo_is_symbol is_logical as_logical as_double dbl lgl


# Constants
special_terms <- c("towarning", "tomessage", "toerror",
                   "display", "beep", "exit", "muffle", "collect",
                   "raise")
options_to_hide <- c("collectors")


# source("R/check-n-clean.R")
# source("R/make-handlers.R")
# source("R/zzz.R")
# source("R/arg-wrangling.R")
# source("R/options.R")
# source("R/printing.R")


# Internal
as_list <- function(x) {
  map(x, identity)
}

# Just for signalling my own custom conditions
signal_custom_condition <- function(msg, type="custom") {
  signalCondition(
    structure(class = c(type, "condition"),
              list(message=msg, call=NULL)))
}


#' Make a string end with a newline character
#'
#' `give_newline` will append a line return ('\\n') to the end of a string if
#' it doesn't already end with one. There is also the option to remove any trailing whitespace before doing so.
#' @param s A string.
#' @param trim Indicates whether to remove trailing whitespace before adding newline.
#' @export
give_newline <- function(s, trim = FALSE) {
  if (trim == T)
    return(paste0(trimws(s, "right"), "\n"))
  if (substr(s, nchar(s), nchar(s)) != "\n")
    return(paste0(s, "\n"))
  else return(s)
}


#' Find the first 'mufflable' restart
#'
#' This function attempts to return the first available \link[base:conditions]{restart} with the string "muffle" in its name. If the condition is an error, it will attempt to find the first restart named "return_error" (used internally in catchr to return a `NULL` value). If the condition is an "interrupt", it will attempt to find the first restart named "resume". If no such restarts can be found, it returns `NULL`.
#'
#' @param cond A condition
#' @return A restart or `NULL` if none can be found.
#' @export
first_muffle_restart <- function(cond) {
  possibleRestarts <- computeRestarts(cond)
  restartNames <- Map(function(x) x$name, possibleRestarts)
  # If its an error, look for the `return_error` restart
  if (inherits(cond, "error")) {
    if ("return_error" %in% restartNames)
      return(possibleRestarts[which(restartNames=="return_error")][[1]])
  } else if (inherits(cond, "interrupt")) {
    if ("resume" %in% restartNames)
      return(possibleRestarts[which(restartNames=="resume")][[1]])
  } else {
    muffleRestarts <- possibleRestarts[grepl("muffle", restartNames, ignore.case = TRUE)]
    if (length(muffleRestarts) > 0)
      # Picks the first restart with "muffle" in its name (hack-y?)
      return(muffleRestarts[[1]])
  }
  # If it doesn't have a muffling restart and isn't an error (ie its a custom condition),
  #   just continue by returning a null value, but don't stop anything/restart
  return(NULL)
}




#' Making catchr plans
#'
#' @description
#'
#' Customizing how conditions are handled in catchr is done by giving catchr 'plans' for when it encounters particular conditions. These plans are essentially just lists of functions that are called in order, and that take in the particular condition as an argument.
#'
#' However, since catchr evaluates things \link[=catchr-DSL]{slightly differently than base R}, the user input to make these plans has to first be passed into `make_plans` (or, for setting the default plan, [set_default_plan()]). `make_plans` also lets users specify options for how they want these plans to be evaluated with the `.opts` argument (see [catchr_opts()] for more details).
#'
#' See the 'Input' section below and the examples for how to use `make_plans`.
#'
#'
#' @section Input:
#'
#' User input to `make_plans` is very similar to how one makes handlers for [base::withCallingHandlers()], [base::tryCatch()] and `rlang`'s [rlang::with_handlers()], albeit with some important differences.
#'
#' Like the functions above, the name of each argument determines which type of condition it will be the plan for. Hence, `warnings = fn` will apply the `fn` function to the warnings raised in evaluating `expr`.
#'
#' However, *unnamed* arguments are *also* accepted: the value of any unnamed arguments will be treated as the type of a condition, which will then have the default plan assigned to it, as specified either in `.opts = catchr_opts(...)` or via `getOption("catchr.default_plan")`. Unnamed arguments must be either strings or unquoted expressions which will then be converted to strings. Currently, unnamed arguments are _never_ evaluated, so cannot be calls that evaluate to strings.
#'
#' **However, this may change in future versions of catchr.**
#'
#' @section Passing input in programmatically:
#'
#' `make_plans` supports \code{\link[rlang]{quasiquotation}}, so if for some reason one wishes to pass input into `make_plans` via a different function, programmatically, etc., one may do so by splicing in quosures. See below for examples.
#'
#' @examples
#' # ### INPUT EXAMPLES ###########################
#'
#' # Named arguments --------------------------------------
#'
#' #   * single functions:
#' p <- make_plans(warning = str, message = function(x) print(x))
#'
#' #   * single unquoted expressions and strings
#' #     (must match catchr's special reserved terms, e.g., 'muffle', 'exit', etc.):
#' p <- make_plans(message = muffle, condition = "collect")
#'
#' #   * lists or vectors of any combinatin of the above:
#' p <- make_plans(error = list(collect, "exit"),
#'                 message = c(cat, "muffle"))
#'
#' #   * anything that evaluates to the above:
#' fn <- function() { list(cat, "muffle") }
#' p <- make_plans(message = fn() )
#'
#' # Unnamed arguments ----------------------
#'
#' #   * single strings:
#' p <- make_plans("warning","condition")
#'
#' #   * unquoted expressions:
#' p <- make_plans(warning,condition)
#'
#' #   * Currently, does NOT accept anything that evaluates to strings:
#' #       (However, this may change in the future)
#' \dontrun{
#' string_fn <- function() { "condition" }
#' make_plans(string_fn()) # will currently raise error
#' }
#'
#' # Mixes of both --------------------------
#' p <- make_plans("warning", message = c(towarning, muffle),
#'                 condition = print)
#'
#' # ### Quasiquotation and splicing in the arguments ###############
#'
#' q <- rlang::quo(function(cond) {print(cond)})
#' name <- "warning"
#'
#' print_plan <- make_plans(!!name := !!q)
#'
#' # 'message' will be assigned the default plan
#' qs <- rlang::quos(warning = muffle, error = exit, message)
#' random_plan <- make_plans(!!!qs)
#'
#' @param \dots Named and unnamed arguments for making plans. See 'Input' for more detail.
#' @param .opts The options to be used for the plan. Generally passed in using [catchr_opts()].
#' @aliases catchr-plans
#' @export
make_plans <- function(..., .opts = catchr_opts()) {
  if (is.null(.opts)) .opts <- catchr_opts()
  akw <- check_and_clean_input(..., spec_names = special_terms)
  args <- give_default(akw$args, default_plan = .opts$default_plan) %>%
    as_list() %>% add_back_arg_pos(akw$args)

  kwargs <- append(as_list(akw$kwargs), args) %>%
    order_by_arg_pos()

  if (length(kwargs) == 0)
    abort("Must have at least one plan specified.")

  original_calls <- enexprs(...)
  compile_plans(kwargs, .opts, original_calls)
}






#' Catch conditions
#'
#' @description
#'
#' These are function that actually evaluate expression and "catch" the conditions. `catch_expr()` evaluates an expression, catching and handling the conditions it raises according to whatever \link[=make_plans]{catchr plans} are specified.  `make_catch_fn()` is a function factory that returns a function that behaves like `catch_expr()` with the plans already specified.
#'
#' Plans can be passed in as output from `make_plans()` or as input that follows the same format as the input to `make_plans()`.
#'
#' @param expr the expression to be evaluated
#' @param \dots a catchr plan as made by [make_plans()] or input for plans that follows the same format as input to `make_plans()`
#' @param .opts The options to be used for the plans (generally passed in using [catchr_opts()]). If the input plans were already made by `make_plans()`, setting this will override whatever options were specified earlier.
#' @return For `catch_expr()`: The value of the evaluated expression if there isn't an error and if the plans don't force an exit. If `getOption("catchr.bare_if_possible")` is `FALSE` (or if any conditions have been collect), it will return a named list, with the "value" element containing the value of the evaluated expression and sublists containing any collected conditions. \cr \cr For `make_catch_fn()` A function that catches conditions for expressions the same way `catch_expr()` would, but with the plans already specified.
#' @examples
#' warner <- function() {
#'   warning("Suppress this!")
#'   "done!"
#' }
#'
#' compiled_warning_plans <- make_plans(warning = muffle)
#' warning_catcher <- make_catch_fn(warning = muffle)
#' warning_catcher2 <- make_catch_fn(compiled_warning_plans)
#'
#' # `results` 1-4 are equivalent
#' results1 <- catch_expr(warner(), warning = muffle)
#' results2 <- warning_catcher(warner())
#' results3 <- catch_expr(warner(), compiled_warning_plans)
#' results4 <- warning_catcher2(warner())
#' @rdname catchers
#' @export
catch_expr <- function(expr, ..., .opts=NULL) {
  make_catch_fn(..., .opts = .opts)(expr)
}

#' @rdname catchers
#' @export
make_catch_fn <- function(..., .opts = NULL) {
  if (check_if_args_compiled(...)) plans <- eval_tidy(enquos(...)[[1]])
  else plans <- make_plans(..., .opts = .opts)

  .opts <- decide_opts(plans, .opts)

  function(expr) {
    .myConditions <- NULL
    baby_env <- child_env(current_env())
    expr <- rlang::new_quosure(substitute(expr), parent.frame())

    # If you keep empty conds, make 'em now
    if (!.opts$drop_empty_conds && length(.opts$collectors) > 0)
      for (c_type in .opts$collectors)
        .myConditions[[c_type]] <- list()

    kwargs <- plans %>%
      map(~`environment<-`(., baby_env))

    res <- withRestarts(with_only_calling_handlers(!!expr, !!!kwargs),
                        return_error = function() NULL)

    if (.opts$bare_if_possible && is.null(.myConditions))
      res
    else
      append(list(value = res), .myConditions)
  }
}

# Checks the handlers one last time before running things
final_handler_check <- function(...) {
  handlers <- list2(...)
  last_stop <- handlers[["last_stop"]]

  if (!is_function(last_stop))
    abort("Plans should have a 'last_stop' handler at this stage")
  handlers[["last_stop"]] <- NULL

  bad_handlers <- handlers %>%
    keep(~!is_function(.)) %>%
    map(typeof) %>% unlist()

  if (length(bad_handlers) > 0)
    stop("'", paste0(names(bad_handlers), collapse="', "), "' handlers are not functions. (",
         paste0(bad_handlers, collapse=", "), ", respectively)")

  if (any(map_lgl(handlers, function(x) inherits_any(x, c("exiting", "rlang_handler_exiting", "calling", "rlang_box_calling_handler", "rlang_handler")))))
    warning("All rlang handler types (e.g., 'calling', 'exiting') are being ignored.")

  return(list(last_stop=last_stop, handlers = handlers))

}

# Internal
with_only_calling_handlers <- function(.expr, ...) {
  all_handlers <- final_handler_check(...)
  last_stop <- all_handlers["last_stop"]
  handlers <- all_handlers$handlers
  expr <- enquo(.expr)
  expr <- quo(withCallingHandlers(!!expr, !!!handlers))
  expr <- quo(tryCatch(!!expr, !!!last_stop))

  eval_tidy(expr)
}

# decides which opts to use
decide_opts <- function(plans, .opts) {
  collectors <- attr(plans, "catchr_opts", exact = T)$collectors
  if (is.null(.opts)) {
    # If there are no opts specified, but the plans have them, use them
    if (!is.null(attr(plans, "catchr_opts")))
      .opts <- attr(plans, "catchr_opts", exact=T)
    # If the are no options anywhere, get the defaults
    else .opts <- catchr_opts()
  }
  .opts$collectors <- collectors
  .opts
}

# Check if the arguments supplied were made by `make_plans` already
check_if_args_compiled <- function(...) {
  qs <- enquos(...)
  if (length(qs) > 1) return(FALSE)
  tryCatch(is_catchr_plan(eval_tidy(qs[[1]])),
           error = function(x) return(FALSE))
}

#' Return the value after raising all collected conditions
#'
#' @description
#'
#' This function takes in the [collected conditions list][collect] that is the output of [catch_expr()] or a function from [make_catch_fn()], raises all the conditions that were collected, and then returns the value the original evaluated expression had returned.  This might be useful in situations in which one had collected conditions from a remote evaluation of an expression, and wishes to raise the conditions locally.
#'
#' The way the errors are treated can be changed as well: they can either be raised as-is, displayed on screen, or raised as warnings.
#'
#' @param l The results of [catch_expr()] or a function from [make_catch_fn()]
#' @param treat_errs One of three strings governing how errors are treated: `"raise"` which will simply raise errors as they are, `"display"` which will just print the error messages on-screen, and `"warn"` which will raise the errors as warnings.
#' @export
dispense_collected <- function(l, treat_errs = c("raise", "display", "warn")) {
  treat_errs <- match.arg(treat_errs)

  raise_cond <- function(cond, err_behav) {
    raise_f <- cnd_signal
    if (inherits(cond, "error"))
      raise_f <- switch(err_behav,
                        raise = raise_f,
                        display = function(x) cat("Error:", trimws(x$message, "both"), file=stderr()),
                        warn = function(cond) {
                          class(cond) <- c("warning","condition")
                          warning(cond)
                        })

    withRestarts(raise_f(cond), catchr_muffle = function() NULL)
  }

  if (!is_named(l) || !is_list(l))
    return(l)
  if (!("value" %in% names(l)))
    return(l)
  res <- l[["value"]]
  l[["value"]] <- NULL

  for (sublist in l) {
    walk(sublist, ~raise_cond(., treat_errs))
  }
  res
}

