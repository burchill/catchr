#' Get/set the input for the default catchr plan
#'
#' These functions allow the user to set and retrieve the input that will be assigned to any conditions passed to [make_plans()] without plans (i.e., as unnamed arguments). Using the same inputting style as `make_plans()`, the argument `new_plan` will essentially be treated the same way a single named argument would be in `make_plans()`, without actually having a name/specific condition.
#'
#' @param new_plan The input (in the style of named arguments to [make_plans()]) that will become the input of default plan.
#' @return `set_default_plan()` will invisibly return a "cleaned up" version of the input (i.e., evaluated, and with the unquoted terms replaced with strings), which is what will also be returned by `get_default_plan()` until a new default is set.
#' @rdname default_plan
#' @seealso [default catchr options][default-catchr-options]
#' @export
set_default_plan <- function(new_plan) {
  q <- enquo(new_plan)
  default_plan <- clean_input(list(default = q))$default
  options("catchr.default_plan" = default_plan)
  invisible(default_plan)
}
#' @rdname default_plan
#' @export
get_default_plan <- function() {
  getOption("catchr.default_plan")
}

#' Pass in catchr-specific options
#'
#' @description
#'
#' catchr offers a number of options for planning condition handling, and the `catchr_opts` function
#'  provides a way of passing those options to whatever function is handling the planning. If any
#'  argument is left unspecified it defaults to the global defaults (accessible via
#'  [catchr_default_opts()], [base::options()], or [base::getOption()]).
#'
#' @section Catchr options:
#'
#'  catchr's options are specified below. The names of the global default option are preceded by `"catchr."` so they don't collide with other packages' options (i.e., `drop_empty_conds` can be accessed via `getOption("catchr.drop_empty_conds")`:
#'
#'  * `default_plan`: The default plan that will be used for unnamed arguments (i.e., conditions specified without plans) to [make_plans()] or the like. See [get_default_plan()] for more details. The original package default is `c("collect", "muffle")`.
#'  * `warn_about_terms`: If one of its [reserved terms][catchr-DSL] would mask a previously defined variable name when catchr is compiling plans, you can specify whether or not a warning will be generated. The original package default is `TRUE`, which will warn the user of these occurrences.
#'  * `bare_if_possible`: When no plans are set to [collect] conditions, you have the option of returning the value of the evaluated expression by itself, *without* being the `$value` element of a list. If `bare_if_possible` is `TRUE` and no plans collect conditions, it will return the value without the wrapping list. If one is using catchr extensively, it might be wise to set this option to `FALSE` so catchr's returned values are always consistent. The original package default is `TRUE`.
#'  * `drop_empty_conds`: If conditions have plans that would collect them but none are raised in the evaluation of an expression, you have the option of dropping their sublists. For example, conditions that aren't warnings, messages, or errors are very rare. If you wanted to return the ["misc"][reserved-conditions] condition sublist only when such conditions were raised, you could do this by setting the value to `TRUE`. The original package default is `FALSE`.
#'
#' @param default_plan The default plan for unnamed input arguments. See [get_default_plan()] for more details.
#' @param warn_about_terms A logical; if `FALSE`, will not warn about masking special terms
#' @param bare_if_possible A logical; if `TRUE`, and no conditions are collected, will return the result of the evaluated expression as-is, without encompassing named list.
#' @param drop_empty_conds A logical; if `TRUE`, the sublists for conditions that used `collect` but didn't collect anything will be dropped from the list. Otherwise, they will appear as empty sublists.
#' @seealso [The default catchr options][default-catchr-options], [set_default_plan()], [get_default_plan()]
#' @export
catchr_opts <- function(default_plan = NULL,
                        warn_about_terms = NULL,
                        bare_if_possible = NULL,
                        drop_empty_conds = NULL) {

  q_plan <- enquo(default_plan)

  if (quo_is_null(q_plan))
    default_plan <- getOption("catchr.default_plan")
  else
    default_plan <- clean_input(list(default = q_plan))$default

  if (is.null(warn_about_terms))
    warn_about_terms <- getOption("catchr.warn_about_terms")
  if (is.null(bare_if_possible))
    bare_if_possible <- getOption("catchr.bare_if_possible")
  if (is.null(drop_empty_conds))
    drop_empty_conds <- getOption("catchr.drop_empty_conds")

  binary_vals <- list(
    warn_about_terms = warn_about_terms,
    bare_if_possible = bare_if_possible,
    drop_empty_conds = drop_empty_conds)

  if (!all(map_lgl(binary_vals, is_true_or_false)))
    abort("All binary catchr options must be either TRUE or FALSE")

  append(list(default_plan = default_plan), binary_vals)
}

#' Default catchr-specific options
#'
#' @description
#'
#' catchr's options for planning condition handling are passed into catchr functions with [catchr_opts()], but when an option isn't specified in the call, `catchr_opts()` uses whatever the default for that option is. You can get and set these global defaults with `catchr_default_opts()` and do a "factory reset" on them to restore the original package values with `restore_catchr_defaults()`.
#'
#' @section Arguments:
#'
#' For `catchr_default_opts()`, unnamed arguments (unquoted terms / strings of the option names) will have their current default values returned, similar to `getOption()`. Named arguments (whose names are option names) will have their default values *set* to whatever their value is. If no arguments are specified, it will return all the current default values.
#'
#' `restore_catchr_defaults()` only accepts unnamed arguments (unquoted terms / strings of the option names). The options specified will have their default values set to the *original* default package values. Leaving the arguments empty will result in _all_ the option defaults being reset to their original values.
#'
#' @param \dots Default options to get or set. See the Arguments section for details.
#' @name default-catchr-options
#' @rdname default_catchr_options
#' @seealso [catchr_opts()] for what the options mean; [get_default_plan()] and [set_default_plan()], which are equivalent to `catchr_default_opts(default_plan)` and `catchr_default_opts(default_plan = ...)`, respectively.
#' @export
# Ugh, BEASTLY (in a bad way) code
catchr_default_opts <- function(...) {
  possible_values <- c("default_plan", "warn_about_terms", "bare_if_possible", "drop_empty_conds")
  possible_values <- c(possible_values, add_catchr_prefix(possible_values))
  akw <- args_and_kwargs(...)

  returning_vals <- unnamed_args_to_strings(akw$args) %>%
    map(~`attr<-`(., "arg_pos", NULL))
  if (length(returning_vals) > 0 &&
      length(unique(add_catchr_prefix(returning_vals))) != length(returning_vals))
    abort("Can't ask for the same default option twice in one call")
  walk(returning_vals, ~if(!(. %in% possible_values))
    abort(paste0("'", returning_vals, "' not a name of a catchr option")))

  returning_vals <- map(returning_vals, ~getOption(add_catchr_prefix(.))) %>%
    set_names(returning_vals)

  # This should be an "atomic" operation, so check to see if names are wrong FIRST
  if (length(akw$kwargs) > 0) {
    bad_kwargs <- names(akw$kwargs) %>%
      keep(~!(. %in% possible_values))
    if (length(bad_kwargs) > 0)
      paste0(bad_kwargs, collapse=", ") %>%
      paste0(" not names of catchr options") %>%
      abort()
    if (length(unique(add_catchr_prefix(akw$kwargs))) != length(akw$kwargs))
      abort("Can't set the same default argument twice!")
  }

  if ("default_plan" %in% names(akw$kwargs) ||
      "catchr.default_plan" %in% names(akw$kwargs)) {
    plan_index <- which(grepl("default_plan", names(akw$kwargs)))
    def_plan <- akw$kwargs[[plan_index]] %>%
      `attr<-`("arg_pos", NULL)
    set_default_plan(!!def_plan)
    akw$kwargs[[plan_index]] <- NULL
  }

  if (length(akw$kwargs) > 0) {
    kwargs <- map(akw$kwargs, eval_tidy) %>%
      map(~`attr<-`(., "arg_pos", NULL)) %>%
      set_names(add_catchr_prefix(akw$kwargs))

    if (!all(map_lgl(kwargs, is_true_or_false)))
      abort("All binary catchr options must be either TRUE or FALSE")

    options(kwargs)
  }

  if (length(returning_vals) > 1) return(returning_vals)
  else if (length(returning_vals) > 0) return(returning_vals[[1]])
  else invisible(NULL)
}

#' @rdname default_catchr_options
#' @export
restore_catchr_defaults <- function(...) {
  akw <- args_and_kwargs(...)

  if (length(akw$kwargs) > 0)
    abort("Can't set arguments in `restore_catchr_defaults`")
  if (length(akw$args) == 0)
    options(catchr_original_default_values)
  else {
    args <-  unnamed_args_to_strings(akw$args) %>%
      map(~`attr<-`(., "arg_pos", NULL)) %>%
      add_catchr_prefix()

    if (!all(args %in% names(catchr_original_default_values)))
      abort("All arguments to `restore_catchr_defaults` must be catchr option names")
    options(catchr_original_default_values[args])
  }
  invisible(NULL)
}

# Default options
catchr_original_default_values <- list(
  # Unless already set, the default plan is to collect and muffle
  catchr.default_plan = list("collect", "muffle"),
  # By default, you'll be warned about catchrs DSL
  catchr.warn_about_terms =TRUE,
  # By default, you wont always return a list when you can collect
  catchr.bare_if_possible =TRUE,
  # By default you don't want to drop stuff
  catchr.drop_empty_conds = FALSE
)

is_true_or_false <- function(x) is_true(x) || is_false(x)

add_catchr_prefix <- function(l) {
  if (!is.null(names(l))) {
    if (!is_named(l))
      abort("All elements must be named")
    l <- names(l)
  }
  ifelse(grepl("^catchr\\.", l), l, paste0("catchr.",l)) %>%
    as.character()
}
