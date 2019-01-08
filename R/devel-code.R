#' To-do:
#'
#' CODE-BASED:
#'  - let code use rlang lambda functions for functions
#'  - figure out how you can avoid overlap between "warning" and "condition"
#'  - make the towarning, etc. remove the call
#'  - make a 'misc' condition thing which gets everything that isn't already being caught
#'  - handle interrupts
#'  - make the order matter
#'
#'  LESS CODE-BASED:
#'  - add examples
#'  - make the spec_terms thing use package defaults somehow
#'  - make a way of stopping warnings from catchr
#'  - standardize terminology
#'  - add a page about all the options
#'  - fill out a page about collecting
#'  - the biggest thing preventing me from removing the rlang/purrr dependencies is the splicing operator, it seems
#'
#'  - make a help page that describes how things are masked:
#'      * The only thing that is masked in evaluation is the non-function versions of the special names.
#'      * Egh, just make a help page and connect it to the warning message


#' @import rlang purrr testthat


special_terms <- c("towarning", "tomessage", "toerror",
                   "display", "beep", "exit", "muffle", "collect",
                   "raise")

default_catchr_plan <- list("collect","muffle")


#' Make a string end with a newline character
#'
#' `give_newline` will append a line return ('\\\n') to the end of a string if
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
#' This function attempts to return the first available \link[base:conditions]{restart} with the string "muffle" in its name. If the condition is an error, it will attempt to find the first restart named "return_error" (used internally in `catchr` to return a `NULL` value). If no such restarts can be found, it returns `NULL`.
#'
#' @param cond A condition
#' @return A restart or `NULL` if none can be found.
#' @export
findFirstMuffleRestart <- function(cond) {
  possibleRestarts <- computeRestarts(cond)
  restartNames <- Map(function(x) x$name, possibleRestarts)
  # If its an error, look for the `return_error` restart
  if (inherits(cond, "error")) {
    if ("return_error" %in% restartNames)
      return(possibleRestarts[which(restartNames=="return_error")][[1]])
    else
      return(NULL)
  } else {
    muffleRestarts <- possibleRestarts[grepl("muffle", restartNames, ignore.case = TRUE)]
    if (length(muffleRestarts) > 0)
      # Picks the first restart with "muffle" in its name (hack-y?)
      return(muffleRestarts[[1]])
    # If it doesn't have a muffling restart and isn't an error (ie its a custom condition),
    #   just continue by returning a null value, but don't stop anything/restart
    else
      return(NULL)
  }
}

#' Make `catchr` plans
#'
#' To-do: add docs
#'
#' @section Input:
#'
#' Input to `make_plans` is very similar to how one makes handlers for \code{\link[base]{conditions}}, \code{\link[base]{conditions}} and `rlang`'s \code{\link[rlang]{with_handlers}}, albeit with some important differences.
#'
#' Like the functions above, the name of each argument determines which type of condition it will catch. Hence, `warnings = fn` will apply the `fn` function to the warnings raised in evaluating `expr`. However, *unnamed* arguments are *also* accepted: the value of any unnamed arguments will be treated as the type of condition to catch, and the way it handles the condition will be set by `default_plan` or `getOption("default.catchr.plan")`.
#'
#'
#' @param \dots Named and unnamed arguments for making plans
#' @param default_plan The default plan. If not supplied, `getOption("default.catchr.plan")` will be used.
#' @export
make_plans <- function(..., default_plan = NULL) {
  akw <- clean_cond_input(..., spec_names = special_terms)
  args <- as_list(give_default(akw$args, default_plan = default_plan)) %>%
    add_back_arg_pos(akw$args)

  kwargs <- append(as_list(akw$kwargs), args) %>%
    order_by_arg_pos()

  kwargs %>% imap(make_handler)
}

# DEAR GOD I DID IT
# This function just applies the catchr_behavior to a single expression

#' Catch conditions
#'
#' To-do: add docs
#'
#' @param expr the expression to be evaluated
#' @param plans the plans from make_plans
#' @rdname catchers
#' @export
catch_expr <- function(expr, plans) {
  .myConditions <- NULL
  baby_env <- child_env(current_env())

  kwargs <- plans %>%
    map(~`environment<-`(., baby_env))

  res <- withRestarts(with_handlers(expr, !!!kwargs),
                      return_error = function() NULL)
  append(list(value = res), .myConditions)

}

#' @rdname catchers
#' @export
make_catch_fn <- function(plans) {
  function(expr) {
    .myConditions <- NULL
    baby_env <- child_env(current_env())

    kwargs <- plans %>%
      map(~`environment<-`(., baby_env))

    res <- withRestarts(with_handlers(expr, !!!kwargs),
                        return_error = function() NULL)
    append(list(value = res), .myConditions)
  }
}
#
# plans <- clean_cond_input(error = exit,
#                         warning = c(collect, muffle),
#                         message = c(collect, towarning),
#                         spec_names = c("exit", "towarning", "display", "muffle", "collect"))
# catch_expr({warning("a"); message("ooo"); message("nsass"); "yay"},
#       plans$args, plans$kwargs)
#
#
# make_catch_fn(plans$args, plans$kwargs)({warning("a"); message("ooo"); message("nsass"); "yay"})
