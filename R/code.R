findFirstMuffleRestart <- function(cond) {
  possibleRestarts <- computeRestarts(cond)
  muffleRestarts <- possibleRestarts[grepl("muffle", Map(function(x) x$name, possibleRestarts))]
  if (length(muffleRestarts) > 0)
    # Picks the first restart with "muffle" in its name (hack-y?)
    invokeRestart(muffleRestarts[[1]])
  # If it doesn't have a muffling restart and isn't an error (ie its a custom condition),
  #   just continue by returning a null value, but don't stop anything/restart
  else
    return(NULL)
}

#' Collect conditions, without halting processes
#'
#' `collect_conditions` (and its shorter alias, `col_cond`) evaluates an expression, and will catch and collect all the conditions
#' specified (e.g., warnings, errors, messages, etc.), without halting/restarting
#' the evaluation of the expression (unless it encounters an error).
#' It will then return a named list with the result of the expression and sublists
#' that contain all the collected conditions (see **Value**).
#'
#' The way this function suppresses conditions is simply by attempting to find the
#' first available \code{\link{restartDescription}} with "muffle" in its name
#' and invoke that. This works well for most warnings and messages, which are
#' muffled by default with "muffleWarning" and "muffleMessage". Barring any
#' restarts with this property, it will simply return `NULL` as a restart (i.e.,
#' most likely the case for custom miscellaneous conditions). If you have some
#' really funky conditions being signalled that somehow 1) don't have
#' "muffle<Condition>" as a restart or 2) have a custom restart with "muffle"
#' in the name that takes precedence, this might lead to unexpected results,
#' and you probably shouldn't use this function. \cr \cr
#' For a more descriptive account of the object returned, see **Value**. If no warnings or message are raised in the expression, their named sublists will still be present, but empty. \cr \cr
#' Additionally, if the expression encounters an error, the value of the "value" sublist will be returned as `NULL`.
#'
#' @param expr The expression to be evaluated.
#' @param catchErrors A logical which, if TRUE, will catch any error, prevent it from being raised, and return it in the named list. The process will still halt when an error occurs however.
#' @param asStrings A logical which, if TRUE, will convert the captured conditions into strings of their messages, instead of keeping them as S3 condition objects.
#' @param dropMiscIfEmpty A logical which, if TRUE, will drop the "conditions" sublist from the returned list if there were no miscellaneous conditions signalled (ie 99\% of the time).
#' @return A named list with the following structure: \cr
#' \itemize{
#'   \item `value`: The output of the expression, if it did not encounter an error.
#'   \item `warnings`: A list of the warnings raised in evaluating the expression.
#'   \item `messages`: A list of the warnings raised in evaluating the expression.
#'   \item `errors`: A list of errors raised in evaluating the expression. If `catchErrors==FALSE`, then this will not be returned.
#'   \item `conditions`: A list of miscellaneous conditions (defined as conditions that do not inherit from warnings, errors, or messages) raised in evaluating the expression. If `dropMiscIfEmpty==TRUE`, this sublist will not be included unless these rare conditions are signalled.
#'   }
#' @examples
#' res <- collect_conditions({
#'   warning("warning 1")
#'   message("message 1")
#'   warning("warning 2")
#'   signal_custom_condition("Custom condition #1!")
#'   signal_custom_condition("Custom condition #2!")
#'   stop("error 1")
#'   message("message 2 (not signalled)")
#'   "test value"
#' },
#' asStrings = FALSE, catchErrors = TRUE, dropMiscIfEmpty = TRUE)
#' print(res)
#'
#' # Without the `dropMiscIfEmpty=TRUE` and no miscellaneous conditions being raised,
#' #   there won't be any `res$conditions`
#' res <- collect_conditions({
#'   warning("warning 1")
#'   message("message 1")
#'   warning("warning 2")
#'   stop("error 1")
#'   message("message 2 (not signalled)")
#'   "test value"
#' },
#' asStrings = TRUE, catchErrors = TRUE, dropMiscIfEmpty = FALSE)
#' print(res)
#' @rdname col_cond_page
#' @export
collect_conditions <- function(expr,
                               catchErrors = TRUE,
                               asStrings = FALSE,
                               dropMiscIfEmpty = TRUE
) {

  if (asStrings == TRUE) convert <- function(x) Map(as.character, x)
  else convert <- identity

  myConditions <- NULL
  # sublists to drop
  dropList <- list()
  if (catchErrors == FALSE) dropList <- c(dropList, "errors")

  conditionHandler <- function(cond) {
    myConditions <<- c(myConditions, list(cond))
    # If its an error, we want to stop there
    if (inherits(cond, "error"))
      invokeRestart("return_NULL")
    # Otherwise, try to get the first restart with "muffle" in its name
    #   (i.e., "muffleMessage" or "muffleWarning" for messages and warnings)
    findFirstMuffleRestart(cond)
  }
  val <- withCallingHandlers(
    withRestarts(expr, return_NULL = function() NULL),
    condition = conditionHandler)

  # If you don't want to catch errors, go ahead and signal them again
  #   (ie raise them)
  if (catchErrors == FALSE) {
    Map(stop, Filter(function(x) inherits(x, "error"), myConditions))
    # And if there aren't any errors (ie we made it this far,
    #   get rid of the named element)
    dropList <- c(dropList, "errors")
  }

  res <- list(
    value = val,
    warnings = convert(Filter(function(x) inherits(x, "warning"), myConditions)),
    messages = convert(Filter(function(x) inherits(x, "message"), myConditions)),
    errors =   convert(Filter(function(x) inherits(x, "error"), myConditions)))
  res$conditions <- convert(Filter(function(x) !(inherits(x, "warning") ||
                                                   inherits(x, "message") ||
                                                   inherits(x, "error")),
                                   myConditions))
  # If you want to drop the miscellaneous messages when there aren't any, add that to the list
  if (dropMiscIfEmpty == TRUE && length(res$conditions) == 0)
    dropList <- c(dropList, "conditions")
  # Drop the elements you don't want
  res[!(names(res) %in% dropList)]
}
#' @rdname col_cond_page
#' @export
col_cond <- collect_conditions




#' Signal custom conditions
#'
#' If you want to signal your own custom conditions (i.e., not messages or warnings),
#' you can do so with this function. However, most of the time (other than testing code),
#' you should probably stick to warnings and errors.
#' @param msg The message you want your condition to have.
#' @param type Your name for the 'type' of condition you want to raise.
#' @export
signal_custom_condition <- function(msg, type="custom") {
  signalCondition(
    structure(class = c(type, "condition"),
              list(message=msg, call=NULL)))
}



#' Raise and display collected conditions
#'
#' There are times where you might want to raise the conditions that you have collected via \code{\link{collect_conditions}}, for example, after your code has processed them, etc. These functions help with that
#'
#' `raise_conditions` will raise all the conditions from the named list returned from \code{\link{collect_conditions}}, and display them in a pretty, readable way at the same time, returning the input invisibly. You have the option to raise errors or not. \cr \cr
#' `evaluate_results` will strip away the condition sublists of a list returned from \code{\link{collect_conditions}}, and optionally raise errors if there are any. \cr \cr
#' `raise_col_warnings`, `raise_col_messages`, and `raise_col_conditions` will raise and display lists of conditions, while `print_errors` will display errors rather than raise them.
#'
#' @param col_conds A named list of the kind returned from \code{\link{collect_conditions}}.
#' @param display_errors A logical, which, if `TRUE`, will display the errors to `stderr` just like the messages and warnings.
#' @param raise_errors A logical, which, if `TRUE`, will raise the collected errors. Otherwise, errors will not be signalled.
#' @param added_text A string that will be appended immediately after the title of each type of condition being raised. E.g., a value of " in x\[1\]:" would print "Messages in x\[1\]:" followed by a newline for the messages.
#' @param conditions A list of conditions (i.e., a sublist from what is returned from \code{\link{collect_conditions}}).
#' @param toStdErr A logical, which, if `TRUE`, prints the results to `stderr()` rather than `stdout()`.
#' @examples
#' res <- collect_conditions({
#'   warning("warning 1")
#'   message("message 1")
#'   warning("warning 2")
#'   signal_custom_condition("Custom condition #1!")
#'   signal_custom_condition("Custom condition #2!")
#'   stop("error 1")
#'   message("message 2 (not signalled)")
#'   "test value"
#' }, catchErrors = TRUE)
#' raise_conditions(res, raise_errors = FALSE)
#'
#' # A demonstration of how it can be used with packages like `purrr`
#' \dontrun{
#'   # Let's pretend you got `l` from doing a purrr::map on some data
#'   l <- list(
#'     collect_conditions({warning("BAD!"); warning("Uh oh!"); 1}),
#'     collect_conditions({message("This is fine"); warning("This is bad!"); 2}),
#'     collect_conditions({message("Have a nice day!"); stop("DIE!"); 3},
#'     catchErrors=TRUE)
#'   )
#'   results <- purrr::imap(l, function(e, i)
#'     raise_conditions(e, raise_errors=FALSE,
#'                       added_text=paste0(" in x[",i,"]:"))) %>%
#'     evaluate_results()
#' }
#' @rdname raising_conds
#' @seealso \code{\link{collect_conditions}}, \code{\link{warn_now}}
#' @export
raise_conditions <- function(col_conds,
                             display_errors = TRUE,
                             raise_errors = TRUE,
                             added_text = ":") {
  if (length(col_conds$messages) > 0)
    raise_col_messages(col_conds$messages, added_text)

  if (length(col_conds$warnings) > 0)
    raise_col_warnings(col_conds$warnings, added_text)

  if (length(col_conds$conditions) > 0)
    raise_col_conditions(col_conds$conditions, added_text)

  if (length(col_conds$errors) > 0 && display_errors == TRUE)
    print_errors(col_conds$errors, added_text)

  if (length(col_conds$errors) > 0 && raise_errors == TRUE)
    Map(stop, col_conds$errors)

  invisible(col_conds)
}

#' @rdname raising_conds
#' @export
evaluate_results <- function (col_conds, raise_errors = TRUE) {
  if (raise_errors == TRUE && length(col_conds$errors) > 0)
    Map(stop, col_conds$errors)
  col_conds$value
}

#' @rdname raising_conds
#' @export
raise_col_warnings <- function(conditions, added_text=":") {
  cat("Warnings", added_text, "\n", file=stderr(), sep="")
  Map(warn_now, conditions)
  invisible()
}
#' @rdname raising_conds
#' @export
raise_col_messages <- function(conditions, added_text=":") {
  cat("Messages", added_text, "\n", file=stderr(), sep="")
  Map(addLF_message, conditions)
  invisible()
}
#' @rdname raising_conds
#' @export
raise_col_conditions <- function(conditions, added_text=":",
                                 toStdErr = TRUE) {
  if (toStdErr == T)
    printer_file = stderr()
  else
    printer_file = stdout()
  cat("Misc. conditions", added_text, "\n", file=printer_file, sep="")
  condition_string <- Reduce(
    function(s, cond) {paste0(s, trimws(cond$message, which="right"), "\n")},
    conditions, "")
  cat(condition_string, file=printer_file)
  Map(signalCondition, conditions)
  invisible()
}
#' @rdname raising_conds
#' @export
print_errors <- function(conditions, added_text=":") {
  cat("Errors", added_text, "\n", file=stderr(), sep="")
  Map(function(e) cat(e$message, "\n", file=stderr(), sep=""),
      conditions)
  invisible()
}



#------------------ Warning/printing functions ---------------------------

#' Custom functions to display signals (experimental)
#'
#' `R` has "issues" signalling pre-made message and warning conditions: when you pass
#' these objects to \code{\link{warning}} or \code{\link{message}}, they
#' ignore all the additional arguments, so you can't use, say, `immediate. = TRUE` or
#' `appendLF = TRUE`. These two functions take care of those problems, respectively. These
#' functions are probably best let be and are more "experimental" in many senses,
#' but they have been exported in case the user wants to make code similar to the other
#' functions in this package. \cr \cr
#' `warn_now`: The function \code{\link{warning}} lets you immediately output warnings with
#' `immediate.=TRUE`, but not if the input is already a condition
#' object (weird, I know). This function is a modified Frankenstein mashup
#' of `warning` and `message` that will immediately output warnings regardless
#' of the form of the input (as well as appending a newline character). The downside is that no other additional arguments can be used, but I can't
#' seem to do anything about that at the moment. This is used primarily for
#' \code{\link{raise_conditions}} and \code{\link{raise_col_warnings}}. \cr \cr
#' `addLF_message`: The function \code{\link{message}} lets you automatically append a newline to
#' a message that you signal, regardless of its form. It suffers from the same argument
#' restrictions as `warn_now`.
#'
#' @param \dots An S3 condition object or whatever values one wants to pass into the original `warning` or `message` functions
#' @rdname print_helpers
#' @export
warn_now <- function (...) {
  args <- list(...)
  if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    cond <- args[[1L]]
    if (nargs() > 1L)
      cat(gettext("additional arguments ignored in warning()"),
          "\n", sep = "", file = stderr())

    # What I added from `message`
    defaultHandler <- function(c) {
      cat(paste0(trimws(conditionMessage(c), "right"), "\n"), file = stderr(), sep = "")
    }
    withRestarts({
      signalCondition(cond)
      # and here
      defaultHandler(cond)
      # this is what I cut out
      # .Internal(.dfltWarn(message, call))
    }, muffleWarning = function() NULL)
    invisible(message)
  }
  else
    warning(..., immediate. = TRUE)
}
#' @rdname print_helpers
#' @export
addLF_message <- function(...) {
  args <- list(...)
  if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    if (nargs() > 1L)
      warning("additional arguments ignored in message()")
    cond <- args[[1L]]
    cond$message <- paste0(
      trimws(cond$message, which="right"),
      "\n")
    message(cond)
  } else {
    message(..., appendLF = TRUE)
  }
}

