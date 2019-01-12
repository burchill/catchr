#' Collect conditions, without halting processes
#'
#' @description
#'
#' One of the most useful aspects of `catchr` is its ability to catch and 'collect' the conditions (e.g., warnings, errors, messages, etc.) raised by an expression without halting/restarting the evaluation of that expression. This can be particularly useful in a number of scenarios:
#'
#'  - If you are trying to catch the warning messages from code that takes a long time to run, where having to restart the whole process from square one would be too costly.
#'
#'  - If you want to collect warnings, messages, and errors from code that is running remotely, where these conditions would not be returned with the rest of the results, such as with the \code{\link[future]{future}} package.
#'
#'  - If you are running lots of code in parallel and want to log all of the conditions within R, such as in a large-scale power simulation, or with packages such \code{\link[purrr:purrr-package]{purrr}}.
#'
#' Using the `collect` term lets you do this. Although the exact details/format of the collection process are still being ironed out across versions, the basic premise will remain the same.  When you use `collect`, the captured condition will be added to a list of other conditions of that same type. When the expression is done being evaluated, `catchr` will return a named list, where "value" is the output of the expression, and the other named elements are sublists with all their collected conditions.
#'
#'
#' @section Advanced interactions with collected conditions:
#'
#'  To-do: add docs
#'
#' @name collecting-conditions
NULL




#' The language of catchr
#'
#' @description
#'
#' `catchr` implements a small but helpful "domain-specific language" (DSL) to make building condition-handling functions simpler to read and type, somewhat like . Essentially, `catchr` reserves special 'terms' that mean something different than they do in the rest of R. When given as part of the input for a `catchr` plan, these terms will be substituted for special `catchr` functions used to handle conditions.
#'
#' These special terms can be inputted as strings (e.g., `warning = list('collect', 'muffle')`) or as unquoted terms (e.g., `warning = c(collect, muffle)`); `catchr` internally converts the unquoted terms to strings regardless, but being able to input them unquoted saves keystrokes and can highlight their special meanings for code readability.
#'
#' @section Special reserved terms:
#'
#' The following are the special terms and what they do:
#'
#' - `tomessage`, `towarning`, `toerror`: these terms will become functions that will convert captured conditions into a message, warning, or error, respectively, and raise them. The original classes of the condition will be lost.
#'
#'  - `beep`: if the \link[beepr:beepr-package]{beepr} package is installed, this will play a sound via \code{\link[beepr:beepr]{beepr::beep}}.
#'
#'  - `display`: the purpose of this term is to immediately display information about the captured condition on the screen without raising additional conditions (as would be done with `tomessage`). Currently, this term just calls \code{\link[utils]{str}} on the condition, **but this will probably change in later versions.**
#'
#' - `muffle`: this term will be substituted for a function that 'muffles' (i.e., 'suppresses', 'catches', 'hides'---whatever you want to call it) the captured condition, preventing it from being raised to higher levels or subsequent plans. \cr
#' Currently, it searches for and uses the first available \link[base:conditions]{restart} with `"muffle"` in its name (the two typical ones are `"muffleMessage"` and `"muffleWarning"`). If the captured condition is an error, which can't be muffled, it will exit the evaluation and give `NULL` for the returned value of the evaluated expression.
#'
#' - `exit`: when encountered, this will exit the evaluation of the expression immediately and by default muffle the captured condition. Any instructions after `exit` in the input will be ignored.
#'
#' - `collect`: this term will store the captured conditions and append them to the output of the evaluated expression. See the \link[=collecting-conditions]{Collecting conditions} help topic for a full explanation.
#'
#' - `raise`: this term will raise the captured condition "as is". The only *real* use for this term is when you want to use `exit` to stop the evaluation, but to still raise the condition past that as well (in which case, put `raise` in the plan before `exit`). The behavior of this raising might be slightly unpredictable for very odd edge-cases (e.g., if a condition were both a warning *and* an error).
#'
#' @section Masking:
#'
#' `catchr` will turn unquoted special terms into functions, but what happens if these unquoted terms are identical to variables previously declared?
#'
#' If `muffle` is the name of a user-defined function, e.g., `muffle <- function(x) print("Wooo!")`, in normal R we would expect `warning = muffle` to make `function(x) print("Wooo!")` the warning handler.
#'
#' *However*, `catchr`'s DSL "masks" any symbol that matches one of its reserved terms, and when it evaluates these symbols, they are converted into strings. For the most part, `catchr` will warn you when this happens.
#'
#' **Importantly**, `catchr` does *not* mask reserved terms when:
#'
#'  - the reserved names are being used as calls, e.g., `warning = collect(foo)`. In these cases, it will attempt to use a previously defined function `collect` on `foo`, and will attempt to use whatever that evaluates to. The reserved terms are all strings/unquoted bare symbols, so it is never a problem anyway.
#'
#'  - when the input specifically references a namespace/package, such as `warning = dplyr::collect`. When the symbol of a special terms is preceded by `::` or `:::`, it will be seen as the function of that package, and not as the special term `collect`.
#'
#'  - the reserved terms are used inside a function definition. For example, if the user had defined `muffle <- function(x) print("not special")`, and `fn <- function(x) muffle`, using the argument `warning = fn()` would not use the special term of `muffle`.
#'
#' @name catchr_DSL
#' @aliases language_of_catchr reserved_terms
NULL
