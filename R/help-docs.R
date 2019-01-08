#' Collecting conditions
#'
#' @description
#'
#' To-do: write docs
#'
#' @name collecting-conditions
NULL


#' The language of catchr
#'
#' @description
#'
#' `catchr` implements a small but helpful "domain-specific language" (DSL) to make building condition-handling functions simpler to read and type, somewhat like . Essentially, `catchr` reserves special 'terms' that mean something different than they do in the rest of R. When given as part of the input for a `catchr` plan, these terms will be substituted for special `catchr` functions when used to catch conditions.
#'
#' These special terms can be inputted as strings (e.g., `warning = list('collect', 'muffle')`) or as unquoted terms (e.g., `warning = c(collect, muffle)`) --- `catchr` converts the unquoted terms to strings internally regardless, but having them unquoted saves keystrokes and can highlight their special meanings for readability.
#'
#' @section Special reserved terms:
#'
#' The following are the special terms and what they are used for:
#'
#' - `tomessage`, `towarning`, `toerror`: these terms will be substituted for functions that will convert captured conditions into a message, warning, or error, respectively, and raise them. The original class of the condition will be lost.
#'
#'  - `beep`: if the \link[beepr:beepr-package]{beepr} package is installed, this will play a sound via \code{\link[beepr:beepr]{beepr::beep}}.
#'
#'  - `display`: the purpose of this term is to immediately display information about the captured condition on the screen without raising additional conditions (as would be done with `tomessage`). Currently, this term just calls \code{\link[utils]{str}} on the condition, but this will probably change in later versions.
#'
#' - `exit`: when encountered, this will exit the evaluation of the expression immediately and by default muffle the captured condition. Any instructions after `exit` in the input will be ignored.
#'
#' - `collect`: this term will store the captured conditions and append them to the output of the evaluated expression. See the \link[=collecting-conditions]{Collecting Conditions} help topic for a full explanation.
#'
#' - `muffle`: this term will be substituted for a function that 'muffles' (i.e., 'suppresses', 'catches', what have you) the captured condition, preventing it from being raised to higher levels. \cr
#' Currently, it searches for and uses the first available \link[base:conditions]{restart} with `"muffle"` in its name (the two typical ones are `"muffleMessage"` and `"muffleWarning"`). If the captured condition is an error, which can't be muffled, it will exit the evaluation and give `NULL` for the returned value.
#'
#' - `raise`: a term that will raise the captured condition "as is". The only *real* use for this term is when you want to use `exit` to stop the evaluation, but to still raise the condition past that as well. The behavior of this raising might be slightly hard to predict for very odd edge-cases (e.g., if a condition were both a warning *and* an error).
#'
#' @section Masking:
#'
#' `catchr` will turn unquoted special terms into functions, but what happens if these unquoted terms are identical to variables previously declared? If `muffle` is the name of a user-defined function, e.g., `muffle <- function(x) print("Wooo!")`, in normal R we would expect `warning = muffle` to make `function(x) print("Wooo!")` the warning handler.
#'
#' *However*, `catchr`'s DSL 'masks' any symbol that matches one of its reserved terms, and when it evaluates these symbols, they are converted into strings. `catchr` for the most part will warn you when this happens.
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
NULL
