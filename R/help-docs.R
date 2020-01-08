#' Collect conditions, without halting processes
#'
#' @description
#'
#' One of the most useful aspects of catchr is its ability to catch and 'collect' the conditions (e.g., warnings, errors, messages, etc.) raised by an expression without halting/redoing the evaluation of that expression. This can be particularly useful in a number of scenarios:
#'
#'  - If you are trying to catch the warning messages from code that takes a long time to run, where having to restart the whole process from square one would be too costly.
#'
#'  - If you want to collect warnings, messages, and errors from code that is running remotely, where these conditions would not be returned with the rest of the results, such as with the \pkg{future} package.
#'
#'  - If you are running lots of code in parallel and want to log all of the conditions within R, such as in a large-scale power simulation, or with packages such \code{\link[purrr:purrr-package]{purrr}}.
#'
#' Using the `collect` term lets you do this. When the plan for a condition uses `collect`, the captured condition will be added to a list of other conditions of that same type. When the expression is done being evaluated, catchr will return a named list, where `$value` is the output of the expression, and the other named elements are sublists with all their collected conditions. The exact behavior of this process is determined by options in [catchr_opts()].
#'
#' @examples
#' one_of_each <- function(with_error) {
#'   rlang::inform("This is a message")
#'   rlang::warn("This is a warning")
#'   if (with_error)
#'     stop("This is an error", call.=FALSE)
#'   "return value!"
#' }
#'
#' collecting_plans <- make_plans(message, warning, error,
#'                                .opts = catchr_opts(default_plan = c(collect, muffle),
#'                                                    drop_empty_conds = FALSE))
#'
#' # When the evaluation completes, the "value" element is the value the expression returns
#' no_error <- catch_expr(one_of_each(FALSE), collecting_plans)
#' no_error$value
#'
#' # If it doesn't return, the value is generally NULL
#' with_error <- catch_expr(one_of_each(TRUE), collecting_plans)
#' with_error$value
#'
#' # If the option `drop_empty_conds` == TRUE, then
#' #   sublists without collected condition will be dropped
#' catch_expr(one_of_each(FALSE), collecting_plans,
#'            .opts = catchr_opts(drop_empty_conds=TRUE))
#'
#' # If the option `bare_if_possible` == TRUE, then even
#' #   functions that don't use `collect` will return the value
#' #   of the expression as a "value" sublist
#' catch_expr("DONE", fake_cond = muffle, .opts = catchr_opts(bare_if_possible=FALSE))
#' @name collecting-conditions
#' @aliases collect
#' @seealso [dispense_collected()] to raise the collected conditions and return the bare result
NULL




#' The language of catchr
#'
#' @description
#'
#' catchr implements a small but helpful "domain-specific language" (DSL) to make building condition-handling functions simpler to read and type. Essentially, catchr reserves special 'terms' that mean something different than they do in the rest of R. When given as part of the input for a catchr plan, these terms will be substituted for special catchr functions used to handle conditions.
#'
#' These special terms can be inputted as strings (e.g., `warning = list('collect', 'muffle')`) or as unquoted terms (e.g., `warning = c(collect, muffle)`); catchr internally converts the unquoted terms to strings regardless, but being able to input them unquoted saves keystrokes and can highlight their special meanings for code readability.
#'
#' @section Special reserved terms:
#'
#' The following are the special terms and what they do. Note that there are also some \link[=reserved-conditions]{special condition names}, but those are different from the following.
#'
#' - **`tomessage`**, **`towarning`**, **`toerror`**: these terms will become functions that will convert captured conditions into a message, warning, or error, respectively, and raise them. The original classes of the condition will be lost.
#'
#'  - **`beep`**: if the \link[beepr:beepr-package]{beepr} package is installed, this will play a sound via \code{\link[beepr:beepr]{beepr::beep}}.
#'
#'  - **`display`**: the purpose of this term is to immediately display information about the captured condition on the output terminal without raising additional conditions (as would be done with `tomessage`). Currently, it attempts to display this information with bold, turquoise-blue text if the [crayon][crayon::crayon] package is installed. In future versions of catchr, this default styling (and other display options) may be able to be changed by the user.
#'
#' - **`muffle`**: this term will be substituted for a function that 'muffles' (i.e., 'suppresses', 'catches', 'hides'---whatever you want to call it) the captured condition, preventing it from being raised to higher levels or subsequent plans. Anything in a plan _after_ `muffle` will be ignored, so put it last. \cr
#' The function `muffle` is built on, [first_muffle_restart()], searches for the first available \link[base:conditions]{restart} with `"muffle"` in its name (the two typical ones are `"muffleMessage"` and `"muffleWarning"`) and calls `invokeRestart` with it. If the captured condition is an error, which can't be muffled, it will exit the evaluation and give `NULL` for the returned value of the evaluated expression.
#'
#' - **`exit`**: when encountered, this will exit the evaluation of the expression immediately and by default muffle the captured condition (use `raise` in the plan if to ensure this doesn't happen). Any instructions after `exit` in the input will be ignored, so put it last.
#'
#' - **`collect`**: this term will store the captured conditions and append them to the output of the evaluated expression. See the \link[=collecting-conditions]{collecting conditions} help topic for a full explanation.
#'
#' - **`raise`**: this term will raise the captured condition "as is". The only *real* use for this term is when you want to use `exit` to stop the evaluation, but to still raise the condition past that as well (in which case, put `raise` in the plan before `exit`). The behavior of this raising might be slightly unpredictable for very odd edge-cases (e.g., if a condition were both a warning *and* an error).
#'
#' @section Masking:
#'
#' catchr will turn unquoted special terms into functions, but what happens if these unquoted terms are identical to variables previously declared?
#'
#' If `muffle` is the name of a user-defined function, e.g., `muffle <- function(x) print("Wooo!")`, in normal R we would expect `warning = muffle` to make `function(x) print("Wooo!")` the warning handler.
#'
#' *However*, catchr's DSL "masks" any symbol that matches one of its reserved terms, and when it evaluates these symbols, they are converted into strings. For the most part, catchr will warn you when this happens.
#'
#' **Importantly**, catchr does *not* mask reserved terms when:
#'
#'  - the reserved names are being used as calls, e.g., `warning = collect(foo)`. In these cases, it will attempt to use a previously defined function `collect` on `foo`, and will attempt to use whatever that evaluates to. The reserved terms are all strings/unquoted bare symbols, so it is never a problem anyway.
#'
#'  - the input specifically references a namespace/package, such as `warning = dplyr::collect`. When the symbol of a special terms is preceded by `::` or `:::`, it will be seen as the function of that package, and not as the special term `collect`.
#'
#'  - the reserved terms are used inside a previously defined function. For example, if the user had defined `muffle <- function(x) print("not special")`, and `fn <- function(x) muffle`, using the argument `warning = fn()` would not use the special term of `muffle`.
#'
#' @name catchr-DSL
#' @aliases language-of-catchr reserved-terms masking tomessage towarning toerror beep display muffle exit raise
NULL


#' Special condition names
#'
#' @description
#'
#' In addition to having \link[=reserved-terms]{reserved terms} for use in making condition-handling plans, catchr also places special meaning on two types of conditions, `misc` and `last_stop`. The `misc` is very useful, `last_stop` is something most users should probably avoid.
#'
#'
#' @section The `misc` condition:
#'
#' The names of the named arguments passed to `make_plans` correspond to the type of conditions each plan is designed for---specifically, if any of a condition's \link[base:class]{classes} match a plan's name, it will be caught by that plan. By default, all conditions have a class of "condition".
#'
#' There is nothing special about a condition with a class of `"misc"` in base R, although there are no normal base R functions that would automatically raise such a condition. However, in catchr, using the name `misc` for a plan means that this plan will be applied to any condition that does _not_ already have a plan specified for it. Consider the following example:
#'
#'  `plans <- make_plans(warning = collect, message = collect, error = exit, misc = collect)`
#'
#' These plans will collect every non-error condition into three sublists, one for warnings, one for messages, and one for everything else---"misc". If one used `condition = collect` instead of `misc`, warnings and messages would be collected twice: once in each of their respective sublists, and another time in "condition", since each type also has that class. `misc` will _not_ catch warnings or messages in the scenario above.
#'
#' Since ~99\% of all conditions encountered in the wild will be errors, warnings, and messages, `misc` is just a short, handy way of making sure you catch anything more "exotic". If you're dealing with conditions that have `"misc"` as a _class_, you're probably at an advanced enough stage where you shouldn't be using catchr. But if you are in this circumstance and feel strongly otherwise, feel free to make a feature request on the [GitHub repo](https://github.com/burchill/catchr).
#'
#' @section The `last_stop` condition:
#'
#' This condition name is reserved for [exit], [user_exit()] and [exit_with()]. There is basically zero chance any code other than catchr will ever raise a condition of `"last_stop"`, so this shouldn't be a problem, but until catchr becomes more mature, do not use this name for any condition or plan.
#'
#' @name reserved-conditions
#' @aliases misc last_stop
NULL
