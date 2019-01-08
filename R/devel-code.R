#' To-do:
#'
#' PRIORITY:
#'  - make it so that handlers that catch an error (both 'error', 'condition', and the general thing) can 'muffle' the error by returning NULL.
#'      Currently, error catching doesn't seem to be able to work with `calling` functions in `with_handlers`. Maybe use `with_restarts`?
#'
#' CODE-BASED:
#'  - make it so that the default catchr plan can work even if you only use a single function of the package
#'  - let code use rlang lambda functions for functions
#'  - figure out how you can avoid overlap between "warning" and "condition"
#'  - determine if the findFirstRestart is best
#'  - make the towarning, etc. remove the call
#'  - make a 'misc' condition thing which gets everything that isn't already being caught
#'  - handle interrupts
#'  - make the order matter
#'
#'  LESS CODE-BASED:
#'  - make the spec_terms thing use package defaults somehow
#'  - make a way of removing warnings
#'  - make a help page for the special reserved terms
#'  - standardize terminology
#'  - add a page about all the options
#'  - add a page about collecting
#'  - the biggest thing preventing me from removing the rlang/purrr dependencies is the splicing operator, it seems
#'
#'  - make a help page that describes how things are masked:
#'      * The only thing that is masked in evaluation is the non-function versions of the special names.
#'      * Egh, just make a help page and connect it to the warning message



#' @import rlang
#' @import purrr
#' @import testthat

special_terms <- c("towarning", "tomessage", "toerror",
                   "display", "beep", "exit", "muffle", "collect",
                   "raise")


#' Find the first 'mufflable' restart
#'
#' This function attempts to return the first available \link[base:conditions]{restart} with the string "muffle" in its name. If the condition is an error, it will attempt to find the first restart named "return_error" (used internally in `catchr` to return a `NULL` value). If no such restarts can be found, it returns `NULL`
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


#' The language of `catchr`
#'
#' @description
#'
#' `catchr` implements a small but helpful "domain-specific language" (DSL) to make building condition-handling functions simpler to read and type, somewhat like \href{https://tidyeval.tidyverse.org/}{tidyeval}. Essentially, `catchr` reserves special 'terms' that mean something different than they do in the rest of R. When given as part of the input for a `catchr` plan, these terms will be substituted for special `catchr` functions when used to catch conditions.
#'
#' These special terms can be inputted as strings (e.g., `warning = list('collect', 'muffle')`) or as unquoted terms (e.g., `warning = c(collect, muffle)`)--`catchr` converts the unquoted terms to strings internally regardless, but having them unquoted saves keystrokes and can highlight their special meanings for readability.
#'
#' @section Special reserved terms
#'
#' The following are the special terms and what they are used for:
#'
#' - `tomessage`, `towarning`, `toerror`: these terms will be substituted for functions that will convert captured conditions into a message, warning, or error, respectively, and raise them. The original class of the condition will be lost.
#'
#'  - `beep`: if the \link[beepr:`beepr-package`]{`beepr`} package is installed, this will play a sound via \link[beepr:beepr]{`beepr::beep`}.
#'
#'  - `display`: the purpose of this term is to immediately display information about the captured condition on the screen without raising additional conditions (as would be done with `tomessage`). Currently, this term just calls \link[utils]{str} on the condition, but this will probably change in later versions.
#'
#' - `exit`: when encountered, this will exit the evaluation of the expression immediately and by default muffle the captured condition. Any instructions after `exit` in the input will be ignored.
#'
#' - `collect`: this term will store the captured conditions and append them to the output of the evaluated expression. See the \link[collecting-conditions]{Collecting Conditions} help topic for a full explanation.
#'
#' - `muffle`: this term will be substituted for a function that 'muffles' (i.e., 'suppresses', 'catches', what have you) the captured condition, preventing it from being raised to higher levels. \cr
#' Currently, it searches for and uses the first available \link[base:conditions]{restart} with `"muffle"` in its name (the two typical ones are `"muffleMessage"` and `"muffleWarning"`). If the captured condition is an error, which can't be muffled, it will exit the evaluation and give `NULL` for the returned value.
#'
#' - `raise`: a term that will raise the captured condition "as is". The only *real* use for this term is when you want to use `exit` to stop the evaluation, but to still raise the condition past that as well. The behavior of this raising might be slightly hard to predict for very odd edge-cases (e.g., if a condition were both a warning *and* an error).
#'
#' @section Masking
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
#'  - the reserved terms are used inside a function definition. For example, if the user had defined `muffle <- function(x) print("not special")`, and `fn <- function(x) muffle`, `warning = fn()` would not use the special term of `muffle`.
#'
#' @name catchr_DSL
NULL

# display, exit, muffle, collect, beep, #also: towarn, toerror, tomessage


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

#' Separate \dots into Python-esque \code{*args} and \code{**kwargs}
#'
#' This function will return a named list with two sublists, 'args' and 'kwargs', which contain the unnamed and named arguments as quosures. \cr
#' This is useful for when you want these two types of arguments to behave differently. The quosures will also have the attribute `'arg_pos'`, which will indicate their position in the original order in which they were supplied.
#'
#' @param \dots Any mix of named and unnamed arguments
#' @param .already_quosure if the arguments are already all quosures (in which case it will just sort them by named vs. unnamed arguments)
#' @return A named list of lists, with `$args` being a list of quosures of the unnamed arguments and `$kwargs` being a list of quosures of the named arguments.
#' @examples
#'
#' x <- args_and_kwargs(unnamed_1, named_1="ba", "unnamed_2", named_2 = letters)
#' print(x$args)
#' print(x$kwargs)
#'
#' \dontrun{
#' # Or see the `share_scales` from the `zplyr` package
#' share_scales <- function(...) {
#'   akw <- args_and_kwargs(...)
#'   # Unnamed arguments are ggplot scales
#'   geom_func_list <- purrr::map(akw$args, rlang::eval_tidy)
#'   # Named arguments are to be passed into those scales
#'   geoms <- purrr::map(geom_func_list, ~.(!!!akw$kwargs))
#'   return(geoms)
#' }
#' }
#' @export
args_and_kwargs <- function(..., .already_quosure = FALSE) {
  if (.already_quosure == TRUE) qs <- list(...)
  else qs <- rlang::enquos(...)
  qs <- map2(qs, seq_along(qs),
             ~`attr<-`(.x, "arg_pos", .y))

  l <- list(args =   qs[rlang::names2(qs) == ""],
            kwargs = qs[rlang::names2(qs) != ""])
  return(l)
}


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


#' Make sure a function can be a handler
#'
#' This makes sure that a given function doesn't require more than one argument passed into it, and has at least one argument (which is what a \link[base:conditions]{handler} needs).
#'
#' @param fn A function
#' @export
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
  if (length(arg) > 1 || is_list(arg)) {
    if (!is_list(arg) && !is_bare_character(arg))
      abort(paste0("`", arg, "` has an invalid type: ", typeof(arg)), val=arg)
    walk(arg, ~classify_el(., nono_words))
  } else
    classify_el(arg, nono_words)
  invisible(arg)
}

# adds back in the argument positions as attribut
add_back_arg_pos <- function(new_l, old_l) {
  map2(new_l, old_l, function(k, old) {
    `attr<-`(k, 'arg_pos', attr(old, 'arg_pos')) })
}

# uses the 'arg_pos' attribute to order a list
order_by_arg_pos <- function(l) {
  l[order(map_dbl(l, ~attr(., "arg_pos")))]
}

#' Make `catchr` plans
#'
#' @section Input
#'
#' Input to `make_plans` is very similar to how one makes handlers for \code{\link[base]{withCallingHandlers}}, \code{\link[base]{tryCatch}} and `rlang`'s \code{\link[rlang]{with_handlers}}, albeit with some important differences.
#'
#' Like the functions above, the name of each argument determines which type of condition it will catch. Hence, `warnings = fn` will apply the `fn` function to the warnings raised in evaluating `expr`. However, *unnamed* arguments are *also* accepted: the value of any unnamed arguments will be treated as the type of condition to catch, and the way it handles the condition will be set by `default_plan` or `getOption("default.catchr.plan")`.
#'
#'
#' @param \dots Named and unnamed arguments for making plans
#' @export
make_plans <- function(..., default_plan = NULL) {
  akw <- clean_cond_input(..., spec_names = special_terms)
  args <- as_list(flesh_args_out(akw$args, default_plan = default_plan)) %>%
    add_back_arg_pos(akw$args)

  kwargs <- append(as_list(akw$kwargs), args) %>%
    order_by_arg_pos()

  kwargs %>% imap(make_handler)
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

# Not sure if this is that good!!!! Might run into environment issues
# Combines functions and expressions into a single big function
combine_functions <- function(...) {
  l <- enexprs(...) %>%
    map(function(el) {
      if (is_function(el))
        substitute(zzzz(cond), c(zzzz = el))
      else
        substitute(zzzz, c(zzzz = el))
    })
  e <- expr({!!!l})
  e <- expr(function(cond) !!e)
  eval_tidy(e)
}



# sub in special term functions
use_special_terms <- function(s, cond_type) {
  switch(
    s,
    towarning = function(cond) {
      class(cond) <- c("warning","condition")
      warning(cond)
    },
    tomessage = function(cond) {
      class(cond) <- c("message","condition")
      if (!is.null(cond$message) & cond$message != "")
        cond$message <- give_newline(cond$message, trim = F)
      message(cond)
    },
    toerror = function(cond) {
      class(cond) <- c("error","condition")
      stop(cond)
    },
    raise = function(cond) {
      cnd_signal(cond)
    },
    beep = function(cond) {
      if (!is_installed("beepr"))
        abort("Package `beepr` needs to be installed if `beep` is to be used.")
      else
        beepr::beep()
    },
    display = function(cond) {
      str(cond, max.level = 1)
    },
    muffle = expr({
      restart = findFirstMuffleRestart(cond)
      if (!is.null(restart))
        on.exit(invokeRestart(restart), add = TRUE)
      NULL
    }),
    collect = substitute({
      .myConditions[[cond_type]] <<- append(.myConditions[[cond_type]], list(cond))
      NULL
    }, list(cond_type=cond_type)),
    stop(paste0("`", s, "` is not a possible choice"))
  )
}

# Makes a handler from a kwarg
make_handler <- function(vals, name) {
  if (!is_vector(vals))
    vals <- list(vals)
  first_exit <- purrr::detect_index(vals, ~is_string(.) && . =="exit")
  exit_bool = F
  if (first_exit > 0) {
    exit_bool = T
    if (first_exit < length(vals))
      warning(paste0("'", name, "' set to exit before ",
                     length(vals)-first_exit,
                     " other defined functions"))
    vals <- vals[1:(first_exit-1)]
    if (first_exit == 1)
      vals <- list(function(x) { NULL })
  }
  vals <- map(vals, function(x) {
    if (is_callable(x)) x
    else use_special_terms(x, name)
  })
  combined_func <- combine_functions(!!!vals)

  if (exit_bool) exiting(combined_func)
  else calling(combined_func)
}

# Turns the unnamed arguments into the defaults
flesh_args_out <- function(args, default_plan = NULL) {
  if (is.null(default_plan))
    default_plan = getOption("default.catchr.plan", default_catchr_plan)
  map(args, ~default_plan) %>%
    set_names(args)
}


# Ones to use: display, collect, beep, to<blank>, muffle
# display, exit, muffle, collect, beep, #also: towarn, toerror, tomessage

# DEAR GOD I DID IT
# This function just applies the catchr_behavior to a single expression
catch_expr <- function(expr, args, kwargs) {
  .myConditions <- NULL
  baby_env <- child_env(current_env())

  kwargs <- append(as_list(kwargs),
                   as_list(flesh_args_out(args)))
  print(kwargs)
  kwargs <- kwargs %>% imap(make_handler) %>%
    map(~`environment<-`(., baby_env))

  res <- withRestarts(with_handlers(expr, !!!kwargs),
                      return_error = function() NULL)
  append(list(value = res), .myConditions)

}

# plans <- clean_cond_input(error = exit,
#                         warning = c(collect, muffle),
#                         message = c(collect, towarning),
#                         spec_names = c("exit", "towarning", "display", "muffle", "collect"))
# blark({warning("a"); message("ooo"); message("nsass"); "yay"},
#       plans$args, plans$kwargs)

# This function makes wrapper functions that use the plan
make_catch_function <- function(args, kwargs) {
  function(expr) {
    .myConditions <- NULL
    baby_env <- child_env(current_env())

    kwargs <- append(as_list(kwargs),
                     as_list(flesh_args_out(args)))
    print(kwargs)
    kwargs <- kwargs %>% imap(make_handler) %>%
      map(~`environment<-`(., baby_env))

    res <- withRestarts(with_handlers(expr, !!!kwargs),
                        return_error = function() NULL)
    append(list(value = res), .myConditions)
  }
}
# make_catch_function(yap$args, yap$kwargs)({warning("a"); message("ooo"); message("nsass"); "yay"})




################ TESTING #########################################

test_that("Namespaces and environments", {
  taboo <- "sup"
  sup <- "NO"
  diffnamespace <- function(x) return(sup)
  samenamespace <- function(x) return(sup)
  environment(diffnamespace) <- child_env(asNamespace("base"),
                                          sup = "YES",
                                          diffnamespace = diffnamespace)
  # diffnamespace() should return "YES"

  # If you define it in the function, it should give a warning
  expect_warning(
    res1 <- clean_cond_input(d1 = function(x) { return(sup) },
                             spec_names = taboo)
  )
  expect_equal(res1$kwargs$d1(""), sup)

  expect_silent(
    res2 <- clean_cond_input(d2 = samenamespace,
                             d3 = diffnamespace,
                             spec_names = taboo)
  )

  expect_equal(sup, "NO")
  expect_equal(res2$kwargs$d2("~"), sup)
  expect_equal(res2$kwargs$d3("~"), "YES")

})

test_that("Explictly package-named functions", {
  # picked a 'random' base function
  acosh <- function(x) { "dummy" }

  expect_warning(
    res1 <- clean_cond_input(d1 = acosh, spec_names = "acosh")
  )
  # the kwarg has arg_pos attributes
  expect_failure(expect_equal(res1$kwargs$d1, "acosh"))
  expect_equivalent(res1$kwargs$d1, "acosh")

  expect_silent(
    res2 <- clean_cond_input(d1 = base::acosh, spec_names = "acosh")
  )
  expect_equal(res2$kwargs$d1(10), base::acosh(10))

})

test_that("Function names are not masked", {
  # picked a 'random' base function
  sup <- function(x) { function(y) {return("dummy")} }

  expect_silent(
    res <- clean_cond_input(d1 = sup(""), spec_names = "sup")
  )
  expect_equal(res$kwargs$d1(""), "dummy")

})

####################################################










# #
# #
# #
# test_envs <- function(..., spec_names) {
#   kwargs2 <- enquos(...)
#   print(kwargs2)
#   parent_envir <- caller_env()
#
#   v <- as_environment(
#     set_names(spec_names, spec_names),
#     parent = parent_envir)
#
#   kwargs <- kwargs2 %>%
#     map(~eval_tidy(set_env(.,v))) %>%
#     map(~classify_arg(., spec_names))
#
#   env_unbind(v, spec_names)
#
#   return(kwargs)
# }
#
#
#
#
# exit <- "A"
# sup <- "NO"
# sip <- function(x) print(sup)
# environment(sip) <- child_env(asNamespace("base"),
#                               sup = "JAMMA",
#                               sip = sip,
#                               chaos="ba")
# sip("fa")
#
#
# gs <- test_envs(fafa = c(function(x) print(exit), "sup"),
#                 nana = c(sip),
#                 # lala = exit,
#                 spec_names = c("exit", "abort", "sup", "display", "muffle", "collect"))
# gs$fafa[[1]]("X")
# gs$nana[[1]]("X")
# env_names(get_env(gs$fafa[[1]]))
# env_names(get_env(gs$nana[[1]]))
#
# env_get(get_env(gs$fafa[[1]]), "~")
#
# # fafa_maker <- function
#
# top <- new_environment(list(xxx = function() print("A")))
# middle <- env(top)
# bottom <- env(middle, xxx = "datasss")
# mask <- new_data_mask(bottom, top)
# eval_tidy(quote(xxx), mask)





