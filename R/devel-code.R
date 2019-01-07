# # Used to pass condition out of `withCallingHandlers`
# exit_call_fn <- function(cond) {
#   stop(cnd(".rlang_exit_calling_condition",
#            message="Internal exit calling error",
#            orig_cond = cond))
# }
#
# # The 'internal' handler for ".rlang_exit_calling_condition" conditions
# rlang_internal_handler <- function(cond) {
#   cnd_signal(cond$orig_cond)
# }
#
# # Adds class of ".rlang_checked_cond" to conditions, muffles the original and raises the new
# mark_orig_conditions <- function(cond) {
#   if (!inherits(cond, "error")) {
#     class(cond) <- c(class(cond), ".rlang_checked_cond")
#     cnd_signal(cond)
#     cnd_muffle(cond)
#   }
# }
# # Removes the ".rlang_checked_cond" class from conditions
# remove_checked_cond_class <- function(cond) {
#   classes <- class(cond)
#   class(cond) <- classes[classes!=".rlang_checked_cond"]
#   cond
# }
#
# # Takes handler functions, and makes new ones that run `remove_checked_cond_class` on the input before processing it
# # Dependent on the fact (I believe) that handler functions are always and only supplied one argument (the condition)
# mod_handlers_to_remove <- function(fn) {
#   first_arg <- fn_fmls_syms(fn)[[1]]
#   body <- substitute(
#     fn(remove_checked_cond_class(first_arg)),
#     list("fn" = fn, "first_arg" = first_arg))
#   new_func <- function(x) x
#   formals(new_func) <- fn_fmls(fn)[1]
#   body(new_func) <- body
#   new_func
# }
#
# # lets you use both handlers
# with_both_handlers <- function (.expr, ...) {
#   handlers <- rlang:::map(list2(...), as_function)
#
#   nms <- names2(handlers)
#   nms <- ifelse(nms == "condition", ".rlang_checked_cond", nms)
#
#   if (any(nms == ""))
#     abort("All handlers must be named arguments")
#   if (length(unique(nms)) != length(nms))
#     abort("Each handler argument must have a unique name")
#
#   fake_calling_fns <- rep(list(exit_call_fn),
#                           length(handlers))
#
#   is_calling <- rlang:::map_lgl(handlers, inherits, "calling")
#   exiting <- handlers[!is_calling]
#
#   calling <- ifelse(is_calling==T,
#                     rlang:::map(handlers, mod_handlers_to_remove),
#                     fake_calling_fns)
#   names(calling) <- nms
#
#   calling <- c(condition = calling(mark_orig_conditions),
#                     rlang:::map(calling, function(x) calling(x)))
#
#   expr <- quote(.expr)
#   expr <- expr(
#     tryCatch(
#       tryCatch(
#         withCallingHandlers(!!expr, !!!calling),
#         .rlang_exit_calling_condition = rlang_internal_handler
#       ), !!!exiting)
#   )
#   # print(calling)
#   # I've been using the following to test my code in place of the `.Call` function
#   eval_tidy(expr)
#   # .Call(rlang_eval, expr, environment())
# }
#
#
# # ------------------ Some basic tests ---------------------------------------------#
# testing_function <- function() {
#   warn("Bottom warning")
#   "end result!"
# }
#
# with_both_handlers(testing_function(),
#                    warning = exiting(function(x)
#                      print(paste0("you had a warning: ", x$message))),
#                    condition = calling(function(x) {
#                      print("There was a condition but I squashed it")
#                      cnd_muffle(x)}),
#                    error = calling(function(x) print("C")))
#
# with_both_handlers(testing_function(),
#                    condition = calling(function(x) {
#                      print("This muffles any warnings before they can be exited")
#                      print(paste0("This is the behavior one would both want and expect,",
#                                   " given that handlers get checked in order."))
#                      cnd_muffle(x)
#                    }),
#                    warning = exiting(function(x)
#                      print(paste0("you had a warning: ", x$message))))
#
#
#
# # This is what I did at a conceptual level:
# # Since `withCallingHandlers` only accepts calling handlers, I turn everything into 'calling handlers'. However, for the handlers that I pass into `withCallingHandlers`, I replace the `exiting` handlers with a function that throws a 'unique' condition with type: '.rlang_exit_calling_condition', which contains the originally thrown condition as data. The unique condition then halts `withCallingHandlers`. However, `withCallingHandlers` is wrapped by a `tryCatch` function that catches this unique condition, extracts the original condition, and signals that to a higher `tryCatch` function that contains all of the "real" `exiting` handlers. They then do whatever they're supposed to.
# # What I did in practice is a bit more complicated. Although my code **IS** built on the assumption that the only code that will ever throw a '.rlang_exit_calling_condition' condition will be `rlang`, the way I've described the process so far would still run into problems when someone tries to catch "general" conditions (e.g., a handler like `condition = calling(print)`), which would catch the '.rlang_exit_calling_condition' condition and possibly prevent it from halting `withCallingHandlers`. I feel like catching general conditions like this is not so uncommon, so this would definitely be an issue.
# # My work-around was to modify the supplied calling handlers and automatically add a specific handler to the front of the list (I believe they checked in order). The new handler is a general condition handler, and takes every condition that is raised from `.expr`, gives it a custom type of '.rlang_checked_cond', muffles the original condition and signals the one with the additional type. Any *supplied* general condition handlers are change to catch '.rlang_checked_cond' conditions, and I modify all the supplied functions so that they remove the '.rlang_checked_cond' class before they process the condition.
# #





# Makes sure a \n is at the end
give_newline <- function(s, trim = FALSE) {
  if (trim == T)
    return(paste0(trimws(s, "right"), "\n"))
  if (substr(s, nchar(s), nchar(s)) != "\n")
    return(paste0(s, "\n"))
  else return(s)
}

# Recursively moves through AST
check_nodes <- function(x, nms) {
  if (is_symbol(x) && deparse(x) %in% nms)
    signal(paste0("Reserved symbol in arguments: ", deparse(x)),
           "passer", val=deparse(x))
  # cnd_signal(msg=paste0("Reserved symbol in arguments: ", deparse(x)),
  #            .cnd="passer", val=deparse(x))
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
    warning("`", paste(bad_boys,collapse = "`, `"),
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
  if (length(arg) > 1 || is_list(arg)) {
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

  # Unbind the special names from v
  env_unbind(v, spec_names)

  # Check args
  walk(args,
       function(arg)
         if (arg %in% names(kwargs))
           abort(paste0("'", arg, "' is both an unnamed and named argument")))
  return(list(args = args, kwargs = kwargs))
}








test_that("Namespaces and environments", {
  exit <- "A"
  sup <- "NO"
  diffnamespace <- function(x) return(sup)
  samenamespace <- function(x) return(sup)
  environment(diffnamespace) <- child_env(asNamespace("base"),
                                sup = "YES",
                                diffnamespace = diffnamespace)
  # diffnamespace("fa") should return "YES"

  expect_warning(res <- clean_cond_input(
    # d1 = function(x) { return(exit) },
    d2 = samenamespace,
    d3 = diffnamespace,
    spec_names = c("sup", "exit")))

  expect_equal(res$kwargs$d1("."), exit)

  expect_equal(sup, "NO")
  expect_equal(res$kwargs$d2("~"), sup)
  expect_equal(res$kwargs$d3("~"), "YES")

})

test_that("Explictly package-named functions", {
  # picked a 'random' base function
  acosh <- function(x) { return("dummy") }

  expect_warning(
    res1 <- clean_cond_input(d1 = acosh, spec_names = "acosh")
  )
  expect_silent(
    res2 <- clean_cond_input(d1 = base::acosh, spec_names = "acosh")
  )

})






# Testing

#


# Testing ######################
surp <- clean_cond_input(#fafa = function(x) { print(exit) },
                         nana = beepr::beep,
                         yaya = function(x) {beepr::beep("beep")},
                         # lala = exit,
                         spec_names = c("exit", "abort", "sup", "display", "muffle", "collect", "beep"))
surp$kwargs$fafa("soop")

# Testing ######################
surp <- clean_cond_input(fafa = function(x) { print(exit) },
                         nana = sip,
                         # lala = exit,
                         spec_names = c("exit", "abort", "sup", "display", "muffle", "collect"))
surp$kwargs$fafa("soop")
###########################

###########################


quo(!!(!!test_cnd))




use_special_terms <- function(s, spec_terms) {
  switch(s,
         towarn = function(cond) {
           class(cond) <- c("warning","condition")
           warning(cond)
         }
         tomessage = function(cond) {
           class(cond) <- c("message","condition")
           if (!is.null(cond$message) & cond$message != "")
             cond$message <- give_newline(cond$message, trim = F)
           message(cond)
         }
         beep = function(cond) {
           if (!is_installed("beepr")) {
             abort("Package `beepr` needs to be installed if `beep` is being used")
           }
         }
  )

}


raise_stuff <- function() {
  warning("YO")
  message("hey")
  warning("2")
}

with_handlers(
  raise_stuff(),
  warning = calling(function(cond) {message(cond); cond})
)







# display, exit, muffle, collect, beep, #also: towarn, toerror, tomessage



withCallingHandlers(warning(a), message = function(x) print(computeRestarts(x)))







  new_environment(data = list(sup = "JAMMA",
                                                sip=sip))
eval_tidy(quote(sip("A")))


ye <- function() {
  ls()
}
environment(ye) <- new_environment(data = list(sup = "JAMMA"))
ye()



#
#
#
test_envs <- function(..., spec_names) {
  kwargs2 <- enquos(...)
  print(kwargs2)
  parent_envir <- caller_env()

  v <- as_environment(
    set_names(spec_names, spec_names),
    parent = parent_envir)

  kwargs <- kwargs2 %>%
    map(~eval_tidy(set_env(.,v))) %>%
    map(~classify_arg(., spec_names))

  env_unbind(v, spec_names)

  return(kwargs)
}




exit <- "A"
sup <- "NO"
sip <- function(x) print(sup)
environment(sip) <- child_env(asNamespace("base"),
                              sup = "JAMMA",
                              sip = sip,
                              chaos="ba")
sip("fa")


gs <- test_envs(fafa = c(function(x) print(exit), "sup"),
          nana = c(sip),
          # lala = exit,
          spec_names = c("exit", "abort", "sup", "display", "muffle", "collect"))
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


function (cnd)
{
  switch(cnd_type(cnd), message = invokeRestart("muffleMessage"),
         warning = invokeRestart("muffleWarning"), interrupt = invokeRestart("resume"))
  if (inherits(cnd, "rlang_condition")) {
    invokeRestart("rlang_muffle")
  }
  abort("Can't find a muffling restart")
}


cnd_muffle <- function (cnd) {
  possibleRestarts <- computeRestarts(cond)
  muffleRestarts <- possibleRestarts[grepl("muffle", Map(function(x) x$name, possibleRestarts))]
  resumeRestarts <- possibleRestarts[grepl("resume", Map(function(x) x$name, possibleRestarts))]
  if (length(muffleRestarts)) {
    invokeRestart(muffleRestarts[[1]])
  } else if (length(muffleRestarts)) {
    invokeRestart(resumeRestarts[[1]])
  } else {
    abort("Can't find a muffling restart")
  }
}




