#' To-do:
#'
#'  - make a more centralized way of collecting--figure out how you can avoid overlap between "warning" and "condition"
#'  - do "muffle"
#'  - make the spec_terms thing use package defaults somehow
#'  - make a 'misc' condition thing which gets everything that isn't already being caught
#'
#'  - make a help page that describes how things are masked:
#'      * The only thing that is masked in evaluation is the non-function versions of the special names.
#'      * Egh, just make a help page and connect it to the warning message


# missing: "muffle", "collect"
special_terms <- c("towarning", "tomessage", "toerror",
                   "display", "beep", "exit")

# Makes sure a \n is at the end
give_newline <- function(s, trim = FALSE) {
  if (trim == T)
    return(paste0(trimws(s, "right"), "\n"))
  if (substr(s, nchar(s), nchar(s)) != "\n")
    return(paste0(s, "\n"))
  else return(s)
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
            "` have special meaning in these arguments, but seem to already be defined elsewhere.  These previous definitions may be masked when determining condition behavior.",
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

# adds back in the argument positions as attribut
add_back_arg_pos <- function(new_l, old_l) {
  map2(new_l, old_l, function(k, old) {
    `attr<-`(k, 'arg_pos', attr(old, 'arg_pos')) })
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
# Combines functions
combine_functions <- function(...) {
  l <- enexprs(...) %>%
    map(~substitute(zzzz(cond), c(zzzz = .)))
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
    beep = function(cond) {
      if (!is_installed("beepr"))
        abort("Package `beepr` needs to be installed if `beep` is to be used.")
      else
        beepr::beep()
    },
    display = function(cond) {
      str(cond, max.level = 1)
    },
    collect = function(cond) {
      .myConditions[[cond_type]] <<- append(.myConditions[[cond_type]],
                                            list(cond))
    },
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
      vals <- function(x) { NULL }
  }
  vals <- map(vals, function(x) {
    if (is_callable(x)) x
    else use_special_terms(x)
  })
  combined_func <- combine_functions(!!!vals)

  if (exit_bool) exiting(combined_func)
  else calling(combined_func)
}

# Turns the unnamed arguments into the defaults
flesh_args_outs <- function(args, default_plan = NULL) {
  if (is.null(default_plan))
    default_plan = getOption("default.catchr.plan", default_catchr_plan)
  map(args, ~default_plan) %>%
    set_names(args)
}

with_handlers({message("aaa"); "done"},
              message = make_handler("message", c("display","towarn")))

make_handler("message", c("display","towarn","exit"))


# Ones to use: display, collect, beep, to<blank>, muffle
# display, exit, muffle, collect, beep, #also: towarn, toerror, tomessage


clean_cond_input(error = exit,
  stop,
  warning = c(display, muffle),
  message = c(collect, warning, muffle),
  spec_names = c("exit", "sup", "display", "muffle", "collect","stop"))$args %>%
  str()

baba <- function(...) {
  args_and_kwargs(...)
}
baba("error",mex="no","gone","asa")




make_catchr_function <- function(args, kwargs) {
  myConditions <- NULL

  kwargs <- append(as_list(kwargs),
                   as_list(flesh_args_outs))


}


conditionHandler <- function(cond) {
  .myConditions[[cond_type]] <<- append(.myConditions[[cond_type]], list(cond))
}

blark <- function(e, f) {
  .myConditions <- NULL

  new_f <- calling(use_special_terms(f,"warning"))
  # environment(new_f) <- current_env()
  new_f("ASASA")
  print(.myConditions)

  za <- withCallingHandlers(
    withRestarts(e, return_NULL = function() NULL),
    warning = new_f)
  print(.myConditions)
  za
}
blark({warning("a"); warning("b"); "yay"}, "collect")



# yowza <- function(s) {
#   return(function(x) { sup })
# }
#
# # for (i in kwargs) {
# #   print(env_names(get_env(i)))
# # }
#
# fwah <- function(f, spec_names) {
#   q
#   q <- quo()
#   q <- quo_set_expr(q, enexpr(f))
#   v <- as_environment(
#     set_names(spec_names, spec_names),
#     parent = caller_env())
#   # q <- set_env(q, v)
#   mask <- new_data_mask(v)
#
#   print(set_env(q, v))
#   # print()
#   # print(eval_tidy(expr(print(sup)), env=v))
#   eval_tidy(q, data = mask)
# }
# fwah(f(),"sup")
#
#
# sip <- f
# environment(sip) <- child_env(asNamespace("base"),
#                               sup = "JAMMA",
#                               sip = sip,
#                               chaos="ba")
# sip()


#
# # ignore stuff in functions that are defined
# clean_cond_input(d1 = yowza, spec_names = taboo)
# clean_cond_input(d1 = function(x) return(sup), spec_names = taboo)
# clean_cond_input(d1 = list(function(x) { sup <- 3; return(sup) }), spec_names = taboo)
#
#
# res1 <- clean_cond_input(d1 = list(sup, function(x) { return(sup) }), spec_names = taboo)
# res1$kwargs$d1[[1]]
# res1$kwargs$d1[[2]]("a")
# res2 <- clean_cond_input(d1 = list(sup, sup), spec_names = taboo)




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

  expect_equal(res$kwargs$d1("."), exit)

  expect_equal(sup, "NO")
  expect_equal(res$kwargs$d2("~"), sup)
  expect_equal(res$kwargs$d3("~"), "YES")

})

test_that("Explictly package-named functions", {
  # picked a 'random' base function
  acosh <- function(x) { "dummy" }

  expect_warning(
    res1 <- clean_cond_input(d1 = acosh, spec_names = "acosh")
  )
  expect_equal(res1$kwargs$d1, "acosh")

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




