
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


#' Force an exit
#'
#' @description
#'
#' These functions force a `catchr` plan to immediately exit the evaluation of an expression (and the rest of the plan), similar to how \code{\link[=catchr_DSL]{exit}} works. But unlike `exit` and most `catchr` functions or special reserved terms, these functions are meant to be used in the user-defined functions of a plan.
#'
#' `force_exit()` forces the code to exit, and after exiting, evaluate whatever expression was supplied. This function should be used _within_ a custom function, i.e., `function(x) {force_exit(print("DONE!"))}`.
#'
#' `exit_with()` can be used at the "top" level of a plan, since it returns a _function_ that calls `force_exit()`. Thus `exit_with(print("DONE!"))` is equivalent to the example above. Additionally, if `as_fn` is set to `TRUE`, it will attempt to coerce `expr` into a function via `rlang`'s [rlang::as_function()]. If `expr` can be converted, `exit_with()` will return a function that takes in a condition, modifies it via `expr`, and then supplies this to `force_exit`.  E.g., `exit_with(~.$message)` is equivalent to `function(cond) {force_exit(cond$message)}`
#' @examples
#' yay <- catch_expr({warning("oops"); "done!"},
#'                   warning = exit_with("YAY"))
#'
#' # This won't work
#' \dontrun{
#' yay <- catch_expr({warning("oops"); "done!"},
#'                   warning = force_exit("YAY"))
#' }
#'
#' check <- function(cond) {
#'   if (inherits(cond, "simpleWarning"))
#'     force_exit(rlang::warn(paste0("Check it: ", cond$message)))
#'   else
#'     invokeRestart(first_muffle_restart(cond))
#'   NULL
#' }
#'
#' rlang::catch_cnd(
#'   result <- catch_expr({
#'     rlang::warn("This will be muffled")
#'     warning("This won't be muffled")
#'   },
#'   warning = check)
#' )
#' # Notice that `result` takes whatever the last (invisibly)
#' #   returned value is. Here, that's the message from the warning
#' result
#'
#' # If you don't want to accidentally assign what is returned by `force_exit`,
#' #   either add `NULL` to the end of the expresion:
#' rlang::catch_cnd(
#'   result2 <- catch_expr({
#'     rlang::warn("This will be muffled")
#'     warning("This won't be muffled")
#'   }, warning = exit_with(~{ warning("This won't be assigned"); "this will be assigned"}, as_fn = T))
#' )
#' result2
#' # Or you can just do the assignment _within_ the expression being evaluated:
#' result3 <- NULL
#' rlang::catch_cnd(
#'   catch_expr(result3 <- {
#'     rlang::warn("This will be muffled")
#'     warning("This won't be muffled")
#'   },
#'   warning = check)
#' )
#' result3
#' @param expr An optional expression which if specified, will be evaluated after `force_exit` exits the evaluation.
#' @param as_fn A logical; if `TRUE`, `catchr` will try to conver `expr` into a function via [rlang::as_function()] which will be applied to the condition. It will fall back to normal behavior if this coercion raises an error.
#' @rdname forcing_exits
#' @export
force_exit <- function(expr = NULL) {
  q <- enquo(expr)
  if (quo_is_null(q))
    q <- NULL
  new_cond <- structure(
    class = c("last_stop", "condition"),
    list(message="Internal `catchr` use only", call=NULL,
         catchr_val = q))
  cnd_signal(new_cond)
  # invokeRestart(first_muffle_restart(cond))
}

#' @rdname forcing_exits
#' @export
exit_with <- function(expr, as_fn = FALSE) {
  q <- enquo(expr)
  if (as_fn)
    tryCatch({
      fn <- as_function(eval_tidy(q))
      function(cond) force_exit(fn(cond))
      },
      error = function(z) {
        warn("`exit_with` can't convert `expr` to function; will return it as instead", error_message = z$message)
        function(cond) force_exit(!! q)
      })
  else
    function(cond) force_exit(!! q)
}




# The default "last_stop" plan
forced_exit_plan <- function(cond) {
  if (!is.null(cond$catchr_val)) eval_tidy(cond$catchr_val)
  else NULL
}

# Modifies any 'condition' handlers to they don't catch the forced exits
add_exit_protector <- function(condition_plan) {
  new_body <- substitute(
    {if (!inherits(cond, "last_stop")) {condition_plan} },
    list(condition_plan = fn_body(condition_plan)))

  fn_body(condition_plan) <- new_body
  condition_plan
}

# Takes a misc handler and makes it workable
make_a_misc_handler_fn <- function(.f, cnames) {
  cnames <- cnames[!(cnames %in% c("misc", "condition"))]
  if (is_empty(cnames))
    return(.f)

  new_body <- substitute({
    if (inherits_any(cond, cnames))
      NULL
    else .f
  },
  list(.f = fn_body(.f), cnames = cnames))

  fn_body(.f) <- new_body
  .f
}

# Checks if a kwarg has "collect" in it
has_collect <- function(kwargs) {
  bools <- map_lgl(
    kwargs,
    function(kwarg) {
      if (is_vector(kwarg))
        reduce(kwarg, ~.x==T || (is.character(.y) && .y=="collect"), .init=F)
      else FALSE })
  names(kwargs[bools])
}



# Gets cleaned plans, turns them into handlers with all their glorious shit
compile_plans <- function(kwargs, .opts) {
  .opts$collectors <- has_collect(kwargs)
  names <- names(kwargs)

  handlers <- kwargs %>%
    imap(make_handler)

  # Takes care of the 'last_stop' handler
  if ("last_stop" %in% names) {
    # Right now, I'm not letting the user specify this type of plan
    stop("'last_stop' is a reserved condition name in catchr!")
    # if (utils::tail(names, 1) != "last_stop")
    #   abort("The 'last_stop' plan, if specified, must be the last plan.")
  } else {
    handlers <- append(handlers, list(last_stop = forced_exit_plan))
    names <- names(handlers)
  }

  # Takes care of the 'misc' handler
  if ("misc" %in% names) {
    if ("condition" %in% names)
      abort("Can't have both a 'misc' plan AND a general 'condition' plan at the same time.")
    # Rename 'misc' to 'condition'
    names <- ifelse(names == "misc", "condition", names)
    names(handlers) <- names
    handlers$condition <- make_a_misc_handler_fn(handlers$condition, names)
  }

  # Takes care of the 'condition' handler
  if ("condition" %in% names) {
    if ("misc" %in% names)
      abort("Can't have both a 'misc' plan *and* a general 'condition' plan")
    handlers$condition <- add_exit_protector(handlers$condition)
  }
  handlers %>%
    make_compiled_qual() %>%
    `attr<-`("catchr_opts", .opts)
}

# Internal
make_compiled_qual <- function(x) {
  `attr<-`(x, "class", "catchr_compiled_plans")
}
is_compiled_plan <- function(x) {
  inherits(x, "catchr_compiled_plans")
}

# with_ordered_handlers(
#   { warning("a"); print("YO")},
#   warning = calling(function(x) {
#     # print(str(x))
#     force_exit(x)
#     invokeRestart("muffleWarning")
#     print("nasssty")
#   }),
#   condition = exiting(function(x) {
#     if ("last_stop" %in% class(x)) {
#       print("gotcha")
#     } else {
#       print("why hello")
#
#       invokeRestart(first_muffle_restart(x))
#     }
#   }),
#   "last_stop" = exiting(function(x) {
#     print(paste0("Found a B condition with class: ",
#                  paste(x$old_class, collapse=" ")))
#     "Failure"}
#   )
# )


# sub in special term functions
use_special_terms <- function(s, cond_type) {
  switch(
    s,
    exit = function(cond) {
      force_exit()
    },
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
      utils::str(cond, max.level = 1)
    },
    muffle = substitute({
      restart = first_muffle_restart(cond)
      if (!is.null(restart))
        on.exit(invokeRestart(restart), add = TRUE)
      NULL
    }, NULL),
    collect = substitute({
      # update_collected(linker,cond_type,cond)
      .myConditions[[cond_type]] <<- append(.myConditions[[cond_type]], list(cond))
      NULL
    }, list(cond_type=cond_type)),
    stop(paste0("`", s, "` is not a possible choice"))
  )
}

# Makes a handler from a kwarg

#' Make catchr handler
#'
#' To-do: add docs. This won't work for `collect` and `muffle` for random user-defined stuff. Not really meant for users, but I exported it anyway.
#'
#'
#' @param vals The values of a 'cleaned' `catchr` plan
#' @param name The type of condition being handled
#' @export
make_handler <- function(vals, name) {
  if (!is_vector(vals))
    vals <- list(vals)

  first_exit <- detect_index(vals, ~is_string(.) && . =="exit")
  if (0 < first_exit && first_exit < length(vals)) {
    warning("'", name, "' set to exit before ",
            length(vals) - first_exit, " other defined functions", call.=F)
    # vals <- vals[1:(first_exit)]
  }

  vals <- map(vals, function(x) {
    if (is_callable(x)) x
    else use_special_terms(x, name)
  })
  combined_func <- combine_functions(!!!vals)
}

