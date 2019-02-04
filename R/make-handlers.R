
#' Force an exit
#'
#' @description
#'
#' These functions force a catchr plan to immediately exit the evaluation of an expression (and the rest of the plan), similar to how \code{\link[=catchr-DSL]{exit}} works. But unlike `exit` and most catchr functions or special reserved terms, these functions are meant to be used in the user-defined functions of a plan.
#'
#' `user_exit()` forces the code to exit, and after exiting, evaluate whatever expression was supplied. This function should be used _within_ a custom function, i.e., `function(x) {user_exit(print("DONE!"))}`.
#'
#' `exit_with()` can be used at the "top" level of a plan, since it returns a _function_ that calls `user_exit()`. Thus `exit_with(print("DONE!"))` is equivalent to the example above. Additionally, if `as_fn` is set to `TRUE`, it will attempt to coerce `expr` into a function via `rlang`'s [rlang::as_function()]. If `expr` can be converted, `exit_with()` will return a function that takes in a condition, modifies it via `expr`, and then supplies this to `user_exit`.  E.g., `exit_with(~.$message)` is equivalent to `function(cond) {user_exit(cond$message)}`
#' @examples
#' yay <- catch_expr({warning("oops"); "done!"},
#'                   warning = exit_with("YAY"))
#'
#' # This won't work, since `user_exit("YAY")` doesn't evaluate to a function/string
#' \dontrun{
#' yay <- catch_expr({warning("oops"); "done!"},
#'                   warning = user_exit("YAY"))
#' }
#'
#' check <- function(cond) {
#'   if (inherits(cond, "simpleWarning"))
#'     user_exit(rlang::warn(paste0("Check it: ", cond$message)))
#'   else
#'     invokeRestart(first_muffle_restart(cond))
#'   NULL
#' }
#'
#' result <- catch_expr(
#'   { rlang::warn("This will be muffled")
#'     warning("This won't be muffled") },
#'   warning = check)
#' # Notice that `result` takes whatever the last (invisibly)
#' #   returned value is. Here, that's the message from the warning
#' result
#'
#' # If you don't want to accidentally assign what is returned by `user_exit`,
#' #   either add `NULL` to the end of the expresion:
#' result2 <- catch_expr(
#'   { rlang::warn("This will be muffled")
#'     warning("This won't be muffled")},
#'   warning = function(x) { user_exit({ warning("This won't be assigned"); NULL})})
#' result2
#'
#' # Or you can just do the assignment _within_ the expression being evaluated:
#' result3 <- NULL
#' catch_expr({result3 <- {
#'     rlang::warn("This will be muffled")
#'     warning("This won't be muffled")}},
#'   warning = check)
#' result3
#' @param expr An optional expression which if specified, will be evaluated after `user_exit` exits the evaluation.
#' @param as_fn A logical; if `TRUE`, catchr will try to convert `expr` into a function via [rlang::as_function()] which will be applied to the condition. It will fall back to normal behavior if this coercion raises an error.
#' @rdname user_exits
#' @seealso the [exit] special term, which essentially becomes `exit_with(NULL)`; [user_display()] and [display_with()] for parallel functions for the [display] special term, and [beep_with()] for a parallel function for the [beep] special term..
#' @export
user_exit <- function(expr = NULL) {
  q <- enquo(expr)
  if (quo_is_null(q))
    q <- NULL
  new_cond <- structure(
    class = c("last_stop", "condition"),
    list(message="Internal catchr use only", call=NULL,
         catchr_val = q))
  cnd_signal(new_cond)
  # invokeRestart(first_muffle_restart(cond))
}

#' @rdname user_exits
#' @export
exit_with <- function(expr, as_fn = FALSE) {
  q <- enquo(expr)
  if (as_fn)
    tryCatch({
      fn <- as_function(eval_tidy(q))
      function(cond) user_exit(fn(cond))
      },
      error = function(z) {
        warn("`exit_with` can't convert `expr` to function; will return it as instead", error_message = z$message)
        function(cond) user_exit(!! q)
      })
  else
    function(cond) user_exit(!! q)
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

# Gets cleaned plans, turns them into handlers with all their glorious shit
compile_plans <- function(kwargs, .opts, original_calls) {
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
    make_compiled_qual(original_calls, .opts)
}

# Internal
make_compiled_qual <- function(l, original_calls, .opts) {
  `attr<-`(l, "class", "catchr_compiled_plans") %>%
    `attr<-`("calls", original_calls) %>%
    `attr<-`("catchr_opts", .opts)
}


#' Check if list is a catchr plan
#'
#' Currently, this function just checks whether a list was made via
#' [make_plans()]. It does so simply by looking at the class of the list. \cr \cr
#' In the future, catchr plans may become more complicated and this will become a more useful part of the API.
#'
#' @param x An object to test
#' @export
is_catchr_plan <- function(x) {
  inherits_all(x, c("catchr_compiled_plans"))
}

#' Make a string to display from a condition
#'
#' Turn a condition into a string comprised of its message, name, and call, in a variety of configurations.
#'
#' @param cond A condition to display or turn into a string
#' @param cond_name Either the name of the condition you want to display, `NA` if you want the condition name to be assigned by default (the first class of the condition), or `NULL` if you don't want the condition type displayed at all.
#' @param include_call A logical; if `FALSE` the call won't be included in the string even if present in the condition.
#' @return A string
#' @export
extract_display_string <- function(cond, cond_name = NA, include_call = T) {
  msg <- cond$message
  call <- cond$call

  if (!is.null(call) && include_call)
    call <- paste0("in `", approx_arg_name(!!call), "`: ")
  else call <- NULL

  if (!is.null(cond_name) && is.na(cond_name))
    cond_name <- class(cond)[[1]]

  if (is.null(call) && !is.null(cond_name))
    cond_name <- paste0(cond_name, ": ")

  paste0(c(cond_name, call), collapse=" ") %>%
    paste0(msg)
}

#' Play short sounds
#'
#' @description
#'
#' If you have the [beepr][beepr::beep()] package installed, catchr can use it to play sounds when certain conditions are being handled with `beep_with()`, similar to how \code{\link[=catchr-DSL]{beep}} works. But unlike `beep` and most catchr functions or special reserved terms, `beep_with()` is meant to be used as a user-defined function in a plan. It is particularly useful for when you're working with `futures` and busy doing something else while code is running in the background, or when you're working in a different window and want something to grab your attention.
#'
#' `beep_with` can be used at the "top" level of a plan, since it returns a _function_ (which is required custom input for a catchr plan) that will play the beeping sound you've specified.
#'
#' @param beepr_sound A character string or number specifying the sound to be played. See the `sound` argument in [beepr::beep()] documentation.
#' @examples
#' warning_in_middle <- function() {
#'   Sys.sleep(2)
#'   message("It's time!")
#'   Sys.sleep(2)
#'   invisible("done")
#' }
#'
#' if (requireNamespace("beepr", quietly = TRUE) == TRUE) {
#'   catch_expr(warning_in_middle(),
#'              message = c(beep_with(2), display, muffle))
#'   # Or you can just use the default sound with "beep":
#'   catch_expr(warning_in_middle(), message = c(beep, display, muffle))
#' }
#' @seealso the [beep] special term, which will play the default beep; [user_exit()] and [exit_with()] for parallel functions for the [exit] special term, and [user_display()] and [display_with()] for parallel functions for the [display] special term.
#' @export
beep_with <- function(beepr_sound) {
  force(beepr_sound)
  if (!is_installed("beepr"))
    abort("Package `beepr` needs to be installed if `beep` is to be used.")
  else
    function(cond) beepr::beep(beepr_sound)
}



#' Display conditions in output terminal
#'
#' @description
#'
#' These functions make a catchr plan immediately print the condition to the output terminal, similar to how \code{\link[=catchr-DSL]{display}} works. But unlike `display` and most catchr functions or special reserved terms, these functions are meant to be used in user-defined functions of a plan.
#'
#' `user_display()` immediately displays a condition n the output terminal, and if [crayon][crayon::crayon] is installed, will style the text with whatever `crayon` styling is supplied (either as a `crayon` function or a character vector for [crayon::combine_styles()]). This function should be used _within_ a custom function, i.e., `function(x) {user_display(x, "pink")}`, and not called by itself, since when it is called, it doesn't evaluate to a function, string or unquoted term, which are required input types to a \link[=make_plans]{catchr plan}.
#'
#' `display_with` can be used at the "top" level of a plan, since it returns a _function_ that calls `user_display()`. Thus `user_display("pink")` is equivalent to the example above.
#'
#' @param cond A condition one wishes to display
#' @param crayon_style If \pkg{crayon} is installed, this can be either a \code{\link[crayon:crayon]{crayon style}} (e.g., [crayon::green()], `blue$bold`, etc.) or a character vector for [crayon::combine_styles()]. These styles will be applied to the output.
#' @param \dots Parameters to pass into [extract_display_string()]; namely `cond_name` (which controls how the condition is introduced) and `include_call`, which determines whether the call is included.
#' @examples
#' make_warnings <- function() {
#'   warning("This warning has a call")
#'   warning("This warning does not", call. = FALSE)
#'   invisible("done")
#' }
#'
#' # The crayon stylings won't work if `crayon` isn't installed.
#' catch_expr(make_warnings(), warning = c(display_with("pink"), muffle))
#' catch_expr(make_warnings(),
#'            warning = c(display_with(c("pink","bold"), include_call = FALSE), muffle))
#' catch_expr(make_warnings(), warning = c(display_with("inverse", cond_name=NULL), muffle))
#' # If you don't want to use crayon styles, just use `NULL`
#' catch_expr(make_warnings(), warning = c(display_with(NULL, cond_name="Warning"), muffle))
#'
#' # You can get a lot of weird crayon styles
#' if (requireNamespace("crayon", quietly = TRUE) == TRUE) {
#'   freaky_colors <- crayon::strikethrough$yellow$bgBlue$bold$blurred
#'   catch_expr(make_warnings(),
#'              warning = c(function(x) user_display(x, freaky_colors), muffle))
#' }
#' @rdname user_displays
#' @seealso the [display] special term, which essentially uses a version of `user_display`; [user_exit()] and [exit_with()] for parallel functions for the [exit] special term, and [beep_with()] for a parallel function for the [beep] special term.
#' @export
user_display <- function(cond, crayon_style, ...) {
  string <- extract_display_string(cond, ...)
  string <- give_newline(string)

  if (is_installed("crayon") && !is.null(crayon_style)) {
    if (is_character(crayon_style))
      crayon_style <- lift_dl(crayon::combine_styles)(crayon_style)
    else if (!inherits(crayon_style, "crayon"))
      abort("The crayon style must be a crayon style function or a string!")
    string <- crayon_style(string)
  }

  cat(string)
  invisible(NULL)
}

#' @rdname user_displays
#' @export
display_with <- function(crayon_style, ...) {
  force(crayon_style)
  # A way of forcing dots that's safe even when there are no dots
  enquos(...)
  function(cond) user_display(cond, crayon_style, ...)
}




# sub in special term functions
use_special_terms <- function(s, cond_type) {
  switch(
    s,
    exit = function(cond) {
      user_exit(NULL)
    },
    towarning = function(cond) {
      class(cond) <- c("warning","condition")
      warning(cond)
    },
    tomessage = function(cond) {
      class(cond) <- c("message","condition")
      if (!is.null(cond$message) & cond$message != "")
        cond$message <- give_newline(cond$message, trim = FALSE)
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
      user_display(cond, c("blue", "bold"), cond_name = cond_type)
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
# vals = The values of a 'cleaned' catchr plan
# name = The type of condition being handled
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

