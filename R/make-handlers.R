
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
      utils::str(cond, max.level = 1)
    },
    muffle = substitute({
      restart = first_muffle_restart(cond)
      if (!is.null(restart))
        on.exit(invokeRestart(restart), add = TRUE)
      NULL
    }, NULL),
    collect = substitute({
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
