#' Separate dots into Python-esque \code{*args} and \code{**kwargs}
#'
#' This function will return a named list with two sublists, 'args' and 'kwargs', which contain the unnamed and named arguments as quosures. \cr \cr
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

# adds back in the argument positions as attribut
add_back_arg_pos <- function(new_l, old_l) {
  map2(new_l, old_l, function(k, old) {
    `attr<-`(k, 'arg_pos', attr(old, 'arg_pos')) })
}

# uses the 'arg_pos' attribute to order a list
order_by_arg_pos <- function(l) {
  l[order(map_dbl(l, ~attr(., "arg_pos")))]
}


#' Make sure a function can be a handler
#'
#' This makes sure that a given function doesn't require more than one argument passed into it, and has at least one argument (which is what a \link[base:conditions]{handler} needs).
#'
#' @param fn A function
#' @export
has_handler_args <- function(fn) {
  args <- Map(is_missing, fn_fmls(fn)) # purrr can't iterate over pairlist
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
