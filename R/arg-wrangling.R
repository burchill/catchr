#' Separate dots into Python-esque \code{*args} and \code{**kwargs}
#'
#' This function will return a named list with two sublists, 'args' and 'kwargs', which contain the unnamed and named arguments as quosures, respectively. \cr \cr
#' This is useful for when you want these two types of arguments to behave differently, e.g., as they do in [make_plans()]. The quosures will also have the attribute `'arg_pos'`, which will indicate their position in the original order in which they were supplied.
#'
#' @note
#'
#' This function started out in the `zplyr` package.
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

# Turns the unnamed arguments into the defaults
give_default <- function(args, default_plan = NULL) {
  if (is.null(default_plan))
    default_plan = getOption("catchr.default_plan")
  map(args, ~default_plan) %>%
    set_names(args)
}



