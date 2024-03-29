% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arg-wrangling.R
\name{args_and_kwargs}
\alias{args_and_kwargs}
\title{Separate dots into Python-esque \code{*args} and \code{**kwargs}}
\usage{
args_and_kwargs(..., .already_quosure = FALSE)
}
\arguments{
\item{\dots}{Any mix of named and unnamed arguments}

\item{.already_quosure}{if the arguments are already all quosures (in which case it will just sort them by named vs. unnamed arguments)}
}
\value{
A named list of lists, with \verb{$args} being a list of quosures of the unnamed arguments and \verb{$kwargs} being a list of quosures of the named arguments.
}
\description{
This function will return a named list with two sublists, 'args' and 'kwargs', which contain the unnamed and named arguments as quosures, respectively. \cr \cr
This is useful for when you want these two types of arguments to behave differently, e.g., as they do in \code{\link[=make_plans]{make_plans()}}. The quosures will also have the attribute \code{'arg_pos'}, which will indicate their position in the original order in which they were supplied.
}
\note{
This function started out in the \code{zplyr} package.
}
\examples{

x <- args_and_kwargs(unnamed_1, named_1="ba", "unnamed_2", named_2 = letters)
print(x$args)
print(x$kwargs)

\dontrun{
# Or see the `share_scales` from the `zplyr` package
share_scales <- function(...) {
  akw <- args_and_kwargs(...)
  # Unnamed arguments are ggplot scales
  geom_func_list <- purrr::map(akw$args, rlang::eval_tidy)
  # Named arguments are to be passed into those scales
  geoms <- purrr::map(geom_func_list, ~.(!!!akw$kwargs))
  return(geoms)
}
}
}
