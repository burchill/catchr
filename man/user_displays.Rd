% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-handlers.R
\name{user_display}
\alias{user_display}
\alias{display_with}
\title{Display conditions in output terminal}
\usage{
user_display(cond, crayon_style, ...)

display_with(crayon_style, ...)
}
\arguments{
\item{cond}{A condition one wishes to display}

\item{crayon_style}{If \pkg{crayon} is installed, this can be either a \code{\link[crayon:crayon]{crayon style}} (e.g., \code{\link[crayon:crayon]{crayon::green()}}, \code{blue$bold}, etc.) or a character vector for \code{\link[crayon:combine_styles]{crayon::combine_styles()}}. These styles will be applied to the output.}

\item{\dots}{Parameters to pass into \code{\link[=extract_display_string]{extract_display_string()}}; namely \code{cond_name} (which controls how the condition is introduced) and \code{include_call}, which determines whether the call is included.}
}
\description{
These functions make a catchr plan immediately print the condition to the output terminal, similar to how \code{\link[=catchr-DSL]{display}} works. But unlike \code{display} and most catchr functions or special reserved terms, these functions are meant to be used in user-defined functions of a plan.

\code{user_display()} immediately displays a condition n the output terminal, and if \link[crayon:crayon]{crayon} is installed, will style the text with whatever \code{crayon} styling is supplied (either as a \code{crayon} function or a character vector for \code{\link[crayon:combine_styles]{crayon::combine_styles()}}). This function should be used \emph{within} a custom function, i.e., \code{function(x) {user_display(x, "pink")}}, and not called by itself, since when it is called, it doesn't evaluate to a function, string or unquoted term, which are required input types to a \link[=make_plans]{catchr plan}.

\code{display_with} can be used at the "top" level of a plan, since it returns a \emph{function} that calls \code{user_display()}. Thus \code{user_display("pink")} is equivalent to the example above.
}
\examples{
make_warnings <- function() {
  warning("This warning has a call")
  warning("This warning does not", call. = FALSE)
  invisible("done")
}

# The crayon stylings won't work if `crayon` isn't installed.
catch_expr(make_warnings(), warning = c(display_with("pink"), muffle))
catch_expr(make_warnings(),
           warning = c(display_with(c("pink","bold"), include_call = FALSE), muffle))
catch_expr(make_warnings(), warning = c(display_with("inverse", cond_name=NULL), muffle))
# If you don't want to use crayon styles, just use `NULL`
catch_expr(make_warnings(), warning = c(display_with(NULL, cond_name="Warning"), muffle))

# You can get a lot of weird crayon styles
if (requireNamespace("crayon", quietly = TRUE) == TRUE) {
  freaky_colors <- crayon::strikethrough$yellow$bgBlue$bold$blurred
  catch_expr(make_warnings(),
             warning = c(function(x) user_display(x, freaky_colors), muffle))
}
}
\seealso{
the \link{display} special term, which essentially uses a version of \code{user_display}; \code{\link[=user_exit]{user_exit()}} and \code{\link[=exit_with]{exit_with()}} for parallel functions for the \link{exit} special term, and \code{\link[=beep_with]{beep_with()}} for a parallel function for the \link{beep} special term.
}
