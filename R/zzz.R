.onLoad <- function(libname, pkgname) {
  ov <- catchr_original_default_values
  # Set the defaults just in case they aren't already
  options("catchr.default_plan" = getOption("catchr.default_plan", ov$catchr.default_plan))
  options("catchr.warn_about_terms" = getOption("catchr.warn_about_terms", ov$catchr.warn_about_terms))
  options("catchr.bare_if_possible" = getOption("catchr.bare_if_possible", ov$catchr.bare_if_possible))
  options("catchr.drop_empty_conds" = getOption("catchr.drop_empty_conds", ov$catchr.drop_empty_conds))
  ov <- NULL
}
