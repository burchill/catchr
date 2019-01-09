.onLoad <- function(libname, pkgname) {
  # Unless already set, the default plan is to collect and muffle
  catchr.default_plan <- getOption("catchr.default_plan", catchr.default_plan)
  # Set the default just in case it isn't already
  options("catchr.default_plan" = catchr.default_plan)

  catchr.warn_about_terms <- getOption("catchr.warn_about_terms", catchr.warn_about_terms)
  catchr.bare_if_possible <- getOption("catchr.warn_about_terms", catchr.bare_if_possible)
  catchr.drop_empty <- getOption("catchr.warn_about_terms", catchr.drop_empty)
}

special_terms <- c("towarning", "tomessage", "toerror",
                   "display", "beep", "exit", "muffle", "collect",
                   "raise")

catchr.default_plan <- list("collect", "muffle")
catchr.warn_about_terms <- T
catchr.bare_if_possible <- F
catchr.drop_empty <- F
