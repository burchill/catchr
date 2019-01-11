.onLoad <- function(libname, pkgname) {
  # Unless already set, the default plan is to collect and muffle
  catchr.default_plan <- getOption("catchr.default_plan", list("collect", "muffle"))
  # By default, you'll be warned about catchrs DSL
  catchr.warn_about_terms <- getOption("catchr.warn_about_terms", TRUE)
  # By default, you want to always return a list when you can collect
  catchr.bare_if_possible <- getOption("catchr.bare_if_possible", FALSE)
  # By default you don't want to drop stuff
  catchr.drop_empty <- getOption("catchr.drop_empty", FALSE)


  # Set the defaults just in case they aren't already
  options("catchr.default_plan" = catchr.default_plan)
  options("catchr.warn_about_terms" = catchr.warn_about_terms)
  options("catchr.bare_if_possible" = catchr.bare_if_possible)
  options("catchr.drop_empty" = catchr.drop_empty)

}
