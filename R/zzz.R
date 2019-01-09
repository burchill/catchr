.onLoad <- function(libname, pkgname) {
  # Unless already set, the default plan is to collect and muffle
  catchr.default_plan <- getOption("catchr.default_plan",
                                   list("collect","muffle"))
  # Set the default just in case it isn't already
  options("catchr.default_plan" = catchr.default_plan)
}
