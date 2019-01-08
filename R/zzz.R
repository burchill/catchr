.onLoad <- function(libname, pkgname) {
  # Unless already set, the default plan is to collect and muffle
  default_catchr_plan <- getOption("default.catchr.plan",
                                   list("collect","muffle"))
  # Set the default just in case it isn't already
  options("default.catchr.plan" = default_catchr_plan)
}
