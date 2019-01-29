context("test-testing")

test_that("Namespaces and environments", {
  taboo <- "sup"
  sup <- "NO"
  diffnamespace <- function(x) return(sup)
  samenamespace <- function(x) return(sup)
  environment(diffnamespace) <- child_env(asNamespace("base"),
                                          sup = "YES",
                                          diffnamespace = diffnamespace)
  # diffnamespace() should return "YES"

  # If you define it in the function, it should give a warning
  expect_warning(
    res1 <- check_and_clean_input(d1 = function(x) { return(sup) },
                             spec_names = taboo)
  )
  expect_equal(res1$kwargs$d1(""), sup)

  expect_silent(
    res2 <- check_and_clean_input(d2 = samenamespace,
                             d3 = diffnamespace,
                             spec_names = taboo)
  )

  expect_equal(sup, "NO")
  expect_equal(res2$kwargs$d2("~"), sup)
  expect_equal(res2$kwargs$d3("~"), "YES")

})

test_that("Explictly package-named functions", {
  # picked a 'random' base function
  acosh <- function(x) { "dummy" }

  expect_warning(
    res1 <- check_and_clean_input(d1 = acosh, spec_names = "acosh")
  )
  # the kwarg has arg_pos attributes
  expect_failure(expect_equal(res1$kwargs$d1, "acosh"))
  expect_equivalent(res1$kwargs$d1, "acosh")

  expect_silent(
    res2 <- check_and_clean_input(d1 = base::acosh, spec_names = "acosh")
  )
  expect_equal(res2$kwargs$d1(10), base::acosh(10))

})

test_that("Function names are not masked", {
  # picked a 'random' base function
  sup <- function(x) { function(y) {return("dummy")} }

  expect_silent(
    res <- check_and_clean_input(d1 = sup(""), spec_names = "sup")
  )
  expect_equal(res$kwargs$d1(""), "dummy")

})

####################################


condition_thrower <- function() {
  warning("1")
  message("A")
  warning("2")
  signal_custom_condition("X","weirdo")
  stop("collaborate and listen")
}


test_that("Equivalences between catching funcs", {
  p <- make_plans(error = c(collect, muffle),
                  misc = c(collect, muffle),
                  warning = c(collect, muffle))
  res1 <- catch_expr(condition_thrower(), p)
  res2 <- make_catch_fn(p)(condition_thrower())
  expect_equal(res1, res2)
})


test_that("Testing collections v1", {
  res <- catch_expr(
    condition_thrower(),
    error = c(collect, muffle),
    misc = c(collect, muffle),
    warning = c(collect, muffle))

  expect_named(res)
  expect_equal(names(res), c("value", "error","misc","warning"))

  lengths <- map_dbl(res, length)
  expect_equivalent(lengths, c(0,1,2,2))
  expect_null(res$value)

  res$value <- NULL

  classes <- map(res, function(x)
    map(x, ~class(.)[[1]]))  %>% unlist(recursive=T,use.names = F)
  expect_equal(classes, c("simpleError", "simpleMessage", "weirdo", "simpleWarning", "simpleWarning"))
})


test_that("Testing misc v2", {
  res <- catch_expr(
    condition_thrower(),
    misc = c(collect, function(x) "YAY", exit),
    warning = c(collect, muffle))

  expect_named(res)
  expect_equal(names(res), c("value", "misc","warning"))

  lengths <- map_dbl(res, length)
  expect_equivalent(lengths, c(1,1,1))
  expect_equal(res$value, "YAY")
})


test_that("Ordered handlers respect order", {
  test_val <- NULL

  expect_silent(res <- with_ordered_handlers(
    warning("woops!"),
    warning = exiting(function(x) "WARNING"),
    condition = calling(function(x) test_val <<- "condition")))

  expect_equal(res, "WARNING")
  expect_equal(test_val, NULL)

})


test_that("Ordered handlers respects order when with_handlers doesn't", {
  test_val <- NULL

  expect_silent(res <- with_handlers(
    warning("woops!"),
    condition = calling(function(x) test_val <<- "condition"),
    warning = exiting(function(x) "WARNING")))

  expect_equal(res, "WARNING")
  expect_equal(test_val, NULL)

  expect_silent(res <- with_ordered_handlers(
    warning("woops!"),
    condition = calling(function(x) test_val <<- "condition"),
    warning = exiting(function(x) "WARNING")))

  expect_equal(test_val, "condition")
  expect_equal(res, "WARNING")

})


