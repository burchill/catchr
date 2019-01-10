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
