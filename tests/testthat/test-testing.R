context("test-testing")


condition_thrower <- function() {
  warning("1")
  message("A")
  warning("2")
  signal_custom_condition("X","weirdo")
  stop("collaborate and listen")
}

test_that("Basic format errors", {
  expect_error(make_plans())
  expect_error(make_plans(condition = NULL))
  expect_error(make_plans(condition = NA_character_))
  expect_error(make_plans(condition, warning = muffle, condition = collect))
})

test_that("trimws testing", {
  message <-"\t test \n\n"
  expect_identical(trimws(message, "left"), "test \n\n")
  expect_identical(trimws(message, "right"), "\t test")
  expect_identical(trimws(message, "both"), "test")
})


test_that("Collecting and raising", {
  opts = catchr_opts(default_plan = c(collect, muffle),
                     drop_empty_conds = FALSE,
                     bare_if_possible = FALSE)
  plans <- make_plans(warning, message, error,
                      .opts = opts)

  res <- catch_expr(condition_thrower(), plans)
  res2 <- catch_expr(dispense_collected(res), plans)
  expect_identical(res, res2)
  expect_warning(dispense_collected(res[c("value", "warning")]))
  expect_message(dispense_collected(res[c("value", "message")]))
  expect_error(dispense_collected(res[c("value", "error")]))

  # Ugh, frickin' capture.output in R 3.1...
  if (as.numeric(paste0(R.Version()$major, ".", floor(as.numeric(R.Version()$minor)))) >= 3.2) {
    output <- capture.output({
      res1.5 <- dispense_collected(res[c("value", "error")], treat_errs = "display")},
      type="message")
    expect_equivalent("Error: collaborate and listen", output)
    expect_null(res1.5)
  }

  res[["value"]] <- "good"
  expect_warning(
    expect_identical(
      dispense_collected(res[c("value", "error")], treat_errs = "warn"), "good"))

  res3 <- catch_expr("no conditions", plans)
  expect_identical(res3$value, dispense_collected(res3))
  expect_identical(res3$value, dispense_collected(res3$value))

})


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




test_that("No collection = no sublists when bare_if_possible", {
  warner <- function() {
    warn("Suppress this!")
    "done!"
  }
  plans <- make_plans(warning = muffle, .opts=catchr_opts(bare_if_possible=TRUE))

  expect_silent(results <- catch_expr(warner(), plans))
  expect_named(results, expected = NULL)

})


test_that("Equivalences between catching funcs", {
  p <- make_plans(error = c(collect, muffle),
                  misc = c(collect, muffle),
                  warning = c(collect, muffle))
  res1 <- catch_expr(condition_thrower(), p)
  res2 <- make_catch_fn(p)(condition_thrower())
  expect_equal(res1, res2)
})


test_that("Beeping breaks when not installed", {
  if (!is_installed("beepr")) {
    expect_error(make_plans(warning = beep))
    expect_error(make_plans(message = muffle, misc = c(beep)))
    expect_error(make_plans(misc = c(beep_with(1))))
  } else {
    expect_silent(make_plans(warning = beep))
    expect_silent(make_plans(message = muffle, misc = c(beep)))
    expect_silent(make_plans(misc = c(beep_with(1))))
  }
})

test_that("Basic display testing", {
  make_warnings <- function() {
    warning("A")
    warning("B", call.=FALSE)
    NULL
  }
  expect_output(catch_expr(make_warnings(), make_plans(warning = c(display, muffle))))

  expect_warning(output1 <- capture.output(
    catch_expr(make_warnings(),
               warning = display)
  ))
  expect_silent(output1 <- capture.output(
    catch_expr(make_warnings(),
               warning = c(display, muffle))
  ))
  expect_length(output1, 3)
  expect_identical(grepl("make_warnings", output1), c(TRUE, FALSE, FALSE))

  expect_silent(output2 <- capture.output(
    catch_expr(make_warnings(),
               warning = c(display_with("red", cond_name="OOO", include_call = FALSE), muffle))
  ))
  expect_identical(grepl("OOO",output2), c(TRUE, TRUE, FALSE))
  expect_identical(grepl("make_warnings",output2), c(FALSE, FALSE, FALSE))

  expect_silent(output3 <- capture.output(
    catch_expr(make_warnings(),
               warning = c(display_with(NULL, cond_name=NULL, include_call = FALSE), muffle))
  ))
  expect_identical(nchar(output3[[1]]), 1L)
  expect_identical(nchar(output3[[2]]), 1L)

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


test_that("Testing misc", {
  expect_error(make_plans(condition, warning = muffle, misc = collect))
  expect_error(make_plans(warning = muffle, misc = collect, condition = collect))

  res <- catch_expr(
    condition_thrower(),
    misc = c(collect, exit_with("YAY"), exit),
    warning = c(collect, muffle))

  expect_named(res)
  expect_equal(names(res), c("value", "misc", "warning"))

  lengths <- map_dbl(res, length)
  expect_equivalent(lengths, c(1,1,1))
  expect_equal(res$value, "YAY")
})

test_that("user_exit/user_display need to be IN a function", {
  cond <- catch_cnd(warning("internal"))

  expect_warning(make_plans(
    misc = c(collect, user_exit("YAY"), muffle),
    warning =  muffle))
  expect_warning(expect_error(make_plans(
    warning =  muffle,
    error = user_exit("YAY"))))

  expect_warning(make_plans(
    misc = c(collect, user_display(cond, "red"), muffle),
    warning =  muffle))
  expect_warning(expect_error(make_plans(
    warning =  muffle,
    error = user_display(cond, "red"))))

  expect_silent(make_plans(
    warning =  muffle,
    error = user_exit))
  expect_silent(make_plans(
    warning =  list(muffle, user_exit),
    error = user_exit))

  expect_silent(make_plans(
    warning =  muffle,
    error = user_display))
  expect_silent(make_plans(
    warning =  list(muffle, user_display),
    error = user_display))
})

#############################

test_that("Testing getting and setting default options", {

  current_default_plan <-     getOption("catchr.default_plan")
  current_warn_about_terms <- getOption("catchr.warn_about_terms")
  current_bare_if_possible <- getOption("catchr.bare_if_possible")
  current_drop_empty_conds <- getOption("catchr.drop_empty_conds")

  on.exit(options(
    "catchr.default_plan" = current_default_plan,
    "catchr.warn_about_terms" = current_warn_about_terms,
    "catchr.bare_if_possible" = current_bare_if_possible,
    "catchr.drop_empty_conds" = current_drop_empty_conds
  ))

  expect_null(catchr_default_opts())
  expect_silent(catchr_default_opts(warn_about_terms = TRUE))
  expect_silent(catchr_default_opts(default_plan = muffle,
                                    drop_empty_conds = FALSE,
                                    bare_if_possible = T))

  what_im_testing <- catchr_default_opts(warn_about_terms, catchr.drop_empty_conds,
                                         default_plan, "catchr.bare_if_possible")
  expect_equal(sort(add_catchr_prefix(what_im_testing)),
               sort(names(catchr_original_default_values)))

  expect_equivalent(
    what_im_testing,
    list(getOption("catchr.warn_about_terms"),
         getOption("catchr.drop_empty_conds"),
         getOption("catchr.default_plan"),
         getOption("catchr.bare_if_possible"))
  )

  # Should all be the same
  expect_identical(get_default_plan(), "muffle")
  expect_identical(catchr_default_opts(default_plan), "muffle")
  expect_identical(catchr_default_opts("default_plan"), "muffle")

  expect_identical(catchr_default_opts(default_plan, "warn_about_terms"),
                   list("default_plan" = "muffle", "warn_about_terms" = TRUE))
  expect_identical(catchr_default_opts(catchr.default_plan, "warn_about_terms"),
                   list("catchr.default_plan" = "muffle", "warn_about_terms" = TRUE))

  expect_identical(catchr_default_opts("bare_if_possible", default_plan),
                   list("bare_if_possible" = TRUE, "default_plan" = "muffle"))

  expect_identical(catchr_default_opts("bare_if_possible"), TRUE)
  expect_identical(catchr_default_opts(default_plan,  bare_if_possible, warn_about_terms = FALSE),
                   list("default_plan" = "muffle", "bare_if_possible" = TRUE))
  expect_identical(catchr_default_opts("warn_about_terms"), FALSE)

})


test_that("Testing warnings and errors for getting and setting default options", {

  current_default_plan <-     getOption("catchr.default_plan")
  current_warn_about_terms <- getOption("catchr.warn_about_terms")
  current_bare_if_possible <- getOption("catchr.bare_if_possible")
  current_drop_empty_conds <- getOption("catchr.drop_empty_conds")

  on.exit(options(
    "catchr.default_plan" = current_default_plan,
    "catchr.warn_about_terms" = current_warn_about_terms,
    "catchr.bare_if_possible" = current_bare_if_possible,
    "catchr.drop_empty_conds" = current_drop_empty_conds
  ))

  expect_error(catchr_default_opts("BABA"))
  expect_error(catchr_default_opts(list()))
  f <- function(x) x
  expect_error(catchr_default_opts(f("warn_about_terms")))
  expect_silent(catchr_default_opts(warn_about_terms = f(FALSE)))
  expect_identical(catchr_default_opts("warn_about_terms"), FALSE)
  expect_error(catchr_default_opts(bare_if_possible = 2))

  expect_error(set_default_plan(~a))
  expect_error(set_default_plan(1))
  expect_error(set_default_plan(TRUE))
  expect_error(catchr_default_opts(default_plan = ~a))
  expect_error(catchr_default_opts(default_plan = TRUE))

  catchr_default_opts(catchr.default_plan = display)
  expect_error(catchr_default_opts(default_plan = raise, mojo = TRUE))
  expect_identical(catchr_default_opts(default_plan), "display")
  expect_identical(catchr_default_opts(catchr.default_plan), "display")

  catchr_default_opts(drop_empty_conds = T)
  expect_error(catchr_default_opts(drop_empty_conds = F,
                                   warn_about_terms,
                                   catchr.drop_empty_conds = F))
  expect_identical(catchr_default_opts(drop_empty_conds), TRUE)

  expect_error(catchr_default_opts(default_plan = muffle,
                                   warn_about_terms,
                                   default_plan = collect))

  expect_error(catchr_default_opts(default_plan = collect,
                                   default_plan = TRUE))
  expect_error(catchr_default_opts(default_plan = collect,
                                   catchr.default_plan = TRUE))
  expect_error(catchr_default_opts(catchr.default_plan = collect,
                                   default_plan = TRUE))
})


test_that("Testing restoring default options", {
  current_default_plan <-     getOption("catchr.default_plan")
  current_warn_about_terms <- getOption("catchr.warn_about_terms")
  current_bare_if_possible <- getOption("catchr.bare_if_possible")
  current_drop_empty_conds <- getOption("catchr.drop_empty_conds")

  on.exit(options(
    "catchr.default_plan" = current_default_plan,
    "catchr.warn_about_terms" = current_warn_about_terms,
    "catchr.bare_if_possible" = current_bare_if_possible,
    "catchr.drop_empty_conds" = current_drop_empty_conds
  ))

  go_back <- function(x) catchr_default_opts(default_plan = beep,
                                             warn_about_terms = F,
                                             drop_empty_conds = T,
                                             bare_if_possible = F)
  expect_silent(go_back())
  expect_failure(expect_identical(
    catchr_original_default_values[sort(names(catchr_original_default_values))],
    catchr_default_opts(!!!sort(names(catchr_original_default_values)))))

  expect_silent(res <- restore_catchr_defaults())
  expect_null(res)
  expect_identical(catchr_original_default_values[sort(names(catchr_original_default_values))],
                   catchr_default_opts(!!!sort(names(catchr_original_default_values))))

  expect_error(restore_catchr_defaults(default_plan=muffle))
  expect_error(restore_catchr_defaults(default_planzzzz))

  expect_silent(go_back())
  expect_silent(restore_catchr_defaults(warn_about_terms, catchr.drop_empty_conds))
  expect_failure(expect_identical(
    catchr_original_default_values[sort(names(catchr_original_default_values))],
    catchr_default_opts(!!!sort(names(catchr_original_default_values)))))
  expect_identical(
    catchr_original_default_values[c("catchr.warn_about_terms", "catchr.drop_empty_conds")],
    catchr_default_opts("catchr.warn_about_terms", "catchr.drop_empty_conds"))

  expect_silent(go_back())
  expect_failure(expect_identical(
    catchr_original_default_values[c("catchr.warn_about_terms", "catchr.drop_empty_conds")],
    catchr_default_opts("catchr.warn_about_terms", "catchr.drop_empty_conds")))
  expect_silent(restore_catchr_defaults("catchr.drop_empty_conds", warn_about_terms))
  expect_identical(
    catchr_original_default_values[c("catchr.warn_about_terms", "catchr.drop_empty_conds")],
    catchr_default_opts("catchr.warn_about_terms", "catchr.drop_empty_conds"))
})


test_that("Testing basic compiled plan printing", {
  expect_silent(
    test_plans <- make_plans(
      warning,
      error=c("muffle"),
      message=list(beep, function(x) {print(paste0(x, "THIS IS A VERY LONG STRING AND I THINK IT WILL GET CUT OFF")); stop(x)}),
      .opts = catchr_opts(
        default_plan = c(display, muffle),
        warn_about_terms = FALSE,
        bare_if_possible = TRUE,
        drop_empty_conds = TRUE))
  )

  test_order_and_existence <- function(to_print, l, ...) {
    expect_silent(output1 <- capture.output(print(to_print, ...)))

    for (i in 1:(length(l)-1)) {

      expect_true(
        which(grepl(l[[i]], output1, fixed=TRUE)) <
          which(grepl(l[[i+1]], output1, fixed=TRUE)),
        label= paste0("'", l[[i]], "' not before '", l[[i+1]],
                      "' or either one is missing from: ",
                      paste(output1, collapse="\n"))
      )
    }
    output1
  }

  o1 <- test_order_and_existence(
    test_plans,
    c("warning:","error:","message:","to see the default plan"),
    total_len = 30)
  expect_true(any(grepl("<default_plan>*", o1, fixed=TRUE)))

  o2 <- test_order_and_existence(
    test_plans,
    c("warning:","error:","message:","catchr options:",
      "warn_about_terms: FALSE", "default_plan:"),
    show_opts = TRUE)

  expect_true(!any(grepl("to see the default plan", o2, fixed=TRUE)))
  expect_true(grepl("default_plan:.*display.*muffle", o2[length(o2)]))

  o3 <- test_order_and_existence(
    test_plans,
    c("warning:","error:","message:","to see the default plan"),
    total_len = 190)
  expect_true(any(grepl("WILL GET CUT OFF", o3, fixed=TRUE)))

  expect_identical(
    capture.output(  print(test_plans, show_opts = TRUE, show_full = TRUE)),
    capture.output(summary(test_plans))
  )

  o4 <- capture.output(make_plans(warning="muffle"))
  expect_true(!any(grepl("default_plan", o4)))

})


