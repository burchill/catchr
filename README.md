
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.com/burchill/catchr.svg?branch=master)](https://travis-ci.com/burchill/catchr)
[![CRAN
status](https://www.r-pkg.org/badges/version/catchr)](https://cran.r-project.org/package=catchr)

# Catchr: the friendlier way of catching errors, warnings, and conditions

## *“Exceptions?” “Handlers?”* Making sense of conditions

Compared to many other programming languages, the way R handles
‘conditions’—errors, warnings, messages, and most things referred to
as ‘exceptions’ in other languages—is pretty unintuitive, and can often
be troublesome to users coming from different backgrounds. For example,
on the surface the way exceptions are caught in Python seems so simple
compared to R—*what even **is** a “restart”? What are those things
people are referring to as “handlers” anyway?*

The purpose of catchr is to provide flexible, useful tools for handling
R conditions with less hassle and head-scratching. One of the most
important goals of this package is to maintain a continuous learning
curve that so new users jump straight in, but more advanced users can
find the depth and complexity they need to take advantage of R’s
powerful condition-handling abilities.

To lower the barrier of entry, keep code clean and readable, and reduce
the amount of typing required, catchr uses a very simple domain-specific
language that simplifies things on the front-end. catchr focuses on
letting users build their own “catching” functions where they can
specify behavior via conceptual “plans”, removing unnecessary
complexities—like the distinction between “calling” vs. “exiting”
handlers—and adding many useful features, like the ability to “collect”
the conditions raised from a call.

## Installation

You can install the released version of catchr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("catchr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("burchill/catchr")
```

## Introduction

In R, “warnings” (which generally indicate something *might* be going
wrong), “errors” (which indicate something *definitely* has gone wrong),
and “messages” (which generally just indicate neutral information) are
all subclasses of “conditions”, and these three types make up a vast
majoirty of the conditions you will ever encounter if you’re not a
developer.

When a condition is “raised”, the code essentially stops, and the
condition floats up through the code until something “catches” it or
not. If nothing catches it and deals with it, warnings, messages, and
errors will print a message out on your screen. Then, unless the
condition was an error, the code picks up where it left off.

Phrased as such, conditions may seem like no big deal. And for many
basic uses of R, maybe they’re not; if you’re just tidying up some data
and making a plot out of it, you can react to warnings and errors as
they come, with little cost. But for more involved R projects, being
able to deal with conditions programmatically becomes *indispensable*.

## A basic example

A (somewhat sassy) introduction to catchr can be found in the vignettes
(`vignette("welcome-to-catchr","catchr")` if you’ve installed it). Here,
we’ll just cover some cases to demonstrate what the code looks like, and
some of the advantages it offers.

Let’s look at a *very* simple case first. As you may know, trying to
take the log of a negative number raises a warning and returns a `NaN`.
There are times where it would be important *not* to encounter a `NaN`,
and maybe you want the code to stop whenever a warning of any kind is
raised.

``` r
library(catchr)

fake_model <- function(x, err = F) {
  y <- log(x)
  if (err) stop("Uh oh!")
  c(y, x+1) 
}
# Works fine
fine_results <- catch_expr(fake_model(5), warning = toerror)
```

But when a `NaN` is made and a warning is raised, catchr converts the
warning into an error and the code stops:

``` r
# Stops the code
bad_results <- catch_expr(fake_model(-7), warning = toerror)
```

But let’s say you want to be alerted about this issue as soon as
possible, and you’re working on something else in a different window
while the code runs. You can have catchr play a beeping sound whenever
this event happens with a simple addition:

``` r
# Stops the code and make a beeping sound
bad_results <- catch_expr(fake_model(-7), warning = c(beep, toerror))
```

catchr is designed so that making “plans” for a condition is simple,
extendable, and flexible. In the example above, we made a “plan” for
conditions of the class “warning” so that when one is raised, first a
beep is played and then the warning is converted to an error.

## catchr “plans”

Instead of using R’s “calling”/“exiting” “handler” terminology, catchr
keeps things simple with a single concept, “plans”. In catchr, users use
functions like building blocks to a “plan” of what to do for particular
conditions. Users can specify their own functions or use catchr
functions, but `catcher` also offers a useful toolbox of behaviors that
work their magic behind the scene through catchr’s simple
[domain-specific language](http://adv-r.had.co.nz/dsl.html).\[1\]

This toolbox consists of special “reserved” terms that users can input
as strings or unquoted terms, and cover some of the most common
behaviors users might want to
use:

| Special “reserved” term             | Function                                                                       |
| ----------------------------------- | ------------------------------------------------------------------------------ |
| `tomessage`, `towarning`, `toerror` | convert conditions to other types of conditions                                |
| `beep`                              | play a short sound                                                             |
| `display`                           | displays the contents of the condition on-screen                               |
| `collect`                           | collects the condition and saves it to a list that will be returned at the end |
| `muffle`                            | “muffles”,\[2\] a condition so it doesn’t keep going up, and restarts the code |
| `exit`                              | immediately stops the code and muffles the condition                           |
| `raise`                             | raises conditions past `exit`                                                  |

These can be used as building blocks just like normal functions. For
example, in the previous example, we saw how `beep` and `toerror` were
strung together to make a plan.

## Reusability

catchr is all about keeping code minimal, and is built around
reusability. You can make and reuse plans across
expressions:

``` r
# Since all conditions have class "condition", this is a plan for all conditions
plans <- make_plans(condition = c(collect, muffle))
plans
#> <catchr_compiled_plans>
#> condition: c(collect, muffle)
#>   to see catchr options, use `summary()`
```

``` r
res1 <- catch_expr(fake_model(-4.0, err = F), plans)
res1
#> $value
#> [1] NaN  -3
#> 
#> $condition
#> $condition[[1]]
#> <simpleWarning in log(x): NaNs produced>
```

``` r
res2 <- catch_expr(fake_model(-3.9, err = T), plans)
res2
#> $value
#> NULL
#> 
#> $condition
#> $condition[[1]]
#> <simpleWarning in log(x): NaNs produced>
#> 
#> $condition[[2]]
#> <simpleError in fake_model(-3.9, err = T): Uh oh!>
```

And even more importantly, you can create your *own* functions that you
can use to catch conditions for *any* code:

``` r
collect_and_muffle <- make_catch_fn(plans)

res1 <- collect_and_muffle(fake_model(-4.0, err = F))
res2 <- collect_and_muffle(fake_model(-3.9, err = T))
```

## “Collecting” conditions

One of the most useful things about catchr is its ability to catch and
store any conditions raised during evaluation with the `collect` term,
returning the conditions after the code is finished without restarting
the evaluation from scratch.

There are a number of situations in which this can be immensely handy.
For example, if you’re trying to catch warning messages from code that
takes a long time to run, where having to restart the whole process from
the beginning would be too costly.

### With `future`

catchr can be incredibly useful when trying to diagnose code run in
parallel or on remote machines, like it is with `future`. Although
`future` has come a long way in terms of how easy it is to debug
(because [Henrik Bengtsson](https://github.com/HenrikBengtsson) is both
a saint and a genius), but capturing and returning every condition that
was raised is easy with catchr.

``` r
library(future)
future::plan(multiprocess) # you could use `remote` or whatever you need

future_res %<-% collect_and_muffle(fake_model(-99, err = TRUE))
future_res
#> $value
#> NULL
#> 
#> $condition
#> $condition[[1]]
#> <simpleWarning in log(x): NaNs produced>
#> 
#> $condition[[2]]
#> <simpleError in fake_model(-99, err = TRUE): Uh oh!>
```

``` r
# If you wanted to raise all the conditions on your local machine and return the value of the evaluated expression:
result <- dispense_collected(future_res)
```

### With `purrr`

Collecting conditions is also great with `purrr` or scenarios where you
want to apply functions programmatically—for example, if you’re running
a bunch of models via `map`.\[3\] If you want to capture which models
had which problems (and then print them all pretty), it’s trivial to do
so.

``` r
library(purrr)
# Let's assume `l` came from running a bunch of models,
#   e.g., `map(datasets, ~collect_and_muffle(model_func(.)))`
results <- l %>% imap(function(e, i) {
  cat("\n")
  cat("in l[[",i,"]]:\n", sep = "")
  dispense_collected(e, treat_errs="display") })
#> 
#> in l[[1]]:
#> Warning: Bad eigenvalues, bro
#> Warning: Convergence failure!
#> 
#> in l[[2]]:
#> Dropping contrasts
#> Warning: Were those contrasts important?
#> 
#> in l[[3]]:
#> I'm tired of this data!
```

``` r
print(results)
#> [[1]]
#> [1] "model-1"
#> 
#> [[2]]
#> [1] "model-2"
#> 
#> [[3]]
#> NULL
```

## Found a bug or have a suggestion?

Please open an issue and I’ll try to get to it\!

## Footnotes

1.  See `help("catchr-DSL", "catchr")` for the details.

2.  i.e., “suppresses”, “catches”, “hides”—whatever you want to call it

3.  I’ve found it’s even *more* useful when you combine `purrr` and
    `future` via [`furrr`](https://github.com/DavisVaughan/furrr) (e.g.,
    to run models in parallel). Shout-out to Davis Vaughan for his
    lovely code\!
