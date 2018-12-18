
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.com/burchill/catchr.svg?branch=master)](https://travis-ci.com/burchill/catchr)

# catchr

The goal of `catchr` is to provide a simple code base for handling conditions (e.g., warnings, errors, messages, etc.) in `R`. Other than the simplicity it offers, one of the primary benefits of `catchr` is that it can catch and collect conditions **without needing to halt or restart the code**. This can be especially useful if you're trying to catch the warning messages from code that takes a long time to run, where having to restart the whole process from square one would be too costly.

`catchr` also comes in handy when trying to collect warnings, messages, and errors from code that is running remotely, where these conditions would not be returned with the rest of the results, such as with the [`future`](https://github.com/HenrikBengtsson/future/) package, or with packages like `purrr`.

## Installation

You can install the released version of furrr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("catchr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("burchill/catchr")
```

## Basic example

Let's look at a simple case first. Here we see how we we can collect different types of conditions.

``` r
library(catchr)
res <- collect_conditions(
  {
    warning("warning 1")
    message("message 1", appendLF = F)
    warning("warning 2")
    signal_custom_condition("Custom condition!")
    "test value"
  },
  catchErrors = FALSE)
print(res)
#> $value
#> [1] "test value"
#> 
#> $warnings
#> $warnings[[1]]
#> <simpleWarning in doWithOneRestart(return(expr), restart): warning 1>
#> 
#> $warnings[[2]]
#> <simpleWarning in doWithOneRestart(return(expr), restart): warning 2>
#> 
#> 
#> $messages
#> $messages[[1]]
#> <simpleMessage in message("message 1", appendLF = F): message 1>
#> 
#> 
#> $conditions
#> $conditions[[1]]
#> <custom: Custom condition!>

# `col_cond` is the shorter alias of `collect_conditions`
col_cond(stop("Error!"), asStrings = T)$errors
#> [[1]]
#> [1] "Error in doWithOneRestart(return(expr), restart): Error!\n"
```

We can then decide to raise and signal all the conditions we've captured, while making the output pretty and easy to read. In the actual console (as opposed to this Markdown file), you won't get the `Warning in doWithOneRestart(return(expr), restart)` text.

``` r
raise_conditions(res)
#> Messages:
#> message 1
#> Warnings:
#> Warning in doWithOneRestart(return(expr), restart): warning 1
#> Warning in doWithOneRestart(return(expr), restart): warning 2
#> Misc. conditions:
#> Custom condition!
#> $value
#> [1] "test value"
#> 
#> $warnings
#> $warnings[[1]]
#> <simpleWarning in doWithOneRestart(return(expr), restart): warning 1>
#> 
#> $warnings[[2]]
#> <simpleWarning in doWithOneRestart(return(expr), restart): warning 2>
#> 
#> 
#> $messages
#> $messages[[1]]
#> <simpleMessage in message("message 1", appendLF = F): message 1>
#> 
#> 
#> $conditions
#> $conditions[[1]]
#> <custom: Custom condition!>
```

## Use in `future`

`catchr` can be incredibly useful when trying to diagnose code run in parallel or on remote machines, like it is with `future`. Although `future` has come a long way in terms of how easy it is to debug (because [Henrik Bengtsson](https://github.com/HenrikBengtsson) is both a saint and a genius), but capturing and returning every condition that was raised is much easier with `catchr`.

``` r
library(future)
plan(multiprocess) # you could use `remote` or whatever you need
future_res %<-% {
  col_cond(
    {
      warning("You'll get an error because of X")
      stop("Why did you get this error?")
      "terminates before this value"
    }
  )
}

raise_conditions(future_res, raise_errors = FALSE)
#> Warnings:
#> Warning in doWithOneRestart(return(expr), restart): You'll get an error
#> because of X
#> Errors:
#> Why did you get this error?
#> $value
#> NULL
#> 
#> $warnings
#> $warnings[[1]]
#> <simpleWarning in doWithOneRestart(return(expr), restart): You'll get an error because of X>
#> 
#> 
#> $messages
#> list()
#> 
#> $errors
#> $errors[[1]]
#> <simpleError in doWithOneRestart(return(expr), restart): Why did you get this error?>
```

## Use in `purrr`

`catchr` is also great with `purrr`, for example, if you're running a bunch of models via `map`.[1] If you want to capture which models had which problems (and then print them all pretty), it's trivial to do so.

``` r
library(purrr)
# Let's assume `l` came from `col_cond`-ing a bunch of models
#   e.g., `map(datasets, ~col_cond(model_func(.)))`
results <- l %>% imap(function(e, i)
  raise_conditions(e, raise_errors=FALSE,
                   added_text=paste0(" in l[",i,"]:"))) %>%
  map(~evaluate_results(., raise_errors = FALSE))
#> Warnings in l[1]:
#> Warning in doWithOneRestart(return(expr), restart): Bad eigenvalues, bro
#> Warning in doWithOneRestart(return(expr), restart): Convergence failure!
#> Messages in l[2]:
#> Dropping contrasts
#> Warnings in l[2]:
#> Warning in doWithOneRestart(return(expr), restart): Were those contrasts
#> important?
#> Messages in l[3]:
#> I'm tired of this data!
#> Errors in l[3]:
#> lmers became sentient!

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

Please open an issue and I'll try to get to it!

### Acknowledgments

The inception of the core of the `collect_conditions` function comes from [Luke Tierney's answer to a question on the R-help mailing list](https://tolstoy.newcastle.edu.au/R/help/04/06/0217.html), which was very helpful in my understanding of `R`'s error-handling.

Likewise, much of this code came from other (more personal) packages I've worked on over the years, such as [`zplyr`](https://github.com/burchill/zplyr) and [`cs`](https://github.com/burchill/cs).

### Footnotes

[1] I've found it's even *more* useful when you combine `purrr` and `future` via [`furrr`](https://github.com/DavisVaughan/furrr) (e.g., to run models in parallel). Shout-out to Davis Vaughan for his lovely code!
