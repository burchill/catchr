# catchr 0.2.1

## Changes

Removed `with_ordered_handlers` since it was essentially a gimmick anyway, and because it doesn't feel worth it to keep up with whatever rlang's current idiomatic handler framework is at any given moment.

## Patches

Fixed some breaking changes from `rlang` updating to version `0.4.0`, primarily with `rlang::fn_fmls()` and `rlang::with_handlers()`. `catchr::with_ordered_handlers()` should be viewed suspiciously, as it hasn't been vetted with the new `rlang::with_handlers` code.

Unfortunately, since `rlang v0.4.0` has stopped supporting versions of R below `v3.2.0`, so has `catchr`. To use `catchr` with older versions of R, use `catchr v0.2.0` with `rlang v0.3.1`.

## Bug fixes

 * `summary` now works correctly on `catchr_compiled_plans`, rather than throwing an error.
 
# catchr 0.2.0

## Major breaking changes: everything

This version of `catchr` is essentially a new package---consider all previous functions to be hard-deprecated, and there to be zero backward-compatibility. These huge, package-breaking changes were made while `catchr` was very young and had few (if any) users. Moving forward, there will be a (normal) emphasis on backward-compatibility, and such drastic changes are unlikely to ever happen again. The version number was only incremented by `0.1.0` to avoid the implication that this package is feature complete with a stable API.

Instead of listing *everything* in the package, check out the introductory vignette and help documentation for new features.

## catchr's philosophy

R has a unique way of dealing with warnings, errors, messages, and other conditions, but it can often be troublesome to users coming from different backgrounds. Starting from the idea that it should be more flexible and generative, the code now lets users generate their own "catching" functions where they can specify the behavior via conceptual "plans".  

In order to lower the barrier of entry, keep code clean and readable, and reduce the amount of typing required, `catchr` now uses a very simple domain-specific language that simplifies things on the front-end. `catchr`'s aim is to maintain a continuous learning curve that lets new users jump straight in to condition-handling, while simultaneously offering depth and complexity for more advanced users.

## Now with dependencies

`catchr` now depends on a few packages. Much of the functions in `catchr v0.1.0` were redundant with functions from `rlang`; now `catchr` leans heavily on this package to bring simplicity to users less familiar with R's condition-handling idiosyncracies. For convenience, it uses a few functions from `purrr` as well.  It also suggests `beepr` and `crayon` for extra functionality in the auditory and visual domains, respectively.

# catchr 0.1.0

* Original GitHub release of `catchr` on 2018-12-17. 

