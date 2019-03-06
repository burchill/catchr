#' View and print 'compiled' catchr plans
#'
#' 'Compiled' catchr plans returned by [make_plans()] look very ugly "naked". These functions make plans understandable at a single glance.
#'
#' @param x The "compiled" plans, i.e., from `make_plans()`
#' @param \dots Currently unused.
#' @param show_opts A logical; if `TRUE`, prints the catchr options set for the plans.
#' @param total_len An integer; sets the total number of characters each line can span before being cut off with "..."
#' @param show_full A logical; if `TRUE`, will print out the full length of each line.
#' @rdname view-plans
#' @export
print.catchr_compiled_plans <- function(x, ...,
                                        show_opts = FALSE,
                                        total_len = getOption("width"),
                                        show_full = FALSE) {
  header <- "<catchr_compiled_plans>"
  footer <- "catchr options: "

  og_qs <- as.list(attr(x, "calls", exact = TRUE))
  is_def_plan <- names2(og_qs) == ""
  if (show_opts == FALSE) {
    if (any(map_lgl(is_def_plan, is_true)))
      footer <- "  * to see the default plan, use `summary()`"
    else
      footer <- "  to see catchr options, use `summary()`"
  }

  cond_headers <- ifelse(is_def_plan, as.character(get_expr(og_qs)), names2(og_qs))
  max_nchar <- max(nchar(cond_headers))
  padding <- get_padding(cond_headers, s=" ")

  if (show_full)
    total_len <- Inf

  og_qs[!is_def_plan] <- map_chr(og_qs[!is_def_plan], ~approx_arg_name(!!., total_len-max_nchar - 2))
  og_qs[is_def_plan] <- "<default_plan>"
  if (!show_opts)
    og_qs[is_def_plan] <- paste0(og_qs[is_def_plan], "*")

  # ----------- Make the options -------------------------------------$
  if (show_opts) {
    opts <- attr(x, "catchr_opts", exact = TRUE)
    opts[options_to_hide] <- NULL
    opts <- append(opts[names(opts) != "default_plan"], opts["default_plan"])
    opt_names <- names2(opts)

    max_opt_nchar <- max(nchar(opt_names))
    opts_padding <- get_padding(opt_names)
    
    opts <- paste0(opts) %>%
      map_chr( ~add_ellipses(., total_len - max_opt_nchar - 4))
    
    # end_padding <- total_len - max_opt_nchar - nchar(opts) - 4
    # end_padding <- map_chr(end_padding, ~join(rep(" ", .)))
  }

  if (is_installed("crayon")) {
    header <- crayon::bold(header)
    og_qs <- crayon::make_style("grey40")(og_qs)
    og_qs[is_def_plan] <- crayon::italic(og_qs[is_def_plan])
    footer <- crayon::make_style("grey50")(footer)

    if (show_opts) {
      opts <- crayon::make_style("grey40")(opts)
      opt_names <- crayon::underline(opt_names)
    }
  }

  opts_string <- NULL
  if (show_opts)
    opts_string <- paste0(" ", opt_names, ": ", opts_padding, opts, collapse = "\n")

  paste(header,
        paste0(cond_headers, ": ", padding, og_qs, collapse="\n"),
        footer, opts_string, sep="\n") %>%
    cat()

  invisible(x)
}

#' @param object The "compiled" plans, i.e., from `make_plans()`
#' @rdname view-plans
#' @export
summary.catchr_compiled_plans <- function(object, ...) {
  print(object, show_opts = TRUE, show_full = TRUE)
  invisible(object)
}



# Internal
join <- function(x, s="") {
  paste0(x, collapse=s)
}
get_padding <- function(x, s=" ") {
  max_nchar <- max(nchar(x))
  padding <- max_nchar - nchar(x)
  padding <- map_chr(padding, ~join(rep(" ", .)))
}
add_ellipses <- function(x, len) {
  if (nchar(x) > len) paste0(substr(x,1,len-3), "...")
  else x
}


# Base R code for v 3.1 support
trimws <- function (x, which = c("both", "left", "right")) {
  which <- match.arg(which)
  mysub <- function(re, x) sub(re, "", x, perl = TRUE)
  if (which == "left")
    return(mysub("^[ \t\r\n]+", x))
  if (which == "right")
    return(mysub("[ \t\r\n]+$", x))
  mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
}
