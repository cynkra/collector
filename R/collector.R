# FIXME: set globals through a safe wrapper
#' @export
globals <- new.env()

#' @export
collector <- function(fun, name = deparse1(substitute(fun)), force = FALSE) {
  body(fun) <- call(
    "{",
    # force args (removed later if irrelevant)
    quote(eapply(environment(), force)),
    # capture call and argument on entrance
    bquote(
      globals[[.(name)]] <- list(
        call = constructive::deparse_call(sys.call()),
        args = constructive::construct_reprex()
        )
    ),
    # capture value on exit
    bquote(
      on.exit(
        globals[[.(name)]]$return_value <- returnValue()
      )
    ),
    body(fun)
  )
  if (isFALSE(force)) body(fun)[[2]] <- NULL
  if (is.character(force)) body(fun)[[2]] <- bquote(lapply(.(force), function(nm, e) force(e[[nm]]), environment()))
  # reconstruct the function with constructive to have a nice srcref
  eval.parent(parse(text = constructive::construct(fun)$code, keep.source = TRUE))
}

#' @export
collected <- function(name) {
  globals[[name]]
}
