# FIXME: set globals through a safe wrapper
#' @export
globals <- new.env()
globals$journal <- list()

#' @export
new_entry <- function(
    #parent_call,
    call,
    args,
    time = Sys.time()
) {
  # FIXME: this assumes calls made of litterals only
  #caller <- constructive::deparse_call(parent_call[[1]])
  # match.call() returns quote(match.call()) if no caller
  # if (caller == "match.call") caller <- NA_character_
  fun <- constructive::deparse_call(call[[1]])
  globals$journal[[length(globals$journal) + 1]] <-
    list(
      time = time,
      #caller = unclass(caller),
      fun = unclass(fun),
      call = list(call),
      args = list(unclass(args))
    )
}

#' @export
collect_value <- function(value) {
  # entry was necessarily created just before
  globals$journal[[length(globals$journal)]]$value <- list(value)
}

#' @export
collector <- function(fun, force = FALSE) {
  body(fun) <- call(
    "{",
    # force args (removed later if irrelevant)
    quote(eapply(environment(), force)),
    # capture call and argument on entrance
    quote(collector::new_entry(
      # sys.call(-1) is brittle with reprexes etc
      #parent_call = eval.parent(quote(match.call())),
      call = sys.call(),
      args = constructive::construct_reprex()$code
    )),
    # capture value on exit
    quote(on.exit(
        collector::collect_value(returnValue(default = structure(NA, error = TRUE)))
        # globals[[.(name)]]$return_value <- returnValue()
      )),
    body(fun)
  )
  # Fixe forcing of args if relevant
  if (isFALSE(force)) body(fun)[[2]] <- NULL
  if (is.character(force)) body(fun)[[2]] <- bquote(lapply(.(force), function(nm, e) force(e[[nm]]), environment()))

  # FIXME: this loses the original formatting and comments of the original source
  # reconstruct the function with constructive to have a nice srcref
  eval.parent(parse(text = constructive::construct(fun)$code, keep.source = TRUE))
}

#' @export
journal <- function() {
  list_of_rows <- lapply(globals$journal, tibble::as_tibble)
  do.call(rbind, list_of_rows)
}

# for some reason this creates a smaller table than it should
# #' @export
# dump_on_finalize <- function(pkg, path) {
#   reg.finalizer(
#     asNamespace(pkg),
#     onexit = TRUE,
#     function(e) {
#       j <- collector::journal()
#       qs::qsave(j, path)
#       # to compare
#       saveRDS(j, "collector_journal.RDS")
#     }
#   )
# }
