# initiate global tibble to store the results
globals <- new.env()
globals$path <- NULL
globals$i <- 0

#' Set functions to use collector
#'
#' After being setup, calls to these functions will trigger a serialization
#' of the calls and necessary environmments.
#'
#' @param funs Names of functions, all exported functions are considered by default
#' @param pkg Package names, can be omitted if called from .onLoad() or if we
#'   can file a DESCRIPTION file in the working directory
#' @param path The path to a directory where to save the collected data
#'
#' For each call A file will be saved, it can be read with `qs::qread()` and
#' contains a list with 2 elements named `call` and `env`.
#'
#' @export
set_collector <- function(funs = NULL, pkg = NULL, path = "collector") {
  globals$path <- path
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  caller <- parent.frame()
  on_load <-
    exists(".onLoad", caller) &&
    identical(sys.function(-1), get(".onLoad", caller))
  if (is.null(pkg)) {
    # default pkg from .onLoad() or DESCRIPTION
    if (on_load) {
      on_load_call <- sys.call(-1)
      pkg <- eval.parent(on_load_call[[3]], 2)
    } else if (file.exists("DESCRIPTION")) {
      pkg <- read.dcf("DESCRIPTION")[, "Package"]
    } else {
      rlang::abort(c(
        "`pkg` could not be infered from `.onLoad()` or a DESCRIPTION file",
        "Provide it explicitly"
      ))
    }
  }
  ns <- asNamespace(pkg)


  replace_funs <- function(...) {
    if (is.null(funs)) {
      # default to all exported functions of the ns
      funs <- Filter(is.function, mget(intersect(getNamespaceExports(ns), names(ns)), ns))
    } else {
      wrong_nms <- Filter(function(x) !exists(x, ns, inherits = FALSE), funs)
      if (length(wrong_nms)) {
        msg <- "All provided functions must exist in the namespace"
        info1 <- paste("Found:", toString(paste0("'", wrong_nms, "'")))
        info2 <- paste(
          "Did you erroneously attempt to patch a re-exported function,",
          "or a generic that is not in the package?"
        )
        rlang::abort(c(msg, x = info1, i = info2))
      }
      funs <- mget(funs, ns)
      wrong_nms <- names(Filter(Negate(is.function), funs))
      if (length(wrong_nms)) {
        msg <- "All provided objects must be functionms"
        info1 <- paste("Found non function object(s):", toString(paste0("'", wrong_nms, "'")))
        rlang::abort(c(msg, x = info1))
      }
    }

    # Replace bodies a call to collect_and_run(), keep original call as attr
    for (nm in names(funs)) {
      val <- ns[[nm]]
      # note: trace() calls it "original" so we picked a new name
      body(val) <- quote(collector::collect_and_run())
      original <- list(ns[[nm]])
      names(original) <- nm
      attr(val, "unmodified") <- original
      utils::assignInNamespace(nm, val, ns)
    }
  }

  # we need exports and they're not qvailable on load so we need a trick
  if (on_load) {
    setHook(
      packageEvent(pkg, "onLoad"),
      replace_funs,
      action = "append"
    )
  } else {
    replace_funs()
  }
}

# * copy caller env and all the chain up to a special env, but every binding is lazy
# * store the call and env in the global tibble
# * on.exit, go through the envs and remove every binding that is still lazy, it's
#   not been used, by reference it will update the global tibble too
# * use the original function to eval the call, and do it in the new env, return
#   this value from the original function call


#' Collect arg and call, and call original function
#'
#' This is not meant to be called by the end user. `set_collector()` will
#' place those calls
#'
#' @return The same output as the original function, in addition to side effects.
#' @export
collect_and_run <- function() {
  # we don't want to collect internal calls to exported functions
  exec_env <- parent.frame()
  caller_env <- parent.frame(2)
  is_called_by_own_ns <- identical(topenv(caller_env), topenv(exec_env))
  call = sys.call(-1)
  original <- attr(sys.function(-1), "unmodified")
  call_to_original <- call
  call_to_original[[1]] <-  original[[1]]
  if (is_called_by_own_ns) {
    return(eval(call_to_original, caller_env))
  }

  new_caller_env <- env_clone_lazy(caller_env)
  on.exit({
    globals$i <- globals$i + 1
    env_cleanup(new_caller_env)
    suppressWarnings(qs::qsave(
      file = sprintf("%s/%.5d-%s.qs", globals$path, globals$i, names(original)),
      list(
        call = call,
        env = new_caller_env,
        value = returnValue()
      )))
  })
  eval(call_to_original, new_caller_env)
}

# note: env_clone doesn't do deep copy, i.e we're not cloning environments that
#   are bound in `e`, or nested in lists, or found in attributes, or as function enclosures.
# We don't take care of active bindings, they'll be serialized with whatever
#   value they have at evaluation time
env_clone_lazy <- function(env) {
  # we stop the recursion when we find a special env, defined by having a name

  if (!env_name(env) %in% c("", "global")) return(env)
  parent_clone <- env_clone_lazy(env_parent(env))
  clone <- rlang::env_clone(env, parent = parent_clone)
  # don't touch dots or bindings that are already lazy
  nms <- names(clone)
  nms <- setdiff(nms[!rlang::env_binding_are_lazy(clone, nms)], "...")
  # this shouldn't be needed, but works around a probable bug in rlang
  # https://github.com/cynkra/collector/issues/2
  rm(list = nms, envir = clone)
  for (nm in nms) {
    env_bind_lazy(clone, !!nm := env[[!!nm]])
  }
  clone
}

# drop lazy bindings, since these were not used
env_cleanup <- function(env) {
  if (!env_name(env) %in% c("", "global")) return(env)
  env_cleanup(env_parent(env))
  lazy_lgl <- env_binding_are_lazy(env)
  rm(list = names(lazy_lgl)[lazy_lgl], envir = env)
}
