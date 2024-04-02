
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collector

draft for discussion

## Installation

Install with

    pak::pak("cynkra/collector")

## Examples

`collector()` creates a new function, with an altered body but the same
environment:

``` r
library(collector)
add <- function(x, y) {
  x + y
}
environment(add) <- asNamespace("stats")
add2 <- collector(add)
add2
#> function(x, y) {
#>   globals[["add"]]$args <- constructive::construct_reprex()
#>   globals[["add"]]$call <- constructive::deparse_call(sys.call())
#>   on.exit(globals[["add"]]$return_value <- returnValue())
#>   {
#>     x + y
#>   }
#> }
#> <environment: namespace:stats>
```

It behaves the same as the original but additionally it collects :

- The arguments
- The call
- The return value

``` r
a <- 1
b <- 2
add2(a, b)
#> [1] 3

collected("add")
#> $args
#> delayedAssign("x", value = a, eval.env = .GlobalEnv)
#> delayedAssign("y", value = b, eval.env = .GlobalEnv)
#> 
#> $call
#> add2(a, b)
#> 
#> $return_value
#> [1] 3
```

Note the use of `delayedAssign()` because arguments are not evaluated
yet at the start of the body, this is robust to functions that use NSE.

If we know we don’t use NSE, we can use `force = TRUE`

``` r
# if we 
add3 <- collector(add, force = TRUE, name = "custom_name")
add3(a, b)
#> [1] 3
collected("custom_name")
#> $args
#> x <- 1
#> 
#> y <- 2
#> 
#> 
#> $call
#> add3(a, b)
#> 
#> $return_value
#> [1] 3
```

Or we can choose

``` r
# if we 
add4 <- collector(add, force = "x", name = "custom_name2")
add4(a, b)
#> [1] 3
collected("custom_name2")
#> $args
#> x <- 1
#> 
#> delayedAssign("y", value = b, eval.env = .GlobalEnv)
#> 
#> $call
#> add4(a, b)
#> 
#> $return_value
#> [1] 3
```
