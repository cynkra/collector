
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
#>   collector::new_entry(parent_call = eval.parent(quote(match.call())), call = sys.call(), args = constructive::construct_reprex()$code)
#>   on.exit(collector::collect_value(returnValue(default = structure(NA, error = TRUE))))
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
f <- function() add2(a, b)
f()
#> [1] 3
```

Note the use of `delayedAssign()` because arguments are not evaluated
yet at the start of the body, this is robust to functions that use NSE.

If we know we don’t use NSE, we can use `force = TRUE`

``` r
# if we 
add3 <- collector(add, force = TRUE)
add3(a, b)
#> [1] 3
```

Or we can choose

``` r
# if we 
add4 <- collector(add, force = "x")
add4(a, b)
#> [1] 3
```

Results are collected in a list internally, which we can collect with
the `journal()` function :

``` r
j <- journal()
j
#> # A tibble: 3 × 6
#>   time                caller fun   call       args      value
#>   <dttm>              <chr>  <chr> <list>     <list>    <dbl>
#> 1 2024-04-03 15:35:23 f      add2  <language> <chr [2]>     3
#> 2 2024-04-03 15:35:23 <NA>   add3  <language> <chr [4]>     3
#> 3 2024-04-03 15:35:23 <NA>   add4  <language> <chr [3]>     3

as.list(j[1,])
#> $time
#> [1] "2024-04-03 15:35:23 CEST"
#> 
#> $caller
#> [1] "f"
#> 
#> $fun
#> [1] "add2"
#> 
#> $call
#> $call[[1]]
#> function() add2(a, b)
#> 
#> 
#> $args
#> $args[[1]]
#> [1] "delayedAssign(\"x\", value = a, eval.env = constructive::.env(\"0x10cc019f8\", parents = \"global\"))"
#> [2] "delayedAssign(\"y\", value = b, eval.env = constructive::.env(\"0x10cc019f8\", parents = \"global\"))"
#> 
#> 
#> $value
#> [1] 3

as.list(j[2,])
#> $time
#> [1] "2024-04-03 15:35:23 CEST"
#> 
#> $caller
#> [1] NA
#> 
#> $fun
#> [1] "add3"
#> 
#> $call
#> $call[[1]]
#> add3(a, b)
#> 
#> 
#> $args
#> $args[[1]]
#> [1] "x <- 1" ""       "y <- 2" ""      
#> 
#> 
#> $value
#> [1] 3

as.list(j[3,])
#> $time
#> [1] "2024-04-03 15:35:23 CEST"
#> 
#> $caller
#> [1] NA
#> 
#> $fun
#> [1] "add4"
#> 
#> $call
#> $call[[1]]
#> add4(a, b)
#> 
#> 
#> $args
#> $args[[1]]
#> [1] "x <- 1"                                                
#> [2] ""                                                      
#> [3] "delayedAssign(\"y\", value = b, eval.env = .GlobalEnv)"
#> 
#> 
#> $value
#> [1] 3
```
