# Parallel simulations

Simulations are necessary to compute the confidence envelopes of the
main functions of the package. They can be run in parallel thanks to the
**future** and **doFuture** (Bengtsson 2021) packages.

## Example

Estimating the confidence envelope of the M function under the null
hypothesis of random labeling of the point types relies on the
[`MEnvelope()`](https://ericmarcon.github.io/dbmss/reference/MEnvelope.md)
function. Simulations are run sequentially. The computing time can be
measured.

``` r

library("dbmss")
system.time(
  MEnvelope(
    paracou16, 
    r = NULL, 
    NumberOfSimulations = 5000, 
    Alpha = 0.05, 
    ReferenceType = "V. Americana", 
    NeighborType = "Q. Rosea", 
    SimulationType = "RandomLabeling", 
    Global = TRUE
  )  
)
```

    ##    user  system elapsed 
    ##  50.814   4.971  37.700

It can be parallelized by setting a hardware strategy. The following
example launches several simultaneous copies of R on the local computer:
it works on all operating systems. The number of parallel processes here
is the number of logical CPUs.

``` r

library("doFuture")
plan(multisession, workers = availableCores())
```

The same set of simulations is now run faster with argument
`parallel = TRUE`.

``` r

system.time(
  MEnvelope(
    paracou16, 
    r = NULL, 
    NumberOfSimulations = 5000, 
    Alpha = 0.05, 
    ReferenceType = "V. Americana", 
    NeighborType = "Q. Rosea", 
    SimulationType = "RandomLabeling", 
    Global = TRUE, 
    parallel = TRUE
  )
)
```

    ##    user  system elapsed 
    ##   1.927   0.163  26.212

The default strategy is `sequential`. It should be restored.

``` r

plan(sequential)
```

Other possible strategies are forking (`multicore`), unavailable on
Windows, and `cluster`, on several computers.

See the documentation of the *future* package, namely
[`future::plan()`](https://future.futureverse.org/reference/plan.html).

## Progress bar

Parallel simulations can report their progress thanks to the
**progressr** package. They ignore the `verbose` argument but rely on
the `parallel_pgb_refresh` argument to refresh the progress bar more or
less often.

The progress bar must be activated and its interface chosen, for example
by the following code, to be run before calling the
[`MEnvelope()`](https://ericmarcon.github.io/dbmss/reference/MEnvelope.md)
function:

``` r

library("progressr")
handlers(global = TRUE)
handlers("txtprogressbar")
```

Note that the choice of the progress bar is free: see
[`?progressr`](https://progressr.futureverse.org/reference/progressr.html).

To limit the overhead of updating the progress bar, this can be done
during a fraction of the simulations. `parallel_pgb_refresh` is $`1/10`$
by default, meaning that the progress bar display is updated one
simulation out of ten. This fraction can be increased to 1 if each
simulation takes a long time (say, one second or more) or decreased to
improve the computing performance. In the example above, $`1/100`$ is a
reasonable choice on a laptop computer.

## Limits

Setting up the parallel code and gathering the results takes time so the
performance of sequential simulations may be better if they are fast.

## References

Bengtsson, Henrik. 2021. “A Unifying Framework for Parallel and
Distributed Processing in R Using Futures.” *The R Journal* 13 (2): 208.
<https://doi.org/10.32614/RJ-2021-048>.
