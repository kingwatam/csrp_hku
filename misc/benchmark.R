library(rbenchmark)
benchmark(rep(1:10^6, 3),
          columns=c('test', 'elapsed', 'replications'))