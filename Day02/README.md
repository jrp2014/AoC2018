# Benchmarking results

Compared to Bartosz Milewski's (superior) [Open Season on
Hylomorphisms](https://bartoszmilewski.com/2018/12/20/open-season-on-hylomorphisms/)
version.

    Day02$ ghc -O2 Day02+.hs -package criterion
    Loaded package environment from /Users/jrp2014/.ghc/x86_64-darwin-8.6.3/environments/default
    [1 of 1] Compiling Main             ( Day02+.hs, Day02+.o )
    Linking Day02+ ...
    Day02$ ./Day02+
    benchmarking Bartosz Milewski/checkSum (Part 1)
    time                 402.7 μs   (400.1 μs .. 406.4 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 401.6 μs   (400.0 μs .. 404.0 μs)
    std dev              6.719 μs   (5.075 μs .. 10.05 μs)

    benchmarking Bartosz Milewski/Brute force (Part 2):
    time                 3.531 ms   (3.494 ms .. 3.566 ms)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 3.567 ms   (3.546 ms .. 3.591 ms)
    std dev              71.89 μs   (58.96 μs .. 93.11 μs)

    benchmarking Bartosz Milewski/Using Trie (Part 2):
    time                 458.8 μs   (455.7 μs .. 463.8 μs)
                         0.999 R²   (0.997 R² .. 1.000 R²)
    mean                 461.7 μs   (458.4 μs .. 468.7 μs)
    std dev              14.92 μs   (8.126 μs .. 27.48 μs)
    variance introduced by outliers: 24% (moderately inflated)

    benchmarking Bartosz Milewski/Using hylo (Part 2):
    time                 2.751 ms   (2.719 ms .. 2.783 ms)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 2.712 ms   (2.697 ms .. 2.730 ms)
    std dev              53.83 μs   (41.59 μs .. 70.02 μs)

    benchmarking Bartosz Milewski/Using hylo (Part 2bis)
    time                 1.192 ms   (1.180 ms .. 1.205 ms)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 1.195 ms   (1.189 ms .. 1.205 ms)
    std dev              25.36 μs   (18.99 μs .. 39.89 μs)

    benchmarking jrp2014/Part 1
    time                 1.356 ms   (1.348 ms .. 1.364 ms)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 1.362 ms   (1.356 ms .. 1.373 ms)
    std dev              26.84 μs   (18.40 μs .. 44.26 μs)

    benchmarking jrp2014/Part 2
    time                 11.12 ms   (11.01 ms .. 11.24 ms)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 11.20 ms   (11.12 ms .. 11.30 ms)
    std dev              235.7 μs   (154.0 μs .. 366.1 μs)
