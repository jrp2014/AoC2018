# Benchmarking results

Compared to Bartosz Milewski's (superior) [Open Season on
Hylomorphisms](https://bartoszmilewski.com/2018/12/20/open-season-on-hylomorphisms/)
version.

    MBP:Day02 $ ./Day02+ +RTS -s
    benchmarking Bartosz Milewski/checkSum (Part 1)
    time                 397.4 μs   (390.0 μs .. 406.2 μs)
                         0.998 R²   (0.997 R² .. 1.000 R²)
    mean                 391.1 μs   (389.6 μs .. 396.0 μs)
    std dev              7.902 μs   (3.453 μs .. 16.58 μs)
    variance introduced by outliers: 12% (moderately inflated)

    benchmarking Bartosz Milewski/Brute force (Part 2):
    time                 3.527 ms   (3.519 ms .. 3.537 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 3.519 ms   (3.511 ms .. 3.528 ms)
    std dev              26.72 μs   (20.74 μs .. 36.62 μs)

    benchmarking Bartosz Milewski/Using Trie (Part 2):
    time                 465.1 μs   (464.0 μs .. 466.2 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 464.3 μs   (463.5 μs .. 465.4 μs)
    std dev              3.103 μs   (2.317 μs .. 4.866 μs)

    benchmarking Bartosz Milewski/Using hylo (Part 2):
    time                 2.683 ms   (2.676 ms .. 2.691 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 2.686 ms   (2.680 ms .. 2.694 ms)
    std dev              22.79 μs   (17.40 μs .. 35.30 μs)

    benchmarking Bartosz Milewski/Using hylo (Part 2bis)
    time                 1.164 ms   (1.160 ms .. 1.170 ms)
                         0.998 R²   (0.995 R² .. 1.000 R²)
    mean                 1.174 ms   (1.165 ms .. 1.214 ms)
    std dev              52.36 μs   (10.89 μs .. 117.4 μs)
    variance introduced by outliers: 34% (moderately inflated)

    benchmarking 2014/Part 1
    time                 1.319 ms   (1.313 ms .. 1.330 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 1.321 ms   (1.316 ms .. 1.328 ms)
    std dev              17.95 μs   (11.07 μs .. 31.47 μs)

    benchmarking 2014/Part 2
    time                 11.31 ms   (11.08 ms .. 11.59 ms)
                         0.998 R²   (0.997 R² .. 1.000 R²)
    mean                 11.07 ms   (11.00 ms .. 11.19 ms)
    std dev              241.7 μs   (149.5 μs .. 357.3 μs)

     125,958,267,216 bytes allocated in the heap
      13,315,286,712 bytes copied during GC
           3,493,888 bytes maximum residency (6931 sample(s))
             162,688 bytes maximum slop
                   3 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0     114936 colls,     0 par    6.216s   6.302s     0.0001s    0.0018s
      Gen  1      6931 colls,     0 par    3.440s   3.473s     0.0005s    0.0057s

      INIT    time    0.000s  (  0.002s elapsed)
      MUT     time   26.367s  ( 26.516s elapsed)
      GC      time    9.656s  (  9.775s elapsed)
      EXIT    time    0.000s  (  0.005s elapsed)
      Total   time   36.024s  ( 36.298s elapsed)

      %GC     time       0.0%  (0.0% elapsed)

      Alloc rate    4,777,065,160 bytes per MUT second

      Productivity  73.2% of total user, 73.0% of total elapsed

Changing foldl to foldl':

    MBP:Day02 $ ghc -O2  -rtsopts  -package criterion Day02+.hs
    Loaded package environment from /Users/jrp/.ghc/x86_64-darwin-8.6.3/environments/default
    [1 of 1] Compiling Main             ( Day02+.hs, Day02+.o )
    Linking Day02+ ...
    MBP:Day02 $ ./Day02+ +RTS -s
    benchmarking Bartosz Milewski/checkSum (Part 1)
    time                 384.1 μs   (382.0 μs .. 386.1 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 386.9 μs   (384.1 μs .. 395.8 μs)
    std dev              15.01 μs   (5.601 μs .. 31.30 μs)
    variance introduced by outliers: 33% (moderately inflated)

    benchmarking Bartosz Milewski/Brute force (Part 2):
    time                 3.467 ms   (3.450 ms .. 3.490 ms)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 3.466 ms   (3.457 ms .. 3.482 ms)
    std dev              36.66 μs   (23.71 μs .. 56.47 μs)

    benchmarking Bartosz Milewski/Using Trie (Part 2):
    time                 464.1 μs   (463.0 μs .. 465.3 μs)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 461.8 μs   (460.4 μs .. 463.3 μs)
    std dev              5.154 μs   (4.184 μs .. 6.787 μs)

    benchmarking Bartosz Milewski/Using hylo (Part 2):
    time                 2.667 ms   (2.642 ms .. 2.706 ms)
                         0.998 R²   (0.996 R² .. 1.000 R²)
    mean                 2.656 ms   (2.637 ms .. 2.685 ms)
    std dev              74.90 μs   (48.03 μs .. 106.7 μs)
    variance introduced by outliers: 14% (moderately inflated)

    benchmarking Bartosz Milewski/Using hylo (Part 2bis)
    time                 1.170 ms   (1.162 ms .. 1.180 ms)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 1.168 ms   (1.164 ms .. 1.174 ms)
    std dev              17.10 μs   (11.13 μs .. 25.72 μs)

    benchmarking 2014/Part 1
    time                 1.310 ms   (1.305 ms .. 1.315 ms)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 1.311 ms   (1.309 ms .. 1.313 ms)
    std dev              8.070 μs   (6.508 μs .. 10.56 μs)

    benchmarking 2014/Part 2
    time                 11.19 ms   (10.96 ms .. 11.53 ms)
                         0.997 R²   (0.994 R² .. 1.000 R²)
    mean                 10.99 ms   (10.93 ms .. 11.10 ms)
    std dev              218.9 μs   (107.4 μs .. 374.6 μs)

     125,956,031,976 bytes allocated in the heap
      13,306,406,600 bytes copied during GC
           3,494,040 bytes maximum residency (6948 sample(s))
             158,400 bytes maximum slop
                   3 MB total memory in use (0 MB lost due to fragmentation)

                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0     114916 colls,     0 par    6.197s   6.284s     0.0001s    0.0008s
      Gen  1      6948 colls,     0 par    3.375s   3.408s     0.0005s    0.0032s

      INIT    time    0.000s  (  0.002s elapsed)
      MUT     time   26.175s  ( 26.325s elapsed)
      GC      time    9.573s  (  9.692s elapsed)
      EXIT    time    0.000s  (  0.004s elapsed)
      Total   time   35.748s  ( 36.024s elapsed)

      %GC     time       0.0%  (0.0% elapsed)

      Alloc rate    4,812,020,128 bytes per MUT second

      Productivity  73.2% of total user, 73.1% of total elapsed

    MBP:Day02 $

