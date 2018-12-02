`./Day1 +RTS -s`

List-based version:

```
525
75749
      25,342,304 bytes allocated in the heap
      17,323,360 bytes copied during GC
       3,581,184 bytes maximum residency (5 sample(s))
         999,496 bytes maximum slop
               9 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        44 colls,     0 par    0.006s   0.006s     0.0001s    0.0004s
  Gen  1         5 colls,     0 par    0.004s   0.006s     0.0012s    0.0027s

  INIT    time    0.000s  (  0.002s elapsed)
  MUT     time   70.794s  ( 71.118s elapsed)
  GC      time    0.010s  (  0.012s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time   70.808s  ( 71.132s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    357,971 bytes per MUT second

  Productivity 100.0% of total user, 100.0% of total elapsed
  ```


Set-based version:
  ```
  525
75749
     149,191,640 bytes allocated in the heap
      31,794,240 bytes copied during GC
       6,498,248 bytes maximum residency (6 sample(s))
          97,080 bytes maximum slop
              14 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       280 colls,     0 par    0.019s   0.020s     0.0001s    0.0002s
  Gen  1         6 colls,     0 par    0.013s   0.016s     0.0027s    0.0078s

  INIT    time    0.000s  (  0.002s elapsed)
  MUT     time    0.102s  (  0.103s elapsed)
  GC      time    0.032s  (  0.037s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time    0.138s  (  0.142s elapsed)

  %GC     time      23.1%  (25.7% elapsed)

  Alloc rate    1,467,627,170 bytes per MUT second

  Productivity  76.8% of total user, 72.8% of total elapsed
  ```
