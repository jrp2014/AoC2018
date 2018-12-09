
{{{
./Day09 +RTS -s
32
8317
146373
2764
54718
37305
437654
3689913905
 270,555,012,328 bytes allocated in the heap
  74,963,367,896 bytes copied during GC
   1,650,106,040 bytes maximum residency (98 sample(s))
      49,234,248 bytes maximum slop
            1573 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     259436 colls,     0 par   49.773s  49.841s     0.0002s    0.0162s
  Gen  1        98 colls,     0 par   51.119s  51.128s     0.5217s    4.8151s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time   45.911s  ( 45.856s elapsed)
  GC      time  100.892s  (100.969s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time  146.803s  (146.825s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    5,893,039,798 bytes per MUT second

  Productivity  31.3% of total user, 31.2% of total elapsed
}}}
