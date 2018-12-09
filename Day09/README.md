
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

Skip unecessary updates:
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
  15,168,120,608 bytes allocated in the heap
   8,956,011,584 bytes copied during GC
   1,362,156,904 bytes maximum residency (68 sample(s))
       9,924,976 bytes maximum slop
            1299 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     14186 colls,     0 par    5.369s   5.377s     0.0004s    0.0194s
  Gen  1        68 colls,     0 par    7.266s   7.280s     0.1071s    3.0918s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    3.091s  (  3.088s elapsed)
  GC      time   12.635s  ( 12.658s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time   15.726s  ( 15.746s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    4,907,849,469 bytes per MUT second

  Productivity  19.7% of total user, 19.6% of total elapsed
}}}



Using Data.Sequence, rather than [] for keeping / updating scores

{{{
./Day09seq +RTS -s            
32
7932
146373
2764
54718
37305
437654
3689913905
   3,785,596,592 bytes allocated in the heap
   6,536,966,496 bytes copied during GC
   1,296,437,472 bytes maximum residency (17 sample(s))
       8,648,512 bytes maximum slop
            1236 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      3255 colls,     0 par    3.765s   3.765s     0.0012s    0.0224s
  Gen  1        17 colls,     0 par    6.036s   6.039s     0.3552s    3.2068s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    1.487s  (  1.487s elapsed)
  GC      time    9.801s  (  9.804s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time   11.288s  ( 11.291s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    2,546,026,433 bytes per MUT second

  Productivity  13.2% of total user, 13.2% of total elapsed
}}}
