;; --- Day 1: Report Repair ---
;; --- Part One ---

;; Assignment (copy from https://adventofcode.com/2020/day/1):
;; Assignment author: Eric Wastl

;; After saving Christmas five years in a row, you've decided to take
;; a vacation at a nice resort on a tropical island. Surely, Christmas
;; will go on without you.

;; The tropical island has its own currency and is entirely
;; cash-only. The gold coins used there have a little picture of a
;; starfish; the locals just call them stars. None of the currency
;; exchanges seem to have heard of them, but somehow, you'll need to
;; find fifty of these coins by the time you arrive so you can pay the
;; deposit on your room.

;; To save your vacation, you need to get all fifty stars by December
;; 25th.

;; Collect stars by solving puzzles. Two puzzles will be made
;; available on each day in the Advent calendar; the second puzzle is
;; unlocked when you complete the first. Each puzzle grants one
;; star. Good luck!

;; Before you leave, the Elves in accounting just need you to fix your
;; expense report (your puzzle input); apparently, something isn't
;; quite adding up.

;; Specifically, they need you to find the two entries that sum to
;; 2020 and then multiply those two numbers together.

;; For example, suppose your expense report contained the following:

;; 1721
;; 979
;; 366
;; 299
;; 675
;; 1456

;; In this list, the two entries that sum to 2020 are 1721 and
;; 299. Multiplying them together produces 1721 * 299 = 514579, so the
;; correct answer is 514579.

;; Of course, your expense report is much larger. Find the two entries
;; that sum to 2020; what do you get if you multiply them together?

;; DISCLAIMER: Beware: these are the first Scheme (and for that
;; matter, Lisp) lines of code I ever wrote. It works, but it can
;; probably solved in better ways.

(use-modules (ice-9 format))

;; create a list of the given list of numbers
(define lst '(1388
              508
              1855
              1249
              1405
              1618
              1286
              1485
              1827
              1188
              1369
              1977
              910
              1398
              1912
              1575
              1713
              1771
              1506
              1056
              1890
              1065
              1591
              1438
              1155
              1275
              1622
              972
              1918
              1959
              1860
              1396
              1832
              1562
              1935
              1687
              1344
              1709
              1498
              1875
              1467
              1557
              1166
              1090
              1363
              1754
              987
              1548
              1334
              1315
              1300
              1043
              1417
              1040
              1955
              1440
              1635
              1519
              1305
              552
              1776
              1723
              1109
              1914
              981
              1886
              1607
              1639
              1582
              1444
              1627
              1157
              2008
              1554
              1781
              1847
              1415
              1915
              1416
              1431
              1579
              1193
              1921
              1971
              1360
              1631
              1972
              1988
              1813
              1378
              1505
              1973
              1585
              1091
              1853
              1531
              731
              1546
              1895
              1348
              1913
              1387
              1885
              1204
              1499
              1975
              1664
              1828
              1616
              1841
              1129
              137
              1676
              1694
              1928
              1354
              1814
              1228
              1588
              1642
              1261
              1446
              1903
              2003
              1751
              1083
              1829
              140
              1599
              1968
              1725
              1987
              1931
              1810
              1628
              2009
              1159
              1142
              1331
              1859
              1111
              1637
              1801
              1376
              1902
              1345
              1307
              1570
              1990
              1784
              1524
              1997
              1098
              1967
              1442
              1927
              1251
              1753
              1194
              1648
              1483
              1609
              1716
              1583
              1128
              1514
              1738
              1881
              1502
              1120
              1112
              433
              1033
              1208
              1982
              1544
              1169
              1306
              1690
              1590
              1938
              1177
              1819
              1568
              1666
              1682
              1844
              1783
              1774
              1688
              1925
              1471
              1203
              2007
              1769
              1323
              1370
              1689
              1268
              1868))

(define (do-num lst-a lst-b i)
  (define l (car lst-a))
  (define r (car lst-b))
  (define sum (+ l r))
  (if (= sum 2020)
      (format #t "\nFOUND: ~d+~d=~d\nSolution: ~d\n" l r sum (* l r)))
  (if (> (length lst-a) (+ i 1))
      (do-num lst-a (cdr lst-b) (+ i 1))
      (if (> (length lst-a) 2)
          (do-num (cdr lst-a) (cdr (cdr lst-a)) 1))))

(do-num lst (cdr lst) 1)
