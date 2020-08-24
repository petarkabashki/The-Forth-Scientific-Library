\ Medium Accuracy Calculation of the Error Function for real values.

\ Forth Scientific Library Algorithm #64

\ This is an ANS Forth program requiring the Floating-Point and Floating-Extension word sets.

\ Non STANDARD words:
\   : S>F  S>D D>F ;
\   : F, HERE 1 FLOATS ALLOT F! ;
\   : F-ROT FROT FROT ;
\   : F**2 FDUP F* ;
\   : FNIP FSWAP FDROP ;

\ (c) Copyright 2013, Hans Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

\ Notes:

\ This algorithm uses two domains. The first is located between
\ |0| and |1.378| and uses the Taylor expansion of

\                       3     5     7     9
\              2       z     z     z     z
\ erf (z) = _______ -  _  + __  - __  + ___  - ...
\           sqrt(pi)   3    10    42    216

\ The denominator terms are sequence A007680 in the OEIS.

\ The second, beyond |1.378|, uses a well-known Abramowitz and Stegun
\ approximation with a maximum error of 3e-7.

\ References:
\  Abramowitz, Milton; Stegun, Irene A., eds. (1972),
\  Handbook of Mathematical Functions with Formulas,
\  Graphs, and Mathematical Tables, New York: Dover Publications,
\  ISBN 978-0-486-61272-0

\ Also see algorithm #62 with its alternative implementations
\ of error functions.

: >taylor F**2 FOVER ;                   ( f1 f2 -- f1 f2*f2 f1)
: (taylor) FOVER F* FROT FOVER S>F F/ ;  ( n --) ( f1 f2 f3 -- f2 f3*f2 f1 f3*f2/n)
: +taylor (taylor) F+ F-ROT ;            ( n --) ( f1 f2 f3 -- f1+f3*f2/n f2 f3*f2)
: -taylor (taylor) F- F-ROT ;            ( n --) ( f1 f2 f3 -- f1-f3*f2/n f2 f3*f2)

CREATE (erf)
  0.0705230784e F,
  0.0422820123e F,
  0.0092705272e F,
  0.0001520143e F,
  0.0002765672e F,
  0.0000430638e F,

: ferf                                   ( f -- erf[f])
  FDUP FDUP FABS 1.378e F<
  IF
    >taylor
            3 -taylor
           10 +taylor
           42 -taylor
          216 +taylor
         1320 -taylor
         9360 +taylor
        75600 -taylor
       685440 +taylor
      6894720 -taylor
     76204800 +taylor
    918086400 -taylor
    FDROP FDROP
    1.1283791670955126e F*
  ELSE
    F0< >R FABS FDUP 1e
    6 0 DO FOVER I FLOATS (erf) + F@ F* F+ F-ROT FOVER F* FROT LOOP
    F**2 F**2 F**2 F**2 1e FSWAP F/ -1e F+
    FNIP FNIP R> 0= IF FNEGATE THEN
  THEN
;
