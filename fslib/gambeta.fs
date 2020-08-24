\ Complete and Incomplete Gamma and Beta Functions

\ Calculates:  Ln(Gamma(x)) for real, positive arguments.
\              Complete Beta function for real, positive arguments.
\              Beta probability density function for real, positive arguments.
\              Beta cumulative density function for real, positive arguments.
\              Incomplete Beta function real, positive arguments.
\              Regularized Gamma function for real, positive arguments.
\              Upper and lower incomplete Gamma functions for real, positive arguments.

\ Forth Scientific Library Algorithm #65

\ This is an ANS Forth program requiring:
\      1. The Floating-Point and Floating-Extension word sets
\      2. The words F, and F=

\ Non STANDARD words:
\   : F, HERE 1 FLOATS ALLOT F! ;
\   : F= F- F0= ;

\ (c) Copyright 2013, Hans Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

\ Notes:

\ Based on a Lanczos approximation:
\ Lanczos, Cornelius (1964), "A Precision Approximation of the Gamma Function"

\             ____              z + 0.5  -(z+g+0.5)
\ G(z + 1) = V2*pi (z + g + 0.5)        e          Ag(z)

\ Here g is a constant that may be chosen arbitrarily subject to the
\ restriction that Re(z+g+1/2) > 0.

\ See also: http://en.wikipedia.org/wiki/Lanczos_approximation

\ Also see algorithm #18 with its alternative implementation
\ of the gammaln function (called loggam).

CREATE _L
  76.18009172947146e     F,
 -86.50532032941677e     F,
  24.01409824083091e     F,
  -1.231739572450155e    F,
   0.1208650973866179e-2 F,
  -0.5395239384953e-5    F,

: gammaln                              ( f1 -- f2)
  FDUP F0< FDUP F0= OR ABORT" gammaln has 0 or negative argument "
  1.000000000190015e FOVER
  6 0 DO 1e F+ _L I FLOATS + F@ FOVER F/ FROT F+ FSWAP LOOP FDROP
  FOVER FDUP 5.5e F+ FDUP FLN
  FROT 0.5e F+ F* F- FNEGATE FSWAP
  2.5066282746310005e F* FROT F/ FLN F+
;                                      \ multiply by SQRT(2*PI)

\ *** Complete Beta function
\ Based on the mathematical identity:

\          G(a) * G(b)
\ B(a,b) = -----------
\           G(a + b)

FVARIABLE _gln                         \ temporary variable for gammaln call
                                       \ be nice to tiny FP stack ;-)
: fbeta                                ( f1 f2 -- f3) 
  FOVER FOVER F+ _gln F! gammaln FSWAP gammaln F+ _gln F@ gammaln F- FEXP
;

\ *** Beta probability density function
\ Based on the mathematical identity:

\              1     a - 1        b - 1
\ f(x;a,b) = ------ x      (1 - x)
\            B(a,b)

FVARIABLE _arg1                        \ first argument
FVARIABLE _arg2                        \ second argument
                                       \ be nice to tiny FP stack ;-)
: fbeta.pdf                            ( f1 f2 f3 -- f4)
  FOVER _arg1 F! FDUP _arg2 F!         ( x a b)
  1e F- FROT 1e FOVER F-               ( a b-1 x 1-x)
  FROT F** FROT FROT                   ( 1-x**b-1 a x)
  FSWAP 1e F- F** F*                   ( 1-x**b-1*x**a-1)
  _arg1 F@ _arg2 F@ fbeta F/
;

\ *** Beta cumulative density function
\ *** Incomplete Beta function ratio
\ *** Regularized incomplete Beta function

\  Reference:
\    KL Majumder, GP Bhattacharjee,
\    Algorithm AS 63:
\    The incomplete Beta Integral,
\    Applied Statistics,
\    Volume 22, Number 3, 1973, pages 409-411.

FVARIABLE _xx
FVARIABLE _pp
FVARIABLE _qq
FVARIABLE _psq
FVARIABLE _cx
FVARIABLE _ai
FVARIABLE _rx

1e-15 FCONSTANT _acu

: fbeta.cdf                            ( fx fp fq -- f1)
  FDUP F0< FDUP F0= OR FOVER F0< FOVER F0= OR OR
  ABORT" fbeta.cdf arguments p,q must be greater than 0 "
                                       ( F: fx fp fq) ( --)
  FOVER FOVER F+ _gln F! FOVER FOVER _qq F! _pp F!
  gammaln FSWAP gammaln F+ _gln F@ gammaln F- _gln F!
  _pp F@ _qq F@ FROT                   ( F: fp fq fx) ( --)

  FDUP F0< 1e FOVER F< OR
  ABORT" fbeta.cdf argument x must be between 0 and 1 "
                                       ( F: fp fq fx) ( --)
  FDUP F0= 1e FOVER f= OR
  1e FOVER F- _cx F! FROT FROT         \ _cx = 1 - x
  IF FDROP FDROP EXIT THEN             ( F: fx) ( --)
                                       \ _psq = p + q
  FOVER FOVER F+ _psq F! FROT FROT     ( F: fq fx fp) ( --)
  FOVER _psq F@ F* FOVER FSWAP F< DUP  ( F: fq fx fp) ( f f)

  IF                                   ( F: fq fx fp) ( f)
    _cx F@ _xx F! _qq F! _cx F! _pp F! ( F: --) ( f)
  ELSE                                 ( F: fq fx fp) ( f)
    _pp F! _xx F! _qq F!               ( F: --) ( f)
  THEN

  1e FDUP _arg1 F! FDUP _ai F!         ( F: fv) ( f)
  _qq F@ FDUP _cx F@ _psq F@ F* F+ F>D
  _xx F@ 2DUP D0= 0= IF _cx F@ F/ THEN ( F: fv ft f1) ( f d1)
  _rx F! FOVER F-                      ( F: fv ft) ( f d1)

  BEGIN
    _arg1 DUP F@ F* _rx F@ F* _pp F@ _ai F@ F+ F/ FDUP F!
    FSWAP FOVER F+ FSWAP FABS
    _acu FOVER F< FOVER _acu F* FOVER F< OR
  WHILE
    FDROP 1. D- _ai DUP F@ 1e F+ F! 2DUP D0<
    IF
      _psq DUP F@ FDUP 1e F+ F!
    ELSE
      _qq F@ _ai F@ F- 2DUP D0= IF _xx F@ _rx F! THEN
    THEN
  REPEAT 2DROP FDROP                   \ value *= ((exp ((pp * ln (xx)) + (qq
                                       \ - 1.0) * ln (cx) - beta )) / pp);
  _xx F@ FLN _pp F@ F* _cx F@ FLN _qq F@ 1e F- F* F+ _gln F@ F- FEXP F*
  _pp F@ F/ IF 1e FSWAP F- THEN
;

\ *** Incomplete Beta function
\ Based on the mathematical identity:

\ B(x;a,b) = I(a,b) * B(a,b)
\             x

: fbetain fbeta.cdf _gln F@ FEXP F* ;

\ *** Regularized Gamma functions

\  Reference:
\    Chi Leung Lau,
\    Algorithm AS 147:
\    A Simple Series for the Incomplete Gamma Integral,
\    Applied Statistics,
\    Volume 29, Number 1, 1980, pages 113-114

\ See also: http://en.wikipedia.org/wiki/Incomplete_gamma_function
                                       \ constant for Ln(1e-37)
-85.1956484407796903086656838233e FCONSTANT _underflow
                                       \ x a -- P(x,a)
: gammaP                               ( f1 f2 -- f3)
  FDUP F0< FDUP F0= OR FOVER F0< FOVER F0= OR OR 
  ABORT" gammaP has 0 or negative argument "
                                       \ save _gln for incomplete gamma
  FSWAP FOVER 1e F+ gammaln FDUP _gln F!
  FOVER F+ _arg1 F! FOVER FOVER FLN F* _arg1 F@ F-

  FDUP _underflow F< ABORT" Floating point underflow "
  FEXP FDUP F0= ABORT" Floating point underflow "

  FROT FROT 1e FDUP _arg1 F!           ( f3 f2 f1 f4=1)

  BEGIN
    FOVER F* FROT 1e F+ FSWAP FOVER F/
    FDUP FDUP _arg1 F@ F+ FDUP _arg1 F! 1000000000e F/ F<
    FROT FSWAP
  UNTIL

  FDROP FDROP FDROP _arg1 F@ F*
;

\ Based on the mathematical identity:
\ Q(s,x) = 1 - P(s,x)

: gammaQ gammaP FNEGATE 1e F+ ;        ( f1 f2 -- f3)

\ *** Upper and lower incomplete Gamma functions

\ Based on the mathematical identities:

\ g(s,x) = P(s,x) * G(s)
\ G(s,x) = Q(s,x) * G(s)

: lgammain FSWAP FOVER gammaP _gln F@ FEXP FROT F/ F* ;
: ugammain FSWAP FOVER gammaQ _gln F@ FEXP FROT F/ F* ;
                                       \ G(x) = G(x+1)/x
\ end of file gambeta.fs