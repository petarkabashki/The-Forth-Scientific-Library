(      Title:  A utility for testing Forth
               data and floating-point stack effects
        File:  ttester-xf.fs
  Test files:  ttester-xf-test.fs, ttester-xfarray-test.fs 
    Log file:  ttester-xf.log
     License:  John Hayes, Public Domain
     Version:  1.3.1
     Revised:  February 5, 2011

For any code derived from John Hayes' tester program:
)
\  (C) 1995 JOHNS HOPKINS UNIVERSITY / APPLIED PHYSICS LABORATORY
\  MAY BE DISTRIBUTED FREELY AS LONG AS THIS COPYRIGHT NOTICE REMAINS.
(
The rest is public domain.

This file revises the code for ttester, a utility for testing
Forth words, as developed by several authors [see below].  The
following is a quote from ttester.
)
\ ttester is based on the original tester suite by Hayes:
\ From: John Hayes S1I
\ Subject: tester.fr
\ Date: Mon, 27 Nov 95 13:10:09 PST  
\ (C) 1995 JOHNS HOPKINS UNIVERSITY / APPLIED PHYSICS LABORATORY
\ MAY BE DISTRIBUTED FREELY AS LONG AS THIS COPYRIGHT NOTICE REMAINS.
\ VERSION 1.1
\ All the subsequent changes have been placed in the public
\ domain.  The primary changes from the original are the
\ replacement of "{" by "T{" and "}" by "}T" (to avoid conflicts
\ with the uses of { for locals and } for FSL arrays),
\ modifications so that the stack is allowed to be non-empty
\ before T{ , and extensions for the handling of floating point
\ tests.  Code for testing equality of floating point values
\ comes from ftester.fs written by David N. Williams, based on
\ the idea of approximate equality in Dirk Zoller's float.4th.
\ Further revisions were provided by Anton Ertl, including the
\ ability to handle either integrated or separate floating point
\ stacks.  Revision history and possibly newer versions can be
\ found at

\ http://www.complang.tuwien.ac.at/cvsweb/cgi-bin/cvsweb/gforth/test/ttester.fs

\ Explanatory material and minor reformatting (no code changes)
\ by C. G. Montgomery March 2009, with helpful comments from
\ David Williams and Krishna Myneni.
(
This file is a consolidation of three files:  xftester.fs,
xtester.fs, and ftester.fs.  Here ftester.fs is different from
the file in the quote above, which has been renamed as
ftester-old.fs.

The designation "x" stands for the generic ANS/ISO Forth
"unspecified cell" data type.

The following revision of the ttester explanatory material, by
David N. Williams, takes into account ttester-xf restrictions
and extras.  Versions 1.2.0 and later and 1.3.0 and later add
new fp comparisons, based on suggestions by Krishna Myneni in
comp.lang.forth.

The basic restriction is that an integrated floating-point stack
is no longer allowed.

A number of words in ttester could be classified as implicitly
private, although a specific user interface was never declared.
In this file the designations PUBLIC and PRIVATE indicate word
sets expected to be supported or not necessarily supported in
future versions.  This is purely a textual affectation, without
actual wordlist management such as that provided by the FSL
PUBLIC: and PRIVATE: words.

Users of ttester who restrict themselves to a separate floating-
point stack, and who use only the following words, can use
ttester-xf as a drop-in replacement:

  T{  ->  }T  VERBOSE  TESTING
  HAS-FLOATING  ABS-NEAR  REL-NEAR  SET-EXACT  SET-NEAR

The ttester words ERROR-XT and ERROR1 are no longer available,
but have been replaced by stack-specific words, XT-ERROR-XT and
FT-ERROR-XT, respectively, XT-ERROR-DEFAULT and FT-ERROR-DEFAULT.

The following files contain enhancements over default error
reporting:  tester-display.fs, xtester-errors.fs, and
ftester-errors.fs.  See ttester-xf-test.fs for examples of their
use.

These words are currently declared to be the ttester-xf user
interface:

PUBLIC WORDS

   tests:  T{  ->  }T  VERBOSE  TESTING  HAS-FLOATING
           X{  X->  }X  F{  F->  }F
           FT-ABS-ERROR  FT-REL-ERROR  FT-AREL-ERROR
           FT-TEST=-XT
           FT-DATUM=  FT-ABS=  FT-REL=  FT-AREL=  FT-REL0=
           SET-FT-MODE-EXACT  SET-FT-MODE-ABS
           SET-FT-MODE-REL  SET-FT-MODE-AREL  SET-FT-MODE-REL0

           for ttester compatibiity
           ABS-NEAR   REL-NEAR
           SET-EXACT  SET-NEAR

  errors:  XT-ERROR-XT  XT-ERROR-DEFAULT  XT-ERROR-INDEX
           FT-ERROR-XT  FT-ERROR-DEFAULT  FT-ERROR-INDEX

  arrays:  XT-INITIALS  XT-RESULTS  XT-GOALS
           FT-INITIALS  FT-RESULTS  FT-GOALS
           XT-A@  XT-A!  XT-A<>
           FT-A@  FT-A!  FT-A<>

  config:  MAX-XT-INITIALS  MAX-XT-RESULTS
           MAX-FT-INITIALS  MAX-FT-RESULTS

Basic usage takes the form:

  <xfinitials> T{ <xfresults> -> <xfgoals> }T <xfinitials>

Here <xfinitials> are possibly nonempty data and fp stack
contents at the start of the test.  The sequence saves
<xfinitials> and removes them from the stacks, executes the code
that produces the data and fp stack <xfresults>, saves and
removes the <xfresults> from the stacks, executes the code that
produces the data and fp stack <xfgoals>, saves and removes
them, compares the saved <xfresults> and <xfgoals>, reports when
there is a discrepancy between the two, and restores the saved
<xfinitials>.

For example:

T{ 1 2 3 swap -> 1 3 2 }T  ok
T{ 1 2 3 swap -> 1 2 2 }T INCORRECT RESULT: T{ 1 2 3 swap -> 1 2 2 }T ok
T{ 1 2 3 swap -> 1 2 }T WRONG NUMBER OF RESULTS: T{ 1 2 3 swap -> 1 2 }T ok

More examples can be found in the file ttester-xf-test.fs.

In addition to the T{..->..}T and F{..F->..}F tests in ttester
[where "}F" was formerly named "F}"], ttester-xf makes
X{..X->..}X tests available.  The X and F tests act only on the
data and fp stacks, respectively.

Floating-point testing can involve further complications.  The
code attempts to determine whether floating-point support is
present, and behaves accordingly.  The constant HAS-FLOATING
contains the results of its efforts, so the behavior of the test
code can be modified by the user if necessary.

Then there are the perennial issues of floating-point value
comparisons.  Exact equality is specified by SET-FT-MODE-EXACT
[the default].  If one of the included approximate equality
tests is desired, execute SET-FT-MODE-ABS, SET-FT-MODE-REL,
SET-FT-MODE-AREL, or SET-FT-MODE-REL0.  Corresponding to these,
the public fvariables FT-ABS [default 0E], FT-REL [default
1E-12], and FT-AREL [default 1E-12] contain the values to be
used in comparisons by the private words FT-ABS=, FT-REL=, and
FT-AREL=.  FT-REL0= conditionally uses both FT-ABS and FT-REL.
Here is an example:

  SET-FT-MODE-REL
  1E-6 FT-REL F!
  F{ S" 3.14159" >FLOAT F-> -1E FACOS TRUE }F

Note that comparisons in this mode are not symmetric.  Measured
values must be in the fresults, and reference values in the
fgoals.

The user may supply a different equality test by storing
his own xt in the public variable FT-TEST=-XT.

Storage of initial, result, and goal data includes overflow
checking.  The user can change the overflow limits by defining
one or both of the following constants *before* loading this
file:

  MAX-XT-INITIALS  MAX-XT-RESULTS	
  MAX-FT-INITIALS  MAX-FT-RESULTS

The xt's and tolerances in the following variables and
fvariables can only be changed after loading this file:

  XT-ERROR-XT  FT-ERROR-XT  FT-TEST=-XT
  FT-ABS  FT-REL  FT-AREL
  ABS-NEAR  REL-NEAR  [ttester compatibility]

As with ttester, loading ttester-xf does not change BASE.
Remember that floating-point input is ambiguous if the base is
not decimal.

This version of ttester-xf uses a few more non-CORE words, and
non-FLOATING words in the floating-point sections, than ttester,
namely,  2>R, 2R>, [UNDEFINED], and INCLUDED.  Here is the
complete list:

  TOOLS EXT:      [IF]  [ELSE]  [THEN]  [UNDEFINED]
  CORE EXT:       2>R  2R>
                  TO  VALUE  [ftester only]
  FILE:           (  INCLUDED
  FLOATING EXT:   F~

URL's for ttester-xf with tests and enhanced error reporting:

http://www.umich.edu/~williams/archive/forth/utilities/#ttester
http://www.umich.edu/~williams/archive/forth/utilities/ttester-xf.zip
)

(
TABLE OF CONTENTS

  XTESTER
  X.1 ERROR REPORTING
  X.2 ARRAYS
  X.3 TESTS

  FTESTER
  F.1 ERROR REPORTING
  F.2 COMPARISONS
  F.3 LEGACY COMPARISONS
  F.4 ARRAYS
  F.5 TESTS

  TTESTER-XF
  T.1 COMMENTS
  T.2 TESTS
)
BASE @ decimal


\ *** XTESTER

\ *** X.1 ERROR REPORTING

VARIABLE XT-ERROR-XT
: XT-ERROR XT-ERROR-XT @ EXECUTE ;  \ vectored error reporting

VARIABLE XT-ERROR-INDEX  \ holds array index|-1 after error

: XT-ERROR-DEFAULT   ( c-addr u -- )
(
Display an error message followed by the line that had the
error.
)
  TYPE SOURCE TYPE CR ;
' XT-ERROR-DEFAULT XT-ERROR-XT !

\ *** X.2 ARRAYS
(
The first array slot has index zero.
)

\ USER CONFIG DEFAULTS
[UNDEFINED] MAX-XT-INITIALS [IF] 32 CONSTANT MAX-XT-INITIALS [THEN]
[UNDEFINED] MAX-XT-RESULTS  [IF] 32 CONSTANT MAX-XT-RESULTS  [THEN]

\ Arrays begin with a cell-sized count.
CREATE XT-INITIALS MAX-XT-INITIALS 1+ CELLS ALLOT
CREATE XT-RESULTS  MAX-XT-RESULTS  1+ CELLS ALLOT
CREATE XT-GOALS    MAX-XT-RESULTS  1+ CELLS ALLOT

\ overflow checking to be done elsewhere
: XT-A!  ( x_[n-1] ... x_0 +n addr -- )
  OVER 1+ CELLS OVER + SWAP DO I ! 1 CELLS +LOOP ;

: XT-A@  ( addr -- x_[n-1] ... x_0 +n )
  DUP @ CELLS OVER + DO I @ -1 CELLS +LOOP ;

[UNDEFINED] NIP [IF] : NIP  ( a b -- b)  SWAP DROP ; [THEN]

: XT-A<>  ( addr1 addr2 -- [index+1]|-1|0 )
(
Nonzero return for errors:
  -1       unequal array size
  index+1  deepest unequal array elements
)
  DUP @ 2>R    ( addr1 r: addr2 #left )
  DUP @ R@ <> IF 2R> 2DROP DROP -1 EXIT THEN
  ( addr1) DUP
  BEGIN  CELL+ R@    ( addr1 addr1' #left)
  WHILE  R> R> CELL+ ( addr1 addr1' #left addr2')
         DUP >R SWAP 1- >R @ OVER @ =
  WHILE
  REPEAT ( addr1 addr1') DROP @ ( len) 2R> NIP - ( index+1)
  ELSE   ( addr1 addr1') 2DROP 2R> 2DROP 0
  THEN ;

\ *** X.3 TESTS

: X{   ( <xinitials> -- )
(
Save the data stack <xinitials> for restoration by }X.  Clear
<xresults> and <xgoals> storage to put it in a defined state for
}X in case X-> is missing.
)
  DEPTH DUP MAX-XT-INITIALS > ABORT" TOO MANY INITIALS"
  XT-INITIALS XT-A!   0 XT-RESULTS !   0 XT-GOALS ! ;

: X->  ( <xresults> -- )
(
Record the depth and contents of the data stack.
)
  DEPTH DUP MAX-XT-RESULTS > ABORT" TOO MANY RESULTS"
  XT-RESULTS XT-A! ;

: }X   ( <xgoals> -- <xinitials> )
(
Save the data stack <xgoals>, compare them with the saved
<xresults>, and restore the <xinitials>.
)
  DEPTH DUP MAX-XT-RESULTS > ABORT" TOO MANY GOALS"
  XT-GOALS XT-A!
  XT-RESULTS XT-GOALS XT-A<> DUP XT-ERROR-INDEX !
  ?DUP IF -1 =
    IF   S" WRONG NUMBER OF RESULTS: "
    ELSE -1 XT-ERROR-INDEX +! S" INCORRECT RESULT: " THEN
    XT-ERROR
  THEN
  XT-INITIALS XT-A@ ( len) DROP ;


\ *** FTESTER

: "FLOATING" S" FLOATING" ;    \ only compiled S" in CORE
"FLOATING" ENVIRONMENT? [IF]
   [IF] TRUE [ELSE] FALSE [THEN]
[ELSE]
   FALSE
[THEN] CONSTANT HAS-FLOATING

HAS-FLOATING [IF]

DEPTH 1E DEPTH 1- FDROP = 0=
[IF] .( FLOATING-STACK REQUIRED) ABORT [THEN]

\ *** F.1 ERROR REPORTING
(
Error reporting in ttester-xf is unavoidably different from that
in ttester, because ttester-xt does not use a common execution
vector for data and fp stack reports.  That of course does not
affect compatibility as long as no errors occur.
)

\ PUBLIC

VARIABLE FT-ERROR-XT
: FT-ERROR   FT-ERROR-XT @ EXECUTE ;

VARIABLE FT-ERROR-INDEX  \ holds array index|-1 after error

: FT-ERROR-DEFAULT   ( c-addr u -- )
(
Display an error message followed by the line that had the
error.
)
  TYPE SOURCE TYPE CR ;
' FT-ERROR-DEFAULT FT-ERROR-XT !

\ *** F.2 COMPARISONS
(
The public words in this section provide an alternative to the
ttester fp equality tests.  The ttester-xf default is the same
exact equality as that of ttester.
)

\ PUBLIC

VARIABLE FT-TEST=-XT

\ PRIVATE

: FT-TEST=  ( f: x y -- ) ( -- flag )  FT-TEST=-XT @ EXECUTE ;

\ PUBLIC

\ The sign of these tolerances must be plus.
FVARIABLE FT-ABS-ERROR      0E FT-ABS-ERROR  F!
FVARIABLE FT-REL-ERROR   1E-12 FT-REL-ERROR  F!
FVARIABLE FT-AREL-ERROR  1E-12 FT-AREL-ERROR F!

\ PUBLIC

: FT-DATUM=  ( f: x y -- ) ( -- flag )
(
Leave TRUE if the two floats have the same internal
representation, including the IEEE-FP 2008 special data, signed
NAN with load, and signed zero and infinity.  Else leave FALSE.

Whether the specials work with F~ is implementation dependent,
according to DPANS94.
)
  0E F~ ;

\ |x - y| < eps
: FT-ABS=  ( f: x y -- ) ( -- flag )  FT-ABS-ERROR F@ F~ ;

\ |m - r| < eps * |r|
: FT-REL=  ( f: meas ref -- ) ( -- flag )
  FSWAP FOVER F- FSWAP F/ FABS FT-REL-ERROR F@ F< ;

\ |x - y| < eps * (|x| + |y|)/2 
: FT-AREL= ( f: x y -- ) ( -- flag )  FT-AREL-ERROR F@ FNEGATE 2E F/ F~ ;

: FT-REL0=  ( f: meas ref -- ) ( -- flag )  \ Krishna Myneni
  FDUP F0= IF FT-ABS=  ELSE  FT-REL= THEN ;

: SET-FT-MODE-EXACT ( -- )  ['] FT-DATUM= FT-TEST=-XT ! ;
: SET-FT-MODE-ABS   ( -- )  ['] FT-ABS=   FT-TEST=-XT ! ;
: SET-FT-MODE-REL   ( -- )  ['] FT-REL=   FT-TEST=-XT ! ;
: SET-FT-MODE-AREL  ( -- )  ['] FT-AREL=  FT-TEST=-XT ! ;
: SET-FT-MODE-REL0  ( -- )  ['] FT-REL0=  FT-TEST=-XT ! ;

SET-FT-MODE-EXACT

\ *** F.3 LEGACY COMPARISONS
(
Exact and approximate fp equality in ttester-xf remain
compatible with ttester, if the public words in this section are
used instead of the newer ttester-xf words.
)

\ PUBLIC
(
Set the following to the relative and absolute tolerances you
want for approximate float equality, to be used with F~ in
FT-NEARLY=.  Keep the signs, because F~ needs them.
)
  FVARIABLE REL-NEAR   1E-12 REL-NEAR F!
  FVARIABLE ABS-NEAR      0E ABS-NEAR F!

\ PRIVATE
(
When FT-EXACT? is TRUE, FT-CONF= uses FT-FDATUM=, otherwise
FT-NEARLY=.
)
  TRUE VALUE FT-EXACT?

: FT~ABS=  ( f: x y -- ) ( -- flag )
(
Leave TRUE if the two floats are equal within the tolerance
stored in FT-ABS-NEAR , else FALSE.
)
  ABS-NEAR F@ F~ ;

: FT~REL=  ( f: x y -- ) ( -- flag )
(
Leave TRUE if the two floats are relatively equal based on the
tolerance stored in FT-REL-NEAR , else FALSE.
)
  REL-NEAR F@ FNEGATE F~ ;

[UNDEFINED] F2DUP  [IF] : F2DUP  FOVER FOVER ; [THEN]
[UNDEFINED] F2DROP [IF] : F2DROP FDROP FDROP ; [THEN]

: FT-NEARLY=  ( f: x y -- ) ( -- flag )
(
Leave TRUE if the two floats are nearly equal, else FALSE.  This
is a refinement of Dirk Zoller's FEQ to also allow x = y,
including both zero, or to allow approximately equality when x
and y are too small to satisfy the relative approximation mode
in the F~ specification.
)
  F2DUP FT-DATUM= IF F2DROP TRUE EXIT THEN
  F2DUP FT~REL=     IF F2DROP TRUE EXIT THEN
  FT~ABS= ;

: FT-CONF=  ( f: x y -- ) ( -- flag )
  FT-EXACT? IF FT-DATUM= ELSE FT-NEARLY= THEN ;

\ PUBLIC

: SET-EXACT  ( -- )   TRUE TO FT-EXACT? ['] FT-CONF= FT-TEST=-XT ! ;
: SET-NEAR   ( -- )  FALSE TO FT-EXACT? ['] FT-CONF= FT-TEST=-XT ! ;

\ *** F.4 ARRAYS
(
The first array slot has index zero.
)

\ PUBLIC

\ USER CONFIG DEFAULTS
[UNDEFINED] MAX-FT-INITIALS [IF] 32 CONSTANT MAX-FT-INITIALS [THEN]
[UNDEFINED] MAX-FT-RESULTS  [IF] 32 CONSTANT MAX-FT-RESULTS  [THEN]

\ Arrays begin with a cell-sized count.
HERE 1 CELLS ALLOT FALIGN MAX-FT-INITIALS FLOATS ALLOT CONSTANT FT-INITIALS
HERE 1 CELLS ALLOT FALIGN MAX-FT-RESULTS  FLOATS ALLOT CONSTANT FT-RESULTS
HERE 1 CELLS ALLOT FALIGN MAX-FT-RESULTS  FLOATS ALLOT CONSTANT FT-GOALS

\ Overflow checking to be done elsewhere.
: FT-A!   ( +n addr -- ) ( f: r_[n-1] ... r_0 -- )
  2DUP ! CELL+ FALIGNED SWAP ( f-addr n) DUP
  IF   FLOATS OVER + SWAP
       DO I F! 1 FLOATS +LOOP 
  ELSE 2DROP THEN ;

: FT-A@   ( addr -- +n ) ( f: -- r_[n-1] ... r_0 )
  DUP @ DUP >R ( addr n r: n)
  IF   CELL+ FALIGNED R@ 1- FLOATS OVER + 
       DO I F@ -1 FLOATS +LOOP
  ELSE DROP THEN R> ;

[UNDEFINED] NIP [IF] : NIP  ( a b -- b)  SWAP DROP ; [THEN]

: FT-A<>  ( addr1 addr2 -- [index+1]|-1|0 )
(
Nonzero return for errors:
  -1       unequal array size
  index+1  deepest unequal array elements
)
  DUP @ SWAP CELL+ FALIGNED >R >R  ( addr1 r: f-addr2 #left )
  DUP @ R@ <> IF 2R> 2DROP DROP -1 EXIT THEN
  ( addr1) DUP CELL+ FALIGNED
  BEGIN  R@ ( addr1 f-addr1 #left)
  WHILE  ( f-addr1) DUP F@ FLOAT+
         R> ( #left) 1- R>
         ( f-addr2) DUP F@ FLOAT+ >R ( #left) >R
         FT-TEST=
  WHILE
  REPEAT ( addr1 f-addr1) DROP @ ( len) 2R> NIP - ( index+1)
  ELSE   ( addr1 f-addr1) 2DROP 2R> 2DROP 0
  THEN ;

\ *** F.5 TESTS

\ PUBLIC

: F{   ( f: <finitials> -- )
(
Save the fp stack <finitials> for restoration by }F.  Clear
<fresults> and <fgoals> storage to put it in a defined state
for }F in case F-> is missing.
)
  FDEPTH DUP MAX-FT-INITIALS > ABORT" TOO MANY FP INITIALS"
  FT-INITIALS FT-A!   0 FT-RESULTS !   0 FT-GOALS ! ;

: F->  ( f: <fresults> -- )
(
Record the depth and contents of the fp stack.
)
  FDEPTH DUP MAX-FT-RESULTS > ABORT" TOO MANY FP RESULTS"
  FT-RESULTS FT-A! ;

: }F   ( f: <fgoals> -- <finitials> )
(
Save the fp stack <fgoals>, compare them with the saved
<fresults>, and restore the <finitials>.  Note that when the
comparison corresponds to SET-FT-MODE-REL, the measured values
must be fresults and the reference values must be fgoals.
)
  FDEPTH DUP MAX-FT-RESULTS > ABORT" TOO MANY FP GOALS"
  FT-GOALS FT-A!
  FT-RESULTS FT-GOALS FT-A<> DUP FT-ERROR-INDEX !
  ?DUP IF -1 =
    IF   S" WRONG NUMBER OF FP RESULTS: "
    ELSE -1 FT-ERROR-INDEX +! S" INCORRECT FP RESULT: " THEN
    FT-ERROR
  THEN
  FT-INITIALS FT-A@ ( len) DROP ;

[THEN]  \ HAS-FLOATING


\ *** TTESTER-XF

\ *** T.1 COMMENTS

\ Set the following flag to TRUE for more verbose output; this may
\ allow you to tell which test caused your system to hang.

  VARIABLE VERBOSE   FALSE VERBOSE !

: TESTING	\ ( -- ) TALKING COMMENT.
  SOURCE VERBOSE @
  IF DUP >R TYPE CR R> >IN !
  ELSE >IN ! DROP
  THEN ;

\ *** T.2 TESTS

\ In the following specifications, references to the fp stack
\ apply when HAS-FLOATING is true.

: T{  ( <xfinitials> -- )
(
Save the data and fp stack <xfinitials> for restoration by }T.
Clear <xfresults> and <xfgoals> storage to put it in a defined
state for }T in case -> is missing.
)
  X{ [ HAS-FLOATING ] [IF] F{ [THEN] ;

: ->  ( <xfresults> -- )
(
Record the depth and contents of the data and fp stacks.
)
  X-> [ HAS-FLOATING ] [IF] F-> [THEN] ;

: }T  ( <xfgoals> -- <xfinitials> )
(
Save the data and fp stack <xfgoals>, compare them with the
saved <xfresults>, and restore the <xfinitials>.
)
  }X [ HAS-FLOATING ] [IF] }F [THEN] ;

BASE !
\ END of ttester-xf.fs
