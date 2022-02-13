.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-


General Introductory Information about the EMTP
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Let this manual begin with a disclaimer as to what it is not!
The current name  "Rule Book"  is a replacement for the older term
"User's Manual"  which was employed for editions date November 1977
and earlier.   The name change was for good reason:  there is a bare
minimum of user instruction in the manual.  So, novice users, do not
expect to read this manual as a tutorial handbook.  **That**, it is **not**!
Instead, it is basically nothing more than a complete book of rules for
EMTP input and output.   And as the modeling capability of the EMTP
expands without any obvious bound (program size is about 70000 UTPF
statements as of the  "M31."  versione of November, 1982), documentation
of the rules begins to resemble more and more the Manhattan Yellow
Pages.   Any beginner needs help of various other forms.

By far the best way to learn about EMTP capability and usage is
by working beside a competent, experienced veteran.   Those power
organizations which have been using the EMTP for a broad range of power
system simulation,  and which have not suffered disabling personnel
changes, represent the ideal learning environment.   If the reader is
working for such an organization,  he should count his blessing,  and
disregard most of what follows.   In case of questions or problems,  go
see the local experts.   Always ask questions (EMTP users have to get
into the habit,  since they will be doing it until the day they retire,
so broad and complex is transient simulation)!

For those not so blessed by local expertes,  the next-best
alternative is a strong national or regional EMTP Users Group.   The
need was first appreciated by the Japanese,  who were organized by
Dr. Akihiro Ametani,  Professor of Electrical Engineering at Doshisha
University (see "Japan" in index for address).   Something like 24
industrial organizations and four universities are involved.   Next
came the Brazilians (see [REF7]_,  Vol. XI,  18 October 1981,  page
SSIA-1),  the Europeans (same volume,  3 October 1981,  pages  IONM-11
through 14),  and most recently the Australians.   EMTP usage has
grown so large that we at BPA simply can not correspond individually
with all EMTP users.   If any user or EMTP-interested party seeks EMTP
information,  we first request that he try his national or regional
organization,  for those parts of the world where these exist.   Only
write or call Portland if the national or regional user support is
unable to answer the question.   Further,  if you do write to Portland,
be sure to send a copy of the letter to the national or regional EMTP
organization,  to keep them informed.   National or regional EMTP user
organizations have been placed on the primary distribution list for
EMTP memoranda ([REF7]_),  so such information is thereby available
before enough memos have accumulated to be bound into a new volume.
Given the existing national or regional organizations,  perhaps the
greatest need now resides very close to home:  the USA and/or Canada.
Are there any volunteers who would provide secondary printing and
distribution of EMTP Memoranda and other printed EMTP materials
in exchange for being placed on the primary distribution list?   Unless
BPA management decides to massively support printing and mailing
activities,  it is to be predicted that available pesonnel will
concentrate on thse who are organized so as to help themselves,  thos
for whom our limited resources can do the most good.   While no promises
can be made for future support of even organized EMTP use groups,  it
seems obvious that the prospects for those who are organized are much
better than for those who are not.   Think about it,  EMTP users!

Availability of program to others
===============================================================================

In conformity with long-standing BPA policy, dissemination of
materials related to the Electromagnetic Transients Program (EMTP, or
T.P. in the older notation) is made freely to any and all interested
parties.  A fee to cover reproduction, handling, and mailinf costs may
be assessed against the organization or individual receiving the
materials, however.  No claim or warranty as to the usefulness,
accuracy, fidelity, or completeness of these materials is, or ever
has been, in any way expressed or implied

It is only for potential program users having the same computer
system as BPA (currently a DEC VAX-11/780 minicomputer, or Apollo micro-
computer) that the EMTP probably can be procured most conveniently


.. raw:: pdf

    PageBreak


Program Maintenance and Development
===============================================================================

Dr. Hermann W. Dommel, founding father of the EMTP, left BPA for
the University of British Columbia in Vancouver during July of 1973.
Primary responsibility for EMTP maintenance and development at BPA
the shifted to Dr. W. Scott Meyer (address and phone number as on the
cover), with Dr. Tsu-huei Liu (same address and phone) added to the
effort in October of 1975.

So much for BPA and ancient history (back when the program was
small, and manageable, and when most of us still nurtured the naive hope
of someday finishing it).   A new phase of EMTP development was begun
during 1975, after the discovery of machine translation (see `REF13`_)
the cooperative development among major program users, for the mutual
benefit of all.  Very substantial contributions to the present EMTP have
been made by non-BPA personnel (though BPA funding has supported most
work at other than power companies).  Three othe North American power
companies have been actively involved in development, as have four
universities:

#. The original dynamic synchronous machine (S.M.) of the
   EMTP was contributed by the Southern California Edison Co.
   (SCE) of Rosemead, California (the Los Angeles area).  Mike
   Hall and Johm Alms did the development work there, as
   documented in [REF8]_, Volume VI, 5 December 1976, pagination
   FOTS.  Much of the theoretical basis came from Dr. George
   Gross of Pacific Gas & Electric (San Francisco).  That original
   code was considerably enhanced since its 1976 introduction
   to the EMTP, thanks to Dr. Vladimir Brandwajn (originally a
   student of Prof. Dommel, thena a contractor of BPA for about
   a year.  Between 1978 and 1982, Dr. Brandwajn handled all
   EMTP cosiderations related to the conventional S.M.
   code, whether it be the original SCE code, or his own (Type-59)
   modeling.  Before moving to System Control in 1982, Vladimir
   removed the Type 50 (original SCE) modeling.

#. The Semlyen recursive convolution modeling of Section 1.27a
   comes from Ontario Hydro (OH) of Toronto, Ontario (Canada).  It
   is named after Professor Adam Semlyen of the University of
   Toronto, who inspired it (see [REF10]_ and [REF21]_).  Alex Dabuleanu
   (now deceased;  formerly an OH employee) did much of the
   original work, as a student of Prof. Semlyen.  Through the end
   of 1979, Russel Brierley of OH continued to be the premier
   expert in use of the feature, and researcher who paved the way
   for generalization to complex exponentials (theory as per [REF21]_
   by Prof. Semlyen).  Bob Eifrig of Oregon State University
   (and a temporary BPA employee for several summers) restructured
   the original Semlyen code during the summer of 1976 (variable
   dimensioning, etc.).  Then during the summer of 1978, Bob
   completely recoded the solution code, to allow for an arbitrary
   number of complex exponentials, followinf the Ontario Hydro
   recommendation.  Bob did work on the associated new "SEMLYEN
   SETUP" (prototype which came from Russ Brierly and Vladimir),
   though a workable production tool never resulted.  See Point 9
   below.

#. The dynamic surge arrester for SiC devices with active current
   limitating gaps as described in Section 1.34 was developed and
   contributed by Chuck Wolf and Dr. Arun Phadke of the American
   Electric Power (AEP) Service Corporation of New York City.

#. The original mathematical formulation and crude outlines of
   TACS (see Section 8.), which is used for control system
   modeling, was developed by Laurent Dube, studying under the
   supervision of Prof. Hermann W. Dommel at the University of
   British Columbia in Vancouver.  See [REF12]_.  Implementation
   and enhancement in the EMTP were made by Laurent working under
   contract with BPA.  This included the valve and diode modeling
   of Section 1.43.  Laurent's first BPA contract ended in the
   summer of 1976, and a second began in the spring of 1979, as
   a second generation of TACS is being developed.  How much of
   the new goodies make the general distribution during the
   summer of 1980 is not now (March, 1980) clear.

#. Professor Akihiro ("Aki") Ametano (now deceased January 2022) of
   Doshisha University in Kyoto (Japan) developed the "CABLE CONSTANTS"
   code of Section 7.7;  and he also inspired the "Ametani linear
   convolution" modeling (see [REF11]_), the implementation of which
   was designed and performed by Bob Eifrig.  Yet this UTPF overlay
   46 was removed during 1983.

#. The Type-96 hysteretic inductor modeling of Section 1.31 was
   developed by Prof. Narendra ("Ned") Mohan and Mr. Jim Frame of
   the University of Minnesota in Minneapolis.  This was during
   1978 and 1979, under contract with BPA.

#. The universal machine (U.M.) modeling of Section 1.63 was
   developed by Prof. Hian Lauw of Oregon State University in
   Corvallis, Oregon.  This was during 1979 and 1980, under
   contract with BPA.

#. The Type-59 dynamic synchronous machine (S.M.) modeling of Section 1.62
   was originally researched by Dr. Vladimir Brandwajn while studying for his
   doctorate in Vacouver (another of Prof. Dommel's graduate students).  See
   [REF15]_.  Implementation in the EMTP was under contract with BPA (see [REF8]_,
   Vol. VII, 23 December 1977, pagination CBVB).  Between 1978 and 1982, Vladimir
   was with Ontario Hydro, and has since moved to Systems Control, Inc. of Palo
   Alto, California.

#. Dr. John Hauer of BPA joined the EMTP development effort during the
   late fall of 1979, to rescue our suspended "SEMLYEN SETUP" work.  See [REF8]_,
   Vol. IX, 4 October 1979, page EDTO-1.  Rather than correcting past work, he
   added is own self-contained, sophisticated, frequency-domain fitting program
   that has evolved over the past decade or two.  This "HAUER SETUP" code of UTPF
   overlays 48 and 49 existed until 1983, when it was removed ("MARTI SETUP" is the
   general replacement).

#. Prof. Jose R. Marti of Central University of Venezuela (in Caracas)
   finished his doctoral study under Prof. Hermann Dommel at UBC in 1981.  Jose's
   dissertation concerned a new, simplified procedure for the frequency-dependent
   representation of transmission lines in a transients program.  Implementation in
   the EMTP began in August of 1981, and continued through the following summer,
   under contract with BPA.  See Sections 1.26b1 and 7.0.

#. Ma Ren-ming from the Wuhan High Voltage Institute (Wuhan, China) moved
   to Portland during March 1982, and has been an integral part of the program
   support team ever since.  During late 1983 and early 1984, he made very
   extensive improvements to TACS modeling.  Other projects with which Ma has been
   closely involved are the new EMTP switch logic, and refinement of the EMTP load
   flow.  When Ma Ren-ming returns home during the summer of 1984, his presence
   will be sorely missed.  For the first two years of his stay, salary, travel, and
   living expenses were all provided by his institute, for which all users should
   be grateful.

#. Frank Rasmussen of Elkraft Power Corporation in Denmark did background
   research, and delivered the initial workable code for what is now referred to as
   the EMTP load flow.  See [REF8]_, Vol. XIII, 23 July 1983, Section II, pages
   VDEL-2 through 9.

Hence there is quite a bit of EMTP competence which is spread among many
individuals.  The effort was long ago bigger than any one person, group of
persons, or even power pool.  Industry-wide development on an international
scale has been involved.  The coordination has been centered largely in Portland
(at BPA), if only by default.  But then the possibility of better-coordinated
and better-funded development motivated the establishment of the EMTP
Development Coordination Group (DCG) in the fall of 1982.  As of April, 1984,
DCG support for S.M. and U.M. contractors (Dr. Brandwajn and Prof. Lauw) has
resulted in obvious, observable, deliverable improvements to the program, and
would seem to be DCG's prime accomplishment.

A new EMTP user is encouraged to seek assistance within his own company
when using the program for the first time, or when using it in a new mode.  If
there are questions as to program and/or problem restrictions, an inquiry to
experienced personel is almost always advisable.  The EMTP and associated
problem modeling are inimitably tricky and sophisticated, with experience
invaluable in order to avoid experimentation of a trial and error (garbage in,
garbage out) nature.  For those who are able to spend corporate funds, and
travel of centers of EMTP education, I strongly recommend any course sponsored
by "EMTP insiders".


.. raw:: pdf

    PageBreak


Program Capability (Summary)
===============================================================================

The Transients Program is used to solve the ordinary differential and/or
algebraic equations associated with an "arbitrary" interconnection of the following
elements:

#. Lumped resistance:  v = Ri

.. image:: images/o-resistor.png
   :alt: resistor
   :align: center
   :width: 25%

#. Lumped inductance:  v = L di/dt

.. image:: images/o-inductance.png
   :alt: inductance
   :align: center
   :width: 25%

#. Lumped capacitance: i = C dv/dt

.. image:: images/o-capacitance.png
   :alt: capacitance
   :align: center
   :width: 12%


#. Multiphase Pi-equivalents, where the preceding scalar
   R, L, C become symmetric square matrices [ R ], [ L ],
   [ C ].

.. image:: images/o-pi-equivalent.png
   :alt: pi-equivalent
   :align: center
   :width: 66%

#. Multiphase distributed-parameter transmission lines, wherein propagation
   time of the line is represented.  Distortionless and externally-lumped-
   resistance approximations are available, as well as "exact" frequency-
   dependent representations.

.. image:: images/o-trellis.png
   :alt: trellis
   :align: center
   :width: 12%

#. Nonlinear resistors, where the curve must be single-valued.

.. image:: images/o-nl-resistance.png
   :alt: non linear resistance
   :align: center
   :width: 25%

#. Nonlinear inductors, either with the conventional single-valued
   characteristics (see sketch at right), or including hysteresis.

.. image:: images/o-nl-inductance.png
   :alt: non linear inductors
   :align: center
   :width: 25%

#. Time-varying resistance.

.. image:: images/o-tv-resistance.png
   :alt: time varying resistance
   :align: center
   :width: 25%

#. Switches, used to simulate circuit breakers, lightning-arrestor
   flashover, or any other network connection change.  Diodes and
   dc converter valves are included.

.. image:: images/o-breaker.png
   :alt: breaker
   :align: center
   :width: 25%

.. image:: images/o-valves.png
   :alt: valves
   :align: center
   :width: 25%

#. Voltage or current sources.  In addition to standard mathematical
   functions (sinusoids, surge functions, steps, and ramps), the user
   may specify sources point by point as functions of time, or in
   FORTRAN, or as defined by TACS (see Point 12 below).

.. image:: images/o-sources.png
   :alt: sources
   :align: center
   :width: 33%

#. Dynamic synchronous machines (3-phase balanced design only).  The
   electrical side is represented by Park's (Blondel's) equations,
   while the mechanical side is modeled as an interconnection of
   masses, springs, and damping.  Arbitrary exciter and governor
   dynamics can be represented, by connection to TACS (see Point 12).

.. image:: images/o-synchronous-machine.png
   :alt: synchronous machine
   :align: center
   :width: 25%

#. Control system dynamics, as are normally represented on differential
   analyzers (analog computers).  This modeling capability goes by the
   name of TACS (ans acronym for Transfient Analysis of Control Systems).
   Nonlinear and logical operations may be represented.  Input and
   output may be interfaced with the electric network of the EMTP,
   providing a hybrid representation.  All TACS representation is
   user-patchable, and hence configuration free.  See Sect. 8.

.. image:: images/o-control-system.png
   :alt: control system
   :align: center
   :width: 50%

#. Unconventional rotating electromechanical energy converters of
   various sorts, including induction machines and dc machines.
   An arbitrary number of windings on each rotor axis is allowed,
   there is no restriction to 3-phase usage etc.  Compensation is
   used, and any mass-spring dynamics of the shaft are represented
   by an electrical analog.  Various control system connections
   are possible, via TACS.  Refer to Section 1.63 (the universal
   machine model).

Trapezoidal-rule (second order) implicit integration is used on the describing
equations of most elements which are described by ordinary differential equations.
The result is to form an associated set of real, simultaneous, algebraic equations
which must be solved at each step (see [REF1]_).  These are placed in nodal-
admittance form (with new unknown voltages as variables), and are solved by ordered
triangular factorization ([REF4]_).

Program output consist of component variables (e.g., branch currents or
voltages, machine torques or speeds, etc.) as functions of time, for those variables
which were requested by the user.  Both printed and plotted output is possible, with
plotting possible in either character or vector-graphic modes.  See Section 5.0 for
the separate, disconnected usage of vector plotting, and Section 9.0 for the details
of interactive (SPY) usage.

Initial conditions for differential equations of the various components can be
determined automatically by the program for many if not most cases of practical
interest.  The most important restriction is to linear elements (nonlinear
components must generally be ignored during phasor steady-state solutions).  Yet
injections of the electric network may be specified in terms of power and voltage
magnitude, thereby providing multi-phase load flow capability.  Control system
modeling (TACS) allows for the superposition of an arbitrary number of linear phasor
solutions of different frequencies.


.. raw:: pdf

    PageBreak




.. raw:: pdf

    PageBreak


Program Availability on Different Computer Systems and Installations
===============================================================================

The EMTP is being made available for execution on the different major
American computer systems by means of machine translation of an installation-
independent master file known as the Universal Transients Program File
(abbreviated UTPF).  Conceived of in November of 1974 (see [REF13]_), this
scheme utilizes a diffrerent Editor/Translator (E/T) program for each different
computer system, so as to machine-process the UTFP, to convert it into legal
EMTP FORTRAN for the particular installation of interest.  Thus all EMTP code
actually begins with the same master file (the UTFP), but differs according to
the built-in or specially-requested properties of the translation.  Within this
framework, the writing of a common EMTP User's Manual for all is thus a little
bit tricky.

Some differences of the EMTP code for different computer systems are
completely hidden, out of sight of the program user, and are of no concern to
him.  For example, alphanumeric storage (e.g., the 6-character named for network
busses) on Univac is handled in FORTRAN INTEGER variables, while on IBM the
mode is REAL*8.  This is a concern of the program developers only, not of
any normal interest to the user.  Reference 13 summarizes the entire process.

Other machine differences affect the user in only a minor way, and he may
not even initialy be aware of them.  For example, computer word length
dictates certain precision or other numerical requirements on the input data.
Control Data with its big 60-bit word is more tolerant of a wide disparity of
input data than is Univac with its 36-bit word, for example.  Here we are
talking about limits which exist for all EMTP users, but which vary in severity
or value according to the computer installation being considered.  Relevant
comments about such considerations will be found in the user instructions,
where appropriate.  The user should always be aware of the computer word length
used in his EMTP translation, needless to say.

Finally, certain EMTP operations are totally different for different
installations, at least in outward appearance, as far the user is concerned.
For example, computers of different manufacture (IBM, CDC, Univac, SEL, etc.)
will require completely different job control cards, in general.  There are the
instructions by means of which one pleads with the installations operating
system, asking for the EMTP, manipulating his data input files and the program
output.  Such job control language (JCL) may even vary among different
installations of the same manufacturer, due to local preferences or costraints
which are placed on the mode of operation.  If in doubt as to what to employ,
the use should always contact his resident EMTP expert for the last word.  Yet
as a general guide, the following "system" instructions as to EMTP setup are
typical, for the different computer system indicated.  Refer to Section 1.5a
onward --- that section applicable to the computer system of interest.

As summarized in [REF13]_, FORTRAN statements which are highly
dependent on computer manufacturer and/or installation usage have been
isolated in installation-dependent EMTP modules.  If a given organization
performs its own translation, such modules will be set up to satisfy its own
peculiar needs, and Program Maintenance of that organization should
thorougly understand the decisions which have been made; in this case, there
should be no complication at all.  But, if a given organization receives a
FORTRAN copy of the EMTP from some other group which uses a computer of the
same manufacture, then perhaps nothing will be known about installation-
dependent modules and the installation-dependent choices which may have been
made therein.  In order to be able to change some of these decisions
if need be, user must know how to locate the modules of interest.
The following chart shows all installation-dependent EMTP modules by
name and also overlay numbers.  The purpose is summarized on the
right (for more description, consult comment cards at the top of the
module in the UTPF [which contains VAX copies]).


.. raw:: pdf

    PageBreak


+-----------------+---------------------------+--------------------------------------------+
| **Module Name** | **UTPF overlay name and** | **Purpose of installation-dependent code** |
+=================+===========================+============================================+
||    EREXIT      ||       MAIN00; -1         | Once used for error recovery (mnemonically |
|                 |                           | "ERror EXIT") on BPA's old CDC-6500, for   |
|                 |                           | five years this was a dummy module.  Then, |
|                 |                           | with interactivity, it was used again.     |
|                 |                           | The UTPF has a VAX module with a call to   |
|                 |                           | the machine-dependent CTRL-C handler, so   |
|                 |                           | all other computers will substitute for it.|
+-----------------+---------------------------+--------------------------------------------+
||    RUNTYM      ||       MAIN00; -1         | Find the current central processor and     |
|                 |                           | input/output job times.  Basically this is |
|                 |                           | used only for the elapsed-time printout of |
|                 |                           | of the case-summary statistics.            |
+-----------------+---------------------------+--------------------------------------------+
||    TIME44      ||       MAIN00; -1         | Find the current wall-clock time, in format|
|                 |                           | "HH.MM.SS".  Automatic plot file naming    |
|                 |                           | (for those systems having OPEN/CLOSE       |
|                 |                           | capability of FORTRAN) is based on the     |
|                 |                           | digits of this time, and also on "DATE44"  |
|                 |                           | results.                                   |
+-----------------+---------------------------+--------------------------------------------+
||    CIMAGE      ||       MAIN00; -1         | Read the next card from unit LUNIT5.       |
|                 |                           | DECODE it as 80A1 if free-format is used.  |
|                 |                           | $-card, all of which are processed within  |
|                 |                           | "CIMAGE".  Skip over comment cards (after  |
|                 |                           | interpreting).                             |
+-----------------+---------------------------+--------------------------------------------+
||     LOCF       ||       MAIN00; -1         | Find the address in memory of the argument,|
|                 |                           | as a number of REAL words. This is used by |
|                 |                           | the EMTP to find the size of certain fixed-|
|                 |                           | dimension arrays (not all code is variably-|
|                 |                           | dimensioned).                              |
+-----------------+---------------------------+--------------------------------------------+
||    LOCINT      ||       MAIN00; -1         | Find the address in memory of the argument,|
|                 |                           | as a number of INTEGER words.  This is a   |
|                 |                           | scaled version of "LOCF", fundamentally.   |
+-----------------+---------------------------+--------------------------------------------+
||    RFUNL1      ||       MAIN00; -1         | All library functions are defined using    |
|     RFUNL2      |                           | ENTRY points in these modules, for those   |
|     RFUNL3      |                           | systems allowing ENTRY usage.  Neutral     |
|     CFUNL1      |                           | names are used (e.g., "SINZ" or "DSIN").   |
|                 |                           | Special limit checking can also be placed  |
|     CMPLXZ      |                           | in these modules.                          |
+-----------------+---------------------------+--------------------------------------------+
||    DLIBRF      ||       MAIN00; -1         | Used to provide double-precision library   |
|                 |                           | functions, originally just for overlay 13, |
|                 |                           | for use by the Semlyen recursive           |
|                 |                           | convolution code.                          |
+-----------------+---------------------------+--------------------------------------------+
||    DLIBR2      ||       MAIN00; -1         | Used to provide double-precision library   |
|                 |                           | functions of 2 arguments.                  |
+-----------------+---------------------------+--------------------------------------------+
||    FRENUM      ||       MAIN00; -1         | Returns a floating-point number from the   |
|                 |                           | next free-field range on the data card     |
|                 |                           | which is currently being processed by the  |
|                 |                           | universal module "FREFLD".                 |
+-----------------+---------------------------+--------------------------------------------+
||    PACKA1      ||       MAIN00; -1         | Packs one character (A1 information) of    |
|                 |                           | one word into any character position of    |
|                 |                           | another words.  Both words must be         |
|                 |                           | ALPHANUMERIC (UTPF type).                  |
+-----------------+---------------------------+--------------------------------------------+
||    PACKCH      ||       MAIN00; -1         | Packs A4 or A6 word strings into           |
|                 |                           | ALPHANUMERIC vector storage so that there  |
|                 |                           | are no imbedded blanks.  This was          |
|                 |                           | originally designed for CalComp plotting,  |
|                 |                           | to remove excess blanks.                   |
+-----------------+---------------------------+--------------------------------------------+
||    SEEDY       ||       MAIN00; -1         | Find the number of seconds since midnight, |
|                 |                           | based on alphanumeric input of the time    |
|                 |                           | ("HH.MM.SS").                              |
+-----------------+---------------------------+--------------------------------------------+
||    RANDNM      ||       MAIN00; -1         | Compute a random number (roll the dice),   |
|                 |                           | uniformly distributed ove the unit interval|
|                 |                           | (0, 1).  This is for zero argument.  For   |
|                 |                           | nonzero argument, initialize the random    |
|                 |                           | number generator using this seed.  "RANDNM"|
|                 |                           | also has access to standard random numbers |
|                 |                           | of "SANDNM" if user-requested.             |
+-----------------+---------------------------+--------------------------------------------+
||    TAPSAV      ||        MAIN10; 0         | This module is called by the universal     |
|                 |                           | "TABLES" to dump/restore /LABEL/ as part of|
|                 |                           | "START AGAIN", "STATISTICS", etc. usage.   |
|                 |                           | The UTPF module (VAX code) assumes COMMON  |
|                 |                           | blocks are in natural or reverse order, so |
|                 |                           | they can very easily be transferred by a   |
|                 |                           | single self-indexed READ/WRITE following   |
|                 |                           | LOCINT location of the ends.  Computers    |
|                 |                           | without such regular order require "TAPSAV"|
|                 |                           | module produced by "VARDIM" (with a        |
|                 |                           | separate READ/WRITE for each COMMON block).|
+-----------------+---------------------------+--------------------------------------------+
||     PLTFIL     ||        MAIN10; 0         | For installation-dependent transfer of     |
|                 |                           | output vector to disk as part of plot-file |
|                 |                           | building on LUNIT4.  Conversion to single- |
|                 |                           | precision (assuming EMTP computation uses  |
|                 |                           | REAL*8) is a common function, to save disk |
|                 |                           | and I/O time.  Interactive EMTP versions   |
|                 |                           | service the ROLLing "PLOT" command of SPY  |
|                 |                           | from here, too.  Module is called only if  |
|                 |                           | M4PLOT is nonzero (1=SPY MOS, 2=REAL*4     |
|                 |                           | disk).                                     |
+-----------------+---------------------------+--------------------------------------------+
||     PLTLU2     ||        MAIN10; 0         | Special version of "PLTFIL" used only for  |
|                 |                           | TACS "STAND ALONE" cases.  Module is called|
|                 |                           | only if M4PLOT .NE. 0.                     |
+-----------------+---------------------------+--------------------------------------------+
||     VECRSV     ||        MAIN10; 0         | Vector dumping/restoring modules used      |
||     VECISV     |                           | during the overlay 6-11 steady state phasor|
|                 |                           | solution and node renumbering.  "VECRSV" is|
|                 |                           | for REALs, while "VECISV" is for INTEGERs. |
|                 |                           | UTPF (VAX) modules are installation-       |
|                 |                           | dependent since they rely on virtual       |
|                 |                           | storage (/C29B01/).  See [REF8]_ Vol. XII, |
|                 |                           | 24 August 1982, Section II, pages HTNT-4   |
|                 |                           | through 8.                                 |
+-----------------+---------------------------+--------------------------------------------+
||     VECRXX     ||        MAIN10; 0         | Near-universal versions of "VECRSV" and    |
||     VECIXX     |                           | "VECISV", or so we thought (see [REF8]_,   |
|                 |                           | Vol. XII, 20 January 1983, Section III-A,  |
|                 |                           | pages MVEM0-16 and 17.                     |
+-----------------+---------------------------+--------------------------------------------+
||     SYSDEP     ||        OVER1; 1          | Performs various system-dependent          |
|                 |                           | initializations at the start of execution  |
|                 |                           | of a new EMTP data case.                   |
+-----------------+---------------------------+--------------------------------------------+
||     MIDOV1     ||        OVER1; 1          | Performs miscellaneous system-dependent    |
|                 |                           | initializations when EMTP control is ready |
|                 |                           | to exit "OVER1".  The call to "SYSDEP" is  |
|                 |                           | too early to perform all system-dependent  |
|                 |                           | initializations, it turns out.             |
+-----------------+---------------------------+--------------------------------------------+
||     NAM999     ||        OVER1; 1          | Installation-dependent module which builds |
|                 |                           | default names for linear branches (LIN001, |
|                 |                           | etc.), nonlinear elements (NLN001, etc.),  |
|                 |                           | nonlinear elements (NLN001, etc.), and     |
|                 |                           | switches (SWT001, etc.).                   |
+-----------------+---------------------------+--------------------------------------------+
||     DATE44     ||        OVER1; 1          | Find the calendar date ("MM/DD/YY").       |
+-----------------+---------------------------+--------------------------------------------+
||     PFATCH     ||        OVER1; 1          | Attach (connect) a disk file of plot points|
|                 |                           | to I/O unit LUNIT4, in conjunction with    |
|                 |                           | "REPLOT" usage of Section 1.0d.  "START    |
|                 |                           | AGAIN" also requires this module (Section  |
|                 |                           | 1.0e15).                                   |
+-----------------+---------------------------+--------------------------------------------+
||     ANALYT     ||        OVER16; 16        | The module which services "ANALYTIC SOURCES|
|                 |                           | USAGE" modeling (user-defined FORTRAN).    |
|                 |                           | Also, "EMTPSPY" of interactive control uses|
|                 |                           | "ANALYT" to honor the "RAMP" command (see  |
|                 |                           | [REF8]_, Vol. XI, 17 July 1981, page       |
|                 |                           | IEEE-35).                                  |
+-----------------+---------------------------+--------------------------------------------+
||     KATALG     ||        OVER20; 20        | Save the contents of unit number LUNIT4 as |
|                 |                           | a permanent file on disk, for possible     |
|                 |                           | later "TPPLOT" or "REPLOT" usage.  But such|
|                 |                           | usage is restricted to those systems (e.g.,|
|                 |                           | BPA-modified CDC) which permit file naming |
|                 |                           | after the creation.  This is rare (VAX OPEN|
|                 |                           | usage of "SYSDEP" is common).              |
+-----------------+---------------------------+--------------------------------------------+
||     FLAGER     ||        OVER20; 20        | Installation-dependent SPY modules. For    |
|      TDELAY     |                           | further details, see Section 9.0 on        |
|      KWITER     |                           | interactivity.  Variations are associated  |
|      SPYAID     |                           | with details of the user-keyed interrupt   |
|      WINDOW     |                           | (FLAGER and KWITER), the audible bell      |
|      APPEND     |                           | (HONKER), hibernation (TDELAY), user       |
|      PROMPT     |                           | instructions (SPYAID), window management   |
|      TEKPLT     |                           | and usage (WINDOW), installation-dependent |
|      SYMTEK     |                           | extensions (APPEND), cursor holding        |
|      TGRID      |                           | (PROMPT) and vector plotting (TEKPLT,      |
|      HONKER     |                           | SYMTEK, and TGRID).                        |
+-----------------+---------------------------+--------------------------------------------+
||     DATAIN     ||        OVER20; 20        | FORTRAN 77 module for EMTP data            |
|                 |                           | modularization and sorting, called by      |
|                 |                           | "EREXIT" in VAX (UTPF) module.             |
+-----------------+---------------------------+--------------------------------------------+
||     STATRS     ||        OVER29; 29        | This module serves to connect to units     |
|                 |                           | LUNIT3 and LUNIT9 the "STATISTICS" results |
|                 |                           | which were previously saved on disk by     |
|                 |                           | STATSV.  This is in response to "TABULATE  |
|                 |                           | ENERGIZATION RESULTS" request of Section   |
|                 |                           | 1.0e6.                                     |
+-----------------+---------------------------+--------------------------------------------+
||     BEGPLT     ||        OVER31; 31        | Module is called by the main plotting      |
|                 |                           | module SUBR31 before any CalComp plotting  |
|                 |                           | is done, for each data case.  Arbitrary    |
|                 |                           | system-dependent intialization is possible.|
+-----------------+---------------------------+--------------------------------------------+
||     ENDPLT     ||        OVER31; 31        | Module is called by the main plotting      |
|                 |                           | module SUBR31 after all plotting of a given|
|                 |                           | data case is completed.  Arbitrary system- |
|                 |                           | dependent initialization is possible.      |
+-----------------+---------------------------+--------------------------------------------+
||     FINTP      ||        OVER31; 31        | Module is called immediately before the one|
|                 |                           | and only (almost) STOP statement of the    |
|                 |                           | EMTP.  Arbitrary program termination       |
|                 |                           | operations (e.g., spooling, file closing,  |
|                 |                           | removal of carriage control characters,    |
|                 |                           | etc.) can be performed.                    |
+-----------------+---------------------------+--------------------------------------------+


Several specific modifications will be of concern to many installations,
particularly those which are not on the U.S.A. (where conventions are different).
Included are the following:


Power system (synchronous; steady state) frequency
-------------------------------------------------------------------------------

The steady-state frequency of power system operation is defined within
module "SYSDEP" of overlay number 1.  Variable "STATFR" should be assigned
this frequency in Hertz (equal to 60.0 for usal usage within the United
States).


Calendar date format
-------------------------------------------------------------------------------

As set up for usage in the States, "MM/DD/YY" is printed, where:

| "MM" ---- two decimal digits for the month (e.g. "03" for March);
| "DD" ---- two decimal digits for the day within the month;
| "YY" ---- two decimal digits for the year (e.g. "77" fot 1977).

In most other parts of the world (including Canada, as I recall), and even in
the U.S. Army, I believe that it is common usage for the day "DD" to precede
the month "MM".  If module "DATE44" (see above chart) is altered so as
to produce "DD/MM/YY", the the associated format within subroutine
"SYSDEP" (which is used for one or two lines of EMTP heading) should be
changed accordingly.


Batch-mode plotting parameters
-------------------------------------------------------------------------------

Several parameters which are related to batch-mode EMTP plotting
(Section 1.10) are defined in module "SYSDEP" (see above chart).
The following might be varied, from one installation to another:

SZPLT
 Height of CalComp plotting paper which is beign used,
 in inches.  Or more precisely, this is maximum
 vertical excursion of the pen (any margin and sprocket
 holes thus are not to be counted).  Recall that the use
 is able to over-ride this default value at execution time,
 using a "PLOTTER PAPER HEIGHT" card of Section 1.0c.

SZBED
 Maximum length of the plot, in inches.  For BPA CDC usage,
 this is set equal to 72.0 (about the size of our
 EAI flatbed surface).  This parameter is used as
 an argument of the overlay #31 call to subroutine
 "PAPRSZ" (whose job it is to protect against illegal
 excursions of the pen).

LNPIN
 The number of lines per inch of the line printer.
 Recall that the scaling of a line printer plot will depend
 on this figure.  The user is able to over-ride the
 default value at execution time, using a "PRINTER
 LINES PER INCH" card of Section 1.0c.

NSMTH
 The number of successive ups and downs before averaging
 of successive points is resorted to, for plot purposes.
 Recall that the user is able to over-ride this default value
 at execution time, using a "LIMIT ON PLOT OS" card
 of Section 1.0c.

LLBUFF
 Variable which is used as the second argument of the
 call to CalComp subroutine "PLOTS" ---- to be found
 within module "BEGPLT" (see above chart) of overlay #31.
 As originally used by CalComp a decade or so ago, this
 was the buffer length of the "LUNIT8" output channel
 on which plotter instructions will be written.  But many
 installations no longer use such a specification (e.g.,
 the buffer may be automatically set by the system, or it
 may be defined by the job control language (JCL) cards).

 The first executable statement of the program, located
 in "MAIN00" (overlay number -1) and never executed
 again, is the assignement of value -3333 to "LLBUFF".
 The first time through "SYSDEP", this is converted
 to a positive value.  Module "BEGPLT" of overlay #31
 then could append a minus sign, as a flag that "PLOTS"
 has be called once, and is not to be called again --- if
 this is the desired usage.  Remember, batch-mode plotting
 is done by a primary-level overlay, if the program is
 overlaid.  For BPA CDC, this requires that "PLOTS"
 be called each time the overlay is used (for each data case
 which uses CalComp plotting).  This seems to be quite
 installation dependent.


Input/output unit numbers
-------------------------------------------------------------------------------

Unless explicitely altered to the contrary by Program Maintenance which
sets up the translation, the following input/output (I/O) unit assignments
will be assumed:

LUNIT5 = 5
 card reader (EMTP input data cards).

LUNIT6 = 6
 line printer (EMTP printed output).

LUNIT7 = 7
 card punch (for EMTP punched-card ouput).

LUNIT8 = 8
 machine-language instructions for plotting hardware
 (as generated by calls to the CalComp subroutines
 during the overlay #31 batch-mode plotting).

LUNIT1 = 1
 scratch tape; very small buffer will suffice
 (for BPA CDC, 64 decimal word were used).

LUNIT2 = 2
 scratch tape which is used for dumping most of /BLANK/
 and /LABEL/, to be read back into central memory
 for each new energization of a "STATISTICS" or
 "SYSTEMATIC" data case.  A big buffer is recommended
 (for BPA CDC, a buffer of 1024 decimal words was used).

LUNIT3 = 3
 like "LUNIT1".

LUNIT4 = 4
 scratch tape which is used for storage of the raw data
 points of the plot file (later to be plotted).  A good
 size buffer is recommended (for BPA CDC, we used 512 decimal
 words).

LUNIT9 = 9 ... LUNIT15 = 15
 like "LUNIT1".


If any of these usages are illegal or inconvenient at the installation of
interest, alternate assignments should be made within module "SYSDEP" (see
above chart).  PRIME is one such system, for which some re-assignements had to
be made.  Actually, as of April, 1980, I do not believe that "LUNIT10"
through "LUNIT16" are actually being used for anything other than possibly
the "HAUER SETUP" code of overlays 48 and 49.


Use of OPEN/CLOSE statements to manage disk files
-------------------------------------------------------------------------------

Most new compilers allow internal (within FORTRAN code) disk file
connection and disconnection via OPEN and CLOSE statements.  Yet
the details difffer from machine to machine, so all such usage is
confined to installation-dependent EMTP modules.  The following is an
explanation of this usage by functional classification (feature by
feature).  The VAX modules, which presently occupy positions in the
UTPF, are used for purpose of illustration.


LUNIT4 file of raw plot data points (SYSDEP, KATALG, PFATCH
-------------------------------------------------------------------------------

The integer miscellaneous data card (Section 1.0h) defines variable
ICAT which indicates wheter or not the user wants to save the raw
plot data points on LUNIT4 once all EMTP processing of a data case is
complete.  Variables ICAT and LUNIT4 are in deck BLKCOM, so the
construct "INSERT DECK BLKCOM" will make them available in any modules
which might be written.

VAX is typical of most computer systems which require that a file
be opened before it is written on.  For this reason, OPENING of the
LUNIT4 files is done within "SYSDEP" of overlay one, even though
variable ICAT is not known that early.  After the determination of
the date and the time, we build a legal file name using these, and
OPEN the file under the assumption that it will be saved:

.. code:: fortran

   CALL DATE44 ( DATE1(1) )
   CALL TIME44 ( TCLOCK(1) )
     < <  Build legal VAX/VMS file name using the digits
          of DATE1 and TCLOCK; put result in FILE25      > >

.. code:: fortran

    OPEN (UNIT=LUNIT4, TYPE='NEW', NAME=FILE25,
   1       FORM='UNFORMATTED')

The TYPE='NEW' specification indicates that we are to create another
(a new) disk file, as opposed to the connection of an existing file.
In building file name FILE25, remember that DATE1(2) and TCLOCK(2)
are ALPHANUMERIC vectors of /BLANK/ ---- REAL*8 for IBM, VAX, PRIME,
SEL, etc.; INTEGER for Burroughs, Univac, CDC; REAL*6 for Harris, etc.
But before such OPENing of a new file, we ask whether the plot file
of the preceding solution really was to be saved permanently; if so,
it is saved; if not, it is deleted.  Thus, at the top of "SYSDEP"
(before the just-listed code) one sees:

.. code:: fortran

       IF (ICAT .EQ. 0) GO TO 120
       IF (ICAT .GT. 2) GO TO 120
   100 CLOSE (UNIT=LUNIT4, DISPOSE='SAVE')
   140 CONTINUE

Here the ICAT which is being used is left over from the precedeing
solution, note (no EMTP data of the upcoming case has yet been read).

The use of FORM='UNFORMATTED' deserves mention.  All WRITEs to
LUNIT4 will be binary (unformatted), and this declaration in the OPEN
statement merely reflects that nature.  Most computer systems do not
make such a distinction between FORMATTED and UNFORMATTED I/O (VAX
was the first we had heard of), fortunately, so the user can ignore
this detail.  For LUNIT4 this is no special complication, since I
can to recall another FORMATTED use of the same channel.  But for
other I/O units, different EMTP features can use different modes,
and OPENing and CLOSEing has become conditional on /BLANK/ variables
(but that need not concern us here).

One use for previously-saved LUNIT4 plot files is batch-mode EMTP
plotting at some late time via a "REPLOT" request (see Section 1.0d of
the Rule Book).  The disk file in question is connected by a call to
installation-dependent "PFATCH" (mnemonically, "permanent file attach")
which is found in "REQUES":

.. code:: fortran

   M28.1295C      $$$$    SPECIAL-REQUEST WORD NO. 4.   'REPLOT'
   M28.1296 8004 IF (NOUTPR .EQ. 0)
   M28.1297     1 WRITE (LUNIT6, 3364)
   M28.1298 3364 FORMAT (  34H+REQUEST TO RE-PLOT OLD PLOT DATA.     )
   M28.1299      DEGMAX = 0.0
   M28.1300      IALTER = LUNIT4
   M28.1301      CALL MIDOV1
   M28.1302      CALL PFATCH
   M28.1303      NCHAIN = 31
   M28.1304      GO TO 5617

The I/O unit number of the connection is carried through variable
IALTER of /BLANK/, and that jump to S.N. 5617 provides a transfer
to overlay NCHAIN = 31 plotting.  As for "PFATCH", the rules
associated with extracting the requested file from the remainder of the
"REPLOT" data card are quite arbitrary and discretionary.  For VAX,
we chose to be quite restrictive, for simplicity.  We require that a
"REPLOT" data card use EMTP free-format (with a comma after the key
word "REPLOT", in column 7), followed by the legal VAX/VMS disk
file name.  In this way, there is no character checking (e.g., to
discard any "/" or "." which are shown in the illustration of
TEXCOL(K) against BLANK), and the search for characters of the name
is terminated when the free-field separator character CSEPAR is
found.  The search begins in column position KOLBEG, which is one
column to the right of the last comma (in this case, the comma which
followed "REPLOT", in column 7).  ENCODE is used to transfer
characters from the input card buffer TEXCOL(80) to our file name
FILEN(25) because of the type difference ---- TEXCOL is ALPHANUMERIC
(REAL*8 for VAX), while FILEN is a byte vector (INTEGER*1).  After
connection of the desired plot file, ICAT = 2 is set so that our
precious disk file will be retained (rather than be destroyed) at the
start of the following case (see previous "SYSDEP" logic).

.. code:: fortran

       5610      SUBROUTINE PFATCH
   M27. 634      INSERT DECK BLKCOM
   M27. 635      BYTE  FILEN(25)
   M27. 636      N4 = 0
   M27. 637      ENCODE (25, 4523, FILEN(1))
   M27. 638 4523 FORMAT ( 25X )
   M27. 639      DO 4532  K=KOLBEG, 80
   M27. 640      IF ( TEXCOL(K) .EQ. BLANK )  GO TO 4532
   M27. 641      IF ( TEXCOL(K) .EQ. CSEPAR ) GO TO 4536
   M27. 642      N4 = N4 + 1
   M27. 643      ENCODE (1, 3041, FILEN(N4))  TEXCOL(K)
   M27. 644 3041 FORMAT ( 80A1 )
   M27. 645 4532 CONTINUE
            4536 CLOSE (UNIT=IALTER)
                 OPEN  (UNIT=IALTER, TYPE='OLD', FORM='UNFORMATTED',
   M27. 652     1       NAME=FILEN)
   M28.2834      ICAT = 2
       5653      RETURN
       5654      END

For those systems which can save files permanently after they have
been written as scratch files, a cleaner alternative exists.  This
goes back to our BPA CDC usage, where all such LUNIT4 considerations
were relagated to "KATALG" of overlay 20.  No more writing on unit
LUNIT4 will occure once overlay 20 is reached (normally by exit of the
time-step loop of overlay 16, but possibly from overlay 12 if a TACS
stand-alone case is involved).  Further, there is now a call within
"OVER20", though it also provides service for EMTP table saving
(if integer miscellaneous data parameter MEMSAV is positive):

.. code:: fortran

   M28.6577 8005 IF ( ICAT .GT.  0     .OR.      MEMSAV  .GT.  0 )
   M22.5384     1 CALL KATALG

Hence an installation-dependent "KATALG" must have /BLANK/ in it
(via "INSERT DECK BLKCOM"), and a check to see wheter this really
does concern the plot file (ICAT positive) is necessary.  The date
and time are still available at this point, and are stored in the same
vectors DATE1(2) and TCLOCK(2) as was illustrated for "SYSDEP".
One final warning might be in order, however: although the EMTP is
done writing on the LUNIT4 file by the time "KATALG" is reached,
this does not mean that the file can be disconnected, since it might
be read as part of the batch-mode EMTP plotting (CalComp EMTP plotting,
delayed longer, and would normally be provided at the top of "SYSDEP"
(as the following data case begins in overlay 1).

Concerning plot file naming, it is recommended that the date and
the tim (DATE1, TCLOCK) be used, since this will then allow easy
association of the plot file with line printer output (which has this
date and time buried in the heading).  But beyond that, there should
be some easy way to access all EMTP plot files as a group ---- possibly
for audit purposes, possibly for copying to tape, possibly for
deletion.  For the VAX, we have seized upon file type for this
special characterization (".PL4").  Thus, if any user wants a list of
his plot files, he just issues the wild-card command "$DIR \*.PL4"; if
he wants to delete all such files, he uses "$DEL \*.PL4;\*"; etc.  Our
file names are slightly limited by the VAX restriction to 9 characters,
however: we use a hexadecimal digits for the time (24 hour clock).  If
the uses's system allows longer names, more elaborate and precise
names for the LUNIT4 disk files are probably desirable.

To complete this treatment of the EMTP plot file, there might
be an indication of what is actually written to LUNIT4.  This is
fully documented in Section 5.0c (page 86a onward) of the EMTP Rule
Book, so nothing more need be said here.


Check point capability ( MEMSAV = 1 and "START AGAIN" )
-------------------------------------------------------------------------------

For most users, the second most important extension (after the
LUNIT4 plot manipulations just described) has to do with honoring the
MEMSAV field (columns 59-56) of the miscellaneous data card.  See
Section 1.0h, page 4h of the Sept 1980 Rule Book.  This provides for
the dumping of EMTP tables onto disk for preservation as a permanent
disk file.  If at some later time the user wants to restart the
simulation, using the identical same EMTP version (warning: dimensions
must not have been altered), then "START AGAIN" of Section 1.0e15

Consider the saving of EMTP tables (MEMSAV = 1) first.  This is
done within "KATALG" of overlay 20, for which the call is as
follows (as previously displayed in Section A):

.. code:: fortran

   M28.6577 8005 IF (ICAT .GT. 0     .OR.      MEMSAV  .GT.  0 )
   M22.5384     1 CALL KATALG

As for the file OPENing and CLOSEing within "KATALG", it will
depend in large part upon how sophisticated a naming procedure is
desired.  In the VAX case, we decided to use a fixed, pre-specified
name TPTABLES.BIN, which simplified things.  The operating system
VAX/VMS would simply create a higher version if MEMSAV = 1 were
used more than once by the user (no problem), and it is the user's
responsibility to specify the correct set of tables during a subsequent
"START AGAIN" request.  Anyway, as for the critical block of code
within "KATALG", VAX uses the following:

.. code:: fortran

   M30.1048 2469 WRITE (LUNIT6, 2472)
   M28.6606 2472 FORMAT ( /,  20X,  '----- "MEMSAV = 1  REPRESENTS',
   M28.6607     1                   ' REQUEST FOR TABLE DUMPING ON DISK.' )
   M28.6608      CLOSE ( UNIT=LUNIT2 )
   M28.6609      OPEN  ( UNIT=LUNIT2, TYPE='NEW', FORM='UNFORMATTED',
   M28.6610     1        NAME='TPTABLES.BIN' )
   M28.6611      CALL TABLES
   M28.6612      CLOSE ( UNIT=LUNIT2, DISP='SAVE' )
            2482 WRITE (LUNIT6, 2483)  LTLABL
   M28.6614 2483 FORMAT (  26X,  'SECCESSFUL SAVING OF EMTP',
   M28.6615     1         ' TABLES AS FILE  "TPTABLES.BIN" .',
   M28.6616     2         '    LTLABL  =',  I8  )

Note that part of the table dumping message (S.N. 2472) is printed before
the dumping actually begins (it is done by "TABLES"), and the remainder
occurs upon completion (S.N. 2482).  This is mainly for interactive use,
to placate the impatient user who may be watching such output on the
screen (and wondering why there is a delay, during the dumping).  Note
that binary (UNFORMATTED) usage of I/O channel LUNIT2 is involved.
Remember to put /BLANK/ in the module (INSERT DECK BLKCOM), since this
carries LUNIT2 and LTLABL.  Also, remeber that if the plot file
is also to be saved in "KATALG" rather than in "SYSDEP" (see preceding
section), then both ICAT and MEMSAV must be checked inside the
module to see which (or both) of the functions is actually to be
performed.

Later use of these EMTP tables is via the "START AGAIN" data card
of Sectioon 1.0e15.  Installation-dependent aspects for VAX are very
similar to "REPLOT" as described in the previous section.  Both
features use "PFATCH" to actually connect the old disk file to I/O
unit IALTER, though here unit LUNIT2 is employed as shown by the
following universal code in "OVER1":

.. code:: fortran

   M22.1329C     $$$$$    SPECIAL-REQUEST WORD NO. 15.   'START AGAIN'
   M22.1330 8015 IALTER = LUNIT2
   M28. 818      IF ( NOUTPR  .EQ.  0 )
   M28. 819     1 WRITE (LUNIT6, 2857)
   M28. 820 2857 FORMAT (   40H+CONTINUE PARTIALLY-COMPLETED DATA CASE. )
                 CALL RUNTYM (D1, D2 )
   M22.1331      CALL PFATCH
   M28. 822      CALL TABLES
   M28. 823      FLSTAT(1) = -D1
   M28. 824      FLSTAT(2) = -D2
   M28. 825      IF ( FLSTAT(16)  .EQ.  LTLABL )  GO TO 2863
   M22.1338      KILL = 201
   M22.1339      LSTAT(19) = 2856
   M22.1341      GO TO 9200
   M28. 826 2863 CONTINUE

Provided "PFATCH" was coded as described in Section A (for plot file
usage), nothing else need be done here.  Note the check on total
table size (LSTAT(16) is the table size of the disk file, as carried
out throgh this /BLANK/ variable, while LTLABL is the total table
size of the present program version (from "DIMENS").  Only if these
two agree (a necessary but not sufficient check for compatibility) is
execution allowed to continue.  Otherwise, a KILL = 201 error stop
results.

User's of "START AGAIN" should be warned that batch mode EMTP
plotting will be possible for the restarted case only if special effort
is made.  The interactive CRT plotting program "TPPLOT" can plot
such results without difficulty.  But if the user insists on EMTP
batch-mode plotting, then put an extraneous "4" in column 13 of
the "START AGAIN" card (after the comma, before the file name),
and previously connect the old plot file to unit LUNIT4 somehow
(IBM can do it via JCL; we on the VAX use $OLDFILE of "CIMAGE" as
described under Point 16 on page x-j8a of the Rule Book).  See also
Section D below (for "CIMAGE" $-card enhancement).


Use of "TABULATE ENERGIZATION RESULTS" (STATSV, STATRS, MIDOV1)
-------------------------------------------------------------------------------

Only the sophisticated "STATISTICS" user will have interest in
the extensions of the section .... or the user of a computer which
crashes a lot!  Monte Carlo studies, where the same basic problem
is solved over and over (with only switch closing or opening times
altered between simulations, by the rolling of dice), are the ony
studies which are affected.  By means of the extensions now to be
detailed, such Monte Carlo studies can be solved in several smaller
pieces, rather than one big one.  If the computer crashes during such
a simulation, it is like dropping a basket with eggs in it.  The
prudent, conservative strategy for anyone who drops eggs from time to
time is to never carry too big a basket!

In terms of Rule Book data structures, we have "STATISTICS
OUTPUT SALVAGE" of Section 1.0e7 (page 4b-5), and "TABULATE
ENERGIZATION RESULTS" of section 1.0e6 (page 4b-4).  An EMTP support
person considering such enhancement should read these two sections
throughly before continuing.

The VAX installation-dependent logic associated with "STATISTICS
OUTPUT SALVAGE" is confined to "MIDOV1" of overlay 1.  Before this
module is called by "OVER1", some universal preparation is performed,
to be carried into "MIDOV1" via /BLANK/:

.. code:: fortran

   M23. 415      N12 = JFLSOS / 100
   M23. 416      N15 = JFLSOS - 100 * N12
   M23. 417      N13 = N15 / 10
   M23. 418      N14 = N15 - 10 * N13
   M23. 419      LSTAT(14) = N12
   M23. 420      LSTAT(15) = N13
   M23. 421      LSTAT(16) = N14
   M22.1558      CALL MIDOV1

That user-supplied sequence number JFLSOS (columns 30-32) is here
broken down into three decimal digits which are carried into "MIDOV1"
via the LSTAT vector.  Disk file names are then built from these
characters, and files are opened, as follows (within "MIDOV1"):

.. code:: fortran

   M29.1170 1815 IF (JFLSOS .EQ. 0) GO TO 4271
   M24. 460      IF (LASTOV .EQ. 20) GO TO 5923
   M24. 461      CLOSE (UNIT=3)
   M24. 462      CLOSE (UNIT=9)
   M24. 463      N4 = 3
   M24. 464 5910 ENCODE (14, 5914, FILNAM(1) )N4, (LSTAT(J), J=14,16)
   M24. 465 5914 FORMAT (2HST, I1, 3HLOG, 3I1, 5H.DAT   )
   M24. 466      DO 4256  J=15, 20
   M24. 467 4256 FILNAM(J) = C1
   M24. 468      OPEN (UNIT=N4,TYPE='NEW',NAME=FILN20,FORM='UNFORMATTED')
   M24. 469      IF (N4 .EQ. 9 )  GO TO 5923
   M24. 470      N4 = 9
   M24. 471      GO TO 5910
   M24. 472 5923 RETURN

The VAX/VMS file name ST3LOG???.DAT is built on the first pass (for
unit 3), and ST9LOG???.DAT is built on the second pass (for unit 9),
where "???" is used to denote the three non-blank digits of the
user-supplied serialization JFLSOS.  The INTEGER*1 vector
FILNAM(20) is equivalenced to the CHARACTER*20 name FILN20 which
is actually used in the OPEN statement.  This is all done before
anything is written on units 3 or 9.

As for the information which is written to these two files, we
have LUNIT3 written to by "OVER12" of overlay 12,

.. code:: fortran

   M17. 601      WRITE (LUNIT3)  (KHIGH(I), KLOW(I), AKEY(I), TSTAT(I), ...
   M23.2046     1 KDEPSW(I), I=1, KSWTCH), KLOAEP

and LUNIT9 written to by "OVER20" of overlay 20:

.. code:: fortran

   M13.2797      KNT = KNT + 1
   M23.5347      IF ( KNT .EQ. 2     .OR.
   M23.5348     1     IABS(NENERG)  .EQ.  1 )
   M23.5349     2 WRITE(LUNIT9) NSTAT, KSWTCH, NUMNVO, NC
   M23.5350      WRITE (LUNIT9)  ( XMAX(L), L=1, NSTAT )

This is done once each energization.  In addition to information
about the switches, LUNIT3 contains the crucial switch-closing times.
On the other hand, LUNIT9 contains the peak overvoltage vectors
which are to be statistically tabulated (NSTAT is the number of
statistical output variables).  KNT keeps track of energization
number, and the exceptional additional dump for KNT = 1 is for
header information, to be written only after the first energization.

For BPA CDC implementation (prior to our switch to the VAX in
February 1979), we were able to name the file after writing on
recovery even after operating system interrupts, from which we would
also recover, and send control to the EMTP error overlays.  So, in
such cases, one can use "STATSV" (mnemonically, "statistics save")
of overlay 55 rather than "MIDOV1" of overlay 1.  But for the VAX
and most computers, "STATSV" remains a dummy, unused module:

.. code:: fortran

          M23.6518      SUBROUTINE STATSV
          M23.6567      RETURN
          M23.6568      END

So much for the Monte Carlo EMTP solution per se.  Now on to the
combination and statistical tabulation, as requested by "TABULATE
ENERGIZATION RESULTS".  Universal aspects of this request are found
in "SUBR29" of overlay 29, where the data cards specifying the
files are read, and where each characteristic
serialization (JF1, JF2, etc.) is decoded into three
decimal digits which are stored in LSTAT(14) through
LSTAT(16).  In a loop over the different serializations, there
are two calls to "STATRS", which interface variables all
communicated via /BLANK/.###The non-obvious ones are:

- LSTAT(13) = 3, 9, or 0, depending upon whether it is the
  file ST3LOG???, ST9LOG???, or nothing which
  is to be connected to LUNIT1 next.  In the
  zero case, we CLOSE LUNIT1;

- LSTAT(14) = first decimal digit of "???" at end of file name;

- LSTAT(15) = 2nd decimal digit of "???" at end of filee name;

- LSTAT(16) = last decimal digit of "???" at end of file name;

Once this communication is understood, and that /BLANK/ is present
in module "STATRS", the code of "STATRS" should be self-explanatory
by analogy to the previously-treated "STATSV".  The VAX module
"STATRS" has the following key statements:

.. code:: fortran

        ENCODE (14, 1804, FILNAM(1))   ( LSTAT(I), I=13, 16 )
   1804 FORMAT ( 2HST, I1, 3HLOG, 3I1, 5H.DAT )
        DO 4256 J = 15, 20
   4256 FILNAM(J) = C1
        CLOSE (UNIT=LUNIT1)
        IF ( LSTAT(13)  .EQ.  0 )  GO TO 9000
        IF ( IPRSUP .GE. 1 )
       1 WRITE (LUNIT6, 1808) FILNAM
   1808 FORMAT ( ' IN "STATRS", B4 OPEN OF UNIT LUNIT1 .' , 20A1)
        OPEN ( UNIT=LUNIT1, TYPE='OLD', NAME=FILN20,
       1      FORM='UNFORMATTED' )
        WRITE (LUNIT6, 1822)  LSTAT(13), FILNAM
   1822 FORMAT ( 20X,
       1  '----  SUCCESSFUL  OPEN   OF  LUNIT',  I1,  '  DATA',
       2  ' ON DISK.   PERMANENT FILE NAME = ', 20A1, 2H .   )
   9000 RETURN
        END


Installation-dependent $-card capabilities (CIMAGE)
-------------------------------------------------------------------------------

"$-cards" are  introduced in Section 1.D of the Rule Book (page
3b), so it is assumed that the reader has studied this universal
material before going any further.  All $-cards are recognized and
acted upon locally in module "CIMAGE" ---- even the universal ones.
In this sense, the universal $-cards are only universal in that their
implementation is possible for any computer; it does not imply that
the keeper of installation-dependent modules has actually done so.
But most likely such code does already exist, and in this present
section we are merely interested in further enhancing $-card capability
to include those installation-dependent commands which require file
OPENing and CLOSEing.

The VAX "CIMAGE" module is large, but the reader should not
be frightened.  Most of the code will work for any computer system.
It is only the ENCODE/DECODE and OPEN/CLOSE usage which might
require conversion.  Further, it is assumed that the reader has
ENCODE/DECODE or its equivalence (e.g. Burroughs READ/WRITE involving
memory), so only the OPEN/CLOSE portions need here to be treated.
This shall be done in order, from top to bottom of "CIMAGE".

We begin with

.. code:: fortran

   M27. 183C               *****    REQUEST NO. 2.    "$PUNCH"       *****

which turns out to be universal id the LUNIT7 I/O channel is the
closest thing to a card punch which os available! But for those who
have a real physical card punch which is not connected to unit 7, and
for those who really want $PUNCH to punch physical cards, then
they can convert the "WRITE (LUNIT7" statement to "PUNCH" or whatever
other command is appropriate.  But cards are rapidly disappearing, and
$PUNCH is so little used anyway that such modification is not generally
recommended.

Next comes:

.. code:: fortran

   M27. 211C               *****    REQUEST NO. 4.    "$SAVE"        *****

which contains three file OPEN/CLOSE operations:

.. code:: fortran

   M27. 214 4423 CLOSE ( UNIT=N7, DISPOSE='DELETE')
   M27. 215      OPEN  ( UNIT=N7, TYPE='NEW', FORM='FORMATTED',  NAME=FILEN )
   M27. 220 4436 CLOSE ( UNIT=N7, DISPOSE='SAVE' )

These must of course be converted.  The file name is stored in vector
FILEN, as extracted by the logic of $INCLUDE immediately below.

Next comes:

.. code:: fortran

   M36. 217C               *****    REQUEST NO. 5.    "$SPYDATA"     *****

Assuming EMTP free format usage with commas (which is both easiest
and most common), the "$SPYDATA" is to be followed by a file name,
followed by one integer for the I/O unit number which can be found by
searching (TEXCOL(K), K=KOLBEG, 80).  The VAX logic ignores blanks,
and will truncate the name whenever a comma (CSEPAR) or parenthesis
is found:

.. code:: fortran

   M27. 228      DO 4532  K=KOLBEG, 80
   M27. 229      IF ( TEXCOL(K)  .EQ.  BLANK )  GO TO 4532
   M27. 230      IF ( TEXCOL(K)  .EQ.  CSEPAR ) GO TO 4536
   M27. 247      IF ( TEXCOL(K)  .EQ.   1H(  )  GO TO 4536
   M27. 231      N4 = N4 + 1
   M27. 232      ENCODE (1, 3041, FILEN(N4))  TEXCOL(K)
   M27. 233 4532 CONTINUE

Here the file name is built into INTEGER*1 working vector FILEN(25).
The only remaining installation-dependent records are the subsequent
OPEN and CLOSE operations whhich follows:

.. code:: fortran

   M27. 259      CLOSE (UNIT=N7)
   M36. 252      OPEN (UNIT=N7, STATUS='OLD', FORM='FORMATTED', FILE=FILEN )

Next comes:

.. code:: fortran

   M27. 276C               *****    REQUEST NO. 8.    "$RETURN"      *****

which has the single installation-dependent record which follows
associated with it:

.. code:: fortran

   M27. 279 4817 CLOSE (UNIT=N1)

This represents a disconnection of whatever disk file may have been
connected to I/O unit number N1.  It "undoes" what $ATTACH or
$NEWFILE did (the connection operation) when they were previously
executed.

Next comes:

.. code:: fortran

   M27. 284C               *****    REQUEST NO. 9.    "$NEWFILE"     *****

which really is no different that $ATTACH except that VAX/VMS makes
a distinction between FORMATTED and UNFORMATTED usage; $ATTACH is
for FORMATTED usage, while $NEWFILE is for UNFORMATTED.  In any
case, there is just one installation-dependent record, which should
be self-explanatory:

.. code:: fortran

   M28. 169 4907 OPEN (UNIT=N7, TYPE='NEW', FORM='UNFORMATTED', NAME=FILEN )

Next comes:

.. code:: fortran

   M27. 298C               *****    REQUEST NO. 11.   "DELETE"       *****

which uses the following self-explanatory records which are
installation-dependent:

.. code:: fortran

   M27. 301 5106 OPEN  ( UNIT=N7, TYPE='OLD', NAME=FILEN )
   M27. 302      CLOSE ( UNIT=N7, DISPOSE='DELETE' )

Next comes:

.. code:: fortran

   M28. 178C               *****    REQUEST NO. 16.   "OLDFILE"      *****

which contains the following installation-dependent records:

.. code:: fortran

   M28. 182 5608 CLOSE (UNIT=N7)
   M28. 183      OPEN (UNIT=N7, TYPE='OLD', FORM='UNFORMATTED', NAME=FILEN )

So much for file operations of "CIMAGE".  Although certainly no
conversion problem, I might also mention other minor conversion
details for some systems.  In several places there will be found
explicitly-counted Hollerith strings on the right hand side of equal
signs, such as the following of $ATTACH:

.. code:: fortran

   M27. 295 4100 TEXT1 = 6HATTACH

this could be easily made universal, but I like the self-explanatory
aspect of having the character string present, so no change is now
contemplated.  Also, there is $MONITOR, which directly writes to
the line printer (whether or not there is a LUNIT6 connection):

.. code:: fortran

   M29. 295      PRINT 3006,  BUFF10
   M27. 307      PRINT 5214, NUMDCD
   M27. 308 5214 FORMAT ( '+CRT MONITOR.  CARD NUMBER =',  I5  )

Systems which do not have such direct printing will just have to
comment out this operation.

.. raw:: pdf

    PageBreak


Former Program Versions That Are No Longer Being Listed
-------------------------------------------------------------------------------

Previous versions of the Rule Book had a page for just about
every different computer that had been seriously considered for
support of the EMTP, beginning with this section.  But this policy
is being abandoned in 1986.  The last such complete listing, then,
is in the "M39." manual dated June, 1984.

In order to save paper, and also to minimize potential bother
to those whose names were once listed, program versions that are
not known to be active today have been downgraded to an
incospicuos and sometimes indefinite mention in the list that
follows.  Even if usage is known to continue, there is no listing
if no active, cooperative contact is known to exist for current
program versions.

For those individual entries that have been removed as
separate pages, a few summary details will be provided.  For more
details, consult older user documentation.

#. CDC as listed before was the old, 60-bit, overlaid
   machinery that could only address with 18 bits.  Support ended
   when Prof. Mohan of the University of Minnesota shifted is EMTP
   usage to Apollo in early 1984.  With all-new, fully-virtual CDC
   machinery now being sold, it is doubted wheter EMTP usage of the
   old CDC hardware has much if any EMTP future.  It is hoped that an
   EMTP version for the new CDC will come from Prof. Hian Lauw of
   Oregon State University in Corvallis, Oregon, eventually.  Such a
   machine was installed in the EE Department during September of
   1985.  Yet timing is unclear, since such work is not a priority
   for those in Corvallis.  Also, with the acquisition of half a
   dozen powerful, new 32-bit Apollo DN3000 nodes, it is conceivable
   that EMTP interest in the new CDC might even disappear entirely.
   Yet we hope not.

#. Univac 1100-series machines were non-virtual, 36-bit word
   machines that used overlaying to handle the EMTP.  Active support
   from Ontario Hydro (Toronto, Ontario, Canada) seemed to end
   shortly after that utility acquired a DEC VAX-11/780, and began
   using VAX for its EMTP studies.  That was during 1981 or 1982, it
   is estimated.  The most recent Univac EMTP version that was known
   to be distributed was of "M31." vintage.  While Univac certainly
   operational convenience and improved economics of smaller
   alternatives to such mainframes.

#. Honeywell mainframes once were supported in grand style
   by Bob Newell of Basin Electric Power Cooperative (see the current
   Prime Computer page). But this Honeywell support ended when Basin
   Electric acquired PRIME and PTI PSS/E software for it.  After a
   lapse in Honeywell EMTP availability of perhaps two years, Bob
   Jones of Southern Company Services in Birmingham, Alabama,
   switched to an existing Honeywell machine for EMTP support, but
   this only ]asted a year or two.  Honeywell EMTP support in
   Birmingham ended in 1986 with the acquisition of Apollo.
   While Honeywell seemed to have no trouble supporting the EMTP, users
   seemed to prefer the operational convenience and improved
   economics of smaller alternatives to such mainframes.

#. Harris once was used by the University of Wisconsin at
   Madison for EMTP support during the annual summer EMTP short
   course.  But such usage was switched first to VAX, and more
   recently to Apollo.  It is doubtful whether any known owner of
   Harris hardware is serious enough about the EMTP to maintain
   proven compatibility in the future.  Yet it should be emphasized
   that there were no fundamental drawbacks or problems with the
   newer hardware, which does indexing with 20 bits (the earlier
   machines with 18-bit indexing were hard pressed to cope with the
   expanding EMTP).

#. Telefunken TR -1 4140 EMTP compatibility was confirmed during
   19801 when the report of generally successful "M19."
   experimentation finally reached BPA from AEG-Telfunken of
   Frankfurt, West Germany.  But the TR 1 4140 was never a commercial
   success, and AEG-Telefunken interest in the EMTP was somewhat
   casual.  No second party with Telefunken EMTP interest has ever
   been identified, and no newer Telefunken EMTP work is known.

#. Floating Point Systems FPS-161 4 Attached Processor
   compatibility with the "M32." EMTP was demonstrated late in 1982.
   But cost effectiveness and convenience were less than clear.
   Testing by BPA was done with the factory in Beaverton (a suburb of
   Portland, Oregon), so no production user with EMTP interest was
   ever located.

#. DEC PDP-10 and System 20 are machines for which the story
   parallels those preceding stories of Univac and Honeywell.
   So does the hardware: 36-bit word machines. Yes, some usage remains
   today, but the days of such usage are clearly numbered, because
   the manufacturer has announced its intention to discontinue word
   machines and concentrate on the more modern, byte-organized VAX-11
   line.  The newest version known to be operating is a true "M39."
   version that is still being used by Prof. Hian Lauw of Oregon
   State University in Corvallis, Oregon.  Interactive CRT plotting
   has been connected,  too. But how long such support will continue
   is speculative (my guess is that use of the 2020 in Corvallis for
   support of the EMTP will end when Apollos begin arriving in
   quantity).

#. Cray supercomputers certainly are capable of EMTP
   support, and an 11 M31.+" version was set up and tested at Lawrence
   Livermore Laboratory (LLL) in Livermore, California, by Dr. Walter
   G. Magnuson, Jr. Is LLL still using the Cray EMTP? Are the
   economics and convenience (or possible inconvenience) of such a
   super computer really desirable for EMTP use? There are more
   questions than answers about the Cray EMTP.

#. Burroughs mainframes are certainly capable of support of
   the EMTP, but much as with Univac, Honeywell, and DEC PDP-10, such
   usage has been eroded in recent years by the switch to smeller,
   more convenient alternatives.  Our last good contact was Ebasco
   Services Incorporated of New York City.  But when Stoney McMurray
   left Ebasco during 1985, the Burroughs EMTP contact ceased. It is
   believed that all Ebasco EMTP usage has shifted to DEC VAX.
   The testing of the Burroughs EMTP is so tricky that it is doubted
   whether any future version will ever be fully tested and generally
   available.

#. MODCOMP was used for a time by EPSRI of Peking, China,
   for support of the EMTP.  But the "Classic" model then available
   was really a control computer, and it had several drawbacks for
   EMTP usage, including limitations on memory addressing,
   unbelievably slow compilation and linkage editing, and lack of
   virtual memory management.  It was decided to shift EMTP usage-to
   other, better-suited machinery during 1983.

#. ICL was used to support an "M31." version of the EMTP
   by The University in Glasgow, Scotland.  For those not familiar
   with the name, International Computers, Limited is the computer
   giant of the British empire.  The hardware being used in Glasgow
   was very similar to an IBM mainframe, and it seemed obviously
   capable of handling the EMTP. Yet nothing has been heard from any
   ICL EMTP user for several years.  If there is a problem, it would
   seem to be a lack of one dedicated industrial user who can produce
   and maintain current program versions for others around the world.
   All machines of interest are located a long way from Portland,
   unfortunately (none having EMTP interest are located in the
   Western hemisphere).  The man most closely associated with ICL
   EMTP computer details back in 1983 was Dr. Paul Rosenberg of The
   Computing Service (the central computer installation).

#. Siemens is the "General Electric of West Germany," and
   the sale of computers that resembled IBM mainframes was a small
   portion of this giant company's business. Such a Siemens computer
   was used by FGH (Forschungs-Gemeinschaft fur Hochspannungs- und
   Hochstromtechnik E.V.) of Mannheim for support of the EMTP until
   1985, when there was a shift to Apollo.  Any questions about the
   future of the Siemens EMTP could best be directed to Dipl.-Ing.
   Bernd Stein of FGH, who once was in charge of producing and
   maintaining it.

#. NEC ACOS is a Japanese computer that once was used in
   two quite different forms for support of the EMTP.  First, there
   was the non-virtual, 36-bit word machine that looked almost
   identical to Honeywell.  This was used by Meidensha Electric Mfg.
   Co., Ltd., of Tokyo during late 1982.  Second, there was the
   virtual, byte-organize.d ACOS computer of Nissin Electric Co., Ltd.
   of Kyoto.  Nothing later than 1983 is known about either 'machine
   or its usage for support of the EMTP.

#. FACOM is another Japanese computer.  In 1982 when EMTP
   work was under way, it looked a lot like an IBM mainframe with
   enhanced software.  Work was done at Century Research Center
   Corporation of Osaka.

#. Hitachi HITAC is still another Japanese computer that
   resembled an IBM mainframe.  It was used by the Kokubu Works of
   Hitachi, Ltd., for support of the EMTP during 1983, when we had
   extensive contact.  1.6) Perkin-Elmer was a manufacturer of powerful
   minicomputers during 1981 when "M28. +" EMTP materials were produced.
   But a cooperating user with EMTP interest was never found.
   We have here an EMTP versions that is looking for an owner and a home.
   On paper, Perkin-Elmer machines should be EMTP compatible, but FORTRAN
   was never tested (although it was produced).

#. IBM compatible PC class based on x86 processor (32 and 64 bits) ...
   (to be done).

.. raw:: pdf

    PageBreak


IBM EMTP Setup (K.U.L. in Belgium and AEP in Columbus)
-------------------------------------------------------------------------------

As this page is being written on 6 April 1984, production EMTP
users can receive IBM program versions of approximately the sane vintage
("M3IL+11) from one of two sources, for approximately the sane copying
fee ($200 or less). Most European users who rely upon IBM computers
have received their IBM EMTP FORTRAN from K.U.L. in Belgium, whereas
most American users rely upon American Electric Power (AEP) in Columbus,
thio, as their source of supply. In the remainder of this section,
further information about these two sources IBM EMTP versions will be
provided, along with information about the latest IBM EMTP research.
Both K.U.L. and AEP use IBM 3033 computers for support of the EMTP.
Although fully-virtual versions are possible, the practical priorities
and econanics of usage have forced the continued reliance upon
overlaying for the IBM EMTP.  In that most recipients will probably
want to operate similarly, and the switch from overlaying to fully-
virtual just requires the deletion on OVERLAY cards during linkage-
editing, there is no loss of generality in this. But what about DOS
installations, for which IBM EMTP usage has nearly vanished in this
country? Changes are required, although we are not prepared to document
then here. Or one continuing contact to such usage is S&C Electric in
Chicago, where Art Jahnke provides years of experience dealing with both
the EMTP and IBM LOS complications. But S&C Electric is a cctmnercial
operation, and can not be expected to provide free advice to the general
public.  A current ("M38.0) IBM EMTP translation was tested during the
first week of March, 1984, when WSM worked on--site with Mike Price in
Columbus. While most test cases solved perfectly, cases involving the
Type-59 S.M. had trouble, and cases involving the U.M. are still
undergoing evaluation (IJTFF corrections have yet to be applied,
reflecting Hian ' s latest changes). When such a current version might be
made available for use by others is unknown, although it seems clear
that this latest IBM experimentation at AEP points the way toward future
IBM EMTP usage. Specifically, there has been a shift to the VS FORTRAN
compiler, using LANGLVL(77) everywhere. For a detailed account of this
work, see Ref. 8, past the end of Vol. XIV (not yet bound), 19 January
1984, Section V-D, pages AESS-48 through 61.
The European IBM EMTP connection shall be described first, since I
have no associated printed documentation of it which can be passed along
as well. Any reader who has interest is invited to contact:


|   Prof. fl. Daniel Van Dommelen
|   Elektrotechnisch Instituut - Departm. E
|   Katholieke Universiteit Leuven
|   Kard. Mercierlaan 94
|   303D Heverlee - Leuven
|   BELGIUM
|   Phone (International): 32-16-220931
|   Telex: elekul b 25941


In addition to his university duties, Daniel serves as Chairman of the
European EMTP Users Group.  His English is excellent, so no party
contemplating a telephone call in that language should hesitate. If the
phone is answered in Dutch by a secretary, just ask for "Professor Van
Immelen" in slow English, and there should be no problem.

The remainder of this IBM EMTP section shall be devoted to AEP
documentation, which begins with a sample letter of response to an inquiry
about the IBM EMTP. After that comes the printed documentation with which
AEP supplies people, in responding to requests for the IBM EMTP. Inquiries
should be made of

|   Michael M. Price; Floor 7
|   Engineering Information Systems
|   Information Systems Department
|   American Electric Power Service Corp.
|   1 Riverside Plaza
|   P. O. Bbx 16631
|   Columbus, Chio 43216-6631
|   Phone: (614) 223-3776


American Electric Power Service Corporation announces the avail-
ability of the M34+ version of the Electromagnetic Transients Program
(EMTP) for the IBM computer. This program, developed under the aus-
pices of the Bonneville Power Administration (BPA), has been set-up
and tested on the IBM computer by AEP. As with previous versions of
EMTP, AEP will supply the program, JCL, and installation documentation
at no charge. However, due to the large number of tapes received, the
problems in handling out-of-company tapes, and the unacceptable condi-
tion of many of the tapes (mislabeled, too short, physically broken
during shipping, etc.), AEP has decided to supply the tapes and
mailers for the program distribution.

To offset our costs for the tape purchase, preparation and mail-
ing, the following fee schedule has been set:

* For all organizations within the USA,
  Canada, or Mexico: ............................................. $150.00

* For all organizations outside the USA,
  Canada, or Mexico: ............................................. $175.00

AEP's offer does not include the EMTP Rule Book. For a copy
please contact Dr. Scott Meyer of BPA. His address is:


|   W. Scott Meyer, Rute EOGA
|   Bonneville Power Authority
|   P. O. Box 3621
|   Portland, Oregon 97208
|   Phone: (503) 230-4404


Through AEP's newly created subsidiary, AEP Energy Services,
Inc., several EMTP-related services could be offered:

* Processing EMTP studies on our corporate computer for
  other organizations.

* Providing an EMTP installation service.

* Performing power system studies.

* Providing training in the use and/or internals (FORTRAN
  Source) of EMTP.

* Providing a user friendly preprocessor for EMTP data.

If you have an interest in obtaining the IBM version of EMTP or
making use of the services mentioned above, please write to:


|   Michael M. Price
|   American Electric Power Service Corporation
|   1 Riverside Plaza - 7th Floor
|   Columbus, Ohio 43216-6631


For your convenience, an order form has been included. Please use a
copy of this form as your invoice. If you have any questions, you may
call Mike Price at (614)223-3776.

The AEP-created tape will be 6250 BPI, 9-track, 2400 foot, with
a standard IBM label (non-label and/or 1600 BPI by special request).
It will include all the JCL needed for installation and execution of
EMTP (system dependent changes will need to be made), the FORTRAN
source code, executable load modules, object modules, link editor
data, and test case data. This version has been successfully compiled
using the IBM "OS/360 FORTRAN H-extended Level 2.2 (Sept.76)"
compiler.


STRUCTURE OF THE EMTP M34 TAPE
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+---------------------+----------------------+---------------------+--------------------------+
| .. class:: center   | .. class:: center    | .. class:: center   | .. class:: center        |
|                     |                      |                     |                          |
| FILE #              | CONTENTS             | # OF RECORDS        | FORMAT                   |
+---------------------+----------------------+---------------------+--------------------------+
| .. class:: center   |                      | .. class:: center   |                          |
|                     |                      |                     |                          |
| 1                   | Job Control Language | 756                 | Sequential with IEBUPDTE |
|                     |                      |                     | control cards            |
+---------------------+----------------------+---------------------+--------------------------+
| .. class:: center   |                      | .. class:: center   |                          |
|                     |                      |                     |                          |
| 2                   | FORTRAN Source, Link | 107587              | Sequential with IEBUPDTE |
|                     | Editor Data, Test    |                     | control cards            |
|                     | Case Data            |                     |                          |
+---------------------+----------------------+---------------------+--------------------------+
| .. class:: center   |                      | .. class:: center   |                          |
|                     |                      |                     |                          |
| 3                   | Executable Load      | ---                 | Unloaded sequential      |
|                     | Modules              |                     |                          |
+---------------------+----------------------+---------------------+--------------------------+
| .. class:: center   |                      | .. class:: center   |                          |
|                     |                      |                     |                          |
| 4                   | Object Modules -     | ---                 | Unloaded sequential      |
|                     | All Modules          |                     |                          |
+---------------------+----------------------+---------------------+--------------------------+
| .. class:: center   |                      | .. class:: center   |                          |
|                     |                      |                     |                          |
| 5                   | Object Modules -     | ---                 | Unloaded sequential      |
|                     | 11 Modules for the   |                     |                          |
|                     | "Big" Version        |                     |                          |
+---------------------+----------------------+---------------------+--------------------------+

Files 1 & 2 have the necessary control cards between each module to
be used as input to the IBM utility program IEBUPDTE. By the use of
IEBUPDTE, the partitioned dataset (PDS) can be reconstructed for the JCL
& source. The JCL maybe kept in a PDS where it can be submitted via
TSO, or the sequential file may be punched to cards. In addition, global
changes can be made to the sequential JCL and FORTRAN files, and then the
PDS can be recreated via IEBUPDTE.

Files 3, 4, & 5 were created by unloading the load and object mod-
ule PDS' via IEBCOPY. These files should be loaded onto your computer
system using IEBCOPY. Note that these three files have been included as
a time saving option since file one includes the JCL needed to recreate
files 3, 4, & 5.

#. JCL
   The following JCL is included in file one:

DUMPTAPE
 used to dump contents onto disk.  As written, all
 files are stored in temporary disk files only. Any files
 required to be kept on disk can be renamed, supplied with a
 VOL t and its DISP changed to KEEP/CATLG.

 If the files are going to be saved on disk, the following
 names will agree with those used in the other JCL modules on
 this tape:


+-------------------+--------------------+--------------------------+
| .. class:: center |  .. class:: center |  .. class:: center       |
|                   |                    |                          |
| DATA DESCRIPTION  | TEMPORARY NAME     | SUGGESTED PERMANENT NAME |
+-------------------+--------------------+--------------------------+
| Source Module PDS | &&SRCPDS           | TST.TRAEMTP.FORT         |
+-------------------+--------------------+--------------------------+
| Load Module PDS   | &&LOADMODS         | TST.TRA                  |
+-------------------+--------------------+--------------------------+



.. comment: the end
