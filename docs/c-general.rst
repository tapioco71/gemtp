.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-


General Introductory Information about the EMTP
*******************************************************************************

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
EMTP memoranda (`REF7`_),  so such information is thereby available
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
   documented in `REF8`_, Volume VI, 5 December 1976, pagination
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
   Toronto, who inspired it (see `REF10`_ and `REF21`_).  Alex Dabuleanu
   (now deceased;  formerly an OH employee) did much of the
   original work, as a student of Prof. Semlyen.  Through the end
   of 1979, Russel Brierley of OH continued to be the premier
   expert in use of the feature, and researcher who paved the way
   for generalization to complex exponentials (theory as per `REF21`_
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
   British Columbia in Vancouver.  See `REF12`_.  Implementation
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
   convolution" modeling (see `REF11`_), the implementation of which
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
   `REF15`_.  Implementation in the EMTP was under contract with BPA (see `REF8`_,
   Vol. VII, 23 December 1977, pagination CBVB).  Between 1978 and 1982, Vladimir
   was with Ontario Hydro, and has since moved to Systems Control, Inc. of Palo
   Alto, California.

#. Dr. John Hauer of BPA joined the EMTP development effort during the
   late fall of 1979, to rescue our suspended "SEMLYEN SETUP" work.  See `REF8`_,
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
   the EMTP load flow.  See `REF8`_, Vol. XIII, 23 July 1983, Section II, pages
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
which must be solved at each step (see [ref1]_).  These are placed in nodal-
admittance form (with new unknown voltages as variables), and are solved by ordered
triangular factorization ([ref4]_).

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


Supporting Reference Material
===============================================================================

Theory behind the EMTP is scattered through various technical
papers and a few books.  The following may be found useful for reference
puroses:

.. [REF1] H. W. Dommel, "Digital computer solution of electromagnetic
          transients in single and multiphase networks", IEEE Trans., vol.
          PAS 88, pp. 388-399, April 1969.

.. [REF2] H. W. Dommel, "Nonlinear and time varying elements in digital
          simulation of electromagnetic transients", IEEE Trans vol. PAS-90,
          pp. 2561-2567, Nov/Dec 1971.

.. [REF3] W. S. Meyer, H. W. Dommel, "Numerical modelling of frequency
          dependent transmission line parameters in an electromagnetic
          transients program", IEEE Trans, vol. PAS-93, pp. 1401-1409.
          Sep/Oct 1974.

.. [REF4] W. F. Tinney, J. W. Walker, "Direct solutions of sparse network
          equations by optimally ordered triangular factorization", Proc.
          IEEE, vol. 55 pp. 1801-1809.  November 1967.  Also available in
          1967 IEEE PES PICA Conference Record.

.. [REF7] H. W. Dommel, W. S. Meyer, "Computation of electromagnetic
          transients", Proc. IEEE, vol. 62, pp. 983-993, July 1974.

.. [REF8] W. S. Meyer, "Transients Program Memoranda". Approximately 150
          pages per volume, this concerns the EMTP development memoranda
          which are regular issued as progress made.  Individual memos
          are bound when there has been sufficient accumulation to form
          a volume.  As of Frebruary, 1984, there were 14 volumes.  Only the
          most recent volume is generally available, however, so do not write
          requesting a complete set.  Older volumes are out of print.  When a
          new volume is released, a fixed number of copies are printed;  and
          when these are exhausted, the volume becomes unavailable through
          regular channels (a Freedom of Information request would always be
          honored, of course, but a copying fee would be charged).  As of
          April, 1984, Memoranda writing has been suspended indefinitely
          with the 74-page contribution dated 19#January 1984 (finished on
          25 March 1984) being the final one unless management encourages
          a resumption.  A request for guidance in setting such priorities
          was made of higher management on March 26th, 1984.

.. [REF9] D. R. Carroll, W. S. Meyer, "Digital and hybrid computation of
          electromagnetic transients in power networks", Sixth Annual
          Pittsburg Conference on Modeling and Simulation, Pittsburg,
          Pennsylvania, April 1975.

.. [REF10] A. Semlyen, A. Dabuleanu, "Fast and accurate switching transient
           calculations on transmission lines with ground return using
           recursive convolutions", IEEE Trans., vol. PAS-94, pp. 561-571,
           1975.

.. [REF11] A. Ametani, "A highly efficient method for calculating transmission
           line transients", IEEE Trans., vol. PAS-95 pp. 1545-1551,
           Sept/Oct 1976.

.. [REF12] L. Dube, H. W. Dommel, "Simulation of control systems in a
           electromagnetic transients program with TACS", IEEE PES PICA
           Conference Record, vol. 10, pp. 266-271, 1977.

.. [REF13] W. S. Meyer, "Machine translation of an electromagnetic transients
           program (EMTP) among different digital computer systems", IEEE PES
           PICA Conference Record, vol. 10, pp. 272-277, 1977.  See also
           [REF8]_, Vol. VI, 5 January 1977, pagination PICA.

.. [REF14] V. Brandwajn, H. W. Dommel, "Synchronous machine parameters in
           analysis of electromagnetic transients". Canadian Communications
           and Power Conference, Montreal (P.Q., Canada), October 20-22, 1976.

.. [REF15] V. Brandwajn, "Synchronous Generator Models for the Simulation of
           Electromagnetic Transients", Ph.D. thesis written at the University
           of British Columbia (Vancouver, B.C., Canada), April 1977, 117 pages
           plus preface.

.. [REF16] G. Gross and M. C. Hall, "Synchronous machine and torsional dynamics
           simulation in the computation of electromagnetic transients", IEEE
           Trans., vol. PAS-97, pp. 1074-1086, July/Aug 1978.

.. [REF18] IEEE SSR Task Force, "First benchmark model for computer simulation
           of subsynchronous resonance", IEEE Trans., vol. PAS-96, pp. 1565-
           1572, Sept/Oct 1977.

.. [REF19] P. M. Anderson, A. A. Fouad, "Power System Control and Stability".
           Ames, Iowa (USA): The Iowa State University Press, 1977.

.. [REF20] V. Brandwajn, H. W. Dommel, "A new method for interfacing generator
           models with an electromagnetic transient program", IEEE PES PICA
           Conference Record, Vol. 10, pp. 260-265, 1977.

.. [REF21] A. Semlyen, "Contributions to the theory of calculation of
           electromagnetic transients on transmission lines with frequency
           dependent parameters", paper submitted to IEEE for presentation at
           the 1979 PES Summer Meeting.

.. [REF22] D. Van Dommelen, Editor, "EMTP Newsletter".  Published about
           4 times/year in Leuven, Belgium, issue number one appeared
           during July of 1979.  For information about subscription, see
           "Newsletter" or "Europe" or "LEC" in the index.

.. [REF23] V. Brandwajn, W. S. Meyer, H. W. Dommel, "Synchronous machine
           initialization for unbalanced network conditions within an electro-
           magnetic transients program", IEEE PES PICA Conference Record, vol.
           11, pp. ???-???, 1979.  Also available in [REF8]_, Vol. VII,
           28 January 1978, pages TDCE-13 through 16.

.. [REF24] H. W. Dommel, B. C. Chiu, W. S. Meyer, "Analyzing transients in
           ac/dc systems with the BPA Electromagnetic Transients Program",
           Proc. IEEE International Conference on Overvoltages and
           Compensation on Integrated ac-dc Systems, Winnipeg, Canada,
           July 8-12, 1980.

.. [REF25] A. Ametani, "A general formulation of impedance and admittance of
           cables", IEEE Trans., vol. PAS-99(3), pp. 902-910, 1980.

.. [REF26] A. Ametani, "Wave propagation characteristics of cables", IEEE
           Trans., vol. PAS-99, No. 2, pp. 499-505, March/April 1980.

.. [REF27] R. H. Lasseter, D. M. Demarest, F. J. Ellert, "Transient Over-
           voltages on the neutral bus of HVDC transmission systems", IEEE
           PES paper No. A78 707-4, presented at the 1978 Summer Meeting.

.. [REF28] R. H. Lasseter, "Electrical characteristics of long overhead HVDC
           transmission lines", IEEE PES paper No. A79 535-6, presented
           at the 1979 Summer Meeting.

.. [REF29] A. G. Phadke, Course Organizer: "Digital Simulation of Electrical
           Transient Phenomena". IEEE Tutorial Course No. 81 EH0173-5-PWR,
           last given at the 1982 IEEE PES Winter Meeting in New York City.
           Sixty pages in length, the notes for this one-day IEEE course
           consist of six chapters, as follows:

           * I --- Introduction to Power System Transients (A. G. Phadke);
           * II --- Present day procedures and program (W. S. Meyer);
           * III --- Extension of the basic solution methods (H. W. Dommel);
           * IV --- Synchronous machine modeling (D. W. Olive);
           * V --- EMTP synchronous machine modeling (D. H. Baker);
           * VI --- HVDC converters & controls (K. G. Fehrle, R. H. Lasseter).

           Anyone seeking a broad overview of the full range of EMTP usage is
           advised to consult this "book".  Although just an outline, it
           points to numerous other sources of information (there are 145
           references), and is the only known such organized summary.

.. [REF30] K. C. Lee, H. W. Dommel, "Addition of modal analysis to the U.B.C.
           Line Constants Program", research report to B.C. Hydro and Power
           Authority, Vancouver, Canada, January, 1980, published by the
           Electrical Engineering Department of the University of British
           Columbia.

.. [REF31] J. G. Frame, N. Mohan, T.-H. Liu, "Hysteresis modeling in am
           electromagnetic transients program", IEEE PES paper No. 82 WM
           152-7, presented at the 1982 Winter Meeting.

.. [REF32] R. H. Lasseter, S. Y. Lee, "Digital simulation of static VAR
           system transients", IEEE PES paper No. 82 WM 178-2, presented
           at the 1982 Winter Meeting.

.. [REF33] V. Brandwajn, H. W. Dommel, I. I. Dommel, "Matrix representation
           of three-phase N-winding transformers for steady-state and transient
           studies", IEEE PES paper No. 81 SM 429-0, presented at the 1981
           Summer Meeting.

.. [REF34] H. W. Dommel, "Transformer models in the simulation of electro-
           magnetic transients", Fifth Power Systems Computation Conference
           held in Cambridge, England, September 1-5, 1975.

.. [REF35] A. S. Morched, V. Brandwajn, "Transmission network equiva-
           lents for electromagnetic transients studies", IEEE PES
           paper No. 83 WM 039-5, presented at the 1983 Winter Meeting.

.. [REF36] H. K. Lauw, W. S. Meyer, "Universal machine modeling for the
           representation of roatating electric machinery in an electro-
           magnetic transients program", IEEE PES paper No. 81 SM
           430-8, presented at the 1981 Summer Meeting. Published in
           Trans. PA&S during April of 1982?

.. [REF37] D. Van Dommelen, "Optimization of initial values of mechanical
           variables of turbine-generator units in an electromagnetic
           transients program", IEEE PES paper No. 81 SM 500-8,
           presented at the 1981 Summer Meeting, and later published in
           Trans. PA&S, Vol. PAS-100, no. 12, pp. 4990-4994, December 1981.

.. [REF38] IEEE Working Group, "Modeling of Current-Limiting Surge
           Arresters", IEEE Trans. PA&S, vol. PAS-100, pp. 4033-4040,
           August 1981.

.. [REF39] J. R. Marti, "Accurate Modelling of Frequency-Dependent
           Transmission Lines in Electromagnetic Transients Simulations"
           Pro. IEEE Power Industry Computer Applications (PICA)
           Conference, Philadelphia, PA, 9 pages, May 1981.

.. [REF40] V. Brandwajn, H. W. Dommel, "Numerical oscillations in the
           transient analysis of circuits with implicit integration
           techniques", paper presented at the XXIV-th Midwestern
           Symposium on Circuits ans Systems, Puebla, Mexico, August,
           1983.

.. [REF41] D. Van. Dommelen, Chairman. The European EMTP Users Group
           (see index) has held meetings biannually since its inception
           during 1981 (the first meeting, an all-Belgian affair, took
           place on 21 January 1981).  Each meeting has an associated
           set of conference documents, of which some have been listed
           in EMTP Memoranda (e.g., [REF8]_, Vol. XIV, 2 January 1984,
           pages FMOE-1 and 2).  Anyone interested in EMTP usage should
           not overlook this valuable source of information about the
           program.

.. [REF42] F. L. Alvarado, R. H. Lasseter, H. Kwon, S. K. Mong, "A
           module-oriented EMTP interface", paper presented at the
           1984 Winter Meeting of the IEEE PES in Dallas, Texsas.

.. [REF43] Frank Rasmussen of ElKraft Power Company Ltd., Copenhagen,
           Denmark, is encouraged to write a paper describing his
           pioneering research and development which made the EMTP load flow
           possible.  If and when this appears, it will be Reference 43.


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




.. raw:: pdf

    PageBreak


.. comment: the end
