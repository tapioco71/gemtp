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

.. code::

   CALL DATE44 ( DATE1(1) )
   CALL TIME44 ( TCLOCK(1) )
     < <  Build legal VAX/VMS file name using the digits
          of DATE1 and TCLOCK; put result in FILE25      > >

.. code::

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

.. code::

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

.. code::

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


.. raw:: pdf

    PageBreak


.. comment: the end
