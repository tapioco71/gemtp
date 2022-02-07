.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-

Conventions
================================================================================

Documents
--------------------------------------------------------------------------------

The documents are written in reStructuredText markup format.  They can
be processed by the variou rst tool chains, notably ``rst2html`` and
``rst2pdf``.

The title underlines used are: # (two +s), +, = (two -), -, " (two '), and ' ::

    H1 title
    ########################

    H2 title
    ++++++++++++++++++++++++

    H3 title
    ========================

    H4 title
    ------------------------

    H5 title
    """"""""""""""""""""""""

    H6 title
    '''''''''''''''''''''''''



Files included in the specification documents:

- reStructuredText files:

    u-\*.txt
        contain a single use case to be included (or not) in the various
        specification documents.

    o-\*.txt
        contain an object (class, entity) description section.

    w-\*.txt
        contain a single wireframe to be included (or not) in the various
        specification documents.

    o-template.txt
        contains a template for class sections. You may copy it
        when creating new class description sections.

    u-template.txt
        contains a template for use case sections. You may copy it when
        creating new use cases.


- png images, generated from yuml text files processed by _`http://yuml.me`:

    u-\*.yuml
        contains a yuml textual representation of a use case diagram.

    o-\*.yuml
        contains a yuml textual representation of a class diagram (object diagram).

    s-\*.yuml
        contains a yuml textual representation of a state diagram.


Class names
--------------------------------------------------------------------------------

In the specifications class names are written in
``Capitalized-Lisp-Convention``.

However, the corresponding files names are all lowercase, with the dashes in the
class names removed, and prefixed by ``o-``, ``s-``, ``u-`` depending on the
contents of the file (object (ie. class) diagram or description, state diagram
or use case diagram or description).

So a class named ``Capitalized-Lisp-Convention`` would have:

- description file name: ``o-capitalizedlispconvention.txt``,

- state diagram file name: ``s-capitalizedlispconvention.yuml``,

- use case file names: ``u-capitalizedlispconvention-does-something.yuml``,

etc.

As much as possible, entities and use case references are written in the
document as hyperlink, using the ``\`Example\`\_`` reStructured Text syntax.

Remember: any (unique) section title is a potential hypertext target, so you can
also add hyperlinks to sections writing ``\`Section Title\`_``.


Use case descriptions
--------------------------------------------------------------------------------

The pre- and post-conditions in use case sections should be written as much as
possible as lists of idenpendent conditions, so that we may easily
copy-and-paste post-conditions from one use case to pre-conditions of other use
cases.


Dependencies
================================================================================

`reStructuredText` file may include other files using directive.

The ``make-rst-depends`` clisp script generates all the dependencies
obtained thru the ``.. image::`` and the ``.. include::`` directives.
It is automatically invoked by the Makefile.

To generate a new PDF file from a reStructuredText file, add to the Makefile: ::

    -include newFile.d
    clean::
        -rm newFile.d

and add ``newFile.pdf`` to the ``all:`` target.



TODO
================================================================================

#. Extract the use cases from specifications.txt like done with
   ``minispecs.txt``.  Be careful not to collide with use cases from
   ``minispecs.txt`` if they're different.  The two files may and
   should share use cases and other resources, but ``minispecs.txt``
   may also define simplier variants of some use cases.

#. Complete the specifications of the minispecs use cases.

#. Complete the wireframes for the applications.

#. Start analysis and design.
