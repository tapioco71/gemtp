.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-
    ###
    +++
    ===
    ---
    """
    ^^^
    +++


.. comment: Diagrams are made with: http://yuml.me/diagram/nofunky/class/draw
            This is a reStructuredText file. http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
            Process it with rst2pdf and rst2html or pandoc.


.. meta::
   :description: Rule Book documentation for the BPA ATP EMTP, Angelo Rossi
   :keywords: Documentation, Rule Book, ATP EMTP
.. header:: ###Section###
.. footer::
   +---------------------------+-------------+
   | smart-monitoring-tools.it |  ###Page### |
   +---------------------------+-------------+


################################################################################
BPA ATP EMTP -- Rule Book
################################################################################

.. raw:: pdf

    PageBreak


.. contents:: Table of Contents
   :depth: 3

.. sectnum::
   :depth: 4
   :start: 1

.. raw:: pdf

    PageBreak


| Methods Development Branch, Route EOGB
| Division of System Engineering
| Bonneville Power Administration
| P. O. Box 3621
| Portland, Oregon  97208
| Phone:  (503) 230 - 4404, or 4402


.. raw:: pdf

    PageBreak


.. list-table:: Revisions
  :widths: 10 10 30 20
  :header-rows: 1

  * - Date
    - Version
    - Revision
    - Name
  * - 1973/01
    -
    -
    -
  * - 1974/07
    -
    -
    -
  * - 1976/01
    -
    -
    -
  * - 1977/11
    -
    -
    -
  * - 1980/05
    -
    -
    -
  * - 1980/09
    -
    -
    -
  * - 1982/04
    -
    -
    -
  * - 1983/03
    -
    -
    -
  * - 1084/06
    -
    -
    -


.. raw:: pdf

    PageBreak


.. comment:

   ;; to insert a new chapter, add its name (without o- or extension) to the list in dolist,
   ;; and type C-x C-e after the sexp.

   (let* ((start (point))
         (end (if (search-forward ".. comment: end-classes" nil t)
                 (progn (goto-char (match-end 0))
                        (end-of-line)
                        (point))
                 start)))
    (delete-region start end)
    (dolist (chapter '(general
                       emtp-input-data
                       descripion-emtp-output
                       formulas
                       interactive-CRT
                       transformer-representation
                       auxiliary-supporting-routines
                       tacs
                       interactivity
                       index
                         )
              (insert "\n.. comment: end-classes   "))
      (insert (format  "\n.. raw:: pdf\n\n    PageBreak\n\n.. include:: c-%s.rst\n" chapter))))
.. raw:: pdf

    PageBreak

.. include:: c-general.rst

.. raw:: pdf

    PageBreak

.. include:: c-emtp-input-data.rst

.. comment: end-classes

.. raw:: pdf

    PageBreak

.. comment: the end
