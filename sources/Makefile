# -*- Mode: makefile; Syntax: makefile; indent-tabs-mode-nil; coding: utf-8; show-trailing-whitespace: t -*-
# Copyright 2019 Angelo Rossi <angelo.rossi.homelab@gmail.com>
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

# file: Makefile

# Compiler to be used.
COMPILER=gcc

# Verbose flag.
VERBOSE=

# Debug flag.
DEBUG=1

# Target name.
TARGET=gemtp

# vardim name.
VARDIM=vardim
LISTSIZE=listsize.dat

# Linker script name.
LINKER_SCRIPT=

# Root path for the project.
ROOT=..

# Include path.
INCLUDE_PATH=	${ROOT}/includes


# Library path.
LIBRARY_PATH=	${ROOT}/libraries

# Sources path
SOURCES_PATH=${ROOT}/sources/

SOURCES=

# Include global definitions.
include ${ROOT}/Makefile.global

# The default rule.
all: ${COMPILER}
all: ${COMPILER}/${VARDIM}
all: dimension
all: ${COMPILER}/${TARGET}

# List rule
lst: all
lst: ${COMPILER}/${TARGET}.lst

.PHONY: clean
# The clean rule.
clean:
	@if [ 'x${VERBOSE}' = x ];				\
	then							\
		echo " [ clean ] ${COMPILER}";			\
	else							\
		echo "rm -rf ${COMPILER}			\
				*.tab.c				\
				${ROOT}/includes/*.tab.h	\
				${ROOT}/includes/*~		\
				*.output			\
				.depend				\
				${wildcard *~} ${<}";		\
	fi
	@rm -rf ${COMPILER} 			\
		*.tab.c				\
		${ROOT}/includes/*.tab.h	\
		${ROOT}/includes/*~		\
		*.output			\
		.depend				\
		${wildcard *~} ${<}

# The rule which creates the .o and executable directory.
${COMPILER}:
	@if [ 'x${VERBOSE}' = x ];		\
	then					\
		echo " [ mkdir ] ${COMPILER}";	\
	else					\
		echo "mkdir -p ${COMPILER}";	\
	fi
	@mkdir -p ${COMPILER}

# Rule for compiling the main program.
${COMPILER}/${TARGET}: ${COMPILER}/build1.o
${COMPILER}/${TARGET}: ${COMPILER}/build2.o
${COMPILER}/${TARGET}: ${COMPILER}/calc.o
${COMPILER}/${TARGET}: ${COMPILER}/calsto.o
${COMPILER}/${TARGET}: ${COMPILER}/comb.o
${COMPILER}/${TARGET}: ${COMPILER}/data.o
${COMPILER}/${TARGET}: ${COMPILER}/elec.o
${COMPILER}/${TARGET}: ${COMPILER}/errstp.o
${COMPILER}/${TARGET}: ${COMPILER}/init.o
${COMPILER}/${TARGET}: ${COMPILER}/pmodl.o
${COMPILER}/${TARGET}: ${COMPILER}/pstmt.o
${COMPILER}/${TARGET}: ${COMPILER}/ptacs.o
${COMPILER}/${TARGET}: ${COMPILER}/puse.o
${COMPILER}/${TARGET}: ${COMPILER}/putil1.o
${COMPILER}/${TARGET}: ${COMPILER}/putil2.o
${COMPILER}/${TARGET}: ${COMPILER}/step.o
${COMPILER}/${TARGET}: ${COMPILER}/synstp.o
${COMPILER}/${TARGET}: ${COMPILER}/ntacs1.o
${COMPILER}/${TARGET}: ${COMPILER}/ntacs1a.o
${COMPILER}/${TARGET}: ${COMPILER}/ntacs1b.o
${COMPILER}/${TARGET}: ${COMPILER}/ntacs2.o
${COMPILER}/${TARGET}: ${COMPILER}/ntacs3.o
${COMPILER}/${TARGET}: ${COMPILER}/tread.o
${COMPILER}/${TARGET}: ${COMPILER}/use1.o
${COMPILER}/${TARGET}: ${COMPILER}/use2.o
${COMPILER}/${TARGET}: ${COMPILER}/xpr1.o
${COMPILER}/${TARGET}: ${COMPILER}/xpr2.o
${COMPILER}/${TARGET}: ${COMPILER}/xref1.o
${COMPILER}/${TARGET}: ${COMPILER}/xref2.o
${COMPILER}/${TARGET}: ${COMPILER}/main00.o
${COMPILER}/${TARGET}: ${COMPILER}/main10.o
${COMPILER}/${TARGET}: ${COMPILER}/over1.o
${COMPILER}/${TARGET}: ${COMPILER}/over2.o
${COMPILER}/${TARGET}: ${COMPILER}/over5.o
${COMPILER}/${TARGET}: ${COMPILER}/over6.o
${COMPILER}/${TARGET}: ${COMPILER}/over7.o
${COMPILER}/${TARGET}: ${COMPILER}/over8.o
${COMPILER}/${TARGET}: ${COMPILER}/over9.o
${COMPILER}/${TARGET}: ${COMPILER}/over10.o
${COMPILER}/${TARGET}: ${COMPILER}/over11.o
${COMPILER}/${TARGET}: ${COMPILER}/over12.o
${COMPILER}/${TARGET}: ${COMPILER}/over13.o
${COMPILER}/${TARGET}: ${COMPILER}/over14.o
${COMPILER}/${TARGET}: ${COMPILER}/over15.o
${COMPILER}/${TARGET}: ${COMPILER}/over16.o
${COMPILER}/${TARGET}: ${COMPILER}/over20.o
${COMPILER}/${TARGET}: ${COMPILER}/over29.o
${COMPILER}/${TARGET}: ${COMPILER}/over31.o
${COMPILER}/${TARGET}: ${COMPILER}/over39.o
${COMPILER}/${TARGET}: ${COMPILER}/over41.o
${COMPILER}/${TARGET}: ${COMPILER}/over42.o
${COMPILER}/${TARGET}: ${COMPILER}/over44.o
${COMPILER}/${TARGET}: ${COMPILER}/over45.o
${COMPILER}/${TARGET}: ${COMPILER}/over47.o
${COMPILER}/${TARGET}: ${COMPILER}/over51.o
${COMPILER}/${TARGET}: ${COMPILER}/over52.o
${COMPILER}/${TARGET}: ${COMPILER}/over53.o
${COMPILER}/${TARGET}: ${COMPILER}/over54.o
${COMPILER}/${TARGET}: ${COMPILER}/over55.o
${COMPILER}/${TARGET}: ${COMPILER}/calcom.o
${COMPILER}/${TARGET}: ${COMPILER}/newmods.o
${COMPILER}/${TARGET}: ${COMPILER}/datain.o
${COMPILER}/${TARGET}: ${COMPILER}/inlmfs.o
#${COMPILER}/${TARGET}: ${COMPILER}/blockdspy.o

#${COMPILER}/${TARGET}: ${COMPILER}/newmods.o
#${COMPILER}/${TARGET}: ${COMPILER}/calcom.o

# Rule for compiling vardim program.
${COMPILER}/${VARDIM}: ${COMPILER}/date44.o
${COMPILER}/${VARDIM}: ${COMPILER}/time44.o
${COMPILER}/${VARDIM}: ${COMPILER}/${VARDIM}.o

dimension:
	@if [ -f ${COMPILER}/${VARDIM} ];					\
	then									\
		if [ -f ${LISTSIZE} ];						\
		then								\
			if [ 'x${VERBOSE}' = x ];				\
			then							\
				echo " [ VARDIM ] ${LISTSIZE}";			\
			else							\
				echo "cat ${LISTSIZE} | ${COMPILER}/${VARDIM}";	\
			fi;							\
			cat ${LISTSIZE} | ${COMPILER}/${VARDIM};		\
		else								\
			echo "No ${LISTSIZE} file";				\
		fi;								\
	else									\
		echo "No ${COMPILER}/${VARDIM}";				\
	fi

# Rule to create a list file.
${COMPILER}/${TARGET}.lst: ${COMPILER}/${TARGET}

# Include the automatically generated dependency files.
ifneq (${MAKECMDGOALS},clean)
-include ${wildcard ${COMPILER}/*.d} __dummy__
endif
# DO NOT DELETE
