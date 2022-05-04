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

# Toolchain
TOOLCHAIN=gnu

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

# Includes path.
INCLUDES_PATH=${ROOT}/includes

# Modules path.
MODULES_PATH=${ROOT}/modules

# Library path.
LIBRARY_PATH=${ROOT}/libraries

# Sources path
SOURCES_PATH=${ROOT}/sources

SOURCES=

MODULES=

# Include global definitions.
include ${ROOT}/Makefile.global

# The default rule.
all: ${TOOLCHAIN}
all: ${TOOLCHAIN}/${TARGET}

# Other rules.

${TARGET}: ${TOOLCHAIN}
${TARGET}: ${TOOLCHAIN}/${TARGET}

vardim: ${TOOLCHAIN}
vardim: ${TOOLCHAIN}/${VARDIM}

# dimensioning via vardim
setup: vardim
setup: dimension
setup: ${MODULES_PATH}/labcom.f90
setup: ${SOURCES_PATH}/newmods.f90

# List rule
lst: all
lst: ${TOOLCHAIN}/${TARGET}.lst

.PHONY: clean
# The clean rule.
clean:	clear_modules
clean:
	@if [ 'x${VERBOSE}' = x ];					\
	then								\
		echo " [ CLEAN ] ${TOOLCHAIN}";				\
	else								\
		echo "rm -rf ${TOOLCHAIN}				\
				*.tab.c					\
				${ROOT}/includes/*.tab.h		\
				${ROOT}/includes/*~			\
				*.o					\
				*.output				\
				.depend					\
				fort.*					\
				${ROOT}/${MODULES_PATH}/labcom.f90	\
				${ROOT}/${SOURCES_PATH}/newmods.f90	\
				${wildcard *~} ${<}";			\
	fi
	@rm -rf ${TOOLCHAIN} 				\
		*.tab.c					\
		${ROOT}/includes/*.tab.h		\
		${ROOT}/includes/*~			\
		*.o					\
		*.output				\
		.depend					\
		fort.*					\
		${ROOT}/${MODULES_PATH}/labcom.f90	\
		${ROOT}/${SOURCES_PATH}/newmods.f90	\
		${wildcard *~} ${<}

clear_modules:
	@rm -rf ${TOOLCHAIN}/*.mod

# The rule which creates the .o and executable directory.
${TOOLCHAIN}:
	@if [ 'x${VERBOSE}' = x ];		\
	then					\
		echo " [ MKDIR ] ${TOOLCHAIN}";	\
	else					\
		echo "mkdir -p ${TOOLCHAIN}";	\
	fi
	@mkdir -p ${TOOLCHAIN}

dimension:
	@if [ -f ${TOOLCHAIN}/${VARDIM} ];						\
	then										\
		if [ -f ${LISTSIZE} ];							\
		then									\
			if [ 'x${VERBOSE}' = x ];					\
			then								\
				echo " [ VARDIM ] ${LISTSIZE}";				\
			else								\
				echo "cat ${LISTSIZE} | ${TOOLCHAIN}/${VARDIM}";	\
			fi;								\
			cat ${LISTSIZE} | ${TOOLCHAIN}/${VARDIM};			\
		else									\
			echo "No ${LISTSIZE} file";					\
		fi;									\
	else										\
		echo "No ${TOOLCHAIN}/${VARDIM}";					\
	fi

${SOURCES_PATH}/newmods.f90:
	@if [ -f fort.7 ];						\
	then								\
		if [ 'x${VERBOSE}' = x ];				\
		then							\
			echo " [ NEWMODS ] ";				\
		else							\
			echo "cp fort.7 newmods.f90";			\
		fi;							\
		cp fort.7 newmods.f90;					\
	else								\
		echo "Cannot create newmods.f90.";			\
	fi

${MODULES_PATH}/labcom.f90:
	@if [ -f fort.9 ];						\
	then								\
		if [ 'x${VERBOSE}' = x ];				\
		then							\
			echo " [ LABCOM ] ";				\
		else							\
			echo "cp fort.9 ${MODULES_PATH}/labcom.f90";	\
		fi;							\
		cp fort.9 ${MODULES_PATH}/labcom.f90;			\
	else								\
		echo "Cannot create labcom.f90.";			\
	fi

# The rule to creates modules.
modules: ${TOOLCHAIN}
modules: ${TOOLCHAIN}/${TARGET}

# Modules.
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/linemodel.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/systematic.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/blkcom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/labcom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/tracom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/pckcom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/strcom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/freedom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/bcddat.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/bcdtim.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/movcop.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/random.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/volt45.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/deck10.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/deck29.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/deck31.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/deck39.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/deck44.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/deck47.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/dekspy.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/dekplt.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/fixcom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/indcom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/iocons.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/labl02.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/labl29.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/labl39.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/labl44.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/ovr44c.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/labl47.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/syncom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/synmac.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/tacsar.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/tacsto.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/umdeck.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/space1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/space2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/veccom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/umlocl.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/fdqlcl.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/kwtvax.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/comlock.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/a8sw.o

# Rule for compiling the main program.
# ${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/blockdspy.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/build1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/build2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/calc.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/calsto.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/comb.o
# ${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/data.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/elec.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/errstp.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/pmodl.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/pstmt.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/ptacs.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/puse.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/putil1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/putil2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/step.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/synstp.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/ntacs1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/ntacs1a.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/ntacs1b.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/ntacs2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/ntacs3.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/tread.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/use1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/use2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/xpr1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/xpr2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/xref1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/xref2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/main00.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/main10.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/newmods.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/init.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/datain.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/calcom.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/inlmfs.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over1.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over2.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over5.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over6.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over7.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over8.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over9.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over10.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over11.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over12.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over13.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over14.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over15.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over16.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over20.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over29.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over31.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over39.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over41.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over42.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over44.o
# ${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over45.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over47.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over51.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over52.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over53.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over54.o
${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/over55.o
# ${TOOLCHAIN}/${TARGET}: ${TOOLCHAIN}/inlmfs.o

# Rule for compiling vardim program.
${TOOLCHAIN}/${VARDIM}: ${TOOLCHAIN}/bcddat.o
${TOOLCHAIN}/${VARDIM}: ${TOOLCHAIN}/bcdtim.o
${TOOLCHAIN}/${VARDIM}: ${TOOLCHAIN}/${VARDIM}.o

# Rule to create a list file.
${TOOLCHAIN}/${TARGET}.lst: ${TOOLCHAIN}/${TARGET}

# Include the automatically generated dependency files.
ifneq (${MAKECMDGOALS},clean)
-include ${wildcard ${TOOLCHAIN}/*.d} __dummy__
endif
# DO NOT DELETE
