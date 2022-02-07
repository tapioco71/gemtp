# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

PDFS= rule-book.pdf README.pdf

all: documents
documents: $(PDFS)

include ../tools/common.make

.PHONY:: questions tbd

-include rule-book.d
clean::
	@if [ 'x${VERBOSE}' = x ];		\
	then					\
		echo " [ clean ] rule-book.d ";	\
	else					\
		echo " -rm rule-book.d ";	\
	fi
	@rm rule-book.d


-include README.d
clean::
	@if [ 'x${VERBOSE}' = x ];		\
	then					\
		echo " [ clean ] README.d ";	\
	else					\
		echo " -rm README.d ";		\
	fi
	@rm README.d


questions:
	grep -nH -e '\<\(QUESTIONS\?\|HOW\)\>' *.txt

tbd:
	grep -nH -e '\<\(TBD\|GENERAL_DESCRIPTION\|DESCRIPTION_OF_THE_RESPONSIBILITIES_OF_THE_ENTITY\|LIST_OF_ATTRIBUTES\|LIST_OF_ASSOCIATIONS_WITH_OTHER_ENTITIES\)\>' *.txt
