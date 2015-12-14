##===- examples/smt-constraint-manager/Makefile ------------*- Makefile -*-===##
# 
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
# 
##===----------------------------------------------------------------------===##

CLANG_LEVEL := ../..
LIBRARYNAME = SMTConstraintManagerPlugin

LINK_LIBS_IN_SHARED = 0
LOADABLE_MODULE = 1

include $(CLANG_LEVEL)/Makefile

ifeq ($(OS),Darwin)
  LDFLAGS=-Wl,-undefined,dynamic_lookup
endif

CXXFLAGS+=-I$(STPPREFIX)/include
LDFLAGS+=-L$(STPPREFIX)/lib -lstp
