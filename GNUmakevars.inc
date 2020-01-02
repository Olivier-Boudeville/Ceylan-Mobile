# Prerequisite: MOBILE_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Mobile
endif


MOBILE_VERSION := 1.0.1

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(MOBILE_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = Ceylan-Mobile
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(MOBILE_TOP)
endif


MOBILE_RELEASE_BASE := mobile-$(PROJECT_VERSION)

MOBILE_RELEASE_ARCHIVE_ZIP := $(MOBILE_RELEASE_BASE).zip
MOBILE_RELEASE_ARCHIVE_BZ2 := $(MOBILE_RELEASE_BASE).tar.bz2
MOBILE_RELEASE_ARCHIVE_XZ  := $(MOBILE_RELEASE_BASE).tar.xz


# Source section.
MOBILE_SRC := $(MOBILE_TOP)/src

# BEAM path section.
MOBILE_BEAM := $(MOBILE_SRC)

# Include path section.
MOBILE_INC = -I$(MOBILE_SRC)



# Gammu section.

# Main header is typically /usr/include/gammu/gammu.h:

#GAMMU_INC_DIR := /usr/include/gammu

# Better than -I$(GAMMU_INC_DIR):
GAMMU_INC := $(shell pkg-config --cflags gammu)



# Library is typically a symbolic link pointing to, say,
# /usr/lib/libGammu.so.8.1.40.0:

GAMMU_LIB_DIR := /usr/lib
GAMMU_BASE_NAME := Gammu
GAMMU_LIB := lib$(GAMMU_BASE_NAME).so
GAMMU_LIB_PATH = $(GAMMU_LIB_DIR)/$(GAMMU_LIB)

# If wanting to use the system-wide library:
GAMMU_LIBS := $(shell pkg-config --libs gammu)

# If wanting to use aany custom (ex: debug) build:
#GAMMU_LIBS := -L$(HOME)/Software/libgammu/lib -lGammu


DOC_ROOT       = $(MOBILE_TOP)/doc
MOBILE_DOC_DIR = $(DOC_ROOT)/web/main/documentation/mobile


INC += $(MOBILE_INC) $(GAMMU_INC)



ifndef VM_TEST_NAME

VM_NAME := mobile_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := mobile_test

endif




# So that they can be fetched from outside
# (see the 'list-beam-dirs' target)
#
MOBILE_BEAM_DIRS = $(MOBILE_TOP)/src


# We rely on Myriad as well:
BEAM_DIRS += $(MOBILE_BEAM_DIRS)


# Overall settings section.


#EXECUTION_TARGET=production

ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET=development

endif


ifeq ($(EXECUTION_TARGET),development)

  #$(info Execution target is development)

  ENABLE_DEBUG=true

else ifeq ($(EXECUTION_TARGET),production)

  #$(info Execution target is production)

  ENABLE_DEBUG=false

else

  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



#ENABLE_DEBUG=false

# By default, this debug mode is enabled:
ifndef ENABLE_DEBUG

	ENABLE_DEBUG = true

endif



# Tells whether the debug mode will be activated for the next Mobile classes to
# be built (maximum performance versus extended checkings).
#
ifeq ($(ENABLE_DEBUG),true)

	MOBILE_DEBUG_OPT = -DDEBUG_MOBILE -DMOBILE_ENABLE_LOG=1

else

	MOBILE_DEBUG_OPT = -DMOBILE_ENABLE_LOG=0

endif



# Build section.

# Libtool not used anymore:

#LIBTOOL = $(shell which libtool)

# This is a C-based (not C++) example:
#LIBTOOL_C_OPT += --tag=CC --verbose


# Compiler section.

# (most settings inherited from Seaplus)

MOBILE_BASE_NAME := mobile



# C subsection.





# Erlang subsection.


ifndef COMPILER_OPT_FOR_MOBILE_CLASSES

	   COMPILER_OPT_FOR_MOBILE_CLASSES =                      \
			$(ERLANG_COMPILER_OPT_BASE)                        \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MOBILE)

endif


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES

	   # Standard (non-class) modules in Mobile shall not be compiled with the
	   #  Mobile parse-transform, but with the Myriad one:
	   #
	   ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
				$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MOBILE)

#endif


# Conditionally defined so that upper layer may update these settings (ex:
# adding pz directories):
#
ifndef ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES

	ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES =                          \
			$(ERLANG_COMPILER_OPT_BASE)                                 \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES) \
			$(MOBILE_DEBUG_OPT)

endif


## Parse-transform related section.


# No PARSE_TRANSFORM_DIRS to be defined here.

# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the Mobile one:
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MOBILE =                \
					$(OVERALL_PZ_OPT)                           \
					-DMOBILE_ROOT=\"$(MOBILE_TOP)\"             \
					-pz $(MOBILE_TOP)/src



# Linking section.





# Checking section.

# For later reuse in the next layer of the software stack:
MOBILE_PLT_FILE := $(MOBILE_TOP)/mobile.plt


# This is the merged PLT of the level just below in the software stack.
# For 'MOBILE', it is the PLT of 'Myriad':
ifndef PREDECESSOR_PLT
	   PREDECESSOR_PLT = $(MYRIAD_PLT_FILE)
endif

# To use a generic rule to produce it (unless already defined):
ifndef PLT_FILE
	   PLT_FILE = $(PACKAGE_TOP)/$(PACKAGE_NAME).plt
endif



# We define the path to the Seaplus layer:
#
# (it may be a symbolic link pointing to the actual Seaplus package to be used,
# which itself may be either a 'Ceylan-Seaplus' directory or a 'seaplus' one)
#
ifneq ($(wildcard $(MOBILE_TOP)/../seaplus),)

	SEAPLUS_TOP = $(MOBILE_TOP)/../seaplus

else

	# Default:
	SEAPLUS_TOP = $(MOBILE_TOP)/../Ceylan-Seaplus

endif
