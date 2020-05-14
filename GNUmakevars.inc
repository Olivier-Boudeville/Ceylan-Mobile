# Prerequisite: MOBILE_TOP must be set.

# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(MOBILE_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-Mobile
endif


# REBAR3_PROJECT_NAME should be defined on a per-project basis.
ifndef REBAR3_PROJECT_NAME
	REBAR3_PROJECT_NAME := mobile
endif


# The uniquely-defined version of this layer, for all uses (including rebar and
# hex):
#
MOBILE_VERSION := 1.0.2


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(MOBILE_VERSION)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(MOBILE_TOP)
endif


# For any quick, local, non release-packaged update thereof:
SEAPLUS_CHECKOUT_BUILD = $(LAYER_TOP)/_checkouts/seaplus
SEAPLUS_SIBLING_BUILD = $(LAYER_TOP)/../seaplus


# The OTP tree of the Seaplus application within a local OTP _build tree:
SEAPLUS_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/seaplus

# Where Seaplus BEAMs are to lie when used as an OTP application:
SEAPLUS_LOCAL_EBIN := $(SEAPLUS_LOCAL_APP)/ebin



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


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
MOBILE_BEAM_DIRS := $(MOBILE_BEAM)



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

# If wanting to use any custom (ex: debug) build:
#GAMMU_LIBS := -L$(HOME)/Software/libgammu/lib -lGammu


INC += $(MOBILE_INC) $(GAMMU_INC)


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
MOBILE_BEAM_DIRS = $(MOBILE_TOP)/src


# We rely on Myriad as well:
BEAM_DIRS += $(MOBILE_BEAM_DIRS)



ifndef VM_TEST_NAME
	VM_NAME := mobile_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := mobile_test
endif



# Overall settings section.

# One may define here:
# EXECUTION_TARGET := production / development [the default]
# ENABLE_DEBUG := false / true [the default]



# Tells whether the debug mode will be activated for the next Mobile classes to
# be built (maximum performance versus extended checkings).
#
ifeq ($(ENABLE_DEBUG),true)

	MOBILE_DEBUG_OPT := -DDEBUG_MOBILE -DMOBILE_ENABLE_LOG=1

else

	MOBILE_DEBUG_OPT := -DMOBILE_ENABLE_LOG=0

endif



# Build section.


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


ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
				$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MOBILE)



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


# Linking section.



# Checking section.

# For later reuse in the next layer of the software stack:
MOBILE_PLT_FILE := $(MOBILE_TOP)/mobile.plt


# This is the merged PLT of the level just below in the software stack.
# For 'Mobile', it is the PLT of 'Seaplus':
ifndef PREDECESSOR_PLT
	PREDECESSOR_PLT = $(SEAPLUS_PLT_FILE)
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
