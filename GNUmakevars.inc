# Prerequisite: MOBILE_TOP must be set.

# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid Mobile execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



ifeq ($(ENABLE_DEBUG),true)
	ENABLE_MOBILE_DEBUG_OPT = -Dmobile_debug_mode
else
	ENABLE_MOBILE_DEBUG_OPT =
endif



# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - base: the basic mode of operation of Mobile (e.g. start/stop)
#  - driver: about any driver-related operation or issue
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#MOBILE_DEBUG_FLAGS += -Dmobile_debug_base
	#MOBILE_DEBUG_FLAGS += -Dmobile_debug_driver
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# Does not exist: MOBILE_CHECK_FLAGS += -Dmobile_check_xx
endif


ERLANG_COMPILER_OPT_BASE += -Dmobile_version=\"$(MOBILE_VERSION)\" \
							$(MOBILE_DEBUG_FLAGS)                  \
							$(MOBILE_CHECK_FLAGS)




# Useful to target for example the root of the current layer (e.g. to locate the
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


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := mobile
endif


# PROJECT_NAME_FOR_OTP should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_OTP
	PROJECT_NAME_FOR_OTP := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses (including rebar, hex
# and the code itself, see mobile:get_mobile_version{_string,}/0):
#
MOBILE_VERSION := 1.1.0


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


# When building Mobile in an OTP context from its usual (GIT) root (e.g. with
# 'make rebar3-application'), the BEAMs of Ceylan dependencies are to be found
# in the OTP build tree - rather than in a supposedly fully-built usual root for
# them:
#
# (these information are useful only at compilation-time, in order to locate the
# needed parse transforms and their dependencies; at runtime, the OTP rules
# ensure that the relevant ebin directories are in the code path)
#
# Note that this implies that the BEAM files in all ebin directories should be
# up to date with the ones in the build trees, otherwise hard-to-debug
# discrepancies may happen (these OTP/rebar-related directories act as default
# catch-alls, should no prior directory correspond in the BEAM paths).
#
SEAPLUS_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(SEAPLUS_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout (not speaking of sibling base
# directories here), at compilation time as well, modules compiled (directly or
# not) by the Seaplus parse transform will expect its modules to be located in:
#
# (a third place could be considered, in the _build tree of Seaplus itself, yet
# it would not provide any additional chance)
#
SEAPLUS_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../seaplus/ebin/


# To locate Seaplus modules from all OTP contexts:
SEAPLUS_PARSE_TRANSFORMS_PZ_OPT = -pz $(SEAPLUS_OTP_BEAM_DIR_FROM_USUAL) \
								  -pz $(SEAPLUS_OTP_BEAM_DIR_FROM_OTP)




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

# If wanting to use any custom (e.g. debug) build:
#GAMMU_LIBS := -L$(HOME)/Software/libgammu/lib -lGammu


INC += $(MOBILE_INC) $(GAMMU_INC)


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
MOBILE_BEAM_DIRS = $(MOBILE_TOP)/src $(MOBILE_TOP)/..


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



# Tells whether the debug mode will be activated for the next Mobile modules to
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

# None for Mobile:
ifndef BOOTSTRAP_MODULES
	BOOTSTRAP_MODULES = $(empty)
endif


# C subsection.




# Erlang subsection.


ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MOBILE =                \
					$(OVERALL_PZ_OPT)                           \
					-DMOBILE_ROOT=\"$(MOBILE_TOP)\"             \
					-pz $(MOBILE_TOP)/src


ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
				$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS)


# Conditionally defined so that upper layer may update these settings (e.g.
# adding pz directories):
#
ifndef ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES

	ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES =                          \
			$(ERLANG_COMPILER_OPT_BASE)                                 \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES) \
			$(MOBILE_DEBUG_OPT)

endif



# Linking section.



# Checking section.

# For later reuse in the next layer of the software stack:
MOBILE_PLT_FILE := $(MOBILE_TOP)/Ceylan-Mobile.plt


# This is the merged PLT of the level just below in the software stack.
# For 'Mobile', it is the PLT of 'Seaplus':
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(SEAPLUS_PLT_FILE)
endif



# Documentation section.


ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {macros,[{$(PROJECT_EDOC_VERSION_KEY),"$(PROJECT_VERSION)"}]}, {includes,["$(LAYER_TOP)/include", "$(MYRIAD_TOP)/..", "$(SEAPLUS_TOP)/.."]}, {overview, "$(LAYER_TOP)/overview.edoc"}]'
endif



# We define the path to the Seaplus layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (e.g. myriad) as opposed to
# project/repository names (e.g. Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(MOBILE_TOP)/_checkouts/seaplus/.),)
	SEAPLUS_TOP = $(MOBILE_TOP)/_checkouts/seaplus
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(MOBILE_TOP)/../seaplus/.),)
		SEAPLUS_TOP = $(MOBILE_TOP)/../seaplus
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(MOBILE_TOP)/_build/default/lib/seaplus/.),)
			SEAPLUS_TOP = $(MOBILE_TOP)/_build/default/lib/seaplus
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-Seaplus from '$(realpath .)'.)
		endif
	endif
endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(SEAPLUS_TOP)
endif

ifndef BASE_PLT_FILES
	BASE_PLT_FILES = $(PREDECESSOR_PLT_FILE)
endif
