# Here are gathered explicit (immediate, static) rules for Mobile.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: MOBILE_TOP must be set.


.PHONY: default-mobile-rule clean clean-obj \
		clean-ast-debug clean-ast-debug-mobile


# The first non-generic, therefore default, rule:
default-mobile-rule: all
