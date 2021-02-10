MOBILE_TOP = .


.PHONY: help help-intro help-mobile check-gammu                          \
		all register-version-in-header register-mobile list-beam-dirs    \
		add-prerequisite-plts link-plt                                   \
		send-release release release-zip release-bz2 release-xz          \
		prepare-release clean-release clean-archive                      \
		info-erlang-for-c info-paths info-compile info-parse-transform


MODULES_DIRS = src doc test


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


MOBILE_RELEASES = $(MOBILE_RELEASE_ARCHIVE_BZ2) \
				  $(MOBILE_RELEASE_ARCHIVE_ZIP) \
				  $(MOBILE_RELEASE_ARCHIVE_XZ)


# First target for default:
help: help-intro help-mobile


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-mobile:
	@cd $(MYRIAD_TOP) && $(MAKE) -s help-myriad


all: check-gammu



check-gammu:
	@echo "Checking availability of gammu"
	@if pkg-config gammu; then echo "Gammu is available." ; else echo "Error, the Gammu dependency does not seem available. Please install this package beforehand (ex: 'pacman -Sy gammu' on Arch Linux)." 1>&2 ; exit 12 ; fi


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 51 ; else \
	$(MAKE) register-mobile ; fi


register-mobile:
	@echo "-define( mobile_version, \"$(MOBILE_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(MOBILE_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'mobile' naming:
link-plt:
	@/bin/ln -s --force $(PLT_FILE) $(MOBILE_PLT_FILE)


# Note: the source archives are not produced in this directory, but in its
# parent, so that everything related to Mobile (including these rules) remains
# self-contained.

send-release:

release: release-zip release-bz2 release-xz
	@$(MAKE) clean-release


release-zip: prepare-release
	@echo "     Creating Mobile release archive $(MOBILE_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(MOBILE_RELEASE_ARCHIVE_ZIP) $(MOBILE_RELEASE_BASE) \
	&& echo "     Archive $(MOBILE_RELEASE_ARCHIVE_ZIP) ready in "`pwd`


release-bz2: prepare-release
	@echo "     Creating Mobile release archive $(MOBILE_RELEASE_ARCHIVE_BZ2)"
	@cd .. && tar chvjf $(MOBILE_RELEASE_ARCHIVE_BZ2) $(MOBILE_RELEASE_BASE) \
	&& echo "     Archive $(MOBILE_RELEASE_ARCHIVE_BZ2) ready in "`pwd`


release-xz: prepare-release
	@echo "     Creating Mobile release archive $(MOBILE_RELEASE_ARCHIVE_XZ)"
	@cd .. && tar chvjf $(MOBILE_RELEASE_ARCHIVE_XZ) $(MOBILE_RELEASE_BASE) \
	&& echo "     Archive $(MOBILE_RELEASE_ARCHIVE_XZ) ready in "`pwd`


# The '-L' option with cp is used so that symbolic links are replaced by their
# actual target file, otherwise tar would include dead links in releases.
prepare-release: clean clean-release
	@echo "     Preparing release archive for Mobile $(MOBILE_VERSION)"
	@cd .. && mkdir -p $(MOBILE_RELEASE_BASE) && /bin/cp -L -r myriad mobile $(MOBILE_RELEASE_BASE)
	@cd ../$(MOBILE_RELEASE_BASE) && mv mobile/top-GNUmakefile-for-releases GNUmakefile
	-@cd .. && find $(MOBILE_RELEASE_BASE) -type d -a -name '.git' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	-@cd .. && find $(MOBILE_RELEASE_BASE) -type f -a -name '*.beam' -exec /bin/rm -f '{}' ';' 2>/dev/null


clean: clean-release clean-archive


clean-release:
	@echo "   Cleaning release archive for Mobile"
	-@cd .. && /bin/rm -rf $(MOBILE_RELEASE_BASE)


clean-archive:
	-@cd .. && /bin/rm -f $(MOBILE_RELEASES)


info-erlang-for-c:
	@echo "ERL_BASE = $(ERL_BASE)"
	@echo "ERL_INTERFACE = $(ERL_INTERFACE)"


info-paths:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-compile: info-erlang-compile info-c-compile


info-erlang-compile:
	@echo "ERLANG_COMPILER_BASE_OPT = $(ERLANG_COMPILER_BASE_OPT)"
	@echo "BEAM_DIRS = $(BEAM_DIRS)"
	@echo "INC = $(INC)"
	@echo "ERLANG_COMPILER_EXEC_TARGET_OPT = $(ERLANG_COMPILER_EXEC_TARGET_OPT)"
	@echo "ERLANG_COMPILER_DEBUG_OPT = $(ERLANG_COMPILER_DEBUG_OPT)"
	@echo "ERLANG_COMPILER_NATIVE_COMPILATION_OPT = $(ERLANG_COMPILER_NATIVE_COMPILATION_OPT)"
	@echo "ERLANG_COMPILER_WARNING_OPT = $(ERLANG_COMPILER_WARNING_OPT)"
	@echo "ERLANG_COMPILER_OPT_BASE = $(ERLANG_COMPILER_OPT_BASE)"
	@echo "OVERALL_PZ_OPT = $(OVERALL_PZ_OPT)"
	@echo "ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES = $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES)"


info-c-compile:
	@echo "C_COMPILER = $(C_COMPILER)"
	@echo "C_LINKER = $(C_LINKER)"
	@echo "C_INC = $(C_INC)"
	@echo "C_LIB = $(C_LIB)"
	@echo "C_COMPILER_OPT = $(C_COMPILER_OPT)"
	@echo "C_LINKER_OPT = $(C_LINKER_OPT)"
	@echo "ERL_COMPILER  = $(ERL_COMPILER)"


info-parse-transform:
	@echo "BOOTSTRAP_MODULES = $(BOOTSTRAP_MODULES)"
	@echo "ERLANG_COMPILER_OPT_FOR_PT = $(ERLANG_COMPILER_OPT_FOR_PT)"
	@echo "META_BEAM_FILES = $(META_BEAM_FILES)"
	@echo "ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT)"


include $(MOBILE_TOP)/GNUmakesettings.inc
