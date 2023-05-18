MOBILE_TOP = .


.PHONY: help help-intro help-mobile all check-gammu                      \
		register-version-in-header register-mobile list-beam-dirs        \
		add-prerequisite-plts link-plt                                   \
		send-release release release-zip release-bz2 release-xz          \
		prepare-release stats                                            \
		clean-release clean-archive                                      \
		info-paths-loca linfo-versions


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
	@if pkg-config gammu; then echo "  Checking that Gammu is available: found."; else echo "Error, the Gammu dependency does not seem available. Please install this package beforehand (ex: 'pacman -Sy gammu' on Arch Linux)." 1>&2; exit 12; fi


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ]; then \
	echo "Error, no version file defined." 1>&2; exit 51; else \
	$(MAKE) register-mobile; fi


register-mobile:
	@echo "-define( mobile_version, \"$(MOBILE_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(MOBILE_BEAM_DIRS); do echo $$(readlink -f $$d); done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'mobile' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(MOBILE_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(MOBILE_PLT_FILE); fi


# Note: the source archives are not produced in this directory, but in its
# parent, so that everything related to Mobile (including these rules) remains
# self-contained.

send-release:

release: release-zip release-bz2 release-xz
	@$(MAKE) clean-release


release-zip: prepare-release
	@echo "     Creating Mobile release archive $(MOBILE_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(MOBILE_RELEASE_ARCHIVE_ZIP) $(MOBILE_RELEASE_BASE) \
	&& echo "     Archive $(MOBILE_RELEASE_ARCHIVE_ZIP) ready in $$(pwd)"


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


stats:
	@$(MAKE_CODE_STATS) $(MYRIAD_TOP)


clean: clean-release clean-archive


clean-release:
	@echo "   Cleaning release archive for Mobile"
	-@cd .. && /bin/rm -rf $(MOBILE_RELEASE_BASE)


clean-archive:
	-@cd .. && /bin/rm -f $(MOBILE_RELEASES)


info-paths: info-paths-local

info-paths-local:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-compile: info-compile-seaplus


rebar3-compile-pre-hook: check-gammu info-context

# Typically useful to know the software context for continuous integration:
info-context: info-platform info-versions info-source-layout

rebar3-compile-post-hook: info-source-layout


info-versions:
	@echo "MYRIAD_VERSION = $(MYRIAD_VERSION)"
	@echo "SEAPLUS_VERSION = $(SEAPLUS_VERSION)"


include $(MOBILE_TOP)/GNUmakesettings.inc
