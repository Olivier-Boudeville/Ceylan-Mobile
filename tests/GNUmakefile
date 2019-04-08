MOBILE_TOP = ..


.PHONY: mobile-test mobile-test-debug mobile-test-terse clean clean-local

# If using a dummy device, its directory must already exist before using it:
# (note that it will be integrally wiped out by the clean-local target)
#
GAMMU_DUMMY_DIR := /tmp/gammu-dummy-device

pre-test: all
	@/bin/mkdir $(GAMMU_DUMMY_DIR)


# Displays the logs iff failed:
mobile-test: pre-test
	@$(MAKE) -s mobile_run LD_LIBRARY_PATH="$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH)"|| cat seaplus-driver.*.log


# Displays the logs in all cases:
mobile-test-debug: pre-test
	@$(MAKE) -s mobile_run PATH=$(MOBILE_TOP)/src:$(PATH) LD_LIBRARY_PATH="$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH)" ; cat seaplus-driver.*.log


# Displays no log:
mobile-test-terse: pre-test
	@$(MAKE) -s mobile_run LD_LIBRARY_PATH="$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH)"


test: test-local


# We cannot execute directly the 'test' target, as we need a proper environment:
test-local:
	@echo "Error, run: 'make mobile-test' instead." 1>&2
	@exit 5



clean: clean-local

clean-local:
	-@/bin/rm -f *.o seaplus-driver.*.log
	-@/bin/rm -rf $(GAMMU_DUMMY_DIR)




include $(MOBILE_TOP)/GNUmakesettings.inc
