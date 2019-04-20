MOBILE_TOP = ..


.PHONY: mobile-test mobile-test-debug mobile-test-terse                   \
		clean clean-local clean-logs                                      \
		info-vars


# If using a dummy device, its directory must already exist before using it:
# (note that it will be integrally wiped out by the clean-local target)
#
GAMMU_DUMMY_DIR := /tmp/gammu-dummy-device

pre-test: all clean-logs
	@/bin/mkdir -p $(GAMMU_DUMMY_DIR)


# Shell environment needed for the testing:
TEST_ENV = PATH=$(MOBILE_TOP)/src:$(PATH) LD_LIBRARY_PATH=$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH)


# Displays the logs iff failed:
mobile-test: pre-test
	@$(MAKE) -s mobile_run $(TEST_ENV) || cat seaplus-driver.*.log


# Displays the logs in all cases:
mobile-test-debug: pre-test
	@$(MAKE) -s mobile_run $(TEST_ENV) ; cat seaplus-driver.*.log


# Displays no log:
mobile-test-terse: pre-test
	@$(MAKE) -s mobile_run $(TEST_ENV)


test: test-local


# We cannot execute directly the 'test' target, as we need a proper environment:
test-local:
	@echo "Error, run: 'make mobile-test' instead." 1>&2
	@exit 5



clean: clean-local


clean-local: clean-logs
	-@/bin/rm -f *.o
	-@/bin/rm -rf $(GAMMU_DUMMY_DIR)


clean-logs:
	-@/bin/rm -f *.o seaplus-driver.*.log


info-vars:
	@echo "TEST_ENV = $(TEST_ENV)"


include $(MOBILE_TOP)/GNUmakesettings.inc
