MOBILE_TOP = ..


.PHONY: mobile-test mobile-test-debug mobile-test-terse                   \
		mobile-class-test mobile-class-test-debug mobile-class-test-terse \
		mobile-read-test mobile-read-test-debug mobile-read-test-terse    \
		clean clean-local clean-logs                                      \
		info-vars



all:


# No more need for a specific target (now just run 'make test' as usual):
#mobile-all-tests: mobile-test mobile-class-test


# If using a dummy device, its directory must already exist before using it:
# (note that it will be integrally wiped out by the clean-local target)
#
GAMMU_DUMMY_DIR := /tmp/gammu-dummy-device

pre-test: all clean-logs
	@echo "    Creating test Gammu dummy directory $(GAMMU_DUMMY_DIR)"
	@/bin/mkdir -p $(GAMMU_DUMMY_DIR)


# Shell environment needed for the testing:

# (now useless, as directly managed from the test cases themselves, as it is
# clearer/more reliable; see mobile_test:set_up_mobile_environment/0)
#
#TEST_ENV = PATH=$(MOBILE_TOP)/src:$(PATH) LD_LIBRARY_PATH=$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH)


# Targets only defined for convenience (the usual 'make test' is now
# sufficient):


# Displays the logs iff failed:
mobile-test: pre-test
	@$(MAKE) -s mobile_run $(TEST_ENV) || cat seaplus-driver.*.log


# Displays the logs in all cases:
mobile-test-debug: pre-test
	@$(MAKE) -s mobile_run $(TEST_ENV) ; cat seaplus-driver.*.log


# Displays no log:
mobile-test-terse: pre-test
	@$(MAKE) -s mobile_run $(TEST_ENV)



# Displays the logs iff failed:
mobile-class-test: pre-test
	@$(MAKE) -s mobile_class_run $(TEST_ENV) || cat seaplus-driver.*.log


# Displays the logs in all cases:
mobile-class-test-debug: pre-test
	@$(MAKE) -s mobile_class_run $(TEST_ENV) ; cat seaplus-driver.*.log


# Displays no log:
mobile-class-test-terse: pre-test
	@$(MAKE) -s mobile_class_run $(TEST_ENV)




# Displays the logs iff failed:
mobile-read-test: pre-test
	@$(MAKE) -s mobile_read_run $(TEST_ENV) || cat seaplus-driver.*.log


# Displays the logs in all cases:
mobile-read-test-debug: pre-test
	@$(MAKE) -s mobile_read_run $(TEST_ENV) ; cat seaplus-driver.*.log


# Displays no log:
mobile-read-test-terse: pre-test
	@$(MAKE) -s mobile_read_run $(TEST_ENV)



test: pre-test #test-local


# We cannot execute directly the 'test' target, as we need a proper environment:
test-local:
	@echo "Error, run: 'make mobile-all-tests' instead." 1>&2
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
