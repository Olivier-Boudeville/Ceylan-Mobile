MOBILE_TOP = ..


.PHONY: info info-compile clean clean-local

%.o: %.c
	@echo "   Compiling $<"
	@$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(INC) $(MOBILE_INC_OPT) -o $@ -c $<


TARGET_DRIVER := mobile_seaplus_driver

TARGET_DRIVER_OBJ := $(TARGET_DRIVER).o
TARGET_DRIVER_EXEC := $(TARGET_DRIVER)


# Here, unlike the C-test Foobar example of Seaplus, the integrated library,
# libGammu, is expected to be already available, so only its Seaplus driver has
# to be built:

all: mobile.beam mobile_test.beam $(TARGET_DRIVER_EXEC)


$(TARGET_DRIVER_EXEC): mobile_seaplus_driver.o $(SEAPLUS_LIB_PATH) $(GAMMU_LIB_PATH)
	@echo "   Linking driver $@"
	@$(C_LINKER) -o $(TARGET_DRIVER_EXEC) $(TARGET_DRIVER_OBJ) \
	 $(SEAPLUS_LIB_OPT) $(C_LINKER_OPT) -L$(GAMMU_LIB_DIR) -l$(GAMMU_BASE_NAME)


# Main interest: ensuring that the driver executable can be run at all.
run-driver: $(TARGET_DRIVER_EXEC) $(GAMMU_LIB_PATH)
	@echo "   Running Ceylan-Mobile driver, which will use $(GAMMU_LIB_PATH)"
	@LD_LIBRARY_PATH=$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH) ./$(TARGET_DRIVER_EXEC)


erlang-test: all
	@$(MAKE) -s mobile_run LD_LIBRARY_PATH=$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH) || cat seaplus-driver.*.log


clean: clean-local

clean-local:
	-@/bin/rm -f mobile_seaplus_api_mapping.h *.o $(TARGET_DRIVER_EXEC)


info: info-compile

info-compile:
	@echo "C_COMPILER = $(C_COMPILER)"
	@echo "C_COMPILER_OPT_FOR_OBJ = $(C_COMPILER_OPT_FOR_OBJ)"
	@echo "INC = $(INC)"
	@echo "MOBILE_INC_OPT = $(MOBILE_INC_OPT)"


include $(MOBILE_TOP)/GNUmakesettings.inc
