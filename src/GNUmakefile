MOBILE_TOP = ..


.PHONY: run-driver info info-compile info-libs info-exec clean clean-local


%.o: %.c
	@echo "   Compiling $<"
	@$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(INC) $(MOBILE_INC_OPT) -o $@ -c $<


TARGET_DRIVER := mobile_seaplus_driver

TARGET_DRIVER_OBJ := $(TARGET_DRIVER).o
TARGET_DRIVER_EXEC := $(TARGET_DRIVER)


# Here, unlike the C-test Foobar example of Seaplus, the integrated library,
# libGammu, is expected to be already available, so only its Seaplus driver has
# to be built:

all-recurse: mobile.beam $(TARGET_DRIVER_EXEC)


# Useful for more selective recompilations:

mobile_seaplus_driver.o: mobile_seaplus_api_mapping.h mobile_seaplus_driver.c

mobile_seaplus_api_mapping.h mobile_seaplus_driver.c: mobile.beam

mobile.beam: mobile.erl


$(TARGET_DRIVER_EXEC): mobile_seaplus_driver.o $(SEAPLUS_LIB_PATH) $(GAMMU_LIB_PATH)
	@echo "   Linking driver $@"
	@$(C_LINKER) -o $(TARGET_DRIVER_EXEC) $(TARGET_DRIVER_OBJ) \
	 $(SEAPLUS_LIB_OPT) $(C_LINKER_OPT) -L$(GAMMU_LIB_DIR) $(GAMMU_LIBS)


# Main interest: ensuring that the driver executable can be run at all.
run-driver: $(TARGET_DRIVER_EXEC) $(GAMMU_LIB_PATH)
	@echo "   Running Ceylan-Mobile driver, which will use $(GAMMU_LIB_PATH)"
	@LD_LIBRARY_PATH=$(GAMMU_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH) ./$(TARGET_DRIVER_EXEC)



clean: clean-local

clean-local:
	-@/bin/rm -f mobile_seaplus_api_mapping.h *.o $(TARGET_DRIVER_EXEC)


info: info-compile info-libs

info-compile:
	@echo "C_COMPILER = $(C_COMPILER)"
	@echo "C_COMPILER_OPT_FOR_OBJ = $(C_COMPILER_OPT_FOR_OBJ)"
	@echo "INC = $(INC)"
	@echo "MOBILE_INC_OPT = $(MOBILE_INC_OPT)"


info-libs:
	@echo "GAMMU_LIB_DIR = $(GAMMU_LIB_DIR)"
	@echo "SEAPLUS_LIB_DIR = $(SEAPLUS_LIB_DIR)"


info-exec:
	@echo "TARGET_DRIVER = $(TARGET_DRIVER)"


include $(MOBILE_TOP)/GNUmakesettings.inc
