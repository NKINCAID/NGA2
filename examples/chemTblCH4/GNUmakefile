# Compilation parameters
PRECISION = DOUBLE
USE_MPI   = TRUE
USE_HYPRE = FALSE
USE_LAPACK= FALSE
USE_FFTW  = FALSE
USE_P3DFFT= FALSE
USE_IRL   = FALSE
PROFILE   = FALSE
DEBUG     = FALSE
COMP      = gnu
EXEBASE   = createDiffusionChemTable

# NGA compilation definitions
include $(NGA_HOME)/tools/GNUMake/Make.defs

# Include NGA base code
Bdirs := libraries chemtable
Bpack += $(foreach dir, $(Bdirs), $(NGA_HOME)/src/$(dir)/Make.package)
include $(Bpack)

# Inform user of Make.packages used
ifdef Ulocs
   $(info Taking user code from: $(Ulocs))
endif
$(info Taking base code from: $(Bdirs))

# Target definition
all: $(executable)
	@echo COMPILATION SUCCESSFUL

# NGA compilation rules
include $(NGA_HOME)/tools/GNUMake/Make.rules