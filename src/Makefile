## Configuration

MF= Makefile_f90

FC= gfortran
FCFLAGS= -O0

EXE= tsunami

SRC = tsunami.f90
MOD_SRC = \
	mod_diff.f90 \
	mod_initial.f90

##
## Build Automation
##

.PHONY: all clean
.SUFFIXES: .f90 .o

# make all is a synonym for building the final executable
all: $(EXE)

OBJ_FILES =	$(MOD_SRC:.f90=.o)
MOD_FILES =	$(MOD_SRC:.f90=.mod)

# Link all object files to create final executable
$(EXE): $(SRC) $(OBJ_FILES)
	$(FC) $(FCFLAGS) $^ -o $@

# Compile all files to object code
.f90.o:
	$(FC) $(FCFLAGS) -c $<

%.o: %.mod

# Remove all compilation results
clean:
	rm -f $(OBJ_FILES) $(EXE) $(MOD_FILES) core
