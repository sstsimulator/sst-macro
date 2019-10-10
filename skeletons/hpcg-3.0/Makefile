# -*- Makefile -*-

arch = SST
setup_file = setup/Make.$(arch)

include $(setup_file)

HPCG_DEPS = build/CG.o \
	    build/CG_ref.o \
	    build/TestCG.o \
	    build/ComputeResidual.o \
	    build/ExchangeHalo.o \
	    build/GenerateGeometry.o \
	    build/GenerateProblem.o \
	    build/GenerateProblem_ref.o \
	    build/CheckProblem.o \
	    build/MixedBaseCounter.o \
	    build/OptimizeProblem.o \
	    build/ReadHpcgDat.o \
	    build/ReportResults.o \
	    build/SetupHalo.o \
	    build/SetupHalo_ref.o \
	    build/TestSymmetry.o \
	    build/TestNorms.o \
	    build/WriteProblem.o \
	    build/YAML_Doc.o \
	    build/YAML_Element.o \
	    build/ComputeDotProduct.o \
	    build/ComputeDotProduct_ref.o \
	    build/mytimer.o \
	    build/ComputeOptimalShapeXYZ.o \
	    build/ComputeSPMV.o \
	    build/ComputeSPMV_ref.o \
	    build/ComputeSYMGS.o \
	    build/ComputeSYMGS_ref.o \
	    build/ComputeWAXPBY.o \
	    build/ComputeWAXPBY_ref.o \
	    build/ComputeMG_ref.o \
	    build/ComputeMG.o \
	    build/ComputeProlongation_ref.o \
	    build/ComputeRestriction_ref.o \
	    build/CheckAspectRatio.o \
	    build/GenerateCoarseProblem.o \
	    build/init.o \
	    build/finalize.o

# These header files are included in many source files, so we recompile every file if one or more of these header is modified.
PRIMARY_HEADERS = src/Geometry.hpp src/SparseMatrix.hpp src/Vector.hpp src/CGData.hpp \
                  src/MGData.hpp src/hpcg.hpp 

ALL_HEADERS = $(shell find src -name '*.hpp')

all: runhpcg-3.0

runhpcg-3.0: build/main.o $(HPCG_DEPS)
	$(LINKER) $(LINKFLAGS) build/main.o $(HPCG_DEPS) $(HPCG_LIBS) -o $@

libhpcg.so: build/main.o $(HPCG_DEPS)
	@echo "Linking SST core .so"
	$(LINKER) $(LINKFLAGS) build/main.o $(HPCG_DEPS) $(HPCG_LIBS) -o $@

clean:
	rm -f build/*.o hpcg-3.0 libhpcg.so

.PHONY: all clean

build/main.o: src/main.cpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/CG.o: src/CG.cpp src/CG.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/CG_ref.o: src/CG_ref.cpp src/CG_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/TestCG.o: src/TestCG.cpp src/TestCG.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeResidual.o: src/ComputeResidual.cpp src/ComputeResidual.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ExchangeHalo.o: src/ExchangeHalo.cpp src/ExchangeHalo.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/GenerateGeometry.o: src/GenerateGeometry.cpp src/GenerateGeometry.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/GenerateProblem.o: src/GenerateProblem.cpp src/GenerateProblem.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/GenerateProblem_ref.o: src/GenerateProblem_ref.cpp src/GenerateProblem_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/CheckProblem.o: src/CheckProblem.cpp src/CheckProblem.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/MixedBaseCounter.o: src/MixedBaseCounter.cpp src/MixedBaseCounter.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/OptimizeProblem.o: src/OptimizeProblem.cpp src/OptimizeProblem.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ReadHpcgDat.o: src/ReadHpcgDat.cpp src/ReadHpcgDat.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ReportResults.o: src/ReportResults.cpp src/ReportResults.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/SetupHalo.o: src/SetupHalo.cpp src/SetupHalo.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/SetupHalo_ref.o: src/SetupHalo_ref.cpp src/SetupHalo_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/TestSymmetry.o: src/TestSymmetry.cpp src/TestSymmetry.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/TestNorms.o: src/TestNorms.cpp src/TestNorms.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/WriteProblem.o: src/WriteProblem.cpp src/WriteProblem.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/YAML_Doc.o: src/YAML_Doc.cpp src/YAML_Doc.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/YAML_Element.o: src/YAML_Element.cpp src/YAML_Element.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeDotProduct.o: src/ComputeDotProduct.cpp src/ComputeDotProduct.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeDotProduct_ref.o: src/ComputeDotProduct_ref.cpp src/ComputeDotProduct_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/finalize.o: src/finalize.cpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/init.o: src/init.cpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/mytimer.o: src/mytimer.cpp src/mytimer.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeOptimalShapeXYZ.o: src/ComputeOptimalShapeXYZ.cpp src/ComputeOptimalShapeXYZ.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeSPMV.o: src/ComputeSPMV.cpp src/ComputeSPMV.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeSPMV_ref.o: src/ComputeSPMV_ref.cpp src/ComputeSPMV_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeSYMGS.o: src/ComputeSYMGS.cpp src/ComputeSYMGS.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeSYMGS_ref.o: src/ComputeSYMGS_ref.cpp src/ComputeSYMGS_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeWAXPBY.o: src/ComputeWAXPBY.cpp src/ComputeWAXPBY.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeWAXPBY_ref.o: src/ComputeWAXPBY_ref.cpp src/ComputeWAXPBY_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeMG_ref.o: src/ComputeMG_ref.cpp src/ComputeMG_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeMG.o: src/ComputeMG.cpp src/ComputeMG.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeProlongation_ref.o: src/ComputeProlongation_ref.cpp src/ComputeProlongation_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/ComputeRestriction_ref.o: src/ComputeRestriction_ref.cpp src/ComputeRestriction_ref.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/GenerateCoarseProblem.o: src/GenerateCoarseProblem.cpp src/GenerateCoarseProblem.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

build/CheckAspectRatio.o: src/CheckAspectRatio.cpp src/CheckAspectRatio.hpp $(PRIMARY_HEADERS)
	$(CXX) -c $(CXXFLAGS) -Isrc $< -o $@

