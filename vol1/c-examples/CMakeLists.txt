cmake_minimum_required(VERSION 2.8)

project(aifh_vol1 C)

# Some code uses OpenMP
find_package(OpenMP REQUIRED)
if (OPENMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()

include_directories(.)

add_executable(aifh_vol1 AIFH-VOL1.c
    Data.c
    Distance.c
    Equilateral.c
    ExampleAnalyze.c
    ExampleDistance.c
    ExampleKMeans.c
    ExampleNormalize.c
    ExamplePI.c
    ExampleRandom.c
    ExampleReadCSV.c
    ExampleSimpleNormalize.c
    ExampleTest.c
    ExampleUtil.c
    KMeans.c
    Normalize.c
    Random.c
    libcsv.c
    mt19937ar.c)

# Needs maths
target_link_libraries(aifh_vol1 m)
