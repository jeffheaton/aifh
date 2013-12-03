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
