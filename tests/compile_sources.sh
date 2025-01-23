TEST_SOURCES=($(ls test_sources/))

for src in ${TEST_SOURCES[@]}
do
    PROGRAM=${src/.num}
    echo "../target/release/numera test_sources/${src} > test_programs/${PROGRAM}.s"
    ../target/release/numera test_sources/${src} > test_programs/${PROGRAM}.s
    
    echo "gcc test_programs/${PROGRAM}.s -o test_programs/bin/${PROGRAM}"
    gcc test_programs/${PROGRAM}.s -o test_programs/bin/${PROGRAM}
done
