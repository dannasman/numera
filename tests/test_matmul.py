import gdb
from os import listdir
from os.path import isfile, join

def matmul():
    a = [i for i in range(16) for _ in range(16)]
    b = [j for _ in range(16) for j in range(16)]
    c = [0]*256

    for i in range(16):
        for j in range(16):
            for k in range(16):
                c[i*16+j] += a[i*16+k]*b[k*16+j]

    return c

def run():
    file = "test_programs/bin/matmul"
    gdb.execute(f"file {file}", to_string=True)
    gdb.execute("b L2", to_string=True)
    
    gdb.execute("r", to_string=True)

    rbp_info = gdb.execute("info register rbp", to_string = True).split()
    
    assert(len(rbp_info) > 0)

    rbp_address = rbp_info[1]
    examine_memory = gdb.execute(f"x/512x ({rbp_address} - 6144)", to_string = True).split()
    n = int(len(examine_memory) / 5)
    arr = [int(elem, 0) for i in range(n) for elem in (examine_memory[i*5+1], examine_memory[i*5+3])]
    
    correct = matmul()
    for c, a in zip(correct, arr):
        assert(c == a, "[test_matmul] FAIL")
    
    print("[test_matmul] OK")
    
    gdb.execute("c", to_string=True)

    gdb.execute("d break", to_string=True)
