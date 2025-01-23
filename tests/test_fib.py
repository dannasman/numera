import gdb
from os import listdir
from os.path import isfile, join

def fib(n):
    if n == 0: return 1
    if n == 1: return 1
    return fib(n-1)+fib(n-2)

def run():
    file = "test_programs/bin/fib"
    gdb.execute(f"file {file}", to_string=True)
    gdb.execute("b L8", to_string=True)
    
    gdb.execute("r", to_string=True)

    rbp_info = gdb.execute("info register rbp", to_string = True).split()
    
    assert(len(rbp_info) > 0)

    rbp_address = rbp_info[1]
    examine_memory = gdb.execute(f"x/20x ({rbp_address} - 80)", to_string = True).split()

    n = int(len(examine_memory) / 5)
    arr = [int(elem, 0) for i in range(n) for elem in (examine_memory[i*5+1], examine_memory[i*5+3])]

    for i, n in enumerate(arr):
        assert(fib(i) == n, "[test_fib] FAILED")

    print("[test_fib] OK")
    
    gdb.execute("c", to_string=True)

    gdb.execute("d break", to_string=True)
