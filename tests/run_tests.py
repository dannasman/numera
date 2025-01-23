from os import listdir
from os.path import isfile, join

import test_fib
import test_matmul

if __name__ == "__main__":
    test_fib.run()
    test_matmul.run()
    
    gdb.execute("q", to_string=True)
