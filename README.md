# numera
Compiler project inspired by the dragon book. Contains only the compiler frontend at the moment.
## Example
Example code snippet `foo.txt` in root directory:
```
{
    x = 1;
    while(true) {
        if (x == 5) break;
        x = x + 1;
    }
}
```
Compile the code with
```
cargo run foo.txt
```
Output:
```
{
    x = 1;
    while(true) {
        if (x == 5) break;
        x = x + 1;
    }
}

----------compiling----------
L1:
	x = 1
L3:
L4:
	iffalse x == 5 goto L5
L6:
	goto L2
L5:
	x = x + 1
	goto L3
L2:

```
