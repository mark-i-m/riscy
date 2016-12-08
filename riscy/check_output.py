#!/usr/bin/env python2

USAGE = """
Usage:
    ./check_output.py <.log> <.ok>

<.log> is the file name of a .log file produced from running `make bench`.

<.ok> is the correct output. See the .ok files in the `riscy/bench` directory
        for examples of format.

The script will compare the final register state to what the .ok file dictates.
It will also compare the order, addresses, values, and sizes of all stores.

The script will exit with a code 0 if pass.
"""

from sys import argv, exit
import re

# will contain final computed register state
regs = {i : None for i in range(32)}

# will contain memory writes in order
stores = []

# will contain the correct final states
correct_regs = {i : None for i in range(32)}
correct_stores = []

if len(argv) < 3:
    print(USAGE)
    exit(1)

# the log file name
logname = argv[1]

# the ok file name
okname = argv[2]

# regex for parsing lines of the log
REG_RE = r'''RF\[\s*([\da-fA-F]+)\] = ([\da-fA-F]+)'''
MEM_RE = r'''MEM\[\s*([\da-fA-F]+)\] = ([\da-fA-F]+)''' # TODO also print the size of the store

# read the log and compute final states
with open(logname, 'r') as log:
    for line in log:
        reg_m = re.match(REG_RE, line)
        mem_m = re.match(MEM_RE, line)

        if reg_m is not None:
            reg = int(reg_m.group(1))
            val = int(reg_m.group(2), 16)

            regs[reg] = val

        elif mem_m is not None:
            adr = int(reg_m.group(1), 16)
            val = int(reg_m.group(2), 16)

            stores.append((adr, val))

# regex for parsing lines of ok file
REG_OK_RE = r'''RF\[\s*(\d+)\] = (0x)?([\da-fA-F]+)'''
MEM_OK_RE = r'''MEM\[\s*(0x)?([\da-fA-F]+)\] = (0x)?([\da-fA-F]+)''' # TODO also print the size of the store

# read the ok file and parse correct state
with open(okname, 'r') as ok:
    for line in ok:
        reg_m = re.match(REG_OK_RE, line)
        mem_m = re.match(MEM_OK_RE, line)

        if reg_m is not None:
            reg = int(reg_m.group(1))
            val = int(reg_m.group(3), 10 if reg_m.group(2) is None else 16)

            correct_regs[reg] = val

        elif mem_m is not None:
            adr = int(mem_m.group(2), 10 if mem_m.group(1) is None else 16)
            val = int(mem_m.group(4), 10 if mem_m.group(3) is None else 16)

            correct_stores.append((adr, val))

        elif len(line.strip()) > 0:
            print("ERROR parsing .ok file")
            print("At line:\n\t%s" % line)
            exit(1)

# simple helpers to print stuff in hex
def store_hex(s):
    return (hex(s[0]), hex(s[1]))

def stores_hex(l):
    return [store_hex(s) for s in l]

# compare correct answer with actual state
failed = False

# compare regs
for i in range(32):
    if regs[i] != correct_regs[i]:
        failed = True
        print("REG[%d]\n\tGOT %s\n\tEXPECTED %s" % 
                (i, hex(regs[i]) if regs[i] is not None else "None",
                    hex(correct_regs[i]) if correct_regs[i] is not None else "None"))

# compare memory address
if stores != correct_stores:
    failed = True

    for i in range(min(len(correct_stores), len(stores))):
        if correct_stores[i] != stores[i]:
            print("STORE#%d\n\tGOT %s\n\tEXPECTED %s" % \
                    (i, store_hex(stores[i]), store_hex(correct_stores[i])))

    print("\nENTIRE STORE SEQUENCE:\n\tGOT %s\n\tEXPECTED %s" % \
            (stores_hex(stores), stores_hex(correct_stores)))

if failed:
    print("FAILED :(")
    exit(1)

print("PASS :D")
