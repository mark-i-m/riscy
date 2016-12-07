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

from sys import argv
import re
import os

# will contain final computed register state
regs = {i : None for i in range(32)}

# will contain memory writes in order
stores = []

# will contain the correct final states
correct_regs = {i : None for i in range(32)}
correct_stores = []

if len(argv) < 2:
    print(USAGE)
    os.exit(1)

# the log file name
logname = argv[0]

# the ok file name
okname = argv[1]

# regex for parsing lines of the log
REG_RE = r'''^RF[\s*(\d+)] = (\d+)$'''
MEM_RE = r'''^MEM[\s*(\d+)] = (\d+)$''' # TODO also print the size of the store

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
REG_OK_RE = r'''^RF[\s*(\d+)] = (0x)?(\d+)$'''
MEM_OK_RE = r'''^MEM[\s*(0x)?(\d+)] = (0x)?(\d+)$''' # TODO also print the size of the store

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
            adr = int(reg_m.group(2), 10 if reg_m.group(1) is None else 16)
            val = int(reg_m.group(4), 10 if reg_m.group(3) is None else 16)

            correct_stores.append((adr, val))

# compare correct answer with actual state
failed = False

# compare regs
for i in range(32):
    if regs[i] != correct_regs[i]:
        failed = True
        print("REG[%d]\n\tGOT %d\n\tEXPECTED %d" % (i, regs[i], correct_regs[i]))

# compare memory address
if stores != correct_stores:
    failed = True

    for i in range(min(len(correct_stores), len(stores))):
        if correct_stores[i] != stores[i]:
            print("STORE#%d\n\tGOT %d\n\tEXPECTED %d" % (i, stores[i], correct_stores[i]))

    print("\nENTIRE STORE SEQUENCE:\n\tGOT %s\n\tEXPECTED %s" % (stores, correct_stores))
    os.exit(1)

print("PASS :D")
