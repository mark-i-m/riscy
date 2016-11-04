#include "emulator.h"
#include "Riscy.h"

#include <iostream>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

/*
 * Run the given benchmark, producing a vcd and trace file.
 *
 * Args:
 * 1) Name of the benchmark file that contains the memory contents
 * 2) Name of the log file to output to
 * 3) Name of the vcd file to output to
 *
 * Based off of:
 *  https://github.com/ucb-bar/riscv-sodor/blob/master/emulator/rv32_1stage/emulator.cpp
 */
int main(int argc, char** argv) {

    // Output files
    const char* loadmem = NULL;
    const char* log = NULL;
    const char* vcd = NULL;
    FILE *vcdfile = NULL;
    FILE *logfile = NULL;

    // Some constants
    const unsigned random_seed = (unsigned)time(NULL) ^ (unsigned)getpid();
    const int memory_size = (1 << 30); // 1GB
    const int disasm_len = 24;

    // Check args
    if(argc < 4) {
        std::cerr << "Usage: ./riscy_emu <bmk> <log> <vcd>" << std::endl;
        exit(-1);
    }

    loadmem = argv[1];
    log = argv[2];
    vcd = argv[3];

    // TODO: open other files

    // Open vcd file
    vcdfile = fopen(vcd, "w");
    assert(vcdfile);
    fprintf(vcdfile, "$scope module Testbench $end\n");
    fprintf(vcdfile, "$var reg %d NDISASM instruction $end\n", disasm_len*8);
    fprintf(vcdfile, "$var reg 64 NCYCLE cycle $end\n");
    fprintf(vcdfile, "$upscope $end\n");

    // The Riscy generated code
    Riscy_t riscy;

    // Set random seed
    srand(random_seed);
    riscy.init(random_seed != 0);

    // TODO: What about Tracer_t in example?
    //  https://github.com/ucb-bar/riscv-sodor/blob/master/emulator/common/tracer.h/cpp

    // Load memory from file
    std::ifstream in(loadmem);
    if (!in) {
        std::cerr << "could not open " << loadmem << std::endl;
        exit(-1);
    }

    // Read one line at a time. Each line should have 32 B (4 instructions)
    std::string line;
    uint64_t mem_idx = 0; // which 4B word are we at?
    while (std::getline(in, line)) {
        // 4 words per line
        assert (line.length()/2/4 == 4);

        uint32_t m[4] = {0,0,0,0}; 

        #define parse_nibble(c) ((c) >= 'a' ? (c)-'a'+10 : (c)-'0')
        for (ssize_t i = line.length()-2, j = 0; i >= 0; i -= 2, j++) {
            uint8_t byte = (parse_nibble(line[i]) << 4) | parse_nibble(line[i+1]); 
            m[j>>2] = (byte << ((j%4)*8)) | m[j>>2];
        }

        // Copy words into the Riscy memory.
        // Make sure we don't copy too much memory
        if (mem_idx < (memory_size/4)) {
            riscy.Riscy_memory__memBank.put(mem_idx  , LIT<32>(m[0]));
            riscy.Riscy_memory__memBank.put(mem_idx+1, LIT<32>(m[1]));
            riscy.Riscy_memory__memBank.put(mem_idx+2, LIT<32>(m[2]));
            riscy.Riscy_memory__memBank.put(mem_idx+3, LIT<32>(m[3]));
        }
        mem_idx += 4;
    }

    std::cerr << "Loaded memory" << std::endl;

    // TODO: start the simulation
    // Might need the "HTIF" stuff here:
    //   https://github.com/ucb-bar/riscv-sodor/blob/master/emulator/common/htif_emulator.h

    // Clean up
    fclose(vcdfile);

    // TODO: close other files

    return 0;
}
