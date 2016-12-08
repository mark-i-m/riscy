#include "emulator.h"
#include "Riscy.h"

#include <iostream>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#define parse_nibble(c) ((c) >= 'a' ? (c)-'a'+10 : (c) >= 'A' ? (c)-'A'+10 : (c)-'0')
#define min(a,b) ((a) < (b) ? (a) : (b))

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
    const uint64_t max_cycles = 1 << 14; // 16K cycles

    // Counter for emulation
    uint64_t trace_count = 0;

    // Check args
    if(argc < 4) {
        std::cerr << "Usage: ./riscy_emu <bmk> <log> <vcd>" << std::endl;
        exit(-1);
    }

    loadmem = argv[1];
    log = argv[2];
    vcd = argv[3];

    // Open log file
    logfile = fopen(log, "w");
    assert(logfile);

    // Open vcd file
    vcdfile = fopen(vcd, "w");
    assert(vcdfile);

    // The Riscy generated code
    Riscy_t* riscy = new Riscy_t;

    // Set random seed
    srand(random_seed);
    riscy->init();

    // Load memory from file
    std::ifstream in(loadmem);
    if (!in) {
        std::cerr << "could not open " << loadmem << std::endl;
        exit(-1);
    }

    // Read one line at a time. Each line should have 8B (1 word); anything
    // past the 8th byte is ignored.
    uint64_t mem_idx = 0; // which word are we currently loading?
    std::string line;
    while (std::getline(in, line)) {
        assert(line.length() >= 16);

        // 8B per word
        uint8_t m[8] = {0,0,0,0,0,0,0,0}; 

        for(ssize_t i = 0; i < 16; i += 2) {
            uint8_t mask = 0xF;
            // beginning of string => high-order bits
            uint8_t nibble1 = parse_nibble(line[i]) & mask;
            uint8_t nibble2 = parse_nibble(line[i+1]) & mask;
            uint8_t byte = (nibble1 << 4) | nibble2;

            m[8 - (i/2) -1] = byte;
        }

        // Copy words into the Riscy memory.
        // Make sure we don't copy too much memory
        if (mem_idx < (memory_size/8)) {
            riscy->Riscy_memory__memBank.put(mem_idx  , LIT<8>(m[0]));
            riscy->Riscy_memory__memBank.put(mem_idx+1, LIT<8>(m[1]));
            riscy->Riscy_memory__memBank.put(mem_idx+2, LIT<8>(m[2]));
            riscy->Riscy_memory__memBank.put(mem_idx+3, LIT<8>(m[3]));
            riscy->Riscy_memory__memBank.put(mem_idx+4, LIT<8>(m[4]));
            riscy->Riscy_memory__memBank.put(mem_idx+5, LIT<8>(m[5]));
            riscy->Riscy_memory__memBank.put(mem_idx+6, LIT<8>(m[6]));
            riscy->Riscy_memory__memBank.put(mem_idx+7, LIT<8>(m[7]));
        }
        mem_idx += 8;
    }

    in.close();

    std::cerr << "Loaded memory" << std::endl;
    
    std::cerr << "Starting emulation" << std::endl;

    // Run the emulation
    while(trace_count < max_cycles) {
        riscy->clock_lo(LIT<1>(trace_count == 0));

        riscy->print(logfile);
        riscy->dump(vcdfile, trace_count);

        riscy->clock_hi(LIT<1>(trace_count == 0));

        if(riscy->Riscy_rob__io_halt.to_bool()) {
            std::cerr << "Processor Halted." << std::endl;
            break;
        }

        trace_count++;
    }

    std::cerr << "Ran for " << trace_count << " cycles. Done." << std::endl;

    // Clean up
    fclose(vcdfile);
    fclose(logfile);
    
    delete riscy;

    return 0;
}
