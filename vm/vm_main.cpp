#include <iostream>
#include <fstream>
#include <vector>
#include "Memory.h"
#include "Reader.h"
#include "Quads.h"
#include "FunctionDef.h"
#include "Processor.h"

using namespace Quads;

int main(int argc, char** argv) {
    if(argc != 2) {
        std::cout << "Must only include filename.\n";
        return -1;
    }

    std::vector<Quad> quads_;
    std::unordered_map<string, FunctionDef> function_def_;
    Memory constant_memory;

    Reader::processFile(argv[1], &quads_, &function_def_, &constant_memory);
    Processor processor(&quads_, &function_def_, &constant_memory);
    processor.startProcessing();

    // Testing purposes only! 
    /*
    std::cout << quads_.size() << std::endl;
    for(int i = 0; i < quads_.size(); i++){
      std::cout << std::to_string(quads_[i].type_) << std::endl;
    }

    std::cout << function_def_.size() << std::endl;
    for(auto i : function_def_){
      std::cout << i.first << " " << i.second.intTy << std::endl;
    }

    for(int i = 0;i < 2; i++){
      std::cout << constant_memory.booleans[i] << std::endl;
    }
    */

    return 0;
}