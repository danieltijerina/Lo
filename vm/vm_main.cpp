#include <iostream>
#include <fstream>
#include "Memory.h"

Memory& readQuads(std::string filename) {
    std::ifstream clo(filename);
    std::string e1, e2, e3, e4;
    clo >> e1 >> e2 >> e3 >> e4;
    while(e1 != "$$$") {
        // std::cout << e1 << " " << e2 << " "
        //           << " " << e3 << " " << e4 << "\n";
        clo >> e1 >> e2 >> e3 >> e4;
    }
    int i=0, f=0, s=0, b=0, c=0;
    while(clo >> e1 >> e2) {
        if(e1 == "IntTy" || e1 == "IntCte" || e1 == "IntTmp") {
            i+=std::stoi(e2);
        }
        if(e1 == "FloatTy" || e1 == "FloatCte" || e1 == "FloatTmp") {
            f+=std::stoi(e2);
        }
        if(e1 == "CharTy" || e1 == "CharCte" || e1 == "CharTmp") {
            c+=std::stoi(e2);
        }
        if(e1 == "StringTy" || e1 == "StringCte" || e1 == "StringTmp") {
            s+=std::stoi(e2);
        }
        if(e1 == "BoolTy" || e1 == "BoolCte" || e1 == "BoolTmp") {
            b+=std::stoi(e2);
        }
    }
}

int main(int argc, char** argv) {
    if(argc != 2) {
        std::cout << "Must only include filename.\n";
        return -1;
    }
    readQuads(argv[1]);
    // Memory current_mem(10, 5, 2, 4);
    // current_mem.strings[0] = "hola";
    // current_mem.strings[1] = "soy goku";

    // for(int i=0; i<10; i++)
    //     std::cout << current_mem.integers[i] << " ";
    // std::cout << "\n";

    return 0;
}