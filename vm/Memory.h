// Memory.h
#ifndef MEMORY_H // include guard
#define MEMORY_H
#include <string>

class Memory {
public:
    int* integers;
    float* floats;
    std::string* strings;
    char* chars;
    bool* booleans;

    Memory(int i, int f, int s, int c, int b) {
        integers = new int[i];
        floats = new float[f];
        strings = new std::string[s];
        chars = new char[c];
        booleans = new bool[b];
    }

    Memory(){}
};

#endif /* MEMORY_H */