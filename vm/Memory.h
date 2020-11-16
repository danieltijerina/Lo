// Memory.h
#ifndef MEMORY_H // include guard
#define MEMORY_H
#include <string>
#include "FunctionDef.h"

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

class FunctionMemory {
  public:
    std::string fname_;
    Memory variables_;
    Memory temporals_;

    FunctionMemory(FunctionDef& func_def, std::string name) {
      fname_ = name;
      
      variables_.integers = new int[func_def.intTy];
      variables_.floats = new float[func_def.floatTy];
      variables_.strings = new std::string[func_def.stringTy];
      variables_.chars = new char[func_def.charTy];
      variables_.booleans = new bool[func_def.boolTy];

      temporals_.integers = new int[func_def.intTmp];
      temporals_.floats = new float[func_def.floatTmp];
      temporals_.strings = new std::string[func_def.stringTmp];
      temporals_.chars = new char[func_def.charTmp];
      temporals_.booleans = new bool[func_def.boolTmp];
    }
};

#endif /* MEMORY_H */