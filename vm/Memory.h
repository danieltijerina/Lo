// Memory.h
#ifndef MEMORY_H // include guard
#define MEMORY_H
#include <string>
#include "FunctionDef.h"
#include "ClassMem.h"

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

class PointerMemory {
public:
  int *integers, *floats, *strings, *chars, *booleans;

  PointerMemory(int i, int f, int s, int c, int b) {
    integers = new int[i];
    floats = new int[f];
    strings = new int[s];
    chars = new int[c];
    booleans = new int[b];
  }

  PointerMemory() {}
};

class FunctionMemory {
  public:
    std::string fname_;
    Memory variables_;
    Memory temporals_;
    PointerMemory pointers_;
    ClassMemory* classes_;

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

      pointers_.integers = new int[func_def.intPtr];
      pointers_.floats = new int[func_def.floatPtr];
      pointers_.strings = new int[func_def.stringPtr];
      pointers_.chars = new int[func_def.charPtr];
      pointers_.booleans = new int[func_def.boolPtr];

      classes_ = new ClassMemory[func_def.classTy];
    }
};

#endif /* MEMORY_H */