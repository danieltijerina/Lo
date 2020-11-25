#ifndef CLASSMEM_H
#define CLASSMEM_H

#include "Memory.h"
#include "ClassDef.h"

class ClassMemory {
  public:
    std::string name_;
    int* integers;
    float* floats;
    std::string* strings;
    char* chars;
    bool* booleans;
    std::unordered_map<std::string, FunctionDef>* function_def_;

    void init(ClassDef& class_def, std::string name){
      integers = new int[class_def.intTy];
      strings = new std::string[class_def.stringTy];
      chars = new char[class_def.charTy];
      floats = new float[class_def.floatTy];
      booleans = new bool[class_def.boolTy];

      name_ = name;
    }
};

#endif