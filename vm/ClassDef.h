#ifndef CLASSDEF_H
#define CLASSDEF_H

#include <unordered_map>

class ClassDef {
  public: 
    int intTy;
    int stringTy;
    int charTy;
    int floatTy;
    int boolTy;
    int classTy;

    std::unordered_map<std::string, FunctionDef>* funcion_def_;
};

#endif