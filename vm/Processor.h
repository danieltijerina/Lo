#ifndef PROCESSOR_H // include guard
#define PROCESSOR_H

#include <iostream>
#include <unordered_map>
#include "Quads.h"
#include "Memory.h"
#include "FunctionDef.h"

using namespace Quads;

class Processor {
  private: 
  Memory* constant_memory;
  std::vector<Quad>* quads_;
  std::unordered_map<std::string, FunctionDef>* function_def_;
  int current_index;

  FunctionMemory* current_mem;

  std::string getStringFromPosition(int position){
    int area = position / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 3){
      return current_mem->variables_.strings[position % 3000];
    }
    if(area == 12){
      return constant_memory->strings[position % 12000];
    }
    
    if(area == 22){
      return current_mem->temporals_.strings[position % 22000];
    }

    // None: Error
    return "";
  }

  public:
  Processor(std::vector<Quad>* quads, 
            std::unordered_map<std::string, FunctionDef>* functions, 
            Memory* constant_mem){
    constant_memory = constant_mem;
    quads_ = quads;
    function_def_ = functions;
    current_index = 0;
  }

  void startProcessing(){
    while(current_index < quads_->size()){
      executeQuad(((*quads_)[current_index]));
      current_index++;
    }
  }

  void executeQuad(Quad& current_quad){
    switch (current_quad.type_)
    {
    case QuadType::plus:
      break;

    case QuadType::era:
    {
      auto fun_def = function_def_ -> find(current_quad.name_);
      if(fun_def != function_def_ -> end()){
        //TODO: push previous memory to stack;
        current_mem = new FunctionMemory(fun_def->second);
      }
      break;
    }

    case QuadType::print:
    {
      std::cout << getStringFromPosition(current_quad.first_) << std::endl;
      break;
    }

    
    default:
      break;
    }
  }

};

#endif