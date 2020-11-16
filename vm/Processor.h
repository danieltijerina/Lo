#ifndef PROCESSOR_H // include guard
#define PROCESSOR_H

#include <iostream>
#include <unordered_map>
#include <stack>
#include <assert.h>
#include <utility>
#include "Quads.h"
#include "Memory.h"
#include "FunctionDef.h"

using namespace Quads;
typedef std::pair<int, FunctionMemory*> previous_mem;

class Processor {
  private: 
  Memory* constant_memory;
  std::vector<Quad>* quads_;
  std::unordered_map<std::string, FunctionDef>* function_def_;
  int current_index;

  FunctionMemory* current_mem;
  FunctionMemory* next_mem;
  std::stack<previous_mem> mem_stack_;

  //Getters
  int getIntFromPosition(int position);
  float getFloatFromPosition(int position);
  std::string getStringFromPosition(int position);
  char getCharFromPosition(int position);
  bool getBoolFromPosition(int position);
  //Setters
  void setIntFromPosition(int leftPos, int rightPos);
  void setIntParamFromPosition(int leftPos, int rightPos);
  void setIntFromValue(int pos, int value);
  void setFloatFromPosition(int leftPos, int rightPos);
  void setFloatFromValue(int pos, float value);
  void setStringFromPosition(int leftPos, int rightPos);
  void setCharFromPosition(int leftPos, int rightPos);
  void setBoolFromPosition(int leftPos, int rightPos);
  void setBoolFromValue(int pos, bool value);
  //Type Checkers
  bool isInt(int pos) {
    int tmp = pos / 1000;
    return tmp == 1 || tmp == 10 || tmp == 20;
  }
  bool isFloat(int pos) {
    int tmp = pos / 1000;
    return tmp == 2 || tmp == 11 || tmp == 21;
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
    current_mem = new FunctionMemory(function_def_->find("main")->second);
    while(current_index < quads_->size() && executeQuad(((*quads_)[current_index]))){
      current_index++;
    }
  }

  bool executeQuad(Quad& current_quad) {
    switch (current_quad.type_) {
      case QuadType::gotoF:
      {
        // std::cout << "first: " << current_quad.first_ << "\nsecond: " << current_quad.second_ << "\nthird: " << current_quad.third_ << "\n";
        // std::cout << getBoolFromPosition(current_quad.second_) << std::endl;
        if(!getBoolFromPosition(current_quad.second_)) {
          current_index = current_quad.first_ - 1;
        }
        break;
      }
      case QuadType::gotoUnc:
      {
        current_index = current_quad.first_ - 1;
        break;
      }

      case QuadType::equal:
      {
        if(isInt(current_quad.first_) && isInt(current_quad.second_)){
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) == getIntFromPosition(current_quad.second_));
        } else if(isInt(current_quad.first_)) {
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) == getFloatFromPosition(current_quad.second_));
        } else if(isInt(current_quad.second_)) {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) == getIntFromPosition(current_quad.second_));
        } else {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) == getFloatFromPosition(current_quad.second_));
        }
        break;
      }

      case QuadType::notEqual:
      {
        if(isInt(current_quad.first_) && isInt(current_quad.second_)){
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) != getIntFromPosition(current_quad.second_));
        } else if(isInt(current_quad.first_)) {
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) != getFloatFromPosition(current_quad.second_));
        } else if(isInt(current_quad.second_)) {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) != getIntFromPosition(current_quad.second_));
        } else {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) != getFloatFromPosition(current_quad.second_));
        }
        break;
      }

      case QuadType::lesser:
      {
        if(isInt(current_quad.first_) && isInt(current_quad.second_)){
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) < getIntFromPosition(current_quad.second_));
        } else if(isInt(current_quad.first_)) {
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) < getFloatFromPosition(current_quad.second_));
        } else if(isInt(current_quad.second_)) {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) < getIntFromPosition(current_quad.second_));
        } else {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) < getFloatFromPosition(current_quad.second_));
        }
        break;
      }

      case QuadType::lesserEqual:
      {
        if(isInt(current_quad.first_) && isInt(current_quad.second_)){
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) <= getIntFromPosition(current_quad.second_));
        } else if(isInt(current_quad.first_)) {
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) <= getFloatFromPosition(current_quad.second_));
        } else if(isInt(current_quad.second_)) {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) <= getIntFromPosition(current_quad.second_));
        } else {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) <= getFloatFromPosition(current_quad.second_));
        }
        break;
      }

      case QuadType::greater:
      {
        if(isInt(current_quad.first_) && isInt(current_quad.second_)){
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) > getIntFromPosition(current_quad.second_));
        } else if(isInt(current_quad.first_)) {
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) > getFloatFromPosition(current_quad.second_));
        } else if(isInt(current_quad.second_)) {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) > getIntFromPosition(current_quad.second_));
        } else {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) > getFloatFromPosition(current_quad.second_));
        }
        break;
      }

      case QuadType::greaterEqual:
      {
        if(isInt(current_quad.first_) && isInt(current_quad.second_)){
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) >= getIntFromPosition(current_quad.second_));
        } else if(isInt(current_quad.first_)) {
          setBoolFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) >= getFloatFromPosition(current_quad.second_));
        } else if(isInt(current_quad.second_)) {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) >= getIntFromPosition(current_quad.second_));
        } else {
          setBoolFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) >= getFloatFromPosition(current_quad.second_));
        }
        break;
      }

      case QuadType::andOp:
      {
        setBoolFromValue(current_quad.third_, getBoolFromPosition(current_quad.first_) && getBoolFromPosition(current_quad.second_));
        break;
      }

      case QuadType::orOp:
      {
        setBoolFromValue(current_quad.third_, getBoolFromPosition(current_quad.first_) || getBoolFromPosition(current_quad.second_));
        break;
      }

      case QuadType::plus:
      {
        if(isInt(current_quad.third_)) {
          // Result of addition is int so both first_ and second_ must be int
          setIntFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) + getIntFromPosition(current_quad.second_));
        } else {
          if(isFloat(current_quad.first_) && isFloat(current_quad.second_)) {
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) + getFloatFromPosition(current_quad.second_));
          }
          else if(isInt(current_quad.first_))
            setFloatFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) + getFloatFromPosition(current_quad.second_));
          else
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) + getIntFromPosition(current_quad.second_));
        }
        break;
      }

      case QuadType::minus:
      {
        if(isInt(current_quad.third_)) {
          // Result of subtraction is int so both first_ and second_ must be int
          setIntFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) - getIntFromPosition(current_quad.second_));
        } else {
          if(isFloat(current_quad.first_) && isFloat(current_quad.second_)) {
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) - getFloatFromPosition(current_quad.second_));
          }
          else if(isInt(current_quad.first_))
            setFloatFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) - getFloatFromPosition(current_quad.second_));
          else
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) - getIntFromPosition(current_quad.second_));
        }
        break; 
      }

      case QuadType::mult:
      {
        if(isInt(current_quad.third_)) {
          // Result of product is int so both first_ and second_ must be int
          setIntFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) * getIntFromPosition(current_quad.second_));
        } else {
          if(isFloat(current_quad.first_) && isFloat(current_quad.second_)) {
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) * getFloatFromPosition(current_quad.second_));
          }
          else if(isInt(current_quad.first_))
            setFloatFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) * getFloatFromPosition(current_quad.second_));
          else
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) * getIntFromPosition(current_quad.second_));
        }
        break; 
      }

      case QuadType::div:
      {
        // Division by 0
        if((isInt(current_quad.second_) && getIntFromPosition(current_quad.second_) == 0) || (isFloat(current_quad.second_) && getFloatFromPosition(current_quad.second_) == 0))
          abort();
        if(isInt(current_quad.third_)) {
          // Result of division is int so both first_ and second_ must be int
          setIntFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) / getIntFromPosition(current_quad.second_));
        } else {
          if(isFloat(current_quad.first_) && isFloat(current_quad.second_)) {
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) / getFloatFromPosition(current_quad.second_));
          }
          else if(isInt(current_quad.first_))
            setFloatFromValue(current_quad.third_, getIntFromPosition(current_quad.first_) / getFloatFromPosition(current_quad.second_));
          else
            setFloatFromValue(current_quad.third_, getFloatFromPosition(current_quad.first_) / getIntFromPosition(current_quad.second_));
        }
        break; 
      }

      case QuadType::assign:
      {
        switch (current_quad.first_ / 1000) {
        case 1:
          setIntFromPosition(current_quad.first_, current_quad.second_);
          // std::cout << "Assigned position: " << current_quad.first_ << " to value: " << getIntFromPosition(current_quad.first_) << "\n";
          break;
        case 2:
          setFloatFromPosition(current_quad.first_, current_quad.second_);
          // std::cout << "Assigned position: " << current_quad.first_ << " to value: " << getFloatFromPosition(current_quad.first_) << "\n";
          break;
        case 3:
          setStringFromPosition(current_quad.first_, current_quad.second_);
          // std::cout << "Assigned position: " << current_quad.first_ << " to value: " << getStringFromPosition(current_quad.first_) << "\n";
          break;
        case 4:
          setCharFromPosition(current_quad.first_, current_quad.second_);
          // std::cout << "Assigned position: " << current_quad.first_ << " to value: " << getCharFromPosition(current_quad.first_) << "\n";
          break;
        case 5:
          setBoolFromPosition(current_quad.first_, current_quad.second_);
          // std::cout << "Assigned position: " << current_quad.first_ << " to value: " << getBoolFromPosition(current_quad.first_) << "\n";
          break;
        }
        break;
      }

      case QuadType::era:
      {
        auto fun_def = function_def_ -> find(current_quad.name_);
        if(fun_def != function_def_ -> end()){
          //TODO: push previous memory to stack;
          next_mem = new FunctionMemory(fun_def->second);
        }
        else{
          assert(false);
        }
        break;
      }

      case QuadType::goSub:
      {
        mem_stack_.push(previous_mem(current_index, current_mem));
        current_mem = next_mem;
        next_mem = nullptr;

        current_index = current_quad.first_ - 1;
        break;
      }

      case QuadType::param:
      {
        switch (current_quad.second_ / 1000) {
        case 1:
          setIntParamFromPosition(current_quad.second_, current_quad.first_);
          break;
        case 2:
          setFloatFromPosition(current_quad.second_, current_quad.first_);
          break;
        case 3:
          setStringFromPosition(current_quad.second_, current_quad.first_);
          break;
        case 4:
          setCharFromPosition(current_quad.second_, current_quad.first_);
          break;
        case 5:
          setBoolFromPosition(current_quad.second_, current_quad.first_);
          break;
        }
        break;
      }

      case QuadType::endFunc:
      {
        if(!mem_stack_.empty()){
          delete current_mem;
          previous_mem m = mem_stack_.top();
          current_mem = m.second;
          current_index = m.first;
          mem_stack_.pop();
        }else{
          return false;
        }
        break;
      }

      case QuadType::print:
      {
        if(isInt(current_quad.first_))
          std::cout << getIntFromPosition(current_quad.first_) << std::endl;
        else if(isFloat(current_quad.first_))
          std::cout << getFloatFromPosition(current_quad.first_) << std::endl;
        else
          std::cout << getStringFromPosition(current_quad.first_) << std::endl;
        break;
      }

      default:
        break;
    }
    return true;
  }
};

int Processor::getIntFromPosition(int position){
    int area = position / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 1){
      return current_mem->variables_.integers[position % 1000];
    }
    if(area == 10){
      return constant_memory->integers[position % 10000];
    }
    
    if(area == 20){
      return current_mem->temporals_.integers[position % 20000];
    }

    // None: Error
    return -1;
}

void Processor::setIntFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 1){
      current_mem->variables_.integers[leftPos % 1000] = current_mem->variables_.integers[rightPos % 1000];
    }
    if(area == 10){
      current_mem->variables_.integers[leftPos % 1000] = constant_memory->integers[rightPos % 10000];
    }
    
    if(area == 20){
      current_mem->variables_.integers[leftPos % 1000] = current_mem->temporals_.integers[rightPos % 20000];
    }
}

void Processor::setIntParamFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 1){
      next_mem->variables_.integers[leftPos % 1000] = current_mem->variables_.integers[rightPos % 1000];
    }
    if(area == 10){
      next_mem->variables_.integers[leftPos % 1000] = constant_memory->integers[rightPos % 10000];
    }
    
    if(area == 20){
      next_mem->variables_.integers[leftPos % 1000] = current_mem->temporals_.integers[rightPos % 20000];
    }
}

void Processor::setIntFromValue(int pos, int value) {
  int area = pos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 1){
      current_mem->variables_.integers[pos % 1000] = value;
    }
    if(area == 20){
      current_mem->temporals_.integers[pos % 20000] = value;
    }
}



float Processor::getFloatFromPosition(int position){
    int area = position / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 2){
      return current_mem->variables_.floats[position % 2000];
    }
    if(area == 11){
      return constant_memory->floats[position % 11000];
    }
    
    if(area == 21){
      return current_mem->temporals_.floats[position % 21000];
    }

    // None: Error
    return -1;
}

void Processor::setFloatFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 2){
      current_mem->variables_.floats[leftPos % 2000] = current_mem->variables_.floats[rightPos % 2000];
    }
    if(area == 11){
      current_mem->variables_.floats[leftPos % 2000] = constant_memory->floats[rightPos % 11000];
    }
    
    if(area == 21){
      current_mem->variables_.floats[leftPos % 2000] = current_mem->temporals_.floats[rightPos % 21000];
    }
}

void Processor::setFloatFromValue(int pos, float value){
    int area = pos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 2){
      current_mem->variables_.floats[pos % 2000] = value;
    }
    if(area == 21){
      current_mem->temporals_.floats[pos % 21000] = value;
    }
}

std::string Processor::getStringFromPosition(int position){
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

void Processor::setStringFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 3){
      current_mem->variables_.strings[leftPos % 3000] = current_mem->variables_.strings[rightPos % 3000];
    }
    if(area == 12){
      current_mem->variables_.strings[leftPos % 3000] = constant_memory->strings[rightPos % 12000];
    }
    
    if(area == 22){
      current_mem->variables_.strings[leftPos % 3000] = current_mem->temporals_.strings[rightPos % 22000];
    }
}

char Processor::getCharFromPosition(int position){
    int area = position / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 4){
      return current_mem->variables_.chars[position % 4000];
    }
    if(area == 13){
      return constant_memory->chars[position % 13000];
    }
    
    if(area == 23){
      return current_mem->temporals_.chars[position % 23000];
    }

    // None: Error
    return -1;
}

void Processor::setCharFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 4){
      current_mem->variables_.chars[leftPos % 4000] = current_mem->variables_.chars[rightPos % 4000];
    }
    if(area == 13){
      current_mem->variables_.chars[leftPos % 4000] = constant_memory->chars[rightPos % 13000];
    }
    
    if(area == 23){
      current_mem->variables_.chars[leftPos % 4000] = current_mem->temporals_.chars[rightPos % 23000];
    }
}

bool Processor::getBoolFromPosition(int position){
    int area = position / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 5){
      return current_mem->variables_.booleans[position % 5000];
    }
    if(area == 14){
      return constant_memory->booleans[position % 14000];
    }
    
    if(area == 24){
      return current_mem->temporals_.booleans[position % 24000];
    }

    // None: Error
    return -1;
}

void Processor::setBoolFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 5){
      current_mem->variables_.booleans[leftPos % 5000] = current_mem->variables_.booleans[rightPos % 5000];
    }
    if(area == 14){
      current_mem->variables_.booleans[leftPos % 5000] = constant_memory->booleans[rightPos % 14000];
    }
    
    if(area == 24){
      current_mem->variables_.booleans[leftPos % 5000] = current_mem->temporals_.booleans[rightPos % 24000];
    }
}

void Processor::setBoolFromValue(int pos, bool value){
    int area = pos / 1000;
    // TODO: not sure if this is the best way to do things
    if(area == 5){
      std::cout << value << std::endl;
      current_mem->variables_.booleans[pos % 5000] = value;
    }   
    if(area == 24){
      current_mem->temporals_.booleans[pos % 24000] = value;
    }
}

#endif