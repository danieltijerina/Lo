#ifndef PROCESSOR_H // include guard
#define PROCESSOR_H

#include <iostream>
#include <unordered_map>
#include <stack>
#include <assert.h>
#include <utility>
#include <iomanip>
#include "Quads.h"
#include "Memory.h"
#include "FunctionDef.h"
#include "ClassDef.h"

using namespace Quads;
typedef std::pair<int, FunctionMemory*> previous_mem;

class Processor {
  private: 
  Memory* constant_memory;
  std::vector<Quad>* quads_;
  std::unordered_map<std::string, FunctionDef>* function_def_;
  std::unordered_map<std::string, ClassDef>* class_def_;
  int current_index;

  FunctionMemory* current_mem;
  FunctionMemory* next_mem;
  ClassMemory* current_class_mem;
  std::stack<previous_mem> mem_stack_;

  //Getters
  int getIntFromPosition(int position);
  float getFloatFromPosition(int position);
  std::string getStringFromPosition(int position);
  char getCharFromPosition(int position);
  bool getBoolFromPosition(int position);
  int getPointerFromPosition(int position);
  //Setters
  void setIntFromPosition(int leftPos, int rightPos);
  void setIntParamFromPosition(int leftPos, int rightPos);
  void setIntFromValue(int pos, int value);
  void setFloatFromPosition(int leftPos, int rightPos);
  void setFloatParamFromPosition(int leftPos, int rightPos);
  void setFloatFromValue(int pos, float value);
  void setStringFromPosition(int leftPos, int rightPos);
  void setStringParamFromPosition(int leftPos, int rightPos);
  void setStringFromValue(int pos, std::string value);
  void setCharFromPosition(int leftPos, int rightPos);
  void setCharParamFromPosition(int leftPos, int rightPos);
  void setCharFromValue(int pos, char value);
  void setBoolFromPosition(int leftPos, int rightPos);
  void setBoolParamFromPosition(int leftPos, int rightPos);
  void setBoolFromValue(int pos, bool value);
  void setPointerFromPosition(int pos, int value);
  //Type Checkers
  bool isInt(int pos) {
    int tmp = pos / 1000;
    return tmp == 1 || tmp == 10 || tmp == 20 || tmp == 40 || (tmp > 50 && tmp % 10 == 1) || (tmp < 0 && (pos + 50000) / 1000 == 1);
  }
  bool isFloat(int pos) {
    int tmp = pos / 1000;
    return tmp == 2 || tmp == 11 || tmp == 21 || tmp == 41 || (tmp > 50 && tmp % 10 == 2) || (tmp < 0 && (pos + 50000) / 1000 == 2);
  }
  bool isString(int pos) {
    int tmp = pos / 1000;
    return tmp == 3 || tmp == 12 || tmp == 22 || tmp == 42 || (tmp > 50 && tmp % 10 == 3) || (tmp < 0 && (pos + 50000) / 1000 == 3);
  }

  public:
  Processor(std::vector<Quad>* quads, 
            std::unordered_map<std::string, FunctionDef>* functions, 
            std::unordered_map<std::string, ClassDef>* class_def,
            Memory* constant_mem){
    constant_memory = constant_mem;
    quads_ = quads;
    function_def_ = functions;
    class_def_ = class_def;
    current_index = 0;
    next_mem = nullptr;
  }

  void startProcessing(){
    current_mem = new FunctionMemory(function_def_->find("main")->second, "main");
    while(current_index < quads_->size() && executeQuad(((*quads_)[current_index]))){
      current_index++;
    }
  }

  bool executeQuad(Quad& current_quad) {
    switch (current_quad.type_) {
      case QuadType::gotoF:
      {
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
        if(current_quad.first_ / 10000 == 4 && current_quad.second_ / 10000 == 4) {
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
        }
        else if(current_quad.second_ / 10000 == 4) {
          setPointerFromPosition(current_quad.third_, getIntFromPosition(current_quad.first_) + getPointerFromPosition(current_quad.second_));
        }
        else if(current_quad.third_ / 10000 == 4) {
          setPointerFromPosition(current_quad.third_, getIntFromPosition(current_quad.first_) + getIntFromPosition(current_quad.second_));
        }
        else if(isInt(current_quad.third_)) {
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

      case QuadType::read:
      {
        int type_num = current_quad.first_ / 1000;
        if(current_quad.first_ > 50000){
          type_num = ((current_quad.first_ - 50000) % 10000) / 1000;
        }else if(current_quad.first_ < 0){
          type_num = (current_quad.first_ + 50000) / 1000;
        }
        switch (type_num) {
          case 1:
          case 40: // int pointers
          {
            string temp_int;
            std::cin >> temp_int;
            try{
            setIntFromValue(current_quad.first_, stoi(temp_int));
            }catch(std::invalid_argument& e){
              std::cout << "Error: Expected an int" << std::endl;
              assert(false);
            }
            std::cin.ignore();
            break;
          }
          case 2:
          case 41: // float pointers
          {
            string temp_float;
            std::cin >> temp_float;
            try{
            setFloatFromValue(current_quad.first_, stof(temp_float));
            }catch(std::invalid_argument& e){
              std::cout << "Error: Expected a float" << std::endl;
            }
            std::cin.ignore();
            break;
          }
          case 3:
          case 42: // string pointers
          {
            std::string temp_string; 
            std::getline(std::cin, temp_string);
            setStringFromValue(current_quad.first_, temp_string);
            break;
          }
          case 4:
          case 43: // char pointers
          {
            string temp_char;
            std::cin >> temp_char;
            if(temp_char.length() != 1){
              std::cout << "Error: Expected a char" << std::endl;
              assert(false);
            }
            setCharFromValue(current_quad.first_, temp_char[0]);
            std::cin.ignore();
            break;
          }
          case 5:
          case 44: // bool pointers
          {
            std::string temp_bool_s;
            std::cin >> temp_bool_s;
            setBoolFromValue(current_quad.first_, temp_bool_s == "true");
            std::cin.ignore();
            break;
          }
        }
        break;
      }

      case QuadType::assign:
      {
        int type_num = current_quad.first_ / 1000;
        if(current_quad.first_ > 50000){
          type_num = ((current_quad.first_ - 50000) % 10000) / 1000;
        }else if(current_quad.first_ < 0){
          type_num = (current_quad.first_ + 50000) / 1000;
        }

        for(int i=0; i<current_quad.third_; i++) {
          switch (type_num) {
          case 1:
            setIntFromPosition(current_quad.first_+i, current_quad.second_+i);
            break;
          case 2:
            setFloatFromPosition(current_quad.first_+i, current_quad.second_+i);
            break;
          case 3:
            setStringFromPosition(current_quad.first_+i, current_quad.second_+i);
            break;
          case 4:
            setCharFromPosition(current_quad.first_+i, current_quad.second_+i);
            break;
          case 5:
            setBoolFromPosition(current_quad.first_+i, current_quad.second_+i);
            break;
          case 40:
            setIntFromPosition(getPointerFromPosition(current_quad.first_+i), current_quad.second_+i);
            break;
          case 41:
            setFloatFromPosition(getPointerFromPosition(current_quad.first_+i), current_quad.second_+i);
            break;
          case 42:
            setStringFromPosition(getPointerFromPosition(current_quad.first_+i), current_quad.second_+i);
            break;
          case 43:
            setCharFromPosition(getPointerFromPosition(current_quad.first_+i), current_quad.second_+i);
            break;
          case 44:
            setBoolFromPosition(getPointerFromPosition(current_quad.first_+i), current_quad.second_+i);
            break;
          }
        }
        break;
      }

      case QuadType::era:
      {
        auto fun_def = function_def_ -> find(current_quad.name_);
        if(fun_def != function_def_ -> end()){
          if(next_mem == nullptr){
            next_mem = new FunctionMemory(fun_def->second, current_quad.name_);
          }
          else{
            mem_stack_.push(previous_mem(current_index, next_mem));
            next_mem = new FunctionMemory(fun_def->second, current_quad.name_);
          }
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
        if(next_mem == nullptr){
          next_mem = mem_stack_.top().second;
          mem_stack_.pop();
        }
        for(int i=0; i<current_quad.third_; i++) {
          switch (current_quad.second_ / 1000) {
          case 1:
            setIntParamFromPosition(current_quad.second_ + i, current_quad.first_ + i);
            break;
          case 2:
            setFloatParamFromPosition(current_quad.second_ + i, current_quad.first_ + i);
            break;
          case 3:
            setStringParamFromPosition(current_quad.second_ + i, current_quad.first_ + i);
            break;
          case 4:
            setCharParamFromPosition(current_quad.second_ + i, current_quad.first_ + i);
            break;
          case 5:
            setBoolParamFromPosition(current_quad.second_ + i, current_quad.first_ + i);
            break;
          }
        }
        break;
      }

      case QuadType::ret:
      {
        auto fun_def = function_def_ -> find(current_mem->fname_);
        if(fun_def != function_def_ -> end()) {
          int type_int = current_quad.first_ / 1000;
          if(current_quad.first_ > 50000){
            type_int = ((current_quad.first_ - 50000) % 50000) / 1000;
          }else if(current_quad.first_ < 0){
            type_int = (current_quad.first_ + 50000) / 1000;
          }
          for(int i=0; i<current_quad.second_; i++) {
            switch (type_int) {
              case 1:
              case 10:
              case 20:
                fun_def->second.intRet[i] = getIntFromPosition(current_quad.first_ + i);
                break;
              case 2:
              case 11:
              case 21:
                fun_def->second.floatRet[i] = getFloatFromPosition(current_quad.first_ + i);
                break;
              case 3:
              case 12:
              case 22:
                fun_def->second.stringRet[i] = getStringFromPosition(current_quad.first_ + i);
                break;
              case 4:
              case 13:
              case 23:
                fun_def->second.charRet[i] = getCharFromPosition(current_quad.first_ + i);
                break;
              case 5:
              case 14:
              case 24:
                fun_def->second.boolRet[i] = getBoolFromPosition(current_quad.first_ + i);
                break;
            }
          }
        }
        else{
          std::cout << "Function not found in function class" << std::endl;
          assert(false);
        }
      }
      // return quad goes to endFunc directly
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

      case QuadType::noRet:
      {
        std::cout << "ERROR: Function " << current_quad.name_ << " must always return a value.\n";
        assert(false);
      }

      case QuadType::retVal:
      {
        auto fun_def = function_def_ -> find(current_quad.name_/*CHANGE THIS TO READ FROM FUNCTION CALLED*/);
        if(fun_def != function_def_ -> end()) {
          for(int i=0; i<current_quad.second_; i++) {
            switch (current_quad.first_ / 1000) {
              case 1:
              case 10:
              case 20:
                setIntFromValue(current_quad.first_ + i, fun_def->second.intRet[i]);
                break;
              case 2:
              case 11:
              case 21:
                setFloatFromValue(current_quad.first_ + i, fun_def->second.floatRet[i]);
                break;
              case 3:
              case 12:
              case 22:
                setStringFromValue(current_quad.first_ + i, fun_def->second.stringRet[i]);
                break;
              case 4:
              case 13:
              case 23:
                setCharFromValue(current_quad.first_ + i, fun_def->second.charRet[i]);
                break;
              case 5:
              case 14:
              case 24:
                setBoolFromValue(current_quad.first_ + i, fun_def->second.boolRet[i]);
                break;
            }
          }
        }
        else{
          assert(false);
        }
        break;
      }

      case QuadType::val:
      {
        // This is only used to validate array indices, so it should always be int
        if(!isInt(current_quad.first_)) {
          abort();
        }
        int index = getIntFromPosition(current_quad.first_);
        if(index < 0 || index >= current_quad.second_) {
          abort();
        }
        break;
      }

      case QuadType::print:
      {
        if(isInt(current_quad.first_))
          std::cout << getIntFromPosition(current_quad.first_) << std::endl;
        else if(isFloat(current_quad.first_))
          std::cout << getFloatFromPosition(current_quad.first_) << std::endl;
        else if(isString(current_quad.first_))
          std::cout << getStringFromPosition(current_quad.first_) << std::endl;
        else
          std::cout << getCharFromPosition(current_quad.first_) << std::endl;
        break;
      }

      case QuadType::classInit:
      {
        current_mem->classes_[current_quad.first_ % 30000].init(class_def_->find(current_quad.name_)->second, current_quad.name_);
        break;
      }

      case QuadType::classMark:
      {
        current_class_mem = &(current_mem->classes_[current_quad.first_ % 30000]);
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
    if(area == 1){
      return current_mem->variables_.integers[position % 1000];
    }
    if(area == 10){
      return constant_memory->integers[position % 10000];
    }
    if(area == 20){
      return current_mem->temporals_.integers[position % 20000];
    }
    if(area == 40) {
      return getIntFromPosition(current_mem->pointers_.integers[position % 40000]);
    }
    if(area > 50){
      int mem_index = (position - 50000) / 10000;
      return current_mem->classes_[mem_index].integers[position % 1000];
    }
    if(area < 0){
      return current_class_mem->integers[(position + 50000) % 1000];
    }

    // None: Error
    return -1;
}

void Processor::setIntFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(leftPos < 50000 && leftPos > 0){
      if(area == 1){
        current_mem->variables_.integers[leftPos % 1000] = current_mem->variables_.integers[rightPos % 1000];
      }
      if(area == 10){
        current_mem->variables_.integers[leftPos % 1000] = constant_memory->integers[rightPos % 10000];
      }
      if(area == 20){
        current_mem->variables_.integers[leftPos % 1000] = current_mem->temporals_.integers[rightPos % 20000];
      }
      if(area==40){
        current_mem->variables_.integers[leftPos % 1000] = getIntFromPosition(current_mem->pointers_.integers[rightPos % 40000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_mem->variables_.integers[leftPos % 1000] = current_mem->classes_[right_mem_index].integers[rightPos % 1000];
      }
      if(area < 0){
        current_mem->variables_.integers[leftPos % 1000] = current_class_mem->integers[(rightPos + 50000) % 1000];
      }
    } else if(leftPos > 50000){
      int mem_index = (leftPos - 50000) / 10000;
      if(area == 1){
        current_mem->classes_[mem_index].integers[leftPos % 1000] = current_mem->variables_.integers[rightPos % 1000];
      }
      if(area == 10){
        current_mem->classes_[mem_index].integers[leftPos % 1000] = constant_memory->integers[rightPos % 10000];
      }
      if(area == 20){
        current_mem->classes_[mem_index].integers[leftPos % 1000] = current_mem->temporals_.integers[rightPos % 20000];
      }
      if(area==40){
        current_mem->classes_[mem_index].integers[leftPos % 1000] = getIntFromPosition(current_mem->pointers_.integers[rightPos % 40000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_mem->classes_[mem_index].integers[leftPos % 1000] = current_mem->classes_[right_mem_index].integers[rightPos % 1000];
      }
      if(area < 0){
        current_mem->classes_[mem_index].integers[leftPos % 1000] = current_class_mem->integers[(rightPos + 50000) % 1000];
      }
    }
    else {
      if(area == 1){
        current_class_mem->integers[(leftPos + 50000) % 1000] = current_mem->variables_.integers[rightPos % 1000];
      }
      if(area == 10){
        current_class_mem->integers[(leftPos + 50000) % 1000] = constant_memory->integers[rightPos % 10000];
      }
      if(area == 20){
        current_class_mem->integers[(leftPos + 50000) % 1000] = current_mem->temporals_.integers[rightPos % 20000];
      }
      if(area==40){
        current_class_mem->integers[(leftPos + 50000) % 1000] = getIntFromPosition(current_mem->pointers_.integers[rightPos % 40000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_class_mem->integers[(leftPos + 50000) % 1000] = current_mem->classes_[right_mem_index].integers[rightPos % 1000];
      }
      if(area < 0){
        
        current_class_mem->integers[(leftPos + 50000) % 1000] = current_class_mem->integers[(rightPos + 50000) % 1000];
      }
    }
}

void Processor::setIntParamFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(area == 1){
      next_mem->variables_.integers[leftPos % 1000] = current_mem->variables_.integers[rightPos % 1000];
    }
    if(area == 10){
      next_mem->variables_.integers[leftPos % 1000] = constant_memory->integers[rightPos % 10000];
    }
    if(area == 20){
      next_mem->variables_.integers[leftPos % 1000] = current_mem->temporals_.integers[rightPos % 20000];
    }
    if(area == 40){
      next_mem->variables_.integers[leftPos % 1000] = getIntFromPosition(current_mem->pointers_.integers[rightPos % 40000]);
    }
    else {
      next_mem->variables_.integers[leftPos % 1000] = getIntFromPosition(rightPos);
    }
}

void Processor::setIntFromValue(int pos, int value) {
  int area = pos / 1000;
  if(pos > 50000 ){
    int mem_index = (pos - 50000) / 50000;
    int int_index = (pos - 50000) % 50000 % 1000;
    current_mem->classes_[mem_index].integers[int_index] = value;
    return;
  }
  if(pos < 0){
    current_class_mem->integers[(pos + 50000) % 1000] = value;
  }

  if(area == 1){
    current_mem->variables_.integers[pos % 1000] = value;
  }
  if(area == 20){
    current_mem->temporals_.integers[pos % 20000] = value;
  }
  if(area == 40){
    setIntFromValue(current_mem->pointers_.integers[pos % 40000], value);
  }
}



float Processor::getFloatFromPosition(int position){
    int area = position / 1000;
    if(area == 2){
      return current_mem->variables_.floats[position % 2000];
    }
    if(area == 11){
      return constant_memory->floats[position % 11000];
    }
    if(area == 21){
      return current_mem->temporals_.floats[position % 21000];
    }
    if(area == 41){
      return getFloatFromPosition(current_mem->pointers_.floats[position % 41000]);
    }
    if(area > 50){
      int mem_index = (position - 50000) / 10000;
      return current_mem->classes_[mem_index].floats[position % 2000];
    }
    if(area < 0){
      return current_class_mem->floats[(position + 50000) % 2000];
    }
    // None: Error
    return -1;
}

void Processor::setFloatParamFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(area == 2){
      next_mem->variables_.floats[leftPos % 2000] = current_mem->variables_.floats[rightPos % 2000];
    }
    if(area == 11){
      next_mem->variables_.floats[leftPos % 2000] = constant_memory->floats[rightPos % 11000];
    }
    if(area == 21){
      next_mem->variables_.floats[leftPos % 2000] = current_mem->temporals_.floats[rightPos % 21000];
    }
    if(area == 41){
      next_mem->variables_.floats[leftPos % 2000] = getFloatFromPosition(current_mem->pointers_.floats[rightPos % 41000]);
    }
    else {
      next_mem->variables_.floats[leftPos % 2000] = getFloatFromPosition(rightPos);
    }
}

void Processor::setFloatFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(leftPos < 50000 && leftPos > 0){
      if(area == 2){
        current_mem->variables_.floats[leftPos % 2000] = current_mem->variables_.floats[rightPos % 2000];
      }
      if(area == 11){
        current_mem->variables_.floats[leftPos % 2000] = constant_memory->floats[rightPos % 11000];
      }
      if(area == 21){
        current_mem->variables_.floats[leftPos % 2000] = current_mem->temporals_.floats[rightPos % 21000];
      }
      if(area == 41){
        current_mem->variables_.floats[leftPos % 2000] = getFloatFromPosition(current_mem->pointers_.floats[rightPos % 41000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_mem->variables_.floats[leftPos % 2000] = current_mem->classes_[right_mem_index].floats[rightPos % 2000];
      }
    }
    else if(leftPos > 50000){
      int mem_index = (leftPos - 50000) / 10000;
      if(area == 2){
        current_mem->classes_[mem_index].floats[leftPos % 2000] = current_mem->variables_.floats[rightPos % 2000];
      }
      if(area == 11){
        current_mem->classes_[mem_index].floats[leftPos % 2000] = constant_memory->floats[rightPos % 11000];
      }
      if(area == 21){
        current_mem->classes_[mem_index].floats[leftPos % 2000] = current_mem->temporals_.floats[rightPos % 21000];
      }
      if(area == 41){
        current_mem->classes_[mem_index].floats[leftPos % 2000] = getFloatFromPosition(current_mem->pointers_.floats[rightPos % 41000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_mem->classes_[mem_index].floats[leftPos % 2000] = current_mem->classes_[right_mem_index].floats[rightPos % 2000];
      }
    }
    else {
      if(area == 1){
        current_class_mem->floats[(leftPos + 50000) % 2000] = current_mem->variables_.floats[rightPos % 2000];
      }
      if(area == 11){
        current_class_mem->floats[(leftPos + 50000) % 2000] = constant_memory->floats[rightPos % 11000];
      }
      if(area == 21){
        current_class_mem->floats[(leftPos + 50000) % 2000] = current_mem->temporals_.floats[rightPos % 21000];
      }
      if(area==41){
        current_class_mem->floats[(leftPos + 50000) % 2000] = getFloatFromPosition(current_mem->pointers_.floats[rightPos % 41000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_class_mem->floats[(leftPos + 50000) % 2000] = current_mem->classes_[right_mem_index].floats[rightPos % 2000];
      }
      if(area < 0){
        current_class_mem->floats[(leftPos + 50000) % 2000] = current_class_mem->floats[(rightPos + 50000) % 2000];
      }
    }

}

void Processor::setFloatFromValue(int pos, float value){
    int area = pos / 1000;
    if(pos > 50000 ){
      int mem_index = (pos - 50000) / 50000;
      int int_index = (pos - 50000) % 50000 % 2000;
      current_mem->classes_[mem_index].floats[int_index] = value;
      return;
    }
    if(pos < 0){
      current_class_mem->floats[(pos + 50000) % 2000] = value;
    }
    if(area == 2){
      current_mem->variables_.floats[pos % 2000] = value;
    }
    if(area == 21){
      current_mem->temporals_.floats[pos % 21000] = value;
    }
    if(area == 41){
      setFloatFromValue(current_mem->pointers_.floats[pos % 41000], value);
    }
}

std::string Processor::getStringFromPosition(int position){
    int area = position / 1000;
    if(area == 3){
      return current_mem->variables_.strings[position % 3000];
    }
    if(area == 12){
      return constant_memory->strings[position % 12000];
    }
    if(area == 22){
      return current_mem->temporals_.strings[position % 22000];
    }
    if(area == 42){
      return getStringFromPosition(current_mem->pointers_.strings[position % 42000]);
    }
    if(area > 50){
      int mem_index = (position - 50000) / 10000;
      int pos_index = ((position - 50000) % 10000) % 3000;
      return current_mem->classes_[mem_index].strings[pos_index];
    }
    if(area < 0){
      return current_class_mem->strings[(position + 50000) % 3000];
    }
    // None: Error
    return "";
}

void Processor::setStringFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(leftPos < 50000 && leftPos > 0){
      if(area == 3){
        current_mem->variables_.strings[leftPos % 3000] = current_mem->variables_.strings[rightPos % 3000];
      }
      if(area == 12){
        current_mem->variables_.strings[leftPos % 3000] = constant_memory->strings[rightPos % 12000];
      }
      if(area == 22){
        current_mem->variables_.strings[leftPos % 3000] = current_mem->temporals_.strings[rightPos % 22000];
      }
      if(area == 42){
        current_mem->variables_.strings[leftPos % 3000] = getStringFromPosition(current_mem->pointers_.strings[rightPos % 42000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_mem->variables_.strings[leftPos % 3000] = current_mem->classes_[right_mem_index].integers[rightPos % 1000];
      }
    }else if(leftPos > 50000) {
      int mem_index = (leftPos - 50000) / 10000;
      int left_pos_index = ((leftPos - 50000) % 10000) % 3000;
      if(area == 3){
        current_mem->classes_[mem_index].strings[left_pos_index] = current_mem->variables_.strings[rightPos % 3000];
      }
      if(area == 12){
        current_mem->classes_[mem_index].strings[left_pos_index] = constant_memory->strings[rightPos % 12000];
      }
      if(area == 22){
        current_mem->classes_[mem_index].strings[left_pos_index] = current_mem->temporals_.strings[rightPos % 22000];
      }
      if(area==42){
        current_mem->classes_[mem_index].strings[left_pos_index] = getStringFromPosition(current_mem->pointers_.strings[rightPos % 42000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        int right_pos_index = ((rightPos - 50000) % 10000) % 3000;
        current_mem->classes_[mem_index].integers[left_pos_index] = current_mem->classes_[right_mem_index].integers[right_pos_index];
      }
    } else {
     if(area == 3){
        current_class_mem->strings[(leftPos + 50000) % 3000] = current_mem->variables_.strings[rightPos % 3000];
      }
      if(area == 12){
        current_class_mem->strings[(leftPos + 50000) % 3000] = constant_memory->strings[rightPos % 12000];
      }
      if(area == 22){
        current_class_mem->strings[(leftPos + 50000) % 3000] = current_mem->temporals_.strings[rightPos % 22000];
      }
      if(area==42){
        current_class_mem->strings[(leftPos + 50000) % 3000] = getFloatFromPosition(current_mem->pointers_.strings[rightPos % 42000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_class_mem->strings[(leftPos + 50000) % 3000] = current_mem->classes_[right_mem_index].strings[rightPos % 3000];
      }
      if(area < 0){
        current_class_mem->strings[(leftPos + 50000) % 3000] = current_class_mem->strings[(rightPos + 50000) % 3000];
      } 
    }
}

void Processor::setStringParamFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(area == 3){
      next_mem->variables_.strings[leftPos % 3000] = current_mem->variables_.strings[rightPos % 3000];
    }
    if(area == 12){
      next_mem->variables_.strings[leftPos % 3000] = constant_memory->strings[rightPos % 12000];
    }
    if(area == 22){
      next_mem->variables_.strings[leftPos % 3000] = current_mem->temporals_.strings[rightPos % 22000];
    }
    if(area == 42){
      next_mem->variables_.strings[leftPos % 3000] = getStringFromPosition(current_mem->pointers_.strings[rightPos % 42000]);
    }
    else {
      next_mem->variables_.strings[leftPos % 3000] = getStringFromPosition(rightPos);
    }
}

void Processor::setStringFromValue(int pos, std::string value) {
  int area = pos / 1000;
  if(pos > 50000 ){
    int mem_index = (pos - 50000) / 50000;
    int int_index = (pos - 50000) % 50000 % 3000;
    current_mem->classes_[mem_index].strings[int_index] = value;
    return;
  }
  if(pos < 0){
    current_class_mem->strings[(pos + 50000) % 3000] = value;
  }
  if(area == 3){
    current_mem->variables_.strings[pos % 3000] = value;
  }
  if(area == 22){
    current_mem->temporals_.strings[pos % 22000] = value;
  }
  if(area == 42){
    setStringFromValue(current_mem->pointers_.strings[pos % 42000], value);
  }
}

char Processor::getCharFromPosition(int position){
    int area = position / 1000;
    if(area == 4){
      return current_mem->variables_.chars[position % 4000];
    }
    if(area == 13){
      return constant_memory->chars[position % 13000];
    }
    if(area == 23){
      return current_mem->temporals_.chars[position % 23000];
    }
    if(area == 43){
      return getCharFromPosition(current_mem->pointers_.chars[position % 43000]);
    }
    if(area > 50){
      int mem_index = (position - 50000) / 10000;
      int pos_index = ((position - 50000) % 10000) % 4000;
      return current_mem->classes_[mem_index].chars[pos_index];
    }
    if(area < 0){
      return current_class_mem->chars[(position + 50000) % 4000];
    }
    // None: Error
    return -1;
}

void Processor::setCharFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(leftPos < 50000 && leftPos > 0){
      if(area == 4){
        current_mem->variables_.chars[leftPos % 4000] = current_mem->variables_.chars[rightPos % 4000];
      }
      if(area == 13){
        current_mem->variables_.chars[leftPos % 4000] = constant_memory->chars[rightPos % 13000];
      }
      if(area == 23){
        current_mem->variables_.chars[leftPos % 4000] = current_mem->temporals_.chars[rightPos % 23000];
      }
      if(area == 43){
        current_mem->variables_.chars[leftPos % 4000] = getCharFromPosition(current_mem->pointers_.chars[rightPos % 43000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        int right_pos_index = ((rightPos - 50000) % 10000) % 4000;
        current_mem->variables_.chars[leftPos % 4000] = current_mem->classes_[right_mem_index].chars[right_pos_index];
      }
    } else if(leftPos > 50000) {
      int mem_index = (leftPos - 50000) / 10000;
      int left_pos_index = ((leftPos - 50000) % 10000) % 4000;
      if(area == 4){
        current_mem->classes_[mem_index].chars[left_pos_index] = current_mem->variables_.chars[rightPos % 4000];
      }
      if(area == 13){
        current_mem->classes_[mem_index].chars[left_pos_index] = constant_memory->chars[rightPos % 13000];
      }
      if(area == 23){
        current_mem->classes_[mem_index].chars[left_pos_index] = current_mem->temporals_.chars[rightPos % 23000];
      }
      if(area==43){
        current_mem->classes_[mem_index].chars[left_pos_index] = getCharFromPosition(current_mem->pointers_.chars[rightPos % 43000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        int right_pos_index = ((rightPos - 50000) % 10000) % 3000;
        current_mem->classes_[mem_index].chars[left_pos_index] = current_mem->classes_[right_mem_index].chars[right_pos_index];
      }
    } else {
     if(area == 4){
        current_class_mem->chars[(leftPos + 50000) % 4000] = current_mem->variables_.chars[rightPos % 4000];
      }
      if(area == 13){
        current_class_mem->chars[(leftPos + 50000) % 4000] = constant_memory->chars[rightPos % 13000];
      }
      if(area == 23){
        current_class_mem->chars[(leftPos + 50000) % 4000] = current_mem->temporals_.chars[rightPos % 23000];
      }
      if(area==43){
        current_class_mem->chars[(leftPos + 50000) % 4000] = getFloatFromPosition(current_mem->pointers_.chars[rightPos % 43000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_class_mem->chars[(leftPos + 50000) % 4000] = current_mem->classes_[right_mem_index].chars[rightPos % 4000];
      }
      if(area < 0){
        current_class_mem->chars[(leftPos + 50000) % 4000] = current_class_mem->chars[(rightPos + 50000) % 4000];
      }
    }
}

void Processor::setCharParamFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(area == 4){
      next_mem->variables_.chars[leftPos % 4000] = current_mem->variables_.chars[rightPos % 4000];
    }
    if(area == 13){
      next_mem->variables_.chars[leftPos % 4000] = constant_memory->chars[rightPos % 13000];
    }
    if(area == 23){
      next_mem->variables_.chars[leftPos % 4000] = current_mem->temporals_.chars[rightPos % 23000];
    }
    if(area == 43){
      next_mem->variables_.chars[leftPos % 4000] = getCharFromPosition(current_mem->pointers_.chars[rightPos % 43000]);
    }
    else {
      next_mem->variables_.chars[leftPos % 4000] = getCharFromPosition(rightPos);
    }
}

void Processor::setCharFromValue(int pos, char value){
  int area = pos / 1000;
  if(pos > 50000 ){
    int mem_index = (pos - 50000) / 50000;
    int int_index = (pos - 50000) % 50000 % 4000;
    current_mem->classes_[mem_index].chars[int_index] = value;
    return;
  }
  if(pos < 0){
    current_class_mem->chars[(pos + 50000) % 4000] = value;
  }
  if(area == 4){
    current_mem->variables_.chars[pos % 4000] = value;
  }   
  if(area == 23){
    current_mem->temporals_.chars[pos % 23000] = value;
  }
  if(area == 43){
    setCharFromValue(current_mem->pointers_.chars[pos % 43000], value);
  }
}

bool Processor::getBoolFromPosition(int position){
    int area = position / 1000;
    if(area == 5){
      return current_mem->variables_.booleans[position % 5000];
    }
    if(area == 14){
      return constant_memory->booleans[position % 14000];
    }
    if(area == 24){
      return current_mem->temporals_.booleans[position % 24000];
    }
    if(area == 44){
      return getBoolFromPosition(current_mem->pointers_.booleans[position % 44000]);
    }
    if(area > 50){
      int mem_index = (position - 50000) / 10000;
      int pos_index = ((position - 50000) % 10000) % 5000;
      return current_mem->classes_[mem_index].booleans[pos_index];
    }
    if(area < 0){
      return current_class_mem->booleans[(position + 50000) % 5000];
    }
    // None: Error
    return -1;
}

void Processor::setBoolFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(leftPos < 50000 && leftPos > 0){
      if(area == 5){
        current_mem->variables_.booleans[leftPos % 5000] = current_mem->variables_.booleans[rightPos % 5000];
      }
      if(area == 14){
        current_mem->variables_.booleans[leftPos % 5000] = constant_memory->booleans[rightPos % 14000];
      }
      if(area == 24){
        current_mem->variables_.booleans[leftPos % 5000] = current_mem->temporals_.booleans[rightPos % 24000];
      }
      if(area == 44){
        current_mem->variables_.booleans[leftPos % 5000] = getBoolFromPosition(current_mem->pointers_.booleans[rightPos % 44000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        int right_pos_index = ((rightPos - 50000) % 10000) % 5000;
        current_mem->variables_.booleans[leftPos % 5000] = current_mem->classes_[right_mem_index].booleans[right_pos_index];
      }
    } else if(leftPos > 50000) {
      int mem_index = (leftPos - 50000) / 10000;
      int left_pos_index = ((leftPos - 50000) % 10000) % 5000;
      if(area == 5){
        current_mem->classes_[mem_index].booleans[left_pos_index] = current_mem->variables_.booleans[rightPos % 5000];
      }
      if(area == 14){
        current_mem->classes_[mem_index].booleans[left_pos_index] = constant_memory->booleans[rightPos % 14000];
      }
      if(area == 24){
        current_mem->classes_[mem_index].booleans[left_pos_index] = current_mem->temporals_.booleans[rightPos % 24000];
      }
      if(area == 44){
        current_mem->classes_[mem_index].booleans[left_pos_index] = getBoolFromPosition(current_mem->pointers_.booleans[rightPos % 44000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        int right_pos_index = ((rightPos - 50000) % 10000) % 5000;
        current_mem->classes_[mem_index].booleans[left_pos_index] = current_mem->classes_[right_mem_index].booleans[right_pos_index];
      }
    } else {
     if(area == 5){
        current_class_mem->booleans[(leftPos + 50000) % 5000] = current_mem->variables_.booleans[rightPos % 5000];
      }
      if(area == 14){
        current_class_mem->booleans[(leftPos + 50000) % 5000] = constant_memory->booleans[rightPos % 14000];
      }
      if(area == 24){
        current_class_mem->booleans[(leftPos + 50000) % 5000] = current_mem->temporals_.booleans[rightPos % 24000];
      }
      if(area==44){
        current_class_mem->booleans[(leftPos + 50000) % 5000] = getFloatFromPosition(current_mem->pointers_.booleans[rightPos % 44000]);
      }
      if(area > 50){
        int right_mem_index = (rightPos - 50000) / 10000;
        current_class_mem->booleans[(leftPos + 50000) % 5000] = current_mem->classes_[right_mem_index].booleans[rightPos % 5000];
      }
      if(area < 0){
        current_class_mem->booleans[(leftPos + 50000) % 5000] = current_class_mem->booleans[(rightPos + 50000) % 5000];
      }
    }
}

void Processor::setBoolParamFromPosition(int leftPos, int rightPos){
    int area = rightPos / 1000;
    if(area == 5){
      next_mem->variables_.booleans[leftPos % 5000] = current_mem->variables_.booleans[rightPos % 5000];
    }
    if(area == 14){
      next_mem->variables_.booleans[leftPos % 5000] = constant_memory->booleans[rightPos % 14000];
    }
    if(area == 24){
      next_mem->variables_.booleans[leftPos % 5000] = current_mem->temporals_.booleans[rightPos % 24000];
    }
    if(area == 44){
      next_mem->variables_.booleans[leftPos % 5000] = getBoolFromPosition(current_mem->pointers_.booleans[rightPos % 44000]);
    }
    else {
      next_mem->variables_.booleans[leftPos % 5000] = getBoolFromPosition(rightPos);
    }
}

void Processor::setBoolFromValue(int pos, bool value){
    int area = pos / 1000;
  if(pos > 50000 ){
    int mem_index = (pos - 50000) / 50000;
    int int_index = (pos - 50000) % 50000 % 5000;
    current_mem->classes_[mem_index].booleans[int_index] = value;
    return;
  }
  if(pos < 0){
    current_class_mem->booleans[(pos + 50000) % 5000] = value;
  }
  if(area == 5){
    current_mem->variables_.booleans[pos % 5000] = value;
  }   
  if(area == 24){
    current_mem->temporals_.booleans[pos % 24000] = value;
  }
  if(area == 44){
    setBoolFromValue(current_mem->pointers_.booleans[pos % 44000], value);
  }
}

int Processor::getPointerFromPosition(int position) {
  int area = position / 1000, res;
  switch(area)
  {
    case 40:
      res = current_mem->pointers_.integers[position % 40000];
      break;
    case 41:
      res = current_mem->pointers_.floats[position % 41000];
      break;
    case 42:
      res = current_mem->pointers_.strings[position % 42000];
      break;
    case 43:
      res = current_mem->pointers_.chars[position % 43000];
      break;
    case 44:
      res = current_mem->pointers_.booleans[position % 44000];
      break;
  }
  return res;
}

void Processor::setPointerFromPosition(int position, int value) {
  int area = position / 1000;
  switch(area) {
    case 40:
      current_mem->pointers_.integers[position % 40000] = value;
      break;
    case 41:
      current_mem->pointers_.floats[position % 41000] = value;
      break;
    case 42:
      current_mem->pointers_.strings[position % 42000] = value;
      break;
    case 43:
      current_mem->pointers_.chars[position % 43000] = value;
      break;
    case 44:
      current_mem->pointers_.booleans[position % 44000] = value;
      break;
  }
}

#endif