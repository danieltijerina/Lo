#ifndef READER_H
#define READER_H

#include <iostream>
#include <fstream>
#include <unordered_map>
#include "Quads.h"
#include "Memory.h"
#include "FunctionDef.h"

using namespace Quads;
using std::string;

namespace Reader {

  Quad processNextQuad(const QuadType& quad_type, std::ifstream& stream, 
                        int index, std::unordered_map<string, int>* tags_,
                        std::unordered_map<string, std::vector<int>>* pending_tags_,
                        std::vector<Quad>* quads_){
    Quad current_quad;
    current_quad.type_ = quad_type;
    switch (quad_type)
    {
      case era:
      {
        stream >> current_quad.name_;
        break;
      }

      case gotoF:
      {
        std::string fname;
        stream >> current_quad.second_ >> fname;
        auto tags_it = tags_->find(fname);
        if(tags_it != tags_->end()){
          current_quad.first_ = tags_it->second;
        }else{
          auto ptags_it = pending_tags_->find(fname);
          if(ptags_it != pending_tags_->end()){
            ptags_it->second.push_back(index);
          }
          else{
            std::vector<int> pending_tag({index});
            pending_tags_ -> insert({{fname, pending_tag}});
          }
          current_quad.first_ = -1;
        }
        break;
      }

      case gotoUnc:
      case goSub:
      {
        std::string fname;
        stream >> fname;
        auto tags_it = tags_->find(fname);
        if(tags_it != tags_->end()){
          current_quad.first_ = tags_it->second;
        }else{
          auto ptags_it = pending_tags_->find(fname);
          if(ptags_it != pending_tags_->end()){
            ptags_it->second.push_back(index);
          }
          else{
            std::vector<int> pending_tag({index});
            pending_tags_ -> insert({{fname, pending_tag}});
          }
          current_quad.first_ = -1;
        }
        break;
      }

      case ftag:
      case tag:
      {
        std::string ftag_name;
        stream >> ftag_name;
        tags_->insert({{ftag_name, index}});

        auto ptags_it = pending_tags_->find(ftag_name);
        if(ptags_it != pending_tags_->end()){
          std::vector<int> *tags = &(ptags_it->second);
          for(auto elem : *tags){
            (*quads_)[elem].first_ = index;
          }
        }

        break;
      }

      case ret:
      {
        stream >> current_quad.first_;
        break;
      }

      case retVal:
      {
        stream >> current_quad.name_ >> current_quad.first_;
        break;
      }

      default:
        std::string temp1, temp2, temp3;
        stream >> temp1 >> temp2 >> temp3;
        current_quad.first_ = std::stoi(temp1);
        current_quad.second_ = std::stoi(temp2);
        current_quad.third_ = std::stoi(temp3);
        break;
    }
    return current_quad;
  }

  void processFile(const std::string& filename, std::vector<Quad>* quads_, 
                   std::unordered_map<string, FunctionDef>* function_def,
                   Memory* constant_mem){
    std::unordered_map<string, QuadType> quad_type_ref({
      {"goSub", QuadType::goSub},
      {"return", QuadType::ret},
      {"retVal", QuadType::retVal},
      {"ftag", QuadType::ftag},
      {"tag", QuadType::tag},
      {"era", QuadType::era},
      {"param", QuadType::param},
      {"print", QuadType::print},
      {"goto", QuadType::gotoUnc},
      {"gotoF", QuadType::gotoF},
      {"endFunc", QuadType::endFunc},
      {"val", QuadType::val},
      {"=", QuadType::assign},
      {"==", QuadType::equal},
      {"!=", QuadType::notEqual},
      {"<=", QuadType::lesserEqual},
      {"<", QuadType::lesser},
      {">=", QuadType::greaterEqual},
      {">", QuadType::greater},
      {"||", QuadType::orOp},
      {"&&", QuadType::andOp},
      {"+", QuadType::plus},
      {"-", QuadType::minus},
      {"/", QuadType::div},
      {"*", QuadType::mult}
    });

    std::ifstream quad_stream(filename);
    std::unordered_map<string, int> tags_;
    std::unordered_map<string, std::vector<int>> pending_tags_;

    string quad_type;
    quad_stream >> quad_type;
    while(quad_type != "$$$"){
      Quad quad = processNextQuad((quad_type_ref.find(quad_type))->second, 
          quad_stream, quads_->size(), &tags_, &pending_tags_, quads_);
      quads_->push_back(quad);
      quad_stream >> quad_type;
    }

    quad_stream >> quad_type;
    string trash; // TODO: Delete this (needed since the type e.g IntTy is printed in clo)
    while(quad_type != "$$$$"){
      FunctionDef func;
      quad_stream >> trash >> func.intTy;
      quad_stream >> trash >> func.floatTy;
      quad_stream >> trash >> func.charTy;
      quad_stream >> trash >> func.stringTy;
      quad_stream >> trash >> func.boolTy;
      quad_stream >> trash >> func.classTy;
      quad_stream >> trash >> func.intTmp;
      quad_stream >> trash >> func.floatTmp;
      quad_stream >> trash >> func.charTmp;
      quad_stream >> trash >> func.stringTmp;
      quad_stream >> trash >> func.boolTmp;
      quad_stream >> trash >> func.intPtr;
      quad_stream >> trash >> func.floatPtr;
      quad_stream >> trash >> func.charPtr;
      quad_stream >> trash >> func.stringPtr;
      quad_stream >> trash >> func.boolPtr;

      function_def->insert({{quad_type, func}});
      quad_stream >> quad_type;
    }
    int cst_amount;
    int cst_location;
    quad_stream >> trash >> cst_amount;
    constant_mem -> integers = new int[cst_amount];
    for(int i = 0; i < cst_amount; i++) {
      quad_stream >> cst_location;
      quad_stream >> constant_mem->integers[cst_location % 10000];
    }

    quad_stream >> trash >> cst_amount;
    constant_mem -> floats = new float[cst_amount];
    for(int i = 0; i < cst_amount; i++) {
      quad_stream >> cst_location;
      quad_stream >> constant_mem->floats[cst_location % 11000];
    }

    quad_stream >> trash >> cst_amount;
    constant_mem -> strings = new std::string[cst_amount];
    for(int i = 0; i < cst_amount; i++){
      quad_stream >> cst_location;
      string s;
      std::getline(quad_stream, s);
      constant_mem->strings[cst_location % 12000] = s.substr(2, s.length()-3);
    }

    quad_stream >> trash >> cst_amount;
    constant_mem -> chars = new char[cst_amount];
    for(int i = 0; i < cst_amount; i++){
      quad_stream >> cst_location;
      quad_stream >> constant_mem->chars[cst_location % 13000];
    }

    quad_stream >> trash >> cst_amount;
    constant_mem -> booleans = new bool[cst_amount];
    for(int i = 0; i < cst_amount; i++){
      quad_stream >> cst_location;
      std::string bool_string;
      quad_stream >> bool_string;
      constant_mem->booleans[cst_location % 14000] = bool_string == "true";
    }

  }

}

#endif // READER_H