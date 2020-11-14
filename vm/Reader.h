#ifndef READER_H
#define READER_H

#include <iostream>
#include <fstream>
#include <unordered_map>
#include "Quads.h"
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
      case goSub:
      case era:
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
                   std::unordered_map<string, FunctionDef>* function_def){
    std::unordered_map<string, QuadType> quad_type_ref({
      {"goSub", QuadType::goSub},
      {"ftag", QuadType::ftag},
      {"tag", QuadType::tag},
      {"era", QuadType::era},
      {"param", QuadType::param},
      {"print", QuadType::print},
      {"goto", QuadType::gotoUnc},
      {"gotoF", QuadType::gotoF},
      {"endFunc", QuadType::endFunc},
      {"=", QuadType::asign},
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
      std::cout << quad_type << std::endl;
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

      function_def->insert({{quad_type, func}});
      quad_stream >> quad_type;
    }
  }

}

#endif // READER_H