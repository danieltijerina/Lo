#ifndef READER_H
#define READER_H

#include <iostream>
#include <fstream>
#include <unordered_map>
#include "Quads.h"

using std::string;

class Reader {
  public: 
    Reader(const std::string& filename){
      processFile(filename);
    }

    void processNextQuad(const QuadType& quad_type, std::ifstream& steam){

    }

    void processFile(const std::string& filename){
      std::unordered_map<string, QuadType> quad_type_ref({
        {"goSub", QuadType::goSub},
        {"ftag", QuadType::ftag},
        {"tag", QuadType::tag},
        {"era", QuadType::era},
        {"param", QuadType::param},
        {"print", QuadType::print},
        {"goto", QuadType::gotoUnc},
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

      string quad_type;
      quad_stream >> quad_type;
      while(quad_type != "$$$"){
        processNextQuad((quad_type_ref.find(quad_type))->second, quad_stream);
        quad_stream >> quad_type;
      }
    }
};

#endif // READER_H