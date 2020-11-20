#ifndef QUADS_H
#define QUADS_H

namespace Quads {

  enum QuadType {
    goSub = 1,
    ret,
    retVal,
    ftag,
    tag,
    era,
    param,
    print,
    gotoUnc,
    gotoF,
    endFunc,
    assign,
    equal,
    notEqual,
    greater,
    greaterEqual,
    lesser,
    lesserEqual,
    orOp,
    andOp,
    plus,
    minus,
    div,
    mult,
    val,
    classMark,
    classInit,
    read
  };

  struct Quad {
    public:
      QuadType type_;
      int first_, second_, third_;
      std::string name_;
  };

}

#endif 
