#ifndef QUADS_H
#define QUADS_H

enum QuadType {
  goSub,
  ftag,
  tag,
  era,
  param,
  print,
  gotoUnc,
  gotoF,
  endFunc,
  asign,
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
  mult
};

struct Quad {
  QuadType type_;
  int first_, second_, third_; 
}

#endif 
