open Printf
open Ast

let rec printQuadList quads oc = 
  match quads with
  | [] -> ()
  | (f :: fs) -> 
      printQuad f oc;
      printQuadList fs oc;

and printQuad quad oc = 
  fprintf oc "%s %d %d %d\n" quad.operator quad.operand_left quad.operand_right quad.stored;
  