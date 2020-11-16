#ifndef FUNCTIONDEF_H
#define FUNCTIONDEF_H

struct FunctionDef {
  public:
    int intTy, intTmp;
    int stringTy, stringTmp;
    int charTy, charTmp;
    int floatTy, floatTmp;
    int boolTy, boolTmp;
    int classTy; //TODO: check how this works
    // return values
    int intRet;
    float floatRet;
    std::string stringRet;
    char charRet;
    bool boolRet;
};

#endif