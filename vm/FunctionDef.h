#ifndef FUNCTIONDEF_H
#define FUNCTIONDEF_H

struct FunctionDef {
  public:
    int intTy, intTmp, intPtr;
    int stringTy, stringTmp, stringPtr;
    int charTy, charTmp, charPtr;
    int floatTy, floatTmp, floatPtr;
    int boolTy, boolTmp, boolPtr;
    int classTy; //TODO: check how this works
    // return values
    int intRet;
    float floatRet;
    std::string stringRet;
    char charRet;
    bool boolRet;
};

#endif