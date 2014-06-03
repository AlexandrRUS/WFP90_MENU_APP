PARAMETERS PrI_O,RegKey1,RegKeyVal1
* чтение или запись настроек программы в реестр Windows 98 
* RegKey - имя ключа в реестре
* PrI_O - признак - ключ .F. - считать ключ .T. - записать
*  RegKey - имя строкового параметра
* nErrNum - код ошибки при выполнении операции 
*pschr="pschr"
*? reg_set(.f.,pschr,"") <<--------- example
*? pschr

#DEFINE regclass     "registry"
#DEFINE HKEY_CURRENT_USER   -2147483647  && BITSET(0,31)+1
#DEFINE  PROG_KEYSET   "Software\Microsoft\VisualFoxPro\MENU_OOO_MB"
#DEFINE PROG_CLASSLIB  "registry.vcx"
LOCAL ReSplash,nErrNum,cValue,nErrNum,cKeyN,cVer_ID,rparam
rparam=PARAMETERS()
ReSplash = .NULL.
nErrNum=0
#IFDEF regclass
IF NOT EMPTY(regclass)
  ReSplash = NewOBJECT(regclass,PROG_CLASSLIB)
  iF VARTYPE(ReSplash) = "O"
    IF rparam>1
      IF TYPE("RegKeyVal1")="C"
        cValue=RegKeyVal1
      ELSE
        IF TYPE("RegKeyVal1")="N"
           cValue=str(RegKeyVal1)
        ELSE
           nErrNum = -2 
           RETURN nErrNum 
        ENDIF    
      ENDIF
      cKeyN=RegKey1
    ELSE
       nErrNum = -1 
       RETURN nErrNum 
    ENDIF
  ELSE
    nErrNum = -3 
    RETURN nErrNum 
  ENDIF
 * SET STEP ON 
  IF .NOT.PrI_O
    nErrNum=ReSplash.GetRegKey(cKeyN,@cValue,PROG_KEYSET,HKEY_CURRENT_USER)
    &&    nErrNum=ReSplash.GetRegKey("Ver_id",@cValue,"Software\Microsoft\VisualFoxPro\BANK_OOO_MB",HKEY_CURRENT_USER)
  ELSE
    nErrNum=ReSplash.SetRegKey(cKeyN,@cValue,PROG_KEYSET,HKEY_CURRENT_USER)
  ENDIF 
  IF nErrNum#0
    RETURN nErrNum
  ELSE
    RegKeyVal1=cValue
    &RegKey1=RegKeyVal1
  ENDIF
ENDIF
#ENDIF
 IF VARTYPE(ReSplash) = "O"
    RELEASE ReSplash 
    ReSplash = .NULL.
 ENDIF
RELEASE ReSplash
RELEASE  ReSplash,cValue,cKeyN,cVer_ID
RETURN   nErrNum