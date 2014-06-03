LOCAL currdbf,CURRSACDX,err_open_dbf,err
err=0 && Код Ошибки открытия 
err_open_dbf="" && признак успешности открытия файла
&&*** справочник МАТЕРИАЛОВ
SELECT 1
CURRSACDX=TMP+"IMATER.CDX"
currdbf=tputi+"MATER.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
    ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 1 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON NMAT TAG INMAT OF &CURRSACDX && ПОРЯДКОВЫЙ НОМЕР
    INDEX ON MAT TAG INAME OF &CURRSACDX ADDITIVE && ИМЯ МАТЕРИАЛА ПРОДУКТЫ
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 14  && описания типов меню. 
CURRSACDX=TMP+"menutype.cdx"
currdbf=tputi+"menutype.dbf"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
    ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 14 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON oid TAG IOID OF &CURRSACDX && ПОРЯДКОВЫЙ НОМЕР
    INDEX ON Descr TAG IDESCR OF &CURRSACDX ADDITIVE && ОПИСАНИЕ
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

IF .T.
SELECT 2 && ЕДИНИЦЫ ИЗМЕРЕНИЯ
CURRSACDX=TMP+"IMEDIZM.CDX"
currdbf=tputi+"MEDIZM.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 2 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON NEI TAG INEI OF &CURRSACDX 
    INDEX ON NAMEI TAG INAMEI OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 
ENDIF 

SELECT 3 && СПРАВОЧНИК БЛЮД (зАГОЛОВКИ)
CURRSACDX=TMP+"IMBLUD.CDX"
currdbf=tputi+"MBLUD.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 3 EXCLUSIVE 
  ON ERROR
  IF err=0
    IF FSIZE('PRLEN')=0
      ALTER TABLE &currdbf ADD PRLEN L
    ENDIF  
    INDEX ON NBLUD TAG INBL OF &CURRSACDX 
    INDEX ON BLUD TAG IBL OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 4 && СПРАВОЧНИК БЛЮД (СОДЕРЖИМОЕ)
CURRSACDX=TMP+"IMBLSOD.CDX"
currdbf=tputi+"MBLSOD.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 4 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON NBLUD TAG INBL OF &CURRSACDX 
    INDEX ON NMAT TAG INMAT OF &CURRSACDX ADDITIVE
    INDEX ON STR(NBLUD)+STR(NMAT) TAG INBLNMAT OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 5 && СПРАВОЧНИК МЕНЮ (ЗАГОЛОВКИ)
CURRSACDX=TMP+"IMMENUS.CDX"
currdbf=tputi+"MMENUS.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 5 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON NMEN TAG INMEN OF &CURRSACDX 
    INDEX ON MEN TAG IMEN OF &CURRSACDX ADDITIVE
    INDEX ON mtype_id TAG ImtypeId OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 6 && СПРАВОЧНИК МЕНЮ (СОДЕРЖИМОЕ)
CURRSACDX=TMP+"IMENSOD.CDX"
currdbf=tputi+"MENSOD.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 6 EXCLUSIVE 
  ON ERROR
  IF err=0
    IF FSIZE('NPOS')=0
      ALTER TABLE (currdbf) ADD COLUMN NPOS C(2)
      FLUSH 
    ENDIF 
    INDEX ON NMEN TAG INMEN OF &CURRSACDX 
    INDEX ON NBLUD TAG IBLUD OF &CURRSACDX ADDITIVE
    INDEX ON str(NMEN)+STR(PRMN)+NPOS TAG INMNPR OF &CURRSACDX ADDITIVE 
    INDEX ON str(NMEN)+STR(NBLUD) TAG INMNNBL OF &CURRSACDX ADDITIVE 
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 7 && СПРАВОЧНИК ПОСТАВЩИКОВ
CURRSACDX=TMP+"IMFIRM.CDX"
currdbf=tputi+"MFIRM.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 7 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON KARTA TAG IKARTA OF &CURRSACDX 
    INDEX ON FIRMA+IM+OT TAG IFIO OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 8 && ОБОРОТ МАТЕРИАЛОВ  приходные расходные списание  (ЗАГОЛОВКИ)
CURRSACDX=TMP+"IMOBOR.CDX"
currdbf=tputi+"MOBOR.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 8 EXCLUSIVE 
  ON ERROR
  IF err=0
    IF FSIZE('KOLCHEL')=0
      ALTER TABLE &currdbf ADD KOLCHEL C(34)
    ENDIF  
    IF FSIZE('CHELDOP')=0
      ALTER TABLE &currdbf ADD CHELDOP N(2)
    ENDIF  
    PACK IN 8
    INDEX ON KARTA TAG IKARTA OF &CURRSACDX 
    INDEX ON STR(PROP)+STR(NAKR) TAG IPRNAK OF &CURRSACDX ADDITIVE
    INDEX ON NAKR TAG INAK OF &CURRSACDX ADDITIVE
    INDEX ON NAIM TAG INAIM OF &CURRSACDX ADDITIVE
    INDEX ON DTOS(DATP)+STR(prop)+STR(nAKR) TAG IDPN OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 9 && ОБОРОТ МАТЕРИАЛОВ  приходные расходные списание  (СОДЕРЖИМОЕ)
CURRSACDX=TMP+"IMOBSOD.CDX"
&&	CURRSACDX1=TMP+"IMOBSOD1.CDX"
&&	CURRSACDX2=TMP+"IMOBSOD2.CDX"
&&	CURRSACDX3=TMP+"IMOBSOD3.CDX"
&&	CURRSACDX4=TMP+"IMOBSOD4.CDX"
&&	CURRSACDX5=TMP+"IMOBSOD5.CDX"
currdbf=tputi+"MOBSOD.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 9 EXCLUSIVE 
  ON ERROR
  IF err=0
    PACK IN 9
    INDEX ON KARTA TAG IKARTA OF &CURRSACDX 
&&	    INDEX ON STR(PROP)+STR(NAKR)+STR(KARTA) TAG IPRNAKKAR OF &CURRSACDX1 &&&CURRSACDX ADDITIVE
&&	    INDEX ON STR(NMAT)+STR(NAKR) TAG INMATNAKR OF &CURRSACDX2 &&&CURRSACDX ADDITIVE
&&	    INDEX ON STR(NMAT)+STR(CEN) TAG INMATCEN OF &CURRSACDX3 &&&CURRSACDX ADDITIVE
&&	    INDEX ON NAKR TAG INAK OF &CURRSACDX4 &&&CURRSACDX ADDITIVE
&&	    INDEX ON DTOS(DATP)+STR(prop)+STR(naKR) TAG IDPN OF &CURRSACDX5 &&&CURRSACDX ADDITIVE
&&	    SET INDEX TO &CURRSACDX,&CURRSACDX1,&CURRSACDX2,&CURRSACDX3,&CURRSACDX4,&CURRSACDX5
    INDEX ON STR(PROP)+STR(NAKR)+STR(KARTA) TAG IPRNAKKAR OF &CURRSACDX ADDITIVE
    INDEX ON STR(NMAT)+STR(NAKR) TAG INMATNAKR OF &CURRSACDX ADDITIVE
    INDEX ON STR(NMAT)+STR(CEN) TAG INMATCEN OF &CURRSACDX ADDITIVE
    INDEX ON NAKR TAG INAK OF &CURRSACDX ADDITIVE
    INDEX ON DTOS(DATP)+STR(prop)+STR(naKR) TAG IDPN OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 

SELECT 10 && Справочник диет 
CURRSACDX=TMP+"IMDIET.CDX"
currdbf=tputi+"MDIET.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 10 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON NDT TAG INDT OF &CURRSACDX 
    INDEX ON DT TAG IDT OF &CURRSACDX ADDITIVE
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 
SELECT 11 && рабочие меню-раскладки  
CURRSACDX=TMP+"MRASCL.CDX"
currdbf=tputi+"MRASCL.DBF"
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
    ON ERROR err=ERROR()
      ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 11 SHARED   &&EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON STR(NMEN)+DTOS(DATDOC) TAG IMENDAT OF &CURRSACDX 
    INDEX ON STR(NMEN)+STR(NBLUD)+STR(NMAT)+DTOS(DATDOC) TAG IMENBLMATD OF &CURRSACDX ADDITIVE  
    INDEX ON STR(NMEN)+STR(PRMN)+STR(NBLUD)+STR(NMAT) TAG IMENPRBLMT OF &CURRSACDX ADDITIVE
    INDEX ON STR(NMEN)+str(ABS(HOMO))+DTOS(DATDOC) TAG IMENHOMDAT OF &CURRSACDX ADDITIVE 
  &&  INDEX ON STR(NMEN)+DTOS(DATDOC) TAG IUNI_MN_DT OF &CURRSACDX UNIQUE ADDITIVE  
 &&   SET INDEX TO IMENBLMATD,IMENPRBLMT
    SET ORDER TO 1
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 
*DO msolvost && просчет остатков по приходу
SELECT 12 && Справочник ДЛИННЫХ ИМЕН ФАЙЛОВ
CURRSACDX=TMP+"IMBLIB.CDX"
currdbf=tputi+"MBLIB.DBF"
if .not.file(currdbf)
 ** создаем структуру таблицы в формате, понятном FOXPRO-2.XX (в целях совместимости)    
 ********************** не трогать!!! ПОУБИВАЮ !!!****************************************** 
 set prin to &currdbf
 set printer on
 SET DEVICE TO PRINT
 SET CONSOLE OFF
 @ 0,0 say chr(03)+chr(04)+chr(08)+chr(10) +chr(00)+chr(00)+chr(00)+chr(00);
          +chr(97)+chr(00)+chr(66)+chr(00) +chr(00)+chr(00)+chr(00)+chr(00);
          +chr(00)+chr(00)+chr(00)+chr(00) +chr(00)+chr(00)+chr(00)+chr(00);
          +chr(00)+chr(00)+chr(00)+chr(00) +chr(00)+chr(101)+chr(00)+chr(00)+'NBLUD';
                                                   +chr(00)+chr(00)+chr(00);
          +chr(00)+chr(00)+chr(00)+chr(78) +chr(01)+chr(00)+chr(00)+chr(00);
          +chr(05)+chr(00)+chr(00)+chr(00) +chr(00)+chr(00)+chr(00)+chr(00);
          +chr(00)+chr(00)+chr(00)+chr(00) +chr(00)+chr(00)+chr(00)+chr(00)+'BLUD';
                                           +chr(00)+chr(00)+chr(00)+chr(00);
          +chr(00)+chr(00)+chr(00)+chr(67) +chr(06)+chr(00)+chr(00)+chr(00);
          +chr(60)+chr(00)+chr(00)+chr(00) +chr(00)+chr(00)+chr(00)+chr(00);
          +chr(00)+chr(00)+chr(00)+chr(00) +chr(00)+chr(00)+chr(00)+chr(00)+chr(13)
 SET DEVICE TO SCREEN
 set printer off
 set printer to
 sET CONSOLE ON
 SET DEVICE TO SCREEN
 *******************************************************************
endif
IF FILE(currdbf) 
  IF FILE(CURRSACDX)
     ON ERROR err=ERROR()
       ERASE &CURRSACDX
    ON ERROR
  ENDIF
  ON ERROR err=ERROR()
    USE(currdbf) IN 12 EXCLUSIVE 
  ON ERROR
  IF err=0
    INDEX ON NBLUD TAG INDT OF &CURRSACDX 
  ELSE
    err_open_dbf=currdbf
  ENDIF
ELSE
  err_open_dbf=currdbf
ENDIF 
RETURN  err_open_dbf  
********************************************************************
