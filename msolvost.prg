*** MSOLVOST.PRG
*** просчет остатков продуктов на складе
RETURN
LOCAL CurNmat,CurMaxNmat
replace kol WITH 0 FOR .t. in 1 &&  обнуляем остатки в справочнике 
GO TOP IN 9
DO WHILE .not.eof(9)
  CurNmat=MOBSOD.NMAT && НОМЕР МАТЕРИАЛА
  IF .NOT.SEEK(CurNmat,1,1) 
    INSERT INTO mater (nmat,mat,kfper,kol) VALUES (CurNmat,"Материал № "+ALLTRIM(STR(CurNmat)),1,IIF(MOBSOD.prop=0,ROUND(MOBSOD.kol*MOBSOD.KFPER,2),ROUND(-MOBSOD.kol*MOBSOD.KFPER,2)))     
    =SEEK(CurNmat,1,1) && ищем повторно
  ELSE 
    replace kol WITH IIF(MOBSOD.prop=0,Mater.kol+ROUND(MOBSOD.kol*MOBSOD.KFPER,2),Mater.kol-ROUND(MOBSOD.kol*MOBSOD.KFPER,2)) IN 1
  ENDIF 
  SKIP IN 9 
ENDDO 
*** Просчет средней цены
SET ORDER TO 4  IN 9
GO TOP IN 9
LOCAL CurSt,CurKol,CurSrCen
DO WHILE .NOT.EOF(9)
    CurNmat=MOBSOD.NMAT
    *CurKol=MOBSOD.NMAT
    STORE 0 TO  CurSt,CurKol
    SCAN WHILE (.not.eof(9)).and.(CurNmat=MOBSOD.NMAT)
       IF MOBSOD.prop=0
         CurKol=CurKol+MOBSOD.kol*MOBSOD.KFPER
         CurSt=CurSt+MOBSOD.kol*MOBSOD.KFPER*MOBSOD.CEN
       ENDIF 
    ENDSCAN 
    IF CurKol>0
      CurSrCen=ROUND(CurSt/CurKol,2)
    ELSE
      CurSrCen=0
    ENDIF 
    IF SEEK(CurNmat,1,1)
      IF MATER.PRCEN=1
        replace CenSr WITH CurSrCen,CEN WITH CurSrCen IN 1 
      ELSE
        replace CenSr WITH CurSrCen IN 1 
      ENDIF  
    ENDIF 
ENDDO 