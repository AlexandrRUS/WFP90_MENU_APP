PARAMETERS cURnOMERnAKR
CLOSE DATABASES ALL 
SET CPDIALOG ON
SET STEP off
SET BELL off
SET CENTURY ON
SET DATE BRITISH
SET TALK OFF
SET CONSOLE OFF
SET ECHO OFF
SET ESCAPE OFF
SET EXCLUSIVE ON
SET HOURS TO 24
SET NEAR OFF
SET STATUS OFF
SET DELETED ON
SET CONSOLE OFF 
SET EXCLUSIVE ON
SET SAFETY OFF 
SET CENTURY ON 
****************************************
* ���� * 
PUBLIC TMP,TPUTI,TPUTA,TPUTR,TPUTdoc
TPUTI=SYS(5)+CURDIR()+"DBF\"
TPUTA=SYS(5)+CURDIR()+"ARHIV\"
TPUTR=SYS(5)+CURDIR()+"RC\"
TPUTdoc=SYS(5)+CURDIR()+"doc\"
TMP=SYS(2023)+"\"
TDEFLTPATH=.T.  && ������������ ���� �� ���������
PUBLIC RDateMenu,TmpSel0
RDateMenu=CTOD("")
TmpSel0=0
*******��������� �� ���������
=NUMLOCK(.T.) 
=CAPSLOCK(.T.)
****************************************
PUBLIC RTprnDEf,TprnDEf,TNomPrnDef
TprnDEf="" && ��� �������� ��� ������ �� ���������
TNomPrnDef=0 && ����� �������� ��� ������ �� ���������
****** ���������
PUBLIC RVERS,RTITL,RVnorm,RDnorm,ROutDate,RInDate,TFIRMA 
RVERS="00.00.03"
RTITL="M���  "+RVERS+"  (C) 2004-14" 
PUBLIC RVERS,RTITL,RVnorm,RVnorm,RInDate,ROutDate,RNormKal,RNormBel,RNormJir,RNormUgl
PUBLIC RDateMenu
PUBLIC TREGV
TREGV="" && �������������������� ������
RDateMenu=CTOD("")
RVnorm=100.00 &&  ����� ��������� �������� ���� ������ ��������� ��������
RDnorm=60.00 &&  ����� ��������� �������� ���� ������ ������� ��������
RNormBel=130.0 && ����� �������� ������
RNormJir=130.0 && ����� �������� �����
RNormUgl=500.0 && ����� �������� ���������
RNormKal=3609.0 && ����/��� �������������� ������ 
************** REKVIZITY
TFIRMA="��� �������"

PUBLIC TRuk,TZKlad,TKlad,TBuh,TDVr,TDSt,TPov                           && ���. ����
PUBLIC TDolRuk,TDolZKlad,TDolKlad,TDolBuh,TDolDVr,TDolDSt,TDolPov      && ����. ���. ���. 
STORE SPACE(0) TO TRuk,TZKlad,TKlad,TBuh,TDVr,TDSt,TPov 
STORE SPACE(0) TO TDolRuk,TDolZKlad,TDolKlad,TDolBuh,TDolDVr,TDolDSt,TDolPov
*RRECV1="��� ������� ��� 640000000000"
*RRECV2="                            "
*RRECV3="                            "


PUBLIC rmenutitle 

rmenutitle = "����"

************* KEYS !
PUBLIC ON_EXIT,TFASTEXIT,CURRSEL,RDatDoc,RKolChel,RNomDok,TLastTreb
RKolChel=78901  
RDatDoc=CTOD("31.12.2004")
RNomDok=999
*CURRSEL="0" && ������� ������� �������
TFASTEXIT=.F.  && .T. - ��������� ������� ����� �� ��������� (��� �������)
ON_EXIT=.F.   && ����� �� ���������
TLastTreb=0 && ��������� ����������, � ������� ��������/�������������/����������� 
********************
** ��������� ������ ��� ������ � ��������
PUBLIC _greg
&&_progrootregdir='SOFTWARE\MENU_APP'
_greg=CREATEOBJECT('CRegistry') 
*_SCREEN.AddObject("_greg",'CRegistry')

********************
DO mdbf 
IF TYPE("cURnOMERnAKR")<>"N"
  cURnOMERnAKR=0
ENDIF 
*DO FORM c:\foxpro\menu\mredmenu.scx WITH cURnOMERnAKR,.F.,.T.,0,.F.
CurReadOnly=.f. 
CurRedMode=.T.
cURnOMERnAKR=14
CurNomMenu=0
CurNewRaskl=.t.
*DO FORM c:\foxpro\menu\mredmenu.scx WITH cURnOMERnAKR,.t.,CurRedMode,0,.t. && �������� ������� ���� CurRedMode=.f.
*DO FORM c:\foxpro\menu\mredmenu.scx WITH cURnOMERnAKR,.f.,CurRedMode,0,.t. && �������������� ������� ���� CurRedMode=.f.
*DO FORM c:\foxpro\menu\mredmenu.scx WITH 0,.f.,CurRedMode,0,.t. && �������� NEW ���� CurRedMode=.f.
*DO FORM c:\foxpro\menu\mredmenu.scx WITH 0,.f.,CurRedMode,0,.t. && �������� ������� ���� CurRedMode=.f.
*DO FORM c:\foxpro\menu\mredmenu.scx WITH 43,.f.,CurRedMode,0,.t. && �������� NEW ����-���������  CurRedMode=.t.

*DO FORM c:\foxpro\menu\mredmenu.scx WITH 1,.f.,CurRedMode,1,.T. && �������� NEW ����-���������  CurRedMode=.t.

* thisform.Init(CurNomMenu,.F.,THISFORM.OnFullRed,CurNomMenu,.T.) 
**PARAMETERS CurNakr,CurViewMode,CurRedMode,CurNomMenu,CurNewRaskl
** ��������� ***********************************
*CurNakr && ����� ��������� (���������) 
*CurViewMode  && ������������� ������� CurViewMode=.F. - �������������e .T. - ������ ��������
*CurRedMode && ������������� ������� ��������������/���������  
*CurNomMenu && ����� ���� ��� ����� ������ ���������� ��� ������������ ������ 
*CurNewRaskl .T. - ��������� ����� ��������� �� ������ ����������� ���� (��� ������ CurNakr ���������� ��� ����� ����) 
*            .F. - ����������� ��� ������������ ��������� ( CurNakr ���������� ��� ����� ���������� ����������)
*SET STEP ON 

*DO Form PROBA2

*_SCREEN.RemoveObject("_greg")  && ���������� ������ 
RELEASE _greg
return
*****************************************************************************
*
*������������
*
*****************************************************************************
FUNCTION GetIniParam
     * ��������� �������� �� INI *
	PARAMETER ParamCtr
	LOCAL CFname,fd,parRawStr
	REtVal = .f.
	CFname = 'menu.ini'
	IF FILE(CFname) AND .NOT.EMPTY(ParamCtr) 
		fd = FOPEN(CFname,10)	
		IF fd > 0
		DO WHILE .t. &&lExpression 
			parRawStr =	FGETS(fd)
			**--? parRawStr
			IF LEFT(parRawStr,LEN(ParamCtr))=ParamCtr
				REtVal = RIGHT(parRawStr,LEN(parRawStr)-1-LEN(ParamCtr))
				EXIT
			ENDIF	
   			IF FEOF(fd)
 				EXIT
 			ENDIF	
		ENDDO
		= FCLOSE(fd)
		ENDIF
	ENDIF
RETURN REtVal

FUNCTION SetIniParam
     * ������ ��������� � INI *
	PARAMETER ParamCtr,ParamValue
	LOCAL CFname,fd,parRawStr
	REtVal = .f.
	CFname = 'menu.ini'
	IF .NOT.EMPTY(ParamCtr) 
	
		IF NOT FILE(CFname)  
			=FCLOSE(FCREATE(CFname))
		ENDIF
		fd = FOPEN(CFname,12)
		IF fd > 0
		pnSize = .F.
		ParamCtr = ParamCtr + '='
		DO WHILE .t. &&lExpression 
			parRawStr =	FGETS(fd)
			*? parRawStr
			IF parRawStr = ParamCtr
				=FSEEK(fd,-LEN(parRawStr)-2,1)
				IF LEN(parRawStr)<LEN(ParamCtr+ParamValue)
					=FPUTS(fd,SPACE(LEN(parRawStr))) 
					=FSEEK(fd,0,2)
					=FPUTS(fd, CHR(13)+CHR(10))
					parRawStr = ParamCtr + ParamValue  
				ENDIF 
				parRawStr = PADR(ParamCtr+ParamValue,LEN(parRawStr)," ") 
   				=FPUTS(fd, parRawStr)
				EXIT
			ENDIF	
   			IF FEOF(fd)
 				=FPUTS(fd, CHR(13)+CHR(10))
 				=FPUTS(fd, ParamCtr+ParamValue)
 				EXIT
 			ENDIF	
		ENDDO
		=FFLUSH(fd,.t.)
		REtVal= FCLOSE(fd)
		ENDIF
	ENDIF
RETURN REtVal


*
PROCEDURE CloseTmpWorkArea
	PARAMETER NumWorkAreaTmpTable 
	** ��������� ��������� ��������� �������, ����������� � ������� ������ NumWorkAreaTmpTable
	PRIVATE CharVar1,CharVar2
	IF NOT EMPTY(NumWorkAreaTmpTable) AND USED(NumWorkAreaTmpTable)
  		CharVar1= ALIAS(NumWorkAreaTmpTable)
  		USE IN (CharVar1)
  		CharVar2=TMP+CharVar1+".cdx"
  		ERASE &CharVar2 && ������� ������
  		CharVar1=TMP+CharVar1+".tmp"
  		ERASE &CharVar1 && ������� �������
	ENDIF 
ENDPROC
*
PROCEDURE SetFileAttrib
  PARAMETER psfilename, piattrib
  * SetFileAttrib -��������� ��������� ����� 
  * psfilename-������ ���� � �����+��� ����� (����.)
  * piattrib -�������� (�����.) 
  DECLARE INTEGER SetFileAttributes IN WIN32API STRING, INTEGER
  setfileattributes(psfilename,piattrib)
  CLEAR DLLS  'SETFILEATTRIBUTES'
  RETURN
ENDPROC
*
FUNCTION IsPathWriteable
  PARAMETER pspath
  PRIVATE lbflag, lserror, lhfile, lstempdir
  IF RIGHT(pspath, 1) <> '\'
     pspath = pspath + '\empty.txt'
  ELSE
     pspath = pspath + 'empty.txt'
  ENDIF
  FCLOSE(FCREATE(pspath))
  IF FILE(pspath)
     DELETE FILE (pspath)
     lbflag = .T.
  ELSE
     lbflag = .F.
  ENDIF
  RETURN lbflag
ENDFUNC
*
PROCEDURE ClearDir
  PARAMETER pspath, psdelroot
  * ClearDir - ������� ���������� PARAMETER pspath (��� �������� (�)), psdelroot (������� .T. - ������� �������.)
  PRIVATE li, lidirscnt, ls, lsdelfilename, ladirs
  DIMENSION ladirs[50, 5]
  IF DIRECTORY(pspath)
     lidirscnt = ADIR(ladirs,pspath + '*.','D')
     FOR li = 1 TO lidirscnt
          IF  .NOT.INLIST(ladirs(li,1),'.', '..')
               cleardir(pspath + ladirs(li,1) + '\',.T.)
          ENDIF
     ENDFOR
     ls = SYS(2000, pspath + '*.*')
     DO WHILE  .NOT. EMPTY(ls)
          lsdelfilename = pspath + ls
          setfileattrib(lsdelfilename,32)
          DELETE FILE (lsdelfilename)
          ls = SYS(2000, pspath + '*.*', 1)
     ENDDO
     IF psdelroot
       RD (pspath)
     ENDIF
  ENDIF
  RETURN
ENDPROC
*
****
FUNCTION SolvSumDok
  PARAMETERS CurNakr
  LOCAL curtag,cursum
  cursum=0
  IF PARAMETERS()>0.and.type("CurNakr")="N"
    SET ORDER TO 5 IN 9  && NAKR
    IF SEEK(CurNakr,9,5)
      DO WHILE (.not.eof(9))
        IF mobsod.nakr=CurNakr
          cursum=cursum+ROUND(mobsod.kol*mobsod.cen,2)
        ELSE 
          exit
        ENDIF 
        SKIP IN 9
      ENDDO   
    ENDIF 
  ENDIF 
RETURN cursum
 
FUNCTION GETVREM
  PARAMETERS CurSel
  LOCAL curtag,CurNakr1,CurSel1
  && �������� CurNakr ������ ��������� ����� ��������� ������� �������
  IF PARAMETERS()>0
    CurSel1=STR(CurSel)
    SELECT &CurSel1
   * SKIP -1
   * IF BOF()
      CurNakr1=prmn
   * ELSE
   *   SKIP
   *   SKIP
      CurNakr1=prmn
   *   SKIP -1  
   * ENDIF 
  ELSE
    RETURN ""
  ENDIF 
  && �� ���������� ��� ������ ���� 
  * � ���������� ������ "�������,����,�������,����,II ����,II ����,������.,"
  DO CASE
  case CurNakr1=1
    curtag="�������"
  CASE CurNakr1=2
    curtag="����"
  CASE CurNakr1=3
    curtag="�������"
  CASE CurNakr1=4
    curtag="����"
  CASE CurNakr1=5
    curtag="II ����"
  CASE CurNakr1=6
    curtag="II ����"
  OTHERWISE
    curtag=""
  ENDCASE 
RETURN curtag

FUNCTION KKPC
PARAMETERS P,nLenStr,lPrEdRu 
* ������� ����������� ����� � ������������� ���
* ���������� ��������� ������ ������
* P - ����� ������
* lPrEdRu - ���� .F. - "�������"  .T. - "�����"
* nLenStr - �������� ����� ������
LOCAL  M11,M12,M13,M14,M15,M16,M17,M18,M19,M21,M22,M23,M24,M25,M26,M27,M28,M29,M31
LOCAL M32,M33,M34,M35,M36,M37,M38,M39,M41,M42,M43,M44,M45,M46,M47,M48,M49,M51,M52
LOCAL M53,M54,M55,M56,M57,M58,V11,V12,V13,V22,V21,V23,V31,V32,V33,V41,V42,V43,V51
LOCAL V52,V53,V50,V61,V62,V63,V70,V50,M30
LOCAL StrPropis
StrPropis="" && ������ ��������
IF TYPE("nLenStr")#"N"
  nLenStr=500
ENDIF
IF TYPE("lPrEdRu")#"L"
  lPrEdRu=.F.
ENDIF

M11="��� "
M12="������ "
M13="������ "
M14="��������� "
M15="������� "
M16="�������� "
M17="������� "
M18="��������� "
M19="��������� "
M21="������ "
M22="�������� "
M23="�������� "
M24="����� "
M25="��������� "
M26="���������� "
M27="��������� "
M28="����������� "
M29="��������� "
M30="���� "
M31="���� "
M32="��� "
M33="��� "
M34="������ "
M35="���� "
M36="����� "
M37="���� "
M38="������ "
M39="������ "
M41="����������� "
M42="���������� "
M43="���������� "
M44="������������ "
M45="���������� "
M46="����������� "
M47="���������� "
M48="������������ "
M49="������������ "
M51="���� "
M52="��� "
M53=""
M54=""
M55=""
M56="����� "
M57=""
M58=""
M59=""
V11="�������� "
V12="��������� "
V13="���������� "
V22="��������� "
V21="�������� "
V23="���������� "
V31="������� "
V32="�������� "
V33="��������� "
V41="������ "
V42="������ "
V43="����� "
V50="� "
IF lPrEdRu
  V51="����� "
  V52="����� "
  V53="������ "

  V61="������� "
  V62="������� "
  V63="������ "
  
  V70="" 
ELSE 
  V51="������� "
  V52="������� "
  V53="������ "

  V61="����� "
  V62="����� "
  V63="����� "
  
  V70="����� "
ENDIF 

***************
LOCAL  P1,P2,P3,P4,P5,S,PS
S=''
IF p>=0
  PS=0
ELSE
  PS=1
  P=ABS(P)
ENDIF 
STORE 0 TO P1,P2,P3,P4,P5,P6
P1=INT(P/10**12)
P2=INT(MOD(P,10**12)/10**9)
P3=INT(MOD(P,10**9)/10**6)
P4=INT(MOD(P,10**6)/10**3)
P5=INT(MOD(P,10**3))
I=5

DO WHILE I>0
  PI='P'+STR(I,1)
  IF &PI#0
    S1=INT(&PI/100)
    S2=INT((&PI-S1*100)/10)
    S3=INT(MOD(&PI,10))
    SS=SPACE(0)
    IF S3#0
      IF S2=1
        SM='M4'+STR(S3,1)
      ELSE
        IF lPrEdRu
          IF I=4.AND.S3<3
            SM='M5'+STR(S3,1)
          ELSE
            SM='M3'+STR(S3,1)
          ENDIF
        ELSE
          IF (I=4.AND.S3<3).or.(I=5.AND.S3<3)
            SM='M5'+STR(S3,1)
          ELSE
            SM='M3'+STR(S3,1)
          ENDIF
        ENDIF 
      ENDIF
      SS=&SM
    ENDIF
    IF S2=1.AND.S3=0
      SS=M21+SS
    ENDIF
    IF S2#0.AND.S2#1
      SM='M2'+STR(S2,1)
      SS=&SM+SS
    ENDIF
    IF S1#0
      SM='M1'+STR(S1,1)
      SS=&SM+SS
    ENDIF
    DO CASE
    CASE S3=1.and.s2#1
      SM='V'+STR(I,1)+'1' 
    CASE (S3>4).OR.(S2=1).or.(s3=0.and.s2=0).or.(s3=0)
      SM='V'+STR(I,1)+'3'
    OTHERWISE
      SM='V'+STR(I,1)+'2'
    ENDCASE
    S=SS+&SM+S
  ENDIF
  I=I-1
ENDDO
IF P5=0
 IF lPrEdRu
  S=S+' ������' && ����� ����� ������
 ELSE 
  S=S+' ������' && ����� ����� ������
 ENDIF 
ENDIF
** ����� ��� �������
IF INT(P)=0 
  S=M30+V53 && ����� 
ENDIF 
P6=int(mod(P,1)*100)
IF P6>0
  i=6
  S1=INT(P6/100) && ������ 0 � ������ ������ (�����)
  S2=INT((P6-S1*100)/10) && �������
  S3=INT(MOD(P6,10)) && �������
  SS=SPACE(0)
  IF S3#0 
    IF S2=1
      SM='M4'+STR(S3,1) && 11 12 ... 19
    ELSE
      IF S3<3
        SM='M5'+STR(S3,1)
      ELSE
        SM='M3'+STR(S3,1)
      ENDIF
    ENDIF
    SS=&SM
  ENDIF
  IF S2=1.AND.S3=0
    SS=M21+SS
  ENDIF
  IF S2#0.AND.S2#1
    SM='M2'+STR(S2,1)
    SS=&SM+SS
  ENDIF
    DO CASE
    CASE S3=1.and.s2#1
      SM='V'+STR(I,1)+'1' 
    CASE (S3>4).OR.(S2=1).or.(s3=0.and.s2=0).or.(s3=0)
      SM='V'+STR(I,1)+'3'
    OTHERWISE
      SM='V'+STR(I,1)+'2'
    ENDCASE
    S=S+V50+SS+&SM
ENDIF
IF ps>0
  S=V70+S
ENDIF 

NJ=1
NI=1
SS=S && ����� ����� �������
StrPropis=S
n=nLenStr && ����� �����
STORE SPACE(N) TO S1,S2,S3,S4,S5
DO WHILE AT(' ',SS)#0
  ni=1
  SSS=SS
  DO WHILE NI+AT(' ',SS)<=N.AND.AT(' ',SS)#0
    NI=NI+AT(' ',SS)
    SS=RIGHT(SS,LEN(SS)-AT(' ',SS))
  ENDDO
  SJ='S'+STR(NJ,1)
  IF AT(' ',SS)=0
    &SJ=SSS
    IF LEN(&SJ)<N
      &SJ=&SJ+SPACE(N-LEN(&SJ))
    ENDIF
    EXIT
  ENDIF
  &SJ=LEFT(SSS,NI-1)
  SS=RIGHT(SSS,LEN(SSS)-NI+1)
  IF LEN(&SJ)<N
    &SJ=&SJ+SPACE(N-LEN(&SJ))
  ENDIF
  SSS=SS
  NJ=NJ+1
ENDDO
SS1=ASC(left(S1,1))
SSS1=0
IF CHR(241)=="�" && ���������� ���������
  DO CASE    && ��� ��������� MS DOS - 866
  CASE SS1<=175
    SSS1=SS1-32
  CASE SS1=241
    SSS1=240
  CASE SS1>=224.AND.SS1<=240
    SSS1=SS1-80
  ENDCASE
else
  DO CASE    && ��� ��������� ANSI - 1251 (Win)
  CASE SS1>=224
    SSS1=SS1-32
  CASE SS1=184
    SSS1=168
  otherwise
    SSS1=SS1
  ENDCASE
ENDIF 
S1=CHR(SSS1)+RIGHT(S1,LEN(S1)-1)
StrPropis=ALLTRIM(StrPropis)
StrPropis=CHR(SSS1)+RIGHT(StrPropis,LEN(StrPropis)-1)
RETURN StrPropis
***********************************************************************
** ������ ������������ ���������� 
***********************************************************************

DEFINE CLASS CBB AS COMBOBOX
  IncrementalSearch = .T. 
  RowSourceType =  1 
  RowSource ="������,������,������,�����.,      "
  Style= 2
  BorderStyle= 0
  PROCEDURE InteractiveChange
  	thisform._changed = .T. && �������������
  ENDPROC  
  PROCEDURE LostFocus
	* ����� ������������ ��� ����� ���������� 
	* � ����� �����������, ���� � �����
	* ������� �������� ���� �������� �����
    LOCAL CurTmpVar,CurNBl,CurPrMn,CurNBl1,CurPrMn1,CurPrNBl
    
  	IF NOT EMPTY(thisform._wa_menu)
    	CurNBl=ALIAS(thisform._wa_menu)+".NBLUD"
    	CurNBl=&CurNBl
    	CurPrMn=ALIAS(thisform._wa_menu)+".PRMN" && ������� ������� 
    	CurPrMn=&CurPrMn
    	IF CurPrMn <> THIS.ListIndex
        	** �������� �� ������� ������� �������
        	GO TOP IN thisform._wa_detail
        	CurPrNBl=.F.
        	DO WHILE .NOT.EOF(thisform._wa_detail)
        		CurNBl1 = ALIAS(thisform._wa_detail)+".NBLUD"
        		CurNBl1 = &CurNBl1
        		CurPrMn1 = ALIAS(thisform._wa_detail)+".PRMN" && ������� ������� 
        		CurPrMn1 = &CurPrMn1
          		IF CurNBl1 = CurNBl AND CurPrMn1 = THIS.ListIndex
            		CurPrNBl=.T.
            		EXIT
          		ENDIF   
          		SKIP IN thisform._wa_detail
        	ENDDO
        	GO TOP IN thisform._wa_detail
        	IF CurPrNBl 
        		THIS.ListIndex = CurPrMn
        	ELSE
				REPLACE PRMN WITH THIS.ListIndex FOR (PRMN = CurPrMn OR EMPTY(PRMN)) AND NBLUD = CurNBl IN thisform._wa_detail
				REPLACE PRMN WITH THIS.ListIndex IN thisform._wa_menu
        		thisform.grSpMatDetail.Refresh
        	ENDIF
    	ENDIF 
  	ENDIF 
  ENDPROC
  PROCEDURE Destroy
   	CLEAR EVENTS
  ENDPROC
  PROCEDURE init 
   	READ EVENTS
  ENDPROC  
ENDDEFINE


DEFINE CLASS CBOTime AS COMBOBOX && FORM
  IncrementalSearch = .T. 
  RowSourceType =  1 
  RowSource ="�������,����,�������,����,II ����,II ����,������.,"
  Style= 2
  BorderStyle= 0
  PROCEDURE LostFocus
	* ����� ������������ ��� ����� ������� 
	* �������� � ����� ����-���������, ���� � �����
	* ������� ���� ��� �� �������� �����
	* ��������� ������ 20.04.2004
    LOCAL CurTmpVar,CurNBl,CurPrMn,CurNBl1,CurPrMn1,CurPrNBl
    IF NOT EMPTY(thisform.onmensel)
      CurNBl=ALIAS(thisform.onmensel)+".NBL"
      CurNBl=&CurNBl
      CurPrMn=ALIAS(thisform.onmensel)+".PrMn" && ������� ������� 
      CurPrMn=&CurPrMn
      IF CurPrMn<>THIS.ListIndex
        ** �������� �� ������� ������� �������
        GO TOP IN thisform.onsel
        CurPrNBl=.F.
        DO WHILE .NOT.EOF(thisform.onsel)
          CurNBl1=ALIAS(thisform.onsel)+".NBLUD"
          CurNBl1=&CurNBl1
          CurPrMn1=ALIAS(thisform.onsel)+".PrMn" && ������� ������� 
          CurPrMn1=&CurPrMn1
          IF CurNBl1=CurNBl.AND.CurPrMn1=THIS.ListIndex
            CurPrNBl=.T.
            EXIT
          ENDIF   
          SKIP IN thisform.onsel
        ENDDO
        GO TOP IN thisform.onsel
        IF CurPrNBl 
          THIS.ListIndex=CurPrMn
          THISFORM.LBLINFO.CAPTION="��������..."
        ELSE
    	  THISFORM.LBLINFO.CAPTION=""
    	  replace PrMn WITH THIS.ListIndex FOR PrMn=CurPrMn.and.nblud=CurNBl IN thisform.onsel
          Replace PrMn WITH THIS.ListIndex IN thisform.onmensel
          thisform.oneditmen=.t.  && ���� �������������� menu .T.- ������ ���������
          THISFORM.GRid1.Refresh 
        ENDIF
      ENDIF 
    ENDIF 
  ENDPROC
  PROCEDURE Destroy
    CLEAR EVENTS
  ENDPROC
ENDDEFINE
*
DEFINE CLASS CRegistry AS Custom  
*** RootToHandle()     PARAMETER psroot 
*** OpenKey()          PARAMETER psrootkey, pspathkey
*** DeleteKey()        PARAMETER psrootkey, pspathkey
*** CreateKey()        PARAMETER psrootkey,pspathkey
*** SetKeyValue()      PARAMETER psvalname, psval
*** SetKeyValueAsInt() PARAMETER psvalname, pival
*** CloseKey()
*** GetKeyValue()      PARAMETER psvalname
*** IsKeyExists()      PARAMETER psrootkey,pspathkey
*** GetSubkeys()
*** DeleteValue()      PARAMETER psname
*** IsErr()
HIDDEN hcurkey, ierr
*
HIDDEN FUNCTION RootToHandle
     PARAMETER psroot
     PRIVATE li
     DO CASE
          CASE psroot =  ;
               'HKEY_CLASSES_ROOT'
               li = -2147483648 
          CASE psroot =  ;
               'HKEY_CURRENT_USER'
               li = -2147483647
          CASE psroot =  ;
               'HKEY_LOCAL_MACHINE'
               li = -2147483646
          CASE psroot =  ;
               'HKEY_USERS'
               li = -2147483645
     ENDCASE
     RETURN li
ENDFUNC
*
PROCEDURE OpenKey
     PARAMETER psrootkey, pspathkey
     PRIVATE lirootkey,  ;
             licreatekeyresult
     DECLARE INTEGER RegCreateKey  ;
             IN Win32API INTEGER,  ;
             STRING @, INTEGER @
     licreatekeyresult = 0
     this.ierr = regcreatekey(this.roottohandle(psrootkey), ;
                 @pspathkey, ;
                 @licreatekeyresult)
     CLEAR DLLS  'REGCREATEKEY'
     this.hcurkey = licreatekeyresult
ENDPROC
*
PROCEDURE DeleteKey
     PARAMETER psrootkey,  ;
               pspathkey
     DECLARE INTEGER RegDeleteKey IN Win32API INTEGER, STRING @
     regdeletekey(this.roottohandle(psrootkey), pspathkey)
     CLEAR DLLS  'REGDELETEKEY'
     *DECLARE Integer RegDeleteKey IN Win32API Integer nHKey, String @cSubKey
ENDPROC
*
PROCEDURE CreateKey
     PARAMETER psrootkey,pspathkey
     DECLARE INTEGER RegCreateKey IN Win32API INTEGER, STRING @, INTEGER @
     regcreatekey(this.roottohandle(psrootkey),pspathkey,0)
     CLEAR DLLS  'REGCREATEKEY'
     *DECLARE Integer RegCreateKey IN Win32API Integer nHKey, String @cSubKey, Integer @nResult
ENDPROC
*
PROCEDURE SetKeyValue
     PARAMETER psvalname, psval
     PRIVATE lsval
     DECLARE INTEGER  ;
             RegSetValueEx IN  ;
             Win32API INTEGER,  ;
             STRING, INTEGER,  ;
             INTEGER, STRING,  ;
             INTEGER
     lsval = psval + CHR(0)
     this.ierr = regsetvalueex(this.hcurkey, ;
                 psvalname,0,1, ;
                 lsval, ;
                 LEN(lsval))
     CLEAR DLLS  'REGSETVALUEEX'
ENDPROC
*
PROCEDURE SetKeyValue
     PARAMETER psvalname, psval
     PRIVATE lsval
     DECLARE INTEGER  ;
             RegSetValueEx IN  ;
             Win32API INTEGER,  ;
             STRING, INTEGER,  ;
             INTEGER, STRING,  ;
             INTEGER
     lsval = psval + CHR(0)
     this.ierr = regsetvalueex(this.hcurkey, ;
                 psvalname,0,1, ;
                 lsval, ;
                 LEN(lsval))
     CLEAR DLLS  'REGSETVALUEEX'
ENDPROC
*
PROCEDURE SetKeyValueAsInt
     PARAMETER psvalname, pival
     DECLARE INTEGER  ;
             RegSetValueEx IN  ;
             Win32API INTEGER,  ;
             STRING, INTEGER,  ;
             INTEGER, LONG @,  ;
             INTEGER
     this.ierr = regsetvalueex(this.hcurkey, ;
                 psvalname,0,4, ;
                 @pival,4)
     CLEAR DLLS  'REGSETVALUEEX'
ENDPROC
*
PROCEDURE CloseKey
     DECLARE INTEGER RegCloseKey  ;
             IN Win32API INTEGER
     this.ierr = regclosekey(this.hcurkey)
     CLEAR DLLS  'REGCLOSEKEY'
ENDPROC
*
FUNCTION GetKeyValue
     PARAMETER psvalname
     PRIVATE listub, litype,  ;
             lsbuf, libuflen,  ;
             lsres
     DECLARE INTEGER  ;
             RegQueryValueEx IN  ;
             Win32API INTEGER,  ;
             STRING, INTEGER,  ;
             INTEGER @, STRING @,  ;
             INTEGER @
     listub = 0
     litype = 0
     lsbuf = SPACE(256)
     libuflen = LEN(lsbuf)
     this.ierr = regqueryvalueex(this.hcurkey, ;
                 psvalname,listub, ;
                 @litype,@lsbuf, ;
                 @libuflen)
     IF this.iserr()
          lsres = 'REG_ERROR'
     ELSE
          lsres = LEFT(lsbuf,  ;
                  libuflen - 1)
     ENDIF
     CLEAR DLLS   ;
           'REGQUERYVALUEEX'
     RETURN lsres
ENDFUNC
*
FUNCTION IsKeyExists
     PARAMETER psrootkey,  ;
               pspathkey
     PRIVATE lirootkey,  ;
             licreatekeyresult,  ;
             lierr
     DECLARE INTEGER RegOpenKey  ;
             IN Win32API INTEGER,  ;
             STRING @, INTEGER @
     DECLARE INTEGER RegCloseKey  ;
             IN Win32API INTEGER
     licreatekeyresult = 0
     lierr = regopenkey(this.roottohandle(psrootkey), ;
             @pspathkey, ;
             @licreatekeyresult)
     regclosekey(this.hcurkey)
     CLEAR DLLS  'REGCREATEKEY'
     CLEAR DLLS  'REGCLOSEKEY'
     RETURN (lierr = 0)
ENDFUNC
*
FUNCTION GetSubkeys
     PRIVATE lsres, ls, li
     DECLARE INTEGER RegEnumKey  ;
             IN WIN32API INTEGER,  ;
             INTEGER, STRING @,  ;
             INTEGER
     lsres = ''
     li = 0
     DO WHILE .T.
          ls = SPACE(100)
          IF regenumkey(this.hcurkey, ;
             li,@ls,100) = 0
               lsres = lsres +  ;
                       ALLTRIM(ls) +  ;
                       CHR(9)
               li = li + 1
          ELSE
               EXIT
          ENDIF
     ENDDO
     CLEAR DLLS  'REGENUMKEY'
     RETURN lsres
ENDFUNC
*
PROCEDURE DeleteValue
     PARAMETER psname
     DECLARE INTEGER  ;
             RegDeleteValue IN  ;
             Win32API INTEGER,  ;
             STRING
     regdeletevalue(this.hcurkey, ;
                   psname)
     CLEAR DLLS  'REGDELETEVALUE'
ENDPROC
*
FUNCTION IsErr
     RETURN (this.ierr <> 0)
ENDFUNC
*
ENDDEFINE
&&& ����� �������� ������ ��� ������ � ��������

