PARAMETERS cURcOMsTR
** ��������� "����" ��������� ���� ���������
* MENU_APP.PRG

* Framework-generated application startup program
* for C:\FOXPRO\MENU\MENU Project
CLOSE ALL
SET CPDIALOG ON
SET STEP off
SET BELL off
SET CENTURY ON   && ! �� ������!
SET DATE BRITISH && !!! �� ������!
SET TALK OFF
SET CONSOLE OFF
SET ECHO OFF
SET ESCAPE ON
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
PUBLIC TMP,TPUTI,TPUTA,TPUTR,TPUTDOC,TPUT,_progrootregdir,TPr_Vzr_Det
_progrootregdir='SOFTWARE\M' && �������� ������ �������,��� �������� ���������
TPUTI=SYS(5)+CURDIR()+"DBF\"
TPUTA=SYS(5)+CURDIR()+"ARHIV\"
TPUTR=SYS(5)+CURDIR()+"RC\"
TPUTDOC=SYS(5)+CURDIR()+"DOC\"
TMP=SYS(2023)+"\"
TPUT=SYS(5)+CURDIR() && ������� �������
IF .not.DIRECTORY(TPUTDOC)
  TPUTDOC=TMP
ENDIF   
TDEFLTPATH=.T.  && ������������ ���� �� ���������
*******��������� �� ���������
=NUMLOCK(.T.)
=CAPSLOCK(.T.)
TPr_Vzr_Det='' && ������� "��������"/"����"/"���"  ��� ������������� ���������.
****************************************
PUBLIC RTprnDEf,TprnDEf,TNomPrnDef,TBRUTMRS
TprnDEf="" && ��� �������� ��� ������ �� ���������
TNomPrnDef=0 && ����� �������� ��� ������ �� ���������
TBRUTMRS=.F. && ����������-���� (TBRUTMRS=.T. - ��������� ���������� ��������� ��� ������ ���)  

PUBLIC rmenutitle, RProgTitle 

rmenutitle = "����"
RProgTitle = ""
 
****** ���������
PUBLIC RVERS,RTITL,RVnorm,RVnorm,RInDate,ROutDate,RNormKal,RNormBel,RNormJir,RNormUgl
PUBLIC RDateMenu
PUBLIC TREGV
TREGV="" && �������������������� ������
RDateMenu=CTOD("")
RVERS="4.00.00"
RTITL="M���  "+RVERS+"  (C) 2004-14"
RVnorm=100.00 &&  ����� ��������� �������� ���� ������ ��������� ��������
RDnorm=60.00 &&  ����� ��������� �������� ���� ������ ������� ��������
RNormBel=130.0 && ����� �������� ������
RNormJir=130.0 && ����� �������� �����
RNormUgl=500.0 && ����� �������� ���������
RNormKal=3609.0 && ����/��� �������������� ������ 

RInDate=CTOD("  .  .    ") && �������� ���� ��� ������
ROutDate=CTOD("  .  .    ")&& ��������� ���� ��� ������
************** REKVIZITY
Tfirma=" -- �� ������� ������������ ���. -- "
PUBLIC TRuk,TZKlad,TKlad,TBuh,TDVr,TDSt,TPov                           && ���. ����
PUBLIC TDolRuk,TDolZKlad,TDolKlad,TDolBuh,TDolDVr,TDolDSt,TDolPov      && ����. ���. ���.
STORE SPACE(0) TO TRuk,TZKlad,TKlad,TBuh,TDVr,TDSt,TPov
STORE SPACE(0) TO TDolRuk,TDolZKlad,TDolKlad,TDolBuh,TDolDVr,TDolDSt,TDolPov
*RRECV1="��� ������� ��� 640000000000"
*RRECV2="                            "
*RRECV3="                            "
************* KEYS !
PUBLIC ON_EXIT,TFASTEXIT,CURRSEL,TBgPic,TLastTreb
*CURRSEL="0" && ������� ������� �������
TLastTreb=0 && ��������� ����������, � ������� ��������/�������������/����������� 
TFASTEXIT=.F.  && .T. - ��������� ������� ����� �� ��������� (��� �������)
ON_EXIT=.F.   && ����� �� ���������
TBgPic="" && �������� ��� ���������� ��������� (cnfyl)
********************
PUBLIC ROtvOtp,RNamMenu
ROtvOtp=""
RNamMenu=""
************* ��������������� ��������� ������
*_screen.Show 
IF WEXIST("Standard") = .T.  
  HIDE WINDOW "Standard"  
ENDIF
_screen.Hide
_screen.AutoCenter = .T.
_SCREEN.Caption=RTITL
_screen.Top =40
_screen.Left =0
_screen.ZoomBox = .T.
_screen.Movable = .T.
_screen.Icon='ICON4.ICO'
_screen.BorderStyle = 3
 ***************************
 set color to n/bg
_screen.Cls
_screen.Picture ='WIZSTONE.BMP'



*************************************
IF TYPE("_greg")<>"O"
  _greg=CREATEOBJECT('CRegistry') && ������ ��� ������ � ��������
ENDIF 
*************************************
** ������ ������ ��������� ������

*!*	TYPECS=TYPE("cURcOMsTR")
*!*	IF TYPECS="C" 
*!*	  cURcOMsTR=UPPER(cURcOMsTR)
*!*	  DO CASE 
*!*	  CASE cURcOMsTR=="/S".OR.cURcOMsTR=="/R"
*!*	    tmp=SYS(5)+CURDIR()
*!*	    IF IsPathWriteable(tmp)
*!*	      _greg.createkey('HKEY_LOCAL_MACHINE',_progrootregdir)
*!*	      _greg.openkey('HKEY_LOCAL_MACHINE',_progrootregdir)
*!*	      =_greg.setkeyvalue('pschr',"")
*!*	      =_greg.setkeyvalue('CustomPath',"0")
*!*	      =_greg.setkeyvalue('path',tmp) && ���� � ���������
*!*	      _greg.closekey()
*!*	      tmp1=tmp+"DBF\"
*!*	      IF .not.DIRECTORY(TMP1) 
*!*	        MD (TMP1)
*!*	      ENDIF 
*!*	      tmp1=tmp+"ARHIV\"
*!*	      IF .not.DIRECTORY(TMP1) 
*!*	        MD (TMP1)
*!*	      ENDIF 
*!*	      tmp1=tmp+"DOC\"
*!*	      IF .not.DIRECTORY(TMP1) 
*!*	        MD (TMP1)
*!*	      ENDIF 
*!*	      tmp1=tmp+"RC\"
*!*	      IF .not.DIRECTORY(TMP1) 
*!*	        MD (TMP1)
*!*	      ENDIF 
*!*	      =MESSAGEBOX("����������� ��������� ���������.",64,RTITL)
*!*	    ELSE
*!*	      =MESSAGEBOX("��� ������� � ��������"+CHR(13)+tmp+CHR(13)+"��������� ��������� ����������.",16,RTITL)
*!*	    ENDIF 
*!*	    TFASTEXIT=.T.
*!*	    DO Prog_SHUTDOWN
*!*	  CASE cURcOMsTR=="/F"
*!*	    =MESSAGEBOX("����� ���������������� ��������� �����������",16,"�������")
*!*	  CASE cURcOMsTR="/U"
*!*	    IF MESSAGEBOX("�� ������������� ������ ������� ��������� ���� � ������� ����������?",32+4+256,RTITL)=6
*!*	      PUBLIC _progname, _gsrootdir,_gregistry
*!*	      PRIVATE sprogver
*!*	      _progname = RTITL+': ��������'
*!*	      _SCREEN.caption = _progname
*!*	      _gregistry = CREATEOBJECT('CRegistry')
*!*	      _gregistry.openkey('HKEY_LOCAL_MACHINE',_progrootregdir)
*!*	      _gsrootdir = alltrim(_gregistry.getkeyvalue('Path'))
*!*	      _gregistry.closekey()
*!*	      _Tmpdir=alltrim(GETENV('Temp'))
*!*	      _Tmpdir=_Tmpdir+IIF(RIGHT(_Tmpdir,1)="\","","\")
*!*	      IF DIRECTORY(_Tmpdir)
*!*	        UnProg=_gsrootdir+IIF(right(_gsrootdir,1)="\","","\")+"UNINSTAL.EX"
*!*	        UnProg1=_Tmpdir+"UNINSTAL.EXE"
*!*	        IF FILE(UnProg)
*!*	          IF .not.DIRECTORY(_gsrootdir)
*!*	            _gsrootdir=SYS(5)+CURDIR()
*!*	          ENDIF 
*!*	          IF IsPathWriteable(_gsrootdir)
*!*	            =_gregistry.setkeyvalue('Path',_gsrootdir))
*!*	            COPY FILE &UnProg TO &UnProg1
*!*	            RUN /n &UnProg1  
*!*	          ELSE 
*!*	            =MESSAGEBOX('����������� ������ � ����� '+_gsrootdir+ CHR(13)+;
*!*	                    '. ���������� � ���������� �������������� ����� �����������.', 16,_progname)
*!*	          ENDIF       
*!*	          TFASTEXIT=.T.
*!*	          DO Prog_SHUTDOWN
*!*	        ELSE
*!*	          ** �� ����� ��������� ������������� - ������� ��, ��� �������� 
*!*	          IF .not.DIRECTORY(_gsrootdir)
*!*	            _gsrootdir=SYS(5)+CURDIR()
*!*	          ENDIF 
*!*	          IF IsPathWriteable(_gsrootdir) 
*!*	            =_gregistry.setkeyvalue('Path',_gsrootdir))
*!*	            cleardir(_gsrootdir,.F.) && ������� �������� �������� ��������
*!*	            _gregistry.deletekey('HKEY_LOCAL_MACHINE',_progrootregdir)
*!*	            =MESSAGEBOX('��������� �������� ��������� �����������.'+CHR(13)+'�� ������� ������� �������:'+CHR(13)+_gsrootdir,64, _progname)
*!*	          ENDIF 
*!*	        ENDIF 
*!*	      ENDIF  
*!*	     RELEASE _progname, _gsrootdir, _gregistry
*!*	    ENDIF 
*!*	    TFASTEXIT=.T.
*!*	    DO Prog_SHUTDOWN
*!*	  CASE cURcOMsTR="/?"
*!*	    =MESSAGEBOX("Usage: MENU [options]"+CHR(13)+"Valid options are:"+CHR(13)+"/S"+CHR(9)+;
*!*	    "Setup program"+CHR(13)+"/U"+CHR(9)+"Uninstall program"+CHR(13)+"/F"+CHR(9)+;
*!*	    "run program in Findung and fixung errors mode",64,RTITL)
*!*	    TFASTEXIT=.T.
*!*	    DO Prog_SHUTDOWN
*!*	  OTHERWISE 
*!*	    =MESSAGEBOX("������������ ��������",16)
*!*	  ENDCASE 
*!*	ENDIF 
*************************************
_SCREEN.Show

LOCAL CurVl


*!*	_greg.openkey('HKEY_LOCAL_MACHINE',_progrootregdir)
*!*	******************************* ������ ��������� �� �������
*!*	LOCAL CurTmpVar
*!*	CurTmpVar = _greg.getkeyvalue('CustomPath')
*!*	TDEFLTPATH=IIF(CurTmpVar=='REG_ERROR',.T.,IIF(CurTmpVar=='1',.F.,.T.)) && ���� �� ���������

CurVl = GetIniParam('CustomPath')
TDEFLTPATH=IIF(VARTYPE(CurVl)='C',IIF(CurVl=='1',.F.,.T.),.T.)&& ���� �� ���������

*!*	*
*!*	CurTmpVar = alltrim(_greg.getkeyvalue('BGPic')) && ������� ������� �����
*!*	IF CurTmpVar==".F." && ���  
*!*	  TBgPic=".F."
*!*	  _screen.Picture=""
*!*	ELSE 
*!*	  IF FILE(CurTmpVar) && �����
*!*	    TBgPic=CurTmpVar
*!*	    _screen.Picture=CurTmpVar
*!*	  ELSE && �����
*!*	    TBgPic=""
*!*	   ** _screen.Picture ='WIZSTONE.BMP'
*!*	  ENDIF
*!*	ENDIF 
*!*	*


CurVl = GetIniParam('RProgTitle')
IF VARTYPE(CurVl)='C' AND  NOT EMPTY(CurVl)
	RProgTitle = CurVl && ��������� ���������
	_SCREEN.Caption=ALLTRIM(RProgTitle)+" "+RTITL
ENDIF

CurVl = GetIniParam('MenuTitle')
IF VARTYPE(CurVl)='C' AND  NOT EMPTY(CurVl)
   rmenutitle = CurVl
ENDIF



CurVl = GetIniParam('BGPic')
IF VARTYPE(CurVl)='C'
	IF CurVl=".F."
  		TBgPic=".F."
  		_screen.Picture=""		
	ENDIF
	IF FILE(CurVl)
    	TBgPic=CurVl
    	_screen.Picture=CurVl
    ELSE
		TBgPic=""
		_screen.Picture ='WIZSTONE.BMP'  		
	ENDIF	
ELSE
	TBgPic=""
	_screen.Picture ='WIZSTONE.BMP'
ENDIF


*!*	CurTmpVar = _greg.getkeyvalue('FstEXT')
*!*	TFASTEXIT=IIF(CurTmpVar=='REG_ERROR',.F.,IIF(CurTmpVar=='1',.T.,.F.)) && ������� ����� �� ���������
*!*	*

CurVl = GetIniParam('FstEXT')
TFASTEXIT=IIF(VARTYPE(CurVl)='C',IIF(CurVl=='1',.T.,.F.),.T.)&& ������� ����� �� ���������

*!*	** ������ ���������� �� �������
*!*	CurTmpVar = _greg.getkeyvalue('FIRMA')
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TFIRMA=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('FIRMA')
TFIRMA=IIF(VARTYPE(CurVl)='C',CurVl,"")

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('Ruk') &&*********<<< ������������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TRuk=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('Ruk')
TRuk=IIF(VARTYPE(CurVl)='C',CurVl,"") &&<<< ������������


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolRuk') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolRuk=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolRuk')
TDolRuk=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('ZKlad') &&*********<<< ���. �������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TZKlad=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('ZKlad')
TZKlad=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolZKlad') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolZKlad=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolZKlad')
TDolZKlad=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('Klad') &&*********<<< ���������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TKlad=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('Klad')
TKlad=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolKlad') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolKlad=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolKlad')
TDolKlad=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('Buh') &&*********<<< ���������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TBuh=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('Buh')
TBuh=IIF(VARTYPE(CurVl)='C',CurVl,"")

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolBuh') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolBuh=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolBuh')
TDolBuh=IIF(VARTYPE(CurVl)='C',CurVl,"")

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DVr') &&*********<<< ��������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDVr=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DVr')
TDVr=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolDVr') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolDVr=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolDVr')
TDolDVr=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DSt') &&*********<<< ���� - ������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDSt=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DSt')
TDSt=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolDSt') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolDSt=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolDSt')
TDolDSt=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('Pov') &&*********<<< �����
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TPov=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('Pov')
TPov=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolPov') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolPov=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolPov')
TDolPov=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('PMRS') && ����������-���� (TBRUTMRS=.T. - ��������� ���������� ��������� ��� ������ ���)   
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TBRUTMRS=IIF(CurTmpVar="1",.t.,.f.)
*!*	ENDIF 
CurVl = GetIniParam('PMRS')
TBRUTMRS=IIF(VARTYPE(CurVl)='C',IIF(CurVl=="1",.T.,.F.),"")

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_bel') && ����� �� �����
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormBel=val(CurTmpVar)
*!*	ENDIF 
CurVl = GetIniParam('n_bel')
RNormBel=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_Jir') && ����� �� �����
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormJir=val(CurTmpVar)
*!*	ENDIF 
CurVl = GetIniParam('n_Jir')
RNormJir=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_ugl') && ����� �� ���������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormUgl=val(CurTmpVar)
*!*	ENDIF 
CurVl = GetIniParam('n_ugl')
RNormUgl=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_kal') && ����� �� ������������
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormKal=VAL(CurTmpVar)
*!*	ENDIF
CurVl = GetIniParam('n_kal')
RNormKal=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)
 
*!*	*
*!*	** ���������� ���������� �� ������� ���������


*************************************** �������� ������ �������**
*!*	CurTmpVar = _greg.getkeyvalue('pschr') &&
*!*	_greg.closekey()
*!*	RELEASE _greg 

CurVl = GetIniParam('pschr')
CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"")

*!*	IF CurTmpVar='REG_ERROR'
*!*	  =MESSAGEBOX([������ �������!]+chr(13)+[���������� � ������������.],48,[������!],10000)
*!*	  TFASTEXIT=.T.
*!*	  DO Prog_SHUTDOWN
*!*	ELSE 
IF LEN(CurTmpVar)>0
    CurPsOk=.F.
    DO FORM Mpschk TO CurPsOk
    IF .NOT.CurPsOk 
      TFASTEXIT=.T.
      DO Prog_SHUTDOWN
    ENDIF 
ENDIF
*!*	ENDIF 
*************************************** �������� ������ ok!**
IF TYPE("_greg")<>"O"
  _greg=CREATEOBJECT('CRegistry') && ������ ��� ������ � ��������
ENDIF 
_greg.openkey('HKEY_LOCAL_MACHINE',_progrootregdir)
*RVnorm=100.00 &&  ����� ��������� �������� ���� ������ ��������� ��������
*RDnorm=60.00 &&  ����� ��������� �������� ���� ������ ������� ��������
*!*	CurTmpVar = _greg.getkeyvalue('VzrNorm') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RVnorm=VAL(CurTmpVar)
*!*	ENDIF 
*!*	CurTmpVar = _greg.getkeyvalue('DetNorm') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RDnorm=VAL(CurTmpVar)
*!*	ENDIF 
**** printer
*TprnDEf="" && ��� �������� ��� ������ �� ���������
*TNomPrnDef=0 && ����� �������� ��� ������ �� ���������

CurVl = GetIniParam('prndef')
CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"REG_ERROR")

**--CurTmpVar = _greg.getkeyvalue('prndef') &&
IF CurTmpVar<>'REG_ERROR'
  TprnDEf=alltrim(CurTmpVar)
ENDIF 
IF .not.empty(TprnDEf)
  APRINTERS(CurPrnList) && ������ ���������
  IF TYPE("CurPrnList")="C"
    TNomPrnDef=INT((ascan(CurPrnList,TprnDEf)+1)/2)
  ELSE
    TNomPrnDef=0
  ENDIF
ENDIF

**
*** ����
IF .not.TDEFLTPATH
  && ������������� �������� ������� �����
  *CurTmpVar = _greg.getkeyvalue('IPATH') &&
  
	CurVl = GetIniParam('IPATH')
	CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"REG_ERROR")
  
  IF CurTmpVar<>'REG_ERROR'
    TPUTI=ALLTRIM(CurTmpVar)+iif(RIGHT(ALLTRIM(CurTmpVar),1)="\","","\")
  ENDIF 
*  CurTmpVar = _greg.getkeyvalue('APATH') &&

	CurVl = GetIniParam('APATH')
	CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"REG_ERROR")
	
  IF CurTmpVar<>'REG_ERROR'
    TPUTA=ALLTRIM(CurTmpVar)+iif(RIGHT(ALLTRIM(CurTmpVar),1)="\","","\")
  ENDIF 
*  CurTmpVar = _greg.getkeyvalue('RPATH') &&

	CurVl = GetIniParam('RPATH')
	CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"REG_ERROR")

  IF CurTmpVar<>'REG_ERROR'
    TPUTR=ALLTRIM(CurTmpVar)+iif(RIGHT(ALLTRIM(CurTmpVar),1)="\","","\")
  ENDIF 
*  CurTmpVar = _greg.getkeyvalue('TPATH') &&

	CurVl = GetIniParam('TPATH')
	CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"REG_ERROR")
	
  IF CurTmpVar<>'REG_ERROR'
    TMP=ALLTRIM(CurTmpVar)+iif(RIGHT(ALLTRIM(CurTmpVar),1)="\","","\")
  ENDIF 
 * CurTmpVar = _greg.getkeyvalue('DPATH') &&
	
	CurVl = GetIniParam('DPATH')
	CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"REG_ERROR")
	  
  IF CurTmpVar<>'REG_ERROR'
    TPUTDOC=ALLTRIM(CurTmpVar)+iif(RIGHT(ALLTRIM(CurTmpVar),1)="\","","\")
  ENDIF 
ENDIF
******************* �������� �����������
*!*	CurTmpVar = _greg.getkeyvalue('RGV') && ReGisfration Value 
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  
*!*	ELSE
*!*	  TREGV="" && �����������
*!*	ENDIF 

&& ������!
TREGV="OK" && ����������� ���� 

*******************
_greg.closekey()
RELEASE _greg 
IF .not.DIRECTORY(tmp)
   TMP=SYS(5)+CURDIR()
ENDIF
IF (.not.DIRECTORY(TPUTI)).or.(.not.file(TPUTI+"MOBOR.DBF"))
  =MESSAGEBOX("�� ������ ������� �������� ������",16,"������!")
  DO FORM msetpath
  IF .not.file(TPUTI+"MOBOR.DBF")
    =MESSAGEBOX("�� ������� ������� �������",16,"������!")
    TFASTEXIT=.T.
    DO Prog_SHUTDOWN
    RETURN 
  ENDIF 
ENDIF
*********************
ON SHUTDOWN DO Prog_SHUTDOWN
************************* open free tables
ERR_OPN=MDBF() && ��������� �������
IF LEN(ERR_OPN)>0
   =MESSAGEBOX([�� ������]+CHR(44)+[ ��������� ��� ���������� ����]+CHR(13)+ERR_OPN+CHR(13);
   +[��������,�� ��������� ��������� ������ ����� ���������]+CHR(13)+[���������� � ������������ ��������� ],16,RTITL)
   TFASTEXIT=.T.
   DO Prog_SHUTDOWN
ENDIF
RELEASE ERR_OPN
***********
IF .not.DIRECTORY(TPUTR) 
  MD (TPUTR)
ENDIF 
***********
SET SYSMENU TO
*****************************************
SET SYSMENU AUTOMATIC
DEFINE MENU _MSYSMENU

DEFINE PAD m1 OF _MSYSMENU PROMPT "�����"
DEFINE PAD m2 OF _MSYSMENU PROMPT "�����������"
DEFINE PAD m3 OF _MSYSMENU PROMPT "������"
DEFINE PAD m4 OF _MSYSMENU PROMPT "�������������"
DEFINE PAD m5 OF _MSYSMENU PROMPT "���������"
DEFINE PAD m6 OF _MSYSMENU PROMPT "������"
DEFINE PAD m7 OF _MSYSMENU PROMPT  "�����" KEY "ALT+X"

ON PAD m1 OF _MSYSMENU ACTIVATE POPUP Spis
ON PAD m2 OF _MSYSMENU ACTIVATE POPUP SPRAV
ON PAD m3 OF _MSYSMENU ACTIVATE POPUP SVOD
ON PAD m4 OF _MSYSMENU ACTIVATE POPUP RK
ON PAD m5 OF _MSYSMENU ACTIVATE POPUP NASTR
ON PAD m6 OF _MSYSMENU ACTIVATE POPUP MPOM && ������

DEFINE POPUP Spis MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF Spis PROMPT "����� ������"
DEFINE BAR 2 OF Spis PROMPT "����� ��������� ����������"
DEFINE BAR 3 OF Spis PROMPT "\������� ��������� ����������"
DEFINE BAR 4 OF Spis PROMPT "����� ��������"
DEFINE BAR 5 OF Spis PROMPT "������"

DEFINE POPUP SPRAV  MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF SPRAV  PROMPT "��������"
DEFINE BAR 2 OF SPRAV PROMPT "�����"
DEFINE BAR 3 OF SPRAV PROMPT "����"
DEFINE BAR 4 OF SPRAV PROMPT "����������"
DEFINE BAR 5 OF SPRAV PROMPT "�����"
DEFINE BAR 6 OF SPRAV PROMPT "���� ����"

**������������,��������,������� �� �����,��������,������
DEFINE POPUP SVOD MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF SVOD PROMPT "��������-��������� ���������"
DEFINE BAR 2 OF SVOD PROMPT "����-���������"
DEFINE BAR 3 OF SVOD PROMPT "����"
DEFINE BAR 4 OF SVOD PROMPT "������ ����������"
DEFINE BAR 5 OF SVOD PROMPT "��������"
DEFINE BAR 6 OF SVOD PROMPT "������������� ���������"
DEFINE BAR 7 OF SVOD PROMPT "������� ��������� ����������"

DEFINE POPUP RK MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF RK PROMPT "�������� ��������� �����" SKIP FOR .not.OnARJ()
DEFINE BAR 2 OF RK PROMPT "�������������� �� ��������� �����" SKIP FOR .not.OnARJ()
DEFINE BAR 3 OF RK PROMPT "��������� ����� ���������"
DEFINE BAR 4 OF RK PROMPT "������ ������������"
DEFINE BAR 5 OF RK PROMPT "������� �������"
DEFINE BAR 6 OF RK PROMPT "������"
DEFINE BAR 7 OF RK PROMPT "�������"

*DEFINE BAR 5 OF RK PROMPT "\����� � �����"

DEFINE POPUP NASTR MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF NASTR PROMPT "���������"
DEFINE BAR 2 OF NASTR PROMPT "����������"
DEFINE BAR 3 OF NASTR PROMPT "������������� ����"

DEFINE POPUP MPOM MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF MPOM PROMPT "�������" KEY F1 SKIP FOR .not.OnHelp(.T.) 
DEFINE BAR 2 OF MPOM PROMPT "� ���������..."
**IF TREGV<>"OK"
**  DEFINE BAR 3 OF MPOM PROMPT "�����������"
**ENDIF

ON SELECTION BAR 1 OF Spis DO FORM mprih
ON SELECTION BAR 2 OF Spis DO FORM mredmenu WITH 0,.F.,.T.,0
ON SELECTION BAR 3 OF Spis DO FORM monsemenu
ON SELECTION BAR 4 OF Spis DO FORM mprodsp
ON SELECTION BAR 5 OF Spis DO FORM moborot

ON SELECTION BAR 1 OF Sprav DO FORM mspmater
ON SELECTION BAR 2 OF Sprav DO FORM mspbl
ON SELECTION BAR 3 OF Sprav DO FORM mspmenu
ON SELECTION BAR 4 OF Sprav DO FORM mspfirm
ON SELECTION BAR 5 OF Sprav DO FORM mspdiet
ON SELECTION BAR 6 OF Sprav DO FORM msprMenu  && ���������� ����  

ON SELECTION BAR 1 OF SVOD DO FORM mobsv
ON SELECTION BAR 2 OF SVOD DO FORM mmakemrs with TLastTreb
ON SELECTION BAR 3 OF SVOD DO FORM mprnmenu
ON SELECTION BAR 4 OF SVOD DO PrepReprt && ������ ����������
ON SELECTION BAR 5 OF SVOD prn_sp_mat() && ������ ������ ����������
ON SELECTION BAR 6 OF SVOD DO FORM mnakved && ������������� ���������
ON SELECTION BAR 7 OF SVOD DO FORM mitotreb && �������� ���������� ���������

ON SELECTION BAR 1 OF RK DO SaveRezervCopy && �������� ��������� �����
ON SELECTION BAR 2 OF RK DO ReplaceRezervCopy && �������������� ��������� �����
ON SELECTION BAR 3 OF RK DO FORM msetpath
ON SELECTION BAR 4 OF RK DO FORM Mps
ON SELECTION BAR 5 OF RK DO SetPrinter
ON SELECTION BAR 6 OF RK DO FORM MNASTR && ����� ��������� �������� �����
ON SELECTION BAR 7 OF RK DO FORM adm_cons && ���������������� ������� (���������������� ���������� ������ FOXPRO �� ���������� ���������)

ON SELECTION BAR 1 OF NASTR DO FORM msetrecv
ON SELECTION BAR 2 OF NASTR DO FORM mconst
ON SELECTION BAR 3 OF NASTR DO FORM motvlica

ON SELECTION BAR 1 OF MPOM DO OnHelp with .F.
ON SELECTION BAR 2 OF MPOM DO FORM frmabout 
IF TREGV<>"OK"
  ON SELECTION BAR 3 OF MPOM DO FORM mreg && ����������� ���������
ENDIF

ON SELECTION PAD m7 OF _MSYSMENU QUIT

ON KEY LABEL ESC QUIT
DO WHILE .T.
  Read EVENTS    && <<--- �� � ���� ������ �� ������� !!!!
  CLEAR EVENTS
ENDDO

ENDPROC

*********** <END OF CODE > ********************

***********************************************************************
***********************************************************************
***********  ������������
***********************************************************************
***********************************************************************

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
FUNCTION prn_sp_mat 
  IF EMPTY(APRINTERS(aa))
    REPORT FORM rptspMat PREVIEW NOCONSOLE
  ELSE
    REPORT FORM rptspMat TO PRINTER PROMPT PREVIEW NOCONSOLE
  ENDIF 
  RELEASE aa
ENDFUNC  

 FUNCTION SaveRezervCopy
  ** ������ ������� ��������� �������  
  ** ��������� ����� ������� ������� ������
  LOCAL CurFile,CurFileName,CURCNTEL,CURRCSCRIPT,CURDATE
  **** �������� ��������� ����� ��������� �����
  SET DEFAULT TO &TPUT
  CURDATE=DTOC(DATE())
  CurFileName='RC'+LEFT(CURDATE,2)+SUBSTR(CURDATE,4,2)+RIGHT(CURDATE,2)
  CurFile = PUTFILE('��� �����:', TPUTR+CurFileName, 'ARJ')
  IF .NOT.EMPTY(CurFile)
    CLOSE DATABASES 
&&	    CURRCSCRIPT='RC.ID'
&&	    gnErrFile = FCREATE(CURRCSCRIPT)
&&	    =FCLOSE(gnErrFile)
    CurFileName=TMP+'TMPP.TMP' && �� � ���� ������� �� ������� �������� �����! ��� ����!
    CURRCSCRIPT='ARJ A '+CurFileName+' '+TPUTI+'*.DBF'
    RUN &CURRCSCRIPT
    IF FILE(CurFileName)
      IF FILE(CurFile)
        ERASE &CurFile
      ENDIF
      COPY FILE &CurFileName TO &CurFile 
      ERASE &CurFileName
    ENDIF 
    SET DEFAULT TO &TPUT
    DO MDBF  
    RELEASE CURARR
    CURCNTEL=ADIR(CURARR,TPUTR+'RC??????.ARJ',"A")
    IF CURCNTEL>9
      && ���� � ������� �������� ������ ���� 
      CRDT=CTOD('01/01/2200')
      CUROLDFILEID=0
      FOR IJ=1 TO CURCNTEL
        IF CRDT>=CURARR(IJ,3)
          CRDT=CURARR(IJ,3)
          CUROLDFILEID=IJ
        ENDIF  
      NEXT
      IF .NOT.EMPTY(CUROLDFILEID)
        CURDATE=TPUTR+CURARR(CUROLDFILEID,1)
        IF FILE(CURDATE)
          ERASE &CURDATE
        ENDIF 
      ENDIF    
    ENDIF
    RELEASE CURARR
  ENDIF 
  IF FILE(CurFile)
    =MESSAGEBOX('��������� ����� ������� �������!',64,RTITL)
  ENDIF  
RETURN CurFile

FUNCTION ReplaceRezervCopy
  ** ������� ��������������� ������� ������� �� ��������� �����
  **
  LOCAL CurFName,Cur_File_Rc_Replaced
  Cur_File_Rc_Replaced=.f.
  CurNVar=0
  SET DEFAULT TO &TPUT
  DO WHILE .T.
    SET DEFAULT TO &TPUTR
    CurFName=GETFILE('ARJ', '��� �����:', '�������', 0,   '�������� ��������� �����')
    SET DEFAULT TO &TPUT
    IF .not.EMPTY(CurFName)
      CurFlOK=GETID(CurFName)
      CurNVar=0
      IF .not.CurFlOK
        CurNvar=MESSAGEBOX("��������� ����, ��������, �� �������� ��������� ������."+CHR(13)+;
        "������������ ����� �� ����� ����� ?",3+32+256,'������')
        DO CASE
        CASE CurNvar=2 && ������
          RETURN .f.
        CASE CurNvar=6 && ��
        CASE CurNvar=7 && ���
          LOOP
        ENDCASE
      ELSE
        CurNvar=MESSAGEBOX("����������� ������ �� ����� ��������� �����?"+CHR(13)+;
        CurFName,4+64+256,'������')
        IF CurNvar=7
          RETURN .F.
        ENDIF
      ENDIF
    ENDIF
    EXIT
  ENDDO
  ***
  IF CurNvar=6 && ��������������� �����
    CURRCSCRIPT=TMP+'RC.BAT'
    SET DEFAULT TO &TPUT
    IF FILE(CURRCSCRIPT)
      ERASE CURRCSCRIPT
    ENDIF
    gnErrFile = FCREATE(CURRCSCRIPT)  && If not create it
    IF gnErrFile > 0
      =FWRITE(gnErrFile , '@ECHO OFF'+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , 'CLS'+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , 'IF EXIST '+TMP+'RC.ID DEL '+TMP+'RC.ID'+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , 'arj t '+CurFName+' *.dbf > nul'+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , 'if errorlevel == 1 GOTO END:'+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , 'ECHO �> '+TMP+'RC.ID'+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , 'arj a '+TPUTR+'LASTCOPY.ARJ '+tputi+'*.DBF'+CHR(13)+CHR(10)) && �� ����� ������!!! ��� ����! 
      =FWRITE(gnErrFile , 'ARJ X -Y '+CurFName+' *.DBF '+TPUTI+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , ':END '+CHR(13)+CHR(10))
      =FCLOSE(gnErrFile )     && Close file
      CLOSE DATABASES
      RUN &CURRCSCRIPT
      IF .NOT.FILE(TMP+'RC.ID')
        =MESSAGEBOX('������ �������� ��������� �����.'+CHR(13)+'�������� ���� ��������� !',16,RTITL)
      ELSE 
        ERASE (TMP+'RC.ID')
      ENDIF
    ELSE
      WAIT '������ �������� �������.' WINDOW NOWAIT
    ENDIF
  ENDIF
  SET DEFAULT TO &TPUT
  DO MDBF
RETURN Cur_File_Rc_Replaced
*
FUNCTION OnARJ
********************
* �������� � ����� ����� ������ �� �����
********************
  LOCAL FlagsArhIsON
  IF FILE('arj.exe')
    FlagsArhIsON=.T.
  ELSE
    FlagsArhIsON=.F.
  ENDIF
  RETURN FlagsArhIsON
ENDFUNC
*
FUNCTION GETID
  *******
  ** ��������������� �������.
  ** ���� ������� ����� �����  �� ������ H22
  ** ����� 'TMP' ��� 'LAS' , �� ������������ .T.
  ** ��� ����� ���������� ������� ����������.
  *******
  PARAMETERS CurFName
  if type('CurFName')<>'C'
    CurFName=''
  endif
  check_is_ok=.f.
  if file(CurFName)
    mhandle = FOPEN(CurFName,0)
    IF mhandle>0
      = FSEEK(mhandle,34)
      CUR=FREAD(mhandle,3)
      if CUR=='TMP'.or.CUR=='LAS'
        check_is_ok=.T.
      endif
      =FCLOSE(mhandle)
    ENDIF
  endif
return check_is_ok
*
FUNCTION OnHelp
********************
* �������� � ����� ����� ������ �� �����
* �������� BackModeCheck = .T. - ������� ��������
* ����������� ����� ������.
********************
  PARAMETERS BackModeCheck
  LOCAL FlagsHelpIsON
  FlagsHelpIsON=.F.
  IF TYPE('BackModeCheck')#'L'
    BackModeCheck=.F.
  ENDIF 
  IF FILE('menu.hlp') 
    IF .not.BackModeCheck
      run /N winhelp.exe menu.hlp
    ENDIF 
    FlagsHelpIsON=.t.
  ENDIF 
  IF FILE('menu.chm') 
    IF .not.BackModeCheck
      run /n hh.exe menu.chm
    ENDIF 
    FlagsHelpIsON=.t.
  ENDIF 
  RETURN FlagsHelpIsON
ENDFUNC  

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
  PRIVATE li, lidirscnt, ls, lsdelfilename, ladirs,err_
  DIMENSION ladirs[50, 5]
  IF DIRECTORY(pspath)
     lidirscnt = ADIR(ladirs,pspath + '*.','D')
     FOR li = 1 TO lidirscnt
          IF  .NOT.INLIST(ladirs(li,1),'.', '..')
               cleardir(pspath + ladirs(li,1) + '\',.T.)
          ENDIF
     ENDFOR
     ls = SYS(2000, pspath + '*.*')
     ON ERROR err_=ERROR()
     DO WHILE  .NOT. EMPTY(ls)
          lsdelfilename = pspath + ls
          setfileattrib(lsdelfilename,32)
          DELETE FILE (lsdelfilename)
          ls = SYS(2000, pspath + '*.*', 1)
     ENDDO
     ON ERROR 
     IF psdelroot
       RD (pspath)
     ENDIF
  ENDIF
  RETURN
ENDPROC
*
PROCEDURE Prog_SHUTDOWN
   IF TFASTEXIT.OR.MESSAGEBOX([����� �� ��������� ?],68,RTITL)=6
       CLOSE DATABASES
&&	       IF TYPE("_SCREEN._greg")="O"
&&	         _SCREEN.RemoveObject("_greg")  && ���������� ������ 
&&	       ENDIF  
&&	       IF TYPE("_greg")="O"
&&	         RELEASE _greg 
&&	         && _greg=CREATEOBJECT('CRegistry') && ������ ��� ������ � ��������
&&	       ENDIF '
		_screen.Hide 
       SET SYSMENU TO default
       CLOSE ALL
       QUIT
  ENDIF
RETURN

FUNCTION SolvSumDok
***************************
* ������� ������������ ����� �� ���������
* ��������� CurNakr- ����� ���������
***************************
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


PROCEDURE PrepReprt
***************************
* ��������� �������������� ����� ��� ������
***************************
  IF TYPE("RNamMenu")#"C" 
    RELEASE RNamMenu 
    PUBLIC RNamMenu
    RNamMenu="" && �� ������o ������������ ��������� 
  ENDIF 
  LOCAL CurPrPer
  DO FORM mperiod TO CurPrPer
  SELECT 8
  SET ORDER TO 
  IF CurPrPer
    RNamMenu="������ � "+DTOC(RinDate)+" �� "+DTOC(ROutDate) && �� ������o ������������ ��������� 
    SET FILTER TO (mobor.datp>=RinDate).and.(mobor.datp<=ROutDate) IN 8
    GO TOP IN 8
    IF EMPTY(APRINTERS(AA))
      REPORT FORM rptspnak PREVIEW NOCONSOLE
    ELSE 
      REPORT FORM rptspnak TO PRINTER PROMPT PREVIEW NOCONSOLE
    ENDIF   
  ELSE
    RNamMenu="" && �� ������o ������������ ��������� 
  ENDIF 
  SET FILTER TO IN 8
  SET ORDER TO 1 IN 8   && NAkr
RETURN

PROCEDURE SetPrinter
LOCAL CurPrn,CurErr
CurErr=0
ON ERROR CurErr=ERROR()
CurPrn=GETPRINTER()
ON ERROR
IF CurErr>0
  CurPrn=0
  =MESSAGEBOX("� ������� ��� ������������ ���������",48,"������")
ENDIF
IF .not.empty(CurPrn)
  && thisform.OnPrnDef=CurPrn
 * thisform.txtPrn.Value = CurPrn
  TprnDEf=CurPrn && ��� �������� ��� ������ �� ���������
  APRINTERS(CurPrnList) && ������ ���������
  IF TYPE("CurPrnList")="C"
    TNomPrnDef=INT((ascan(CurPrnList,TprnDEf)+1)/2)
  ELSE
    TNomPrnDef=0
  ENDIF
ENDIF
RETURN

FUNCTION GETVREM
  PARAMETERS CurSel
  LOCAL curtag,CurNakr1,CurSel1
  && �������� CurNakr ������ ��������� ����� ��������� ������� �������
  IF PARAMETERS()>0
    CurSel1=STR(CurSel)
    SELECT &CurSel1
  *  SKIP -1
  *  IF BOF()
      CurNakr1=prmn
  *  ELSE
  *    SKIP
  *    SKIP
  *    CurNakr1=prmn
  *    SKIP -1
  *  ENDIF
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

FUNCTION GETVREM11111
&& SET STEP O
SELECT 2
USE KFIRMA
if fsize('PRUPR')=0    && �������� ������� ����
  COPY TO KFIRMA.DB_ FOR .T.
  COPY TO QM3 STRUCTURE EXTENDED
  USE QM3
  APPEND BLANK
  REPLACE FIELD_NAME WITH  "PRUPR"
  REPLACE FIELD_TYPE WITH  "N"
  REPLACE FIELD_LEN  WITH  1
  REPLACE FIELD_DEC  WITH  0
  USE
  CREATE KFIRMA FROM QM3
  APPEND FROM KFIRMA.DB_ FOR .T.
  REPLACE PRUPR WITH 0 FOR .T.
  ERASE 'QM3.DBF'
ENDIF
IF FILE('KFIRKAR.IDX').AND.FILE('KFIR.IDX')
  SET INDEX TO KFIR,KFIRKAR,kpr
  REINDEX
ELSE
  INDEX ON FIRMA TO KFIR
  INDEX ON KARTA TO KFIRKAR
  index on str(priz1)+str(priz)+str(karta) to kpr
  SET INDEX TO KFIR,KFIRKAR,kpr
ENDIF
RETURN .T.

***********************************************************************
** ������ ������������ ���������� 
***********************************************************************

DEFINE CLASS CBB AS COMBOBOX
  IncrementalSearch = .T. 
  RowSourceType =  1 
  RowSource ="������,������,������,�����.,      "
  Style= 2
  BorderStyle= 0
  PROCEDURE LostFocus
  ENDPROC  
  PROCEDURE InteractiveChange
	* ����� ������������ ��� ����� ���������� 
	* � ����� �����������, ���� � �����
	* ������� �������� ���� �������� �����
    LOCAL CurTmpVar,CurNBl,CurPrMn,CurNBl1,CurPrMn1,CurPrNBl
    
  	IF NOT EMPTY(thisform._wa_menu)
  		thisform._changed = .T. && �������������
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

*****
DEFINE CLASS CBOTime AS COMBOBOX && FORM
  IncrementalSearch = .T. 
  RowSourceType =  1 
  RowSource ="�������,����,�������,����,II ����,II ����,������.,"
  Style= 2
  BorderStyle= 0
  PROCEDURE InteractiveChange
    thisform.onprsavmenu = .t. && ���� ������
    thisform.onchsavmenu && �������� �� ���������
  ENDPROC  
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
          *     thisform.onedittreb=.t.  && ���� �������������� trebovani'a .T.- ������ ���������
          thisform.ongrid=.F. 
          THISFORM.GRid1.Refresh 
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
*
&&& ������� ������ ��� ������ � ��������
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
***********************************************************************


***************8
&&	CustomPath="CustomPath"
&&	IF reg_set(.f.,CustomPath,"")=0
&&	ENDIF
&&	IF CustomPath="1"
&&	  TDEFLTPATH=.f.
&&	ENDIF
&&	RELEASE CustomPath

&&	BGPic="BGPic"
&&	IF reg_set(.f.,BGPic,"")=0
&&	  BGPic=alltrim(BGPic)
&&	  DO CASE
&&	    CASE BGPic=".F."
&&	      TBgPic=".F."
&&	      _screen.Picture=""
&&	    CASE EMPTY(BGPic)
&&	      TBgPic=""
&&	    OTHERWISE
&&	      IF FILE(BGPic)
&&	        TBgPic=BGPic
&&	        _screen.Picture=ALLTRIM(BgPic)
&&	      ENDIF
&&	  ENDCASE
&&	ENDIF
&&	RELEASE BGPic
&&	FstEXT="FstEXT"
&&	IF reg_set(.f.,FstEXT,"")=0
&&	  IF FstEXT="1"
&&	    TFASTEXIT=.T.
&&	  ENDIF
&&	ENDIF
&&	RELEASE FstEXT
*******************************
**************************
