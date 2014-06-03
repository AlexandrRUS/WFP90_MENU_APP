PARAMETERS cURcOMsTR
** Программа "Меню" Стартовый файл программы
* MENU_APP.PRG

* Framework-generated application startup program
* for C:\FOXPRO\MENU\MENU Project
CLOSE ALL
SET CPDIALOG ON
SET STEP off
SET BELL off
SET CENTURY ON   && ! не менять!
SET DATE BRITISH && !!! не менять!
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
* ПУТИ *
PUBLIC TMP,TPUTI,TPUTA,TPUTR,TPUTDOC,TPUT,_progrootregdir,TPr_Vzr_Det
_progrootregdir='SOFTWARE\M' && КОРНЕВОЙ РАЗДЕЛ РЕЕСТРА,ГДЕ ХРАНЯТСЯ НАСТРОЙКИ
TPUTI=SYS(5)+CURDIR()+"DBF\"
TPUTA=SYS(5)+CURDIR()+"ARHIV\"
TPUTR=SYS(5)+CURDIR()+"RC\"
TPUTDOC=SYS(5)+CURDIR()+"DOC\"
TMP=SYS(2023)+"\"
TPUT=SYS(5)+CURDIR() && рабочий каталог
IF .not.DIRECTORY(TPUTDOC)
  TPUTDOC=TMP
ENDIF   
TDEFLTPATH=.T.  && использовать пути по умолчанию
*******УСТАНОВКИ ПО УМОЛЧАНИЮ
=NUMLOCK(.T.)
=CAPSLOCK(.T.)
TPr_Vzr_Det='' && признак "взрослые"/"Дети"/"все"  для накопительной ведомости.
****************************************
PUBLIC RTprnDEf,TprnDEf,TNomPrnDef,TBRUTMRS
TprnDEf="" && имя принтера для печати по умолчанию
TNomPrnDef=0 && номер принтера для печати по умолчанию
TBRUTMRS=.F. && переменная-ключ (TBRUTMRS=.T. - учитывать коэффицент пересчета при печати МРС)  

PUBLIC rmenutitle, RProgTitle 

rmenutitle = "Меню"
RProgTitle = ""
 
****** КОНСТАНТЫ
PUBLIC RVERS,RTITL,RVnorm,RVnorm,RInDate,ROutDate,RNormKal,RNormBel,RNormJir,RNormUgl
PUBLIC RDateMenu
PUBLIC TREGV
TREGV="" && незарегистрированная версия
RDateMenu=CTOD("")
RVERS="4.00.00"
RTITL="MЕНЮ  "+RVERS+"  (C) 2004-14"
RVnorm=100.00 &&  норма стоимости дневонго меню одного ВЗРОСЛОГО человека
RDnorm=60.00 &&  норма стоимости дневонго меню одного РЕБЕНКА человека
RNormBel=130.0 && норма расходав белков
RNormJir=130.0 && норма расходав жиров
RNormUgl=500.0 && норма расходав углеводов
RNormKal=3609.0 && ККал/сут энергетический расход 

RInDate=CTOD("  .  .    ") && входящая дата для сводок
ROutDate=CTOD("  .  .    ")&& исходящая дата для сводок
************** REKVIZITY
Tfirma=" -- не указано наименование учр. -- "
PUBLIC TRuk,TZKlad,TKlad,TBuh,TDVr,TDSt,TPov                           && отв. лица
PUBLIC TDolRuk,TDolZKlad,TDolKlad,TDolBuh,TDolDVr,TDolDSt,TDolPov      && долж. отв. лиц.
STORE SPACE(0) TO TRuk,TZKlad,TKlad,TBuh,TDVr,TDSt,TPov
STORE SPACE(0) TO TDolRuk,TDolZKlad,TDolKlad,TDolBuh,TDolDVr,TDolDSt,TDolPov
*RRECV1="ООО ТОПОРОК ИНН 640000000000"
*RRECV2="                            "
*RRECV3="                            "
************* KEYS !
PUBLIC ON_EXIT,TFASTEXIT,CURRSEL,TBgPic,TLastTreb
*CURRSEL="0" && ТЕКУЩАЯ РАБОЧАЯ ОБЛАСТЬ
TLastTreb=0 && последнее требование, с которым работали/просматривали/формировали 
TFASTEXIT=.F.  && .T. - РАЗРЕШИТЬ БЫСТРЫЙ ВЫХОД ИЗ ПРОГРАММЫ (БЕЗ ЗАПРОСА)
ON_EXIT=.F.   && ВЫХОД ИЗ ПРОГРАММЫ
TBgPic="" && картинка для оформления программы (cnfyl)
********************
PUBLIC ROtvOtp,RNamMenu
ROtvOtp=""
RNamMenu=""
************* ПРЕДВАРИТЕЛЬНЫЕ НАСТРОЙКИ ЭКРАНА
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
  _greg=CREATEOBJECT('CRegistry') && ОБЬЕКТ ДЛЯ РАБОТЫ С РЕЕСТРОМ
ENDIF 
*************************************
** чтение ключей командной строки

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
*!*	      =_greg.setkeyvalue('path',tmp) && путь к программе
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
*!*	      =MESSAGEBOX("Регистрация программы завершена.",64,RTITL)
*!*	    ELSE
*!*	      =MESSAGEBOX("Нет доступа к каталогу"+CHR(13)+tmp+CHR(13)+"Установка программы невозможна.",16,RTITL)
*!*	    ENDIF 
*!*	    TFASTEXIT=.T.
*!*	    DO Prog_SHUTDOWN
*!*	  CASE cURcOMsTR=="/F"
*!*	    =MESSAGEBOX("Режим самотестирования программы недоработан",16,"Отладка")
*!*	  CASE cURcOMsTR="/U"
*!*	    IF MESSAGEBOX("Вы действительно хотите удалить программу МЕНЮ с данного компьютера?",32+4+256,RTITL)=6
*!*	      PUBLIC _progname, _gsrootdir,_gregistry
*!*	      PRIVATE sprogver
*!*	      _progname = RTITL+': удаление'
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
*!*	            =MESSAGEBOX('Отсутствует доступ к папке '+_gsrootdir+ CHR(13)+;
*!*	                    '. Обратитесь к системному администратору вашей организации.', 16,_progname)
*!*	          ENDIF       
*!*	          TFASTEXIT=.T.
*!*	          DO Prog_SHUTDOWN
*!*	        ELSE
*!*	          ** не нашли программу деинсталляции - удаляем то, что возможно 
*!*	          IF .not.DIRECTORY(_gsrootdir)
*!*	            _gsrootdir=SYS(5)+CURDIR()
*!*	          ENDIF 
*!*	          IF IsPathWriteable(_gsrootdir) 
*!*	            =_gregistry.setkeyvalue('Path',_gsrootdir))
*!*	            cleardir(_gsrootdir,.F.) && попытка удаления рабочего каталога
*!*	            _gregistry.deletekey('HKEY_LOCAL_MACHINE',_progrootregdir)
*!*	            =MESSAGEBOX('Процедура удаления завершена некорректно.'+CHR(13)+'Не удалось удалить каталог:'+CHR(13)+_gsrootdir,64, _progname)
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
*!*	    =MESSAGEBOX("Недопустимый параметр",16)
*!*	  ENDCASE 
*!*	ENDIF 
*************************************
_SCREEN.Show

LOCAL CurVl


*!*	_greg.openkey('HKEY_LOCAL_MACHINE',_progrootregdir)
*!*	******************************* ЧТЕНИЕ УСТАНОВОК ИЗ РЕЕСТРА
*!*	LOCAL CurTmpVar
*!*	CurTmpVar = _greg.getkeyvalue('CustomPath')
*!*	TDEFLTPATH=IIF(CurTmpVar=='REG_ERROR',.T.,IIF(CurTmpVar=='1',.F.,.T.)) && пути по умолчанию

CurVl = GetIniParam('CustomPath')
TDEFLTPATH=IIF(VARTYPE(CurVl)='C',IIF(CurVl=='1',.F.,.T.),.T.)&& пути по умолчанию

*!*	*
*!*	CurTmpVar = alltrim(_greg.getkeyvalue('BGPic')) && рисунок рабочей формы
*!*	IF CurTmpVar==".F." && НЕТ  
*!*	  TBgPic=".F."
*!*	  _screen.Picture=""
*!*	ELSE 
*!*	  IF FILE(CurTmpVar) && внешн
*!*	    TBgPic=CurTmpVar
*!*	    _screen.Picture=CurTmpVar
*!*	  ELSE && станд
*!*	    TBgPic=""
*!*	   ** _screen.Picture ='WIZSTONE.BMP'
*!*	  ENDIF
*!*	ENDIF 
*!*	*


CurVl = GetIniParam('RProgTitle')
IF VARTYPE(CurVl)='C' AND  NOT EMPTY(CurVl)
	RProgTitle = CurVl && заголовок программы
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
*!*	TFASTEXIT=IIF(CurTmpVar=='REG_ERROR',.F.,IIF(CurTmpVar=='1',.T.,.F.)) && быстрый выход из программы
*!*	*

CurVl = GetIniParam('FstEXT')
TFASTEXIT=IIF(VARTYPE(CurVl)='C',IIF(CurVl=='1',.T.,.F.),.T.)&& быстрый выход из программы

*!*	** чтение реквизитов из реестра
*!*	CurTmpVar = _greg.getkeyvalue('FIRMA')
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TFIRMA=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('FIRMA')
TFIRMA=IIF(VARTYPE(CurVl)='C',CurVl,"")

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('Ruk') &&*********<<< руководитель
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TRuk=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('Ruk')
TRuk=IIF(VARTYPE(CurVl)='C',CurVl,"") &&<<< руководитель


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('DolRuk') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TDolRuk=CurTmpVar
*!*	ENDIF 
CurVl = GetIniParam('DolRuk')
TDolRuk=IIF(VARTYPE(CurVl)='C',CurVl,"")


*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('ZKlad') &&*********<<< Зав. складом
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
*!*	CurTmpVar = _greg.getkeyvalue('Klad') &&*********<<< кладовщик
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
*!*	CurTmpVar = _greg.getkeyvalue('Buh') &&*********<<< Бухгалтер
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
*!*	CurTmpVar = _greg.getkeyvalue('DVr') &&*********<<< Диетврач
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
*!*	CurTmpVar = _greg.getkeyvalue('DSt') &&*********<<< Диет - сестра
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
*!*	CurTmpVar = _greg.getkeyvalue('Pov') &&*********<<< Повар
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
*!*	CurTmpVar = _greg.getkeyvalue('PMRS') && переменная-ключ (TBRUTMRS=.T. - учитывать коэффицент пересчета при печати МРС)   
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  TBRUTMRS=IIF(CurTmpVar="1",.t.,.f.)
*!*	ENDIF 
CurVl = GetIniParam('PMRS')
TBRUTMRS=IIF(VARTYPE(CurVl)='C',IIF(CurVl=="1",.T.,.F.),"")

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_bel') && норма по белку
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormBel=val(CurTmpVar)
*!*	ENDIF 
CurVl = GetIniParam('n_bel')
RNormBel=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_Jir') && норма по жирам
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormJir=val(CurTmpVar)
*!*	ENDIF 
CurVl = GetIniParam('n_Jir')
RNormJir=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_ugl') && норма по углеводам
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormUgl=val(CurTmpVar)
*!*	ENDIF 
CurVl = GetIniParam('n_ugl')
RNormUgl=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)

*!*	*
*!*	CurTmpVar = _greg.getkeyvalue('n_kal') && норма по калорийности
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RNormKal=VAL(CurTmpVar)
*!*	ENDIF
CurVl = GetIniParam('n_kal')
RNormKal=IIF(VARTYPE(CurVl)='C',VAL(CurVl),0)
 
*!*	*
*!*	** считывание реквизитов из реестра закончено


*************************************** проверка пароля доступа**
*!*	CurTmpVar = _greg.getkeyvalue('pschr') &&
*!*	_greg.closekey()
*!*	RELEASE _greg 

CurVl = GetIniParam('pschr')
CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"")

*!*	IF CurTmpVar='REG_ERROR'
*!*	  =MESSAGEBOX([Ошибка доступа!]+chr(13)+[Обратитесь к разработчику.],48,[Ошибка!],10000)
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
*************************************** проверка пароля ok!**
IF TYPE("_greg")<>"O"
  _greg=CREATEOBJECT('CRegistry') && ОБЬЕКТ ДЛЯ РАБОТЫ С РЕЕСТРОМ
ENDIF 
_greg.openkey('HKEY_LOCAL_MACHINE',_progrootregdir)
*RVnorm=100.00 &&  норма стоимости дневонго меню одного ВЗРОСЛОГО человека
*RDnorm=60.00 &&  норма стоимости дневонго меню одного РЕБЕНКА человека
*!*	CurTmpVar = _greg.getkeyvalue('VzrNorm') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RVnorm=VAL(CurTmpVar)
*!*	ENDIF 
*!*	CurTmpVar = _greg.getkeyvalue('DetNorm') &&
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  RDnorm=VAL(CurTmpVar)
*!*	ENDIF 
**** printer
*TprnDEf="" && имя принтера для печати по умолчанию
*TNomPrnDef=0 && номер принтера для печати по умолчанию

CurVl = GetIniParam('prndef')
CurTmpVar=IIF(VARTYPE(CurVl)='C',CurVl,"REG_ERROR")

**--CurTmpVar = _greg.getkeyvalue('prndef') &&
IF CurTmpVar<>'REG_ERROR'
  TprnDEf=alltrim(CurTmpVar)
ENDIF 
IF .not.empty(TprnDEf)
  APRINTERS(CurPrnList) && массив принтеров
  IF TYPE("CurPrnList")="C"
    TNomPrnDef=INT((ascan(CurPrnList,TprnDEf)+1)/2)
  ELSE
    TNomPrnDef=0
  ENDIF
ENDIF

**
*** пути
IF .not.TDEFLTPATH
  && использование заданных наперед путей
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
******************* проверка регистрации
*!*	CurTmpVar = _greg.getkeyvalue('RGV') && ReGisfration Value 
*!*	IF CurTmpVar<>'REG_ERROR'
*!*	  
*!*	ELSE
*!*	  TREGV="" && незарегистр
*!*	ENDIF 

&& ВСЕГДА!
TREGV="OK" && регистрация была 

*******************
_greg.closekey()
RELEASE _greg 
IF .not.DIRECTORY(tmp)
   TMP=SYS(5)+CURDIR()
ENDIF
IF (.not.DIRECTORY(TPUTI)).or.(.not.file(TPUTI+"MOBOR.DBF"))
  =MESSAGEBOX("Не найден каталог основных таблиц",16,"Ошибка!")
  DO FORM msetpath
  IF .not.file(TPUTI+"MOBOR.DBF")
    =MESSAGEBOX("Не найдены рабочие таблицы",16,"Ошибка!")
    TFASTEXIT=.T.
    DO Prog_SHUTDOWN
    RETURN 
  ENDIF 
ENDIF
*********************
ON SHUTDOWN DO Prog_SHUTDOWN
************************* open free tables
ERR_OPN=MDBF() && Открываем Таблицы
IF LEN(ERR_OPN)>0
   =MESSAGEBOX([Не найден]+CHR(44)+[ поврежден или недоступен файл]+CHR(13)+ERR_OPN+CHR(13);
   +[Вероятно,вы пытаетесь запустить вторую копию программы]+CHR(13)+[Обратитесь к разработчику программы ],16,RTITL)
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

DEFINE PAD m1 OF _MSYSMENU PROMPT "Склад"
DEFINE PAD m2 OF _MSYSMENU PROMPT "Справочники"
DEFINE PAD m3 OF _MSYSMENU PROMPT "Отчеты"
DEFINE PAD m4 OF _MSYSMENU PROMPT "Администратор"
DEFINE PAD m5 OF _MSYSMENU PROMPT "Настройки"
DEFINE PAD m6 OF _MSYSMENU PROMPT "Помощь"
DEFINE PAD m7 OF _MSYSMENU PROMPT  "Выход" KEY "ALT+X"

ON PAD m1 OF _MSYSMENU ACTIVATE POPUP Spis
ON PAD m2 OF _MSYSMENU ACTIVATE POPUP SPRAV
ON PAD m3 OF _MSYSMENU ACTIVATE POPUP SVOD
ON PAD m4 OF _MSYSMENU ACTIVATE POPUP RK
ON PAD m5 OF _MSYSMENU ACTIVATE POPUP NASTR
ON PAD m6 OF _MSYSMENU ACTIVATE POPUP MPOM && помощь

DEFINE POPUP Spis MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF Spis PROMPT "Новый приход"
DEFINE BAR 2 OF Spis PROMPT "Новое расходное требование"
DEFINE BAR 3 OF Spis PROMPT "\Разовое расходное требование"
DEFINE BAR 4 OF Spis PROMPT "Новое списание"
DEFINE BAR 5 OF Spis PROMPT "Оборот"

DEFINE POPUP SPRAV  MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF SPRAV  PROMPT "Продукты"
DEFINE BAR 2 OF SPRAV PROMPT "Блюда"
DEFINE BAR 3 OF SPRAV PROMPT "Меню"
DEFINE BAR 4 OF SPRAV PROMPT "Поставщики"
DEFINE BAR 5 OF SPRAV PROMPT "Диеты"
DEFINE BAR 6 OF SPRAV PROMPT "Типы меню"

**ПРОИЗВОДСТВО,ПЕРЕДАЧА,ВОЗВРАТ НА СКЛАД,СПИСАНИЕ,ПОТЕРИ
DEFINE POPUP SVOD MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF SVOD PROMPT "Оборотно-сальдовая ведомость"
DEFINE BAR 2 OF SVOD PROMPT "Меню-раскладка"
DEFINE BAR 3 OF SVOD PROMPT "Меню"
DEFINE BAR 4 OF SVOD PROMPT "Оборот документов"
DEFINE BAR 5 OF SVOD PROMPT "Продукты"
DEFINE BAR 6 OF SVOD PROMPT "Накопительная ведомость"
DEFINE BAR 7 OF SVOD PROMPT "Дневное расходное требование"

DEFINE POPUP RK MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF RK PROMPT "Создание резервной копии" SKIP FOR .not.OnARJ()
DEFINE BAR 2 OF RK PROMPT "Восстановление из резервной копии" SKIP FOR .not.OnARJ()
DEFINE BAR 3 OF RK PROMPT "Настройка путей программы"
DEFINE BAR 4 OF RK PROMPT "Пароль пользователя"
DEFINE BAR 5 OF RK PROMPT "Выбрать Принтер"
DEFINE BAR 6 OF RK PROMPT "Сервис"
DEFINE BAR 7 OF RK PROMPT "Консоль"

*DEFINE BAR 5 OF RK PROMPT "\Сброс в архив"

DEFINE POPUP NASTR MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF NASTR PROMPT "Реквизиты"
DEFINE BAR 2 OF NASTR PROMPT "Постоянные"
DEFINE BAR 3 OF NASTR PROMPT "Ответственные лица"

DEFINE POPUP MPOM MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF MPOM PROMPT "Разделы" KEY F1 SKIP FOR .not.OnHelp(.T.) 
DEFINE BAR 2 OF MPOM PROMPT "О программе..."
**IF TREGV<>"OK"
**  DEFINE BAR 3 OF MPOM PROMPT "Регистрация"
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
ON SELECTION BAR 6 OF Sprav DO FORM msprMenu  && справочник меню  

ON SELECTION BAR 1 OF SVOD DO FORM mobsv
ON SELECTION BAR 2 OF SVOD DO FORM mmakemrs with TLastTreb
ON SELECTION BAR 3 OF SVOD DO FORM mprnmenu
ON SELECTION BAR 4 OF SVOD DO PrepReprt && список документов
ON SELECTION BAR 5 OF SVOD prn_sp_mat() && печать списка материалов
ON SELECTION BAR 6 OF SVOD DO FORM mnakved && накопительная ведомость
ON SELECTION BAR 7 OF SVOD DO FORM mitotreb && итоговая роасходная ведомость

ON SELECTION BAR 1 OF RK DO SaveRezervCopy && создание резервной копии
ON SELECTION BAR 2 OF RK DO ReplaceRezervCopy && восстановление резервной копии
ON SELECTION BAR 3 OF RK DO FORM msetpath
ON SELECTION BAR 4 OF RK DO FORM Mps
ON SELECTION BAR 5 OF RK DO SetPrinter
ON SELECTION BAR 6 OF RK DO FORM MNASTR && ПРЧИЕ НАСТРОЙКИ РАБОЧЕГО СТОЛА
ON SELECTION BAR 7 OF RK DO FORM adm_cons && административная консоль (непосредственное выполнение команд FOXPRO на работающей программе)

ON SELECTION BAR 1 OF NASTR DO FORM msetrecv
ON SELECTION BAR 2 OF NASTR DO FORM mconst
ON SELECTION BAR 3 OF NASTR DO FORM motvlica

ON SELECTION BAR 1 OF MPOM DO OnHelp with .F.
ON SELECTION BAR 2 OF MPOM DO FORM frmabout 
IF TREGV<>"OK"
  ON SELECTION BAR 3 OF MPOM DO FORM mreg && РЕГИСТРАЦИЯ ПРОГРАММЫ
ENDIF

ON SELECTION PAD m7 OF _MSYSMENU QUIT

ON KEY LABEL ESC QUIT
DO WHILE .T.
  Read EVENTS    && <<--- Ни в коем случае не удалять !!!!
  CLEAR EVENTS
ENDDO

ENDPROC

*********** <END OF CODE > ********************

***********************************************************************
***********************************************************************
***********  подпрограммы
***********************************************************************
***********************************************************************

FUNCTION GetIniParam
     * СЧИТЫВАЕМ ПАРАМЕТР ИЗ INI *
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
     * запись параметра в INI *
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
	** процедура закрывает временную таблицу, находящуюся в области памяти NumWorkAreaTmpTable
	PRIVATE CharVar1,CharVar2
	IF NOT EMPTY(NumWorkAreaTmpTable) AND USED(NumWorkAreaTmpTable)
  		CharVar1= ALIAS(NumWorkAreaTmpTable)
  		USE IN (CharVar1)
  		CharVar2=TMP+CharVar1+".cdx"
  		ERASE &CharVar2 && удаляем индекс
  		CharVar1=TMP+CharVar1+".tmp"
  		ERASE &CharVar1 && удаляем таблицу
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
  ** данная функция позволяет создать  
  ** резервную копию текущих рабочих таблиц
  LOCAL CurFile,CurFileName,CURCNTEL,CURRCSCRIPT,CURDATE
  **** ВЫЯСНЯЕМ ПОСЛЕДНИЙ НОМЕР РЕЗЕРВНОЙ КОПИИ
  SET DEFAULT TO &TPUT
  CURDATE=DTOC(DATE())
  CurFileName='RC'+LEFT(CURDATE,2)+SUBSTR(CURDATE,4,2)+RIGHT(CURDATE,2)
  CurFile = PUTFILE('Имя файла:', TPUTR+CurFileName, 'ARJ')
  IF .NOT.EMPTY(CurFile)
    CLOSE DATABASES 
&&	    CURRCSCRIPT='RC.ID'
&&	    gnErrFile = FCREATE(CURRCSCRIPT)
&&	    =FCLOSE(gnErrFile)
    CurFileName=TMP+'TMPP.TMP' && ни в коем случаен не меняйте название файла! Это ключ!
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
      && ИЩЕМ И УДАЛЯЕМ НАИБОЛЕЕ СТАРЫЙ ФАЙЛ 
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
    =MESSAGEBOX('Резервная копия создана успешно!',64,RTITL)
  ENDIF  
RETURN CurFile

FUNCTION ReplaceRezervCopy
  ** функция восстанавливает рабочие таблицы из резервной копии
  **
  LOCAL CurFName,Cur_File_Rc_Replaced
  Cur_File_Rc_Replaced=.f.
  CurNVar=0
  SET DEFAULT TO &TPUT
  DO WHILE .T.
    SET DEFAULT TO &TPUTR
    CurFName=GETFILE('ARJ', 'Имя файла:', 'Открыть', 0,   'Открытие резервной копии')
    SET DEFAULT TO &TPUT
    IF .not.EMPTY(CurFName)
      CurFlOK=GETID(CurFName)
      CurNVar=0
      IF .not.CurFlOK
        CurNvar=MESSAGEBOX("Выбранный файл, возможно, не является резервной копией."+CHR(13)+;
        "Восстановить копию из этого файла ?",3+32+256,'Запрос')
        DO CASE
        CASE CurNvar=2 && отмена
          RETURN .f.
        CASE CurNvar=6 && да
        CASE CurNvar=7 && нет
          LOOP
        ENDCASE
      ELSE
        CurNvar=MESSAGEBOX("Распаковать данные из файла резервной копии?"+CHR(13)+;
        CurFName,4+64+256,'Запрос')
        IF CurNvar=7
          RETURN .F.
        ENDIF
      ENDIF
    ENDIF
    EXIT
  ENDDO
  ***
  IF CurNvar=6 && восстанавливаем копию
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
      =FWRITE(gnErrFile , 'ECHO я> '+TMP+'RC.ID'+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , 'arj a '+TPUTR+'LASTCOPY.ARJ '+tputi+'*.DBF'+CHR(13)+CHR(10)) && не меняй строку!!! это ключ! 
      =FWRITE(gnErrFile , 'ARJ X -Y '+CurFName+' *.DBF '+TPUTI+CHR(13)+CHR(10))
      =FWRITE(gnErrFile , ':END '+CHR(13)+CHR(10))
      =FCLOSE(gnErrFile )     && Close file
      CLOSE DATABASES
      RUN &CURRCSCRIPT
      IF .NOT.FILE(TMP+'RC.ID')
        =MESSAGEBOX('Ошибка открытия резервной копии.'+CHR(13)+'Возможно файл поврежден !',16,RTITL)
      ELSE 
        ERASE (TMP+'RC.ID')
      ENDIF
    ELSE
      WAIT 'Ошибка создания скрипта.' WINDOW NOWAIT
    ENDIF
  ENDIF
  SET DEFAULT TO &TPUT
  DO MDBF
RETURN Cur_File_Rc_Replaced
*
FUNCTION OnARJ
********************
* проверка и вызов файла помощи на экран
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
  ** вспомогательная функция.
  ** если символа байта файла  по адресу H22
  ** равны 'TMP' или 'LAS' , то возвращается .T.
  ** имя файла передается функции параметром.
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
* проверка и вызов файла помощи на экран
* Параметр BackModeCheck = .T. - фоновая проверка
* доступности файла помощи.
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
  * SetFileAttrib -Установка атрибутов файла 
  * psfilename-полный путь к файлу+имя файла (симв.)
  * piattrib -атрибуты (Целое.) 
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
  * ClearDir - очистка директории PARAMETER pspath (имя каталога (С)), psdelroot (Признак .T. - Удалить каталог.)
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
   IF TFASTEXIT.OR.MESSAGEBOX([Выйти из программы ?],68,RTITL)=6
       CLOSE DATABASES
&&	       IF TYPE("_SCREEN._greg")="O"
&&	         _SCREEN.RemoveObject("_greg")  && уничтожаем обьект 
&&	       ENDIF  
&&	       IF TYPE("_greg")="O"
&&	         RELEASE _greg 
&&	         && _greg=CREATEOBJECT('CRegistry') && ОБЬЕКТ ДЛЯ РАБОТЫ С РЕЕСТРОМ
&&	       ENDIF '
		_screen.Hide 
       SET SYSMENU TO default
       CLOSE ALL
       QUIT
  ENDIF
RETURN

FUNCTION SolvSumDok
***************************
* Функция просчитывает сумму по документу
* Параметры CurNakr- номер накладной
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
* Процедура подготавливает среду для отчета
***************************
  IF TYPE("RNamMenu")#"C" 
    RELEASE RNamMenu 
    PUBLIC RNamMenu
    RNamMenu="" && не указанo наименование документа 
  ENDIF 
  LOCAL CurPrPer
  DO FORM mperiod TO CurPrPer
  SELECT 8
  SET ORDER TO 
  IF CurPrPer
    RNamMenu="Период с "+DTOC(RinDate)+" по "+DTOC(ROutDate) && не указанo наименование документа 
    SET FILTER TO (mobor.datp>=RinDate).and.(mobor.datp<=ROutDate) IN 8
    GO TOP IN 8
    IF EMPTY(APRINTERS(AA))
      REPORT FORM rptspnak PREVIEW NOCONSOLE
    ELSE 
      REPORT FORM rptspnak TO PRINTER PROMPT PREVIEW NOCONSOLE
    ENDIF   
  ELSE
    RNamMenu="" && не указанo наименование документа 
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
  =MESSAGEBOX("В системе нет установленых принтеров",48,"Ошибка")
ENDIF
IF .not.empty(CurPrn)
  && thisform.OnPrnDef=CurPrn
 * thisform.txtPrn.Value = CurPrn
  TprnDEf=CurPrn && имя принтера для печати по умолчанию
  APRINTERS(CurPrnList) && массив принтеров
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
  && ПАРАМЕТР CurNakr ДОЛЖЕН СОДЕРЖАТЬ НОМЕР ВРЕМЕННОЙ РАБОЧЕЙ ОБЛАСТИ
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
  && пп ВОЗВРАЩАЕТ ТИП НАБОРА БЛЮД
  * В СИМВОЛЬНУЮ СТРОКУ "Завтрак,Обед,Полдник,Ужин,II ужин,II Обед,Дополн.,"
  DO CASE
  case CurNakr1=1
    curtag="Завтрак"
  CASE CurNakr1=2
    curtag="Обед"
  CASE CurNakr1=3
    curtag="Полдник"
  CASE CurNakr1=4
    curtag="Ужин"
  CASE CurNakr1=5
    curtag="II ужин"
  CASE CurNakr1=6
    curtag="II обед"
  OTHERWISE
    curtag=""
  ENDCASE
RETURN curtag

FUNCTION KKPC
PARAMETERS P,nLenStr,lPrEdRu 
* функция преобразует число в эквивалентную ему
* символьную прописную строку строку
* P - сумма числом
* lPrEdRu - ключ .F. - "единицы"  .T. - "рубли"
* nLenStr - желаемая длина строки
LOCAL  M11,M12,M13,M14,M15,M16,M17,M18,M19,M21,M22,M23,M24,M25,M26,M27,M28,M29,M31
LOCAL M32,M33,M34,M35,M36,M37,M38,M39,M41,M42,M43,M44,M45,M46,M47,M48,M49,M51,M52
LOCAL M53,M54,M55,M56,M57,M58,V11,V12,V13,V22,V21,V23,V31,V32,V33,V41,V42,V43,V51
LOCAL V52,V53,V50,V61,V62,V63,V70,V50,M30
LOCAL StrPropis
StrPropis="" && строка прописью
IF TYPE("nLenStr")#"N"
  nLenStr=500
ENDIF
IF TYPE("lPrEdRu")#"L"
  lPrEdRu=.F.
ENDIF

M11="сто "
M12="двести "
M13="триста "
M14="четыреста "
M15="пятьсот "
M16="шестьсот "
M17="семьсот "
M18="восемьсот "
M19="девятьсот "
M21="десять "
M22="двадцать "
M23="тридцать "
M24="сорок "
M25="пятьдесят "
M26="шестьдесят "
M27="семьдесят "
M28="восемьдесят "
M29="девяносто "
M30="ноль "
M31="один "
M32="два "
M33="три "
M34="четыре "
M35="пять "
M36="шесть "
M37="семь "
M38="восемь "
M39="девять "
M41="одиннадцать "
M42="двенадцать "
M43="тринадцать "
M44="четырнадцать "
M45="пятнадцать "
M46="шестнадцать "
M47="семнадцать "
M48="восемнадцать "
M49="девятнадцать "
M51="одна "
M52="две "
M53=""
M54=""
M55=""
M56="шесть "
M57=""
M58=""
M59=""
V11="триллион "
V12="триллиона "
V13="триллионов "
V22="миллиарда "
V21="миллиард "
V23="миллиардов "
V31="миллион "
V32="миллиона "
V33="миллионов "
V41="тысяча "
V42="тысячи "
V43="тысяч "
V50="и "
IF lPrEdRu
  V51="рубль "
  V52="рубля "
  V53="рублей "

  V61="копейка "
  V62="копейки "
  V63="копеек "
  
  V70="" 
ELSE 
  V51="единица "
  V52="единицы "
  V53="единиц "

  V61="сотая "
  V62="сотых "
  V63="сотых "
  
  V70="минус "
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
  S=S+' рублей' && целое число рублей
 ELSE 
  S=S+' единиц' && целое число единиц
 ENDIF 
ENDIF
** сотые или копейки
IF INT(P)=0 
  S=M30+V53 && целое 
ENDIF 
P6=int(mod(P,1)*100)
IF P6>0
  i=6
  S1=INT(P6/100) && ВСЕГДА 0 В ДАННОМ СЛУЧАЕ (СОТНИ)
  S2=INT((P6-S1*100)/10) && десятки
  S3=INT(MOD(P6,10)) && ЕДИНИЦЫ
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
SS=S && сумма одной строкой
StrPropis=S
n=nLenStr && длина строк
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
IF CHR(241)=="ё" && распознаем кодировку
  DO CASE    && для кодировки MS DOS - 866
  CASE SS1<=175
    SSS1=SS1-32
  CASE SS1=241
    SSS1=240
  CASE SS1>=224.AND.SS1<=240
    SSS1=SS1-80
  ENDCASE
else
  DO CASE    && для кодировки ANSI - 1251 (Win)
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
if fsize('PRUPR')=0    && КОНТРОЛЬ НАЛИЧИЯ ПОЛЯ
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
** классы определенные программно 
***********************************************************************

DEFINE CLASS CBB AS COMBOBOX
  IncrementalSearch = .T. 
  RowSourceType =  1 
  RowSource ="первое,второе,третье,допол.,      "
  Style= 2
  BorderStyle= 0
  PROCEDURE LostFocus
  ENDPROC  
  PROCEDURE InteractiveChange
	* метод используется для смены очерёдности 
	* в сетке детализации, если в сетке
	* состава разового меню меняется время
    LOCAL CurTmpVar,CurNBl,CurPrMn,CurNBl1,CurPrMn1,CurPrNBl
    
  	IF NOT EMPTY(thisform._wa_menu)
  		thisform._changed = .T. && редактировали
    	CurNBl=ALIAS(thisform._wa_menu)+".NBLUD"
    	CurNBl=&CurNBl
    	CurPrMn=ALIAS(thisform._wa_menu)+".PRMN" && прежний признак 
    	CurPrMn=&CurPrMn
    	IF CurPrMn <> THIS.ListIndex
        	** ПРОВЕРКА НА НАЛИЧИЕ ПОХОЖИХ ЗАПИСЕЙ
        	GO TOP IN thisform._wa_detail
        	CurPrNBl=.F.
        	DO WHILE .NOT.EOF(thisform._wa_detail)
        		CurNBl1 = ALIAS(thisform._wa_detail)+".NBLUD"
        		CurNBl1 = &CurNBl1
        		CurPrMn1 = ALIAS(thisform._wa_detail)+".PRMN" && прежний признак 
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
  RowSource ="Завтрак,Обед,Полдник,Ужин,II Ужин,II Обед,Дополн.,"
  Style= 2
  BorderStyle= 0
  PROCEDURE InteractiveChange
    thisform.onprsavmenu = .t. && меню меняли
    thisform.onchsavmenu && проверка на изменения
  ENDPROC  
  PROCEDURE LostFocus
	* метод используется для смены времени 
	* кормежки в сетке меню-раскладки, если в сетке
	* состава меню так же меняется время
	* Последняя запись 20.04.2004
    LOCAL CurTmpVar,CurNBl,CurPrMn,CurNBl1,CurPrMn1,CurPrNBl
    IF NOT EMPTY(thisform.onmensel)
      CurNBl=ALIAS(thisform.onmensel)+".NBL"
      CurNBl=&CurNBl
      CurPrMn=ALIAS(thisform.onmensel)+".PrMn" && прежний признак 
      CurPrMn=&CurPrMn
      IF CurPrMn<>THIS.ListIndex
        ** ПРОВЕРКА НА НАЛИЧИЕ ПОХОЖИХ ЗАПИСЕЙ
        GO TOP IN thisform.onsel
        CurPrNBl=.F.
        DO WHILE .NOT.EOF(thisform.onsel)
          CurNBl1=ALIAS(thisform.onsel)+".NBLUD"
          CurNBl1=&CurNBl1
          CurPrMn1=ALIAS(thisform.onsel)+".PrMn" && прежний признак 
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
          THISFORM.LBLINFO.CAPTION="Отменено..."
        ELSE
    	  THISFORM.LBLINFO.CAPTION=""
    	  replace PrMn WITH THIS.ListIndex FOR PrMn=CurPrMn.and.nblud=CurNBl IN thisform.onsel
          Replace PrMn WITH THIS.ListIndex IN thisform.onmensel
          thisform.oneditmen=.t.  && Флаг редактирования menu .T.- состав изменялся
          *     thisform.onedittreb=.t.  && Флаг редактирования trebovani'a .T.- состав изменялся
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
&&& описане класса для работы с реестром
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
&&& конец описания класса для работы с реестром
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
