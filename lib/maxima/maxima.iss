;;----------------------------------------------------------------;;
;; -- maxima.iss --  Maxima InnoSetup installation script         ;;
;;----------------------------------------------------------------;;
;;                                                                ;;
;;  * InnoSetup version 5.1.7 or above is recommended             ;;
;;                                                                ;;
;;  * IMPORTANT!                                                  ;;
;;    Section [CustomMessages] below contains messages            ;;
;;    in several 8-bit encodings (code pages 1251, 1252).         ;;
;;    Don't use any encoding conversion tool (e.g. iconv,         ;;
;;    recode) on this file directly!                              ;;
;;                                                                ;;
;;----------------------------------------------------------------;;


[Setup]
AppName=Maxima
AppVerName=Maxima 5.14.0cvs
AppId=Maxima-5.14.0cvs
UsePreviousAppDir=no
AppPublisher=The Maxima Development Team
AppPublisherURL=http://maxima.sourceforge.net
AppSupportURL=http://maxima.sourceforge.net
AppUpdatesURL=http://maxima.sourceforge.net
AppVersion=5.14.0cvs
OutputBaseFilename=maxima-5.14.0cvs
DefaultDirName={pf}\Maxima-5.14.0cvs
DefaultGroupName=Maxima-5.14.0cvs
InfoBeforeFile=interfaces\xmaxima\win32\InfoBefore.txt
InfoAfterFile=interfaces\xmaxima\win32\readme_en.txt
AllowNoIcons=yes
LicenseFile=/usr/local\share\maxima\5.14.0cvs\doc\COPYING
Compression=lzma/ultra
SolidCompression=yes
Uninstallable=yes
UninstallFilesDir={app}\uninst
ShowLanguageDialog=yes
ChangesAssociations=yes


[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"; InfoBeforeFile: "interfaces\xmaxima\win32\InfoBefore_fr.txt"; InfoAfterFile: "interfaces\xmaxima\win32\readme_fr.txt"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"; InfoBeforeFile: "interfaces\xmaxima\win32\InfoBefore_de.txt"; InfoAfterFile: "interfaces\xmaxima\win32\readme_de.txt"
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"; InfoBeforeFile: "interfaces\xmaxima\win32\InfoBefore_it.txt"; InfoAfterFile: "interfaces\xmaxima\win32\readme_it.txt"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"; InfoBeforeFile: "interfaces\xmaxima\win32\InfoBefore_es.txt"; InfoAfterFile: "interfaces\xmaxima\win32\readme_es.txt"
Name: "pt_BR"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"; InfoBeforeFile: "interfaces\xmaxima\win32\InfoBefore_pt_BR.txt"; InfoAfterFile: "interfaces\xmaxima\win32\readme_pt_BR.txt"
Name: "pt"; MessagesFile: "compiler:Languages\Portuguese.isl"; InfoBeforeFile: "interfaces\xmaxima\win32\InfoBefore_pt.txt"; InfoAfterFile: "interfaces\xmaxima\win32\readme_pt.txt"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"; InfoBeforeFile: "interfaces\xmaxima\win32\InfoBefore_ru.txt"; InfoAfterFile: "interfaces\xmaxima\win32\readme_ru.txt"


[Types] 
Name: "full"; Description: "{cm:FullInstallation}" 
Name: "compact"; Description: "{cm:CompactInstallation}" 
Name: "custom"; Description: "{cm:CustomInstallation}"; Flags: iscustom


[Components]
Name: "core"; Description: "{cm:MaximaCore}"; Types: full compact custom; Flags: fixed
Name: "wxmaxima"; Description: "{cm:wxMaximaGraphicShell}"; Types: full custom
Name: "xmaxima"; Description: "{cm:XMaximaGraphicShell}"; Types: full custom
Name: "lang"; Description: "{cm:MaximaLanguagePacks}"; Types: full custom
Name: "lang\spanish"; Description: "{cm:Spanish}"; Types: full custom
Name: "lang\portuguese"; Description: "{cm:Portuguese}"; Types: full custom
Name: "lang\brazilian"; Description: "{cm:BrazilianPortuguese}"; Types: full custom


[Tasks]
Name: "wxmdesktopicon"; Description: "{cm:CreateMyDesktopIcon,wxMaxima}"; GroupDescription: "{cm:AdditionalIcons}"; MinVersion: 4,4;  Components: "wxmaxima"
Name: "xmdesktopicon"; Description: "{cm:CreateMyDesktopIcon,XMaxima}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; MinVersion: 4,4;  Components: "xmaxima"


[Files]
; core
Source: "/usr/local\bin\maxima-command.ico"; DestDir: "{app}\bin\"; Flags: ignoreversion; Components: "core"
Source: "/usr/local\bin\maxima.bat"; DestDir: "{app}\bin\"; Flags: ignoreversion; AfterInstall: ReplaceApp(); Components: "core" 
Source: "/usr/local\readme_*.txt"; DestDir: "{app}"; Components: "core"
Source: "/usr/local\*.*"; Excludes: "\bin\xmaxima.*,\bin\xmaxima,\wxMaxima,\info,\share\maxima\5.14.0cvs\doc\html,\share\maxima\5.14.0cvs\doc\chm"; DestDir: "{app}\";  Flags: recursesubdirs; Components: "core"
Source: "/usr/local\share\maxima\5.14.0cvs\doc\html\intromax.html"; DestDir: "{app}\share\maxima\5.14.0cvs\doc\html\"; Flags: ignoreversion; Components: "core"
Source: "/usr/local\share\maxima\5.14.0cvs\doc\chm\maxima.chm"; DestDir: "{app}\share\maxima\5.14.0cvs\doc\chm\"; Flags: ignoreversion; Components: "core"
Source: "/usr/local\info\*.*"; DestDir: "{app}\info\";  Flags: ignoreversion;  Components: "core"

; wmaxima
Source: "/usr/local\wxMaxima\*.*"; DestDir: "{app}\wxMaxima\"; Flags: ignoreversion recursesubdirs; Components: "wxmaxima"

; xmaxima
Source: "/usr/local\bin\xmaxima.*"; DestDir: "{app}\bin\"; Flags: ignoreversion; Components: "xmaxima"
Source: "/usr/local\bin\xmaxima"; DestDir: "{app}\bin\"; Flags: ignoreversion; Components: "xmaxima"

; lang\spanish
Source: "/usr/local\info\es\*.*"; DestDir: "{app}\info\es\";  Flags: recursesubdirs;  Components: "lang\spanish"
Source: "/usr/local\share\maxima\5.14.0cvs\doc\chm\es\maxima.chm"; DestDir: "{app}\share\maxima\5.14.0cvs\doc\chm\es\"; Components: "lang\spanish"
; lang\portuguese
Source: "/usr/local\info\pt\*.*"; DestDir: "{app}\info\pt\";  Flags: recursesubdirs;  Components: "lang\portuguese"
Source: "/usr/local\share\maxima\5.14.0cvs\doc\chm\pt\maxima.chm"; DestDir: "{app}\share\maxima\5.14.0cvs\doc\chm\pt\"; Components: "lang\portuguese"
; lang\brazilian
Source: "/usr/local\info\pt_BR\*.*"; DestDir: "{app}\info\pt_BR\";  Flags: recursesubdirs;  Components: "lang\brazilian"
Source: "/usr/local\share\maxima\5.14.0cvs\doc\chm\pt_BR\maxima.chm"; DestDir: "{app}\share\maxima\5.14.0cvs\doc\chm\pt_BR\"; Components: "lang\brazilian"


[Icons]
; core
Name: "{group}\{cm:CommandLineMaxima}"; Filename: "{app}\bin\maxima.bat"; IconFilename: "{app}\bin\maxima-command.ico";  Components: "core"
Name: "{group}\{cm:Introduction}"; Filename: "{app}\share\maxima\5.14.0cvs\doc\html\intromax.html"; WorkingDir: "{app}\share\maxima\5.14.0cvs";  Components: "core"
Name: "{group}\{cm:ReferenceManual}"; Filename: "{app}\share\maxima\5.14.0cvs\doc\chm\maxima.chm"; WorkingDir: "{app}\share\maxima\5.14.0cvs";  Components: "core"
Name: "{group}\{cm:ReadmeMenuEntry}"; Filename: "{app}\readme_{language}.txt"; Components: "core"

; wxmaxima
Name: "{group}\wxMaxima"; Filename: "{app}\wxMaxima\wxMaxima.exe"; WorkingDir: "{app}\wxMaxima";  Components: "wxmaxima"
Name: "{group}\{cm:ProgramOnTheWeb,wxMaxima}"; Filename: "{app}\wxMaxima\wxMaxima.url";  Components: "wxmaxima"
Name: "{group}\{cm:OnlineForum,wxMaxima}"; Filename: "{app}\wxMaxima\Forum.url";  Components: "wxmaxima"
Name: "{userdesktop}\wxMaxima"; Filename: "{app}\wxMaxima\wxMaxima.exe"; MinVersion: 4,4; Tasks: wxmdesktopicon; WorkingDir: "{app}\wxMaxima";  Components: "wxmaxima"

; xmaxima
Name: "{group}\XMaxima"; Filename: "{app}\bin\xmaxima.exe"; IconFilename: "{app}\share\maxima\5.14.0cvs\xmaxima\maxima-icon.ico";  Components: "xmaxima"
Name: "{userdesktop}\XMaxima"; Filename: "{app}\bin\xmaxima.exe"; MinVersion: 4,4; Tasks: xmdesktopicon; IconFilename: "{app}\share\maxima\5.14.0cvs\xmaxima\maxima-icon.ico";  Components: "xmaxima"

Name: "{group}\{cm:Uninstall}"; Filename:"{uninstallexe}"

; lang\spanish
Name: "{group}\{cm:ReferenceManualSpanish}"; Filename: "{app}\share\maxima\5.14.0cvs\doc\chm\es\maxima.chm"; WorkingDir: "{app}\share\maxima\5.14.0cvs";  Components: "lang\spanish"
; lang\portuguese
Name: "{group}\{cm:ReferenceManualPortuguese}"; Filename: "{app}\share\maxima\5.14.0cvs\doc\chm\pt\maxima.chm"; WorkingDir: "{app}\share\maxima\5.14.0cvs";  Components: "lang\portuguese"
; lang\brazilian
Name: "{group}\{cm:ReferenceManualBrazilianPortuguese}"; Filename: "{app}\share\maxima\5.14.0cvs\doc\chm\pt_BR\maxima.chm"; WorkingDir: "{app}\share\maxima\5.14.0cvs";  Components: "lang\brazilian"


;[Run]
;Filename: "{app}\bin\xmaxima.exe"; Description: "{cm:LaunchProgram,XMaxima}"; Flags: postinstall skipifsilent skipifdoesntexist
;Filename: "{app}\wxMaxima\wxMaxima.exe"; Description: "{cm:LaunchProgram,wxMaxima}"; Flags: postinstall skipifsilent skipifdoesntexist


[Registry]
Root: HKCR; Subkey: ".wxm"; ValueType: string; ValueName: ""; ValueData: "Maxima.wxMaxima"; Flags: uninsdeletekey
Root: HKCR; Subkey: "Maxima.wxMaxima"; ValueType: string; ValueName: ""; ValueData: "{cm:wxMaximaSession}"; Flags: uninsdeletekey
Root: HKCR; Subkey: "Maxima.wxMaxima\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\wxMaxima\wxMaxima.exe,0"
Root: HKCR; Subkey: "Maxima.wxMaxima\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\wxMaxima\wxMaxima.exe"" ""%1"""


[CustomMessages]


;=== English ===  LID=$0409  CodePage=0

FullInstallation=Full installation
CompactInstallation=Compact installation
CustomInstallation=Custom installation
Uninstall=Uninstall
wxMaximaSession=wxMaxima Session

MaximaCore=Maxima core with command line interface
wxMaximaGraphicShell=wxMaxima graphic shell
XMaximaGraphicShell=XMaxima graphic shell
MaximaLanguagePacks=Maxima language packs

German=German
French=French
Italian=Italian
Portuguese=Portuguese
BrazilianPortuguese=Brazilian Portuguese
Russian=Russian
Spanish=Spanish

CreateMyDesktopIcon=Create %1 desktop icon

CommandLineMaxima=Command line Maxima
Introduction=Introduction
ReferenceManual=Reference Manual
Readme=README
ReadmeMenuEntry=README

OnlineForum=%1 Online Forum

ReferenceManualGerman=Reference Manual (German)
ReferenceManualFrench=Reference Manual (French)
ReferenceManualItalian=Reference Manual (Italian)
ReferenceManualPortuguese=Reference Manual (Portuguese)
ReferenceManualBrazilianPortuguese=Reference Manual (Brazilian Portuguese)
ReferenceManualRussian=Reference Manual (Russian)
ReferenceManualSpanish=Reference Manual (Spanish)


;=== German ===  LID=$0407  CodePage=1252

de.FullInstallation=Vollst�ndige Installation
de.CompactInstallation=Kompakte Installation
de.CustomInstallation=Benutzerdefinierte Installation
de.Uninstall=Deinstallation
de.wxMaximaSession=wxMaxima-Sitzung

de.MaximaCore=Maxima-Programmkern mit Eingabeaufforderung 
de.wxMaximaGraphicShell=wxMaxima (graphische Oberfl�che)
de.XMaximaGraphicShell=XMaxima (graphische Oberfl�che)
de.MaximaLanguagePacks=Maxima-Sprachpakete

de.German=Deutsch
de.French=Franz�sich
de.Italian=Italienisch
de.Portuguese=Portugiesisch
de.BrazilianPortuguese=Brasilianisches Portugiesisch
de.Russian=Russisch
de.Spanish=Spanisch

de.CreateMyDesktopIcon=Erzeuge %1-Desktopsymbol

de.CommandLineMaxima=Maxima-Eingabeaufforderung
de.Introduction=Einf�hrung
de.ReferenceManual=Referenzhandbuch
de.Readme=LIESMICH
de.ReadmeMenuEntry=LIESMICH

de.OnlineForum=%1-Online-Forum

de.ReferenceManualGerman=Referenzhandbuch (Deutsch)
de.ReferenceManualFrench=Referenzhandbuch (Franz�sich)
de.ReferenceManualItalian=Referenzhandbuch (Italienisch)
de.ReferenceManualPortuguese=Referenzhandbuch (Portugiesisch)
de.ReferenceManualBrazilianPortuguese=Referenzhandbuch (Brasilianisches Portugiesisch)
de.ReferenceManualRussian=Referenzhandbuch (Russisch)
de.ReferenceManualSpanish=Referenzhandbuch (Spanisch)


;=== French ===  LID=$040C  CodePage=1252

fr.FullInstallation=Installation compl�te
fr.CompactInstallation=Installation compacte
fr.CustomInstallation=Installation personnalis�e
fr.Uninstall=D�sinstallation
fr.wxMaximaSession=Session wxMaxima

fr.MaximaCore=Noyau Maxima avec interface en ligne de commande
fr.wxMaximaGraphicShell=Interface graphique wxMaxima
fr.XMaximaGraphicShell=Interface graphique XMaxima
fr.MaximaLanguagePacks=Paquets de langues Maxima

fr.German=Allemand
fr.French=Fran�ais
fr.Italian=Italien
fr.Portuguese=Portugais
fr.BrazilianPortuguese=Portugais br�silien
fr.Russian=Russe
fr.Spanish=Espagnol

fr.CreateMyDesktopIcon=Cr�er un raccourci sur le bureau pour %1

fr.CommandLineMaxima=Ligne de commande Maxima
fr.Introduction=Introduction
fr.ReferenceManual=Manuel de r�f�rence
fr.Readme=LISEZMOI
fr.ReadmeMenuEntry=README

fr.OnlineForum=Forum en ligne %1

fr.ReferenceManualGerman=Manuel de r�f�rence (Allemand)
fr.ReferenceManualFrench=Manuel de r�f�rence (Fran�ais)
fr.ReferenceManualItalian=Manuel de r�f�rence (Italien)
fr.ReferenceManualPortuguese=Manuel de r�f�rence (Portugais)
fr.ReferenceManualBrazilianPortuguese=Manuel de r�f�rence (Portugais br�silien)
fr.ReferenceManualRussian=Manuel de r�f�rence (Russe)
fr.ReferenceManualSpanish=Manuel de r�f�rence (Espagnol)


;=== Italian ===  LID=$0410  CodePage=1252

it.FullInstallation=Installazione completa
it.CompactInstallation=Installazione compatta
it.CustomInstallation=Installazione personalizzata
it.Uninstall=Disinstallazione
it.wxMaximaSession=Sessione wxMaxima

it.MaximaCore=Nucleo di Maxima con interfaccia a riga di comando
it.wxMaximaGraphicShell=Shell grafica wxMaxima
it.XMaximaGraphicShell=Shell grafica XMaxima
it.MaximaLanguagePacks=Pacchetti lingua di Maxima

it.Germen=Tedesco
it.French=Francese
it.Italian=Italiano
it.Portuguese=Portoghese
it.BrazilianPortuguese=Portoghese brasiliano
it.Russian=Russo
it.Spanish=Spagnolo

it.CreateMyDesktopIcon=Crea %1 icona sul desktop

it.CommandLineMaxima=Maxima a riga di comando
it.Introduction=Introduzione
it.ReferenceManual=Manuale di riferimento
it.Readme=LEGGIMI
it.ReadmeMenuEntry=LEGGIMI

it.OnlineForum=%1 Forum online

it.ReferenceManualGerman=Manuale di Riferimento (Tedesco)
it.ReferenceManualFrench=Manuale di Riferimento (Francese)
it.ReferenceManualItalian=Manuale di Riferimento (Italiano)
it.ReferenceManualPortuguese=Manuale di Riferimento (Portoghese)
it.ReferenceManualBrazilianPortuguese=Manuale di Riferimento (Portoghese brasiliano)
it.ReferenceManualRussian=Manuale di Riferimento (Russo)
it.ReferenceManualSpanish=Manuale di Riferimento (Spagnolo)


;=== Portuguese (Brazilian) ===  LID=$0416  CodePage=1252

pt_BR.FullInstallation=Instala��o completa
pt_BR.CompactInstallation=Instala��o compacta
pt_BR.CustomInstallation=Instala��o personalizada
pt_BR.Uninstall=Desinstalar
pt_BR.wxMaximaSession=Sess�o do wxMaxima

pt_BR.MaximaCore=N�cleo do Maxima com interface de linha de comandos
pt_BR.wxMaximaGraphicShell=shell gr�fica wxMaxima
pt_BR.XMaximaGraphicShell=shell gr�fica XMaxima
pt_BR.MaximaLanguagePacks=pacotes lingu�sticos do Maxima

pt_BR.German=Alem�o
pt_BR.French=Franc�s
pt_BR.Italian=Italiano
pt_BR.Portuguese=Portugu�s
pt_BR.BrazilianPortuguese=Portugu�s do Brasil
pt_BR.Russian=Russo
pt_BR.Spanish=Espanhol

pt_BR.CreateMyDesktopIcon=Criar �cone na �rea de Trabalho para %1

pt_BR.CommandLineMaxima=Maxima em Linha de Comandos
pt_BR.Introduction=Introdu��o 
pt_BR.ReferenceManual=Manual de Refer�ncia
pt_BR.Readme=LEIAME
pt_BR.ReadmeMenuEntry=LEIAME

pt_BR.OnlineForum=F�rum na internet para %1

pt_BR.ReferenceManualGerman=Manual de Refer�ncia (Alem�o)
pt_BR.ReferenceManualFrench=Manual de Refer�ncia (Franc�s)
pt_BR.ReferenceManualItalian=Manual de Refer�ncia (Italiano)
pt_BR.ReferenceManualPortuguese=Manual de Refer�ncia (Portugu�s)
pt_BR.ReferenceManualBrazilianPortuguese=Manual de Refer�ncia (Portugu�s do Brasil)
pt_BR.ReferenceManualRussian=Manual de Refer�ncia (Russo)
pt_BR.ReferenceManualSpanish=Manual de Refer�ncia (Espanhol)


;=== Portuguese ===  LID=$0816  CodePage=1252

pt.FullInstallation=Instala��o completa
pt.CompactInstallation=Instala��o compacta
pt.CustomInstallation=Instala��o personalizada
pt.Uninstall=Desinstalar
pt.wxMaximaSession=Sess�o do wxMaxima

pt.MaximaCore=N�cleo do Maxima com interface de linha de comandos
pt.wxMaximaGraphicShell=shell gr�fica wxMaxima
pt.XMaximaGraphicShell=shell gr�fica XMaxima
pt.MaximaLanguagePacks=pacotes lingu�sticas do Maxima

pt.German=Alem�o
pt.French=Franc�s
pt.Italian=Italiano
pt.Portuguese=Portugu�s
pt.BrazilianPortuguese=Portugu�s do Brasil
pt.Russian=Russo
pt.Spanish=Espanhol

pt.CreateMyDesktopIcon=Criar �cone no escrit�rio para %1

pt.CommandLineMaxima=Maxima em Linha de Comandos
pt.Introduction=Introdu��o 
pt.ReferenceManual=Manual de Refer�ncia
pt.Readme=LEIAME
pt.ReadmeMenuEntry=LEIAME

pt.OnlineForum=F�rum na rede para %1

pt.ReferenceManualGerman=Manual de Refer�ncia (Alem�o)
pt.ReferenceManualFrench=Manual de Refer�ncia (Franc�s)
pt.ReferenceManualItalian=Manual de Refer�ncia (Italiano)
pt.ReferenceManualPortuguese=Manual de Refer�ncia (Portugu�s)
pt.ReferenceManualBrazilianPortuguese=Manual de Refer�ncia (Portugu�s do Brasil)
pt.ReferenceManualRussian=Manual de Refer�ncia (Russo)
pt.ReferenceManualSpanish=Manual de Refer�ncia (Espanhol)


;=== Russian ===  LID=$0419  CodePage=1251

ru.FullInstallation=������ ���������
ru.CompactInstallation=���������� ���������
ru.CustomInstallation=���������� ���������
ru.Uninstall=�������������
ru.wxMaximaSession=������ wxMaxima

ru.MaximaCore=���� Maxima � ����������� ��������� ������
ru.wxMaximaGraphicShell=����������� �������� wxMaxima
ru.XMaximaGraphicShell=����������� �������� XMaxima
ru.MaximaLanguagePacks=������ ��������� ������ Maxima

ru.German=��������
ru.French=�����������
ru.Italian=�����������
ru.Portuguese=�������������
ru.BrazilianPortuguese=����������� �������������
ru.Russian=�������
ru.Spanish=���������

ru.CreateMyDesktopIcon=������� ������ %1 �� ������� �����

ru.CommandLineMaxima=Maxima � ��������� ������
ru.Introduction=��������
ru.ReferenceManual=���������� �����������
ru.Readme=README
ru.ReadmeMenuEntry=README

ru.OnlineForum=����� %1

ru.ReferenceManualGerman=���������� ����������� (��������)
ru.ReferenceManualFrench=���������� ����������� (�����������)
ru.ReferenceManualItalian=���������� ����������� (�����������)
ru.ReferenceManualPortuguese=���������� ����������� (�������������)
ru.ReferenceManualBrazilianPortuguese=���������� ����������� (����������� �������������)
ru.ReferenceManualRussian=���������� ����������� (�������)
ru.ReferenceManualSpanish=���������� ����������� (���������)


;=== Spanish ===  LID=$0c0a  CodePage=1252

es.FullInstallation=Instalaci�n Completa
es.CompactInstallation=Instalaci�n Compacta
es.CustomInstallation=Instalaci�n Personalizada
es.Uninstall=Desinstalar
es.wxMaximaSession=Sesi�n de wxMaxima

es.MaximaCore=N�cleo de Maxima e interfaz de l�nea de comandos
es.wxMaximaGraphicShell=Entorno gr�fico wxMaxima
es.XMaximaGraphicShell=Entorno gr�fico XMaxima
es.MaximaLanguagePacks=Internacionalizaci�n de Maxima

es.German=Alem�n
es.French=Franc�s
es.Italian=Italiano
es.Portuguese=Portugu�s
es.BrazilianPortuguese=Portugu�s Brasile�o
es.Russian=Ruso
es.Spanish=Espa�ol

es.CreateMyDesktopIcon=Crear icono de escritorio

es.CommandLineMaxima=L�nea de comandos de Maxima
es.Introduction=Introducci�n
es.ReferenceManual=Manual de Referencia
es.Readme=LEAME
es.ReadmeMenuEntry=LEAME

es.OnlineForum=Foro en l�nea para %1

es.ReferenceManualGerman=Manual de Referencia (Alem�n)
es.ReferenceManualFrench=Manual de Referencia (Franc�s)
es.ReferenceManualItalian=Manual de Referencia (Italiano)
es.ReferenceManualPortuguese=Manual de Referencia (Portugu�s)
es.ReferenceManualBrazilianPortuguese=Manual de Referencia (Portugu�s Brasile�o)
es.ReferenceManualRussian=Manual de Referencia (Ruso)
es.ReferenceManualSpanish=Manual de Referencia (Espa�ol)

;=============


[Code]

{ Check if a path contains spaces.  If it does, convert it to
  the equivalent short path }
function PathWithoutSpaces( strIn: String): String;
begin
  if (Pos(' ',strIn) = 0) then
    Result := strIn
  else
    Result := GetShortName(strIn);
end;


{ Based on code from Inno Setup Extensions Knowledge Base
  Article 14 - How to replace a line in a text file
  http://www13.brinkster.com/vincenzog/isxart.asp?idart=14
  Author: Stefan Bracke }

{ Note: Functions called by AfterInstall can have maximum of one argument }

procedure ReplaceApp();
var
  iLineCounter : Integer;
  a_strTextfile : TArrayOfString;
  strFilename : String;
  strFind : String;
  strNewLine : String;
  strApp : String;
begin
  strApp := ExpandConstant('{app}');
  strFilename := strApp + '\bin\maxima.bat';
  strFind := 'set maxima_prefix';
  strNewLine := 'set maxima_prefix='+PathWithoutSpaces(strApp);
  
  { Load textfile into string array }
  LoadStringsFromFile(strFilename, a_strTextfile);
  
  { Search through all textlines for given text }
  for iLineCounter := 0 to GetArrayLength(a_strTextfile)-1 do
    begin
      { Overwrite textline when text searched for is part of it }
      if (Pos(strFind, a_strTextfile[iLineCounter]) > 0) then
        a_strTextfile[iLineCounter] := strNewLine;
    end;

  { Save string array to textfile (overwrite, no append!) }
  SaveStringsToFile(strFilename, a_strTextfile, False);

end;

