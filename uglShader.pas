unit uglShader;

interface

uses
  DGLOpenGL, SysUtils, Classes;

type
  TLogEvent = procedure(Sender: TObject; const Msg: String) of Object;

  glShaderException = class(Exception);
  TglShaderObject = class(TObject)
  private
    //Handle des Shader-Objekts
    fShaderObj: GLHandle;
    //Typ des Shaders
    fShaderType: Cardinal;
    //Code des Shaders
    fCode: String;
    //Event das zum loggen von Nachrichten genutzt wird
    fOnLog: TLogEvent;

    function GetInfoLog(aObj: GLHandle): String;
    function GetCompiled: Boolean;
    procedure Log(const aMsg: String);
  public
    property ShaderObj : GLHandle  read fShaderObj;
    property ShaderType: Cardinal  read fShaderType;
    property Code      : String    read fCode  write fCode;
    property OnLog     : TLogEvent read fOnLog write fOnLog;
    property Compiled  : Boolean   read GetCompiled;
    
    procedure Compile;

    constructor Create(aShaderType: Cardinal; aLogEvent: TLogEvent = nil);
    destructor Destroy; override;
  end;

  TglShaderObjectList = class(TObject)
  private
    function GetShaderObject(aID: Integer): TglShaderObject;
    function GetCount: Integer;
    function GetLast: TglShaderObject;
  protected
    fList: TList;
  public
    property ShaderObjects[Index: Integer]: TglShaderObject read GetShaderObject; default;
    property Count: Integer         read GetCount;
    property Last : TglShaderObject read GetLast;

    procedure Add(aShaderObj: TglShaderObject); virtual;
    procedure Delete(aID: Integer; aFreeOwnedObj: Boolean = True); virtual;
    procedure Clear(aFreeOwnedObj: Boolean = True);
    function Find(aShaderObj: TglShaderObject): Integer;

    constructor Create;
    destructor Destroy; override;
  end;

  TglShaderProgram = class(TglShaderObjectList)
  private
    //Handle des Programm-Objekts
    fProgramObj: GLHandle;
    //Event das zum loggen von Nachrichten genutzt wird
    fOnLog: TLogEvent;
    //Dateiname der Datei, aus der die Shader geladen wurden
    fFilename: String;

    function GetUniformLocation(const aName: String; out aPos: glInt): Boolean;
    function GetInfoLog(Obj: GLHandle): String;
    function GetCompiled: Boolean;
    function GetLinked: Boolean;
    procedure Log(const msg: String);
  public
    property ProgramObj: glHandle read fProgramObj;
    property OnLog     : TLogEvent read fOnLog write fOnLog;
    property Filename  : String    read fFilename;
    property Compiled  : Boolean   read GetCompiled;
    property Linked    : Boolean   read GetLinked;

    procedure Compile;
    procedure Enable;
    procedure Disable;

    procedure Add(ShaderObj: TglShaderObject); override;
    procedure Delete(ID: Integer; FreeOwnedObj: Boolean = True); override;

    function Uniform1f(const aName: String; aP1: GLFloat): Boolean;
    function Uniform2f(const aName: String; aP1, aP2: GLFloat): Boolean;
    function Uniform3f(const aName: String; aP1, aP2, aP3: GLFloat): Boolean;
    function Uniform4f(const aName: String; aP1, aP2, aP3, aP4: GLFloat): Boolean;
    function Uniform1i(const aName: String; aP1: GLint): Boolean;
    function Uniform2i(const aName: String; aP1, aP2: GLint): Boolean;
    function Uniform3i(const aName: String; aP1, aP2, aP3: GLint): Boolean;
    function Uniform4i(const aName: String; aP1, aP2, aP3, aP4: GLint): Boolean;
    function Uniform1fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
    function Uniform2fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
    function Uniform3fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
    function Uniform4fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
    function Uniform1iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
    function Uniform2iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
    function Uniform3iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
    function Uniform4iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
    function UniformMatrix2fv(const aName: String; aTranspose: Boolean; aCount: GLint; aP1: PGLFloat): Boolean;
    function UniformMatrix3fv(const aName: String; aTranspose: Boolean; aCount: GLint; aP1: PGLFloat): Boolean;
    function UniformMatrix4fv(const aName: String; aTranspose: Boolean; aCount: GLint; aP1: PGLFloat): Boolean;

    function GetUniformfv(const aName: String; var aP: PGLfloat): Boolean;
    function GetUniformfi(const aName: String; var aP: PGLint): Boolean;

    procedure LoadFromFile(aFilename: String);
    procedure LoadFromStream(aStream: TStream);
    procedure SaveToFile(aFilename: String);
    procedure SaveToStream(aStream: TStream);

    constructor Create(aLogEvent: TLogEvent = nil);
    destructor Destroy; override;
  end;

implementation

const
  ERROR_STR_VAR_NAME: String = 'can''t find the variable ''%s'' in the program';
  GLSL_FILE_HEADER  : String = 'glslShaderFile';

//schreibt einen String in einen Stream
//@str: String der geschrieben werden soll;
//@stream: Stream in den geschrieben werden soll;
procedure WriteString(const str: String; const Stream: TStream);
var
  len: Integer;
begin
  len := Length(str);
  Stream.Write(len, SizeOf(len));
  if len > 0 then
    Stream.Write(str[1], len);
end;

//schreibt einen Integer in einen Stream
//@i: Integer der geschreieben werden soll;
//@Stream: Strem in den geschrieben werden soll;
procedure WriteInt(const i: Integer; const Stream: TStream);
begin
  Stream.Write(i, SizeOf(i));
end;

//ließt einen String aus einem Stream
//@Stream: Stream aus dem gelesen werden soll;
//@result gelesener String;
function ReadString(const aStream: TStream): String;
var
  len: Integer;
begin
  aStream.Read(len, SizeOf(len));
  SetLength(result, len);
  if len > 0 then
    aStream.Read(result[1], len);
end;

//ließt einen Integer aus einem Stream
//@Stream: Stream aus dem gelesen werden soll;
//@result gelesener Integer;
function ReadInt(const aStream: TStream): Integer;
begin
  aStream.Read(result, SizeOf(result));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//glShaderObject////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRI//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//ließt das Log eines OpenGL-Objekts aus
//@Obj: Handle des Objekts, dessen Log ausgelesen werden soll;
//@result: Log des Objekts;
function TglShaderObject.GetInfoLog(aObj: GLHandle): String;
var
  Msg: PChar;
  bLen, sLen: GLint;
begin
  bLen := 0;
  glGetShaderiv(aObj, GL_INFO_LOG_LENGTH, @bLen);
  if bLen > 1 then begin
    GetMem(Msg, bLen * SizeOf(Char));
    glGetShaderInfoLog(aObj, bLen, sLen, Msg);
    result := PChar(Msg);
    Dispose(Msg);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//ließt aus, ob der Shader ohne Fehler kompiliert wurde
//@result: TRUE wenn ohne Fehler kompiliert, sonst FALSE;
function TglShaderObject.GetCompiled: Boolean;
var
  value: glInt;
begin
  glGetShaderiv(fShaderObj, GL_COMPILE_STATUS, @value);
  result := (value = GL_TRUE);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//ruft das Log-Event auf, wenn es gesetzt ist
//@msg: Nachricht die geloggt werden soll;
procedure TglShaderObject.Log(const aMsg: String);
begin
  if Assigned(fOnLog) then begin
    fOnLog(self, aMsg);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBL//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//kompiliert das Shader-Objekt
procedure TglShaderObject.Compile;
var
  len, i: GLint;
  List: TStringList;
begin
  len := Length(fCode);
  if len > 0 then begin
    glShaderSource(fShaderObj, 1, @fCode, @len);
    glCompileShader(fShaderObj);
    List := TStringList.Create;
    List.Text := GetInfoLog(fShaderObj);
    for i := 0 to List.Count-1 do
      Log(List[i]);
    List.Free;
  end else Log('error while compiling: no bound shader code');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//erzeugt das Objekt
//@ShaderType: Typ des Shader-Objekts;
//@LogEvent: Event zum loggen von Fehlern und Ereignissen;
//@raise: glShaderException wenn der Shadertyp unbekannt oder ungültig ist;
constructor TglShaderObject.Create(aShaderType: Cardinal; aLogEvent: TLogEvent = nil);
begin
  inherited Create;

  fCode       := '';
  fOnLog      := aLogEvent;
  fShaderType := aShaderType;
  fShaderObj  := glCreateShader(fShaderType);
  if fShaderObj = 0 then
    raise glShaderException.Create('TglShaderObject.Create - can''t create ShaderObject');
    
  Log('shader object created: #'+IntToHex(fShaderObj, 4));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//gibt das Objekt frei
destructor TglShaderObject.Destroy;
begin
  glDeleteShader(fShaderObj);

  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//glShaderObjectList////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRI//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//holt ein ShaderObjekt aus der Liste
//@raise: glShaderException wenn ID außerhalb der Grenzen der Liste;
function TglShaderObjectList.GetShaderObject(aID: Integer): TglShaderObject;
begin
  if (aID >= 0) and (aID < fList.Count) then
    result := TglShaderObject(fList[aID])
  else
    raise glShaderException.Create('TglShaderObjectList.GetShaderObject - index out of bounds ('+IntToStr(aID)+')');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Get-Methode für die Count-Eigenschaft
//@result: Anzahl an Objekte in der Liste
function TglShaderObjectList.GetCount: Integer;
begin
  result := fList.Count;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Get-Methode für die Last-Eigenschaft
//@result: Element, das zuletzt in die Liste eingefügt wurde
function TglShaderObjectList.GetLast: TglShaderObject;
begin
  result := TglShaderObject(fList.Last);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBL//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//fügt der Liste einen Shader hinzu
//@ShaderObj: Objekt, das hinzugefügt werden soll;
procedure TglShaderObjectList.Add(aShaderObj: TglShaderObject);
begin
  fList.Add(aShaderObj);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//löscht ein ShaderObjekt aus der Liste
//@ID: Index des Objekts, das gelöscht werden soll;
//@FreeOwnedObj: wenn TRUE wird das gelöschte Objekt freigegeben;
//@raise: glShaderException wenn ID außerhalb der Grenzen der Liste;
procedure TglShaderObjectList.Delete(aID: Integer; aFreeOwnedObj: Boolean = True);
begin
  if (aID >= 0) and (aID < fList.Count) then begin
    if aFreeOwnedObj then
      TglShaderObject(fList[aID]).Free;
    fList.Delete(aID);
  end else
    raise glShaderException.Create('TglShaderObjectList.Delete - index out of bounds ('+IntToStr(aID)+')');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//löscht alle ShaderObjekte ind er Liste
//@FreeOwnedObj: wenn TRUE wird das gelöschte Objekt freigegeben;
procedure TglShaderObjectList.Clear(aFreeOwnedObj: Boolean = True);
begin
  while Count > 0 do
    Delete(Count-1, aFreeOwnedObj);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//sucht nach einem Objekt in der Liste
//@ShaderObj: Objekt nach dem gesucht werden soll;
//@ID: Index des gesuchten Objekts, oder -1 wenn nichts gefunden wurde; 
function TglShaderObjectList.Find(aShaderObj: TglShaderObject): Integer;
var
 i: Integer;
begin
  result := -1;
  for i := 0 to Count-1 do
    if ShaderObjects[i] = aShaderObj then begin
      result := 0;
      exit;
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//erzeugt das Objekt
constructor TglShaderObjectList.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//gibt das Objekt und alle ShaderObjekte frei
//um die ShaderObjekte später noch zu verwenden "Clear(false)" aufrufen 
destructor TglShaderObjectList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//glShaderProgram///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRIVATE//PRI//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TglShaderProgram.GetUniformLocation(const aName: String; out aPos: glInt): Boolean;
var
  e: Cardinal;
begin
  aPos := glGetUniformLocation(fProgramObj, PAnsiChar(aName));
  result := (aPos <> -1);
  if not result then
    Log(StringReplace(ERROR_STR_VAR_NAME, '%s', aName, [rfIgnoreCase, rfReplaceAll]));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//ließt das Log eines OpenGL-Objekts aus
//@Obj: Handle des Objekts, dessen Log ausgelesen werden soll;
//@result: Log des Objekts;
function TglShaderProgram.GetInfoLog(Obj: GLHandle): String;
var
  Msg: PChar;
  bLen, sLen: GLint;
begin
  bLen := 0;
  glGetProgramiv(Obj, GL_INFO_LOG_LENGTH, @bLen);
  if bLen > 1 then begin
    GetMem(Msg, bLen * SizeOf(Char));
    glGetProgramInfoLog(Obj, bLen, sLen, Msg);
    result := PChar(Msg);
    Dispose(Msg);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//prüft ob alle Shader ohne Fehler compiliert wurden 
//@result: TRUE wenn alle erfolgreich compiliert, sonst FALSE;
function TglShaderProgram.GetCompiled: Boolean;
var
  i: Integer;
begin
  result := true;
  for i := 0 to Count-1 do
    result := result and ShaderObjects[i].Compiled; 
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//prüft ob das Programm ohne Fehler gelinkt wurde
//@result: TRUE wenn linken erfolgreich, sonst FASLE;
function TglShaderProgram.GetLinked: Boolean;
var
  value: glInt;
begin
  glGetProgramiv(fProgramObj, GL_LINK_STATUS, @value);
  result := (value = GL_TRUE);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//ruft das Log-Event auf, wenn es gesetzt ist
//@msg: Nachricht die geloggt werden soll;
procedure TglShaderProgram.Log(const msg: String);
begin
  if Assigned(fOnLog) then begin
    fOnLog(self, msg);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBLIC//PUBL//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Kompiliert den Shader-Code
procedure TglShaderProgram.Compile;
var
  i: Integer;
  List: TStringList;
begin
  for i := 0 to Count-1 do
    ShaderObjects[i].Compile;
  glLinkProgram(fProgramObj);
  List := TStringList.Create;
  List.Text := GetInfoLog(fProgramObj);
  for i := 0 to List.Count-1 do
    Log(List[i]);
  List.Free;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//aktiviert den Shader
procedure TglShaderProgram.Enable;
begin
  glUseProgram(fProgramObj);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//deaktiviert den Shader
procedure TglShaderProgram.Disable;
begin
  glUseProgram(0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//fügt der Liste einen Shader hinzu
//@ShaderObj: Objekt, das hinzugefügt werden soll;
procedure TglShaderProgram.Add(ShaderObj: TglShaderObject);
begin
  inherited Add(ShaderObj);
  glAttachShader(fProgramObj, ShaderObj.fShaderObj);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//löscht ein ShaderObjekt aus der Liste
//@ID: Index des Objekts, das gelöscht werden soll;
//@FreeOwnedObj: wenn TRUE wird das gelöschte Objekt freigegeben;
procedure TglShaderProgram.Delete(ID: Integer; FreeOwnedObj: Boolean = True);
begin
  if (ID >= 0) and (ID < Count) then
    glDetachShader(fProgramObj, ShaderObjects[ID].fShaderObj);
  inherited Delete(ID, FreeOwnedObj);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 1-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform1f(const aName: String; aP1: GLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform1f(pos, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 2-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@p2: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform2f(const aName: String; aP1, aP2: GLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform2f(pos, aP1, aP2);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 3-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@p2: Wert der Variable, der gesetzt werden soll;
//@p3: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform3f(const aName: String; aP1, aP2, aP3: GLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform3f(pos, aP1, aP2, aP3);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 4-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@p2: Wert der Variable, der gesetzt werden soll;
//@p3: Wert der Variable, der gesetzt werden soll;
//@p4: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform4f(const aName: String; aP1, aP2, aP3, aP4: GLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform4f(pos, aP1, aP2, aP3, aP4);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 1-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform1i(const aName: String; aP1: GLint): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform1i(pos, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 2-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform2i(const aName: String; aP1, aP2: GLint): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform2i(pos, aP1, aP2);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 3-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@p2: Wert der Variable, der gesetzt werden soll;
//@p3: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform3i(const aName: String; aP1, aP2, aP3: GLint): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform3i(pos, aP1, aP2, aP3);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen 4-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@p1: Wert der Variable, der gesetzt werden soll;
//@p2: Wert der Variable, der gesetzt werden soll;
//@p3: Wert der Variable, der gesetzt werden soll;
//@p4: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform4i(const aName: String; aP1, aP2, aP3, aP4: GLint): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform4i(pos, aP1, aP2, aP3, aP4);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 1-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform1fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform1fv(pos, aCount, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 2-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform2fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform2fv(pos, aCount, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 3-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform3fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform3fv(pos, aCount, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 4-Komponenten Float-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform4fv(const aName: String; aCount: GLint; aP1: PGLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform4fv(pos, aCount, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 1-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform1iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform1iv(pos, aCount, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 2-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform2iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform2iv(pos, aCount, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 3-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform3iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform3iv(pos, aCount, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt einen oder mehrere 4-Komponenten Integer-Vektoren an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@count: Anzahl an Parametern auf die p1 zeigt;
//@p1: Zeiger auf den ersten Wert der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.Uniform4iv(const aName: String; aCount: GLint; aP1: PGLInt): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniform4iv(pos, aCount, aP1) ;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt eine oder mehrere 2x2-Matrizen an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@Transpose: wenn TRUe wird die matrix vor der Übergabe transponiert;
//@Count: Anzahl der zu übergebenden Elemente;
//@p1: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.UniformMatrix2fv(const aName: String; aTranspose: Boolean; aCount: GLint; aP1: PGLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniformMatrix2fv(pos, aCount, aTranspose, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt eine oder mehrere 3x3-Matrizen an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@Transpose: wenn TRUe wird die matrix vor der Übergabe transponiert;
//@Count: Anzahl der zu übergebenden Elemente;
//@p1: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.UniformMatrix3fv(const aName: String; aTranspose: Boolean; aCount: GLint; aP1: PGLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniformMatrix3fv(pos, aCount, aTranspose, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//übergibt eine oder mehrere 4x4-Matrizen an den Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gesetzt werden soll;
//@Transpose: wenn TRUe wird die matrix vor der Übergabe transponiert;
//@Count: Anzahl der zu übergebenden Elemente;
//@p1: Wert der Variable, der gesetzt werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.UniformMatrix4fv(const aName: String; aTranspose: Boolean; aCount: GLint; aP1: PGLFloat): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glUniformMatrix4fv(pos, aCount, aTranspose, aP1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//holt den Wert einer Float-Uniform-Variable aus dem Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gelesen werden soll;
//@p: Zeiger auf die Variable, in die der gelesene Wert geschrieben werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.GetUniformfv(const aName: String; var aP: PGLfloat): Boolean;
var
  pos: GLint;
begin
  if GetUniformLocation(aName, pos) then
    glGetUniformfv(fProgramObj, pos, aP);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//holt den Wert einer Integer-Uniform-Variable aus dem Shader
//!!!Der Shader muss dazu aktiviert sein!!!
//@Name: Name der Variablen die gelesen werden soll;
//@p: Zeiger auf die Variable, in die der gelesene Wert geschrieben werden soll;
//@result: TRUE wenn erfolgreich, sonst FALSE (Variablenname konnte nicht aufgelöst werden);
function TglShaderProgram.GetUniformfi(const aName: String; var aP: PGLint): Boolean;
var
  pos: GLint;
begin
  result := GetUniformLocation(aName, pos);
  if result then
    glGetUniformiv(fProgramObj, pos, aP);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//läd den Shader aus einer Datei
//@Filename: Datei aus der gelesen werden soll;
//@raise: glShaderException, wenn Datei nicht vorhanden ist;
procedure TglShaderProgram.LoadFromFile(aFilename: String);
var
  Stream: TFileStream;
begin
  if FileExists(aFilename) then begin
    Stream := TFileStream.Create(aFilename, fmOpenRead);
    try
      LoadFromStream(Stream);
      fFilename := aFilename;
    finally
      Stream.Free;
    end;
  end else raise glShaderException.Create('TglShaderProgram.LoadFromFile - file not found: '+Filename);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//läd den Shader aus einem Stream
//@Stream: Stream aus dem gelesen werden soll;
//@raise: glShaderException wenn kein Stream-Objekt übergeben wurde;
procedure TglShaderProgram.LoadFromStream(aStream: TStream);
var
  i, v, c: Integer;                  
begin
  if Assigned(aStream) then begin
    Clear;
    if ReadString(aStream) <> GLSL_FILE_HEADER then
      raise glShaderException.Create('TglShaderProgram.SaveToStream - incompatible file');
    v := ReadInt(aStream);

    if v >= 100 then begin //version 1.00
      c := ReadInt(aStream);
      for i := 0 to c-1 do begin
        Add(TglShaderObject.Create(Cardinal(ReadInt(aStream)), fOnLog));
        Last.fCode := ReadString(aStream);
      end;
    end;

    fFilename := '';
  end else raise glShaderException.Create('TglShaderProgram.SaveToStream - stream is nil');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//speichert den Shader in einer Datei
//@Filename: Datei in die geschrieben werden soll;
procedure TglShaderProgram.SaveToFile(aFilename: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(aFilename, fmCreate);
  try
    SaveToStream(Stream);
    fFilename := aFilename;
  finally
    Stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//speichert den Shader in einen Stream
//@Stream: Stream in den geschrieben werden soll;
//@raise: glShaderException wenn kein Stream-Objekt übergeben wurde;
//@raise: glShaderException wenn ungültige Datei;
procedure TglShaderProgram.SaveToStream(aStream: TStream);
var
  i: Integer;
  sObj: TglShaderObject;
begin
  if Assigned(aStream) then begin
    WriteString(GLSL_FILE_HEADER, aStream);
    WriteInt(100, aStream); //version 1.00
    WriteInt(Count, aStream);
    for i := 0 to Count-1 do begin
      sObj := ShaderObjects[i];
      WriteInt(Integer(sObj.fShaderType), aStream);
      WriteString(sObj.fCode, aStream);
    end;

    fFilename := '';
  end else raise glShaderException.Create('TglShaderProgram.LoadFromStream - stream is nil');
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//erzeugt das Objekt
//@LogEvent: Event zum loggen von Fehlern und Ereignissen;
//@raise: glShaderException wenn OpenGL nicht initialisiert werden konnte;
//@raise:
constructor TglShaderProgram.Create(aLogEvent: TLogEvent = nil);
begin
  if GL_LibHandle = nil then
    raise glShaderException.Create('TglShaderProgram.Create - OpenGL not initialized');

  if wglGetCurrentDC() = 0 then
    raise glShaderException.Create('TglShaderProgram.Create - no valid device context');

  if wglGetCurrentContext() = 0 then
    raise glShaderException.Create('TglShaderProgram.Create - no valid render context');

  inherited Create;
  fProgramObj := glCreateProgram();
  fOnLog      := aLogEvent;
  fFilename   := '';

  Log('shader program created: #'+IntToHex(fProgramObj, 4));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//gibt das Objekt frei
destructor TglShaderProgram.Destroy;
begin
  glDeleteProgram(fProgramObj);

  inherited Destroy;
end;

end.
