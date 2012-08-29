unit uFonts;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Graphics, dglOpenGL, TextSuite, Types;

type
  TtsFont = class
  private
    FFont: tsFontID;
  public
    constructor Create(Name: string; Size: integer; AA: boolean; Style: TFontStyles = []);
    procedure TextOut(X, Y: Single; Str: string; HAlign: Cardinal=TS_ALIGN_LEFT);
    procedure BlockOut(X, Y, Width, Height: Integer; str: String); overload;
    procedure BlockOut(Rect: TRect; str: String); overload;

    class procedure InitTS;
    class procedure DoneTS;
  end;

implementation

uses
  Dialogs;

var
  FFontContext: tsContextID;

  { TtsFont }

procedure TextSuiteCheckError;
var
  Error: tsError;
begin
  Error:= tsGetError;
  if Error <> TS_NO_ERROR then begin
    ShowMessage('TextSuite error: ' + tsGetErrorStringA(Error) + ' (0x' + IntToHex(tsGetErrorFunction, 4) + ')');
    Halt;
  end;

  Error:= glGetError;
  if Error <> GL_NO_ERROR then begin
    ShowMessage('OpenGL error: ' + gluGetString(Error));
    Halt;
  end;
end;

class procedure TtsFont.InitTS;
begin
  if tsInit(TS_INIT_TEXTSUITE) = TS_FALSE then begin
    ShowMessage('Initialing TextSuite failed.');
    Halt;
  end;

  if tsInit(TS_INIT_OPENGL) = TS_FALSE then begin
    ShowMessage('Initialing OpenGL failed.');
    Halt;
  end;

  if tsInit(TS_INIT_GDI) = TS_FALSE then begin
    ShowMessage('Initialing GDI failed.');
    Halt;
  end;

  tsContextCreate(@FFontContext);

  tsSetParameteri(TS_RENDERER, TS_RENDERER_OPENGL);
  TextSuiteCheckError;
end;

class procedure TtsFont.DoneTS;
begin
  tsContextDestroy(FFontContext);

  tsQuit;
end;

constructor TtsFont.Create(Name: string; Size: integer; AA: boolean; Style: TFontStyles);
  function IsInStyle(f: TFontStyle): byte;
  begin
    if f in Style then
      Result:= $FF
    else
      Result:= 0;
  end;
var
  a: Cardinal;
begin
  tsSetParameteri(TS_CREATOR, TS_CREATOR_GDI_FACENAME);
  TextSuiteCheckError;

  if AA then
    a:= TS_ANTIALIASING_NORMAL
  else
    a:= TS_ANTIALIASING_NONE;
  tsFontCreateCreatorA(
    PAnsiChar(Name),
    Size,
    TS_STYLE_NORMAL or
    (TS_STYLE_BOLD and IsInStyle(fsBold)) or
    (TS_STYLE_ITALIC and IsInStyle(fsItalic)) or
    (TS_STYLE_UNDERLINE and IsInStyle(fsUnderline)) or
    (TS_STYLE_STRIKEOUT and IsInStyle(fsStrikeOut)),
    a,
    TS_DEFAULT,
    @FFont);
  TextSuiteCheckError;
end;

procedure TtsFont.TextOut(X, Y: Single; Str: string; HAlign: Cardinal);
begin
  tsFontBind(FFont);
  tsSetParameteri(TS_ALIGN, HAlign);

  glPushMatrix;
    glTranslatef(x, y, 0);
    tsTextOutA(PAnsiChar(Str));
    glDisable(GL_TEXTURE_2D);
  glPopMatrix;
end;

procedure TtsFont.BlockOut(X, Y, Width, Height: Integer; str: String);
begin
  tsFontBind(FFont);
  tsTextBeginBlock(X, Y, Width, Height, TS_BLOCKFLAG_NONE);
    tsTextOutA(PAnsiChar(str));
  tsTextEndBlock;
end;

procedure TtsFont.BlockOut(Rect: TRect; str: String);
begin
  BlockOut(Rect.Left, Rect.Top, Rect.Right-Rect.Left, Rect.Bottom-Rect.Top, str);
end;

end.

