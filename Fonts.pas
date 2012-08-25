unit Fonts;

interface

uses
  Windows, Graphics, dglOpenGL, TextSuite;

type
  TtsFont = class
  private
    FFont: tsFontID;
  public
    constructor Create(Name:String;DC:HDC; Height:integer;Style:TFontStyles=[]);
    procedure TextOut(X,Y: Single; Str: String);

    class procedure InitTS;
  end;

implementation

{ TGLFont }


{ TtsFont }

procedure TViewFrame.TextSuiteCheckError;
var
  Error: tsError;
begin
  Error := tsGetError;
  if Error <> TS_NO_ERROR then begin
    ShowMessage('TextSuite error: ' + tsGetErrorStringA(Error) + ' (0x' + IntToHex(tsGetErrorFunction, 4) + ')');
    Halt;
  end;

  Error := glGetError;
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

  tsSetParameteri(TS_CREATOR, TS_CREATOR_GDI_FACENAME);
  TextSuiteCheckError;

  tsFontCreateCreatorA('Arial', 35, TS_STYLE_NORMAL, TS_FALSE, TS_DEFAULT, @fLargeFontID);

  tsFontCreateCreatorA('Arial', 25, TS_STYLE_NORMAL, TS_FALSE, TS_DEFAULT, @fSmallFontID);

  TextSuiteCheckError;
end;

constructor TtsFont.Create(Name: String; DC: HDC; Height: integer; Style: TFontStyles);
begin
tsFontID
end;

procedure TtsFont.TextOut(X, Y: Single; Str: String);
begin

end;

end.
