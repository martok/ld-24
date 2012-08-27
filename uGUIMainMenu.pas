unit uGUIMainMenu;

interface

uses
  Windows, SysUtils, Classes, dglOpenGL, Graphics, GLHelper, uViewFrame, TextSuite,
  glBitmap;

type
  TGUIMainMenu = class(TGUILayer)
  private
    FFrame: TViewFrame;
    FFiles: TStringList;
    procedure HelpClick(Sender: TObject);
    procedure StartGameClick(Sender: TObject);
  public
    constructor Create(aFrame: TViewFrame);
    destructor Destroy; override;
    procedure Render; override;
    procedure ViewportResize(const aWidth, aHeight: Integer); override;
  end;

  TGUIMainHelp = class(TGUILayer)
  private
    procedure BackClick(Sender: TObject);
  public
    constructor Create;
    procedure Render; override;
    procedure ViewportResize(const aWidth, aHeight: Integer); override;
  end;

implementation

uses uGUIBlock, uGlobals, uConfigFile;

{ TGUIMainMenu }

constructor TGUIMainMenu.Create(aFrame: TViewFrame);
var
  c: TGUIClickable;
  sr: TSearchRec;
  f: TFileStream;
  map: TkcfConfigFile;
  s: string;
begin
  inherited Create;
  FFrame:= aFrame;
  c:= TGUIClickable.Create(Bounds(300, 20, 280, 35), HelpClick);
  c.Text:= 'What''ts this?';
  fClickables.Add(c);

  FFiles:= TStringList.Create;
  if FindFirst(ExtractFilePath(ParamStr(0))+'maps/*.map',faAnyFile,sr)=0 then begin
    repeat
      f:= TFileStream.Create(ExtractFilePath(ParamStr(0))+'maps/'+sr.Name, fmOpenRead);
      try
        map:= TkcfConfigFile.Create(f);
        try
          if map.SectionExists('Map') then begin
            s:= map.Section('Map').GetValue('Title',ChangeFileExt(sr.Name,''));
            c:= TGUIClickable.Create(Bounds(300, 120+FFiles.Count*40, 280, 35), StartGameClick);
            c.Tag:= FFiles.Add(s);
            c.Text:= s;
            fClickables.Add(c);
          end;
        finally
          FreeAndNil(map);
        end;
      finally
        FreeAndNil(f);
      end;
    until FindNext(sr)<>0;
    FindClose(sr);
  end;
end;

destructor TGUIMainMenu.Destroy;
begin
  FreeAndNil(FFiles);
  inherited;
end;

procedure TGUIMainMenu.Render;
var
  i: Integer;
  r: TRect;
begin
  inherited;
  Textures.GameLogo.SetFilter(GL_NEAREST, GL_NEAREST);
  Textures.GameLogo.Bind();
  glColor4f(1,1,1,1);
  fieldAtRect(RectOffset(Bounds(20, 30, 256,256), ClientRect.TopLeft));
  Textures.GameLogo.Unbind();

  tsSetParameteri(TS_ALIGN, TS_ALIGN_LEFT);
  tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
  Fonts.GUIText.BlockOut(RectOffset(Bounds(300, 90, 280, 35), ClientRect.TopLeft), 'Start playing map: ');

  tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
  tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
  Fonts.LargeText.BlockOut(RectOffset(Rect(0, 350, 600, 400), ClientRect.TopLeft),
    'Created for Ludum Dare #24'+sLineBreak+
    '  by BitSpace, 24-27 Aug 2012'
    );

  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  for i := 0 to fClickables.Count-1 do begin
    r := TGUIClickable(fClickables[i]).Rect;
    glDisable(GL_TEXTURE_2D);
    if (PtInRect(r, fMousePos)) then
      glColor4f(1, 1, 1, 0.2)
    else
      glColor4f(0, 0, 0, 0.5);
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));

    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glColor4f(1, 1, 1, 1);
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
    tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
    Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), TGUIClickable(fClickables[i]).Text);
  end;
end;

procedure TGUIMainMenu.ViewportResize(const aWidth, aHeight: Integer);
const
  w = 600;
  h = 400;
begin
  inherited;
  ClientRect := Rect((aWidth-w) div 2, (aHeight-h) div 2, (aWidth+w) div 2, (aHeight+h) div 2);
end;

procedure TGUIMainMenu.HelpClick(Sender: TObject);
begin
  FFrame.PushLayer(TGUIMainHelp.Create);
end;

procedure TGUIMainMenu.StartGameClick(Sender: TObject);
var
  f: string;
begin
  f:= FFiles[TGUIClickable(Sender).Tag];
  FFrame.StartGame(f);
  Close;
end;

{ TGUIMainHelp }

constructor TGUIMainHelp.Create;
var
  click: TGUIClickable;
begin
  inherited;
  click := TGUIClickable.Create(Rect(200,10,400,35), BackClick);
  click.Text:= '<<< Back <<<';
  fClickables.Add(click);
end;

procedure TGUIMainHelp.BackClick(Sender: TObject);
begin
  Close;
end;

procedure TGUIMainHelp.Render;
var
  i: integer;
  r: TRect;
begin
  inherited;
  tsSetParameteri(TS_ALIGN, TS_ALIGN_LEFT);
  tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
  Fonts.GUIText.BlockOut(RectOffset(Bounds(20, 50, 580, 380), ClientRect.TopLeft),
    'Game Of City Life is a variation on the classical Game of Life.'+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    'Instead of a lifeform, you manage a city evolving from one tiny house up to a city of millions.'+sLineBreak+
    'Every city block affects those around it, by spreading it''s positive effects as well as it''s negative.'+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    ''+sLineBreak+
    'Explanation of acronyms:'+sLineBreak+
    'PPL: Population, MON: Money, EDU: Education (required to build advanced buildings),'+sLineBreak+
    'IND: industry (generates money), SPA: Living Space (how many people fit in that area),'+sLineBreak+
    'POL: Pollution (the side effect of industry), LUX: Luxury (keeps people happy),'+sLineBreak+
    'RNG: Range (how many blocks affects this), LPB: how much effect is lost per distance'
  );

  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  for i := 0 to fClickables.Count-1 do begin
    r := TGUIClickable(fClickables[i]).Rect;
    glDisable(GL_TEXTURE_2D);
    if (PtInRect(r, fMousePos)) then
      glColor4f(1, 1, 1, 0.2)
    else
      glColor4f(0, 0, 0, 0.5);
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));

    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glColor4f(1, 1, 1, 1);
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
    tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
    Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), TGUIClickable(fClickables[i]).Text);
  end;
end;

procedure TGUIMainHelp.ViewportResize(const aWidth, aHeight: Integer);
const
  w = 600;
  h = 400;
begin
  inherited;
  ClientRect := Rect((aWidth-w) div 2, (aHeight-h) div 2, (aWidth+w) div 2, (aHeight+h) div 2);
end;

end.
