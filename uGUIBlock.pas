unit uGUIBlock;

interface

uses
  SysUtils, dglOpenGL, Graphics, GLHelper, uCity, uCityBlock, uViewFrame, TextSuite,
  glBitmap, Windows;

type
  TGUIMain = class(TGUILayer)
  private
  public
  end;

  TGUIButton = (btOK, btYes, btNo, btCancel, btAbort);
  TGUIButtons = set of TGUIButton;
  TGUIMessage = class(TGUILayer)
  private
    fMessage: String;
    fAnswer: String;
    procedure OnClick(Sender: TObject);
  public
    property Answer: String read fAnswer;

    constructor Create(const aMessage: String; aButtons: TGUIButtons);
    procedure Render; override;
  end;

  TGUIBlock = class(TGUILayer)
  private
    FCity: TCity;
    FBlock: TCityBlock;
  protected
    procedure SetClientRect(const aRect: TRect); override;
    procedure BuildClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  public
    constructor Create(ACity: TCity; X, Y: integer);
    procedure Render; override;
  end;

implementation

uses Classes, uGlobals, uBldHouse, uBldIndustry;

{ TGUIBlock }

procedure fieldAtRect(rect: TRect);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);    glVertex2f(rect.Left,  rect.Top);
    glTexCoord2f(1, 0);    glVertex2f(rect.Right, rect.Top);
    glTexCoord2f(1, 1);    glVertex2f(rect.Right, rect.Bottom);
    glTexCoord2f(0, 1);    glVertex2f(rect.Left,  rect.Bottom);
  glEnd;
end;

function RectOffset(Rect: TRect; p: TPoint): TRect;
begin
  result := Rect;
  inc(result.Left,    p.X);
  inc(result.Right,   p.X);
  inc(result.Top,     p.Y);
  inc(result.Bottom,  p.Y);
end;

constructor TGUIBlock.Create(ACity: TCity; X, Y: integer);
var
  a, b, i: Integer;
  c: TGUIClickable;
begin
  inherited Create;
  FCity:= ACity;
  FBlock:= FCity.Block[X, Y];

  for i := 0 to 8 do begin
    a := i mod 3;
    b := i div 3;
    c:= TGUIClickable.Create(Rect(
      a*50 + 10,
      b*50 + 10,
      a*50 + 55,
      b*50 + 55), BuildClick);
    c.Tag:= i;
    fClickables.Add(c);
  end;
  c := TGUIClickable.Create(Rect(
    10,
    ClientRect.Bottom - ClientRect.Top - 35,
    ClientRect.Right - ClientRect.Left - 10,
    ClientRect.Bottom - ClientRect.Top - 10),
    CloseClick);
  c.Text := '<<< Back <<<';
  fClickables.Add(c);
end;

procedure TGUIBlock.BuildClick(Sender: TObject);
begin
  ViewFrame.PushLayer(TGUIMessage.Create('Test String lalal a blubb bäng', [btOK, btCancel, btAbort]));
//  Close;
end;

procedure TGUIBlock.Render;
var
  i: integer;
  t: TglBitmap2D;
  b: TBuilding;
  r: TRect;  
begin
  inherited;
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  for i := 0 to fClickables.Count-1 do begin
    r := TGUIClickable(fClickables[i]).Rect;
    if (PtInRect(r, fMousePos)) then
      glColor4f(1, 1, 1, 0.2)
    else
      glColor4f(0, 0, 0, 0.5);
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));

    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glColor4f(1, 1, 1, 1);
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    if (i <= 8) then begin
      b := FBlock.Building[i];
      if Assigned(b) then begin
        t := b.Texture;
        t.Bind();
        glColor4f(1, 1, 1, 1);
        fieldAtRect(RectOffset(r, ClientRect.TopLeft));
        t.Unbind();
      end;
    end else begin
      tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
      tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
      Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), TGUIClickable(fClickables[i]).Text);
    end;
  end;

  tsSetParameteri(TS_ALIGN, TS_ALIGN_LEFT);
  tsSetParameteri(TS_VALIGN, TS_VALIGN_TOP);
  tsTextColor3f(1, 1, 1);
  Fonts.GUIText.BlockOut(ClientRect.Left + 20, ClientRect.Top + 200, 100, 100,
    Format(
      'IND: %f'+sLineBreak+
      'POL: %f'+sLineBreak+
      'EDU: %f'+sLineBreak+
      'LUX: %f'+sLineBreak+
      'SPA: %f'+sLineBreak+sLineBreak+
      'PPL: %f'+sLineBreak+
      'GRW: %f',
      [FBlock.Industry, FBlock.Pollution, FBlock.Education, FBlock.Luxury, FBlock.Space, FBlock.People, FBlock.GrowthRate]));
end;

procedure TGUIBlock.SetClientRect(const aRect: TRect);
begin
  inherited;
  TGUIClickable(fClickables[9]).Rect := Rect(
    10,
    ClientRect.Bottom - ClientRect.Top - 35,
    ClientRect.Right - ClientRect.Left - 10,
    ClientRect.Bottom - ClientRect.Top - 10);
end;

procedure TGUIBlock.CloseClick(Sender: TObject);
begin
  Close;
end;

{ TGUIMessage }

procedure TGUIMessage.OnClick(Sender: TObject);
begin
  fAnswer := TGUIClickable(Sender).Text;
  Close;
end;

constructor TGUIMessage.Create(const aMessage: String; aButtons: TGUIButtons);
var
  c, x, y, i: Integer;
  click: TGUIClickable;

  procedure AddButton(Caption: String);
  var
    x, y: Integer;
  begin
    x := Round(200 + (i - (c-1)/2) * 60);
    y := 80;
    click := TGUIClickable.Create(Rect(
      -25+x, -10+y, 25+x, 10+y), OnClick);
    click.Text := Caption;
    fClickables.Add(click);
    inc(i);
  end;

begin
  inherited Create;
  x := ViewFrame.ClientWidth div 2;
  y := ViewFrame.ClientHeight div 2;
  fClientRect := Rect(x-200, y-50, x+200, y+50);

  fMessage:= aMessage;

  c := 0;
  if btOK in aButtons then
    inc(c);
  if btYes in aButtons then
    inc(c);
  if btNo in aButtons then
    inc(c);
  if btCancel in aButtons then
    inc(c);
  if btAbort in aButtons then
    inc(c);

  i := 0;
  if btOK in aButtons then
    AddButton('OK');
  if btYes in aButtons then
    AddButton('Yes');
  if btNo in aButtons then
    AddButton('No');
  if btCancel in aButtons then
    AddButton('Cancel');
  if btAbort in aButtons then
    AddButton('Abort');
end;

procedure TGUIMessage.Render;
var
  i: Integer;
  r: TRect;
begin
  inherited;

  tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
  tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
  Fonts.GUIText.BlockOut(Rect(fClientRect.Left,fClientRect.Top, fClientRect.Right, fClientRect.Bottom-60), fMessage);

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

    Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), TGUIClickable(fClickables[i]).Text);
  end;  
end;

end.

