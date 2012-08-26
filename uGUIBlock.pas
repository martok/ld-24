unit uGUIBlock;

interface

uses
  SysUtils, Classes, dglOpenGL, Graphics, GLHelper, uCity, uCityBlock, uViewFrame, TextSuite,
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
    fAnswer: TGUIButton;
    FOnClick: TNotifyEvent;
    procedure OnBtnClick(Sender: TObject);
  public
    property Answer: TGUIButton read fAnswer;

    constructor Create(const aMessage: String; aButtons: TGUIButtons; aOnClick: TNotifyEvent = nil);
    procedure Render; override;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TGUIBlock = class(TGUILayer)
  private
    FCity: TCity;
    FBlock: TCityBlock;
    FClickedBld: integer;
    procedure BuildDestructClick(Sender: TObject);
    procedure BuildCreateClick(Sender: TObject);
  protected
    procedure SetClientRect(const aRect: TRect); override;
    procedure BuildClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  public
    constructor Create(ACity: TCity; X, Y: integer);
    procedure Render; override;
  end;

  TGUIChooseBuilding = class(TGUILayer)
  private
    FOnClick: TNotifyEvent;
    fResult: TBuildingClass;
    procedure OnBtnClick(Sender: TObject);
  public
    property Result: TBuildingClass read fResult;

    constructor Create(aOnClick: TNotifyEvent = nil);
    procedure Render; override;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses uGlobals, uBldHouse, uBldIndustry;

var
  AllBuildings: array[0..3] of TBuildingClass =
  (
    TBHouse, TBAppartement,
    TBSmallIndustry, TBFactory
  );

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
      b*50 + 40,
      a*50 + 55,
      b*50 + 85), BuildClick);
    c.Tag:= i;
    fClickables.Add(c);
  end;
  c := TGUIClickable.Create(Rect(
    10,
    10,
    GUI_WIDTH - 10,
    55),
    CloseClick);
  c.Text := '<<< Back <<<';
  fClickables.Add(c);
end;

procedure TGUIBlock.BuildClick(Sender: TObject);
begin
  FClickedBld:= TGUIClickable(Sender).Tag;
  if Assigned(FBlock.Building[FClickedBld]) then begin
    ViewFrame.PushLayer(TGUIMessage.Create(format('Tear down this %s?',[FBlock.Building[FClickedBld].DisplayName]), [btOK, btCancel],BuildDestructClick));
  end else begin
    ViewFrame.PushLayer(TGUIChooseBuilding.Create(BuildCreateClick));
  end;
//  Close;
end;

procedure TGUIBlock.BuildDestructClick(Sender: TObject);
begin
  if TGUIMessage(Sender).Answer=btOK then
    FCity.DestroyBuilding(FBlock,FClickedBld);
end;

procedure TGUIBlock.BuildCreateClick(Sender: TObject);
begin
  if TGUIChooseBuilding(Sender).Result<>nil then
    FCity.CreateBuilding(TGUIChooseBuilding(Sender).Result, FBlock);
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
    10,
    ClientRect.Right - ClientRect.Left - 10,
    35);
end;

procedure TGUIBlock.CloseClick(Sender: TObject);
begin
  Close;
end;

{ TGUIMessage }

procedure TGUIMessage.OnBtnClick(Sender: TObject);
begin
  fAnswer := TGUIButton(TGUIClickable(Sender).Tag);
  if Assigned(FOnClick) then
    FOnClick(Self);
  Close;
end;

constructor TGUIMessage.Create(const aMessage: String; aButtons: TGUIButtons; aOnClick: TNotifyEvent);
var
  c, x, y, i: Integer;
  click: TGUIClickable;

  procedure AddButton(b: TGUIButton; Caption: String);
  var
    x, y: Integer;
  begin
    x := Round(200 + (i - (c-1)/2) * 60);
    y := 80;
    click := TGUIClickable.Create(Rect(
      -25+x, -10+y, 25+x, 10+y), OnBtnClick);
    click.Text := Caption;
    click.Tag:= ord(b);
    fClickables.Add(click);
    inc(i);
  end;

begin
  inherited Create;
  x := ViewFrame.ClientWidth div 2;
  y := ViewFrame.ClientHeight div 2;
  fClientRect := Rect(x-200, y-50, x+200, y+50);

  fMessage:= aMessage;
  FOnClick:= aOnClick;

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
    AddButton(btOK, 'OK');
  if btYes in aButtons then
    AddButton(btYes, 'Yes');
  if btNo in aButtons then
    AddButton(btNo, 'No');
  if btCancel in aButtons then
    AddButton(btCancel, 'Cancel');
  if btAbort in aButtons then
    AddButton(btAbort, 'Abort');
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

{ TGUIChooseBuilding }

constructor TGUIChooseBuilding.Create(aOnClick: TNotifyEvent);
var
  x, y, i, c: Integer;
  click: TGUIClickable;

  procedure AddButton(Cls: TBuildingClass);
  var
    x, y: Integer;
  begin
    x := 10;
    y := 10 + (i * 40);

    click := TGUIClickable.Create(Rect(x,y,x+35,y+35), OnBtnClick);
    click.Tag:= Integer(Cls);
    fClickables.Add(click);

    click := TGUIClickable.Create(Rect(x+45,y,x+300,y+35), OnBtnClick);
    click.Tag:= Integer(Cls);
    click.Text:= Cls.DisplayName;
    fClickables.Add(click);
  end;

begin
  inherited Create;
  x := ViewFrame.ClientWidth div 2;
  y := ViewFrame.ClientHeight div 2;
  c := 20 + (Length(AllBuildings)+1) * 40;
  fClientRect := Rect(x-160, y-c div 2, x+160, y+c div 2);

  FOnClick:= aOnClick;

  for i:= 0 to high(AllBuildings) do begin
    AddButton(AllBuildings[i]);
  end;

  x := 10;
  y := 10 + (Length(AllBuildings) * 40);
  click := TGUIClickable.Create(Rect(x,y+10,x+150,y+35), OnBtnClick);
  click.Tag:= 0;
  click.Text:= '<<< Back <<<';
  fClickables.Add(click);
end;

procedure TGUIChooseBuilding.OnBtnClick(Sender: TObject);
var
  cls: TBuildingClass;
begin
  cls:= TBuildingClass(TGUIClickable(Sender).Tag);
  fResult:= cls;
  if Assigned(FOnClick) then
    FOnClick(Self);
  Close;
end;

procedure TGUIChooseBuilding.Render;
var
  i: Integer;
  r: TRect;
  b: TBuildingClass;
  t: TglBitmap2D;
begin
  inherited;

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

    if TGUIClickable(fClickables[i]).Text='' then begin
      b := TBuildingClass(TGUIClickable(fClickables[i]).Tag);
      t := b.Texture;
      t.Bind();
      glColor4f(1, 1, 1, 1);
      fieldAtRect(RectOffset(r, ClientRect.TopLeft));
      t.Unbind();
    end else begin
      tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
      tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
      Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), TGUIClickable(fClickables[i]).Text);
    end;
  end;  
end;

end.

