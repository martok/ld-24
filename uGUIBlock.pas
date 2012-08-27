unit uGUIBlock;

interface

uses
  SysUtils, Classes, dglOpenGL, Graphics, GLHelper, uCity, uCityBlock, uViewFrame, TextSuite,
  glBitmap, Windows, Contnrs;

type
  TGUIMain = class(TGUILayer)
  private
    FAutoplayBtn: TGUIClickable;
    FFrame: TViewFrame;
    procedure NextRoundClick(Sender: TObject);
    procedure AutoPlayClick(Sender: TObject);
  public
    constructor Create(aFrame: TViewFrame);
    procedure Render; override;
    procedure ViewportResize(const aWidth, aHeight: Integer); override;
    procedure SetAutoplayState(Playing: boolean);
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
    property OnClick: TNotifyEvent read FOnClick write FOnClick;    

    constructor Create(const aMessage: String; aButtons: TGUIButtons; aOnClick: TNotifyEvent = nil);
    procedure Render; override;
    procedure ViewportResize(const aWidth, aHeight: Integer); override;    
  end;

  TGUIBlock = class(TGUILayer)
  private
    FCity: TCity;
    FBlock: TCityBlock;
    FSelectedID: Integer;
    procedure BuildDestructClick(Sender: TObject);
    procedure BuildCreateClick(Sender: TObject);
  protected
    procedure SetClientRect(const aRect: TRect); override;
    procedure TearDownClick(Sender: TObject);
    procedure BuildClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  public
    constructor Create(ACity: TCity; X, Y: integer);
    destructor Destroy; override;
    procedure Render; override;
    procedure ViewportResize(const aWidth, aHeight: Integer); override;
  end;

  TBuildingCategory = (bcEdu=1, bcLux, bcLive, bcInd, bcSpec);
  TGUIChooseBuilding = class(TGUILayer)
  private
    FOnClick: TNotifyEvent;
    fResult: TBuildingClass;
    fCatList: TObjectList;
    procedure OnBtnClick(Sender: TObject);
    procedure OnCatClick(Sender: TObject);
  protected
    procedure ShowCat(aCat: TBuildingCategory);
  public
    property Result: TBuildingClass read fResult;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;    

    constructor Create(aOnClick: TNotifyEvent = nil);
    destructor Destroy; override;
    procedure Render; override;
    procedure ViewportResize(const aWidth, aHeight: Integer); override;
  end;

implementation

uses uGlobals, uBldHouse, uBldIndustry, uBldEducation, uBldLuxury, uBldSpecial;

var
  AllBuildings: array[TBuildingCategory, 0..5] of TBuildingClass =
  (
    (TBElementarySchool, TBLibrary, TBHighschool, TBCollege, nil, nil),
    (TBPark, TBCinema, TBPool, TBShopping, TBTheater, TBCasino),
    (TBHouse, TBAppartement, TBAppartement1stClass, nil, nil, nil),
    (TBSmallIndustry, TBFactory, TBFactories, nil ,nil, nil),
    (TBResearchCenter, TBWellnessCenter, TBBusinessApartmentComplex, TBWaterFront, nil, nil)
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

  procedure AddClickable(const aRect: TRect; aEvent: TNotifyEvent; aCaption: String);
  begin
    c := TGUIClickable.Create(aRect, aEvent);
    c.Text := aCaption;
    fClickables.Add(c);
  end;

begin
  inherited Create;
  FCity:= ACity;
  FCity.ActiveBlock:= Point(X, Y);
  FBlock:= FCity.Block[X, Y];
  FSelectedID := -1;

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
  AddClickable(Rect(
    10, 10, GUI_WIDTH - 10, 55),
    CloseClick, '<<< Back <<<');
  AddClickable(Rect(
    20, 370, GUI_WIDTH-20, 390),
    TearDownClick, 'tear down');
end;

procedure TGUIBlock.TearDownClick(Sender: TObject);
begin
  if (FSelectedID >= 0) and Assigned(FBlock.Building[FSelectedID]) then begin
    ViewFrame.PushLayer(TGUIMessage.Create(format('Tear down this %s?',[FBlock.Building[FSelectedID].DisplayName]), [btOK, btCancel], BuildDestructClick));        
  end;
end;

procedure TGUIBlock.BuildClick(Sender: TObject);
begin
  FSelectedID := TGUIClickable(Sender).Tag;
  if not Assigned(FBlock.Building[FSelectedID]) then begin
    ViewFrame.PushLayer(TGUIChooseBuilding.Create(BuildCreateClick));
  end;
end;

procedure TGUIBlock.BuildDestructClick(Sender: TObject);
begin
  if TGUIMessage(Sender).Answer=btOK then
    FCity.DestroyBuilding(FBlock, FSelectedID);
end;

procedure TGUIBlock.BuildCreateClick(Sender: TObject);
begin
  if TGUIChooseBuilding(Sender).Result <> nil then
    case FCity.CreateBuilding(TGUIChooseBuilding(Sender).Result, FBlock, FSelectedID) of
      brErrorUnknown: ViewFrame.PushLayer(TGUIMessage.Create('You can not build here!', [btOK], nil));
      brErrorMoney: ViewFrame.PushLayer(TGUIMessage.Create('You don''t have the money to build this!', [btOK], nil));
      brErrorEdu: ViewFrame.PushLayer(TGUIMessage.Create('You don''t have the education level to build this!', [btOK], nil));
    end;
end;

procedure TGUIBlock.Render;
var
  i: integer;
  t: TglBitmap2D;
  b: TBuilding;
  r: TRect;
begin
  inherited;
//Gebäude  
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
      if Assigned(b) and (not (b is TBSpecial) or (i = 4)) then begin
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
      glDisable(GL_TEXTURE_2D);
    end;
  end;                       

//selektiertes Gebäude
  glDisable(GL_TEXTURE_2D);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  r := Rect(10, 200, ClientRect.Right-ClientRect.Left-10, 400);
  fieldAtRect(RectOffset(r, ClientRect.TopLeft));
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  b := nil;
  if (FSelectedID >= 0) then
    b := FBlock.Building[FSelectedID];
  if Assigned(b) then begin
    tsSetParameteri(TS_ALIGN, TS_ALIGN_LEFT);
    tsSetParameteri(TS_VALIGN, TS_VALIGN_TOP);
    with b do
      Fonts.GUIText.BlockOut(ClientRect.Left+15, ClientRect.Top+203, ClientRect.Right-ClientRect.Left-30, 150,
        format(
          '%s'#10#13#10#13+

          'IND: %d'#10#13+
          'SPA: %d'#10#13#10#13+

          'POL: %d'#10#13+
          'EDU: %d'#10#13+
          'LUX: %d'#10#13#10#13+

          'RNG: %d block(s)'#10#13+
          'LPB: %f%%',
          [DisplayName,
          SIndustryValue, SLivingSpace,
          SPollution, SEducation, SLuxury,
          SRange, SEffectLoss*100]));
  end;

//Blockwerte
  glDisable(GL_TEXTURE_2D);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  r := Rect(10, 410, ClientRect.Right-ClientRect.Left-10, 530);
  fieldAtRect(RectOffset(r, ClientRect.TopLeft));
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  
  tsTextColor3f(1, 1, 1);
  tsSetParameteri(TS_ALIGN, TS_ALIGN_LEFT);
  tsSetParameteri(TS_VALIGN, TS_VALIGN_TOP);
  Fonts.GUIText.BlockOut(ClientRect.Left + 15, ClientRect.Top + 413, ClientRect.Right-ClientRect.Left-30, 100,
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

procedure TGUIBlock.ViewportResize(const aWidth, aHeight: Integer);
begin
  inherited;
  ClientRect := Rect(aWidth - GUI_WIDTH, 0, aWidth, aHeight);
end;

destructor TGUIBlock.Destroy;
begin
  FCity.ActiveBlock:= Point(-1,-1);
  inherited;
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

procedure TGUIMessage.ViewportResize(const aWidth, aHeight: Integer);
begin
  inherited;
  ClientRect := Rect(aWidth div 2-200, aHeight div 2-50, aWidth div 2+200, aHeight div 2+50);
end;

{ TGUIChooseBuilding }

constructor TGUIChooseBuilding.Create(aOnClick: TNotifyEvent);
var
  x, y: Integer;
  click: TGUIClickable;

  procedure AddCategoryBtn(cat: TBuildingCategory);
  var
    x, y: Integer;
  begin
    x:= 10 + (ord(cat)-1)*40;
    y:= 10;
    click:= TGUIClickable.Create(rect(x,y,x+35,y+35),OnCatClick);
    click.Tag:= ord(cat);
    fClickables.Add(click);
  end;

begin
  inherited Create;
  x := ViewFrame.ClientWidth div 2;
  y := ViewFrame.ClientHeight div 2;
  fClientRect := Rect(x-320, y-200, x+320, y+200);

  FOnClick:= aOnClick;
  fCatList:= TObjectList.Create(false);

  AddCategoryBtn(bcLive);
  AddCategoryBtn(bcInd);
  AddCategoryBtn(bcEdu);
  AddCategoryBtn(bcLux);
  AddCategoryBtn(bcSpec);

  x:= 10+6*40;
  y:= 10;
  click := TGUIClickable.Create(Rect(x,y,x+100,y+35), OnBtnClick);
  click.Tag:= 0;
  click.Text:= '<<< Back <<<';
  fClickables.Add(click);
end;

destructor TGUIChooseBuilding.Destroy;
begin
  FreeAndNil(fCatList);
  inherited;
end;

procedure TGUIChooseBuilding.ShowCat(aCat: TBuildingCategory);
var
  i: integer;
  click: TGUIClickable;
  procedure AddButton(Cls: TBuildingClass);
  var
    x, y: Integer;
    effects: string;
    inst: TBuilding;
  begin
    x := 20;
    y := 60 + (i * 40);

    click := TGUIClickable.Create(Rect(x,y,x+35,y+35), OnBtnClick);
    click.Tag:= Integer(Cls);
    fCatList.Add(click);

    effects:= '';
    inst:= Cls.Create;
    try
      with inst do begin
        if SIndustryValue<>0 then
          effects:= effects + format('IND: %d  ',[SIndustryValue]);
        if SLivingSpace<>0 then
          effects:= effects + format('SPA: %d  ',[SLivingSpace]);

        if SPollution<>0 then
          effects:= effects + format('POL: %d  ',[SPollution]);
        if SEducation<>0 then
          effects:= effects + format('EDU: %d  ',[SEducation]);
        if SLuxury<>0 then
          effects:= effects + format('LUX: %d  ',[SLuxury]);

        effects:= effects + sLineBreak+format('RNG: %d block(s) LPB: %f%%', [SRange, SEffectLoss*100]);
      end;
      inst.SLivingSpace
    finally
      inst.Free;
    end;

    click := TGUIClickable.Create(Rect(x+50,y,x+610,y+35), OnBtnClick);
    click.Tag:= Integer(Cls);
    click.Text:= Cls.DisplayName + sLineBreak+'$'+IntToStr(Cls.Price)+#9+effects;
    fCatList.Add(click);
  end;
begin
  for i:= 0 to fCatList.Count-1 do
    fClickables.Remove(fCatList[i]);
  fCatList.Clear;

  for i:= 0 to high(AllBuildings[aCat]) do
    if nil<>AllBuildings[aCat][i] then begin
      AddButton(AllBuildings[aCat][i]);
    end;

  for i:= 0 to fCatList.Count-1 do
    fClickables.Add(fCatList[i]);
end;

procedure TGUIChooseBuilding.OnCatClick(Sender: TObject);
begin
  ShowCat(TBuildingCategory(TGUIClickable(Sender).Tag));
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
  s,q: string;
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

    case TGUIClickable(fClickables[i]).Tag of
      0: begin
        tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
        tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
        Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), TGUIClickable(fClickables[i]).Text);
      end;
      ord(low(TBuildingCategory))..ord(high(TBuildingCategory)): begin
        t:= nil;
        case TBuildingCategory(TGUIClickable(fClickables[i]).Tag) of
          bcEdu: t:= Textures.BLibrary;
          bcLux: t:= Textures.BPark;
          bcLive: t:= Textures.BHouse;
          bcInd: t:= Textures.BFactory;
          bcSpec: t := Textures.BSpecial;
        end;
        t.Bind();
        glColor4f(1, 1, 1, 1);
        fieldAtRect(RectOffset(r, ClientRect.TopLeft));
        t.Unbind();
      end;
      else begin
        if TGUIClickable(fClickables[i]).Text>'' then begin
          tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
          s:= TGUIClickable(fClickables[i]).Text;
          q:= Copy(s,1, pos(#9,s)-1);
          Delete(s,1,length(q)+1);
          InflateRect(r, -6, 0);
          tsSetParameteri(TS_ALIGN, TS_ALIGN_LEFT);
          Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), q);
          tsSetParameteri(TS_ALIGN, TS_ALIGN_RIGHT);
          Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), s);
        end else begin
          b := TBuildingClass(TGUIClickable(fClickables[i]).Tag);
          t := b.Texture;
          t.Bind();
          glColor4f(1, 1, 1, 1);
          fieldAtRect(RectOffset(r, ClientRect.TopLeft));
          t.Unbind();
        end;
      end;
    end;
  end;
  if fCatList.Count=0 then begin
    tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);
    tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
    Fonts.GUIText.BlockOut(ClientRect, 'Select a builing category');
  end;
end;

procedure TGUIChooseBuilding.ViewportResize(const aWidth, aHeight: Integer);
var
  x, y, c: Integer;
begin
  inherited;
  x := ViewFrame.ClientWidth div 2;
  y := ViewFrame.ClientHeight div 2;
  c := 20 + (10+1) * 40;
  fClientRect := Rect(x-320, y-c div 2, x+320, y+c div 2);
end;

{ TGUIMain }

constructor TGUIMain.Create(aFrame: TViewFrame);
var
  c: TGUIClickable;
begin
  inherited Create;

  FFrame:= aFrame;

  c:= TGUIClickable.Create(Rect(10,10, GUI_WIDTH-10, 35), NextRoundClick);
  c.Text:= 'Next Round';
  fClickables.Add(c);

  FAutoplayBtn:= TGUIClickable.Create(Rect(10,45, GUI_WIDTH-10, 70), AutoplayClick);
  FAutoplayBtn.Text:= 'Autoplay >>';
  fClickables.Add(FAutoplayBtn);
end;

procedure TGUIMain.Render;
var
  i: Integer;
  r: TRect;
begin
  inherited;
  tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
  tsSetParameteri(TS_VALIGN, TS_VALIGN_CENTER);

  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  for i := 0 to fClickables.Count-1 do begin
    r := TGUIClickable(fClickables[i]).Rect;
    glDisable(GL_TEXTURE_2D);
    if (PtInRect(r, fMousePos)) then
      glColor4f(1, 1, 1, 0.2)
    else begin
      if i=0 then
        glColor4f(1, 0, 0, 0.5)
      else
        glColor4f(0, 0, 0, 0.5);
    end;
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));

    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glColor4f(1, 1, 1, 1);
    fieldAtRect(RectOffset(r, ClientRect.TopLeft));
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    Fonts.GUIText.BlockOut(RectOffset(r, ClientRect.TopLeft), TGUIClickable(fClickables[i]).Text);
  end;
end;

procedure TGUIMain.AutoPlayClick(Sender: TObject);
begin
  FFrame.ToggleAutoplay;
end;

procedure TGUIMain.NextRoundClick(Sender: TObject);
begin
  FFrame.NextRound;
end;

procedure TGUIMain.SetAutoplayState(Playing: boolean);
begin
  if Playing then
    FAutoplayBtn.Text:= 'Stop Autoplay'
  else
    FAutoplayBtn.Text:= 'Autoplay >>';
end;

procedure TGUIMain.ViewportResize(const aWidth, aHeight: Integer);
begin
  inherited;
  ClientRect := Rect(aWidth - GUI_WIDTH, 0, aWidth, aHeight);  
end;

end.

