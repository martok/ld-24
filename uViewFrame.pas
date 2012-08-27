unit uViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Contnrs, Dialogs, dglOpenGL, TextSuite, AppEvnts, ExtCtrls, uCity, FastGL,
  uSound;

type
  TGUILayer = class;

  TViewFrame = class(TForm)
    Timer1: TTimer;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    RC: TgluRenderContext;
    MP: TPoint;
    FFrameCount: integer;
    LastFrameTime: Double;
    LastEvolve: Single;
    HasMoved: Boolean;
    BGMusic: TsndInstance;
    AutoPlay: Boolean;
    procedure LoadFonts;
    procedure LoadTextures;
    procedure LoadSounds;
    function RenderForMouseClick(x, y: integer): TPoint;
    procedure PrepareMatrix;
  public
    { Public-Deklarationen }
    Camera: packed record
      pos: array[0..2] of Single;
      turn, tilt, zoom: Single;
    end;
    City: TCity;
    GUIStack: TObjectList;
    Sound: TSoundSystem;
    SoundEmitter: TsndEmitter;
    procedure StartGame(Level: string);
    procedure PopLayer(const aLayer: TGUILayer = nil);
    procedure PushLayer(const aLayer: TGUILayer);
    procedure NextRound;
    procedure ToggleAutoplay;
    procedure Timestep(DT: Single);
    procedure Render;
  end;

  TGUILayer = class
  protected
    fClientRect: TRect;
    fClickables: TObjectList;
    fMousePos: TPoint;

    procedure SetClientRect(const aRect: TRect); virtual;
  public
    property ClientRect: TRect read fClientRect write SetClientRect;

    constructor Create;
    destructor Destroy; override;
    function MouseClick(X, Y: Integer): Boolean; virtual;
    procedure MouseMove(X, Y: Integer);
    procedure ViewportResize(const aWidth, aHeight: Integer); virtual;
    procedure Close;
    procedure Render; virtual;
  end;

  TGUIClickable = class
  private
    FText: string;
    FOnClick: TNotifyEvent;
    FTag: integer;
  protected
    ActiveRect: TRect;
    procedure Click;
  public
    constructor Create(Rect: TRect; aOnClick: TNotifyEvent = nil);

    property Rect: TRect read ActiveRect write ActiveRect;
    property Text: string read FText write FText;
    property Tag: integer read FTag write FTag;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

var
  ViewFrame: TViewFrame;

implementation

uses GLHelper, uCityBlock, uFonts, uGlobals, uGUIBlock, glBitmap, Math, uGUIMainMenu;

{$R *.dfm}

var
  pfc: Int64;
  
function GetPrecisionTime: Double;
var
  pc: Int64;
begin
  QueryPerformanceCounter(pc);
  Result:= pc / pfc * 1000;
end;

function LoadTexture(Name: string): TglBitmap2D;
begin
  Result:= TglBitmap2D.Create;
  try
    Result.LoadFromFile(ExtractFilePath(ParamStr(0))+'textures\'+name+'.tga');
    Result.SetWrap(GL_CLAMP, GL_CLAMP, GL_CLAMP);
    Result.SetFilter(GL_LINEAR, GL_LINEAR);
    Result.GenTexture();
  except
    FreeAndNil(result);
  end;
end;

function LoadSound(Kind: TSoundKind; Name: string): TsndSound;
var
  fs: TFileStream;
begin
  fs:= TFileStream.Create(ExtractFilePath(ParamStr(0))+'sounds\'+name+'.ogg',fmOpenRead or fmShareDenyWrite);
  try
    Result:= TsndSound.Create(Kind);
    Result.LoadFromStream(fs, ftOggVorbis);
  finally
    fs.Free;
  end;
end;

procedure TViewFrame.FormCreate(Sender: TObject);
var
  PFList, SampleList: array[0..31] of glInt;
  c, i, maxSample, PF: Integer;
begin
  randomize;

  QueryPerformanceFrequency(pfc);
  ClientWidth:= 800;
  ClientHeight:= 600;
  InitOpenGL();
  maxSample := 0;
  PF := 0;
  {gluGetAntiAliasingPixelFormats(@PFList[0], @SampleList[0], 32, c);
  for i := 0 to c-1 do begin
    if SampleList[i] > maxSample then begin
      PF := PFList[i];
      maxSample := sampleList[i];
    end;
  end;
  }
  if PF = 0 then
    PF := gluGetPixelFormat(Handle, [opDoubleBuffered], 32, 24, 0, 0, 0, 0);
  RC := gluCreateRenderContext(Handle, PF);
  gluActivateRenderContext(RC);
  LastFrameTime:= GetPrecisionTime;

  TtsFont.InitTS;
  Sound:= TSoundSystem.Create;
  if not Sound.Available then begin
    //TODO
    raise Exception.Create('No OpenAL found. Pleas see included Readme to find out what to do about that.');
  end;

  LoadFonts;
  LoadTextures;
  LoadSounds;

  GUIStack := TObjectList.Create(true);
  FFrameCount:= 0;

  BGMusic := TsndInstance.Create(Sounds.BackgroundMusic, SoundEmitter);
  BGMusic.Loop(-1, 10).Gain:= 0.3;

  PushLayer(TGUIMainMenu.Create(Self));
  Application.OnIdle := ApplicationIdle;  
end;

procedure TViewFrame.FormDestroy(Sender: TObject);
begin
  FreeAndNil(BGMusic);
  FreeAndNil(City); 
  FreeAndNil(Sound);
  GUIStack.Free;
  TtsFont.DoneTS;
  gluDestroyRenderContext(RC);

  FreeTextures;
  FreeFonts;
  FreeSounds;

  inherited;
end;

procedure TViewFrame.LoadFonts;
begin
  Fonts.GUIText:= TtsFont.Create('Tahoma', 12, false, []);
  Fonts.LargeText:= TtsFont.Create('Arial', 18, false, []);
end;

procedure TViewFrame.LoadTextures;
begin
  Textures.GameLogo:= LoadTexture('Logo');  
  Textures.MenuBG:= LoadTexture('MenuBG');

  Textures.BUnknown:= LoadTexture('BUnknown');
  Textures.BElementarySchool:= LoadTexture('BElementarySchool');
  Textures.BHighschool:= LoadTexture('BHighschool');
  Textures.BLibrary:= LoadTexture('BLibrary');
  Textures.BCollege:= LoadTexture('BCollege');

  Textures.BHouse:= LoadTexture('BHouse');
  Textures.BAppartement:= LoadTexture('BAppartement');
  Textures.BAppartement1stClass:= LoadTexture('BAppartement1stClass');

  Textures.BSmallIndustry:= LoadTexture('BSmallIndustry');
  Textures.BFactory:= LoadTexture('BFactory');
  Textures.BFactories:= LoadTexture('BFactories');

  Textures.BPark:= LoadTexture('BPark');
  Textures.BCinema:= LoadTexture('BCinema');
  Textures.BPool:= LoadTexture('BPool');
  Textures.BShopping:= LoadTexture('BShopping');
  Textures.BTheater:= LoadTexture('BTheater');
  Textures.BCasino:= LoadTexture('BCasino');

  Textures.BSpecial := LoadTexture('BSpecial');
  Textures.BResearchCenter := LoadTexture('BResearchCenter');
  Textures.BWellnessCenter := LoadTexture('BWellnessCenter');
  Textures.BBusinessApartmentComplex := LoadTexture('BBusinessApartmentComplex');
  Textures.BWaterFront := LoadTexture('BWaterFront');  
end;

procedure TViewFrame.LoadSounds;
begin
  SoundEmitter:= TsndEmitter.Create;
  Sounds.BackgroundMusic:= LoadSound(skStream,'maximum_chill');
  Sounds.EffectClick:= LoadSound(skBlock,'click');  
  Sounds.EffectMenu:= LoadSound(skBlock,'menu_open'); 
  Sounds.EffectBuild:= LoadSound(skBlock,'buy');
  Sounds.EffectDestroy:= LoadSound(skBlock,'destroy');
  Sounds.EffectNextTurn:= LoadSound(skBlock,'next_turn');
end;

procedure TViewFrame.StartGame(Level: string);
var
  guiMain: TGUIMain;
  ct: TPoint;
begin
  FillChar(Camera.pos[0], SizeOf(Camera.pos), 0);
  Camera.turn := 15;
  Camera.tilt := 35;
  Camera.zoom := -100;

  LastEvolve:= 0;
  AutoPlay:= false;

  guiMain := TGUIMain.Create(Self);
  guiMain.ClientRect := Rect(ClientWidth - GUI_WIDTH, 0, ClientWidth, ClientHeight);
  PushLayer(guiMain);

  City:= TCity.Create;
  City.LoadFromFile(ExtractFilePath(Application.ExeName)+'maps\'+Level+'.map');
  ct:= City.PopulateStart;
  Camera.pos[0] := -ct.X * City.BlockDist;
  Camera.pos[2] := -ct.Y * City.BlockDist;
end;

procedure TViewFrame.ApplicationIdle(Sender: TObject; var Done: Boolean);
const
  time_per_frame = 1000 / 80;
var
  dt: Single;
begin
  dt:= GetPrecisionTime - LastFrameTime;
  if dt < time_per_frame then begin
    Sleep(1);
    exit;
  end;
  LastFrameTime := GetPrecisionTime;

  if Assigned(City) then
    Timestep(dt / 1000);

  Render;
  inc(FFrameCount);

  Done := false;
end;

procedure TViewFrame.Timer1Timer(Sender: TObject);
begin
  Caption:= Format('Game Of City Life - FPS: %d', [FFrameCount]);
  FFrameCount:= 0;
end;

function KeyPressed(Key: Integer): Boolean;
begin
  Result:= ForegroundTask and Application.Active and (GetKeyState(Key) < 0);
end;

procedure TViewFrame.Timestep(DT: Single);
var
  dx, dy: Single;
  i, c: integer;
begin
  dx := 0;
  dy := 0;
  if KeyPressed(VK_LEFT) then
    dx := dx - 3000 * DT;
  if KeyPressed(VK_RIGHT) then
    dx := dx + 3000 * DT;
  if KeyPressed(VK_DOWN) then
    dy := dy + 3000 * DT;
  if KeyPressed(VK_UP) then
    dy := dy - 3000 * DT;
  Camera.pos[0] := Camera.pos[0] + cos(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;
  Camera.pos[2] := Camera.pos[2] + sin(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;
  Camera.pos[0] := Camera.pos[0] - sin(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;
  Camera.pos[2] := Camera.pos[2] + cos(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;

  City.Progress(DT);
  LastEvolve:= LastEvolve + DT;
  if (LastEvolve >= 0.1) and (City.TotalPeople > 0) then begin
    c:= Round(Random*Log10(City.TotalPeople)*0.8);
    if c > 5 then c:= 5;
    for i:= 1 to c do
      City.CreateRandomCar; 
    if AutoPlay then
      City.Evolve;
    LastEvolve:= 0;
  end;
end;

procedure TViewFrame.PrepareMatrix;
const
  FOV = 45;
  CLIP_NEAR = 0.1;
  CLIP_FAR  = 1000;
begin
  glMatrixMode(GL_PROJECTION);
  glViewport(0, 0, ClientWidth, ClientHeight);
  glLoadIdentity();
  gluPerspective(FOV, ClientWidth / ClientHeight, CLIP_NEAR, CLIP_FAR);

  glMatrixMode(GL_MODELVIEW);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glEnable(GL_DEPTH_TEST);
end;

procedure TViewFrame.Render;
var
  r: TRect;
  i: integer;
begin
  PrepareMatrix;
  if Assigned(City) then begin
    glEnable(GL_LIGHTING);

    glPushMatrix;
    glLoadIdentity;
    glTranslatef(0, 0, Camera.zoom);
    glRotatef(Camera.tilt, 1, 0, 0);
    glRotatef(Camera.turn, 0, 1, 0);
    glTranslatef(Camera.pos[0], Camera.pos[1], Camera.pos[2]);
    City.Render(KeyPressed(VK_F3));
    glPopMatrix;
  end;

  Enter2dMode(ClientWidth, ClientHeight);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  if Assigned(City) then begin
    r:= rect(-1,0,ClientWidth-GUI_WIDTH, 40);
    for i := 0 to 1 do begin
      case i of
        0: begin
          SetGLColor(ColorToRGBA(0, 0, 0, 0.75));
          glBegin(GL_QUADS);
        end;
        1: begin
          SetGLColor(ColorToRGBA(1, 1, 1, 1));
          glBegin(GL_LINES);
        end;
      end;

        glVertex2f(r.Left, r.Top);
        glVertex2f(r.Right+1, r.Top);
        glVertex2f(r.Right+1, r.Bottom+1);
        glVertex2f(r.Left, r.Bottom+1);
      glEnd;
    end;   
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    InflateRect(r,-10,-10);
    tsSetParameteri(TS_VALIGN, TS_VALIGN_TOP);
    tsSetParameteri(TS_ALIGN, TS_ALIGN_LEFT);
    Fonts.LargeText.BlockOut(r, format('PPL: %.0n RND: %d',[City.TotalPeople, City.Round]));
    tsSetParameteri(TS_ALIGN, TS_ALIGN_CENTER);
    Fonts.LargeText.BlockOut(r, format('MON: $%.0n',[City.TotalMoney]));
    tsSetParameteri(TS_ALIGN, TS_ALIGN_RIGHT);
    Fonts.LargeText.BlockOut(r, format('EDU: %n',[City.TotalEducation]));
    glDisable(GL_TEXTURE_2D);
  end else begin
    if GUIStack[0] is TMainMenuGUI then
      TMainMenuGUI(GUIStack[0]).BackgroundDraw(ClientWidth, ClientHeight);
  end;
  if GUIStack.Count > 0 then
    TGUILayer(GUIStack[GUIStack.Count-1]).Render;
  Exit2dMode;
  SwapBuffers(RC.DC);
end;

function TViewFrame.RenderForMouseClick(x, y: integer): TPoint;
var
  clr: array[0..2] of byte;
begin
  PrepareMatrix;
  glDisable(GL_LIGHTING);

  glPushMatrix;
  glLoadIdentity;
  glTranslatef(0, 0, Camera.zoom);
  glRotatef(Camera.tilt, 1, 0, 0);  
  glRotatef(Camera.turn, 0, 1, 0);
  glTranslatef(Camera.pos[0], Camera.pos[1], Camera.pos[2]);
  City.Render(true);
  glPopMatrix;

  glReadPixels(x, ClientHeight - y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, @clr);
  if (clr[2] > 0) then
    result := Point(clr[0], clr[1])
  else
    result := Point(-1, -1);
end;

procedure TViewFrame.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HasMoved := false;
end;

procedure TViewFrame.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  dx := (MP.X - X);
  dy := (MP.Y - Y);  

  if (dx <> 0) and (dy <> 0) and ((ssLeft in Shift) or (ssRight in Shift)) then
    HasMoved := true;

  if (ssRight in Shift) and not (ssLeft in Shift) then begin
    Camera.turn := Camera.turn - dx/2;
    while Camera.turn < 0 do
      Camera.turn := Camera.turn + 360;
    while Camera.turn > 360 do
      Camera.turn := Camera.turn - 360;

    Camera.tilt := Camera.tilt - dy/2;
    if Camera.tilt < 5 then
      Camera.tilt := 5;
    if Camera.tilt > 90 then
      Camera.tilt := 90;
  end;

  if (ssRight in Shift) and (ssLeft in Shift) then begin
    Camera.zoom := Camera.zoom + dy;
    if Camera.zoom > -10 then
      Camera.zoom := -10;
  end;

  if not (ssRight in Shift) and (ssLeft in Shift) then begin
    Camera.pos[0] := Camera.pos[0] + cos(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;
    Camera.pos[2] := Camera.pos[2] + sin(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;

    Camera.pos[0] := Camera.pos[0] - sin(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;
    Camera.pos[2] := Camera.pos[2] + cos(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;
  end;

  if (GUIStack.Count > 0) then
    TGUILayer(GUIStack[GUIStack.Count-1]).MouseMove(X, Y);

  MP := Point(X, Y);
end;

procedure TViewFrame.FormClick(Sender: TObject);
var
  p: TPoint;
  block: TGUIBlock;
begin
  if not HasMoved then begin
    if not ((GUIStack.Count > 0) and TGUILayer(GUIStack[GUIStack.Count-1]).MouseClick(MP.X, MP.Y)) and
      Assigned(City) then begin
      p := RenderForMouseClick(MP.X, MP.Y);
      if (p.X >= 0) and (p.Y >= 0) then begin
        while GUIStack.Count > 1 do
          GUIStack.Delete(GUIStack.Count-1);

        block := TGUIBlock.Create(Self, City, p.X, p.Y);
        block.ClientRect := Rect(ClientWidth - GUI_WIDTH, 0, ClientWidth, ClientHeight);
        PushLayer(block);
      end;
    end;
    TsndInstance.Create(Sounds.EffectClick, SoundEmitter).Play().Gain:= 1;
  end;
end;

{ TGUILayer }

procedure TGUILayer.Close;
begin
  ViewFrame.PopLayer(self);
end;

constructor TGUILayer.Create;
begin
  inherited Create;
  fClickables:= TObjectList.Create(true);
end;

destructor TGUILayer.Destroy;
begin
  FreeAndNil(fClickables);
  inherited;
end;

function TGUILayer.MouseClick(X, Y: integer): Boolean;
var
  i: integer;
begin
  result := PtInRect(ClientRect, Point(X, Y));
  if result then begin
    dec(X, ClientRect.Left);
    dec(Y, ClientRect.Top);
    for i:= 0 to fClickables.Count - 1 do begin
      if PtInRect(TGUIClickable(fClickables[i]).ActiveRect, Point(X, Y)) then begin
        TGUIClickable(fClickables[i]).Click;
        break;
      end;
    end;
  end;
end;

procedure TGUILayer.MouseMove(X, Y: Integer);
begin
  fMousePos := Point(X - ClientRect.Left, Y - ClientRect.Top);
end;

procedure TGUILayer.Render;
var
  i: integer;
begin
  for i := 0 to 1 do begin
    case i of
      0: begin
        SetGLColor(ColorToRGBA(0, 0, 0, 0.75));
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      end;
      1: begin
        SetGLColor(ColorToRGBA(1, 1, 1, 1));
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      end;
    end;
    glBegin(GL_QUADS);
      glVertex2f(ClientRect.Left, ClientRect.Top);
      glVertex2f(ClientRect.Right+1, ClientRect.Top);
      glVertex2f(ClientRect.Right+1, ClientRect.Bottom+1);
      glVertex2f(ClientRect.Left, ClientRect.Bottom+1);
    glEnd;
  end;
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure TGUILayer.SetClientRect(const aRect: TRect);
begin
  fClientRect := aRect;
end;

procedure TGUILayer.ViewportResize(const aWidth, aHeight: Integer);
begin
 //DUMMY
end;

{ TGUIClickable }

procedure TGUIClickable.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

constructor TGUIClickable.Create(Rect: TRect; aOnClick: TNotifyEvent = nil);
begin
  inherited Create;
  FText:= '';
  ActiveRect:= Rect;
  FOnClick := aOnClick;
end;

procedure TViewFrame.PopLayer(const aLayer: TGUILayer = nil);
var
  i: Integer;
begin
  if not Assigned(aLayer) then begin
    if (GUIStack.Count > 0) then
      GUIStack.Delete(GUIStack.Count-1);
  end else begin
    for i := 0 to GUIStack.Count-1 do
      if GUIStack[i] = aLayer then begin
        GUIStack.Delete(i);
        break;
      end;
  end;
end;

procedure TViewFrame.PushLayer(const aLayer: TGUILayer);
begin
  GUIStack.Add(aLayer);
  aLayer.ViewportResize(ClientWidth, ClientHeight);
  if GUIStack.Count>1 then begin
    TsndInstance.Create(Sounds.EffectMenu, SoundEmitter).Play.Gain:= 0.6;
  end;
end;

procedure TViewFrame.FormResize(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(GUIStack) then
    for i := 0 to GUIStack.Count-1 do
        TGUILayer(GUIStack[i]).ViewportResize(ClientWidth, ClientHeight);
end;

procedure TViewFrame.NextRound;
begin
  if not AutoPlay then begin
    TsndInstance.Create(Sounds.EffectNextTurn, SoundEmitter).Play().Gain:= 1;
    City.Evolve;
  end;
end;

procedure TViewFrame.ToggleAutoplay;
begin
  AutoPlay:= not AutoPlay;
  if GUIStack[0] is TGUIMain then
    TGUIMain(GUIStack[0]).SetAutoplayState(AutoPlay);
end;

end.

