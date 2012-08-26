unit uViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Contnrs, Dialogs, dglOpenGL, TextSuite, AppEvnts, ExtCtrls, uCity;

type
  TGUILayer = class;
  TViewFrame = class(TForm)
    Timer1: TTimer;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
    DC: HDC;
    RC: HGLRC;

    MC: boolean;
    MP: TPoint;
    pressed: set of TMouseButton;
    FFrameCount: integer;
    LastFrameTime: Double;
    LastEvolve: single;
    PausedForInput: boolean;
    seltext: string;
    procedure HandleInputs;
    procedure RenderForMouseClick(x, y: integer);
    procedure PrepareMatrix;
  public
    { Public-Deklarationen }
    Camera: packed record
      pos: array[0..2] of Single;
      turn, tilt, zoom: Single;
    end;
    City: TCity;
    GUILayer: TGUILayer;
    procedure Timestep(DT: Single);
    procedure Render;
  end;

  TGUILayer = class
  protected
    Owner: TGUILayer;
    ClientRect: TRect;
    Clickables: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MouseClick(X, Y: integer); virtual;
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
    constructor Create(Rect: TRect);
    property Text: string read FText write FText;
    property Tag: integer read FTag write FTag;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

var
  ViewFrame: TViewFrame;

implementation

uses GLHelper, uCityBlock, uFonts, uGlobals, uGUIBlock, glBitmap;

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
    //Result.DeleteTextureOnFree := False;
    //Result.FreeDataAfterGenTexture := False;
    Result.GenTexture();
  except
    FreeAndNil(result);
  end;
end;

procedure TViewFrame.FormCreate(Sender: TObject);
begin
  randomize;

  QueryPerformanceFrequency(pfc);
  ClientWidth:= 800;
  ClientHeight:= 600;
  InitOpenGL();
  DC:= GetDC(Handle);
  RC:= CreateRenderingContext(DC, [opDoubleBuffered], 32, 24, 0, 0, 0, 0);
  ActivateRenderingContext(DC, RC);
  wglSwapIntervalEXT:= wglGetProcAddress('wglSwapIntervalEXT');
  wglSwapIntervalEXT(0);
  LastFrameTime:= GetPrecisionTime;

  TtsFont.InitTS;
  Fonts.GUIText:= TtsFont.Create('Tahoma', 12, false, []);
  Fonts.LargeText:= TtsFont.Create('Arial', 25, false, [fsUnderline]);

  Textures.BFactories:= LoadTexture('BFactories');
  Textures.BFactory:= LoadTexture('BFactory');
  Textures.BHouse:= LoadTexture('BHouse');
  Textures.BSmallIndustry:= LoadTexture('BSmallIndustry');
  Textures.BUnknown:= LoadTexture('BUnknown');

  FFrameCount:= 0;

  FillChar(Camera.pos[0], SizeOf(Camera.pos), 0);
  Camera.turn := 15;
  Camera.tilt := 35;
  Camera.zoom := -100;

  PausedForInput:= false;
  LastEvolve:= 0;

  City:= TCity.Create;
  City.LoadFromFile(ExtractFilePath(Application.ExeName)+'maps\test2.map');

  Application.OnIdle := ApplicationIdle;
end;

procedure TViewFrame.FormDestroy(Sender: TObject);
begin
  FreeAndNil(City);

  TtsFont.DoneTS;
  if RC <> 0 then begin
    DeactivateRenderingContext;
    DestroyRenderingContext(RC);
  end;
  RC:= 0;
  inherited;
end;

procedure TViewFrame.ApplicationIdle(Sender: TObject; var Done: Boolean);
const
  time_per_frame = 1000 / 60;
begin
  Done:= false;

  if GetPrecisionTime - LastFrameTime < time_per_frame then begin
    Sleep(1);
    exit;
  end;

  HandleInputs;
  Timestep((GetPrecisionTime - LastFrameTime) / 1000);
  Render;

  inc(FFrameCount);
  LastFrameTime:= GetPrecisionTime;
end;

procedure TViewFrame.Timer1Timer(Sender: TObject);
begin
  Caption:= Format('FPS: %f', [FFrameCount / (Timer1.Interval / 1000)]);
  FFrameCount:= 0;
end;

function KeyPressed(Key: Integer): Boolean;
begin
  Result:= ForegroundTask and Application.Active and (GetKeyState(Key) < 0);
end;

procedure TViewFrame.HandleInputs;
var
  ax, ay: integer;
  dx, dy: integer;
begin
  ax:= ScreenToClient(Mouse.CursorPos).x;
  ay:= ScreenToClient(Mouse.CursorPos).y;

  if mc then begin
    dy := (MP.Y - aY);
    dx := (MP.X - aX);

    if pressed = [mbRight] then begin
      Camera.turn := Camera.turn - dx;
      while Camera.turn < 0 do
        Camera.turn := Camera.turn + 360;
      while Camera.turn > 360 do
        Camera.turn := Camera.turn - 360;
        
      Camera.tilt := Camera.tilt - dy;
      if Camera.tilt < 10 then
        Camera.tilt := 10;
      if Camera.tilt > 90 then
        Camera.tilt := 90;
    end;

    if pressed = [mbRight, mbLeft] then begin
      Camera.zoom := Camera.zoom + dy;
      if Camera.zoom > -10 then
        Camera.zoom := -10;
    end;

    if pressed = [mbLeft] then begin
      Camera.pos[0] := Camera.pos[0] + cos(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;
      Camera.pos[2] := Camera.pos[2] + sin(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;

      Camera.pos[0] := Camera.pos[0] - sin(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;
      Camera.pos[2] := Camera.pos[2] + cos(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;
    end;

    MP.X:= ax;
    MP.y:= ay;

    if PausedForInput then begin
      if pressed = [mbLeft] then begin
        pressed:= [];

        if not Assigned(GUILayer) then
          RenderForMouseClick(MP.X, mp.Y)
        else
          GUILayer.MouseClick(MP.X, mp.Y);
      end;
    end;
  end;

  dx := 0;
  dy := 0;
  if KeyPressed(VK_LEFT) then
    dx := dx - 50;
  if KeyPressed(VK_RIGHT) then
    dx := dx + 50;
  if KeyPressed(VK_DOWN) then
    dy := dy + 50;
  if KeyPressed(VK_UP) then
    dy := dy - 50;
  if KeyPressed(VK_SPACE) then
    PausedForInput:= true;

  Camera.pos[0] := Camera.pos[0] + cos(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;
  Camera.pos[2] := Camera.pos[2] + sin(Camera.turn/180*Pi)*dx * Camera.zoom / 1000;
  Camera.pos[0] := Camera.pos[0] - sin(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;
  Camera.pos[2] := Camera.pos[2] + cos(Camera.turn/180*Pi)*dy * Camera.zoom / 1000;    
end;

procedure TViewFrame.Timestep(DT: Single);
begin
  City.Progress(DT);
  if not PausedForInput then begin
    LastEvolve:= LastEvolve + DT;
    if LastEvolve >= 0.1 then begin
      City.CreateRandomCar;
      City.CreateRandomCar;
      City.CreateRandomCar;
      City.CreateRandomCar;
      City.CreateRandomCar;
            
      City.Evolve;
      LastEvolve:= 0;
    end;
  end;
end;

procedure TViewFrame.PrepareMatrix;
const
  FOV = 40;
  CLIP_NEAR = 0.1;
  CLIP_FAR = 1000;
begin
  glMatrixMode(GL_PROJECTION);
  glViewport(0, 0, ClientWidth, ClientHeight);
  glLoadIdentity();
  //TODO: Orto!!!!
  gluPerspective(FOV, ClientWidth / ClientHeight, CLIP_NEAR, CLIP_FAR);

  glMatrixMode(GL_MODELVIEW);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glEnable(GL_DEPTH_TEST);
end;

procedure TViewFrame.Render;
begin
  PrepareMatrix;
  glEnable(GL_LIGHTING);

  glPushMatrix;
  glLoadIdentity;
  glTranslatef(0, 0, Camera.zoom);
  glRotatef(Camera.tilt, 1, 0, 0);
  glRotatef(Camera.turn, 0, 1, 0);  
  glTranslatef(Camera.pos[0], Camera.pos[1], Camera.pos[2]);
  City.Render(false);
  if PausedForInput then
    ;
  glPopMatrix;

  Enter2dMode(800, 600);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  if PausedForInput then begin
    tsTextColor3f(1, 0, 0);
    Fonts.LargeText.TextOut(400, 550, 'Pause', TS_ALIGN_CENTER);
  end;

  tsTextColor3f(1, 1, 1);
  Fonts.LargeText.TextOut(400, 50, seltext, TS_ALIGN_CENTER);

  glDisable(GL_TEXTURE_2D);
  if Assigned(GUILayer) then
    GUILayer.Render;
  Exit2dMode;

  SwapBuffers(DC);
end;

procedure TViewFrame.RenderForMouseClick(x, y: integer);
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
  case clr[2] of
    0: seltext:= 'nothing';
    1..9: seltext:= Format('bld %d @ %d %d', [clr[2] - 1, clr[0], clr[1]]); // building
    255: seltext:= Format('block %d %d', [clr[0], clr[1]]); // block
  end;
  if clr[2] > 0 then begin
    GUILayer:= TGUIBlock.Create(City, clr[0], clr[1]);
  end else
    PausedForInput:= false;
end;

procedure TViewFrame.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MP:= Point(x, y);
  MC:= true;
  include(pressed, Button);
end;

procedure TViewFrame.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mc:= False;
  Exclude(pressed, Button);
end;

{ TGUILayer }

procedure TGUILayer.Close;
begin
  if Assigned(Owner) then begin
    ViewFrame.GUILayer:= Owner;
  end else begin
    ViewFrame.GUILayer:= nil;
  end;
  Free;
end;

constructor TGUILayer.Create;
begin
  inherited Create;
  Clickables:= TObjectList.Create(true);
end;

destructor TGUILayer.Destroy;
begin
  FreeAndNil(Clickables);
  inherited;
end;

procedure TGUILayer.MouseClick(X, Y: integer);
var
  i: integer;
begin
  if not PtInRect(ClientRect, Point(X, Y)) then begin
    Close;
    exit;
  end;
  for i:= 0 to Clickables.Count - 1 do begin
    if PtInRect(TGUIClickable(Clickables[i]).ActiveRect, Point(X, Y)) then begin
      TGUIClickable(Clickables[i]).Click;
      break;
    end;
  end;
end;

procedure TGUILayer.Render;
var
  i: integer;
begin
  if Assigned(Owner) then
    Owner.Render;

  glBegin(GL_QUADS);
  SetGLColor(ColorToRGBA(clSilver));
  glVertex2f(ClientRect.Left, ClientRect.Top);
  glVertex2f(ClientRect.Right, ClientRect.Top);
  glVertex2f(ClientRect.Right, ClientRect.Bottom);
  glVertex2f(ClientRect.Left, ClientRect.Bottom);
  glEnd;

  for i:= 0 to Clickables.Count - 1 do begin
    with TGUIClickable(Clickables[i]) do begin
      glBegin(GL_QUADS);
      SetGLColor(ColorToRGBA(clGray));
      glVertex2f(ActiveRect.Left, ActiveRect.Top);
      glVertex2f(ActiveRect.Right, ActiveRect.Top);
      glVertex2f(ActiveRect.Right, ActiveRect.Bottom);
      glVertex2f(ActiveRect.Left, ActiveRect.Bottom);
      glEnd;
      tsTextColor3f(0, 0, 0);
      Fonts.GUIText.TextOut((ActiveRect.Right + ActiveRect.Left) div 2,
       (ActiveRect.Top + ActiveRect.Bottom) div 2 + 6, FText, TS_ALIGN_CENTER);
    end;
  end;
end;

{ TGUIClickable }

procedure TGUIClickable.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

constructor TGUIClickable.Create(Rect: TRect);
begin
  inherited Create;
  FText:= '';
  ActiveRect:= Rect;
end;

end.

