unit uViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dglOpenGL, TextSuite, AppEvnts, ExtCtrls, Camera, uCity;

type
  TViewFrame = class(TForm)
    ApplicationEvents1: TApplicationEvents;
    Timer1: TTimer;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
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
    LastFrame: cardinal;
    LastEvolve: single;
    PausedForInput: boolean;
    seltext: string;
    procedure HandleInputs;
    procedure RenderForMouseClick(x,y: integer);
    procedure PrepareMatrix;
  public
    { Public-Deklarationen }
    Camera: TCamera;
    City: TCity;
    procedure Timestep(DT: Single);
    procedure Render;
  end;

var
  ViewFrame: TViewFrame;

implementation

uses GLHelper, uCityBlock, uFonts, uGlobals;

{$R *.dfm}

procedure TViewFrame.FormCreate(Sender: TObject);
begin
  ClientWidth:= 800;
  ClientHeight:= 600;
  InitOpenGL();
  DC:= GetDC(Handle);
  RC:= CreateRenderingContext(DC, [opDoubleBuffered], 32, 24, 0, 0, 0, 0);
  ActivateRenderingContext(DC, RC);
  wglSwapIntervalEXT:= wglGetProcAddress('wglSwapIntervalEXT');
  wglSwapIntervalEXT(0);

  TtsFont.InitTS;
  Fonts.LargeText:= TtsFont.Create('Arial', 25, false, [fsUnderline]);

  FFrameCount:= 0;

  Camera:= TCamera.Create;
  Camera.Y:= 50;
  Camera.X:= -20;
  Camera.Z:= 50;
  Camera.Beta:= -45;
  Camera.Alpha:= -45;
  PausedForInput:= false;
  LastEvolve:= 0;

  City:= TCity.Create;
end;

procedure TViewFrame.FormDestroy(Sender: TObject);
begin
  FreeAndNil(City);
  FreeAndNil(Camera);

  TtsFont.DoneTS;
  if RC <> 0 then begin
    DeactivateRenderingContext;
    DestroyRenderingContext(RC);
  end;
  RC:= 0;
  inherited;
end;

procedure TViewFrame.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
const
  time_per_frame = 1000 / 60;
begin
  Done:= false;

  if GetTickCount - LastFrame < time_per_frame then begin
    Sleep(1);
    exit;
  end;

  HandleInputs;
  Timestep((GetTickCount - LastFrame) / 1000);
  Render;

  inc(FFrameCount);
  LastFrame:= GettickCount;
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
    dy:= (MP.Y - aY);
    dx:= (MP.X - aX);
    with Camera do begin
      if pressed = [mbRight] then begin
        BewegVorZuruck(-dy * 0.3);
        BewegLinksRechts(dx * 0.3);
      end;
      if pressed = [mbMiddle] then begin
        if ((dy > 0) and (Camera.Y > 4)) or
          ((dy < 0) and (Camera.Y < 100)) then
          BewegReinRaus(dy*0.5)
      end;
    end;
    MP.X:= ax;
    MP.y:= ay;

    if pressed=[mbLeft] then begin
      pressed:=[];
      RenderForMouseClick(MP.X, mp.Y);
    end;
  end;

  if KeyPressed(VK_LEFT) then
    Camera.BewegLinksRechts(-2);
  if KeyPressed(VK_RIGHT) then
    Camera.BewegLinksRechts(+2);
  if KeyPressed(VK_DOWN) then
    Camera.BewegVorZuruck(-2);
  if KeyPressed(VK_UP) then
    Camera.BewegVorZuruck(+2);
  if KeyPressed(VK_SPACE) then
    PausedForInput:= true;
end;

procedure TViewFrame.Timestep(DT: Single);
begin
  if not PausedForInput then begin
    LastEvolve:= LastEvolve + DT;
    if LastEvolve >= 1 then begin
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
  Camera.Apply;
  City.Render(false);
  if PausedForInput then
    ;
  glPopMatrix;

  Enter2dMode(800,600);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    tsTextColor3f(1,1,1);
    Fonts.LargeText.TextOut(400,50,seltext, TS_ALIGN_CENTER);
  Exit2dMode;

  SwapBuffers(DC);
end;

procedure TViewFrame.RenderForMouseClick(x,y: integer);
var
  clr: Array[0..2] of byte;
begin
  PrepareMatrix;
  glDisable(GL_LIGHTING);

  glPushMatrix;
  Camera.Apply;
  City.Render(true);
  glPopMatrix;

  glReadPixels(x, ClientHeight-y, 1,1,GL_RGB,GL_UNSIGNED_BYTE,@clr);
  case clr[2] of
    0: seltext:= 'nothing';
    1..9: seltext:= Format('bld %d @ %d %d',[clr[2]-1, clr[0],clr[1]]); // building
    255: seltext:= Format('block %d %d',[clr[0],clr[1]]); // block
  end;
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

end.

