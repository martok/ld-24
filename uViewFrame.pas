unit uViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dglOpenGL, AppEvnts, ExtCtrls, Camera;

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
    procedure HandleInputs;
  public
    { Public-Deklarationen }
    Camera: TCamera;
    procedure Timestep(DT: Single);
    procedure Render;
  end;

var
  ViewFrame: TViewFrame;

implementation

{$R *.dfm}

procedure TViewFrame.FormCreate(Sender: TObject);
begin
  InitOpenGL();
  DC:= GetDC(Handle);
  RC:= CreateRenderingContext(DC, [opDoubleBuffered], 32, 24, 0, 0, 0, 0);
  ActivateRenderingContext(DC, RC);
  glEnable(GL_DEPTH_TEST);
  wglSwapIntervalEXT:= wglGetProcAddress('wglSwapIntervalEXT');
  wglSwapIntervalEXT(0);
  FFrameCount:= 0;

  Camera:= TCamera.Create;
  Camera.Y:= 20;
  Camera.X:= -20;
  Camera.Z:= 20;
  Camera.Beta:= -45;
  Camera.Alpha:= -45;
end;

procedure TViewFrame.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Camera);
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
  HandleInputs;

  if GetTickCount - LastFrame < time_per_frame then begin
    Sleep(1);
    exit;
  end;

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
      if pressed = [mbLeft] then begin
        if ((dy > 0) and (Camera.Y > 2)) or
          ((dy < 0) and (Camera.Y < 100)) then
          BewegReinRaus(dy)
      end;
      {      if pressed=[mbRight,mbLeft] then begin
              Dist := Dist   - (dy div 2);
            end;       }
    end;
    MP.X:= ax;
    MP.y:= ay;
  end;

  if KeyPressed(VK_LEFT) then
    Camera.BewegLinksRechts(-2);
  if KeyPressed(VK_RIGHT) then
    Camera.BewegLinksRechts(+2);
  if KeyPressed(VK_DOWN) then
    Camera.BewegVorZuruck(-2);
  if KeyPressed(VK_UP) then
    Camera.BewegVorZuruck(+2);
end;

procedure TViewFrame.Timestep(DT: Single);
begin

end;

procedure TViewFrame.Render;
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

  glPushMatrix;
  Camera.Apply;
  glBegin(GL_TRIANGLES);
  glColor3f(1, 0, 0);
  glVertex3f(-1, -1, 0);
  glColor3f(0, 0, 1);
  glVertex3f(1, -1, 0);
  glColor3f(0, 1, 0);
  glVertex3f(0, 1, 0);
  glEnd;
  glPopMatrix;

  SwapBuffers(DC);
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

