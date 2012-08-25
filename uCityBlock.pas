unit uCityBlock;

interface

uses
  SysUtils,dglOpenGL;

type
  TBuilding = class;

  { TCityBlock }

  TCityBlock = class
  private
    FFields: array[0..8] of TBuilding;
    FPosX, FPosY: integer;
    FPeople: integer;
    FLuxury: integer;
    FSatisfaction: integer;
    FPollution: integer;
    FIndustry: integer;
    FEducation: integer;
    function GetBuilding(Index: integer): TBuilding;
  protected
    procedure RenderFloor;
  public
    constructor Create(X, Y: Integer);
    destructor Destroy; override;
    property Industry: integer read FIndustry;
    property Pollution: integer read FPollution;
    property Education: integer read FEducation;
    property Luxury: integer read FLuxury;
    property People: integer read FPeople;
    property Satisfaction: integer read FSatisfaction;
    property Building[Index: integer]: TBuilding read GetBuilding;
    procedure Render(Selection: boolean);
  end;

  TBuilding = class
  private
  protected
    function ClickHeight: single; virtual;
  public
    procedure RenderSelect(r,g,b: byte);
    procedure Render; virtual;
  end;

implementation

uses
  uBldElementarySchool;

{ TCityBlock }

constructor TCityBlock.Create(X, Y: Integer);
begin
  inherited Create;
  FPosX:= X;
  FPosY:= Y;
  FFields[0]:= TBElementarySchool.Create;
  FFields[2]:= TBElementarySchool.Create;
end;

destructor TCityBlock.Destroy;
var
  b: integer;
begin
  for b:= 0 to high(FFields) do
    FreeAndNil(FFields[b]);
  inherited;
end;

function TCityBlock.GetBuilding(Index: integer): TBuilding;
begin
  Result:= FFields[Index];
end;

procedure TCityBlock.Render(Selection: boolean);
var
  b,x,y: integer;
begin
  if not Selection then
    RenderFloor;

  for b:= 0 to high(FFields) do begin
    if Assigned(FFields[b]) then begin
      x:= b mod 3;
      y:= b div 3;
      glPushMatrix;
        glTranslatef(x,0,y);
        if Selection then
          FFields[b].RenderSelect(FPosX,FPosY,b)
        else
          FFields[b].Render;
      glPopMatrix;
    end;
  end;
end;

procedure TCityBlock.RenderFloor;
begin
  glBegin(GL_QUADS);
  glColor3f(0.5, 0.5, 0.5);
  glVertex3f(0.02, 0, 0.02);
  glVertex3f(2.98, 0, 0.02);
  glVertex3f(2.98, 0, 2.98);
  glVertex3f(0.02, 0, 2.98);
  glEnd;
end;

{ TBuilding }

function TBuilding.ClickHeight: single;
begin
  Result:= 5;
end;

procedure TBuilding.Render;
begin

end;

procedure TBuilding.RenderSelect(r,g,b: byte);
begin
  glBegin(GL_QUADS);
  glColor3ub(r,g,b);
  glVertex3f(0,0,0);
  glVertex3f(0,0,1);
  glVertex3f(0,ClickHeight,1);
  glVertex3f(0,ClickHeight,0);

  glVertex3f(0,0,1);
  glVertex3f(1,0,1);
  glVertex3f(1,ClickHeight,1);
  glVertex3f(0,ClickHeight,1);

  glVertex3f(0,ClickHeight,0);
  glVertex3f(0,ClickHeight,1);
  glVertex3f(1,ClickHeight,1);
  glVertex3f(1,ClickHeight,0);
  glEnd;
end;

end.

