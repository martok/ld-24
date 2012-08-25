unit uCityBlock;

interface

uses
  SysUtils, dglOpenGL, GLHelper;

type
  TBuilding = class;

  { TCityBlock }

  TCityBlock = class
  private
    FFields: array[0..8] of TBuilding;
    FPosX, FPosY: integer;
    FPeople: integer;
    FLuxury: integer;
    FHappiness: integer;
    FPollution: integer;
    FIndustry: integer;
    FEducation: integer;
    FSpace: integer;
    function GetBuilding(Index: integer): TBuilding;
    procedure SetBuilding(Index: integer; const Value: TBuilding);
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
    property Space: integer read FSpace;
    property Happiness: integer read FHappiness;
    property Building[Index: integer]: TBuilding read GetBuilding write SetBuilding;
    procedure Render(Selection: boolean);
    procedure Update;
  end;

  TBuildingClass = class of TBuilding;
  TBuilding = class
  private
  protected
    function ClickHeight: single; virtual;
    procedure RenderSimple(Color: TRGBA; Height: Single);
  public
    constructor Create; virtual;
    procedure RenderSelect(r, g, b: byte);
    procedure Render; virtual;
    function SLivingSpace: integer; virtual;
    function SIndustryValue: integer; virtual;
    function SPollution: integer; virtual;
    function SHappiness: integer; virtual;
  end;

implementation

uses
  uBldHouse;

{ TCityBlock }

constructor TCityBlock.Create(X, Y: Integer);
begin
  inherited Create;
  FPosX:= X;
  FPosY:= Y;
  FFields[0]:= TBHouse.Create;
  FFields[5]:= TBAppartement.Create;
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
  b, x, y: integer;
begin
  if not Selection then
    RenderFloor;

  for b:= 0 to high(FFields) do begin
    if Assigned(FFields[b]) then begin
      x:= b mod 3;
      y:= b div 3;
      glPushMatrix;
      glTranslatef(x, 0, y);
      if Selection then
        FFields[b].RenderSelect(FPosX, FPosY, b+1)
      else
        FFields[b].Render;
      glPopMatrix;
    end;
  end;
end;

procedure TCityBlock.RenderFloor;
begin
  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LINE_BIT);
  glEnable(GL_LIGHTING);
  glBegin(GL_QUADS);
  SetGLMaterial(ColorToRGBA(0.9, 0.9, 0.9));
  glVertex3f(3, 0, 0);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 0, 3);
  glVertex3f(3, 0, 3);
  glEnd;
  glDisable(GL_LIGHTING);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2);
  glBegin(GL_QUADS);
  SetGLColor(c_Black);
  glVertex3f(3, 0, 0);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 0, 3);
  glVertex3f(3, 0, 3);
  glEnd;
  glPopAttrib;
end;

procedure TCityBlock.SetBuilding(Index: integer; const Value: TBuilding);
begin
  FFields[Index]:= Value;
end;

procedure TCityBlock.Update;
var
  i: integer;
  b: TBuilding;
  ind, living, happi, pollu: integer;
begin
  ind:= 0;
  living:= 50;
  happi:= 0;
  pollu:= 0;
  for i:= 0 to 8 do
    if Assigned(FFields[i]) then begin
      b:= TBuilding(FFields[i]);
      inc(ind, b.SIndustryValue);
      inc(living, b.SLivingSpace);
      inc(pollu, b.SPollution);
      inc(happi, b.SHappiness);
    end;

  FHappiness:= happi;
  FPollution:= pollu;
  FIndustry:= ind;
  FSpace:= living;
end;

{ TBuilding }

function TBuilding.ClickHeight: single;
begin
  Result:= 5;
end;

constructor TBuilding.Create;
begin
  inherited;
end;

procedure TBuilding.Render;
begin
end;

procedure TBuilding.RenderSelect(r, g, b: byte);
begin
  glBegin(GL_QUADS);
  glColor3ub(r, g, b);
  glVertex3f(0, 0, 0);
  glVertex3f(0, 0, 1);
  glVertex3f(0, ClickHeight, 1);
  glVertex3f(0, ClickHeight, 0);

  glVertex3f(0, 0, 1);
  glVertex3f(1, 0, 1);
  glVertex3f(1, ClickHeight, 1);
  glVertex3f(0, ClickHeight, 1);

  glVertex3f(0, ClickHeight, 0);
  glVertex3f(0, ClickHeight, 1);
  glVertex3f(1, ClickHeight, 1);
  glVertex3f(1, ClickHeight, 0);
  glEnd;
end;

procedure TBuilding.RenderSimple(Color: TRGBA; Height: Single);
  procedure Cube;
  begin
    glBegin(GL_QUADS);
      // Front Face
    glNormal3f( 0.0, 0.0, 1.0);                  // Normal Pointing Towards Viewer
    glTexCoord2f(0.0, 0.0); glVertex3f(0.0, 0.0,  1.0);  // Point 1 (Front)
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, 0.0,  1.0);  // Point 2 (Front)
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  Height,  1.0);  // Point 3 (Front)
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0,  Height,  1.0);  // Point 4 (Front)
    // Back Face
    glNormal3f( 0.0, 0.0,0.0);                  // Normal Pointing Away From Viewer
    glTexCoord2f(1.0, 0.0); glVertex3f(0.0, 0.0, 0.0);  // Point 1 (Back)
    glTexCoord2f(1.0, 1.0); glVertex3f(0.0,  Height, 0.0);  // Point 2 (Back)
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  Height, 0.0);  // Point 3 (Back)
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, 0.0, 0.0);  // Point 4 (Back)
    // Top Face
    glNormal3f( 0.0, 1.0, 0.0);                  // Normal Pointing Up
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0,  Height, 0.0);  // Point 1 (Top)
    glTexCoord2f(0.0, 0.0); glVertex3f(0.0,  Height,  1.0);  // Point 2 (Top)
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  Height,  1.0);  // Point 3 (Top)
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  Height, 0.0);  // Point 4 (Top)
    // Right face
    glNormal3f( 1.0, 0.0, 0.0);                  // Normal Pointing Right
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, 0.0, 0.0);  // Point 1 (Right)
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  Height, 0.0);  // Point 2 (Right)
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  Height,  1.0);  // Point 3 (Right)
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, 0.0,  1.0);  // Point 4 (Right)
    // Left Face
    glNormal3f(0.0, 0.0, 0.0);                  // Normal Pointing Left
    glTexCoord2f(0.0, 0.0); glVertex3f(0.0, 0.0, 0.0);  // Point 1 (Left)
    glTexCoord2f(1.0, 0.0); glVertex3f(0.0, 0.0,  1.0);  // Point 2 (Left)
    glTexCoord2f(1.0, 1.0); glVertex3f(0.0,  Height,  1.0);  // Point 3 (Left)
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0,  Height, 0.0);  // Point 4 (Left)
    glEnd;
  end;
begin
  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LINE_BIT);
  glEnable(GL_LIGHTING);
  SetGLMaterial(Color);
  Cube;

  glDisable(GL_LIGHTING);
  SetGLColor(Color);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2);
  Cube;
  glPopAttrib;
end;

function TBuilding.SHappiness: integer;
begin
  Result:= 0;
end;

function TBuilding.SIndustryValue: integer;
begin
  Result:= 0;
end;

function TBuilding.SLivingSpace: integer;
begin
  Result:= 0;
end;

function TBuilding.SPollution: integer;
begin
  Result:= 0;
end;

end.

