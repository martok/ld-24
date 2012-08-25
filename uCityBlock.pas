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
    procedure RenderSimple(Color: TRGBA; Height: Single);
  public
    procedure RenderSelect(r, g, b: byte);
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

{ TBuilding }

function TBuilding.ClickHeight: single;
begin
  Result:= 5;
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
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);  // Point 3 (Front)
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0,  1.0,  1.0);  // Point 4 (Front)
    // Back Face
    glNormal3f( 0.0, 0.0,0.0);                  // Normal Pointing Away From Viewer
    glTexCoord2f(1.0, 0.0); glVertex3f(0.0, 0.0, 0.0);  // Point 1 (Back)
    glTexCoord2f(1.0, 1.0); glVertex3f(0.0,  1.0, 0.0);  // Point 2 (Back)
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, 0.0);  // Point 3 (Back)
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, 0.0, 0.0);  // Point 4 (Back)
    // Top Face
    glNormal3f( 0.0, 1.0, 0.0);                  // Normal Pointing Up
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0,  1.0, 0.0);  // Point 1 (Top)
    glTexCoord2f(0.0, 0.0); glVertex3f(0.0,  1.0,  1.0);  // Point 2 (Top)
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);  // Point 3 (Top)
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, 0.0);  // Point 4 (Top)
    // Bottom Face
    glNormal3f( 0.0,0.0, 0.0);                  // Normal Pointing Down
    glTexCoord2f(1.0, 1.0); glVertex3f(0.0, 0.0, 0.0);  // Point 1 (Bottom)
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, 0.0, 0.0);  // Point 2 (Bottom)
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, 0.0,  1.0);  // Point 3 (Bottom)
    glTexCoord2f(1.0, 0.0); glVertex3f(0.0, 0.0,  1.0);  // Point 4 (Bottom)
    // Right face
    glNormal3f( 1.0, 0.0, 0.0);                  // Normal Pointing Right
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, 0.0, 0.0);  // Point 1 (Right)
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, 0.0);  // Point 2 (Right)
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);  // Point 3 (Right)
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, 0.0,  1.0);  // Point 4 (Right)
    // Left Face
    glNormal3f(0.0, 0.0, 0.0);                  // Normal Pointing Left
    glTexCoord2f(0.0, 0.0); glVertex3f(0.0, 0.0, 0.0);  // Point 1 (Left)
    glTexCoord2f(1.0, 0.0); glVertex3f(0.0, 0.0,  1.0);  // Point 2 (Left)
    glTexCoord2f(1.0, 1.0); glVertex3f(0.0,  1.0,  1.0);  // Point 3 (Left)
    glTexCoord2f(0.0, 1.0); glVertex3f(0.0,  1.0, 0.0);  // Point 4 (Left)
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

end.

