unit uCityBlock;

interface

uses
  SysUtils, dglOpenGL, GLHelper, glBitmap, uGlobals;

type
  TBuilding = class;

  { TCityBlock }

  TBlockType = (btNormal = 0, btWater = 1);
  TCityBlock = class
  private
    FFields: array[0..8] of TBuilding;
    FPosX, FPosY: integer;
    FPeople: single;
    FLuxury: single;
    FPollution: single;
    FIndustry: single;
    FEducation: single;
    FSpace: single;
    FBlockType: TBlockType;
    FGrowthRate: single;
    function GetBuilding(Index: integer): TBuilding;
    procedure SetBuilding(Index: integer; const Value: TBuilding);
  protected
    procedure RenderFloor;
  public
    constructor Create(X, Y: Integer; BlockType: TBlockType);
    destructor Destroy; override;

    property Industry: single read FIndustry write FIndustry;
    property Pollution: single read FPollution write FPollution;
    property Education: single read FEducation write FEducation;
    property Luxury: single read FLuxury write FLuxury;
    property Space: single read FSpace write FSpace;

    property GrowthRate: single read FGrowthRate write FGrowthRate;
    property People: single read FPeople write FPeople;
    property Building[Index: integer]: TBuilding read GetBuilding write SetBuilding;
    procedure Render(Selection: boolean);
  end;

  TBuildingClass = class of TBuilding;
  TBuilding = class
  private
  protected
    FRndHeight: Single;
    FColor: TRGBA;
    FSelectMode: boolean;
    // !!! DOES NOT SET ANY COLORS, IF FSelectMode = TRUE !!!
    procedure RenderShape(Height: Single); virtual;
  public
    constructor Create; virtual;
    class function Texture: TglBitmap2D; virtual;
    procedure RenderSelect(r, g, b: byte; Height: Single);
    procedure Render(aHeight: Single);

    function SLivingSpace: integer; virtual;
    function SIndustryValue: integer; virtual;
    function SPollution: integer; virtual;
    function SEducation: integer; virtual;
    function SLuxury: integer; virtual;

    function SRange: integer; virtual;
    function SEffectLoss: single; virtual;

    function DisplayName: string; virtual;
  end;

implementation

uses
  uBldHouse;

{ TCityBlock }

constructor TCityBlock.Create(X, Y: Integer; BlockType: TBlockType);
begin
  inherited Create;
  FPosX:= X;
  FPosY:= Y;
  FBlockType := BlockType;
  FFields[0]:= TBHouse.Create;
  FFields[1]:= TBHouse.Create;
  FFields[2]:= TBHouse.Create;
  FFields[3]:= TBHouse.Create;
  FFields[5]:= TBHouse.Create;
  FFields[6]:= TBHouse.Create;
  FFields[7]:= TBHouse.Create;
  FFields[8]:= TBHouse.Create;
  FFields[4]:= TBAppartement.Create;
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
  if not Selection then begin
    glDisable(GL_LIGHTING);
    RenderFloor;
    glEnable(GL_LIGHTING);    
  end;
  for b:= 0 to high(FFields) do begin
    if Assigned(FFields[b]) then begin
      x:= b mod 3 - 1;
      y:= b div 3 - 1;
      glPushMatrix;
      glTranslatef(4*x, 0, 4*y);
      glScalef(1.5, 1, 1.5);
      if Selection then
        FFields[b].RenderSelect(FPosX, FPosY, b+1, 1 + FPeople/25)
      else
        FFields[b].Render(1 + FPeople/25);
      glPopMatrix;
    end;
  end;
end;

procedure TCityBlock.RenderFloor;
begin
  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LINE_BIT);
  glBegin(GL_LINE_LOOP);
    //SetGLMaterial(ColorToRGBA(0.9, 0.9, 0.9));
    glLineWidth(2);
    glColor4f(1, 1, 1, 1);
    glVertex3f( 6, 0,-6);
    glVertex3f(-6, 0,-6);
    glVertex3f(-6, 0, 6);
    glVertex3f( 6, 0, 6);
  glEnd;
  glPopAttrib;
end;

procedure TCityBlock.SetBuilding(Index: integer; const Value: TBuilding);
begin
  FFields[Index]:= Value;
end;

{ TBuilding }

constructor TBuilding.Create;
begin
  inherited;
  FRndHeight := (random - 0.5) / 4;
end;

procedure TBuilding.Render(aHeight: Single);
begin
  FSelectMode:= false;

  if aHeight < 0.5 then
    aHeight := 0.5;
  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LINE_BIT);
  glEnable(GL_LIGHTING);
  SetGLMaterial(FColor);
  RenderShape(aHeight);

  glDisable(GL_LIGHTING);
  SetGLColor(FColor);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2);
  RenderShape(aHeight);
  glPopAttrib;
end;

procedure TBuilding.RenderSelect(r, g, b: byte; Height: Single);
begin
  FSelectMode:= true;
  try
    if Height < 0.5 then
      Height := 0.5;
    glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LINE_BIT);
    glDisable(GL_LIGHTING);
    glColor3ub(r,g,b);
    RenderShape(Height);
    glPopAttrib;
  finally
    FSelectMode:= false;
  end;
end;

procedure TBuilding.RenderShape(Height: Single);
begin
  glBegin(GL_QUADS);
    // Front Face
  glNormal3f( 0.0, 0.0, 1.0);                  // Normal Pointing Towards Viewer
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, 0.0,  1.0);  // Point 1 (Front)
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, 0.0,  1.0);  // Point 2 (Front)
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  Height,  1.0);  // Point 3 (Front)
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  Height,  1.0);  // Point 4 (Front)
  // Back Face
  glNormal3f( 0.0, 0.0, -1.0);                  // Normal Pointing Away From Viewer
  glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, 0.0, -1.0);  // Point 1 (Back)
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  Height, -1.0);  // Point 2 (Back)
  glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  Height, -1.0);  // Point 3 (Back)
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, 0.0, -1.0);  // Point 4 (Back)

  // Top Face
  glNormal3f( 0.0, 1.0, 0.0);                  // Normal Pointing Up
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  Height, -1.0);  // Point 1 (Top)
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  Height,  1.0);  // Point 2 (Top)
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  Height,  1.0);  // Point 3 (Top)
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  Height, -1.0);  // Point 4 (Top)
  if not FSelectMode then begin
    glEnd;

    Texture.Bind(true);
    glPushAttrib(GL_LIGHTING_BIT or GL_ENABLE_BIT or GL_DEPTH_FUNC);
    glDisable(GL_LIGHTING);
    glEnable(GL_BLEND);
    glDepthFunc(GL_LEQUAL);
    glColor3f(1,1,1);

    glBegin(GL_QUADS);
    glNormal3f( 0.0, 1.0, 0.0);                  // Normal Pointing Up
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  Height, -1.0);  // Point 1 (Top)
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  Height,  1.0);  // Point 2 (Top)
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  Height,  1.0);  // Point 3 (Top)
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  Height, -1.0);  // Point 4 (Top)
    glEnd;

    Texture.Unbind(true);
    glPopAttrib;

    glBegin(GL_QUADS);
  end;


  // Right face
  glNormal3f( 1.0, 0.0, 0.0);                  // Normal Pointing Right
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, 0.0, -1.0);  // Point 1 (Right)
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  Height, -1.0);  // Point 2 (Right)
  glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  Height,  1.0);  // Point 3 (Right)
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, 0.0,  1.0);  // Point 4 (Right)
  // Left Face
  glNormal3f(-1.0, 0.0, 0.0);                  // Normal Pointing Left
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, 0.0, -1.0);  // Point 1 (Left)
  glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, 0.0,  1.0);  // Point 2 (Left)
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  Height,  1.0);  // Point 3 (Left)
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  Height, -1.0);  // Point 4 (Left)
  glEnd;
end;

function TBuilding.SLuxury: integer;
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

function TBuilding.SEducation: integer;
begin
  Result:= 0;
end;

function TBuilding.SRange: integer;
begin
  Result:= 1;
end;

function TBuilding.SEffectLoss: single;
begin
  Result:= 0.5;
end;

class function TBuilding.Texture: TglBitmap2D;
begin
  Result:= textures.BUnknown;
end;

function TBuilding.DisplayName: string;
begin
  Result:= ClassName;
end;

end.

