unit uCityBlock;

interface

uses
  SysUtils, dglOpenGL, GLHelper, glBitmap, uGlobals;

type
  TBuilding = class;

  { TCityBlock }

  TBlockType = (btNormal = 0, btIndustry = 1, btResearch = 2, btHouse = 3, btLuxury = 4);
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
    FDisplayList: GLuint;
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
    procedure RenderGeometry(Selection: boolean);
    procedure Render(Selection: boolean);
    procedure RenderFocus;
    procedure UpdateDisplayList;    
  end;

  TBuildingClass = class of TBuilding;
  TBuilding = class
  private
  protected
    FRndHeight: Single;
    FColor: TRGBA;
    FSelectMode: boolean;
    // !!! DOES NOT SET ANY COLORS, IF FSelectMode = TRUE !!!
    procedure RenderShape(Height: Single; Outline: Boolean = false); virtual;
  public
    constructor Create; virtual;
    procedure RenderSelect(r, g, b: byte; Height: Single);
    procedure Render(aHeight: Single);

    function SLivingSpace: integer; virtual;
    function SIndustryValue: integer; virtual;
    function SPollution: integer; virtual;
    function SEducation: integer; virtual;
    function SLuxury: integer; virtual;

    function SRange: integer; virtual;
    function SEffectLoss: single; virtual;

    class function Texture: TglBitmap2D; virtual;
    class function DisplayName: string; virtual;
    class function Price: Integer; virtual;
    class function MinEducation: Integer; virtual;
  end;

implementation

uses
  uBldHouse, uBldSpecial;

{ TCityBlock }

constructor TCityBlock.Create(X, Y: Integer; BlockType: TBlockType);
var
  i: Integer;
begin
  inherited Create;
  FPosX:= X;
  FPosY:= Y;
  FBlockType := BlockType;
  FDisplayList := glGenLists(1);

  for i := 0 to 8 do
    FFields[i] := TBAppartement1stClass.Create;

  UpdateDisplayList;
end;

destructor TCityBlock.Destroy;
var
  b: integer;
begin
  glDeleteLists(FDisplayList, 1);
  for b:= 0 to high(FFields) do
    FreeAndNil(FFields[b]);
  inherited;
end;

function TCityBlock.GetBuilding(Index: integer): TBuilding;
begin
  Result:= FFields[Index];
end;

procedure TCityBlock.RenderGeometry(Selection: boolean);
var
  b, x, y: integer;
begin
  if not Selection then begin
    glDisable(GL_LIGHTING);
    RenderFloor;
    glEnable(GL_LIGHTING);    
  end;
  for b:= 0 to high(FFields) do begin
    x:= b mod 3 - 1;
    y:= b div 3 - 1;
    glPushMatrix;
    glTranslatef(4*x, 0, 4*y);
    //glScalef(1.5, 1, 1.5);    
    if Assigned(FFields[b]) then begin
      if Selection then
        FFields[b].RenderSelect(FPosX, FPosY, b+1, 1 + FPeople/10)
      else
        FFields[b].Render(1 + FPeople/10);
    end else begin
      if not Selection then begin
        glDisable(GL_LIGHTING);
        glColor4f(1, 1, 1, 1);
        glDepthFunc(GL_LEQUAL);
        glBegin(GL_LINE_LOOP);
          glVertex3f(-1.5, 0,-1.5);
          glVertex3f(-1.5, 0, 1.5);
          glVertex3f( 1.5, 0, 1.5);
          glVertex3f( 1.5, 0,-1.5);
        glEnd;
        glDepthFunc(GL_LESS);
        glEnable(GL_LIGHTING);
      end;
    end;
    glPopMatrix;    
  end;
end;
           
procedure TCityBlock.Render(Selection: boolean);
begin
  if not Selection then
    glCallList(FDisplayList)
  else
    RenderGeometry(Selection);
end;

procedure TCityBlock.RenderFocus;
  procedure c(a: single);
  begin
    glColor4f(0, 1, 0, a);
  end;

  procedure draw(min, max, h: Single);
  begin
    glBegin(GL_QUADS);
      c(max);
      glVertex3f( 6, 0,-6);
      glVertex3f(-6, 0,-6);
      c(min);
      glVertex3f(-6, h,-6);
      glVertex3f( 6, h,-6);

      c(max);
      glVertex3f(-6, 0,-6);
      glVertex3f(-6, 0, 6);
      c(min);
      glVertex3f(-6, h, 6);
      glVertex3f(-6, h,-6);

      c(max);
      glVertex3f(-6, 0, 6);
      glVertex3f( 6, 0, 6);
      c(min);
      glVertex3f( 6, h, 6);
      glVertex3f(-6, h, 6);

      c(max);
      glVertex3f( 6, 0, 6);
      glVertex3f( 6, 0,-6);
      c(min);
      glVertex3f( 6, h,-6);
      glVertex3f( 6, h, 6);
    glEnd;
  end;
begin
  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LINE_BIT);
  glEnable(GL_BLEND);
  glEnable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glFrontFace(GL_CW);
  draw(0.0, 0.5, 15);
  glFrontFace(GL_CCW);
  draw(0.0, 0.5, 15);
  glPopAttrib;
end;

procedure TCityBlock.RenderFloor;
begin
  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LINE_BIT);
  glBegin(GL_QUADS);
    glColor4f(0, 0, 0, 1);
    glVertex3f( 6, -0.1,-6);
    glVertex3f(-6, -0.1,-6);
    glVertex3f(-6, -0.1, 6);
    glVertex3f( 6, -0.1, 6);
  glEnd;
  glDepthFunc(GL_LEQUAL);
  glBegin(GL_LINE_LOOP);
    //SetGLMaterial(ColorToRGBA(0.9, 0.9, 0.9));
    glLineWidth(2);
    glColor4f(1, 1, 1, 1);
    glVertex3f( 6, 0,-6);
    glVertex3f(-6, 0,-6);
    glVertex3f(-6, 0, 6);
    glVertex3f( 6, 0, 6);
  glEnd;
  glDepthFunc(GL_LESS);
  glPopAttrib;
end;

procedure TCityBlock.UpdateDisplayList;
begin
  glNewList(FDisplayList, GL_COMPILE);
    RenderGeometry(false);
  glEndList;
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
  RenderShape(aHeight, false);

  glDisable(GL_LIGHTING);
  SetGLColor(FColor);
  //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2);
  RenderShape(aHeight, true);
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

procedure TBuilding.RenderShape(Height: Single; Outline: Boolean = false);
var
  w: Single;
begin
  glDepthFunc(GL_LEQUAL);
  w := 1.5;
  if (self is TBSpecial) then
    w := 5.5;
  if not Outline then
    glBegin(GL_QUADS)
  else
    glBegin(GL_LINE_LOOP);
    // Front Face
  glNormal3f( 0.0, 0.0, 1.0);                  // Normal Pointing Towards Viewer
  glTexCoord2f(0.0, 0.0); glVertex3f(-w, 0.0,  w);  // Point 1 (Front)
  glTexCoord2f(1.0, 0.0); glVertex3f( w, 0.0,  w);  // Point 2 (Front)
  glTexCoord2f(1.0, 1.0); glVertex3f( w,  Height,  w);  // Point 3 (Front)
  glTexCoord2f(0.0, 1.0); glVertex3f(-w,  Height,  w);  // Point 4 (Front)
  if Outline then begin
    glEnd;
    glBegin(GL_LINE_LOOP);
  end;
  // Back Face
  glNormal3f( 0.0, 0.0, -1.0);                  // Normal Pointing Away From Viewer
  glTexCoord2f(1.0, 0.0); glVertex3f(-w, 0.0, -w);  // Point 1 (Back)
  glTexCoord2f(1.0, 1.0); glVertex3f(-w,  Height, -w);  // Point 2 (Back)
  glTexCoord2f(0.0, 1.0); glVertex3f( w,  Height, -w);  // Point 3 (Back)
  glTexCoord2f(0.0, 0.0); glVertex3f( w, 0.0, -w);  // Point 4 (Back)
  if Outline then begin
    glEnd;
    glBegin(GL_LINE_LOOP);
  end;
  // Top Face
  glNormal3f( 0.0, 1.0, 0.0);                  // Normal Pointing Up
  glTexCoord2f(0.0, 1.0); glVertex3f(-w,  Height, -w);  // Point 1 (Top)
  glTexCoord2f(0.0, 0.0); glVertex3f(-w,  Height,  w);  // Point 2 (Top)
  glTexCoord2f(1.0, 0.0); glVertex3f( w,  Height,  w);  // Point 3 (Top)
  glTexCoord2f(1.0, 1.0); glVertex3f( w,  Height, -w);  // Point 4 (Top)
  if not FSelectMode then begin
    glEnd;

    Texture.Bind(true);
    glPushAttrib(GL_LIGHTING_BIT or GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glEnable(GL_BLEND);
    glColor3f(1,1,1);

    if not Outline then
      glBegin(GL_QUADS)
    else
      glBegin(GL_LINE_LOOP);
      glNormal3f( 0.0, 1.0, 0.0);                  // Normal Pointing Up
      glTexCoord2f(0.0, 0.0); glVertex3f(-w,  Height, -w);  // Point 1 (Top)
      glTexCoord2f(0.0, 1.0); glVertex3f(-w,  Height,  w);  // Point 2 (Top)
      glTexCoord2f(1.0, 1.0); glVertex3f( w,  Height,  w);  // Point 3 (Top)
      glTexCoord2f(1.0, 0.0); glVertex3f( w,  Height, -w);  // Point 4 (Top)
    glEnd;

    Texture.Unbind(true);
    SetGLColor(FColor);    
    glPopAttrib;

    if not Outline then
      glBegin(GL_QUADS)
    else
      glBegin(GL_LINE_LOOP);
  end;


  // Right face
  glNormal3f( 1.0, 0.0, 0.0);                  // Normal Pointing Right
  glTexCoord2f(1.0, 0.0); glVertex3f( w, 0.0, -w);  // Point 1 (Right)
  glTexCoord2f(1.0, 1.0); glVertex3f( w, Height, -w);  // Point 2 (Right)
  glTexCoord2f(0.0, 1.0); glVertex3f( w, Height,  w);  // Point 3 (Right)
  glTexCoord2f(0.0, 0.0); glVertex3f( w, 0.0,  w);  // Point 4 (Right)
  if Outline then begin
    glEnd;
    glBegin(GL_LINE_LOOP);
  end;  
  // Left Face
  glNormal3f(-1.0, 0.0, 0.0);                  // Normal Pointing Left
  glTexCoord2f(0.0, 0.0); glVertex3f(-w, 0.0, -w);  // Point 1 (Left)
  glTexCoord2f(1.0, 0.0); glVertex3f(-w, 0.0,  w);  // Point 2 (Left)
  glTexCoord2f(1.0, 1.0); glVertex3f(-w, Height,  w);  // Point 3 (Left)
  glTexCoord2f(0.0, 1.0); glVertex3f(-w, Height, -w);  // Point 4 (Left)
  glEnd;
  glDepthFunc(GL_LESS);
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

class function TBuilding.DisplayName: string;
begin
  Result:= ClassName;
end;

class function TBuilding.Price: Integer;
begin
  Result:= 1000;
end;

class function TBuilding.MinEducation: Integer;
begin
  Result:= 0;
end;

end.

