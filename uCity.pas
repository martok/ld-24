unit uCity;

interface

uses
  dglOpenGL, uCityBlock, Geometry, GLHelper, FastGL;

type
  TCityBlocks = array of array of TCityBlock;

  { TCity }

  TCity = class
  private
    FCityBlocks: TCityBlocks;
    function GetBlock(X, Y: integer): TCityBlock;
  public
    constructor Create;
    destructor Destroy; override;
    property Block[X, Y: integer]: TCityBlock read GetBlock;
    procedure Evolve;
    procedure Render(Selection: boolean);

    procedure CreateBuilding(Building: TBuildingClass; Where: TCityBlock);
  end;

implementation

{ TCity }

constructor TCity.Create;
var
  x, y: integer;
begin
  inherited Create;
  SetLength(FCityBlocks, 10, 10);

  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      FCityBlocks[x, y]:= TCityBlock.Create(x,y);
    end;
  end;
end;

destructor TCity.Destroy;
begin

  inherited;
end;

function TCity.GetBlock(X, Y: integer): TCityBlock;
begin
  Result:= FCityBlocks[X, Y];
end;

procedure TCity.Render(Selection: boolean);
var
  x, y: integer;
begin
  glPushMatrix;
  if not Selection then begin
    glBegin(GL_QUADS);
    SetGLMaterial(ColorToRGBA(0.5, 0.9, 0.5));
    glVertex3f(1000, -0.2, -1000);
    glVertex3f(-1000, -0.2, -1000);
    glVertex3f(-1000, -0.2, 1000);
    glVertex3f(1000, -0.2, 1000);
    glEnd;
  end;


  if not Selection then begin
    glEnable(GL_LIGHTING);
    glShadeModel(GL_SMOOTH);
    SetGLMaterial(c_Yellow);
    glLight(VectorMake(-5, 5, 5),
            ColorToRGBA(0.2, 0.2, 0.2), ColorToRGBA(0.8, 0.8, 0.8),
            ColorToRGBA(1.0, 1.0, 1.0), 0, 0);
  end else begin
    glDisable(GL_LIGHTING);
  end;
  if not Selection then begin
    glBegin(GL_QUADS);
    SetGLMaterial(ColorToRGBA(0.3, 0.3, 0.3));
    glVertex3f(4*length(FCityBlocks), -0.1, -1);
    glVertex3f(-1, -0.1, -1);
    glVertex3f(-1, -0.1, 4*length(FCityBlocks[0]));
    glVertex3f(4*length(FCityBlocks), -0.1, 4*length(FCityBlocks[0]));
    glEnd;
  end;
  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      glPushMatrix;
      glTranslatef(x * 4, 0, y * 4);
      if Selection then begin
        glBegin(GL_QUADS);
        glColor3ub(x, y, 255);
        glVertex3f(0, 0, 0);
        glVertex3f(3, 0, 0);
        glVertex3f(3, 0, 3);
        glVertex3f(0, 0, 3);
        glEnd;
      end;

      FCityBlocks[x, y].Render(Selection);
      glPopMatrix;
    end;
  end;
  glPopMatrix;
end;

procedure TCity.Evolve;
begin
end;

procedure TCity.CreateBuilding(Building: TBuildingClass; Where: TCityBlock);
var
  i: integer;
begin
  for i:= 0 to 8 do
    if Where.Building[i]=nil then begin
      Where.Building[i]:= Building.Create;
      break;
    end;
end;

end.

