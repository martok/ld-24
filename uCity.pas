unit uCity;

interface

uses
  dglOpenGL, uCityBlock;

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
  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      glPushMatrix;
      glTranslatef(x * 3, 0, y * 3);
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

end.

