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
    procedure LoadFromFile(const aFilename: String);

    procedure CreateBuilding(Building: TBuildingClass; Where: TCityBlock);
  end;

implementation

uses SysUtils, uConfigFile, Classes;

{ TCity }

constructor TCity.Create;
var
  x, y: integer;
begin
  inherited Create;
  SetLength(FCityBlocks, 10, 10);

  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      FCityBlocks[x, y]:= TCityBlock.Create(x, y, btNormal);
    end;
  end;
end;

destructor TCity.Destroy;
var
  x, y: integer;
begin
  for x:= 0 to high(FCityBlocks) do
    for y:= 0 to high(FCityBlocks[x]) do
      FreeAndNil(FCityBlocks[x, y]);
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
  /*
  if not Selection then begin
    glBegin(GL_QUADS);
    SetGLMaterial(ColorToRGBA(0.5, 0.9, 0.5));
    glVertex3f(1000, -0.2, -1000);
    glVertex3f(-1000, -0.2, -1000);
    glVertex3f(-1000, -0.2, 1000);
    glVertex3f(1000, -0.2, 1000);
    glEnd;
  end;
  */
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
    glVertex3f(4 * length(FCityBlocks), -0.1, -1);
    glVertex3f(-1, -0.1, -1);
    glVertex3f(-1, -0.1, 4 * length(FCityBlocks[0]));
    glVertex3f(4 * length(FCityBlocks), -0.1, 4 * length(FCityBlocks[0]));
    glEnd;
  end;
  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      if Assigned(FCityBlocks[x, y]) then begin
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
  end;
  glPopMatrix;
end;

procedure TCity.CreateBuilding(Building: TBuildingClass; Where: TCityBlock);
var
  i: integer;
begin
  for i:= 0 to 8 do
    if Where.Building[i] = nil then begin
      Where.Building[i]:= Building.Create;
      break;
    end;
end;

procedure TCity.Evolve;
var
  x, y: integer;
  ind, living, happi, pollu, people: single;
  procedure AddUp(A, B: integer);
  var
    cb: TCityBlock;
    f: Single;
  begin
    if (A < 0) or (B < 0) or (A > high(FCityBlocks)) or (B > high(FCityBlocks[A])) then
      exit;
    cb:= FCityBlocks[a, b];
    if Assigned(cb) then begin
      f:= 1 / (sqrt(sqr(a - x) + sqr(b - y))+1);
      ind:= ind + f * cb.Industry;
      living:= living + f * cb.Space;
      happi:= happi + f * cb.Happiness;
      pollu:= pollu + f * cb.Pollution;
      people:= people + f * cb.People;
    end;
  end;
var
  cb: TCityBlock;
begin
  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      if Assigned(FCityBlocks[x, y]) then
        FCityBlocks[x, y].Update;
    end;
  end;

  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      cb:= FCityBlocks[x, y];
      if Assigned(cb) then begin
        ind:= 0;
        living:= 0;
        happi:= 0;
        pollu:= 0;
        people:= 0;
        AddUp(x, y);
        AddUp(x - 1, y);
        AddUp(x + 1, y);
        AddUp(x, y - 1);
        AddUp(x, y + 1);

        if people > living * 0.9 then
          cb.Happiness:= cb.Happiness - 1
        else if people < living * 0.3 then
          cb.Happiness:= cb.Happiness - 1
        else
          cb.Happiness:= cb.Happiness + 1;

        cb.Happiness:= cb.Happiness - trunc(pollu / 10);

        cb.People:= cb.People + cb.Happiness;
      end;
    end;
  end;
end;

procedure TCity.LoadFromFile(const aFilename: String);
var
  kcf: TkcfConfigFile;
  stream: TFileStream;
  w, h, x, y, c, i, j, t: Integer;
begin
  if not FileExists(aFilename) then
    raise Exception.Create('invalid filepath: '+aFilename);

  stream := TFileStream.Create(aFilename, fmOpenRead);
  try
    kcf := TkcfConfigFile.Create(stream);
    try
      with kcf.Section('Map') do begin
        w := GetValue('Width', 10);
        h := GetValue('Height', 10);
        SetLength(FCityBlocks, w, h);
        for i := 0 to High(FCityBlocks) do
          for j := 0 to High(FCityBlocks[i]) do
            FCityBlocks[i, j] := nil;
        c := GetValue('BlockCount', 0);
        with Section('Blocks') do begin
          for i := 0 to c-1 do begin
            with Section(IntToStr(i)) do begin
              x := GetValue('x', -1);
              y := GetValue('y', -1);
              t := GetValue('type', 0);
              if (x >= 0) and (x < w) and (y >= 0) and (y < h) then begin
                FCityBlocks[x, y] := TCityBlock.Create(x, y, TBlockType(t))
              end else
                raise Exception.Create(format('invalid value (x: %d, y: %d)', [x, y]));
            end;
          end;
        end;
      end;
    finally
      kcf.Free;
    end;
  finally
    stream.Free;
  end;
end;

end.

