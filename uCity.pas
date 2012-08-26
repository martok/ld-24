unit uCity;

interface

uses
  dglOpenGL, uCityBlock, Geometry, GLHelper, FastGL, contnrs;

type
  TCityBlocks = array of array of TCityBlock;
  TStreets = array of packed record
    width: Byte;
    startPos, endPos: packed record
      x, y: Single;
    end;
  end;

  TPos2D = array[0..1] of Single;
  TCar = class(TObject)
  private
    fStartPos, fEndPos: TPos2D;
    fSpeed, fPos: Single;
  public
    property Position: Single read fPos;

    procedure Progress(const aDeltaTime: Single);
    procedure Render;

    constructor Create(const aStartX, aStartY, aEndX, aEndY, aSpeed: Single);
    destructor Destroy; override;
  end;

  { TCity }

  TCity = class
  private
    FCityBlocks: TCityBlocks;
    FStreets: TStreets;
    FBlockDist: Single;
    FCars: TObjectList;
    function GetBlock(X, Y: integer): TCityBlock;
    procedure ClearBlocks; 
  public
    constructor Create;
    destructor Destroy; override;
    property Block[X, Y: integer]: TCityBlock read GetBlock;
    procedure Evolve;
    procedure Render(Selection: boolean);
    procedure Progress(const aDeltaTime: Single);
    procedure CreateRandomCar;
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
  FCars := TObjectList.Create(true);
  SetLength(FCityBlocks, 10, 10);
  SetLength(FStreets, 0);
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
  FCars.Free;
  for x:= 0 to high(FCityBlocks) do
    for y:= 0 to high(FCityBlocks[x]) do
      FreeAndNil(FCityBlocks[x, y]);
  inherited;
end;

function TCity.GetBlock(X, Y: integer): TCityBlock;
begin
  Result:= FCityBlocks[X, Y];
end;

procedure TCity.ClearBlocks;
var
  x, y: Integer;
begin
  for x := 0 to High(FCityBlocks) do
    for y := 0 to High(FCityBlocks[x]) do
      if Assigned(FCityBlocks[x, y]) then
        FreeAndNil(FCityBlocks[x, y]);
  SetLength(FCityBlocks, 0, 0);
end;

procedure TCity.Render(Selection: boolean);
var
  x, y, i: integer;
begin
  glPushMatrix;

//Streets
  glPushMatrix;
  glTranslatef(-7.5, 0, -7.5);
  //glColor4f(0.7, 0.7, 0.7, 1);
  SetGLMaterial(ColorToRGBA(0.7, 0.7, 0.7));
  glBegin(GL_QUADS);
    glNormal3f(0, 1, 0);
    for i := 0 to High(FStreets) do with FStreets[i] do begin
      if (Sqr(startPos.x-endPos.x) / Sqr(startPos.y-endPos.y)) > 1 then begin
        glVertex3f(startPos.x*FBlockDist-1, 0, startPos.y*FBlockDist+width);
        glVertex3f(startPos.x*FBlockDist-1, 0, startPos.y*FBlockDist-width);
        glVertex3f(endPos.x*FBlockDist+1, 0, endPos.y*FBlockDist-width);
        glVertex3f(endPos.x*FBlockDist+1, 0, endPos.y*FBlockDist+width);
      end else begin
        glVertex3f(startPos.x*FBlockDist+width, 0, startPos.y*FBlockDist-1);
        glVertex3f(startPos.x*FBlockDist-width, 0, startPos.y*FBlockDist-1);
        glVertex3f(endPos.x*FBlockDist-width, 0, endPos.y*FBlockDist+1);
        glVertex3f(endPos.x*FBlockDist+width, 0, endPos.y*FBlockDist+1);
      end;
    end;
  glEnd;
  glPopMatrix;

//Cars
  glDepthFunc(GL_ALWAYS);
  glColor4f(1, 1, 1, 1);
  SetGLMaterial(ColorToRGBA(1, 1, 1));
  glDisable(GL_LIGHTING);
  for i := 0 to FCars.Count-1 do
    TCar(FCars[i]).Render;
  glDepthFunc(GL_LESS);

  if not Selection then begin
    glEnable(GL_LIGHTING);
    glShadeModel(GL_SMOOTH);
    SetGLMaterial(c_Yellow);
    glLight(VectorMake(2, 10, 5, 0),
      ColorToRGBA(0.1, 0.1, 0.1),
      ColorToRGBA(0.8, 0.8, 0.8),
      ColorToRGBA(0.0, 0.0, 0.0), 0, 0);
  end else begin
    glDisable(GL_LIGHTING);
  end;

  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      if Assigned(FCityBlocks[x, y]) then begin
        glPushMatrix;
        glTranslatef(x * FBlockDist, 0, y * FBlockDist);
        if Selection then begin
          glBegin(GL_QUADS);
          glColor3ub(x, y, 255);
          glVertex3f( 6, 0,-6);
          glVertex3f(-6, 0,-6);
          glVertex3f(-6, 0, 6);
          glVertex3f( 6, 0, 6);
          glEnd;
        end;

        FCityBlocks[x, y].Render(Selection);
        glPopMatrix;
      end;
    end;
  end;

  glPopMatrix;
end;

procedure TCity.Progress(const aDeltaTime: Single);
var
  i: Integer;
begin
  for i := FCars.Count-1 downto 0 do begin
    with TCar(FCars[i]) do begin
      Progress(aDeltaTime);
      if Position >= 1 then
        FCars.Delete(i);
    end;
  end;
end;

procedure TCity.CreateRandomCar;
var
  r: Integer;
  o, s: Single;
begin
  r := random(Length(fStreets));
  with fStreets[r] do begin
    s := 10 + random*3;
    if random(2) = 1 then
      o := 0.5 + random(width)
    else
      o := -0.5 - random(width);
    if (Sqr(startPos.x-endPos.x) / Sqr(startPos.y-endPos.y)) < 1 then begin
      if (startPos.y-endPos.y) > 0 then
        o := -o;
      if (o < 0) then begin
        FCars.Add(TCar.Create(
          endPos.x*FBlockDist-7.5-o,
          endPos.y*FBlockDist-7.5,
          startPos.x*FBlockDist-7.5-o,
          startPos.y*FBlockDist-7.5,
          s));
      end else begin
        FCars.Add(TCar.Create(
          startPos.x*FBlockDist-7.5-o,
          startPos.y*FBlockDist-7.5,
          endPos.x*FBlockDist-7.5-o,
          endPos.y*FBlockDist-7.5,
          s));
      end;
    end else begin
      if (startPos.x-endPos.x) > 0 then
        o := -o;
      if o < 0 then begin
        FCars.Add(TCar.Create(
          endPos.x*FBlockDist-7.5,
          endPos.y*FBlockDist-7.5+o,
          startPos.x*FBlockDist-7.5,
          startPos.y*FBlockDist-7.5+o,
          s));
      end else begin
        FCars.Add(TCar.Create(
          startPos.x*FBlockDist-7.5,
          startPos.y*FBlockDist-7.5+o,
          endPos.x*FBlockDist-7.5,
          endPos.y*FBlockDist-7.5+o,
          s));
      end;
    end;
  end;
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
        FBlockDist := GetValue('BlockDist', 15);
        ClearBlocks;
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

        c := GetValue('StreetCount', 0);
        SetLength(FStreets, c);
        with Section('Streets') do begin
          for i := 0 to c-1 do begin
            with Section(IntToStr(i)) do begin
              FStreets[i].startPos.x := GetValue('StartX', 0);
              FStreets[i].startPos.y := GetValue('StartY', 0);
              FStreets[i].endPos.x := GetValue('EndX', 0);
              FStreets[i].endPos.y := GetValue('EndY', 0);
              FStreets[i].width := GetValue('Width', 1);
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

{ TCar }

constructor TCar.Create(const aStartX, aStartY, aEndX, aEndY, aSpeed: Single);
begin
  inherited Create;

  fStartPos[0] := aStartX;
  fStartPos[1] := aStartY;

  fEndPos[0] := aEndX;
  fEndPos[1] := aEndY;

  fSpeed := aSpeed/Sqrt(Sqr(fStartPos[0]-fEndPos[0]) + Sqr(fStartPos[1]-fEndPos[1]));
  fPos   := 0;
end;

destructor TCar.Destroy;
begin
  inherited;
end;

procedure TCar.Progress(const aDeltaTime: Single);
begin
  fPos := fPos + fSpeed * aDeltaTime;
end;

procedure TCar.Render;
var
  x1, y1: Single;
const
  C = 0.4;
begin
  x1 := fStartPos[0] + (fEndPos[0] - fStartPos[0]) * fPos;
  y1 := fStartPos[1] + (fEndPos[1] - fStartPos[1]) * fPos;
  glPointSize(5);
  glBegin(GL_QUADS);
    glVertex3f(x1-C, 0, y1-C);
    glVertex3f(x1-C, 0, y1+C);
    glVertex3f(x1+C, 0, y1+C);
    glVertex3f(x1+C, 0, y1-C);
  glEnd;
end;

end.

