unit uCity;

interface

uses
  dglOpenGL, uCityBlock, Geometry, GLHelper, FastGL;

type
  TCityBlocks = array of array of TCityBlock;
  TStreets = array of packed record
    width: Byte;
    startPos, endPos: packed record
      x, y: Single;
    end;
  end;

  { TCity }

  TCity = class
  private
    FCityBlocks: TCityBlocks;
    FStreets: TStreets;
    FBlockDist: Single;
    function GetBlock(X, Y: integer): TCityBlock;
    procedure ClearBlocks;
    procedure FillBuildingEffect(Bdg: TBuilding; StartX, StartY: integer);
    procedure UpdateStats;
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

uses SysUtils, uConfigFile, Classes, Contnrs, Types;

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

  glTranslatef(-7.5, 0, -7.5);
  //glColor4f(0.7, 0.7, 0.7, 1);
  SetGLMaterial(ColorToRGBA(0.7, 0.7, 0.7));
  glBegin(GL_QUADS);
    glNormal3f(0, 1, 0);
    for i := 0 to High(FStreets) do with FStreets[i] do begin
      if (Sqr(startPos.x-endPos.x) / Sqr(startPos.y-endPos.y)) > 1 then begin
        glVertex3f(startPos.x*FBlockDist-width, 0, startPos.y*FBlockDist+width);
        glVertex3f(startPos.x*FBlockDist-width, 0, startPos.y*FBlockDist-width);
        glVertex3f(endPos.x*FBlockDist+width, 0, endPos.y*FBlockDist-width);
        glVertex3f(endPos.x*FBlockDist+width, 0, endPos.y*FBlockDist+width);
      end else begin
        glVertex3f(startPos.x*FBlockDist+width, 0, startPos.y*FBlockDist-width);
        glVertex3f(startPos.x*FBlockDist-width, 0, startPos.y*FBlockDist-width);
        glVertex3f(endPos.x*FBlockDist-width, 0, endPos.y*FBlockDist+width);
        glVertex3f(endPos.x*FBlockDist+width, 0, endPos.y*FBlockDist+width);
      end;
    end;
  glEnd;

  glPopMatrix;
end;

procedure TCity.FillBuildingEffect(Bdg: TBuilding; StartX,StartY: integer);
var
  visited: array of array of boolean;
  visit: TQueue;

  procedure PushPt(x,y: integer);
  var p: PPoint;
  begin
    if (x<0) or (y<0) or (x>high(visited)) or (y>high(visited[x])) or
      visited[x,y] then exit;
    visited[x, y]:= true;
    New(P);
    p^:= Point(x,y);
    visit.Push(p);
  end;

  function PopPt: TPoint;
  var p: PPoint;
  begin
    p:= visit.Pop;
    Result:= p^;
    Dispose(p);
  end;

  procedure FloodFill;
  var
    cb: TCityBlock;
    dist: integer;
    f: single;
    p: TPoint;
    procedure Affect;
    begin
      cb:= FCityBlocks[p.x,p.y];
      if Assigned(cb) then begin
        f:= 1 - Dist * Bdg.SEffectLoss;
        if f < 0 then f:= 0;           
        if f > 1 then f:= 1;
        cb.Industry:= cb.Industry + Bdg.SIndustryValue * f;
        cb.Pollution:= cb.Pollution + Bdg.SPollution * f;
        cb.Education:= cb.Education + Bdg.SEducation * f;
        cb.Luxury:= cb.Luxury + Bdg.SLuxury * f;
        cb.Space:= cb.Space + Bdg.SLivingSpace * f;
      end;
    end;
  begin
    PushPt(StartX, StartY);

    while visit.AtLeast(1) do begin
      p:= PopPt;
      dist:= abs(p.X-StartX) + abs(p.Y-StartY);
      if dist <= Bdg.SRange then begin
        Affect;
        if dist < Bdg.SRange then begin
          PushPt(p.X-1,p.Y);
          PushPt(p.X+1,p.Y);
          PushPt(p.X,p.Y+1);
          PushPt(p.X,p.Y-1);
        end;
      end;
    end;
  end;

var
  x: integer;
begin
  SetLength(visited, length(FCityBlocks), length(FCityBlocks[0]));
  for x:= 0 to High(visited) do
    FillChar(visited[x,0],sizeof(visited[x])*sizeof(boolean), 0);
  visit:=TQueue.Create;
  try
    FloodFill;
  finally
    visit.Free;
  end;
end;

procedure TCity.UpdateStats;
var
  x, y, b: integer;
  cb: TCityBlock;  
begin
  for x:= 0 to high(FCityBlocks) do
    for y:= 0 to high(FCityBlocks[x]) do begin
      cb:= FCityBlocks[x,y];
      if Assigned(cb) then begin
        cb.Industry:= 0;
        cb.Pollution:= 0;
        cb.Education:= 0;
        cb.Luxury:= 0;
        cb.Space:= 0;
      end;
    end;

  for x:= 0 to high(FCityBlocks) do
    for y:= 0 to high(FCityBlocks[x]) do begin
      cb:= FCityBlocks[x,y];
      if Assigned(cb) then begin
        for b:= 0 to 8 do
          if Assigned(cb.Building[b]) then begin
            FillBuildingEffect(cb.Building[b], x,y);
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
  UpdateStats;
end;

procedure TCity.Evolve;
var
  x, y: integer;
  cb: TCityBlock;
begin
  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      cb:= FCityBlocks[x, y];
      if Assigned(cb) then begin

        //TODO
        if cb.People > cb.Space * 0.9 then
          cb.GrowthRate:= cb.GrowthRate - (cb.People - cb.Space * 0.95)
        else
          cb.GrowthRate:= cb.GrowthRate + cb.People / cb.Space;
        cb.GrowthRate:= cb.GrowthRate + 0.5 * (1 + cb.Luxury);
        cb.GrowthRate:= cb.GrowthRate + 0.5 * cb.Education;
        cb.GrowthRate:= cb.GrowthRate + 0.2 * cb.Industry;
        cb.GrowthRate:= cb.GrowthRate - 0.1 * cb.Pollution;

        cb.People:= cb.People + cb.GrowthRate;
        if cb.People<0 then
          cb.People:= 0;
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
  UpdateStats;
end;

end.

