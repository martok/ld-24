unit uCity;

{$MODE Delphi}

interface

uses
  dglOpenGL, uCityBlock, GLHelper, FastGL, contnrs, LCLIntf, LCLType, LMessages,
  uglShader, Forms, FileUtil, glBitmap, Types;

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

  TBuildResult = (brBuilt, brErrorUnknown, brErrorMoney, brErrorEdu);
  TCity = class
  private
    FSize: TPoint;
    FCityBlocks: TCityBlocks;
    FStreets: TStreets;
    FBlockDist: Single;
    FCars: TObjectList;
    FTotalEducation: Single;
    FTotalMoney: Single;
    FTotalPeople: Single;
    FHeightMapList: GLuint;
    FStreetsList: GLuint;
    FHeightMap: TglBitmap2D;
    FHeightMapHeight: Single;
    FActiveBlock: TPoint;
    FHasHeightMap: Boolean;
    FRound: Integer;
    function GetBlock(X, Y: integer): TCityBlock;
    function GetSize: TPoint;
    procedure ClearBlocks;
    procedure FillBuildingEffect(Bdg: TBuilding; StartX, StartY: integer);
    procedure UpdateStats;
    procedure CreateHeightMap;
    procedure CreateStreets;
    procedure ShaderLog(Sender: TObject; const Msg: String);
  public
    constructor Create;
    destructor Destroy; override;
    property Block[X, Y: integer]: TCityBlock read GetBlock;
    procedure Evolve;
    procedure Render(Selection: boolean);
    property ActiveBlock: TPoint read FActiveBlock write FActiveBlock;
    procedure Progress(const aDeltaTime: Single);
    procedure CreateRandomCar;
    procedure LoadFromFile(const aFilename: String);
    function PopulateStart: TPoint;

    function CreateBuilding(Building: TBuildingClass; Where: TCityBlock): TBuildResult; overload;
    function CreateBuilding(Building: TBuildingClass; Where: TCityBlock; Slot: Integer): TBuildResult; overload;
    procedure DestroyBuilding(Where: TCityBlock; index: integer);

    property TotalPeople: Single read FTotalPeople;
    property TotalMoney: Single read FTotalMoney;
    property TotalEducation: Single read FTotalEducation;
    property Round: Integer read FRound;
    property Size: TPoint read GetSize;
    property BlockDist: Single read FBlockDist;
  end;

implementation

uses
  SysUtils, uConfigFile, Classes, uBldSpecial, uBldHouse, Dialogs;

{ TCity }

constructor TCity.Create;
var
  x, y: integer;
begin
  inherited Create;
  FCars := TObjectList.Create(true);
  FSize := Point(10, 10);
  SetLength(FCityBlocks, 10, 10);
  SetLength(FStreets, 0);
  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      FCityBlocks[x, y]:= TCityBlock.Create(x, y, btNormal);
    end;
  end;
  FActiveBlock:=Point(-1,-1);
  FTotalPeople:= 0;
  FTotalMoney:= 10000000;        //TODO
  FTotalEducation:= 0;
  FRound := 0;

  FHeightMap := TglBitmap2D.Create;
  FHeightMap.SetWrap(GL_REPEAT, GL_REPEAT, GL_REPEAT);
  FHeightMap.SetFilter(GL_LINEAR, GL_LINEAR);
  FHasHeightMap := False;

  CreateHeightMap;
end;

destructor TCity.Destroy;
var
  x, y: integer;
begin
  FreeAndNil(FHeightMap);
  glDeleteLists(FHeightMapList, 1);
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

function TCity.GetSize: TPoint;
begin
  result := Point(System.Round(FSize.X * FBlockDist), System.Round(FSize.Y * FBlockDist));
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
//Terrain
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  if not Selection then begin
    glEnable(GL_CULL_FACE);
    glColor4f(0, 0, 0, 1);
    glPushMatrix;
    glTranslatef(0, -0.5, 0);
    glCallList(FHeightMapList);
    glTranslatef(-FBlockDist/2, -1, -FBlockDist/2);
    glColor4f(0, 0, 0.5, 1);
    glBegin(GL_QUADS);
      glVertex3f(                 0, 0,                  0);
      glVertex3f(                 0, 0, FSize.Y*FBlockDist);
      glVertex3f(FSize.X*FBlockDist, 0, FSize.Y*FBlockDist);
      glVertex3f(FSize.X*FBlockDist, 0,                  0);
    glEnd;
    glPopMatrix;
    glDisable(GL_CULL_FACE);
    glEnable(GL_LIGHTING);
    //glClear(GL_DEPTH_BUFFER_BIT);
  end;
  glPushMatrix;

  if not Selection then begin
    //Streets
    glCallList(FStreetsList);

    //Cars
    glDepthFunc(GL_ALWAYS);
    glColor4f(1, 1, 1, 1);
    SetGLMaterial(ColorToRGBA(1, 1, 1));
    glDisable(GL_LIGHTING);
    for i := 0 to FCars.Count-1 do
      TCar(FCars[i]).Render;
    glDepthFunc(GL_LESS);

    glEnable(GL_LIGHTING);
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
  if not Selection then begin
    if FActiveBlock.X >= 0 then begin
      x:= FActiveBlock.x;
      y:= FActiveBlock.y;
      if Assigned(FCityBlocks[x, y]) then begin
        glPushMatrix;
        glTranslatef(x * FBlockDist, 0, y * FBlockDist);
        FCityBlocks[x, y].RenderFocus;
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
  if Length(fStreets) = 0 then
    exit;
  r := random(Length(fStreets));
  with fStreets[r] do begin
    s := 3 + random*3;
    if random(2) = 1 then
      o := 0.5 + random(width)
    else
      o := -0.5 - random(width);
    if (startPos.y-endPos.y <> 0) and ((Sqr(startPos.x-endPos.x) / Sqr(startPos.y-endPos.y)) < 1) then begin
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
        cb.Luxury:= cb.Luxury + Bdg.SLuxury * f;
        if dist = 0 then begin
          cb.Education:= cb.Education + Bdg.SEducation * f;
          cb.Space:= cb.Space + Bdg.SLivingSpace * f;
        end;
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

function TCity.CreateBuilding(Building: TBuildingClass; Where: TCityBlock): TBuildResult;
var
  i: integer;
begin
  result := brErrorUnknown;
  for i:= 0 to 8 do
    if Where.Building[i] = nil then begin
      result := CreateBuilding(Building, Where, i);
      break;
    end;
end;

function TCity.CreateBuilding(Building: TBuildingClass; Where: TCityBlock; Slot: Integer): TBuildResult;

  function IsEmpty: Boolean;
  var
    i: Integer;
  begin
    result := false;
    for i := 0 to 8 do
      if Assigned(Where.Building[i]) then
        exit;
    result := true;
  end;

begin
  result := brErrorUnknown;
  if Assigned(Where.Building[Slot]) then
    raise Exception.Create('slot is not empty');
  if (Building.ClassParent = TBSpecial) then begin
    if not IsEmpty then
      exit;
    slot := 4;
    if TBSpecialClass(Building).NeededBlockType <> Where.BlockType then
      exit;
  end else begin
    if (Where.Building[4] is TBSpecial) then
      exit;
  end;
  if Building.MinEducation > FTotalEducation then begin
    Result:= brErrorEdu;
    exit;
  end;
  if Building.Price > FTotalMoney then begin
    Result:= brErrorMoney;
    exit;
  end;
  FTotalMoney:= FTotalMoney - Building.Price;
  Where.Building[Slot]:= Building.Create;
  Where.UpdateDisplayList;
  result := brBuilt;
  UpdateStats;
end;

procedure TCity.DestroyBuilding(Where: TCityBlock; index: integer);
begin
  if Where.Building[index] <> nil then begin
    FTotalMoney:= FTotalMoney + 0.6 * Where.Building[index].Price;
    Where.Building[index].Free;
    Where.Building[index]:= nil;
    Where.UpdateDisplayList;
  end;
  UpdateStats;
end;

procedure TCity.Evolve;
var
  x, y: integer;
  cb: TCityBlock;
  a,b,c: Single;
begin
  FTotalPeople:= 0;
  FTotalEducation:= 0;
  inc(FRound);
  for x:= 0 to high(FCityBlocks) do begin
    for y:= 0 to high(FCityBlocks[x]) do begin
      cb:= FCityBlocks[x, y];
      if Assigned(cb) then begin
        cb.GrowthRate := -1;
        if cb.Space > 0 then begin
          a:= cb.People/cb.Space;
          b:= a + 0.06;
          c:= 0.315693 + 1.1698*b + 0.377239*b*b - 1.74776*b*b*b;
          cb.GrowthRate:= c*2;
        end;
        cb.GrowthRate:= cb.GrowthRate + 0.5 * (0.3 + cb.Luxury);
        cb.GrowthRate:= cb.GrowthRate + 0.5 * cb.Education;
        cb.GrowthRate:= cb.GrowthRate + 0.2 * cb.Industry;
        cb.GrowthRate:= cb.GrowthRate - 0.4 * cb.Pollution;

        cb.People:= cb.People + cb.GrowthRate;
        if cb.People<0 then
          cb.People:= 0;

        FTotalPeople:= FTotalPeople + cb.People;
        FTotalMoney:= FTotalMoney + cb.Industry; 
        FTotalEducation:= FTotalEducation + cb.Education;
      end;
    end;
  end;
  for x:= 0 to high(FCityBlocks) do
    for y:= 0 to high(FCityBlocks[x]) do
      if Assigned(FCityBlocks[x, y]) then
        FCityBlocks[x, y].UpdateDisplayList;
end;

procedure TCity.LoadFromFile(const aFilename: String);
var
  kcf: TkcfConfigFile;
  stream: TFileStream;
  w, h, x, y, c, i, j, t: Integer;
  s: String;
begin
  if not FileExistsUTF8(aFilename) { *Converted from FileExists*  } then
    raise Exception.Create('invalid filepath: '+aFilename);

  stream := TFileStream.Create(aFilename, fmOpenRead);
  try
    kcf := TkcfConfigFile.Create(stream);
    try
      with kcf.Section('Map') do begin
        w := GetValue('Width', 10);
        h := GetValue('Height', 10);
        s := ExtractFilePath(Application.ExeName) + GetValue('Heightmap', '');
        FHasHeightMap := False;
        if FileExistsUTF8(s) { *Converted from FileExists*  } then begin
          FHeightMap.LoadFromFile(s);
          FHeightMap.GenTexture;
          FHasHeightMap := true;
        end;
        FHeightMapHeight:= GetValue('MapHeight', 30);
        FSize := Point(w, h);
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
      CreateHeightMap;
      CreateStreets;
    finally
      kcf.Free;
    end;
  finally
    stream.Free;
  end;
  UpdateStats;
end;

procedure TCity.CreateHeightMap;
var
  x, y: Integer;
  h: single;
const
  GRID = 8;

  function HeightAtTextCoord(s,t: single): Single;
  var pp: TglBitmapPixelPosition;
      pd: TglBitmapPixelData;
  begin
    pp.X:= System.Round(FHeightMap.Width * s);
    pp.Y:= System.Round(FHeightMap.Height * t);
    FHeightMap.GetPixel(pp, pd);
    Result:= -(0.5-pd.Red/255)*FHeightMapHeight;
  end;
begin
  if FHeightMapList <> 0 then
    glDeleteLists(FHeightMapList, 1);
  FHeightMap.GetDataFromTexture;

  FHeightMapList := glGenLists(1);
  glNewList(FHeightMapList, GL_COMPILE);
    glColor4f(0, 0, 0, 1);
    glPushMatrix;
    glTranslatef(-FBlockDist/2, 0, -FBlockDist/2);
    for y := 0 to FSize.Y*GRID-1 do begin
      glBegin(GL_QUAD_STRIP);
        for x := 0 to FSize.X*GRID do begin
          h:= HeightAtTextCoord((x+1)/(FSize.X*GRID+2), (y+1)/(FSize.Y*GRID+2));
          glVertex3f(x * FBlockDist/GRID, h,  y    * FBlockDist/GRID);
          h:= HeightAtTextCoord((x+1)/(FSize.X*GRID+2), (y+2)/(FSize.Y*GRID+2));
          glVertex3f(x * FBlockDist/GRID, h, (y+1) * FBlockDist/GRID);
        end;
      glEnd;
    end;
    glColor4f(0.2, 0.2, 0.2, 1);
    glDepthFunc(GL_LEQUAL);
    glLineWidth(2);
    glBegin(GL_LINES);
      for x := 0 to FSize.X*GRID do begin
        for y := 0 to FSize.Y*GRID do begin
          if (y < FSize.Y*GRID) then begin
            h:= HeightAtTextCoord((x+1)/(FSize.X*GRID+2), (y+1)/(FSize.Y*GRID+2));
            glVertex3f(x * FBlockDist/GRID, h,  y    * FBlockDist/GRID);
            h:= HeightAtTextCoord((x+1)/(FSize.X*GRID+2), (y+2)/(FSize.Y*GRID+2));
            glVertex3f(x * FBlockDist/GRID, h, (y+1) * FBlockDist/GRID);
          end;

          if (x < FSize.X*GRID) then begin
            h:= HeightAtTextCoord((x+1)/(FSize.X*GRID+2), (y+1)/(FSize.Y*GRID+2));
            glVertex3f( x    * FBlockDist/GRID, h, y * FBlockDist/GRID);
            h:= HeightAtTextCoord((x+2)/(FSize.X*GRID+2), (y+1)/(FSize.Y*GRID+2));
            glVertex3f((x+1) * FBlockDist/GRID, h, y * FBlockDist/GRID);
          end;
        end;
      end;
    glEnd;
    glLineWidth(1);
    glDepthFunc(GL_LESS);
    glPopMatrix;
  glEndList;
end;

procedure TCity.CreateStreets;
var
  i, o: Integer; 
begin
  if FStreetsList <> 0 then
    glDeleteLists(FStreetsList, 1);
  FStreetsList := glGenLists(1);
  glNewList(FStreetsList, GL_COMPILE);
    glPushMatrix;
    glTranslatef(-7.5, 0, -7.5);
    //glColor4f(0.7, 0.7, 0.7, 1);
    SetGLMaterial(ColorToRGBA(0.7, 0.7, 0.7));
    glBegin(GL_QUADS);
      glNormal3f(0, 1, 0);
      for i := 0 to High(FStreets) do with FStreets[i] do begin
        if (startPos.y-endPos.y = 0) or ((Sqr(startPos.x-endPos.x) / Sqr(startPos.y-endPos.y)) > 1) then begin
          if (startPos.x < endPos.x) then
            o := 1
          else
            o := -1;
          glVertex3f(startPos.x*FBlockDist-o, 0, startPos.y*FBlockDist+width);
          glVertex3f(startPos.x*FBlockDist-o, 0, startPos.y*FBlockDist-width);
          glVertex3f(endPos.x*FBlockDist+o, 0, endPos.y*FBlockDist-width);
          glVertex3f(endPos.x*FBlockDist+o, 0, endPos.y*FBlockDist+width);
        end else begin
          if (startPos.y < endPos.y) then
            o := 1
          else
            o := -1;
          glVertex3f(startPos.x*FBlockDist+width, 0, startPos.y*FBlockDist-o);
          glVertex3f(startPos.x*FBlockDist-width, 0, startPos.y*FBlockDist-o);
          glVertex3f(endPos.x*FBlockDist-width, 0, endPos.y*FBlockDist+o);
          glVertex3f(endPos.x*FBlockDist+width, 0, endPos.y*FBlockDist+o);
        end;
      end;
    glEnd;
    glPopMatrix;
  glEndList;
end;

procedure TCity.ShaderLog(Sender: TObject; const Msg: String);
begin
  ShowMessage(Msg);
end;

function TCity.PopulateStart: TPoint;
var
  x,y: integer;
begin
  repeat
    x:= random(length(FCityBlocks));
    y:= random(length(FCityBlocks[0]));
    if Assigned(FCityBlocks[x,y]) then begin
      FCityBlocks[x,y].Building[Random(9)]:= TBHouse.Create;
      FCityBlocks[x,y].UpdateDisplayList;
      Result:= Point(x,y);
      break;
    end;
  until False;
  UpdateStats;
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

