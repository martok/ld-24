unit uBldLuxury;

interface

uses
  dglOpenGL, uCityBlock, GLHelper, glBitmap, uGlobals;

type
  TBPark = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLuxury: Integer; override;
    function SPollution: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
  end;

  TBCinema = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLuxury: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    function SRange: Integer; override;
    function SEffectLoss: Single; override;
  end;

  TBPool = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLuxury: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
  end;

  TBShopping = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLuxury: Integer; override;
    function SPollution: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
  end;

  TBTheater = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLuxury: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    function SRange: Integer; override;
    function SEffectLoss: Single; override;
  end;

  TBCasino = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLuxury: Integer; override;
    function SPollution: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    function SRange: Integer; override;
    function SEffectLoss: Single; override;
  end;

implementation


{ TBPark }

constructor TBPark.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5,1,0);
end;

procedure TBPark.RenderShape(Height: Single);
begin
  inherited RenderShape(0.5);
end;

class function TBPark.DisplayName: String;
begin
  Result:= 'Park';
end;

class function TBPark.Price: Integer;
begin
  Result:= 500;
end;

function TBPark.SLuxury: Integer;
begin
  Result:= 1;
end;

function TBPark.SPollution: Integer;
begin
  Result:= -1;
end;

class function TBPark.Texture: TglBitmap2D;
begin
  Result:= Textures.BPark;
end;

{ TBCinema }

constructor TBCinema.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5,1,0);
end;

procedure TBCinema.RenderShape(Height: Single);
begin
  inherited RenderShape(1.5);
end;

class function TBCinema.DisplayName: String;
begin
  Result:= 'Cinema';
end;

class function TBCinema.Price: Integer;
begin
  Result:= 1300;
end;

function TBCinema.SLuxury: Integer;
begin
  Result:= 2;
end;

class function TBCinema.Texture: TglBitmap2D;
begin
  Result:= Textures.BCinema;
end;

function TBCinema.SEffectLoss: Single;
begin
  Result:= 0.3;
end;

function TBCinema.SRange: Integer;
begin
  Result:= 3;
end;

{ TBPool }

constructor TBPool.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5,1,0);
end;

procedure TBPool.RenderShape(Height: Single);
begin
  inherited RenderShape(1);
end;

class function TBPool.DisplayName: String;
begin
  Result:= 'Pool';
end;

class function TBPool.Price: Integer;
begin
  Result:= 1500;
end;

function TBPool.SLuxury: Integer;
begin
  Result:= 3;
end;

class function TBPool.Texture: TglBitmap2D;
begin
  Result:= Textures.BPool;
end;

{ TBShopping }

constructor TBShopping.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5,1,0);
end;

procedure TBShopping.RenderShape(Height: Single);
begin
  inherited RenderShape(2);
end;

class function TBShopping.DisplayName: String;
begin
  Result:= 'Shopping Mall';
end;

class function TBShopping.Price: Integer;
begin
  Result:= 1700;
end;

function TBShopping.SLuxury: Integer;
begin
  Result:= 4;
end;

function TBShopping.SPollution: Integer;
begin
  Result:= 1;
end;

class function TBShopping.Texture: TglBitmap2D;
begin
  Result:= Textures.BShopping;
end;

{ TBTheater }

constructor TBTheater.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5,1,0);
end;

procedure TBTheater.RenderShape(Height: Single);
begin
  inherited RenderShape(2);
end;

class function TBTheater.DisplayName: String;
begin
  Result:= 'Theater';
end;

class function TBTheater.Price: Integer;
begin
  Result:= 1200;
end;

function TBTheater.SLuxury: Integer;
begin
  Result:= 3;
end;

class function TBTheater.Texture: TglBitmap2D;
begin
  Result:= Textures.BTheater;
end;

function TBTheater.SEffectLoss: Single;
begin
  Result:= 0.2;
end;

function TBTheater.SRange: Integer;
begin
  Result:= 3;
end;

{ TBCasino }

constructor TBCasino.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5,1,0);
end;

procedure TBCasino.RenderShape(Height: Single);
begin
  inherited RenderShape(3);
end;

class function TBCasino.DisplayName: String;
begin
  Result:= 'Casino';
end;

class function TBCasino.Price: Integer;
begin
  Result:= 5000;
end;

function TBCasino.SLuxury: Integer;
begin
  Result:= 5;
end;

function TBCasino.SPollution: Integer;
begin
  Result:= -1;
end;

class function TBCasino.Texture: TglBitmap2D;
begin
  Result:= Textures.BPark;
end;

function TBCasino.SEffectLoss: Single;
begin
  Result:= 0.1;
end;

function TBCasino.SRange: Integer;
begin
  Result:= 5;
end;

end.
