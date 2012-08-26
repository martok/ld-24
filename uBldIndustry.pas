unit uBldIndustry;

interface

uses
  dglOpenGL, uCityBlock, GLHelper, glBitmap, uGlobals;

type
  TBSmallIndustry = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SIndustryValue: Integer; override;
    function SPollution: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
  end;

  TBFactory = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SIndustryValue: Integer; override;
    function SPollution: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    function SEffectLoss: Single; override;
    function SRange: Integer; override;
  end;

  TBFactories = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SIndustryValue: Integer; override;
    function SPollution: Integer; override;
    constructor Create; override;
    class function Texture: TglBitmap2D; override;
    class function DisplayName: String; override;
    class function Price: Integer; override;
    function SEffectLoss: Single; override;
    function SRange: Integer; override;
  end;

implementation

{ TBSmallIndustry }

constructor TBSmallIndustry.Create;
begin
  inherited;
  FColor:= ColorToRGBA(1,0.3,0.0);
end;

procedure TBSmallIndustry.RenderShape(Height: Single);
begin
  inherited RenderShape(1);
end;

class function TBSmallIndustry.DisplayName: String;
begin
  Result:= 'Small Industry';
end;

class function TBSmallIndustry.Price: Integer;
begin
  Result:= 200;
end;

function TBSmallIndustry.SIndustryValue: Integer;
begin
  Result:= 2;
end;

function TBSmallIndustry.SPollution: Integer;
begin
  Result:= 1;
end;

class function TBSmallIndustry.Texture: TglBitmap2D;
begin
  Result:= Textures.BSmallIndustry;
end;

{ TBFactory }

constructor TBFactory.Create;
begin
  inherited;
  FColor:= ColorToRGBA(1,0.3,0.0);
end;

procedure TBFactory.RenderShape(Height: Single);
begin
  inherited RenderShape(2);
end;

class function TBFactory.DisplayName: String;
begin
  Result:= 'Factory';
end;

class function TBFactory.Price: Integer;
begin
  Result:= 400;
end;

function TBFactory.SIndustryValue: Integer;
begin
  Result:= 4;
end;

function TBFactory.SPollution: Integer;
begin
  Result:= 3;
end;

class function TBFactory.Texture: TglBitmap2D;
begin
  Result:= Textures.BFactory;
end;

function TBFactory.SEffectLoss: Single;
begin
  Result:= 0.3;
end;

function TBFactory.SRange: Integer;
begin
  Result:= 2;
end;

{ TBFactories }

constructor TBFactories.Create;
begin
  inherited;
  FColor:= ColorToRGBA(1,0.3,0.0);
end;

procedure TBFactories.RenderShape(Height: Single);
begin
  inherited RenderShape(4);
end;

class function TBFactories.DisplayName: String;
begin
  Result:= 'Factory Complex';
end;

class function TBFactories.Price: Integer;
begin
  Result:= 800;
end;

function TBFactories.SIndustryValue: Integer;
begin
  Result:= 9;
end;

function TBFactories.SPollution: Integer;
begin
  Result:= 5;
end;

class function TBFactories.Texture: TglBitmap2D;
begin
  Result:= Textures.BFactories;
end;

function TBFactories.SEffectLoss: Single;
begin
  Result:= 0.2;
end;

function TBFactories.SRange: Integer;
begin
  Result:= 4; 
end;

end.
