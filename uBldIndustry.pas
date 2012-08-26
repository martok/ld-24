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
    class function Texture: TglBitmap2D; override;
    constructor Create; override;
  end;

  TBFactory = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SIndustryValue: Integer; override;
    function SPollution: Integer; override;
    class function Texture: TglBitmap2D; override;
    constructor Create; override;
    function SRange: Integer; override;
    function SEffectLoss: Single; override;
  end;

implementation

{ TBSmallIndustry }

constructor TBSmallIndustry.Create;
begin
  inherited;
  FColor:= ColorToRGBA(1,0.5,0.5);
end;

procedure TBSmallIndustry.RenderShape(Height: Single);
begin
  inherited RenderShape(0.5);
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
  FColor:= ColorToRGBA(1,0.5,0.5);
end;

procedure TBFactory.RenderShape(Height: Single);
begin
  inherited RenderShape(1);
end;

function TBFactory.SIndustryValue: Integer;
begin
  Result:= 5;
end;

function TBFactory.SPollution: Integer;
begin
  Result:= 3;
end;

function TBFactory.SEffectLoss: Single;
begin
  Result:= 0.3;
end;

function TBFactory.SRange: Integer;
begin          
  Result:= 2;
end;

class function TBFactory.Texture: TglBitmap2D;
begin
  Result:= Textures.BFactory;
end;

end.
