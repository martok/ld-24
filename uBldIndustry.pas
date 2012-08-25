unit uBldIndustry;

interface

uses
  dglOpenGL, uCityBlock, GLHelper, glBitmap, uGlobals;

type
  TBSmallIndustry = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render(const aHeight: Single); override;
    function SIndustryValue: Integer; override;
    function SPollution: Integer; override;
    class function Texture: TglBitmap2D; override;
  end;

  TBFactory = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render(const aHeight: Single); override;
    function SIndustryValue: Integer; override;
    function SPollution: Integer; override;  
    class function Texture: TglBitmap2D; override;
  end;

implementation

{ TBSmallIndustry }

function TBSmallIndustry.ClickHeight: Single;
begin
  Result:= 0.5;
end;

procedure TBSmallIndustry.Render(const aHeight: Single);
begin
  inherited;
  RenderSimple(ColorToRGBA(1,0.5,0.5), 0.5);
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

function TBFactory.ClickHeight: Single;
begin
  Result:= 1;
end;

procedure TBFactory.Render(const aHeight: Single);
begin
  inherited;
  RenderSimple(ColorToRGBA(1,0.5,0.5), 1);
end;

function TBFactory.SIndustryValue: Integer;
begin
  Result:= 5;
end;

function TBFactory.SPollution: Integer;
begin
  Result:= 3;
end;

class function TBFactory.Texture: TglBitmap2D;
begin
  Result:= Textures.BFactory;
end;

end.
