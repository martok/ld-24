unit uBldHouse;

interface

uses
  dglOpenGL, uCityBlock, GLHelper, glBitmap, uGlobals;

type
  TBHouse = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLivingSpace: Integer; override;
    class function Texture: TglBitmap2D; override;
    constructor Create; override;
  end;

  TBAppartement = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    function SLivingSpace: Integer; override;
    class function Texture: TglBitmap2D; override;
    constructor Create; override;
  end;

implementation

{ TBHouse }

constructor TBHouse.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5, 0.5, 1);
end;

procedure TBHouse.RenderShape(Height: Single);
begin
  inherited RenderShape((0.9 + FRndHeight) * Height);
end;

function TBHouse.SLivingSpace: Integer;
begin
  Result:= 10;
end;

class function TBHouse.Texture: TglBitmap2D;
begin
  Result:= Textures.BHouse;
end;

{ TBAppartement }

constructor TBAppartement.Create;
begin
  inherited;
  FColor:= ColorToRGBA(0.5,0.5,1);
end;

procedure TBAppartement.RenderShape(Height: Single);
begin
  inherited RenderShape((1.0 + FRndHeight) * Height);
end;

function TBAppartement.SLivingSpace: Integer;
begin
  Result:= 50;
end;

class function TBAppartement.Texture: TglBitmap2D;
begin
  Result:= Textures.BHouse;
end;

end.
