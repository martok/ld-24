unit uBldHouse;

interface

uses
  dglOpenGL, uCityBlock, GLHelper, glBitmap, uGlobals;

type
  TBHouse = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render; override;
    function SLivingSpace: Integer; override;
    function SHappiness: Integer; override;
    class function Texture: TglBitmap2D; override;
  end;

  TBAppartement = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render; override;
    function SLivingSpace: Integer; override;
    function SHappiness: Integer; override;
    class function Texture: TglBitmap2D; override;
  end;

implementation

{ TBHouse }

function TBHouse.ClickHeight: Single;
begin
  Result:= 0.5;
end;

procedure TBHouse.Render;
begin
  inherited;
  RenderSimple(ColorToRGBA(0.5,0.5,1), 0.5);
end;


function TBHouse.SHappiness: Integer;
begin
  Result:= 2;
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

function TBAppartement.ClickHeight: Single;
begin
  Result:= 1;
end;

procedure TBAppartement.Render;
begin
  inherited;
  RenderSimple(ColorToRGBA(0.5,0.5,1), 1);
end;

function TBAppartement.SHappiness: Integer;
begin
  Result:= 0;
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
