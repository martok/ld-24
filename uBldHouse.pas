unit uBldHouse;

interface

uses
  dglOpenGL, uCityBlock, GLHelper;

type
  TBHouse = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render; override;
  end;

  TBAppartement = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render; override;
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

end.
