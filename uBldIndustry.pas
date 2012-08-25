unit uBldIndustry;

interface

uses
  dglOpenGL, uCityBlock, GLHelper;

type
  TBSmallIndustry = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render; override;
  end;

  TBFactory = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render; override;
  end;

implementation

{ TBSmallIndustry }

function TBSmallIndustry.ClickHeight: Single;
begin
  Result:= 0.5;
end;

procedure TBSmallIndustry.Render;
begin
  inherited;
  RenderSimple(ColorToRGBA(1,0.5,0.5), 0.5);
end;


{ TBFactory }

function TBFactory.ClickHeight: Single;
begin
  Result:= 1;
end;

procedure TBFactory.Render;
begin
  inherited;
  RenderSimple(ColorToRGBA(1,0.5,0.5), 1);
end;

end.
