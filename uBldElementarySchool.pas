unit uBldElementarySchool;

interface

uses
  dglOpenGL, uCityBlock, GLHelper;

type
  TBElementarySchool = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render(const aHeight: Single); override;
  end;

implementation

{ TBElementarySchool }

function TBElementarySchool.ClickHeight: Single;
begin
  Result:= 1;
end;

procedure TBElementarySchool.Render(const aHeight: Single);
begin
  inherited;
  RenderSimple(c_Red, 2);
end;

end.
