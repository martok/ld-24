unit uBldElementarySchool;

interface

uses
  dglOpenGL, uCityBlock, GLHelper;

type
  TBElementarySchool = class(TBuilding)
  protected
    procedure RenderShape(Height: Single); override;
  public
    constructor Create; override;
  end;

implementation

{ TBElementarySchool }

constructor TBElementarySchool.Create;
begin
  inherited;
  FColor:= c_Red;
end;


procedure TBElementarySchool.RenderShape(Height: Single);
begin
  inherited RenderShape(2.0);
end;

end.
