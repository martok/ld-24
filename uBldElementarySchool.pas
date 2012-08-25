unit uBldElementarySchool;

interface

uses
  dglOpenGL, uCityBlock;

type
  TBElementarySchool = class(TBuilding)
  protected
    function ClickHeight: Single; override;
  public
    procedure Render; override;
  end;

implementation

{ TBElementarySchool }

function TBElementarySchool.ClickHeight: Single;
begin
  Result:= 1;
end;

procedure TBElementarySchool.Render;
begin
  inherited;
  glBegin(GL_TRIANGLES);
  glColor3f(1, 0, 0); glVertex3f(0, 0, 0);
  glColor3f(0, 0, 1); glVertex3f(1, 0, 1);
  glColor3f(0, 1, 0); glVertex3f(0.5, 1, 0.5);

  glColor3f(1, 0, 0); glVertex3f(0, 0, 1);
  glColor3f(0, 0, 1); glVertex3f(1, 0, 0);
  glColor3f(0, 1, 0); glVertex3f(0.5, 1, 0.5);
  glEnd;
end;

end.
