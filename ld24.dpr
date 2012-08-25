program ld24;

uses
  Forms,
  uViewFrame in 'uViewFrame.pas' {ViewFrame},
  uBldElementarySchool in 'uBldElementarySchool.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewFrame, ViewFrame);
  Application.Run;
end.
