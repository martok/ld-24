program ld24;

uses
  Forms,
  uViewFrame in 'uViewFrame.pas' {ViewFrame},
  uBldIndustry in 'uBldIndustry.pas',
  uFonts in 'uFonts.pas',
  uGlobals in 'uGlobals.pas',
  uGUIBlock in 'uGUIBlock.pas',
  Camera in 'Camera.pas',
  dglOpenGL in 'dglOpenGL.pas',
  FastGL in 'FastGL.pas',
  Geometry in 'Geometry.pas',
  GLHelper in 'GLHelper.pas',
  uBldElementarySchool in 'uBldElementarySchool.pas',
  uBldHouse in 'uBldHouse.pas',
  uCity in 'uCity.pas',
  uCityBlock in 'uCityBlock.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewFrame, ViewFrame);
  Application.Run;
end.
