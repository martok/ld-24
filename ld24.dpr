program ld24;

uses
  FastMM4,
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
  uBldEducation in 'uBldEducation.pas',
  uBldHouse in 'uBldHouse.pas',
  uCity in 'uCity.pas',
  uCityBlock in 'uCityBlock.pas',
  glBitmap in 'glBitmap.pas',
  uBldLuxury in 'uBldLuxury.pas',
  uBldSpecial in 'uBldSpecial.pas';

{$R *.res}
{$SetPEFlags $20}

begin
  Application.Initialize;
  Application.CreateForm(TViewFrame, ViewFrame);
  Application.Run;
end.
