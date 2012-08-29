program ld24;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uViewFrame in 'uViewFrame.pas' {ViewFrame},
  uBldIndustry in 'uBldIndustry.pas',
  uFonts in 'uFonts.pas',
  uGlobals in 'uGlobals.pas',
  uGUIBlock in 'uGUIBlock.pas',
  dglOpenGL in 'dglOpenGL.pas',
  FastGL in 'FastGL.pas',
  GLHelper in 'GLHelper.pas',
  uBldEducation in 'uBldEducation.pas',
  uBldHouse in 'uBldHouse.pas',
  uCity in 'uCity.pas',
  uCityBlock in 'uCityBlock.pas',
  glBitmap in 'glBitmap.pas',
  uBldLuxury in 'uBldLuxury.pas',
  uBldSpecial in 'uBldSpecial.pas',
  uGUIMainMenu in 'uGUIMainMenu.pas';

{$R *.res}

begin
//  fastmm4.ReportMemoryLeaksOnShutdown:= false;
  Application.Initialize;
  Application.CreateForm(TViewFrame, ViewFrame);
  Application.Run;
end.
