program ld24;

uses
  Forms,
  uViewFrame in 'uViewFrame.pas' {ViewFrame},
  uBldHouse in 'uBldHouse.pas',
  uFonts in 'uFonts.pas',
  uGlobals in 'uGlobals.pas',
  uGUIBlock in 'uGUIBlock.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewFrame, ViewFrame);
  Application.Run;
end.
