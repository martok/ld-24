program ld24;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  uViewFrame in 'uViewFrame.pas' {ViewFrame},
  uCity in 'uCity.pas',
  uCityBlock in 'uCityBlock.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewFrame, ViewFrame);
  Application.Run;
end.
