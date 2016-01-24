program GalaExamples;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Gala in '..\Gala.pas',
  GalaSignals in '..\GalaSignals.pas',
  GalaContainer in '..\GalaContainer.pas',
  GalaUtils in '..\GalaUtils.pas',
  ExamplesForm in 'ExamplesForm.pas' {FormMain},
  Example01 in 'Example01.pas' {Form01},
  Example02 in 'Example02.pas' {Form02},
  PMU in 'PMU.pas' {FormPMU},
  Example03 in 'Example03.pas' {Form03},
  Example04 in 'Example04.pas' {Form04},
  Example05 in 'Example05.pas' {Form05},
  Example06 in 'Example06.pas' {Form06},
  Phil5 in 'Phil5.pas' {FormPhil5},
  Example07 in 'Example07.pas' {Form07},
  Example08 in 'Example08.pas' {Form08},
  Example09 in 'Example09.pas' {Form09},
  Example10 in 'Example10.pas' {Form10},
  Example11 in 'Example11.pas' {Form11},
  Example12 in 'Example12.pas' {Form12},
  Example13 in 'Example13.pas' {Form13};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GalaExamples';
  Application.HelpFile := '';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
