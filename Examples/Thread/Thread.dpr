program Thread;

uses
  System.StartUpCopy,
  FMX.Forms,
  RadStudioHelpers.Example.Thread.FrmMain in 'RadStudioHelpers.Example.Thread.FrmMain.pas' {FrmMain},
  Helper.Thread in '..\..\src\Helper.Thread.pas',
  Helper.Loading in '..\..\src\Helper.Loading.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
