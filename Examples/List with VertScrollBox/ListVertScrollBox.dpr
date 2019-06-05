program ListVertScrollBox;

uses
  System.StartUpCopy,
  FMX.Forms,
  RadStudioHelpers.Example.ListVertScrollBox.FrmMain in 'RadStudioHelpers.Example.ListVertScrollBox.FrmMain.pas' {FrmMain},
  Helper.ItemListDefault in '..\..\src\Helper.ItemListDefault.pas',
  Helper.ItemListSimplePath in '..\..\src\Helper.ItemListSimplePath.pas',
  Helper.Loading in '..\..\src\Helper.Loading.pas',
  Helper.Thread in '..\..\src\Helper.Thread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
