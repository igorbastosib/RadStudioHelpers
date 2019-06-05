program Message;

uses
  System.StartUpCopy,
  FMX.Forms,
  RadStudioHelpers.Example.Message.FrmMain in 'RadStudioHelpers.Example.Message.FrmMain.pas' {Form2},
  Helper.Message in '..\..\src\Helper.Message.pas',
  Helper.CommonTypes in '..\..\src\Helper.CommonTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
