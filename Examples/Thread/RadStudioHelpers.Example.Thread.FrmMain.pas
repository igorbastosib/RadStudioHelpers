unit RadStudioHelpers.Example.Thread.FrmMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects;

type
  TFrmMain = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    vtsList: TVertScrollBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Helper.Thread, Helper.Loading;

{$R *.fmx}

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  TThreadCustom.Start(
    procedure
    begin // OnShow
      TLoading.Show('Espere', 'Verdana', 'Black', 'White', 'Red');
      vtsList.BeginUpdate;
    end,
    procedure
    var
      i: Integer;
      LRct: TRectangle;
    begin // OnProcess
      for i := 0 to 500 do
      begin
        LRct := TRectangle.Create(vtsList);
        LRct.Align := TAlignLayout.Top;
        LRct.Margins.Bottom := 2;
        LRct.Margins.Top := 2;
        LRct.Name := LRct.ClassName + i.ToString;
        vtsList.AddObject(LRct);
        TLoading.ChangeMessage('Espere ' + i.ToString);
        TThread.Synchronize(nil,
          procedure()
          begin
            Button1.Text := i.ToString;
          end);
        Sleep(5);
      end;
    end,
    procedure
    begin // OnComplete
      vtsList.EndUpdate;
      TLoading.Hide;
    end,
    procedure(const AExceptionMessage: string)
    begin // OnError
      TLoading.Hide;
      ShowMessage(AExceptionMessage);
    end, False);
end;

end.
