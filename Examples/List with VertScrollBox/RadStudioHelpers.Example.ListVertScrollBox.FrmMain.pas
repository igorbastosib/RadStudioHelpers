unit RadStudioHelpers.Example.ListVertScrollBox.FrmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFrmMain = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    vtsList: TVertScrollBox;
    lblIndex: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure ClearList(AVts: TVertScrollBox; AClassType: TComponentClass);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Helper.Thread, Helper.Loading, Helper.ItemListSimplePath;

{$R *.fmx}

procedure TFrmMain.Button1Click(Sender: TObject);
var
  i: Integer;
  LFil: TItemListaSimplePath;
begin // OnProcess
  try
    ClearList(vtsList, TItemListaSimplePath);
    vtsList.BeginUpdate;
    for i := 0 to 500 do
    begin
      LFil := TItemListaSimplePath.Create(vtsList);
      LFil.Align := TAlignLayout.Top;
      LFil.Name := LFil.ClassName + i.ToString;

      LFil.Text := i.ToString;
      LFil.Detail := LFil.ClassName;

      vtsList.AddObject(LFil);

      lblIndex.Text := i.ToString;
    end;
  finally
    vtsList.EndUpdate;
  end;
end;

procedure TFrmMain.Button2Click(Sender: TObject);
begin
  TThreadCustom.Start(
    procedure
    begin // OnShow
      ClearList(vtsList, TItemListaSimplePath);
      TLoading.Show('Espere', 'Verdana', 'Black', 'White', 'Red');
      vtsList.Visible := False;
      vtsList.BeginUpdate;
    end,
    procedure
    var
      i: Integer;
      LFil: TItemListaSimplePath;
    begin // OnProcess
      for i := 0 to 500 do
      begin
        LFil := TItemListaSimplePath.Create(vtsList);
        LFil.Align := TAlignLayout.Top;
        LFil.Name := LFil.ClassName + i.ToString;

        LFil.Text := i.ToString;
        LFil.Detail := LFil.ClassName;

        vtsList.AddObject(LFil);
        TLoading.ChangeMessage('Espere ' + i.ToString);

        TThread.Synchronize(nil,
          procedure()
          begin
            lblIndex.Text := i.ToString;
          end);
      end;
    end,
    procedure
    begin // OnComplete
      vtsList.EndUpdate;
      vtsList.Visible := True;
      TLoading.Hide;
    end,
    procedure(const AExceptionMessage: string)
    begin // OnError
      TLoading.Hide;
      ShowMessage(AExceptionMessage);
    end, False);
end;

procedure TFrmMain.ClearList(AVts: TVertScrollBox; AClassType: TComponentClass);
var
  i: Integer;
  AComponent: TComponent;
begin
  try
    AVts.BeginUpdate;
    for i := Pred(AVts.Content.ChildrenCount) downto 0 do
    begin
      if(AVts.Content.Children.Items[i] is AClassType)then
      begin
        AComponent := AVts.Content.Children.Items[i];
        AComponent.DisposeOf;
      end;
    end;
  finally
    AVts.EndUpdate;
  end;
end;

end.
