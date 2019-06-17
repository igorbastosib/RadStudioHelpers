unit LayoutsInForms.Mobile.View.FormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.MultiView, System.Actions, FMX.ActnList;

type
  TFrmMain = class(TForm)
    lytBackground: TLayout;
    tbcMain: TTabControl;
    tbiLogin: TTabItem;
    tbiMain: TTabItem;
    lytMain: TLayout;
    StyleBook1: TStyleBook;
    mtvMenu: TMultiView;
    rctBackMenu: TRectangle;
    lytMnuConfiguration: TLayout;
    lytPthMnuConfiguration: TLayout;
    pthMnuConfiguration: TPath;
    lytLblMnuConfiguration: TLayout;
    lblMnuConfiguration: TLabel;
    btnMnuConfiguration: TButton;
    Line1: TLine;
    lblAppVersion: TLabel;
    lytMnuAux: TLayout;
    lytPthMnuAux: TLayout;
    pthMnuAux: TPath;
    lytLblMnuAux: TLayout;
    lblMnuAux: TLabel;
    btnMnuAux: TButton;
    rctTopMenu: TRectangle;
    lblTopMenu: TLabel;
    actAcoes: TActionList;
    actChangeTab: TChangeTabAction;
    procedure FormCreate(Sender: TObject);
    procedure btnMnuConfigurationClick(Sender: TObject);
    procedure btnMnuAuxClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FFrmActive: TForm;

    procedure OpenForm(AClass: TComponentClass);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
Helper.Lib.Utils,

LayoutsInForms.Mobile.View.FormModel,
LayoutsInForms.Mobile.View.Configuration.Config,
LayoutsInForms.Mobile.View.Venda.Lista
;

{$R *.fmx}

procedure TFrmMain.btnMnuConfigurationClick(Sender: TObject);
begin
  OpenForm(TFrmConfig);
end;

procedure TFrmMain.btnMnuAuxClick(Sender: TObject);
begin
  OpenForm(TFrmVendaLista);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FFrmActive := nil;

  tbcMain.ActiveTab := tbiMain;
  tbcMain.TabPosition := TTabPosition.None;

  TUtils.ChangeTabAction := actChangeTab;

  OpenForm(TFrmVendaLista);
end;

procedure TFrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if (FFrmActive <> nil) and
      (Assigned(FFrmActive)) then
      if not(TFrmModel(FFrmActive).Back()) then
      begin
        ShowMessage('Ultima Aba');
      end;

    Key := 0;
  end;
end;

procedure TFrmMain.OpenForm(AClass: TComponentClass);
begin
  mtvMenu.HideMaster;
  TUtils.OpenForm(AClass, lytMain, FFrmActive, mtvMenu, 'btnMainTopLeft');
end;

end.
