unit LayoutsInForms.Mobile.View.Venda.Lista;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  LayoutsInForms.Mobile.View.FormModel, FMX.Objects, FMX.Controls.Presentation,
  FMX.TabControl, FMX.Layouts;

type
  TFrmVendaLista = class(TFrmModel)
    procedure btnMainTopRightClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Back(): Boolean; override;
  end;

var
  FrmVendaLista: TFrmVendaLista;

implementation

uses
  Helper.Lib.Utils,
  Helper.Lib.SvgPath,

  LayoutsInForms.Mobile.View.Configuration.Config;

{$R *.fmx}

function TFrmVendaLista.Back: Boolean;
begin
  Result := True;
  if(tbcMain.ActiveTab = tbiAux)then
  begin
    if not(TFrmModel(FFrmAux).Back)then
    begin
      TUtils.ChangeTab(tbcMain, tbiMain);
      FreeAndNil(FFrmAux);
    end;
  end else
    Result := False;
end;

procedure TFrmVendaLista.btnMainTopRightClick(Sender: TObject);
begin
  inherited;
  TUtils.OpenForm(TFrmVendaLista, tbiAux, FFrmAux, nil, '');

  TFrmConfig(FFrmAux).pthMainTopLeft.Data.Data := cChevronLeft;
  TFrmConfig(FFrmAux).btnMainTopLeft.OnClick := BtnBackOnClick;

  TUtils.ChangeTab(tbcMain, tbiAux);
end;

end.
