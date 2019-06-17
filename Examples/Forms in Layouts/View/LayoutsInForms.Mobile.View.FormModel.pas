unit LayoutsInForms.Mobile.View.FormModel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TFrmModel = class(TForm)
    lytBackground: TLayout;
    tbcMain: TTabControl;
    tbiMain: TTabItem;
    tbiAux: TTabItem;
    lytMainTop: TLayout;
    rctMainTop: TRectangle;
    btnMainTopLeft: TButton;
    btnMainTopRight: TButton;
    lblMainTop: TLabel;
    pthMainTopRight: TPath;
    pthMainTopLeft: TPath;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function Back(): Boolean; virtual;
    { Public declarations }
  protected
    var
      FFrmAux: TForm;
    procedure BtnBackOnClick(Sender: TObject);
    { Protected declarations }
  end;

var
  FrmModel: TFrmModel;

implementation

{$R *.fmx}

function TFrmModel.Back: Boolean;
begin
  //
end;

procedure TFrmModel.BtnBackOnClick(Sender: TObject);
begin
  Back;
end;

procedure TFrmModel.FormCreate(Sender: TObject);
begin
  FFrmAux := nil;
  
  tbcMain.ActiveTab := tbiMain;
  tbcMain.TabPosition := TTabPosition.None;
end;

end.
