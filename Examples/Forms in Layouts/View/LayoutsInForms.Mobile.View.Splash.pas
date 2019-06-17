unit LayoutsInForms.Mobile.View.Splash;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Layouts;

type
  TFrmSplash = class(TForm)
    imgLogo: TImage;
    tmrCreateForms: TTimer;
    lytBackground: TLayout;
    aniLoading: TAniIndicator;
    procedure tmrCreateFormsTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgLogoPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSplash: TFrmSplash;

implementation

uses
LayoutsInForms.Mobile.View.FormMain
;

{$R *.fmx}

procedure TFrmSplash.FormCreate(Sender: TObject);
begin
  aniLoading.Enabled := True;
end;

procedure TFrmSplash.FormDestroy(Sender: TObject);
begin
  aniLoading.Enabled := False;
end;

procedure TFrmSplash.FormResize(Sender: TObject);
begin
  imgLogo.Height := Trunc(imgLogo.Width * 0.7);
end;

procedure TFrmSplash.imgLogoPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if not(tmrCreateForms.Enabled) then
    tmrCreateForms.Enabled := True;
end;

procedure TFrmSplash.tmrCreateFormsTimer(Sender: TObject);
var
  LFrm: TForm;
begin
  tmrCreateForms.Enabled := False;

  //Application.CreateForm(TLiberstenDataSet, LiberstenDataSet);

  LFrm := TFrmMain.Create(Application);
  LFrm.Show;
  Application.MainForm := LFrm;

  Close;
end;

end.
