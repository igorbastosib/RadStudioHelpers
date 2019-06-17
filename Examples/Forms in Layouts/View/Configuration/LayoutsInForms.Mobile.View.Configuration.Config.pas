unit LayoutsInForms.Mobile.View.Configuration.Config;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  LayoutsInForms.Mobile.View.FormModel, FMX.Objects, FMX.Controls.Presentation,
  FMX.TabControl, FMX.Layouts;

type
  TFrmConfig = class(TFrmModel)
    procedure btnMainTopRightClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmConfig: TFrmConfig;

implementation

uses
Helper.Lib.SvgPath
;

{$R *.fmx}

procedure TFrmConfig.btnMainTopRightClick(Sender: TObject);
begin
  inherited;
  case btnMainTopRight.Tag of
    0:
    begin
      pthMainTopRight.Data.Data := cWindowClose;
      btnMainTopRight.Tag := 1;
    end;
    1:
    begin
      pthMainTopRight.Data.Data := cSquareEditOutline;
      btnMainTopRight.Tag := 0;
    end;
  end;
end;

end.
