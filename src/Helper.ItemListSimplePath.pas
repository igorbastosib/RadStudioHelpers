unit Helper.ItemListSimplePath;

interface

uses
  System.Classes,
  System.SysUtils,

  Helper.ItemListDefault,

  FMX.StdCtrls,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics
;

type
  TItemListaSimplePath = class(IlTItemListDefault)
  private
    FPthIcon: FMX.Objects.TPath;
    FLblDetail: TLabel;
    FLblText: TLabel;
    FDetail: string;
    FText: string;
    procedure SetDetail(const Value: string);
    procedure SetText(const Value: string);
    
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Text: string read FText write SetText;
    property Detail: string read FDetail write SetDetail;

    procedure SetIconData(const Value: string);
    procedure SetIconMargins(const ABottom, ALeft, ARight, ATop: Integer);
  published
    { published declarations }
  end;

implementation

{ TItemListaSimplePath }

constructor TItemListaSimplePath.Create(AOwner: TComponent);
begin
  inherited;  
  FLblText := GetNewLabel('Verdana');
  FLblText.Align := TAlignLayout.Top;
  FLblText.Visible := False;
  FLblDetail := GetNewLabel('Verdana');
  FLblDetail.Visible := False;

  FPthIcon := FMX.Objects.TPath.Create(LytClient);
  FPthIcon.Parent := LytClient;
  FPthIcon.Align := TAlignLayout.MostRight;
  FPthIcon.HitTest := False;
  FPthIcon.Visible := False;
  FPthIcon.Stroke.Kind := TBrushKind.None;
  FPthIcon.Fill.Color := StrToAlphaColor('#FF8B8F8B');
end;

destructor TItemListaSimplePath.Destroy;
begin
  FreeAndNil(FPthIcon);
  FreeAndNil(FLblText);
  FreeAndNil(FLblDetail);
  inherited;
end;

procedure TItemListaSimplePath.SetDetail(const Value: string);
begin
  FDetail := Value;
  FLblDetail.Visible := True;
  FLblDetail.Text := Value;
end;

procedure TItemListaSimplePath.SetIconData(const Value: string);
begin
  FPthIcon.Visible := not(Value.Trim.IsEmpty);
  FPthIcon.Width := FPthIcon.Height;
  FPthIcon.Data.Data := Value;
end;

procedure TItemListaSimplePath.SetIconMargins(const ABottom, ALeft, ARight,
  ATop: Integer);
begin
  FPthIcon.Margins.Bottom := ABottom;
  FPthIcon.Margins.Left := ALeft;
  FPthIcon.Margins.Right := ARight;
  FPthIcon.Margins.Top := ATop;
end;

procedure TItemListaSimplePath.SetText(const Value: string);
begin
  FText := Value;
  FLblText.Visible := True;
  FLblText.Text := Value;
end;

end.
