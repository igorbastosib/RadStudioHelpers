unit Helper.ItemListDefault;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UIConsts,
  System.UITypes,

  FMX.Layouts,
  FMX.Types,
  FMX.Objects,
  FMX.Graphics,
  FMX.StdCtrls;

type
  IlTItemListDefault = class(TLayout)
  private
    { private declarations }
    FOwner: TComponent;
    FRctBackground: TRectangle;
    FRctBase: TRectangle;
    FLneDivisor: TLine;
    FLytClient: TLayout;
    FIdentify: string;
    FListIndex: Integer;

    procedure SetIdentify(const Value: string);
    procedure SetListIndex(const Value: Integer);
  protected
    { protected declarations }
    function GetButtonWithPath(const AName: string;
      const APthData: string): TButton;
    function GetNewLabel(const AFontName: string): TLabel;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Identify: string read FIdentify write SetIdentify;
    property ListIndex: Integer read FListIndex write SetListIndex;

    property RctBackground: TRectangle read FRctBackground write FRctBackground;
    property RctBase: TRectangle read FRctBase write FRctBase;
    property LneDivisor: TLine read FLneDivisor write FLneDivisor;
    property LytClient: TLayout read FLytClient write FLytClient;

    procedure SetOnClickDefault(AOnClick: TNotifyEvent = nil);
    procedure SetOnTapDefault(ATapEvent: TTapEvent = nil);

    function StrToAlphaColor(const Value: string): TAlphaColor;
  published
    { published declarations }
  end;

implementation

{ TFillAviso }

constructor IlTItemListDefault.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;

  Self.Align := TAlignLayout.Top;

  FRctBackground := TRectangle.Create(Self);
  FRctBackground.Parent := Self;
  FRctBackground.Align := TAlignLayout.Contents;
  FRctBackground.Fill.Color := StrToAlphaColor('White');
  FRctBackground.Stroke.Kind := TBrushKind.None;
  FRctBackground.Stroke.Color := StrToAlphaColor('Null');
  FRctBackground.HitTest := False;

  FRctBase := TRectangle.Create(Self);
  FRctBase.Parent := Self;
  FRctBase.Align := TAlignLayout.Client;
  FRctBase.Fill.Color := StrToAlphaColor('White');
  FRctBase.Stroke.Kind := TBrushKind.None;
  FRctBase.Stroke.Color := StrToAlphaColor('Null');
  FRctBase.Margins.Bottom := 2;
  FRctBase.Margins.Left := 2;
  FRctBase.Margins.Right := 2;
  FRctBase.Margins.Top := 2;
  FRctBase.HitTest := True;

  FLneDivisor := TLine.Create(FRctBase);
  FLneDivisor.Parent := FRctBase;
  FLneDivisor.Align := TAlignLayout.Bottom;
  FLneDivisor.HitTest := False;
  FLneDivisor.LineType := TLineType.Bottom;
  FLneDivisor.Opacity := 0.3;
  FLneDivisor.Stroke.Color := StrToAlphaColor('#FF8B8F8B');

  FLytClient := TLayout.Create(FRctBase);
  FLytClient.Parent := FRctBase;
  FLytClient.Align := TAlignLayout.Contents;
  FLytClient.Margins.Bottom := 2;
  FLytClient.Margins.Left := 2;
  FLytClient.Margins.Right := 2;
  FLytClient.Margins.Top := 2;
  FLytClient.HitTest := False;
end;

destructor IlTItemListDefault.Destroy;
begin
  FreeAndNil(FLytClient);
  FreeAndNil(FLneDivisor);
  FreeAndNil(FRctBase);
  FreeAndNil(FRctBackground);
  inherited;
end;

function IlTItemListDefault.GetButtonWithPath(const AName: string;
  const APthData: string): TButton;
var
  LPth: TPath;
begin
  Result := TButton.Create(Self);
  Result.Name := 'btn' + AName;
  Result.Parent := LytClient;
  Result.Align := TAlignLayout.Right;
  Result.CanFocus := False;
  Result.Text := '';
  Result.StyleLookup := 'btnTransparente';
  Result.Width := Result.Height;

  LPth := TPath.Create(Self);
  LPth.Name := 'pth' + AName;
  LPth.Parent := Result;
  LPth.Align := TAlignLayout.Contents;
  LPth.HitTest := False;
  LPth.Margins.Bottom := 5;
  LPth.Margins.Left := 5;
  LPth.Margins.Right := 5;
  LPth.Margins.Top := 5;
  LPth.Data.Data := APthData;
  LPth.Stroke.Color := StrToAlphaColor('Null');
  LPth.Stroke.Kind := TBrushKind.None;
end;

function IlTItemListDefault.GetNewLabel(const AFontName: string): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := LytClient;
  Result.Align := TAlignLayout.Client;
  Result.TextSettings.Font.Family := AFontName;
  Result.StyledSettings := [TStyledSetting.Style];
  Result.AutoSize := True;
  Result.WordWrap := False;
  Result.HitTest := False;
end;

procedure IlTItemListDefault.SetIdentify(const Value: string);
begin
  FIdentify := Value;
end;

procedure IlTItemListDefault.SetListIndex(const Value: Integer);
begin
  FListIndex := Value;
end;

procedure IlTItemListDefault.SetOnClickDefault(AOnClick: TNotifyEvent);
begin
  FRctBase.OnClick := AOnClick;
end;

procedure IlTItemListDefault.SetOnTapDefault(ATapEvent: TTapEvent);
begin
  FRctBase.OnTap := ATapEvent;
end;

function IlTItemListDefault.StrToAlphaColor(const Value: string): TAlphaColor;
begin
  Result := StringToAlphaColor(Value);
end;

end.
