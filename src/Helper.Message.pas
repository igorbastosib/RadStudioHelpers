unit Helper.Message;

interface

uses
  System.Classes,
  System.UITypes,
  System.UIConsts,
  System.SysUtils,

  FMX.Controls,
  FMX.Objects,
  FMX.Graphics,
  FMX.Layouts,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Dialogs,
  FMX.Forms,

  IdeaL.Lib.CommonTypes, FMX.Ani;

type
  TMessage = class
  private
  class var
    FLytBackground: TLayout;
    FLytButtons: TLayout;
    FRctBackground: TRectangle;
    FRctMessage: TRectangle;
    FRectSbtOk: TRectangle;
    FRectSbtYes: TRectangle;
    FRectSbtNo: TRectangle;
    FBtnOk: TButton;
    FBtnYes: TButton;
    FBtnNo: TButton;
    FSdwMessage: TShadowEffect;
    FLblHeader: TLabel;
    FLblMessage: TLabel;
    FTextColor: string;
    FTextFont: string;
    FBtnColor: string;
    FBackgroundColor: string;
    FProcToastFloatAnimationFinish: TProc;

    FStyleBook: TStyleBook;
    FParent: TControl;
    FCloseDialogProc: TInputCloseDialogProc;

    class procedure PrepareBackground();
    class procedure FRctBackgroundClick(Sender: TObject);
    class procedure ButtonClick(Sender: TObject);
    class procedure Resize();
    class procedure AddButtons(const aMsgTypeButton: TMsgTypeButton = mtbOk);
    class procedure ShowMessage();
    class procedure ToastFloatAnimationFinish(Sender: TObject);
  public
    class procedure Hide();
    class procedure Show(const ATextColor: string; const ATextFont: string;
      const ABtnColor: string; const ABackgroundColor: string;
      const aMsg: String = ''; const aTitle: String = '';
      const aMsgTypeButton: TMsgTypeButton = mtbOk;
      const aMsgType: TMsgType = mtInformation;
      const ACloseDialogProc: TInputCloseDialogProc = nil);
    class procedure Toast(const aMsg, AFontName: string; const ADelay: Single;
      const AProc: TProc = nil);
  end;

implementation

{ TMessage }

class procedure TMessage.AddButtons(const aMsgTypeButton: TMsgTypeButton);

  procedure ConfiguraRectangle(var aRec: TRectangle; const aAlign: TAlignLayout;
    aColor: String);
  begin
    if not(Assigned(aRec)) then
      aRec := TRectangle.Create(Application.MainForm);
    aRec.Parent := FLytButtons;
    aRec.Visible := True;
    aRec.Align := aAlign;
    aRec.Stroke.Kind := TBrushKind.None;
    aRec.Fill.Color := StringToAlphaColor(aColor);
    aRec.Width := aRec.Height;
    aRec.ClipChildren := True;
    aRec.XRadius := 8;
    aRec.YRadius := 8;
  end;

  procedure ConfiguraBotao(var aBtn: TButton; const aParant: TControl;
    const aTexto: String; const aModalResult: TModalResult);
  begin
    if not(Assigned(aBtn)) then
      aBtn := TButton.Create(Application.MainForm);
    aBtn.Parent := aParant;
    aBtn.StyleLookup := 'btnTransparente';
    aBtn.Align := TAlignLayout.Contents;
    aBtn.Text := aTexto;
    aBtn.StyledSettings := [];
    aBtn.Font.Style := [TFontStyle.fsBold];
    aBtn.TextSettings.Font.Family := FTextFont;
    aBtn.TextSettings.Font.Size := 16;
    aBtn.TextSettings.FontColor := StringToAlphaColor('White');
    aBtn.CanFocus := False;
    aBtn.ModalResult := aModalResult;
    aBtn.OnClick := ButtonClick;
  end;

begin
  if (Assigned(FRectSbtOk)) then
    FRectSbtOk.Visible := False;

  if (Assigned(FRectSbtYes)) then
    FRectSbtYes.Visible := False;

  if (Assigned(FRectSbtNo)) then
    FRectSbtNo.Visible := False;

  case aMsgTypeButton of
    mtbOk:
      begin
        ConfiguraRectangle(FRectSbtOk, TAlignLayout.HorzCenter, FBtnColor);
        ConfiguraBotao(FBtnOk, FRectSbtOk, 'OK', idOK);
      end;
    mtbYesNo:
      begin
        ConfiguraRectangle(FRectSbtYes, TAlignLayout.Left, FBtnColor);
        ConfiguraBotao(FBtnYes, FRectSbtYes, 'Sim', idYes);

        ConfiguraRectangle(FRectSbtNo, TAlignLayout.Right, FBtnColor);
        ConfiguraBotao(FBtnNo, FRectSbtNo, 'Não', idNo);

        FLytButtons.Margins.Left := { FRctMessage.Width - } FRectSbtYes.
          Width * 1.2;
        FLytButtons.Margins.Right := { FRctMessage.Width - } FRectSbtYes.
          Width * 1.2;
      end;
    mtbLoading:
      ;
    mtbNone:
      ;
  end;
end;

class procedure TMessage.ButtonClick(Sender: TObject);
begin
  Hide;
  if (TInputCloseDialogProc(FCloseDialogProc) <> nil) then
    FCloseDialogProc(TButton(Sender).ModalResult);
  Application.ProcessMessages;
end;

class procedure TMessage.FRctBackgroundClick(Sender: TObject);
begin
  Hide();
end;

class procedure TMessage.Hide;
begin
  FLytBackground.Visible := False;
  FParent.SetFocus;
end;

class procedure TMessage.PrepareBackground();
begin
  FParent := TControl(Application.MainForm); // FormMain.tbcMain;

  if not(Assigned(FLytBackground)) then
    FLytBackground := TLayout.Create(Application.MainForm);
  FLytBackground.Visible := False;
  FLytBackground.Align := TAlignLayout.Contents;
  //
{$REGION 'FRctBackground'}
  if not(Assigned(FRctBackground)) then
    FRctBackground := TRectangle.Create(Application.MainForm);
  FRctBackground.Stroke.Kind := TBrushKind.None;
  FRctBackground.Fill.Color := StringToAlphaColor('Black');
  FRctBackground.Align := TAlignLayout.Contents;
  FRctBackground.Parent := FLytBackground;
  FRctBackground.OnClick := FRctBackgroundClick;
{$ENDREGION}
  //
{$REGION 'FRctMessage'}
  if not(Assigned(FRctMessage)) then
    FRctMessage := TRectangle.Create(Application.MainForm);
  FRctMessage.Stroke.Kind := TBrushKind.None;
  FRctMessage.Fill.Color := StringToAlphaColor(FBackgroundColor);
  FRctMessage.Align := TAlignLayout.VertCenter;
  FRctMessage.Parent := FLytBackground;
  FRctMessage.Margins.Left := 20;
  FRctMessage.Margins.Right := 20;
  FRctMessage.XRadius := 8;
  FRctMessage.YRadius := 8;
  FRctMessage.BringToFront;
  FRctMessage.Height := 100;

  FSdwMessage := TShadowEffect.Create(Application.MainForm);
  FSdwMessage.Parent := FRctMessage;
{$ENDREGION}
  //
{$REGION 'FLytButtons'}
  if not(Assigned(FLytButtons)) then
    FLytButtons := TLayout.Create(Application.MainForm);
  FLytButtons.Align := TAlignLayout.Bottom;
  FLytButtons.Parent := FRctMessage;
  FLytButtons.Margins.Bottom := 10;
  FLytButtons.Margins.Left := 4;
  FLytButtons.Margins.Right := 4;
  FLytButtons.Margins.Top := 5;
  FLytButtons.Height := 50;
  FLytButtons.BringToFront;
{$ENDREGION}
  //
{$REGION 'FLblHeader'}
  if not(Assigned(FLblHeader)) then
    FLblHeader := TLabel.Create(Application.MainForm);
  FLblHeader.Parent := FRctMessage;
  FLblHeader.Align := TAlignLayout.MostTop;
  FLblHeader.Margins.Bottom := 5;
  FLblHeader.Margins.Left := 4;
  FLblHeader.Margins.Right := 4;
  FLblHeader.Margins.Top := 5;
  FLblHeader.StyledSettings := [];
  FLblHeader.AutoSize := True;
  FLblHeader.Font.Style := [];
  FLblHeader.TextSettings.Font.Family := FTextFont;
  FLblHeader.TextSettings.Font.Size := 16;
  FLblHeader.TextSettings.FontColor := StringToAlphaColor(FTextColor);
  FLblHeader.TextSettings.HorzAlign := TTextAlign.Center;
{$ENDREGION}
  //
{$REGION 'FLblMessage'}
  if not(Assigned(FLblMessage)) then
    FLblMessage := TLabel.Create(Application.MainForm);
  FLblMessage.Parent := FRctMessage;
  FLblMessage.Align := TAlignLayout.Top;
  FLblMessage.Margins.Bottom := 5;
  FLblMessage.Margins.Left := 4;
  FLblMessage.Margins.Right := 4;
  FLblMessage.Margins.Top := 5;
  FLblMessage.StyledSettings := [];
  FLblMessage.AutoSize := True;
  FLblMessage.Font.Style := [];
  FLblMessage.TextSettings.Font.Family := FTextFont;
  FLblMessage.TextSettings.Font.Size := 16;
  FLblMessage.TextSettings.FontColor := StringToAlphaColor(FTextColor);
  FLblMessage.TextSettings.HorzAlign := TTextAlign.Center;
{$ENDREGION}
  //
  FLytBackground.Parent := Application.MainForm;
end;

class procedure TMessage.Resize();
var
  vTam: Single;
begin
  FRctMessage.Height := FLytButtons.Height + FLytButtons.Margins.Bottom +
    FLytButtons.Margins.Top + FLblHeader.Height + FLblHeader.Margins.Bottom +
    FLblHeader.Margins.Top + FLblMessage.Height + FLblMessage.Margins.Bottom +
    FLblMessage.Margins.Top;

  // para nao permitir que ele suma com os botoes
  if (FRctMessage.Height > Application.MainForm.ClientHeight) then
    FRctMessage.Height := Application.MainForm.ClientHeight - 20;
end;

class procedure TMessage.Show(const ATextColor: string; const ATextFont: string;
  const ABtnColor: string; const ABackgroundColor: string; const aMsg: String;
  const aTitle: String; const aMsgTypeButton: TMsgTypeButton;
  const aMsgType: TMsgType; const ACloseDialogProc: TInputCloseDialogProc);
begin
  FTextColor := ATextColor;
  FTextFont := ATextFont;
  FBtnColor := ABtnColor;
  FBackgroundColor := ABackgroundColor;

  PrepareBackground();

  FLblHeader.Visible := (Trim(aTitle) <> '');

  FLblHeader.Text := aTitle;
  FLblMessage.Text := aMsg;

  AddButtons(aMsgTypeButton);
  FCloseDialogProc := ACloseDialogProc;

  ShowMessage;
end;

class procedure TMessage.ShowMessage;
begin
  FLytBackground.Visible := True;

  FLytBackground.Opacity := 0;
  FRctBackground.Opacity := 0;

  FLytBackground.Visible := True;
  FRctBackground.Visible := True;

  Resize();

  FRctBackground.AnimateFloat('opacity', 0.5);
  FLytBackground.AnimateFloat('opacity', 1);
  FLytBackground.BringToFront;
  FLytBackground.SetFocus;

end;

class procedure TMessage.Toast(const aMsg, AFontName: string;
  const ADelay: Single; const AProc: TProc = nil);
var
  LLytToastBackground: TLayout;
  LLytToast: TLayout;
  LRctToastBackground: TRectangle;
  LLblMsg: TLabel;
  LFloatAnimation: TFloatAnimation;
  LRandomNumber: Integer;
begin
  FProcToastFloatAnimationFinish := AProc;
  LRandomNumber := 1 + Random(1000);

  LLytToastBackground :=
    TLayout(Application.MainForm.FindComponent('lytToastBackground'));
  if (LLytToastBackground = nil) then
    LLytToastBackground := TLayout.Create(Application.MainForm);
  LLytToastBackground.Align := TAlignLayout.Contents;
  LLytToastBackground.Visible := False;
  LLytToastBackground.Name := 'lytToastBackground';

{$REGION 'LLytToast'}
  LLytToast := TLayout.Create(LLytToastBackground);
  LLytToast.Align := TAlignLayout.Bottom;
  LLytToast.Parent := LLytToastBackground;
  LLytToast.Height := 50;
  if (LLytToastBackground.ChildrenCount = 1) then
    LLytToast.Margins.Bottom := 40
  else
    LLytToast.Margins.Bottom := 10;
  LLytToast.Margins.Left := 40;
  LLytToast.Margins.Right := 40;
  LLytToast.Opacity := 0;
  LLytToast.HitTest := True;
  LLytToast.Name := 'lytToast' + LRandomNumber.ToString;
{$ENDREGION}
  //
{$REGION 'LRctToastBackground'}
  LRctToastBackground := TRectangle.Create(LLytToast);
  LRctToastBackground.Parent := LLytToast;
  LRctToastBackground.Align := TAlignLayout.Contents;
  LRctToastBackground.XRadius := 5;
  LRctToastBackground.YRadius := 5;
  LRctToastBackground.Stroke.Kind := TBrushKind.None;
  LRctToastBackground.Stroke.Color := StringToAlphaColor('Null');
  LRctToastBackground.Fill.Color := StringToAlphaColor('Black');
  LRctToastBackground.Opacity := 0;
{$ENDREGION}
  //
{$REGION 'LLblMsg'}
  LLblMsg := TLabel.Create(LLytToast);
  LLblMsg.Parent := LLytToast;
  LLblMsg.Align := TAlignLayout.Top;
  LLblMsg.Margins.Left := 10;
  LLblMsg.Margins.Right := 10;
  LLblMsg.StyledSettings := [TStyledSetting.Size, TStyledSetting.Style];
  LLblMsg.TextSettings.HorzAlign := TTextAlign.Center;
  LLblMsg.TextSettings.Font.Family := AFontName;
  LLblMsg.TextSettings.FontColor := StringToAlphaColor('White');
  LLblMsg.TextSettings.WordWrap := True;
  LLblMsg.AutoSize := True;
  LLblMsg.Text := aMsg;
{$ENDREGION}
  //
{$REGION 'FloatAnimation'}
  LFloatAnimation := TFloatAnimation.Create(LLytToast);
  LFloatAnimation.Delay := ADelay;
  LFloatAnimation.Duration := 0.2;
  LFloatAnimation.PropertyName := 'Opacity';
  LFloatAnimation.StartValue := 1;
  LFloatAnimation.StopValue := 0;
  LFloatAnimation.Tag := LRandomNumber;
  LFloatAnimation.OnFinish := ToastFloatAnimationFinish;
  LFloatAnimation.Parent := LLytToast;
{$ENDREGION}
  //
  LLytToastBackground.Parent := Application.MainForm;
  LLytToastBackground.Visible := True;
  //
{$REGION 'Resize'}
  LLblMsg.RecalcSize;

  if (LLblMsg.Height > LLytToast.Height) then
    LLytToast.Height := LLblMsg.Height + 10;

  LLblMsg.Align := TAlignLayout.Client;
{$ENDREGION}
  //

  LLytToastBackground.BringToFront;
  LRctToastBackground.AnimateFloat('opacity', 0.7);
  LLytToast.AnimateFloat('opacity', 1);
  LLytToastBackground.SetFocus;

  LFloatAnimation.Enabled := True;
end;

class procedure TMessage.ToastFloatAnimationFinish(Sender: TObject);
var
  LLytBackground: TLayout;
  LLyt: TLayout;
begin
  TFloatAnimation(Sender).Enabled := False;
  LLyt := TLayout(TFloatAnimation(Sender).Parent);
  if (LLyt = nil) then
    Exit;

  TFloatAnimation(Sender).Parent := nil;
  LLyt.Parent := nil;
  FreeAndNil(LLyt);

  LLytBackground := TLayout(Application.MainForm.FindComponent
    ('lytToastBackground'));
  if (LLytBackground = nil) then
    Exit;
  if (LLytBackground.ChildrenCount = 0) then
  begin
    LLytBackground.Parent := nil;
    FreeAndNil(LLytBackground);
  end;

  if(Assigned(FProcToastFloatAnimationFinish))then
    FProcToastFloatAnimationFinish;
end;

end.
