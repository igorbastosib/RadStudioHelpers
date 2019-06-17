unit Helper.Lib.Utils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,
  System.StrUtils,
  System.Zip,
  System.ZLib,

  Data.DB,
  Data.Bind.DBScope,
  Data.Bind.Components,

  FMX.Edit,
  FMX.DateTimeCtrls,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Platform,
  FMX.Types,
  FMX.Forms,
  FMX.MultiView,
  FMX.StdCtrls

{$IFDEF ANDROID}
    , FMX.Platform.Android,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.Provider,
  Androidapi.JNI.Net,

  Androidapi.JNI.App,
  FMX.Helpers.Android,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI.OS,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText
{$ENDIF}
{$IF declared(FireMonkeyVersion)}

{$IFDEF MACOS}
, Macapi.CoreFoundation
{$IFEND MACOS}
{$IFEND}
{$IFDEF MSWINDOWS}
    , Vcl.Forms,

  Winapi.Windows,
  Winapi.ShellAPI
{$IFEND}
    , IdTCPClient;

type
  TUtils = class
  private
    class var FChangeTabAction: TChangeTabAction;
    class procedure SetChangeTabAction(const Value: TChangeTabAction); static;
  public
    class property ChangeTabAction: TChangeTabAction read FChangeTabAction
      write SetChangeTabAction;

    class function CountSubString(const aText, aSubstring: string): Integer;
    class function FirstWord(const aText: string): string;
    class function RemovePathFromDir(const APath: string;
      const ANumberToRemove: Integer): string;
    class procedure ForceDirectory(const ADirectory: string);
    class function ClipboardCopyText(const AValue: string): Boolean;

    class function ZipFile(const AZipFile, AFileName: string): Boolean;
    class function UnZipFile(const AZipFile, APathToExtract: string): Boolean;
    class function CompressString(AValue: string): string;
    class function DecompressString(AValue: string): string;

    class function ExecuteFile(const AFileName, AParams, ADefaultDir: string;
      AShowCmd: Integer): THandle;
    class function CheckInternet(const AHost: string;
      const APort: Integer): Boolean;

    class function GetApplicationPath(): string;
    class function GetApplicationVersion(): string;

    class procedure GravaSqlEmTxt(aStrSql: string;
      aManterTexto: Boolean = False);
    class procedure GravaDadosDataSetToTxt(aDataSet: TDataSet;
      aManterTexto: Boolean = False);
    class function LerTxt(const aFileFullPath: String): String;

    class function StreamToString(aStream: TStream): string;
    class procedure ClearVtsList(AVertScroll: TVertScrollBox;
      AClassType: TClass);

    class procedure ManipulaComponentes(aTela: TComponent);
    class function GetCorrectJsonString(const AValue: string): String;

    class procedure ChangeTab(ATabControl: TTabControl; ATabItem: TTabItem);
    class procedure OpenForm(const AFrmClass: TComponentClass;
      ATarget: TFMXObject; var AFrmActive: FMX.Forms.TForm;
      AMainMenu: TMultiView; const AMasterButtonName: string = '');

    class function FormatCurrency(AValue: String; ACasasDecimais: Integer;
      ABackspaceIsPressedEdtCurrency: Boolean = False): String;
    class function PadL(S: string; Ch: Char; Len: Integer): string;
    class function PadR(S: string; Ch: Char; Len: Integer): string;

    class function GetIMEI(): string;

    class function RandomNumber(const ALimit: Integer): Integer;
  private
  end;

implementation

{ TUtils }

class procedure TUtils.ManipulaComponentes(aTela: TComponent);
var
  i: Integer;
begin
  for i := 0 to Pred(aTela.ComponentCount) do
  begin
    if (TLinkControlToField = aTela.Components[i].ClassType) and
      (TLinkControlToField(aTela.Components[i]).Control <> nil) and
      (TLinkControlToField(aTela.Components[i]).Control is TEdit) and
      (Trim(TLinkControlToField(aTela.Components[i]).FieldName) <> '') and
      (TLinkControlToField(aTela.Components[i]).DataSource <> nil) and
      (TBindSourceDB(TLinkControlToField(aTela.Components[i]).DataSource)
      .DataSet <> nil) then
    begin
      TEdit(TLinkControlToField(aTela.Components[i]).Control).MaxLength :=
        TBindSourceDB(TLinkControlToField(aTela.Components[i]).DataSource)
        .DataSet.FieldByName(TLinkControlToField(aTela.Components[i])
        .FieldName).Size;
    end
    else if (TDateEdit = aTela.Components[i].ClassType) then
    begin
      TDateEdit(aTela.Components[i]).IsEmpty := True;
      TDateEdit(aTela.Components[i]).TodayDefault := True;
    end;
  end;
end;

class procedure TUtils.OpenForm(const AFrmClass: TComponentClass;
  ATarget: TFMXObject; var AFrmActive: FMX.Forms.TForm; AMainMenu: TMultiView;
  const AMasterButtonName: string);
var
  LLytBase: TComponent;
  LBtnMenu: TComponent;
begin
  if (Assigned(AFrmActive)) then
  begin
    if (AFrmActive.ClassType = AFrmClass) then
    begin
      Exit;
    end
    else
    begin
      AFrmActive.DisposeOf;
    end;
  end;

  Application.CreateForm(AFrmClass, AFrmActive);

  LLytBase := AFrmActive.FindComponent('lytBackground');
  LBtnMenu := nil;
  if not(AMasterButtonName.IsEmpty) then
    LBtnMenu := AFrmActive.FindComponent(AMasterButtonName);
  if (Assigned(LLytBase)) then
  begin
    TLayout(ATarget).AddObject(TLayout(LLytBase));

    if (Assigned(AMainMenu)) then
    begin
      if Assigned(LBtnMenu) then
        AMainMenu.MasterButton := TButton(LBtnMenu);
      AMainMenu.HideMaster;
    end;
  end;
end;

class function TUtils.GetCorrectJsonString(const AValue: string): string;
begin
  Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

class function TUtils.GetIMEI: string;
{$IFDEF ANDROID}
var
  obj: JObject;
  tm: JTelephonyManager;
  IMEI: String;

  function LocalGetImei(): String;
  var
    obj: JObject;
    tm: JTelephonyManager;
    IMEI: String;
  begin
    Result := 'SEM PERMISSAO';
    { obj := TAndroidHelper.Context.getSystemService
      (TJContext.JavaClass.TELEPHONY_SERVICE);
      // obj := SharedActivityContext.getSystemService(TJContext.JavaClass.TELEPHONY_SERVICE);
      if obj <> nil then
      begin
      tm := TJTelephonyManager.Wrap((obj as ILocalObject).GetObjectID);
      if tm <> nil then
      IMEI := JStringToString(tm.getDeviceId);
      end;
      if IMEI = '' then }
    IMEI := JStringToString(TJSettings_Secure.JavaClass.getString
      (TAndroidHelper.Activity.getContentResolver,
      // SharedActivity.getContentResolver,
      TJSettings_Secure.JavaClass.ANDROID_ID));
    Result := IMEI;
  end;

{$ENDIF}
{$IFDEF IOS}

var
  Device: UIDevice;
{$ENDIF IOS}
begin
  Result := 'SEM PERMISSAO';
{$IFDEF MSWINDOWS}
  Result := 'Windows';
{$ELSE}
{$IFDEF ANDROID}
  Result := 'SEM PERMISSAO';
  obj := TAndroidHelper.Context.getSystemService
    (TJContext.JavaClass.TELEPHONY_SERVICE);
  if obj <> nil then
  begin
    tm := TJTelephonyManager.Wrap((obj as ILocalObject).GetObjectID);
    if tm <> nil then
      IMEI := JStringToString(tm.getDeviceId);
  end;
  if IMEI = '' then
    IMEI := JStringToString(TJSettings_Secure.JavaClass.getString
      (TAndroidHelper.Activity.getContentResolver,
      TJSettings_Secure.JavaClass.ANDROID_ID));
  Result := IMEI;
{$ELSE}
{$IFDEF IOS}
  Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
  Result := '00'; // Device.uniqueIdentifier.UTF8String;
{$ENDIF IOS}
{$ENDIF ANDROID}
{$ENDIF MSWINDOWS}
end;
{

  begin

}

class function TUtils.PadL(S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := S + StringOfChar(Ch, RestLen);
end;

class function TUtils.PadR(S: string; Ch: Char; Len: Integer): string;
var
  RestLen: Integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := StringOfChar(Ch, RestLen) + S;
end;

class function TUtils.RandomNumber(const ALimit: Integer): Integer;
begin
  Result := Random(ALimit) + 1;
end;

class function TUtils.RemovePathFromDir(const APath: string;
  const ANumberToRemove: Integer): string;
var
  i: Integer;
  LText: string;
begin
  Result := APath;
  if (Copy(Result, Result.Length, Result.Length) = TPath.DirectorySeparatorChar)
  then
    Result := Copy(Result, 0, Result.Length - 1);

  for i := 1 to ANumberToRemove do
  begin
    if (Pos(TPath.DirectorySeparatorChar, Result) = 0) then
      Break;
    Result := Copy(Result, 0, LastDelimiter(TPath.DirectorySeparatorChar,
      Result) - 1);
  end;
end;

class procedure TUtils.ChangeTab(ATabControl: TTabControl; ATabItem: TTabItem);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      // ATabControl.SetActiveTabWithTransition(ATabItem, TTabTransition.Slide);
      // ATabControl.ActiveTab := ATabItem;
      FChangeTabAction.Tab := ATabItem;
      FChangeTabAction.ExecuteTarget(ATabControl);
    end);
end;

class function TUtils.CheckInternet(const AHost: string;
const APort: Integer): Boolean;
var
  IdTCPClient: TIdTCPClient;
begin
  Result := False;
  try
    try
      IdTCPClient := TIdTCPClient.Create();
      IdTCPClient.ReadTimeout := 2000;
      IdTCPClient.ConnectTimeout := 2000;
      IdTCPClient.Port := APort;
      IdTCPClient.Host := AHost;
      IdTCPClient.Connect;
      IdTCPClient.Disconnect;
      Result := True;
    Except

    end;
  finally
    IdTCPClient.DisposeOf;
  end;
end;

class procedure TUtils.ClearVtsList(AVertScroll: TVertScrollBox;
AClassType: TClass);
var
  i: Integer;
  LFrame: TComponent;
begin
  if not(Assigned(AVertScroll)) then
    Exit;
  try
    // Pesquisar e deixar isso no formulario padrao de listas.
    AVertScroll.BeginUpdate;
    for i := Pred(AVertScroll.Content.ChildrenCount) downto 0 do
    begin
      if (AVertScroll.Content.Children[i] is AClassType) then
      begin
        LFrame := TComponent(AVertScroll.Content.Children[i] as AClassType);
        LFrame.DisposeOf;
        LFrame := nil;
      end;

    end;
  finally
    AVertScroll.EndUpdate;
  end;
end;

class function TUtils.ClipboardCopyText(const AValue: string): Boolean;
var
  Svc: IFMXClipboardService;
begin
  Result := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc)
  then
  begin
    Svc.SetClipboard(AValue);
    Result := True;
  end;
end;

class function TUtils.CompressString(AValue: string): string;
var
  strInput, strOutput: TStringStream;
  Zipper: TZCompressionStream;
begin
  Result := '';
  strInput := TStringStream.Create(AValue);
  strOutput := TStringStream.Create;
  try
    Zipper := TZCompressionStream.Create(TCompressionLevel.clMax, strOutput);
    try
      Zipper.CopyFrom(strInput, strInput.Size);
    finally
      Zipper.Free;
    end;
    Result := strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

class function TUtils.CountSubString(const aText, aSubstring: string): Integer;
var
  offset: Integer;
begin
  Result := 0;
  offset := PosEx(aSubstring, aText, 1);
  while offset <> 0 do
  begin
    inc(Result);
    offset := PosEx(aSubstring, aText, offset + Length(aSubstring));
  end;
end;

class function TUtils.DecompressString(AValue: string): string;
var
  strInput, strOutput: TStringStream;
  Unzipper: TZDecompressionStream;
begin
  Result := '';
  strInput := TStringStream.Create(AValue);
  strOutput := TStringStream.Create;
  try
    Unzipper := TZDecompressionStream.Create(strInput);
    try
      strOutput.CopyFrom(Unzipper, Unzipper.Size);
    finally
      Unzipper.Free;
    end;
    Result := strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

class function TUtils.ExecuteFile(const AFileName, AParams, ADefaultDir: string;
AShowCmd: Integer): THandle;
begin
{$IFDEF MSWINDOWS}
  Result := ShellExecute(0, 'Open', PChar(AFileName), PChar(AParams),
    PChar(ADefaultDir), AShowCmd);
{$ENDIF}
{$IFDEF MACOS}
  _system(PAnsiChar('open ' + AnsiString(AFileName)));
{$ENDIF}
end;

class function TUtils.FirstWord(const aText: string): string;
var
  i: Integer;
begin
  i := Pos(' ', aText);
  if i > 0 then
    Result := Copy(aText, 1, i - 1)
  else
    Result := aText;
end;

class procedure TUtils.ForceDirectory(const ADirectory: string);
var
  LStrList: TStringList;
  LDirectory: string;
  LDirectorySeparator: string;
  i: Integer;
begin
  try
    LDirectorySeparator := System.IOUtils.TPath.DirectorySeparatorChar;
    LDirectory := StringReplace(ADirectory, LDirectorySeparator, '|',
      [rfReplaceAll]);
    LDirectory := StringReplace(LDirectory, ' ', '_123Espaco123_',
      [rfReplaceAll]);
    LStrList := TStringList.Create;
    LStrList.Delimiter := '|';
    LStrList.DelimitedText := LDirectory;
    LDirectory := EmptyStr;

    LDirectory := LStrList[0] + System.IOUtils.TPath.DirectorySeparatorChar;

    if not(DirectoryExists(LDirectory)) then
      System.SysUtils.ForceDirectories(LDirectory);

    for i := 1 to Pred(LStrList.Count) do
    begin
      LDirectory := System.IOUtils.TPath.Combine(LDirectory, LStrList[i]);

      LDirectory := StringReplace(LDirectory, '_123Espaco123_', ' ',
        [rfReplaceAll]);

      if not(DirectoryExists(LDirectory)) then
        System.SysUtils.ForceDirectories(LDirectory);
    end;
  finally
    FreeAndNil(LStrList);
  end;
end;

class function TUtils.FormatCurrency(AValue: String; ACasasDecimais: Integer;
ABackspaceIsPressedEdtCurrency: Boolean): String;
var
  VAntesSeparador, VDepoisSeparador, VMaiorQue100: String;
  VLength: Integer;
  i: Integer;
begin
  VMaiorQue100 := '';
  if (Pos(',', AValue) <= 0) then
  begin
    AValue := AValue + ',' + PadR('', '0', ACasasDecimais);
  end;

  if not(ABackspaceIsPressedEdtCurrency) then
    if ((Length(AValue)) - (Pos(',', AValue)) < ACasasDecimais) then
    begin
      while ((Length(AValue)) - (Pos(',', AValue)) < ACasasDecimais) do
      begin
        AValue := AValue + '0';
      end;
    end;

  AValue := StringReplace(AValue, ',', '', [rfReplaceAll]);
  AValue := StringReplace(AValue, '.', '', [rfReplaceAll]);

  { Tira os 0 aa esquerda }
  if (TryStrToInt(AValue, i)) then
    AValue := IntToStr(i);

  while (AValue.Length <= ACasasDecimais) do
  begin
    AValue := '0' + AValue;
  end;

  VLength := AValue.Length;

  VAntesSeparador := LeftStr(AValue, VLength - ACasasDecimais);
  VDepoisSeparador := RightStr(AValue, ACasasDecimais);

  Result := VMaiorQue100 + VAntesSeparador + ',' + VDepoisSeparador;
end;

class function TUtils.GetApplicationPath: string;
begin
{$IFDEF MSWINDOWS}
  Result := ExtractFilePath(Application.ExeName);
{$ELSE}
  Result := System.SysUtils.GetCurrentDir;
{$ENDIF}
end;

{$REGION 'GetApplicationVersion'}

class function TUtils.GetApplicationVersion: string;
{$IFDEF ANDROID}
var
  PackageManager: JPackageManager;
  PackageInfo: JPackageInfo;
{$ENDIF}
{$IFDEF MACOS}
var
  CFStr: CFStringRef;
  Range: CFRange;
{$ENDIF}
{$IFDEF  MSWINDOWS}
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
{$ENDIF}
begin
{$IFDEF MACOS}
  CFStr := CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle,
    kCFBundleVersionKey);
  Range.location := 0;
  Range.Length := CFStringGetLength(CFStr);
  SetLength(Result, Range.Length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
{$ENDIF}
{$IFDEF MSWINDOWS}
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d.%d.%d', [LongRec(FixedPtr.dwFileVersionMS).Hi,
  // major
  LongRec(FixedPtr.dwFileVersionMS).Lo, // minor
  LongRec(FixedPtr.dwFileVersionLS).Hi, // release
  LongRec(FixedPtr.dwFileVersionLS).Lo]) // build
{$ENDIF}
{$IFDEF ANDROID}
    PackageManager := SharedActivity.getPackageManager;
  PackageInfo := PackageManager.getPackageInfo
    (SharedActivityContext.getPackageName(),
    TJPackageManager.JavaClass.GET_ACTIVITIES);
  Result := JStringToString(PackageInfo.versionName);
{$ENDIF}
end;
{$ENDREGION}

class procedure TUtils.GravaDadosDataSetToTxt(aDataSet: TDataSet;
aManterTexto: Boolean);
var
  VArq: TextFile;
  VTextoLido: String;
  vTextoAtual: TStrings;
  i: Integer;
begin
{$IFDEF DEBUG}
  // Exit;
  try
    try
      try
        { [ 1 ] Associa a variável do programa "arq" ao arquivo externo "tabuada.txt" na unidade de disco "d" }
        AssignFile(VArq, TPath.Combine(System.SysUtils.GetCurrentDir,
          'debugDadosDataSet.txt'));

        vTextoAtual := TStringList.Create();
        VTextoLido := '';

        if (aManterTexto) then
        begin
          Reset(VArq);

          // Display the file contents
          while not Eof(VArq) do
          begin
            ReadLn(VArq, VTextoLido);
            vTextoAtual.Add(VTextoLido);
          end;
        end;

        vTextoAtual.Add('');
        vTextoAtual.Add('/* ********** */');
        vTextoAtual.Add('');

        with (aDataSet) do
        begin
          try
            DisableControls;
            First;

            while not(Eof) do
            begin
              VTextoLido := '';
              for i := 0 to Fields.Count - 1 do
              begin
                VTextoLido := VTextoLido + ';' + Fields[i].AsString;
              end;
              vTextoAtual.Add(VTextoLido);
              Next;
            end;
          finally
            EnableControls;
          end;
        end;

        { [ 2 ] Cria o arquivo texto "tabuada.txt" na unidade de disco "d" }
        Rewrite(VArq);
        { [ 8 ] Grava uma linha da tabuada no arquivo }
        Writeln(VArq, vTextoAtual.Text);
      finally
        { [ 8 ] Fecha o arquivo texto "tabuada.txt". }
        try
          FreeAndNil(vTextoAtual);
        except
        end;
        CloseFile(VArq);
      end;
    finally
      // try FreeAndNil(VArq); except end;
    end;
  except
    on E: Exception do
    begin
      StrToInt('1');
    end;
  end;
{$ENDIF}
end;

class procedure TUtils.GravaSqlEmTxt(aStrSql: string; aManterTexto: Boolean);
var
  VArq: TextFile;
  vStrList: TStringList;
  vTextoAtual: String;
begin
{$IFDEF DEBUG}
  Exit;
  try
    try
      vStrList := TStringList.Create;

      { [ 1 ] Associa a variável do programa "arq" ao arquivo externo "tabuada.txt" na unidade de disco "d" }
      AssignFile(VArq, TPath.Combine(System.SysUtils.GetCurrentDir,
        'debugSql.txt'));

      vTextoAtual := '';
      if (aManterTexto) then
      begin
        Reset(VArq);

        // Display the file contents
        while not Eof(VArq) do
        begin
          ReadLn(VArq, vTextoAtual);
          vStrList.Add(vTextoAtual);
        end;

        vStrList.Add('');
        vStrList.Add('/* ********** */');
        vStrList.Add('');
      end;

      vStrList.Add(aStrSql + ';');

      { [ 2 ] Cria o arquivo texto "tabuada.txt" na unidade de disco "d" }
      Rewrite(VArq);
      { [ 8 ] Grava uma linha da tabuada no arquivo }
      Writeln(VArq, vStrList.Text);
    finally
      { [ 8 ] Fecha o arquivo texto "tabuada.txt". }
      try
        FreeAndNil(vStrList);
      except
      end;
      CloseFile(VArq);
    end;
  except

  end;
{$ENDIF}
end;

class function TUtils.LerTxt(const aFileFullPath: String): String;
var
  VArq: TextFile; { declarando a variável "arq" do tipo arquivo texto }
  vStrList: TStringList;
  vLinha: string;
begin
  try
    vStrList := TStringList.Create;
    // [ 1 ] Associa a variável do programa "arq" ao arquivo externo "tabuada.txt"
    // na unidade de disco "d"
    AssignFile(VArq, aFileFullPath);

{$I-}         // desativa a diretiva de Input
    Reset(VArq); // [ 3 ] Abre o arquivo texto para leitura
{$I+}         // ativa a diretiva de Input

    if (IOResult <> 0) // verifica o resultado da operação de abertura
    then
      Result := '[ERRO] Erro na abertura do arquivo'
    else
    begin
      // [ 11 ] verifica se o ponteiro de arquivo atingiu a marca de final de arquivo
      while (not Eof(VArq)) do
      begin
        ReadLn(VArq, vLinha);
        // [ 6 ] Lê uma linha do arquivo
        vStrList.Add(vLinha);
      end;

      CloseFile(VArq); // [ 8 ] Fecha o arquivo texto aberto

      Result := vStrList.Text
    end;
  finally
    try
      FreeAndNil(vStrList);
    except
    end;
  end;
end;

class procedure TUtils.SetChangeTabAction(const Value: TChangeTabAction);
begin
  FChangeTabAction := Value;
end;

class function TUtils.StreamToString(aStream: TStream): string;
var
  SS: TStringStream;
begin
  if aStream <> nil then
  begin
    SS := TStringStream.Create('');
    try
      SS.CopyFrom(aStream, 0);
      // No need to position at 0 nor provide size
      Result := SS.DataString;
    finally
      try
        FreeAndNil(SS);
      except
      end;
    end;
  end
  else
  begin
    Result := '';
  end;
end;

class function TUtils.UnZipFile(const AZipFile, APathToExtract: string)
  : Boolean;
var
  LZip: TZipFile;
begin
  Result := False;
  try
    LZip := TZipFile.Create;
    LZip.Open(AZipFile, zmRead);
    LZip.ExtractAll(APathToExtract);
    LZip.Close;
    Result := True;
  finally
    FreeAndNil(LZip);
  end;
end;

class function TUtils.ZipFile(const AZipFile, AFileName: string): Boolean;
var
  LZip: TZipFile;
begin
  Result := False;
  try
    LZip := TZipFile.Create;
    if FileExists(AZipFile) then
      LZip.Open(AZipFile, zmReadWrite)
    else
      LZip.Open(AZipFile, zmWrite);
    LZip.Add(AFileName, ExtractFileName(AFileName));
    LZip.Close;
    Result := True;
  finally
    FreeAndNil(LZip);
  end;
end;

end.
