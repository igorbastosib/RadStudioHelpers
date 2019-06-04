unit Helper.ManageIniFile;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.IniFiles;

type
  TManageIniFile = class
  public
    class function ClearIni(const AFilePath, AFileName: String): Boolean;

    class function ReadIni(const AFilePath, AFileName, ASection, AIdent,
      AValue: string): String;
    class function ReadIniInt(const AFilePath, AFileName, ASection, AIdent,
      AValue: string): Integer;
    class function ReadIniBool(const AFilePath, AFileName, ASection, AIdent,
      AValue: string): Boolean;

    class function WriteIni(const AFilePath, AFileName, ASection, AIdent,
      AValue: string): Boolean;
  private
  end;

implementation

{ TIniFile }

class function TManageIniFile.ClearIni(const AFilePath,
  AFileName: String): Boolean;
begin
  Result := False;
  if (FileExists(System.IOUtils.TPath.Combine(AFilePath, AFileName))) then
  begin
    try
      DeleteFile(System.IOUtils.TPath.Combine(AFilePath, AFileName));
      Result := True;
    except

    end;
  end;
end;

class function TManageIniFile.ReadIni(const AFilePath, AFileName, ASection,
  AIdent, AValue: string): String;
var
  vIniFile: TIniFile;
begin
  Result := AValue;
  if (FileExists(System.IOUtils.TPath.Combine(AFilePath, AFileName))) then
  begin
    try
      vIniFile := TIniFile.Create(System.IOUtils.TPath.Combine(AFilePath,
        AFileName));
      Result := vIniFile.ReadString(ASection, AIdent, AValue);
    finally
      FreeAndNil(vIniFile);
    end;
  end;
end;

class function TManageIniFile.ReadIniInt(const AFilePath, AFileName, ASection,
  AIdent, AValue: string): Integer;
begin
  TryStrToInt(ReadIni(AFilePath, AFileName, ASection, AIdent, AValue), Result);
end;

class function TManageIniFile.ReadIniBool(const AFilePath, AFileName, ASection,
  AIdent, AValue: string): Boolean;
begin
  TryStrToBool(ReadIni(AFilePath, AFileName, ASection, AIdent, AValue), Result);
end;

class function TManageIniFile.WriteIni(const AFilePath, AFileName, ASection,
  AIdent, AValue: string): Boolean;
var
  vIniFile: TIniFile;
begin
  Result := False;
  try
    vIniFile := TIniFile.Create(System.IOUtils.TPath.Combine(AFilePath,
      AFileName));
    vIniFile.WriteString(ASection, AIdent, AValue);

    Result := True;
  finally
    FreeAndNil(vIniFile);
  end;
end;

end.
