unit RadStudioHelpers.Example.Message.FrmMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts;

type
  TForm2 = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TLibrary = class
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    class procedure MessageOk(const AMsg: string;
      const AProc: TInputCloseDialogProc);
    class procedure MessageYesNo(const AMsg: string;
      const AProc: TInputCloseDialogProc);
    class procedure Toast(const AMsg: string; const ADelay: Single = 2;
      const AProc: TProc = nil);
  published
    { published declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Helper.Message,
  Helper.CommonTypes;
{$R *.fmx}
{ TLibrary }

class procedure TLibrary.MessageOk(const AMsg: string;
  const AProc: TInputCloseDialogProc);
begin
  TMessage.Show('Black', 'Verdana', 'Blue', 'White', AMsg, '',
    TMsgTypeButton.mtbOk, TMsgType.mtInformation, AProc);
end;

class procedure TLibrary.MessageYesNo(const AMsg: string;
  const AProc: TInputCloseDialogProc);
begin
  TMessage.Show('Black', 'Verdana', 'Blue', 'White', AMsg, '',
    TMsgTypeButton.mtbYesNo, TMsgType.mtQuestion, AProc);
end;

class procedure TLibrary.Toast(const AMsg: string; const ADelay: Single;
  const AProc: TProc);
begin
  TMessage.Toast(AMsg, 'Verdana', ADelay, AProc);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  TLibrary.MessageOk('Test OK', nil);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  TLibrary.Toast('Showing Toast');
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  TLibrary.MessageYesNo('Test Yes or No?',
    procedure (const AResult: TModalResult)
    begin
      case AResult of
        idYes: ShowMessage('Yes');
        idNo: ShowMessage('No');
      end;
    end
    //nil //can You use Nil equal
  );
end;

end.
