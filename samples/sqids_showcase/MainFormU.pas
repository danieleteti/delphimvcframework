unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MVCFramework.Commons,
  Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    EditAlphabet: TEdit;
    Label1: TLabel;
    TrackBarMinLength: TTrackBar;
    Label2: TLabel;
    btnShuffle: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    EditNumbers: TEdit;
    btnEncode: TButton;
    EditSqidsOutput: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    EditIntegersOutput: TEdit;
    btnDecode: TButton;
    Label6: TLabel;
    EditSqidsInput: TEdit;
    lblMinSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnEncodeClick(Sender: TObject);
    procedure TrackBarMinLengthChange(Sender: TObject);
    procedure btnDecodeClick(Sender: TObject);
    procedure btnShuffleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnDecodeClick(Sender: TObject);
begin
  var lSqids: IMVCSqidsEncoder := TMVCSqidsEncoder.Create(
    EditAlphabet.Text, TrackBarMinLength.Position);

  var lIntegers := lSqids.Decode(EditSqidsInput.Text);
  var lOutput := '';
  for var I in lIntegers do
  begin
    lOutput := lOutput + ',' + I.ToString;
  end;
  EditIntegersOutput.Text := lOutput.Substring(1);
end;

procedure TMainForm.btnEncodeClick(Sender: TObject);
begin
  var lSqids: IMVCSqidsEncoder := TMVCSqidsEncoder.Create(
    EditAlphabet.Text, TrackBarMinLength.Position);
  var lPieces := String(EditNumbers.Text).Split([',']);
  var lIntegers: TArray<UInt64> := [];
  for var lPiece in lPieces do
  begin
    lIntegers := lIntegers + [StrToInt(lPiece)];
  end;

  EditSqidsOutput.Text := lSqids.Encode(lIntegers);
end;

function GetScrambled(const Alphabet: String): String;
var
  I: Integer;
  lIdx1: Integer;
  lSize: Integer;
  lIdx2: Integer;
  lTmp: Char;
begin
  Randomize;
  Result := Alphabet;
  lSize := Length(Result);
  for I := 1 to 100 do
  begin
    lIdx1 := Random(lSize) + 1;
    lIdx2 := Random(lSize) + 1;
    lTmp := Result[lIdx1];
    Result[lIdx1] := Result[lIdx2];
    Result[lIdx2] := lTmp;
  end;
end;


procedure TMainForm.btnShuffleClick(Sender: TObject);
begin
  EditAlphabet.Text := GetScrambled(EditAlphabet.Text);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  EditAlphabet.Text := TMVCSqids.DEFAULT_ALPHABET;
  TrackBarMinLength.Position := 5;
  TrackBarMinLengthChange(TrackBarMinLength);
end;

procedure TMainForm.TrackBarMinLengthChange(Sender: TObject);
begin
  lblMinSize.Caption := TrackBarMinLength.Position.ToString;
end;

end.
