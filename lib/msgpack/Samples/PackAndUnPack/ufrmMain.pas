unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SimpleMsgPack, uByteTools, DMsgPackHelper;

type
  TForm2 = class(TForm)
    btnTester: TButton;
    edtData: TEdit;
    mmoOutPut: TMemo;
    btnDelete: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    btnCheckInteger: TButton;
    Button4: TButton;
    btnFile: TButton;
    Button5: TButton;
    btnDMsgPacker: TButton;
    btnCheckInt2: TButton;
    procedure btnCheckInt2Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnTesterClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnCheckIntegerClick(Sender: TObject);
    procedure btnDMsgPackerClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnDeleteClick(Sender: TObject);
var
  lvmsgPack,lvMsgPack2, lvTempPack:TSimpleMsgPack;
  lvBytes:TBytes;
begin
  lvmsgPack := TSimpleMsgPack.Create;
  lvMsgPack2 := TSimpleMsgPack.Create;
  try
    lvmsgPack.S['key.obj.name'] := edtData.Text;
    lvmsgPack.DeleteObject('key.obj.name');


    lvBytes := lvMsgPack.EncodeToBytes;

    lvMsgPack2.DecodeFromBytes(lvBytes);


    mmoOutPut.Lines.Add(lvMsgPack2.S['key']);

  finally
    lvMsgPack2.Free;
    lvMsgPack.Free;
  end;
end;

procedure TForm2.btnTesterClick(Sender: TObject);
var
  lvmsgPack,lvMsgPack2, lvTempPack:TSimpleMsgPack;
  lvBytes:TBytes;
begin
  lvmsgPack := TSimpleMsgPack.Create;
  lvMsgPack2 := TSimpleMsgPack.Create;
  try
    lvmsgPack.I['int'] := High(Integer);
    lvmsgPack.I['Cardinal'] := High(Cardinal);
    lvmsgPack.I['Int64'] := High(Int64);
    //lvmsgPack.I['start'] := lvmsgPack.I['start'] + 600;

    lvBytes := lvMsgPack.EncodeToBytes;

    lvMsgPack2.clear;
    lvMsgPack2.DecodeFromBytes(lvBytes);


    mmoOutPut.Lines.Add(lvMsgPack2.S['key.obj']);
    mmoOutPut.Lines.Add(IntToStr(lvMsgPack2.I['int']));
    mmoOutPut.Lines.Add(IntToStr(lvMsgPack2.I['Cardinal']));
    mmoOutPut.Lines.Add(IntToStr(lvMsgPack2.I['Int64']));

  finally
    lvMsgPack2.Free;
    lvMsgPack.Free;
  end;

end;

procedure TForm2.Button1Click(Sender: TObject);
var
  lvStream:TMemoryStream;
  lvmsgPack :TSimpleMsgPack;
begin
  lvStream := TMemoryStream.Create;
  lvStream.LoadFromFile('C:\msgpack.dat');
  lvmsgPack := TSimpleMsgPack.Create;
  lvStream.Position := 0;
  lvmsgPack.DecodeFromStream(lvStream);

  mmoOutPut.Lines.Add(IntToStr(lvmsgPack.I['start']));

end;

procedure TForm2.Button2Click(Sender: TObject);
var
  lvmsgPack,lvMsgPack2, lvTempPack:TSimpleMsgPack;
  lvBytes:TBytes;
begin
  lvmsgPack := TSimpleMsgPack.Create;
  lvMsgPack2 := TSimpleMsgPack.Create;
  try
    lvmsgPack.AsInteger := ShortInt($E0);

    lvBytes := lvMsgPack.EncodeToBytes;
    mmoOutPut.Lines.Add(TByteTools.varToHexString(lvBytes[0], Length(lvBytes)));

    lvMsgPack2.DecodeFromBytes(lvBytes);

    ShowMessage(IntToSTr(lvMsgPack2.AsInteger));

   // ShowMessage(lvMsgPack2.AsVariant);

  finally
    lvMsgPack2.Free;
    lvMsgPack.Free;
  end;

end;

procedure TForm2.Button3Click(Sender: TObject);
var
  lvmsgPack, lvMsgPack2: TSimpleMsgPack;
  lvStream: TMemoryStream;
begin
  lvmsgPack := TSimpleMsgPack.Create;
  lvmsgPack2 := TSimpleMsgPack.Create;
  lvmsgPack.ForcePathObject('index').AsInteger := -1;
  lvStream := TMemoryStream.Create;
  try
    lvmsgPack.EncodeToStream(lvStream);
    lvStream.Position := 0;
    lvMsgPack2.DecodeFromStream(lvStream);
    //下面一句：结果等于 'index'，而不是预期中的 -1
    ShowMessage(lvMsgPack2.ForcePathObject('index').AsString);
  finally
    FreeAndNil(lvStream);
  end;

end;

procedure TForm2.btnCheckIntegerClick(Sender: TObject);
var
  lvmsgPack,lvMsgPack2, lvTempPack:TSimpleMsgPack;
  lvBytes:TBytes;
  i, z, j: Int64;
begin
  lvmsgPack := TSimpleMsgPack.Create;
  lvMsgPack2 := TSimpleMsgPack.Create;
  try
    z := High(Int64)- 10000;
    j := z + 10000 - 1;

    i := z;

    while i <= j do
    begin
      lvmsgPack.I[IntToStr(i)] := i;
      i := i + 1;
    end;

    lvBytes := lvMsgPack.EncodeToBytes;

    lvMsgPack2.DecodeFromBytes(lvBytes);
    i := z;
    while i <= j do
    begin
      Assert(lvMsgPack2.I[IntToStr(i)] = i, IntToStr(i));
      i := i + 1;
    end;

  finally
    lvMsgPack2.Free;
    lvMsgPack.Free;
  end;

end;

// v and outVal is can't the same value
procedure swap64Ex(const v; out outVal);
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@outVal)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@outVal) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 7)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap32Ex(const v; out outVal);
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@outVal)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(@v)^;
end;

procedure TForm2.btnCheckInt2Click(Sender: TObject);
var
  lvStream:TMemoryStream;
  lvBytes:TBytes;
  i, z, j,  t2: Int64;
  t:Cardinal;
begin

  t := GetTickCount;
  lvStream := TMemoryStream.Create;
  try
    z := Low(Int64);
    j := z + 10000000 - 1;

    i := z;

    t2 := 0;
    while i <= j do
    begin
      lvStream.Position := 0;
      TDMsgPackHelper.Write(lvStream, i);
      lvStream.Position := 0;
      Assert(i = TDMsgPackHelper.ReadInt(lvStream));
      i := i + 1;
      inc(t2);
    end;

    ShowMessage(format('succ[%d: %d]', [t2, getTickcount- t]));

    z := Low(Integer);
    j := z + 10000000 - 1;

    i := z;

    t2 := 0;
    while i <= j do
    begin
      lvStream.Position := 0;
      TDMsgPackHelper.Write(lvStream, i);
      lvStream.Position := 0;
      Assert(i = TDMsgPackHelper.ReadInt(lvStream));
      i := i + 1;
      inc(t2);
    end;
    ShowMessage(format('succ[%d: %d]', [t2, getTickcount- t]));

    z := Low(SmallInt);
    j := z + 10000000 - 1;

    i := z;

    t2 := 0;
    while i <= j do
    begin
      lvStream.Position := 0;
      TDMsgPackHelper.Write(lvStream, i);
      lvStream.Position := 0;
      Assert(i = TDMsgPackHelper.ReadInt(lvStream));
      i := i + 1;
      inc(t2);
    end;
    ShowMessage(format('succ[%d: %d]', [t2, getTickcount- t]));


    z := High(Int64) - 10000000;
    j := High(Int64) - 1;      // 必须-1 , i:=i+1的时候会超过时会出现问题

    i := z;

    t2 := 0;
    while i <= j do
    begin
      lvStream.Position := 0;
      TDMsgPackHelper.Write(lvStream, i);
      lvStream.Position := 0;
      Assert(i = TDMsgPackHelper.ReadInt(lvStream));
      i := i + 1;
      inc(t2);
    end;
    ShowMessage(format('succ[%d: %d]', [t2, getTickcount- t]));
  finally
    lvStream.Free;
  end;


end;

procedure TForm2.btnDMsgPackerClick(Sender: TObject);
var
  lvStream:TFileStream;
  lvBinary, lvReadStream:TMemoryStream;
  s:AnsiString;
  lvFile:string;
  lvBytes:TBytes;
begin
  lvFile := 'C:\simplemsgpack.msgpack';
  DeleteFile(lvFile);
  lvBinary := TMemoryStream.Create;
  s :='abc_中国人民解放军';
  lvBinary.Write(PAnsiChar(s)^, length(s));
  lvReadStream := TMemoryStream.Create;
  lvStream := TFileStream.Create(lvFile, fmCreate);
  TDMsgPackHelper.Write(lvStream, '中国人民解放军');
  TDMsgPackHelper.Write(lvStream, lvBinary.Memory, lvBinary.Size);

  lvStream.Position := 0;

  mmoOutPut.Lines.Add(TDMsgPackHelper.ReadString(lvStream));
  lvBytes := TDMsgPackHelper.ReadBinary(lvStream);
  mmoOutPut.Lines.Add(TByteTools.varToHexString(lvBytes[0], Length(lvBytes)));



  lvBinary.Free;
  lvStream.Free;
  lvReadStream.Free;
//  P:=TSimpleMsgPack.Create;
//  P.I['A']:=234;
//  P.EncodeToFile('C:\a.txt');
//  P.Free;
//
//
//
//  P2:=TSimpleMsgPack.Create;
//  P2.DecodeFromFile('C:\a.txt');
//  ShowMessage(IntToStr(P2.I['A']));
//  P2.Free;


end;

//function swap(v: Double): Double;
//var
//  d1:Double;
//begin
//  swap64Ex(v, d1);
//  Result := d1;
//end;

procedure TForm2.btnFileClick(Sender: TObject);
var
  P:TSimpleMsgPack;

var
  P2:TSimpleMsgPack;
begin
  P:=TSimpleMsgPack.Create;
  P.I['A']:=234;
  P.EncodeToFile('C:\a.txt');
  P.Free;



  P2:=TSimpleMsgPack.Create;
  P2.DecodeFromFile('C:\a.txt');
  ShowMessage(IntToStr(P2.I['A']));
  P2.Free;

end;

procedure TForm2.Button4Click(Sender: TObject);
var
  d, d1:Double;
  s1, s2:Single;
  i:Integer;
begin
  d := 2.507182;
  mmoOutPut.Lines.Add(TByteTools.varToByteString(d, SizeOf(Double)));

  swap64Ex(d, d1);
  //d1 := swap(d);
  mmoOutPut.Lines.Add(TByteTools.varToByteString(d1, SizeOf(Double)));

  s1 := 1.1;
  mmoOutPut.Lines.Add(TByteTools.varToByteString(s1, SizeOf(Single)));

  swap32Ex(s1, i);
  mmoOutPut.Lines.Add(TByteTools.varToByteString(i, SizeOf(Integer)));

end;

procedure TForm2.Button5Click(Sender: TObject);
var
  d, d2:SmallInt;
  //lvBytes:TBytes;
  lvBytes : array[0..1024*1024] of Byte;
begin
  //SetLength(lvBytes, 1000 * 1000 * 10);

  mmoOutPut.Lines.Add(TByteTools.varToHexString(lvBytes[0], 10));

  d := -128;
  mmoOutPut.Lines.Add(TByteTools.varToHexString(d, SizeOf(SmallInt)));
  d2 := Swap(d);
  mmoOutPut.Lines.Add(TByteTools.varToHexString(d2, SizeOf(SmallInt)));

end;

end.
