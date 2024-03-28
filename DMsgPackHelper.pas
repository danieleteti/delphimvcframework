unit DMsgPackHelper;

interface

uses
  Classes, SysUtils;

type
  // copy from qmsgPack
  TMsgPackValue= packed record
    ValueType:Byte;
    case Integer of
      0:(U8Val:Byte);
      1:(I8Val:Shortint);
      2:(U16Val:Word);
      3:(I16Val:Smallint);
      4:(U32Val:Cardinal);
      5:(I32Val:Integer);
      6:(U64Val:UInt64);
      7:(I64Val:Int64);
      //8:(F32Val:Single);
      //9:(F64Val:Double);
      10:(BArray:array[0..16] of Byte);
  end;
  {$IF RTLVersion<25}
    IntPtr=Integer;
  {$IFEND IntPtr}

  {$if CompilerVersion < 18} //before delphi 2007
    TBytes = array of Byte;
  {$ifend}


  /// <summary>
  ///   按照msgPack协议
  ///     1.将数据写入到Stream
  ///     2.从流中读取出数据
  /// </summary>
  TDMsgPackHelper = class(TObject)
  public
    /// <summary>
    ///   MsgPack协议方式写入一个字符串

    /// </summary>
    class procedure Write(pvStream: TStream; pvValue: string); overload;


    /// <summary>
    ///   按照MsgPack协议 写入一个二进制数据
    /// </summary>
    class procedure Write(pvStream: TStream; pvBuf:Pointer; pvLen:Cardinal); overload;

    /// <summary>
    ///   按照MsgPack协议 读取一个二进制数据
    /// </summary>
    class function ReadBinary(pvStream: TStream): TBytes; overload;

    /// <summary>
    ///   按照MsgPack协议 读取一个二进制数据,写入到另外一个流
    /// </summary>
    class procedure ReadBinary(pvSourceStream, pvDestStream: TStream); overload;

    /// <summary>
    ///   MsgPack协议读取一个字符串， 如果是null值会返回''
    /// </summary>
    class function ReadString(pvStream:TStream):String;

    /// <summary>
    ///   MsgPack协议方式写入一个整形数据
    ///   copy from qmsgPack
    ///   2015-09-30 12:57:02
    ///   未全面测试
    /// </summary>
    class procedure Write(pvStream: TStream; pvValue: Int64); overload;

    
    /// <summary>
    ///   MsgPack协议读取一个整形， 如果是null值会返回0
    ///   2015-09-30 12:57:02
    ///   未全面测试
    /// </summary>
    class function ReadInt(pvStream:TStream): Int64;


    /// <summary>
    ///   MsgPack协议方式写入一个浮点数据
    ///   2015-09-30 12:57:02
    ///   未全面测试
    /// </summary>
    class procedure Write(pvStream: TStream; pvValue: Double); overload;

    
    /// <summary>
    ///   MsgPack协议读取一个浮点， 如果是null值会返回0
    ///   2015-09-30 12:57:02
    ///   未全面测试
    /// </summary>
    class function ReadFloat(pvStream:TStream): Double;

  end;

implementation

resourcestring
  strErrorStringData = '[%d]非法的字符串协议格式数据';
  strErrorBinaryData = '[%d]非法的二进制协议格式数据';
  strErrorIntData    = '[%d]非法的整数协议格式数据';
  strErrorFloatData  = '[%d]非法的浮点格式数据';

function swap16(const v): Word;
begin
  // FF, EE : EE->1, FF->2
  PByte(@result)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 1)^ := PByte(@v)^;
end;

function swap32(const v): Cardinal;
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@result)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 3)^ := PByte(@v)^;
end;

function swap64(const v): Int64;
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@result)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@result) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@result) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 7)^ := PByte(@v)^;
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

// v and outVal is can't the same value
procedure swap16Ex(const v; out outVal);
begin
  // FF, EE : EE->1, FF->2
  PByte(@outVal)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(@v)^;
end;

// overload swap, result type is integer, because single maybe NaN
function swap(v:Single): Integer; overload;
begin
  swap32Ex(v, Result);
end;

// overload swap
function swap(v:word): Word; overload;
begin
  swap16Ex(v, Result);
end;

// overload swap
function swap(v:Cardinal):Cardinal; overload;
begin
  swap32Ex(v, Result);
end;

// swap , result type is Int64, because Double maybe NaN
function swap(v:Double): Int64; overload;
begin
  swap64Ex(v, Result);
end;


// copy from qstring
function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): string;
const
  B2HConvert: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PChar;
  pb: PByte;
begin
  if SizeOf(Char) = 2 then
  begin
    SetLength(Result, l shl 1);
  end else
  begin
    SetLength(Result, l);
  end;
  pd := PChar(Result);
  pb := p;
  if ALowerCase then
  begin
    while l > 0 do
    begin
      pd^ := B2HConvertL[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvertL[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end
  else
  begin
    while l > 0 do
    begin
      pd^ := B2HConvert[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvert[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end;
end;


function Utf8DecodeEx(pvValue:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF}; len:Cardinal):string;
{$IFDEF UNICODE}
var             
  lvBytes:TBytes;
{$ENDIF}
begin
{$IFDEF UNICODE}
  lvBytes := TEncoding.Convert(TEncoding.UTF8, TEncoding.Unicode, pvValue);
  SetLength(Result, Length(lvBytes) shr 1);
  Move(lvBytes[0], PChar(Result)^, Length(lvBytes));
{$ELSE}
  result:= UTF8Decode(pvValue);
{$ENDIF}
end;

function Utf8EncodeEx(pvValue:string):{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
{$IFDEF UNICODE}
var
  lvBytes:TBytes;
  len:Cardinal;
{$ENDIF}
begin
{$IFDEF UNICODE}
  len := length(pvValue) shl 1;
  SetLength(lvBytes, len);
  Move(PChar(pvValue)^, lvBytes[0], len);
  Result := TEncoding.Convert(TEncoding.Unicode, TEncoding.UTF8, lvBytes);
{$ELSE}
  result:= UTF8Encode(pvValue);
{$ENDIF}
end;


class function TDMsgPackHelper.ReadBinary(pvStream: TStream): TBytes;
var
  lvByte:Byte;
  l:Cardinal;
  lvSavePosition:Int64;
begin
  lvSavePosition := pvStream.Position;
  pvStream.Read(lvByte, 1);
  case lvByte of
    $C0: // null
      begin
        SetLength(Result, 0);
      end;
    $C4: // 短二进制，最长255字节
      begin
        l := 0; // fill zero
        pvStream.Read(l, 1);

        SetLength(Result, l);
        pvStream.Read(Result[0], l);
      end;
    $C5: // 二进制，16位，最长65535B
      begin
        l := 0; // fill zero
        pvStream.Read(l, 2);
        l := swap16(l);

        SetLength(Result, l);
        pvStream.Read(Result[0], l);
      end;
    $C6: // 二进制，32位，最长2^32-1
      begin
        l := 0; // fill zero
        pvStream.Read(l, 4);
        l := swap32(l);

        SetLength(Result, l);
        pvStream.Read(Result[0], l);
      end;
  else
    begin
      pvStream.Position := lvSavePosition;
      raise Exception.CreateFmt(strErrorStringData, [lvByte]);
    end;
  end;
end;

class procedure TDMsgPackHelper.ReadBinary(pvSourceStream, pvDestStream:
    TStream);
var
  lvByte:Byte;
  l:Cardinal;
  lvSavePosition:Int64;
begin
  lvSavePosition := pvSourceStream.Position;
  pvSourceStream.Read(lvByte, 1);
  case lvByte of
    $C4: // 短二进制，最长255字节
      begin
        l := 0; // fill zero
        pvSourceStream.Read(l, 1);

        pvDestStream.CopyFrom(pvSourceStream, l);
      end;
    $C5: // 二进制，16位，最长65535B
      begin
        l := 0; // fill zero
        pvSourceStream.Read(l, 2);
        l := swap16(l);

        pvDestStream.CopyFrom(pvSourceStream, l);
      end;
    $C6: // 二进制，32位，最长2^32-1
      begin
        l := 0; // fill zero
        pvSourceStream.Read(l, 4);
        l := swap32(l);

        pvDestStream.CopyFrom(pvSourceStream, l);
      end;
  else
    begin
      pvSourceStream.Position := lvSavePosition;
      raise Exception.CreateFmt(strErrorStringData, [lvByte]);
    end;
  end;
end;

class function TDMsgPackHelper.ReadFloat(pvStream:TStream): Double;
var
  lvByte:Byte;
  l:Cardinal;
  lvSavePosition:Int64;
  lvBData: array[0..15] of Byte;
  lvSwapData: array[0..15] of Byte;
begin
  lvSavePosition := pvStream.Position;
  pvStream.Read(lvByte, 1);

  case lvByte of
    $C0: // null
      begin
        Result := 0;
      end;
    $CA: // float 32
      begin
        pvStream.Read(lvBData[0], 4);

        swap32Ex(lvBData[0], lvSwapData[0]);

        Result := PSingle(@lvSwapData[0])^;
      end;
    $cb: // Float 64
      begin

        pvStream.Read(lvBData[0], 8);

        swap64Ex(lvBData[0], lvSwapData[0]);

        Result := PDouble(@lvSwapData[0])^;
      end;
  else
    begin
      pvStream.Position := lvSavePosition;
      raise Exception.CreateFmt(strErrorFloatData, [lvByte]);
    end;
  end;
    
end;

class function TDMsgPackHelper.ReadInt(pvStream:TStream): Int64;
var
  lvByte:Byte;
  l:Cardinal;
  lvSavePosition, i64:Int64;
begin
  lvSavePosition := pvStream.Position;
  pvStream.Read(lvByte, 1);
  if lvByte in [$00 .. $7F] then   //positive fixint	0xxxxxxx	0x00 - 0x7f
  begin
    Result := lvByte;
  end else if lvByte in [$E0 .. $FF] then
  begin
    //  negative fixnum stores 5-bit negative integer
    //  +--------+
    //  |111YYYYY|
    //  +--------+
    Result := Shortint(lvByte);
  end else
  begin
    case lvByte of
      $C0: // null
        begin
          Result := 0;
        end;
      $cc: // UInt8
        begin
          //      uint 8 stores a 8-bit unsigned integer
          //      +--------+--------+
          //      |  0xcc  |ZZZZZZZZ|
          //      +--------+--------+
          l := 0;
          pvStream.Read(l, 1);
          Result := l;
        end;
      $cd:
        begin
          //    uint 16 stores a 16-bit big-endian unsigned integer
          //    +--------+--------+--------+
          //    |  0xcd  |ZZZZZZZZ|ZZZZZZZZ|
          //    +--------+--------+--------+
          l := 0;
          pvStream.Read(l, 2);
          l := swap16(l);
          Result := Word(l);
        end;
      $ce:
        begin
          //  uint 32 stores a 32-bit big-endian unsigned integer
          //  +--------+--------+--------+--------+--------+
          //  |  0xce  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ
          //  +--------+--------+--------+--------+--------+
          l := 0;
          pvStream.Read(l, 4);
          l := swap32(l);
          Result := Cardinal(l);
        end;
      $cf:
        begin
          //  uint 64 stores a 64-bit big-endian unsigned integer
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          //  |  0xcf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          i64 := 0;
          pvStream.Read(i64, 8);
          i64 := swap64(i64);
          Result :=UInt64(i64);
        end;
      $d0:   //int 8
        begin
          //      int 8 stores a 8-bit signed integer
          //      +--------+--------+
          //      |  0xd0  |ZZZZZZZZ|
          //      +--------+--------+

          l := 0;
          pvStream.Read(l, 1);
          Result := ShortInt(l);
        end;
      $d1:
        begin
          //    int 16 stores a 16-bit big-endian signed integer
          //    +--------+--------+--------+
          //    |  0xd1  |ZZZZZZZZ|ZZZZZZZZ|
          //    +--------+--------+--------+

          l := 0;
          pvStream.Read(l, 2);
          l := swap16(l);
           Result := SmallInt(l);
        end;
      $d2:
        begin
          //  int 32 stores a 32-bit big-endian signed integer
          //  +--------+--------+--------+--------+--------+
          //  |  0xd2  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+
          l := 0;
          pvStream.Read(l, 4);
          l := swap32(l);
          Result := Integer(l);
        end;
      $d3:
        begin
          //  int 64 stores a 64-bit big-endian signed integer
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          //  |  0xd3  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          i64 := 0;
          pvStream.Read(i64, 8);
          i64 := swap64(i64);
          Result := Int64(i64);
        end;
    else
      begin
        pvStream.Position := lvSavePosition;
        raise Exception.CreateFmt(strErrorIntData, [lvByte]);
      end;
    end;
  end;
end;

class function TDMsgPackHelper.ReadString(pvStream: TStream): String;
var
  lvByte:Byte;
  lvAnsiStr:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l:Cardinal;
  lvSavePosition:Int64;
begin
  lvSavePosition := pvStream.Position;
  pvStream.Read(lvByte, 1);
  if lvByte in [$A0 .. $BF] then //fixstr	101xxxxx	0xa0 - 0xbf
  begin
    l := lvByte - $A0;   // str len
    if l > 0 then
    begin  
      SetLength(lvAnsiStr, l);
      pvStream.Read(PByte(lvAnsiStr)^, l);
      Result :=UTF8DecodeEx(lvAnsiStr, l);
    end else
    begin
      Result :='';
    end;
  end else
  begin
    case lvByte of
      $C0: // null
        begin
          Result := '';
        end;
      $d9:   //str 8 , 255
        begin
          //  str 8 stores a byte array whose length is upto (2^8)-1 bytes:
          //  +--------+--------+========+
          //  |  0xd9  |YYYYYYYY|  data  |
          //  +--------+--------+========+
          l := 0;
          pvStream.Read(l, 1);
          if l > 0 then  // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            Result :=UTF8DecodeEx(lvAnsiStr, l);
          end else
          begin
            Result :='';
          end;
        end;
      $da:    // str 16
        begin
          //      str 16 stores a byte array whose length is upto (2^16)-1 bytes:
          //      +--------+--------+--------+========+
          //      |  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
          //      +--------+--------+--------+========+

          l := 0; // fill zero
          pvStream.Read(l, 2);
          l := swap16(l);
          if l > 0 then  // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            Result :=UTF8DecodeEx(lvAnsiStr, l);
          end else
          begin
            Result :='';
          end;

  //        SetLength(lvBytes, l + 1);
  //        lvBytes[l] := 0;
  //        pvStream.Read(lvBytes[0], l);
  //        setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;
      $db:    // str 16
        begin
          //  str 32 stores a byte array whose length is upto (2^32)-1 bytes:
          //  +--------+--------+--------+--------+--------+========+
          //  |  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
          //  +--------+--------+--------+--------+--------+========+

          l := 0; // fill zero
          pvStream.Read(l, 4);
          l := swap32(l);
          if l > 0 then  // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            Result :=UTF8DecodeEx(lvAnsiStr, l);
          end else
          begin
            Result :='';
          end;
        end;
    else
      begin
        pvStream.Position := lvSavePosition;
        raise Exception.CreateFmt(strErrorBinaryData, [lvByte]);
      end;
    end;
  end;
end;

class procedure TDMsgPackHelper.Write(pvStream: TStream; pvValue: string);
var
  lvRawData:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l:Integer;
  lvValue:TMsgPackValue;
begin
  lvRawData := Utf8EncodeEx(pvValue);
  l:=Length(lvRawData);
  
  //
  //fixstr stores a byte array whose length is upto 31 bytes:
  //+--------+========+
  //|101XXXXX|  data  |
  //+--------+========+
  //
  //str 8 stores a byte array whose length is upto (2^8)-1 bytes:
  //+--------+--------+========+
  //|  0xd9  |YYYYYYYY|  data  |
  //+--------+--------+========+
  //
  //str 16 stores a byte array whose length is upto (2^16)-1 bytes:
  //+--------+--------+--------+========+
  //|  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
  //+--------+--------+--------+========+
  //
  //str 32 stores a byte array whose length is upto (2^32)-1 bytes:
  //+--------+--------+--------+--------+--------+========+
  //|  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
  //+--------+--------+--------+--------+--------+========+
  //
  //where
  //* XXXXX is a 5-bit unsigned integer which represents N
  //* YYYYYYYY is a 8-bit unsigned integer which represents N
  //* ZZZZZZZZ_ZZZZZZZZ is a 16-bit big-endian unsigned integer which represents N
  //* AAAAAAAA_AAAAAAAA_AAAAAAAA_AAAAAAAA is a 32-bit big-endian unsigned integer which represents N
  //* N is the length of data

  if L<=31 then
  begin
    lvValue.ValueType:=$A0+Byte(L);
    pvStream.WriteBuffer(lvValue.ValueType,1);
  end
  else if L<=255 then
  begin
    lvValue.ValueType:=$d9;
    lvValue.U8Val:=Byte(L);
    pvStream.WriteBuffer(lvValue,2);
  end
  else if L<=65535 then
  begin
    lvValue.ValueType:=$da;
    lvValue.U16Val:=((L shr 8) and $FF) or ((L shl 8) and $FF00);
    pvStream.Write(lvValue,3);
  end else
  begin
    lvValue.ValueType:=$db;
    lvValue.BArray[0]:=(L shr 24) and $FF;
    lvValue.BArray[1]:=(L shr 16) and $FF;
    lvValue.BArray[2]:=(L shr 8) and $FF;
    lvValue.BArray[3]:=L and $FF;
    pvStream.WriteBuffer(lvValue,5);
  end;

  pvStream.Write(PByte(lvRawData)^, l);
end;

class procedure TDMsgPackHelper.Write(pvStream: TStream; pvBuf:Pointer;
    pvLen:Cardinal);
var
  lvValue:TMsgPackValue;
begin
  if pvLen <= 255 then
  begin
    lvValue.ValueType := $C4;
    lvValue.U8Val := Byte(pvLen);
    pvStream.WriteBuffer(lvValue, 2);
  end
  else if pvLen <= 65535 then
  begin
    lvValue.ValueType := $C5;
    lvValue.BArray[0] := (pvLen shr 8) and $FF;
    lvValue.BArray[1] := pvLen and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $C6;
    lvValue.BArray[0] := (pvLen shr 24) and $FF;
    lvValue.BArray[1] := (pvLen shr 16) and $FF;
    lvValue.BArray[2] := (pvLen shr 8) and $FF;
    lvValue.BArray[3] := pvLen and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;

  if pvLen > 0 then
  begin
    pvStream.WriteBuffer(pvBuf^, pvLen);
  end;
end;


class procedure TDMsgPackHelper.Write(pvStream: TStream; pvValue: Int64);
var
  lvValue:TMsgPackValue;
begin
  if pvValue>=0 then
  begin
    if pvValue<=127 then
    begin
      lvValue.U8Val:=Byte(pvValue);
      pvStream.WriteBuffer(lvValue.U8Val,1);
    end
    else if pvValue<=255 then//UInt8
    begin
      lvValue.ValueType:=$cc;
      lvValue.U8Val:=Byte(pvValue);
      pvStream.WriteBuffer(lvValue,2);
    end
    else if pvValue<=65535 then
    begin
      lvValue.ValueType:=$cd;
      lvValue.BArray[0]:=(pvValue shr 8);
      lvValue.BArray[1]:=(pvValue and $FF);
      pvStream.WriteBuffer(lvValue,3);
    end
    else if pvValue<=Cardinal($FFFFFFFF) then
    begin
      lvValue.ValueType:=$ce;
      lvValue.BArray[0]:=(pvValue shr 24) and $FF;
      lvValue.BArray[1]:=(pvValue shr 16) and $FF;
      lvValue.BArray[2]:=(pvValue shr 8) and $FF;
      lvValue.BArray[3]:=pvValue and $FF;
      pvStream.WriteBuffer(lvValue,5);
    end
    else
    begin
      lvValue.ValueType:=$cf;
      lvValue.BArray[0]:=(pvValue shr 56) and $FF;
      lvValue.BArray[1]:=(pvValue shr 48) and $FF;
      lvValue.BArray[2]:=(pvValue shr 40) and $FF;
      lvValue.BArray[3]:=(pvValue shr 32) and $FF;
      lvValue.BArray[4]:=(pvValue shr 24) and $FF;
      lvValue.BArray[5]:=(pvValue shr 16) and $FF;
      lvValue.BArray[6]:=(pvValue shr 8) and $FF;
      lvValue.BArray[7]:=pvValue and $FF;
      pvStream.WriteBuffer(lvValue,9);
    end;
  end
  else//<0
  begin
    if pvValue<=Low(Integer) then  //-2147483648  // 64 bit
    begin
      lvValue.ValueType:=$d3;
      lvValue.BArray[0]:=(pvValue shr 56) and $FF;
      lvValue.BArray[1]:=(pvValue shr 48) and $FF;
      lvValue.BArray[2]:=(pvValue shr 40) and $FF;
      lvValue.BArray[3]:=(pvValue shr 32) and $FF;
      lvValue.BArray[4]:=(pvValue shr 24) and $FF;
      lvValue.BArray[5]:=(pvValue shr 16) and $FF;
      lvValue.BArray[6]:=(pvValue shr 8) and $FF;
      lvValue.BArray[7]:=pvValue and $FF;
      pvStream.WriteBuffer(lvValue,9);
    end
    else if pvValue<=Low(SmallInt) then     // -32768    // 32 bit
    begin
      lvValue.ValueType:=$d2;
      lvValue.BArray[0]:=(pvValue shr 24) and $FF;
      lvValue.BArray[1]:=(pvValue shr 16) and $FF;
      lvValue.BArray[2]:=(pvValue shr 8) and $FF;
      lvValue.BArray[3]:=pvValue and $FF;
      pvStream.WriteBuffer(lvValue,5);
    end
    else if pvValue<=-128 then
    begin
      lvValue.ValueType:=$d1;
      lvValue.BArray[0]:=(pvValue shr 8);
      lvValue.BArray[1]:=(pvValue and $FF);
      pvStream.WriteBuffer(lvValue,3);
    end
    else if pvValue<-32 then
    begin
      lvValue.ValueType:=$d0;
      lvValue.I8Val:=pvValue;
      pvStream.WriteBuffer(lvValue,2);
    end
    else
    begin
      lvValue.I8Val:=pvValue;
      pvStream.Write(lvValue.I8Val,1);
    end;
  end;//End <0

end;

class procedure TDMsgPackHelper.Write(pvStream: TStream; pvValue: Double);
var
  lvValue:TMsgPackValue;
begin
  lvValue.i64Val := swap(pvValue);
  lvValue.ValueType := $CB;
  pvStream.WriteBuffer(lvValue, 9);
end;

end.
