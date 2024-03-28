unit uByteTools;

interface

uses
  SysUtils;

type
  {$IF RTLVersion<25}
  IntPtr=Integer;
  {$IFEND IntPtr}
  
  TByteTools = class(TObject)
  public   
     class function varToByteString(const v; len: Cardinal; Split: string = ' '):
         String;

     class function varToHexString(const v; len: Cardinal; Split: string = ' '):
         String;

     /// <summary>
     ///  16进制转 二进制
     /// </summary>
     class function HexToBin(pvHexStr:String; buf:Pointer):Integer;

     /// <summary>
     ///  16进制字符到二进制
     /// </summary>
     class function HexValue(c: Char): Integer;

     /// <summary>
     ///   是否16进制字符
     /// </summary>
     class function IsHexChar(c: Char): Boolean;

     /// <summary>
     ///   高低位进行交换
     /// </summary>
     class function swap32(v:Integer):Integer;

     /// <summary>
     ///   高低位进行交换
     /// </summary>
     class function swap64(v:int64):Int64;

     /// <summary>
     ///   高低位进行交换
     /// </summary>
     class function swap16(const v):Word;
  end;

implementation



class function TByteTools.HexToBin(pvHexStr: String;
  buf: Pointer): Integer;
var
  l: Integer;
  p, ps: PChar;
  pd: PByte;
begin
  l := Length(pvHexStr);
  p := PChar(pvHexStr);
  ps := p;
  pd := PByte(buf);
  Result := 0;
  while p - ps < l do
  begin
    if IsHexChar(p[0]) and IsHexChar(p[1]) then
    begin
      pd^ := (HexValue(p[0]) shl 4) + HexValue(p[1]);
      inc(Result);
      Inc(pd);
      Inc(p, 2);
      end
    else
    begin
      Exit;
    end;
  end;
end;

class function TByteTools.HexValue(c: Char): Integer;
begin
  if (c >= '0') and (c <= '9') then
    Result := Ord(c) - Ord('0')
  else if (c >= 'a') and (c <= 'f') then
    Result := 10 + Ord(c) - Ord('a')
  else
    Result := 10 + Ord(c) - Ord('A');
end;

class function TByteTools.IsHexChar(c: Char): Boolean;
begin
  Result := ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or ((c >= 'A') and (c <= 'F'));
end;


class function TByteTools.swap16(const v): Word;
begin
  // FF, EE : EE->1, FF->2
  PByte(@result)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 1)^ := PByte(@v)^;
end;

class function TByteTools.swap32(v: Integer): Integer;
var
  lvPByte : PByte;
begin
  result := v;
  lvPByte := PByte(@result);
  PByte(lvPByte)^ := byte(v shr 24);
  PByte(IntPtr(lvPByte) + 1)^ := byte(v shr 16);
  PByte(IntPtr(lvPByte) + 2)^ := byte(v shr 8);
  PByte(IntPtr(lvPByte) + 3)^ := byte(v);
end;

class function TByteTools.swap64(v: int64): Int64;
var
  lvPByte : PByte;
begin
  result := v;
  lvPByte := PByte(@result);
  PByte(lvPByte)^ := byte(v shr 56);  //8 * 7
  PByte(IntPtr(lvPByte) + 1)^ := byte(v shr 48); //6
  PByte(IntPtr(lvPByte) + 2)^ := byte(v shr 40); //5
  PByte(IntPtr(lvPByte) + 3)^ := byte(v shr 32); //4
  PByte(IntPtr(lvPByte) + 4)^ := byte(v shr 24); //3
  PByte(IntPtr(lvPByte) + 5)^ := byte(v shr 16); //2
  PByte(IntPtr(lvPByte) + 6)^ := byte(v shr 8); //2
  PByte(IntPtr(lvPByte) + 7)^ := byte(v); //1
end;

class function TByteTools.varToByteString(const v; len: Cardinal; Split: string
    = ' '): String;
var
  lvSource:PByte;
  i: Integer;
begin
  lvSource := PByte(@v);
  for i := 1 to len do
  begin
    Result := Result + IntToStr(lvSource^) + Split;
    Inc(lvSource);
  end;

end;

class function TByteTools.varToHexString(const v; len: Cardinal; Split: string
    = ' '): String;
var
  lvSource:PByte;
  i: Integer;
begin
  lvSource := PByte(@v);
  for i := 1 to len do
  begin
    Result := Result + IntToHex(lvSource^, 2) + Split;
    Inc(lvSource);
  end;
  
end;

end.
