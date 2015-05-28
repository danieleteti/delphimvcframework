unit Iocp.VariantPacket;

{*
 * 数据包结构(发送和返回共用这个结构)
 * [SIZE] 4B
 * [CMD] cmd_len(1B) - cmd_name(string)
 * [PARAM_COUNT] 1B
 * [PARAM1] param_name_len(1B) - param_name(string) - param_dim(1B) - param_type(2B) - param_data(nB)
 * [PARAM2] ...
 * [PARAM3] ...
 * [DATA] data_len(4B) - data(nB)
 *}

interface

uses
  Classes, SysUtils, System.Generics.Collections, Variants, VarUtils;

type
  // 使用OleVariant，现在连字符串数组的可变类型都能支持了
  // 除了array of Int64和array of UInt64外，其它大多数可变类型都支持
  // 经过实际测试，VarArrayCreate([0, n - 1], varInt64)直接就会失败
  // 原因是 VarArrayCreate 中 VarTypeIsValidArrayType(varInt64) 会返回 False，
  // 然后就会触发异常 (CVarTypeToElementInfo[varInt64].ValidBase = False)
  // Int64数组可以用TArray<Variant>代替
  TParams = class(TDictionary<string, OleVariant>)
  private
    function GetItem(const Key: string): OleVariant;
    procedure SetItem(const Key: string; const Value: OleVariant);
    function GetName(Index: Integer): string;
    function GetVariant(Index: Integer): OleVariant;
    procedure SetVariant(Index: Integer; const Value: OleVariant);
  protected
    function VarNull(const V: OleVariant): OleVariant;
  public
    procedure Assign(Source: TParams);
    function ParamsToStr: string;

    property Items[const Key: string]: OleVariant read GetItem write SetItem; default;
    property Names[Index: Integer]: string read GetName;
    property Variants[Index: Integer]: OleVariant read GetVariant write SetVariant;
  end;

  TBufStream = class(TMemoryStream)
  public
    constructor Create(Buf: Pointer; Size: Integer); reintroduce;
  end;

  PPackHeader = ^TPackHeader;
  TPackHeader = packed record
    ParamSize: Integer;
    DataSize: Integer;
  end;

  TIocpVariantPacket = class
  private
    FPackSize: Integer;
    FCmd: string;
    FParams: TParams;
    FData: TMemoryStream;

    function GetSimpleVarSize(AVarType: TVarType): Integer;
    procedure PackOneVar(const AVar: OleVariant; AStream: TStream);
    procedure PackVariant(const AVar: OleVariant; AStream: TStream);
    function ReadOneVar(AStream: TStream; AVarType: TVarType = varUnknown): OleVariant;
    function ExtractVariant(AStream: TStream): OleVariant;
  public
    constructor Create(const ACmd: string = '');
    destructor Destroy; override;

    procedure Assign(const Pack: TIocpVariantPacket);
    procedure Clear;
    procedure SaveToStream(const Stream: TMemoryStream);
    procedure LoadFromStream(const Stream: TMemoryStream);
    procedure LoadFromBuf(Buf: Pointer; Size: Integer);

    property PackSize: Integer read FPackSize;
    property Cmd: string read FCmd write FCmd;
    property Params: TParams read FParams;
    property Data: TMemoryStream read FData;
  end;

implementation

function IncIdxs(var idxs: TArray<Integer>; const nLBounds, nHBounds: TArray<Integer>): Integer;
var
  i: Integer;
begin
  i := Length(idxs) - 1;
  while i >= 0 do
  begin
    Inc(idxs[i]);
    if (i >= 0) and (idxs[i] > nHBounds[i]) then
    begin
      idxs[i] := nLBounds[i];
      Dec(i);
    end
    else
      Break;
  end;
  Result := i + 1; //返回 第几维 下标发送变化，最左边维数为1
end;

{ TParams }

procedure TParams.Assign(Source: TParams);
var
  p: TPair<string, OleVariant>;
begin
  Clear;
  for p in Source do
  begin
    AddOrSetValue(p.Key, p.Value);
  end;
end;

function TParams.GetItem(const Key: string): OleVariant;
begin
  TryGetValue(Key, Result);
  Result := VarNull(Result);
end;

function TParams.GetName(Index: Integer): string;
var
  Key: string;
begin
  Result := '';
  if (Index < 0) or (Index >= Count) then Exit;

  for Key in Keys do
  begin
    if (Index = 0) then Exit(Key);
    Dec(Index);
  end;
end;

function TParams.GetVariant(Index: Integer): OleVariant;
var
  Value: OleVariant;
begin
  Result := Unassigned;
  if (Index < 0) or (Index >= Count) then Exit;

  for Value in Values do
  begin
    if (Index = 0) then Exit(VarNull(Value));
    Dec(Index);
  end;
end;

procedure TParams.SetItem(const Key: string; const Value: OleVariant);
begin
  AddOrSetValue(Key, Value);
end;

procedure TParams.SetVariant(Index: Integer; const Value: OleVariant);
var
  Key: string;
begin
  if (Index < 0) or (Index >= Count) then Exit;

  for Key in Keys do
  begin
    if (Index = 0) then
    begin
      AddOrSetValue(Key, Value);
    end;
    Dec(Index);
  end;
end;

function TParams.VarNull(const V: OleVariant): OleVariant;
begin
  if not VarIsNull(V) then
    Result := V
  else
    Result := Unassigned;
end;

function TParams.ParamsToStr: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + ';';
    Result := Result + Names[i] + '=' + VarToStr(Variants[i]);
  end;
end;

{ TBufStream }

constructor TBufStream.Create(Buf: Pointer; Size: Integer);
begin
  inherited Create;
  inherited SetPointer(Buf, Size);
end;

{ TIocpVariantPacket }

constructor TIocpVariantPacket.Create(const ACmd: string = '');
begin
  inherited Create;

  FCmd := ACmd;
  FParams := TParams.Create;
  FData := TMemoryStream.Create;
end;

destructor TIocpVariantPacket.Destroy;
begin
  Data.Free;
  Params.Free;
  inherited Destroy;
end;

procedure TIocpVariantPacket.Assign(const Pack: TIocpVariantPacket);
begin
  Cmd := Pack.Cmd;
  Params.Assign(Pack.Params);
  Data.SetSize(Pack.Data.Size);
  if (Pack.Data.Size > 0) then
    Move(PByte(Pack.Data.Memory)^, PByte(Data.Memory)^, Pack.Data.Size);
  Data.Position := 0;
end;

procedure TIocpVariantPacket.Clear;
begin
  Cmd := '';
  Params.Clear;
  Data.Clear;
end;

function TIocpVariantPacket.GetSimpleVarSize(AVarType: TVarType): Integer;
var
  VTypeInfo: TVarTypeToElementInfo;
begin
  if (AVarType in [CMinArrayVarType..CMaxArrayVarType]) then
  begin
    VTypeInfo := CVarTypeToElementInfo[AVarType];
    if (VTypeInfo.Flags = ARR_NONE) then Exit(VTypeInfo.Size);
  end;
  Result := -1;
end;

procedure TIocpVariantPacket.PackOneVar(const AVar: OleVariant; AStream: TStream);
var
  VSize: Integer;
  AnsiStr: AnsiString;
  Str: string;
  WideStr: WideString;
  n: Integer;
begin
  // 简单数据类型(Flags = ARR_NONE)，直接整体处理
  VSize := GetSimpleVarSize(TVarData(AVar).VType);
  if (VSize > 0) then
  begin
    AStream.Write(TVarData(AVar).VByte, VSize);
    Exit;
  end;

  case TVarData(AVar).VType of
    varOleStr:
      begin
        WideStr := PWideChar(TVarData(AVar).VOleStr);
        n := Length(WideStr);
        AStream.Write(n, 4); // 4字节表示字符串长度
        AStream.Write(PWideChar(WideStr)^, n * SizeOf(WideChar));
      end;
    varString:
      begin
        AnsiStr := PAnsiChar(TVarData(AVar).VString);
        n := Length(AnsiStr);
        AStream.Write(n, 4); // 4字节表示字符串长度
        AStream.Write(PAnsiChar(AnsiStr)^, n);
      end;
    varUString:
      begin
        Str := PChar(TVarData(AVar).VUString);
        n := Length(Str);
        AStream.Write(n, 4); // 4字节表示字符串长度
        AStream.Write(PChar(Str)^, n * SizeOf(Char));
      end;
  end;
end;

procedure TIocpVariantPacket.PackVariant(const AVar: OleVariant; AStream: TStream);
var
  VSize: Integer;
  ndim: Byte;
  vtype: TVarType;
  idxs, nLBounds, nHBounds: TArray<Integer>;
  i: Integer;
  v: OleVariant;
  p: Pointer;
  BlockSize: Integer;
begin
  ndim := VarArrayDimCount(AVar);
  if ndim = 0 then
  begin
    vtype := TVarData(AVar).VType;
    AStream.Write(ndim, 1);
    AStream.Write(vtype, 2);
    PackOneVar(AVar, AStream);
  end
  else
  begin
    SetLength(idxs, ndim);
    SetLength(nLBounds, ndim);
    SetLength(nHBounds, ndim);
    for i := 0 to ndim - 1 do
    begin
      idxs[i] := VarArrayLowBound(AVar, i + 1);
      nLBounds[i] := idxs[i];
      nHBounds[i] := VarArrayHighBound(AVar, i + 1);
      if (nHBounds[i] < nLBounds[i]) then Exit;
    end;

    vtype := TVarData(AVar).VType and varTypeMask; //元素的类型

    AStream.Write(ndim, 1); //数组维数
    AStream.Write(vtype, 2); //元素的类型
    AStream.Write(nLBounds[0], SizeOf(Integer) * ndim); //每一维的LowBound
    AStream.Write(nHBounds[0], SizeOf(Integer) * ndim); //每一维的HighBound

    // 简单数据类型(Flags = ARR_NONE)，直接整体处理
    VSize := GetSimpleVarSize(vtype);
    if (VSize > 0) then
    begin
      BlockSize := 1;
      for i := ndim - 1 downto 0 do
        BlockSize := BlockSize * (nHBounds[i] - nLBounds[i] + 1);
      BlockSize := BlockSize * VSize;

      SafeArrayAccessData(TVarData(AVar).VArray, p);
      try
        AStream.Write(p^, BlockSize)
      finally
        SafeArrayUnaccessData(TVarData(AVar).VArray);
      end;

      Exit;
    end;

    repeat
      v := VarArrayGet(AVar, idxs);

      if vtype = varVariant then
        PackVariant(v, AStream)
      else
        PackOneVar(v, AStream); //不保存每个元素的类型
    until (IncIdxs(idxs, nLBounds, nHBounds) = 0);
  end;
end;

function TIocpVariantPacket.ReadOneVar(AStream: TStream; AVarType: TVarType): OleVariant;
var
  VSize: Integer;
  AnsiStr: AnsiString;
  Str: string;
  WideStr: WideString;
  n: Integer;
begin
  Result := Unassigned;
  // 从Stream中获取类型
  if AVarType = varUnknown then //单独的Variant，不是数组中的元素
    AStream.Read(AVarType, 2);

  VarCast(Result, Result, AVarType); //from: http://npavlov.kodar.net/blog/?p=12   procedure ReadSingleData (VariantType: word; out V: variant)

  // 简单数据类型(Flags = ARR_NONE)，直接整体处理
  VSize := GetSimpleVarSize(AVarType);
  if (VSize > 0) then
  begin
    AStream.Read(TVarData(Result).VByte, VSize);
    Exit;
  end;

  case AVarType of
    varOleStr:
      begin
        AStream.Read(n, 4); // 4字节表示字符串长度
        SetLength(WideStr, n);
        AStream.Read(PWideChar(WideStr)^, n * SizeOf(WideChar));
        Result := WideStr;
      end;
    varString:
      begin
        AStream.Read(n, 4); // 4字节表示字符串长度
        SetLength(AnsiStr, n);
        AStream.Read(PAnsiChar(AnsiStr)^, n);
        Result := AnsiStr;
      end;
    varUString:
      begin
        AStream.Read(n, 4); // 4字节表示字符串长度
        SetLength(Str, n);
        AStream.Read(PChar(Str)^, n * SizeOf(Char));
        Result := Str;
      end;
  end;
end;

function TIocpVariantPacket.ExtractVariant(AStream: TStream): OleVariant;
var
  VSize: Integer;
  ndim: Byte;
  i, BlockSize: Integer;
  vtype: TVarType;
  idxs, nLBounds, nHBounds, nBounds: TArray<Integer>;
  v: OleVariant;
  p: Pointer;
begin
  Result := Unassigned;

  AStream.Read(ndim, 1);
  if ndim = 0 then
    Result := ReadOneVar(AStream)
  else
  begin
    SetLength(idxs, ndim);
    SetLength(nLBounds, ndim);
    SetLength(nHBounds, ndim);
    SetLength(nBounds, ndim * 2);

    AStream.Read(vtype, 2);
    AStream.Read(nLBounds[0], SizeOf(Integer) * ndim);
    AStream.Read(nHBounds[0], SizeOf(Integer) * ndim);
    for i := 0 to ndim - 1 do
    begin
      nBounds[i * 2] := nLBounds[i];
      nBounds[i * 2 + 1] := nHBounds[i];
      if (nHBounds[i] < nLBounds[i]) then Exit;
    end;

    Result := VarArrayCreate(nBounds, vtype);

    // 简单数据类型(Flags = ARR_NONE)，直接整体处理
    VSize := GetSimpleVarSize(vtype);
    if (VSize > 0) then
    begin
      BlockSize := 1;
      for i := ndim - 1 downto 0 do
        BlockSize := BlockSize * (nHBounds[i] - nLBounds[i] + 1);
      BlockSize := BlockSize * VSize;

      SafeArrayAccessData(TVarData(Result).VArray, p);
      try
        AStream.Read(p^, BlockSize)
      finally
        SafeArrayUnaccessData(TVarData(Result).VArray);
      end;

      Exit;
    end;

    Move(nLBounds[0], idxs[0], SizeOf(Integer) * ndim); //初始化idx
    repeat
      if (vtype = varVariant) then
        v := ExtractVariant(AStream)
      else
        v := ReadOneVar(AStream, vtype);

      VarArrayPut(Variant(Result), v, idxs);
    until (IncIdxs(idxs, nLBounds, nHBounds) = 0);
  end;
end;

procedure TIocpVariantPacket.SaveToStream(const Stream: TMemoryStream);
var
  PackHeader: TPackHeader;
  b: Byte;
  v: OleVariant;
  pname: string;

  procedure PackOneParam(const ParamName: string; const v: OleVariant); //const VType: Word; const ParamData; const DataSize: Integer);
  var
    b: Byte;
  begin
    b := Byte(Length(ParamName));
    Stream.Write(b, SizeOf(b));
    Stream.Write(Pointer(ParamName)^, b * SizeOf(Char));

    PackVariant(v, Stream);
  end;

begin
  Stream.Clear;

  // 预先写入包头，到最后修正
  FillChar(PackHeader, SizeOf(PackHeader), 0);
  Stream.Write(PackHeader, SizeOf(PackHeader));

  // 写入命令（长度1字节-字符窜序列）
  b := Byte(Length(Self.Cmd));
  Stream.Write(b, SizeOf(b));
  if (b > 0) then
    Stream.Write(Pointer(Self.Cmd)^, b * SizeOf(Char));

  // 写入参数个数（1字节）
  b := Byte(Self.Params.Count);
  Stream.Write(b, SizeOf(b));

  // 写入每个参数数据（参数名长度1字节-参数名字符串序列-参数内容长度4字节-参数内容）
  for pname in Params.Keys do
  begin
    v := Params[pname];
    PackOneParam(pname, v);
  end;

  // 写入参数包尺寸
  PPackHeader(Stream.Memory)^.ParamSize := Stream.Size - SizeOf(TPackHeader);

  if (Self.Data.Size > 0) then
  begin
    // 写入数据包
    Stream.Write(PByte(Self.Data.Memory)^, Self.Data.Size);
  end;

  // 修正整个数据包尺寸
  PPackHeader(Stream.Memory)^.DataSize := Self.Data.Size;

  Stream.Position := 0;
  FPackSize := Stream.Size;
end;

procedure TIocpVariantPacket.LoadFromStream(const Stream: TMemoryStream);
var
  PackHeader: TPackHeader;
  b: Byte;
  i: Integer;

  procedure UnpackOneParam;
  var
    b: Byte;
    pname: string;
    v: OleVariant;
  begin
    // 读取参数名长度1字节
    Stream.Read(b, SizeOf(b));
    SetLength(pname, b);

    // 读取参数名字符串序列
    Stream.Read(Pointer(pname)^, b * SizeOf(Char));
    v := ExtractVariant(Stream);
    Self.Params.Add(pname, v);
  end;

begin
  Self.Cmd := '';
  Self.Params.Clear;
  Self.Data.Clear;

  Stream.Position := 0;

  // 读整个数据包尺寸
  Stream.Read(PackHeader, SizeOf(PackHeader));

  // 检查数据完整性
  if (PackHeader.ParamSize + PackHeader.DataSize + SizeOf(PackHeader) <> Stream.Size) then
  begin
    raise Exception.Create('非法数据包');
    Exit;
  end;

  // 读取命令数据
  Stream.Read(b, SizeOf(b));
  SetLength(Self.FCmd, b);
  Stream.Read(Pointer(Self.FCmd)^, b * SizeOf(Char));

  // 读取参数个数
  Stream.Read(b, SizeOf(b));

  for i := 1 to b do
  begin
    UnpackOneParam;
  end;

  if (PackHeader.DataSize > 0) then
  begin
    Self.Data.SetSize(PackHeader.DataSize);
    // 读取数据包数据
    Stream.Read(PByte(Self.Data.Memory)^, PackHeader.DataSize);
  end;

  FPackSize := Stream.Size;
end;

procedure TIocpVariantPacket.LoadFromBuf(Buf: Pointer; Size: Integer);
var
  Stream: TBufStream;
begin
  Stream := TBufStream.Create(Buf, Size);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.

