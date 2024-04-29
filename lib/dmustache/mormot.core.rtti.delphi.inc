{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   Delphi specific definitions used by mormot.core.rtti.pas implementation
}

type
  AlignToPtr = Pointer;

{$ifdef HASINLINE} // Delphi RTL TypInfo.GetTypeData() is awful on x86_64

function GetTypeData(TypeInfo: pointer): PTypeData;
begin
  // weird code which compiles and inlines best on Delphi Win32 and Win64
  {$ifdef CPU64}
  result := pointer(PtrInt(TypeInfo) + ord(PRttiInfo(TypeInfo)^.RawName[0]) + 2);
  {$else}
  result := TypeInfo;
  inc(PByte(result), ord(PRttiInfo(result)^.RawName[0]) + 2);
  {$endif CPU64}
end;

{$else}

function GetTypeData(TypeInfo: pointer): PTypeData;
asm
        // faster code for oldest Delphi
        movzx   edx, byte ptr [eax].TTypeInfo.Name
        lea     eax, [eax + edx].TTypeInfo.Name[1]
end;

{$endif HASINLINE}

function TRttiInfo.RttiClass: PRttiClass; // for proper inlining below
begin
  if @self <> nil then
    result := pointer(GetTypeData(@self))
  else
    result := nil;
end;

function TRttiInfo.RttiNonVoidClass: PRttiClass;
begin
  result := pointer(GetTypeData(@self))
end;

function TRttiClass.PropCount: integer;
begin
  result := PTypeData(@self)^.PropCount;
end;

function TRttiClass.ParentInfo: PRttiInfo;
begin
  result := pointer(PTypeData(@self)^.ParentInfo);
  if result <> nil then
    result := PPointer(result)^;
end;

function TRttiClass.RttiProps: PRttiProps;
begin
  result := @self;
  if result <> nil then
    with PTypeData(result)^ do
      result := @UnitName[ord(UnitName[0]) + 1];
end;

function GetRttiProps(RttiClass: TClass): PRttiProps;
var
  p: PTypeInfo;
begin
  // code is a bit abstract, but compiles very well
  p := PPointer(PtrInt(RttiClass) + vmtTypeInfo)^;
  if p <> nil then // avoid GPF if no RTTI available for this class
    with PTypeData(@p^.Name[ord(p^.Name[0]) + 1])^ do
      result := @UnitName[ord(UnitName[0]) + 1]
    else
      result := nil;
end;

function TRttiProps.PropCount: integer;
begin
  result := PPropData(@self)^.PropCount;
end;

function TRttiProps.PropList: PRttiProp;
begin
  result := pointer(@PPropData(@self)^.PropList);
end;

function GetRttiProp(C: TClass; out PropInfo: PRttiProp): integer;
var
  p: PTypeInfo;
begin
  if C <> nil then
  begin
    p := PPointer(PtrInt(C) + vmtTypeInfo)^;
    if p <> nil then // avoid GPF if no RTTI available
      with PTypeData(@p^.Name[ord(p^.Name[0]) + 1])^,
           PPropData(@UnitName[ord(UnitName[0]) + 1])^ do
      begin
        PropInfo := @PropList;
        result := PropCount;
        exit;
      end;
  end;
  result := 0;
end;

function TRttiEnumType.EnumBaseType: PRttiEnumType;
begin
  with PTypeData(@self).BaseType^^ do
    result := @Name[ord(Name[0]) + 1];
end;

function TRttiEnumType.SetBaseType: PRttiEnumType;
begin
  with PTypeData(@self).CompType^^ do
    result := @Name[ord(Name[0]) + 1];
end;

function TRttiEnumType.GetEnumNameOrd(Value: cardinal): PShortString;
begin
  if Value <= cardinal(PTypeData(@self).MaxValue) then
  begin
    result := @PTypeData(@self).NameList;
    if Value > 0 then
      repeat
        inc(PByte(result), PByte(result)^ + 1);  // next
        dec(Value);
        if Value = 0 then
          break;
        inc(PByte(result), PByte(result)^ + 1);  // unrolled twice
        dec(Value);
      until Value = 0;
  end
  else
    result := @NULCHAR;
end;

{$ifdef CPUX86} // Delphi is not efficient when inlining code :(

function GetEnumName(aTypeInfo: PRttiInfo; aIndex: integer): PShortString;
asm     // eax=aTypeInfo edx=aIndex
        test    eax, eax
        jz      @0
        cmp     byte ptr [eax], tkEnumeration
        jnz     @0
        movzx   ecx, byte ptr [eax + TTypeInfo.Name]
        mov     eax, [eax + ecx + TTypeData.BaseType + 2]
        mov     eax, [eax]
        movzx   ecx, byte ptr [eax + TTypeInfo.Name]
        cmp     edx, [eax + ecx + TTypeData.MaxValue + 2]
        ja      @0
        lea     eax, [eax + ecx + TTypeData.NameList + 2]
        test    edx, edx
        jz      @z
        push    edx
        shr     edx, 2 // fast by-four scanning
        jz      @1
@4:     movzx   ecx, byte ptr [eax]
        lea     eax, [eax + ecx + 1]
        movzx   ecx, byte ptr [eax]
        lea     eax, [eax + ecx + 1]
        movzx   ecx, byte ptr [eax]
        lea     eax, [eax + ecx + 1]
        movzx   ecx, byte ptr [eax]
        lea     eax, [eax + ecx + 1]
        dec     edx
        jnz     @4
        pop     edx
        and     edx, 3
        jnz     @s
        ret
@1:     pop     edx
@s:     movzx   ecx, byte ptr [eax]
        lea     eax, [eax + ecx + 1] // last 1..3 iterations
        dec     edx
        jnz     @s
@z:     ret
@void:  db      0
@0:     lea     eax, @void
end;

{$else}

function GetEnumName(aTypeInfo: PRttiInfo; aIndex: integer): PShortString;
begin
  if PRttiKind(aTypeInfo)^ = rkEnumeration then
    with GetTypeData(aTypeInfo).BaseType^^ do
      result := PRttiEnumType(@Name[ord(Name[0]) + 1])^.GetEnumNameOrd(aIndex)
  else
    result := @NULCHAR;
end;

{$endif ASMX86}


function TRttiInterfaceTypeData.IntfGuid: PGuid;
begin
  {$ifdef ISDELPHI102} // adapt to latest TypInfo.pas changes
  result := @PTypeData(@self)^.IntfGuid;
  {$else}
  result := @PTypeData(@self)^.Guid;
  {$endif ISDELPHI102}
end;

function TRttiInterfaceTypeData.IntfParent: PRttiInfo;
begin
  result := Pointer(PTypeData(@self)^.IntfParent^);
end;

function InterfaceEntryIsStandard(Entry: PInterfaceEntry): boolean;
begin
  result := Entry^.IOffset <> 0;
end;

function TRttiProp.TypeInfo: PRttiInfo;
begin
  result := pointer(PPropInfo(@self)^.PropType^);
end;

function TRttiProp.GetterIsField: boolean;
begin
  result := PropWrap(PPropInfo(@self)^.GetProc).Kind = ptField;
end;

function TRttiProp.SetterIsField: boolean;
begin
  result := PropWrap(PPropInfo(@self)^.SetProc).Kind = ptField;
end;

function TRttiProp.WriteIsDefined: boolean;
begin
  result := PtrUInt(PPropInfo(@self)^.SetProc) <> 0;
end;

function TRttiProp.IsStored(Instance: TObject): boolean;
begin
  if (PtrUInt(PPropInfo(@self)^.StoredProc) and
     (not PtrUInt($ff))) = 0 then
    result := boolean(PtrUInt(PPropInfo(@self)^.StoredProc))
  else
    result := IsStoredGetter(Instance);
end;

function TRttiProp.IsStoredKind: TRttiPropStored;
begin
  if (PtrUInt(PPropInfo(@self)^.StoredProc) and
     (not PtrUInt($ff))) = 0 then
    if boolean(PtrUInt(PPropInfo(@self)^.StoredProc)) then
      result := rpsTrue
    else
      result := rpsFalse
  else
    result := rpsGetter;
end;

function TRttiProp.IsStoredGetter(Instance: TObject): boolean;
type
  TGetProc = function: boolean of object;
  TGetIndexed = function(Index: integer): boolean of object;
var
  call: TMethod;
begin
  if @self = nil then
    result := true
  else
    with PPropInfo(@self)^ do
    if (PtrUInt(StoredProc) and
       (not PtrUInt($ff))) = 0 then
      result := boolean(PtrUInt(StoredProc))
    else
    begin
      case PropWrap(StoredProc).Kind of
        ptField:
          begin
            result := PBoolean(
              PtrUInt(Instance) + PtrUInt(StoredProc) and $00ffffff)^;
            exit;
          end;
        ptVirtual:
          call.Code := PPointer(
            PPtrUInt(Instance)^ + PtrUInt(StoredProc) and $00ffffff)^;
        else
          call.Code := pointer(StoredProc);
      end;
      call.Data := Instance;
      if Index <> NO_INDEX then
        result := TGetIndexed(call)(Index)
      else
        result := TGetProc(call);
    end;
end;

function TRttiProp.Getter(Instance: TObject; Call: PMethod): TRttiPropCall;
begin
  with PPropInfo(@self)^ do
  begin
    if GetProc = nil then
    begin
      // no 'read' was defined -> try from 'write' field
      if (SetProc <> nil) and
         (PropWrap(SetProc).Kind = ptField) then
      begin
        Call.Data := pointer(
          PtrUInt(Instance) + PtrUInt(SetProc) and $00ffffff);
        result := rpcField;
      end
      else
        result := rpcNone;
      exit;
    end
    else
    case PropWrap(GetProc).Kind of
      ptField:
        begin
          // GetProc is an offset to the instance fields
          Call.Data := pointer(
            PtrUInt(Instance) + PtrUInt(GetProc) and $00ffffff);
          result := rpcField;
          exit;
        end;
      ptVirtual:
        // GetProc is an offset to the class VMT
        if Instance <> nil then // e.g. from GetterCall()
          Call.Code := PPointer(
            PPtrUInt(Instance)^ + PtrUInt(GetProc) and $00ffffff)^;
    else
      // ptStatic: GetProc is the method code itself
      Call.Code := pointer(GetProc);
    end;
    Call.Data := Instance;
    result := rpcMethod;
    if Index <> NO_INDEX then
      result := rpcIndexed;
  end;
end;

function TRttiProp.Setter(Instance: TObject; Call: PMethod): TRttiPropCall;
begin
  with PPropInfo(@self)^ do
  begin
    if SetProc = nil then
    begin
      // no 'write' was defined -> try from 'read' field
      if (GetProc <> nil) and
         (PropWrap(GetProc).Kind = ptField) then
      begin
        Call.Data := pointer(
          PtrUInt(Instance) + PtrUInt(GetProc) and $00ffffff);
        result := rpcField;
      end
      else
        result := rpcNone;
      exit;
    end
    else
    case PropWrap(SetProc).Kind of
      ptField:
        begin
          // SetProc is an offset to the instance fields
          Call.Data := pointer(
            PtrUInt(Instance) + PtrUInt(SetProc) and $00ffffff);
          result := rpcField;
          exit;
        end;
      ptVirtual:
        // SetProc is an offset to the class VMT
        if Instance <> nil then // e.g. from SetterCall()
          Call.Code := PPointer(
            PPtrUInt(Instance)^ + PtrUInt(SetProc) and $00ffffff)^;
    else
      // ptStatic: SetProc is the method code itself
      Call.Code := pointer(SetProc);
    end;
    Call.Data := Instance;
    result := rpcMethod;
    if Index <> NO_INDEX then
      result := rpcIndexed;
  end;
end;

const
  // RawUtf8 is defined as weak system.UTF8String type in mormot.core.base
  UTF8_NAME: string[7] = 'RawUtf8';

function TRttiInfo.Name: PShortString;
begin
  result := pointer(@self);
  if result <> nil then
    if result <> TypeInfo(RawUtf8) then
      result := @RawName   // as stored in RTTI
    else
      result := @UTF8_NAME // instead of 'UTF8String'
  else
    result := @NULCHAR;
end;

function TRttiInfo.RecordSize: PtrInt;
begin
  result := PRecordInfo(GetTypeData(@self))^.RecSize;
end;

procedure TRttiInfo.RecordManagedFields(out Fields: TRttiRecordManagedFields);
var
  nfo: PRecordInfo;
begin
  nfo := pointer(GetTypeData(@self));
  Fields.Size := nfo^.RecSize;
  Fields.Count := nfo^.ManagedFldCount;
  Fields.Fields := @PIntegerArray(@nfo^.ManagedFldCount)[1];
end;

function TRttiInfo.RecordManagedFieldsCount: integer;
begin
  result := PRecordInfo(GetTypeData(@self))^.ManagedFldCount;
end;

{$ifdef HASEXTRECORDRTTI} // read enhanced RTTI available since Delphi 2010

type
  /// map Delphi tkRecord TypeInfo with enhanced RTTI
  TRecordEnhancedTypeData = packed record
    RecSize: cardinal;
    ManagedCount: integer;
    // ManagedFields: array[0..0] of TManagedField;
    NumOps: byte;
    // RecOps: array[0..0] of pointer;
    AllCount: integer; // !!!! may need $RTTI EXPLICIT FIELDS([vcPublic])
    AllFields: array[0..0] of TRecordTypeField; // as defined in TypInfo.pas
  end;

function TRttiInfo.RecordAllFields(out RecSize: PtrInt): TRttiRecordAllFields;
var
  info: ^TRecordEnhancedTypeData;
  p: PRecordTypeField;
  f: PtrInt;
begin
  result := nil; // don't reallocate previous answer
  info := pointer(GetTypeData(@self));
  RecSize := info^.RecSize;
  inc(PByte(info), info^.ManagedCount * SizeOf(TManagedField));
  inc(PByte(info), info^.NumOps * SizeOf(pointer));
  SetLength(result, info^.AllCount);
  p := @info^.AllFields[0];
  for f := 0 to info^.AllCount - 1 do
    begin
      with result[f] do
      begin
        TypeInfo := pointer(p^.Field.TypeRef);
        if TypeInfo = nil then
        begin
          // this field has no RTTI -> we can't trust it for serialization
          result := nil;
          exit;
        end;
        TypeInfo := PPointer(TypeInfo)^;
        Offset := p^.Field.FldOffset;
        Name := @p^.Name;
      end;
      p := pointer(PtrInt(@p^.Name[1]) + ord(p^.Name[0]));
      inc(PByte(p), PWord(p)^); // jump attributes
    end;
end;

{$else}

function TRttiInfo.RecordAllFields(out RecSize: PtrInt): TRttiRecordAllFields;
begin
  RecSize := self.RecordSize;
  result := nil; // extended record information not available before Delphi 2010
end;

{$endif HASEXTRECORDRTTI}

function TRttiInfo.IsQWord: boolean;
begin
  if @self = TypeInfo(QWord) then
    result := true
  else
    {$ifdef UNICODE}
    if Kind = rkInt64 then
      with PHash128Rec(PAnsiChar(@RawName[1]) + ord(RawName[0]))^ do
        result := Lo > Hi // check MinInt64Value>MaxInt64Value
    else
    {$endif UNICODE}
      result := false;
end;

function TRttiInfo.IsBoolean: boolean;
begin
  result := (@self = TypeInfo(boolean)) or
            (@self = TypeInfo(wordbool));
end;

function TRttiInfo.EnumBaseType: PRttiEnumType;
begin
  result := pointer(GetTypeData(@self));
  result := result^.EnumBaseType;
end;

function TRttiInfo.DynArrayItemType: PRttiInfo;
begin
  result := pointer(GetTypeData(@self)^.elType);
  if result <> nil then // nil e.g. for TIntegerDynArray or T*ObjArray
    result := PPointer(result)^;
end;

function TRttiInfo.DynArrayItemTypeExtended: PRttiInfo;
begin
  with GetTypeData(@self)^ do
  begin
    result := pointer(elType);
    if result <> nil then // nil e.g. for TIntegerDynArray or T*ObjArray
      result := PPointer(result)^;
    {$ifdef HASDYNARRAYTYPE}
    if result = nil then
    begin
      // try the second slot, which may be set even for unmanaged types
      result := pointer(elType2);
      if result <> nil then
        result := PPointer(result)^;
    end;
    {$endif HASDYNARRAYTYPE}
  end;
end;

function TRttiInfo.DynArrayItemType(out aDataSize: PtrInt): PRttiInfo;
begin
  with GetTypeData(@self)^ do
  begin
    aDataSize := elSize;
    result := pointer(elType);
    if result <> nil then
      result := PPointer(result)^;
  end;
end;

function TRttiInfo.ArrayItemType(out aDataCount, aDataSize: PtrInt): PRttiInfo;
var
  nfo: PArrayInfo;
begin
  // nfo^.DimCount=1 is not tested explicitly -> assume single dimension array
  nfo := pointer(GetTypeData(@self));
  aDataCount := nfo^.ElCount;
  aDataSize := nfo^.ArraySize;
  result := pointer(nfo^.ArrayType);
  if result <> nil then
    result := PPointer(result)^;
end;

function TRttiInfo.ArraySize: PtrInt;
begin
  result := PArrayInfo(GetTypeData(@self))^.ArraySize;
end;

function GetPublishedMethods(Instance: TObject;
  out Methods: TPublishedMethodInfoDynArray; aClass: TClass): integer;

  procedure AddParentsFirst(C: TClass);
  type
    TMethodInfo = packed record
      Len: Word;
      Addr: Pointer;
      Name: ShortString;
    end;
  var
    Table: PWordArray;
    M: ^TMethodInfo;
    i: integer;
  begin
    if C = nil then
      exit;
    AddParentsFirst(GetClassParent(C)); // put children published methods afterward
    Table := PPointer(PtrUInt(C) + PtrUInt(vmtMethodTable))^;
    if Table = nil then
      exit;
    SetLength(Methods, result + Table^[0]);
    M := @Table^[1];
    for i := 1 to Table^[0] do  // Table^[0] = methods count
      with Methods[result] do
      begin
        ShortStringToAnsi7String(M^.Name, Name);
        Method.Data := Instance;
        Method.Code := M^.Addr;
        inc(PByte(M), M^.Len);
        inc(result);
      end;
  end;

begin
  result := 0;
  if aClass <> nil then
    AddParentsFirst(aClass)
  else if Instance <> nil then
    AddParentsFirst(PPointer(Instance)^); // use recursion for adding
end;

{$ifndef ISDELPHI2010} // not defined on Delphi 7/2007/2009
type
  TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall);
{$endif ISDELPHI2010}

/// fake TTypeInfo RTTI used for TGuid/THash128... on Delphi 7/2007
{$ifdef HASNOSTATICRTTI}
                       
type
  // enough Delphi RTTI for TRttiInfo.RecordManagedFields
  TFakeTypeInfo = packed record
    Kind: TTypeKind;
    case integer of
      5: (
        Name5: string[5];
        RecSize5: cardinal;
        ManagedCount5: integer);
      8: (
        Name8: string[8];
        RecSize8: cardinal;
        ManagedCount8: integer);
      9: (
        Name9: string[9];
        RecSize9: cardinal;
        ManagedCount9: integer);
  end;
const
  _TGUID: TFakeTypeInfo = (    // stored in PT_INFO[ptGuid]
    Kind: tkRecord;
    Name5: 'TGUID';
    RecSize5: SizeOf(TGUID);
    ManagedCount5: 0);

  _THASH128: TFakeTypeInfo = ( // stored in PT_INFO[ptHash128]
    Kind: tkRecord; // note: is a tkArray when HASNOSTATICRTTI
    Name8: 'THash128';
    RecSize8: SizeOf(THash128);
    ManagedCount8: 0);

  _THASH256: TFakeTypeInfo = ( // stored in PT_INFO[ptHash256]
    Kind: tkRecord;
    Name8: 'THash256';
    RecSize8: SizeOf(THash256);
    ManagedCount8: 0);

  _THASH512: TFakeTypeInfo = ( // stored in PT_INFO[ptHash512]
    Kind: tkRecord;
    Name8: 'THash512';
    RecSize8: SizeOf(THash512);
    ManagedCount8: 0);

  _PUTF8CHAR: TFakeTypeInfo = ( // stored in PT_INFO[ptPUtf8Char]
    Kind: tkRecord;  // don't mess with ordinals - just a record with a pointer
    Name9: 'PUtf8Char';
    RecSize9: SizeOf(pointer);
    ManagedCount9: 0);

{$endif HASNOSTATICRTTI}


procedure TGetRttiInterface.AddMethodsFromTypeInfo(aInterface: PTypeInfo);
var
  mn, an: integer;
  ancestor: PTypeInfo;
  kind: TMethodKind;
  cc: TCallConv;
  flags: ^TParamFlags;
  name: PShortString;
  p: PByte;
  pw: PWord absolute p;
  pi: PTypeData absolute p;
  ps: PShortString absolute p;

  procedure AddArgFromRtti;
  var
    pp: ^PPRttiInfo absolute p;
    argtypnfo: PRttiInfo;
    argtypnam: PShortString;
    {$ifdef HASNOSTATICRTTI}
    rc: TRttiCustom;
    {$endif HASNOSTATICRTTI}
  begin
    argtypnam := ps;
    ps := @ps^[ord(ps^[0]) + 1];
    argtypnfo := pp^^;
    if pp^ = nil then
    begin
      {$ifdef HASNOSTATICRTTI} // detect e.g. TGuid/THash128 -> fake TypeInfo()
      rc := Rtti.FindName(argtypnam^, []);
      if rc <> nil then
        argtypnfo := rc.Info
      else
      {$endif HASNOSTATICRTTI}
        RaiseError('"%: %" parameter has no RTTI', [name^, argtypnam^]);
    end;
    inc(pp);
    AddArgument(name, argtypnam, argtypnfo, flags^);
  end;

begin
  pi := GetTypeData(aInterface);
  if IdemPropName(pi^.IntfUnit, 'System') then
    exit;
  if Definition.Name = '' then
  begin
    ShortStringToAnsi7String(aInterface^.Name, Definition.Name);
    ShortStringToAnsi7String(pi^.IntfUnit, Definition.UnitName);
    Definition.Guid := pi^.Guid;
  end;
  ancestor := pi^.IntfParent^;
  if ancestor <> nil then
  begin
    AddMethodsFromTypeInfo(ancestor); // recursive call of parents
    inc(Level);
  end;
  p := @pi^.IntfUnit[ord(pi^.IntfUnit[0]) + 1];
  mn := pw^;
  inc(pw);
  if (pw^ = $ffff) or
     (mn = 0) then
    exit; // no method
  inc(pw);
  SetLength(Definition.Methods, MethodCount + mn);
  repeat
    name := ps;
    ps := @ps^[ord(ps^[0]) + 1];
    kind := TMethodKind(p^);
    inc(p);
    cc := TCallConv(p^);
    inc(p);
    an := p^;
    inc(p);
    AddMethod(name^, an, kind);
    if cc <> ccReg then
      RaiseError('unsupported %', [GetEnumName(TypeInfo(TCallConv), ord(cc))^]);
    while an > 0 do
    begin
      flags := pointer(p);
      inc(p, SizeOf(flags^));
      name := ps;
      ps := @ps^[ord(ps^[0]) + 1];
      AddArgFromRtti;
      {$ifdef ISDELPHIXE}
      inc(p, pw^); // skip custom attributes
      {$endif ISDELPHIXE}
      dec(an);
    end;
    name := nil;
    if kind = mkFunction then
      AddArgFromRtti;
    {$ifdef ISDELPHIXE}
    inc(p, pw^); // skip custom attributes
    {$endif ISDELPHIXE}
    dec(mn);
  until mn = 0;
  CurrentMethod := nil;
end;


const
  // gather rk* to reduce number of TRttiCustomListPairs hash slots in memory
  RK_TOSLOT_MAX = 12;
  RK_TOSLOT: array[TRttiKind] of byte = (
    0,  // rkUnknown
    1,  // rkInteger
    2,  // rkChar
    3,  // rkEnumeration
    4,  // rkFloat
    0,  // rkSString
    5,  // rkSet
    6,  // rkClass
    0,  // rkMethod
    7,  // rkWChar
    8,  // rkLString
    7,  // rkWString
    9,  // rkVariant
    2,  // rkArray
    10, // rkRecord
    9,  // rkInterface
    11, // rkInt64
    12  // rkDynArray
    {$ifdef UNICODE} ,
    7,  // rkUString
    0,  // rkClassRef
    0,  // rkPointer
    0,  // rkProcedure
    0   // rkMRecord
    {$endif UNICODE});

