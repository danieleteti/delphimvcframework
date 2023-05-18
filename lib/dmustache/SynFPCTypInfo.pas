/// wrapper around FPC typinfo.pp unit for SynCommons.pas and mORMot.pas
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynFPCTypInfo;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2022 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Alfred Glaenzer.

  Portions created by the Initial Developer are Copyright (C) 2022
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez


  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. if you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. if you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Unit created to avoid polluting the SynCommons.pas/mORMot.pas namespace
  with overloaded typinfo.pp types.

}

interface

{$ifndef FPC}
  'this unit is for FPC only - do not include it in any Delphi project!'
{$endif FPC}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

uses
  SysUtils,
  TypInfo;

{$ifdef FPC_PROVIDE_ATTR_TABLE}
type
  // if you have a compilation error here, your FPC trunk is too old
  // - TTypeData.AttributeTable was introduced in SVN 42356-42411 (2019/07)
  // -> undefine FPC_PROVIDE_ATTR_TABLE in Synopse.inc and recompile
  PFPCAttributeTable = TypInfo.PAttributeTable;
{$endif FPC_PROVIDE_ATTR_TABLE}

{$ifdef HASALIGNTYPEDATA}
function AlignTypeData(p: pointer): pointer; inline;
function AlignTypeDataClean(p: pointer): pointer; inline;
{$else}
type
  AlignTypeData = pointer;
  AlignTypeDataClean = pointer;
{$endif HASALIGNTYPEDATA}


{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
function AlignToPtr(p: pointer): pointer; inline;
function AlignPTypeInfo(p: pointer): pointer; inline;
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
type
  AlignToPtr = pointer;
  AlignPTypeInfo = pointer;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

type
  /// some type definition to avoid inclusion of TypInfo in SynCommons/mORMot.pas
  PFPCInterfaceData = TypInfo.PInterfaceData;
  PFPCVmtMethodParam = TypInfo.PVmtMethodParam;
  PFPCIntfMethodTable = TypInfo.PIntfMethodTable;
  PFPCIntfMethodEntry = TypInfo.PIntfMethodEntry;
{$ifdef FPC_NEWRTTI}
  PFPCRecInitData = TypInfo.PRecInitData;

{$endif FPC_NEWRTTI}

procedure FPCDynArrayClear(var a: Pointer; TypeInfo: Pointer);
procedure FPCFinalizeArray(p: Pointer; TypeInfo: Pointer; elemCount: PtrUInt);
procedure FPCFinalize(Data: Pointer; TypeInfo: Pointer);
procedure FPCRecordCopy(const Source; var Dest; TypeInfo: pointer);
procedure FPCRecordAddRef(var Data; TypeInfo : pointer);


implementation

procedure FPCDynArrayClear(var a: Pointer; TypeInfo: Pointer);
  external name 'FPC_DYNARRAY_CLEAR';
procedure FPCFinalizeArray(p: Pointer; TypeInfo: Pointer; elemCount: PtrUInt);
  external name 'FPC_FINALIZE_ARRAY';
procedure FPCFinalize(Data: Pointer; TypeInfo: Pointer);
  external name 'FPC_FINALIZE';
procedure FPCRecordCopy(const Source; var Dest; TypeInfo: pointer);
  external name 'FPC_COPY';
procedure FPCRecordAddRef(var Data; TypeInfo : pointer);
  external name 'FPC_ADDREF';

{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT} // copied from latest typinfo.pp
function AlignToPtr(p: pointer): pointer;
begin
  result := align(p,sizeof(p));
end;

function AlignTypeData(p: pointer): pointer;
{$packrecords c}
  type
    TAlignCheck = record // match RTTI TTypeInfo definition
      b : byte;    // = TTypeKind
      q : qword;   // = this is where the PTypeData begins
    end;
{$packrecords default}
begin
{$ifdef VER3_0}
  result := Pointer(align(p,SizeOf(Pointer)));
{$else VER3_0}
    result := Pointer(align(p,PtrInt(@TAlignCheck(nil^).q)));
{$endif VER3_0}
  {$ifdef FPC_PROVIDE_ATTR_TABLE}
    inc(PByte(result),SizeOf(PFPCAttributeTable)); // ignore attributes table
    result := Pointer(align(result,PtrInt(@TAlignCheck(nil^).q)));
  {$endif FPC_PROVIDE_ATTR_TABLE}
end;
{$else}
{$ifdef FPC_PROVIDE_ATTR_TABLE}
function AlignTypeData(p: pointer): pointer;
begin
  result := p;
  inc(PByte(result),SizeOf(PFPCAttributeTable)); // ignore attributes table
end;
{$endif FPC_PROVIDE_ATTR_TABLE}
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT} // copied from latest typinfo.pp

function AlignTypeDataClean(p: pointer): pointer;
{$packrecords c}
  type
    TAlignCheck = record // match RTTI TTypeInfo definition
      b : byte;    // = TTypeKind
      q : qword;   // = this is where the PTypeData begins
    end;
{$packrecords default}
begin
  {$ifdef VER3_0}
    result := Pointer(align(p,SizeOf(Pointer)));
  {$else VER3_0}
    result := Pointer(align(p,PtrInt(@TAlignCheck(nil^).q)));
  {$endif VER3_0}
end;

function AlignPTypeInfo(p: pointer): pointer; inline;
{$packrecords c}
  type
    TAlignCheck = record
      b : byte;
      p : pointer;
    end;
{$packrecords default}
begin
  Result := Pointer(align(p,PtrInt(@TAlignCheck(nil^).p)))

end;

{$else}
{$ifdef HASALIGNTYPEDATA}
function AlignTypeDataClean(p: pointer): pointer;
begin
  result := p;
end;
{$endif HASALIGNTYPEDATA}

{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

end.
