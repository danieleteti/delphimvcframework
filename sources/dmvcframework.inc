// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

{
  http://delphi.wikia.com/wiki/CompilerVersion_Constant
}

{ ---- Enable this if you are on DELPHI STARTED EDITION --------- }
{ .$DEFINE STARTEREDITION }
{ ------------ DO NOT CHANGE UNDER THIS LINE -------------------- }

{$IF CompilerVersion >= 27} // XE6
{$DEFINE SYSTEMJSON}
{$DEFINE WEBAPACHEHTTP}
{$DEFINE USEFIREDAC}
{$ELSE}
{$IF CompilerVersion >= 26} // XE5
{$DEFINE USEFIREDAC}
{$ELSE}
{$DEFINE USEDBX} // XE3, XE4
{$ENDIF}
{$ENDIF}

{$IF CompilerVersion >= 28} // XE7
{$DEFINE SYSTEMNETENCODING}
{$DEFINE TOJSON}
{$ENDIF}

{$IF CompilerVersion >= 30} // SEATTLE
{$DEFINE JSONBOOL}
{$ENDIF}

{$IFDEF STARTEREDITION}
{$UNDEF USEFIREDAC}
{$UNDEF USEDBX}
{$ENDIF}

{$IFDEF ANDROID OR IOS}
{$DEFINE MOBILE}
{$ENDIF}

{$IF CompilerVersion >= 24.0} // xe3
{$DEFINE XE3ORBETTER}
{$ENDIF}

{$IF CompilerVersion >= 31} // 10.1 Berlin
{$DEFINE BERLINORBETTER}
{$ENDIF}

{$IFDEF MOBILE}
{$LEGACYIFEND OFF}
{$ENDIF}

