// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

{$IFDEF STARTEREDITION}
{$UNDEF USEFIREDAC}
{$UNDEF USEDBX}
{$ENDIF}

{$IFDEF ANDROID OR IOS}
{$DEFINE MOBILE}
{$ENDIF}

{$IF CompilerVersion <= 29} // XE8
DelphiMVCFramework is compatible with Delphi 10.0 or better
{$ENDIF}

{$IF CompilerVersion >= 30} // 10.0 Seattle
{$DEFINE SEATTLEORBETTER}
{$ENDIF}

{$IF CompilerVersion >= 31} // 10.1 Berlin
{$DEFINE BERLINORBETTER}
{$ENDIF}

{$IF CompilerVersion >= 32} // 10.2 Tokyo
{$DEFINE TOKYOORBETTER}
{$ENDIF}

{$IF CompilerVersion >= 33} // 10.3 Rio
{$DEFINE RIOORBETTER}
{$ENDIF}

{$IF CompilerVersion >= 34} // 10.4 Sydney
{$DEFINE SYDNEYORBETTER}
{$ENDIF}

{$IF CompilerVersion >= 35} // 11 Alexandria
{$DEFINE ALEXANDRIAORBETTER}
{$ENDIF}

{$IF CompilerVersion >= 36} // 12 Athens
{$DEFINE ATHENSORBETTER}
{$ENDIF}

{$IFDEF MOBILE}
{$LEGACYIFEND OFF}
{$ENDIF}

{$IFDEF SEATTLEORBETTER}
{$DEFINE JSONBOOL}
{$ENDIF}

{$IFDEF SEATTLEORBETTER}
{$DEFINE SYSTEMNETENCODING}
{$DEFINE TOJSON}
{$ENDIF}

{$DEFINE SYSTEMJSON}
{$IFNDEF MOBILE}
{$IFNDEF MACOS}
{$DEFINE WEBAPACHEHTTP}
{$ENDIF}
{$ENDIF}
{$DEFINE USEFIREDAC}

{$IF Defined(SYDNEYORBETTER)}
{$DEFINE CUSTOM_MANAGED_RECORDS}
{$ENDIF}

{$UNDEF WEBSTENCILS}
{$IF Defined(ATHENSORBETTER)}
{$IF declared(RTLVersion122) or (RTLVersion >= 37)}
{$DEFINE WEBSTENCILS}
{$ENDIF}
{$ENDIF}