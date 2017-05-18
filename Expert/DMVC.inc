{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

 //Basic Version of Compiler Supported
{$IFDEF CONDITIONALEXPRESSIONS}  //Started being defined with D2009
   {$IF CompilerVersion < 21.0} // Before RAD Studio 2010
      {$DEFINE UNSUPPORTED_COMPILER_VERSION}
   {$IFEND}
{$ELSE}
  {$DEFINE UNSUPPORTED_COMPILER_VERSION}
{$ENDIF}

{$IFDEF UNSUPPORTED_COMPILER_VERSION}
  Unsupported Compiler Version (Delphi 2010 or later required!)
{$ENDIF}

//Use namespaces
{$DEFINE USE_NS}



{$DEFINE DELPHI_XE8_DOWN}
{$DEFINE DELPHI_XE8_DOWN}
{$DEFINE DELPHI_XE7_DOWN}
{$DEFINE DELPHI_XE6_DOWN}
{$DEFINE DELPHI_XE5_DOWN}
{$DEFINE DELPHI_XE4_DOWN}
{$DEFINE DELPHI_XE3_DOWN}
{$DEFINE DELPHI_XE2_DOWN}
{$DEFINE DELPHI_XE_DOWN}
{$DEFINE DELPHI_2010_DOWN}

{$IFDEF VER210} // RAD Studio 2010
  {$DEFINE DELPHI_2010}
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE CPUX86}
  {$UNDEF USE_NS}
{$ENDIF VER210}

{$IFDEF VER220} // RAD Studio XE
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE CPUX86}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF USE_NS}
{$ENDIF VER220}

{$IFDEF VER230} // RAD Studio XE2
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE USE_NS}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
{$ENDIF VER230}

{$IFDEF VER240} // RAD Studio XE3
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
{$ENDIF VER240}

{$IFDEF VER250} // RAD Studio XE4
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE USE_NS}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
{$ENDIF VER250}

{$IFDEF VER260} // RAD Studio XE5
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE USE_NS}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
{$ENDIF VER260}

{$IFDEF VER270} // RAD Studio XE6
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE USE_NS}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
{$ENDIF VER270}

{$IFDEF VER280} // RAD Studio XE7
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE7}
  {$DEFINE DELPHI_XE7_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE USE_NS}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
  {$UNDEF DELPHI_XE6_DOWN}
{$ENDIF VER280}

{$IFDEF VER290} // RAD Studio XE8
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE7}
  {$DEFINE DELPHI_XE7_UP}
  {$DEFINE DELPHI_XE8_UP}
  
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE USE_NS}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
  {$UNDEF DELPHI_XE6_DOWN}
  {$UNDEF DELPHI_XE7_DOWN}
{$ENDIF VER290}

{$IFDEF VER300} // RAD Studio 10 Seattle
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE7}
  {$DEFINE DELPHI_XE7_UP}
  {$DEFINE DELPHI_XE8_UP}
  {$DEFINE DELPHIX_SEATTLE_UP}
  {$DEFINE DELPHIX_SEATTLE}
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE USE_NS}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
  {$UNDEF DELPHI_XE6_DOWN}
  {$UNDEF DELPHI_XE7_DOWN}
  {$UNDEF DELPHI_XE8_DOWN}
{$ENDIF VER290}





