(* *****************************************************************************
  * Copyright (C) 1994-2008 Lua.org, PUC-Rio.  All rights reserved.
  *
  * Permission is hereby granted, free of charge, to any person obtaining
  * a copy of this software and associated documentation files (the
  * "Software"), to deal in the Software without restriction, including
  * without limitation the rights to use, copy, modify, merge, publish,
  * distribute, sublicense, and/or sell copies of the Software, and to
  * permit persons to whom the Software is furnished to do so, subject to
  * the following conditions:
  *
  * The above copyright notice and this permission notice shall be
  * included in all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ***************************************************************************** *)

{$IFDEF FPC}
{$MODE OBJFPC}{$M+}
{$DEFINE HAVEINLINE}
{$ENDIF}

unit LuaBind.Intf;

interface

uses classes;

type
  size_t = Cardinal;
  Psize_t = ^size_t;

  {$IFNDEF FPC}

  PtrInt = Integer;

  {$ENDIF}


const

  {$IFDEF UNIX}

  LUA_LIB = 'liblua5.1.so';

  {$ELSE}

  LUA_LIB = 'lua5.1.dll';

  {$ENDIF}


const
  LUA_IDSIZE = 60;
  LUAL_BUFFERSIZE = 512;

  (*
    ** $Id: lua.h,v 1.218.1.5 2008/08/06 13:30:12 roberto Exp $
    ** Lua - An Extensible Extension Language
    ** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
    ** See Copyright Notice at the end of this file
  *)


  // #include "luaconf.h"

const
  LUA_VERSION = 'Lua 5.1';
  LUA_RELEASE = 'Lua 5.1.4';
  LUA_VERSION_NUM = 501;
  LUA_COPYRIGHT = 'Copyright (C) 1994-2008 Lua.org, PUC-Rio';
  LUA_AUTHORS = 'R. Ierusalimschy, L. H. de Figueiredo & W. Celes';

  (* mark for precompiled code (`<esc>Lua') *)
  LUA_SIGNATURE = #33'Lua';

  (* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = - 1;

  (*
    ** pseudo-indices
  *)
  LUA_REGISTRYINDEX = - 10000;
  LUA_ENVIRONINDEX = - 10001;
  LUA_GLOBALSINDEX = - 10002;
function lua_upvalueindex(i: Integer): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}


(* thread status; 0 is OK *)
const
  LUA_YIELD_ = 1;
  LUA_ERRRUN = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM = 4;
  LUA_ERRERR = 5;

type
  Plua_State = ^lua_State;

  lua_State = record

  end;

  lua_CFunction = function(L: Plua_State): Integer; cdecl;

  (*
    ** functions that read/write blocks when loading/dumping Lua chunks
  *)
  lua_Reader = function(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: Psize_t; ud: Pointer): Integer; cdecl;

  (*
    ** prototype for memory-allocation functions
  *)
  lua_Alloc = function(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;

  (*
    ** basic types
  *)
const
  LUA_TNONE = - 1;

  LUA_TNIL = 0;
  LUA_TBOOLEAN = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER = 3;
  LUA_TSTRING = 4;
  LUA_TTABLE = 5;
  LUA_TFUNCTION = 6;
  LUA_TUSERDATA = 7;
  LUA_TTHREAD = 8;

  (* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

  (*
    ** generic extra include file
  *)
  // #if defined(LUA_USER_H)
  // #include LUA_USER_H
  // #endif

type
  (* type of numbers in Lua *)
  lua_Number = Double; // LUA_NUMBER

  (* type for integer functions *)
  lua_Integer = PtrInt; // LUA_INTEGER

  (*
    ** state manipulation
  *)
function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_State; cdecl;
  external LUA_LIB name 'lua_newstate';
procedure lua_close(L: Plua_State); cdecl; external LUA_LIB name 'lua_close';
function lua_newthread(L: Plua_State): Plua_State; cdecl; external LUA_LIB name 'lua_newthread';

function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl;
  external LUA_LIB name 'lua_atpanic';

(*
  ** basic stack manipulation
*)
function lua_gettop(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_gettop';
procedure lua_settop(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_settop';
procedure lua_pushvalue(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_pushvalue';
procedure lua_remove(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_remove';
procedure lua_insert(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_insert';
procedure lua_replace(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_replace';
function lua_checkstack(L: Plua_State; sz: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_checkstack';

procedure lua_xmove(from: Plua_State; to_: Plua_State; n: Integer); cdecl;
  external LUA_LIB name 'lua_xmove';

(*
  ** access functions (stack -> C)
*)

function lua_isnumber(L: Plua_State; idx: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_isnumber';
function lua_isstring(L: Plua_State; idx: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_isstring';
function lua_iscfunction(L: Plua_State; idx: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_iscfunction';
function lua_isuserdata(L: Plua_State; idx: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_isuserdata';
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_type';
function lua_typename(L: Plua_State; tp: Integer): PAnsiChar; cdecl;
  external LUA_LIB name 'lua_typename';

function lua_equal(L: Plua_State; idx1: Integer; idx2: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_equal';
function lua_rawequal(L: Plua_State; idx1: Integer; idx2: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_rawequal';
function lua_lessthan(L: Plua_State; idx1: Integer; idx2: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_lessthan';

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; cdecl;
  external LUA_LIB name 'lua_tonumber';
function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer; cdecl;
  external LUA_LIB name 'lua_tointeger';
function lua_toboolean(L: Plua_State; idx: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_toboolean';
function lua_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PAnsiChar; cdecl;
  external LUA_LIB name 'lua_tolstring';
function lua_objlen(L: Plua_State; idx: Integer): size_t; cdecl; external LUA_LIB name 'lua_objlen';
function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl;
  external LUA_LIB name 'lua_tocfunction';
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl;
  external LUA_LIB name 'lua_touserdata';
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl;
  external LUA_LIB name 'lua_tothread';
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl;
  external LUA_LIB name 'lua_topointer';

(*
  ** push functions (C -> stack)
*)
procedure lua_pushnil(L: Plua_State); cdecl; external LUA_LIB name 'lua_pushnil';
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl;
  external LUA_LIB name 'lua_pushnumber';
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl;
  external LUA_LIB name 'lua_pushinteger';
procedure lua_pushlstring(L: Plua_State; const s: PAnsiChar; len: size_t); cdecl;
  external LUA_LIB name 'lua_pushlstring';
procedure lua_pushstring(L: Plua_State; const s: PAnsiChar); cdecl;
  external LUA_LIB name 'lua_pushstring';
function lua_pushvfstring(L: Plua_State; const fmt: PAnsiChar): PAnsiChar; varargs; cdecl;
  external LUA_LIB name 'lua_pushvfstring';
function lua_pushfstring(L: Plua_State; const fmt: PAnsiChar): PAnsiChar; varargs; cdecl;
  external LUA_LIB name 'lua_pushfstring';
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl;
  external LUA_LIB name 'lua_pushcclosure';
procedure lua_pushboolean(L: Plua_State; b: Integer); cdecl;
  external LUA_LIB name 'lua_pushboolean';
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl;
  external LUA_LIB name 'lua_pushlightuserdata';
function lua_pushthread(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_pushthread';

(*
  ** get functions (Lua -> stack)
*)
procedure lua_gettable(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_gettable';
procedure lua_getfield(L: Plua_State; idx: Integer; const k: PAnsiChar); cdecl;
  external LUA_LIB name 'lua_getfield';
procedure lua_rawget(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_rawget';
procedure lua_rawgeti(L: Plua_State; idx: Integer; n: Integer); cdecl;
  external LUA_LIB name 'lua_rawgeti';
procedure lua_createtable(L: Plua_State; narr: Integer; nrec: Integer); cdecl;
  external LUA_LIB name 'lua_createtable';
function lua_newuserdata(L: Plua_State; sz: size_t): Pointer; cdecl;
  external LUA_LIB name 'lua_newuserdata';
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_getmetatable';
procedure lua_getfenv(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_getfenv';

(*
  ** set functions (stack -> Lua)
*)
procedure lua_settable(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_settable';
procedure lua_setfield(L: Plua_State; idx: Integer; const k: PAnsiChar); cdecl;
  external LUA_LIB name 'lua_setfield';
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_rawset';
procedure lua_rawseti(L: Plua_State; idx: Integer; n: Integer); cdecl;
  external LUA_LIB name 'lua_rawseti';
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_setmetatable';
function lua_setfenv(L: Plua_State; idx: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_setfenv';

(*
  ** `load' and `call' functions (load and run Lua code)
*)
procedure lua_call(L: Plua_State; nargs: Integer; nresults: Integer); cdecl;
  external LUA_LIB name 'lua_call';
function lua_pcall(L: Plua_State; nargs: Integer; nresults: Integer; errfunc: Integer): Integer;
  cdecl; external LUA_LIB name 'lua_pcall';
function lua_cpcall(L: Plua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl;
  external LUA_LIB name 'lua_cpcall';
function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname: PAnsiChar)
  : Integer; cdecl; external LUA_LIB name 'lua_load';

function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer): Integer; cdecl;
  external LUA_LIB name 'lua_dump';

(*
  ** coroutine functions
*)
function lua_yield(L: Plua_State; nresults: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_yield';
function lua_resume(L: Plua_State; narg: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_resume';
function lua_status(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_status';

(*
  ** garbage-collection function and options
*)

const
  LUA_GCSTOP = 0;
  LUA_GCRESTART = 1;
  LUA_GCCOLLECT = 2;
  LUA_GCCOUNT = 3;
  LUA_GCCOUNTB = 4;
  LUA_GCSTEP = 5;
  LUA_GCSETPAUSE = 6;
  LUA_GCSETSTEPMUL = 7;

function lua_gc(L: Plua_State; what: Integer; data: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_gc';

(*
  ** miscellaneous functions
*)

function lua_error(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_error';

function lua_next(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_next';

procedure lua_concat(L: Plua_State; n: Integer); cdecl; external LUA_LIB name 'lua_concat';

function lua_getallocf(L: Plua_State; var ud: Pointer): lua_Alloc; cdecl;
  external LUA_LIB name 'lua_getallocf';
procedure lua_setallocf(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl;
  external LUA_LIB name 'lua_setallocf ';

(*
  ** ===============================================================
  ** some useful macros
  ** ===============================================================
*)

procedure lua_pop(L: Plua_State; n: Integer); {$IFDEF HAVEINLINE}inline; {$ENDIF}


procedure lua_newtable(L: Plua_State); {$IFDEF HAVEINLINE}inline; {$ENDIF}


procedure lua_register(L: Plua_State; n: PAnsiChar; f: lua_CFunction); {$IFDEF HAVEINLINE}inline;

{$ENDIF}

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction); {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_strlen(L: Plua_State; i: Integer): size_t; cdecl; external LUA_LIB name 'lua_objlen';
function lua_isfunction(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_istable(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_islightuserdata(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;

{$ENDIF}

function lua_isnil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_isboolean(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_isthread(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_isnone(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_isnoneornil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


procedure lua_pushliteral(L: Plua_State; const s: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}


procedure lua_setglobal(L: Plua_State; s: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}


procedure lua_getglobal(L: Plua_State; const s: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_tostring(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline; {$ENDIF}


(*
  ** compatibility macros and functions
*)

function lua_open: Plua_State; cdecl; external LUA_LIB name 'luaL_newstate';
procedure lua_getregistry(L: Plua_State); {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_getgccount(L: Plua_State): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}


type
  lua_Chunkreader = type lua_Reader;
  lua_Chunkwriter = type lua_Writer;

  (* hack *)
procedure lua_setlevel(from: Plua_State; to_: Plua_State); cdecl;
  external LUA_LIB name 'lua_setlevel ';

(*
  ** {======================================================================
  ** Debug API
  ** =======================================================================
*)

(*
  ** Event codes
*)
const
  LUA_HOOKCALL = 0;
  LUA_HOOKRET = 1;
  LUA_HOOKLINE = 2;
  LUA_HOOKCOUNT = 3;
  LUA_HOOKTAILRET = 4;

  (*
    ** Event masks
  *)
  LUA_MASKCALL = (1 shl LUA_HOOKCALL);
  LUA_MASKRET = (1 shl LUA_HOOKRET);
  LUA_MASKLINE = (1 shl LUA_HOOKLINE);
  LUA_MASKCOUNT = (1 shl LUA_HOOKCOUNT);
  LUA_MASKTAILRET = (1 shl LUA_HOOKTAILRET);

type
  Plua_Debug = ^lua_Debug;

  lua_Debug = record
    event: Integer;
    name: PAnsiChar; (* (n) *)
    namewhat: PAnsiChar; (* (n) `global', `local', `field', `method' *)
    what: PAnsiChar; (* (S) `Lua', `C', `main', `tail' *)
    source: PAnsiChar; (* (S) *)
    currentline: Integer; (* (l) *)
    nups: Integer; (* (u) number of upvalues *)
    linedefined: Integer; (* (S) *)
    lastlinedefined: Integer; (* (S) *)
    short_src: array [0 .. LUA_IDSIZE - 1] of AnsiChar; (* (S) *)
    (* private part *)
    i_ci: Integer; (* active function *)
  end; (* activation record *)

  (* Functions to be called by the debuger in specific events *)
  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl;
  external LUA_LIB name 'lua_getstack';
function lua_getinfo(L: Plua_State; const what: PAnsiChar; ar: Plua_Debug): Integer; cdecl;
  external LUA_LIB name 'lua_getinfo';
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl;
  external LUA_LIB name 'lua_getlocal';
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl;
  external LUA_LIB name 'lua_setlocal';
function lua_getupvalue(L: Plua_State; funcindex: Integer; n: Integer): PAnsiChar; cdecl;
  external LUA_LIB name 'lua_getupvalue';
function lua_setupvalue(L: Plua_State; funcindex: Integer; n: Integer): PAnsiChar; cdecl;
  external LUA_LIB name 'lua_setupvalue';

function lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer): Integer; cdecl;
  external LUA_LIB name 'lua_sethook';
function lua_gethook(L: Plua_State): lua_Hook; cdecl; external LUA_LIB name 'lua_gethook';
function lua_gethookmask(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_gethookmask';
function lua_gethookcount(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_gethookcount';

(* }====================================================================== *)

(*
  ** $Id: lauxlib.h,v 1.88.1.1 2007/12/27 13:02:25 roberto Exp $
  ** Auxiliary functions for building Lua libraries
  ** See Copyright Notice in lua.h
*)

// #if defined(LUA_COMPAT_GETN)
// LUALIB_API int (luaL_getn) (lua_State *L, int t);
// LUALIB_API void (luaL_setn) (lua_State *L, int t, int n);
// #else
function luaL_getn(L: Plua_State; idx: Integer): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}
// #define luaL_setn(L,i,j)        ((void)0)  (* no op! *)
// #endif

// #if defined(LUA_COMPAT_OPENLIB)
// #define luaI_openlib	luaL_openlib
// #endif

(* extra error code for `luaL_load' *)
const
  LUA_ERRFILE = LUA_ERRERR + 1;

type
  PluaL_Reg = ^luaL_Reg;

  luaL_Reg = record
    name: PAnsiChar;
    func: lua_CFunction;
  end;

procedure luaI_openlib(L: Plua_State; const libname: PAnsiChar; const lr: PluaL_Reg; nup: Integer);
  cdecl; external LUA_LIB name 'luaI_openlib';
procedure luaL_register(L: Plua_State; const libname: PAnsiChar; const lr: PluaL_Reg); cdecl;
  external LUA_LIB name 'luaL_register';
function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl;
  external LUA_LIB name 'luaL_getmetafield';
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl;
  external LUA_LIB name 'luaL_callmeta';
function luaL_typerror(L: Plua_State; narg: Integer; const tname: PAnsiChar): Integer; cdecl;
  external LUA_LIB name 'luaL_typerror';
function luaL_argerror(L: Plua_State; numarg: Integer; const extramsg: PAnsiChar): Integer; cdecl;
  external LUA_LIB name 'luaL_argerror';
function luaL_checklstring(L: Plua_State; numarg: Integer; len: Psize_t): PAnsiChar; cdecl;
  external LUA_LIB name 'luaL_checklstring';
function luaL_optlstring(L: Plua_State; numarg: Integer; const def: PAnsiChar; len: Psize_t)
  : PAnsiChar; cdecl; external LUA_LIB name 'luaL_optlstring';
function luaL_checknumber(L: Plua_State; numarg: Integer): lua_Number; cdecl;
  external LUA_LIB name 'luaL_checknumber';
function luaL_optnumber(L: Plua_State; narg: Integer; def: lua_Number): lua_Number; cdecl;
  external LUA_LIB name 'luaL_optnumber';
function luaL_checkinteger(L: Plua_State; numarg: Integer): lua_Integer; cdecl;
  external LUA_LIB name 'luaL_checkinteger';
function luaL_optinteger(L: Plua_State; narg: Integer; def: lua_Integer): lua_Integer; cdecl;
  external LUA_LIB name 'luaL_optinteger';
procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PAnsiChar); cdecl;
  external LUA_LIB name 'luaL_checkstack';
procedure luaL_checktype(L: Plua_State; narg: Integer; t: Integer); cdecl;
  external LUA_LIB name 'luaL_checktype';
procedure luaL_checkany(L: Plua_State; narg: Integer); cdecl; external LUA_LIB name 'luaL_checkany';
function luaL_newmetatable(L: Plua_State; const tname: PAnsiChar): Integer; cdecl;
  external LUA_LIB name 'luaL_newmetatable';
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PAnsiChar): Pointer; cdecl;
  external LUA_LIB name 'luaL_checkudata';
procedure luaL_where(L: Plua_State; lvl: Integer); cdecl; external LUA_LIB name 'luaL_where';
function luaL_error(L: Plua_State; const fmt: PAnsiChar): Integer; varargs; cdecl;
  external LUA_LIB name 'luaL_error';
function luaL_checkoption(L: Plua_State; narg: Integer; const def: PAnsiChar; const lst: PPAnsiChar)
  : Integer; cdecl; external LUA_LIB name 'luaL_checkoption';
function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl; external LUA_LIB name 'luaL_ref';
procedure luaL_unref(L: Plua_State; t: Integer; ref: Integer); cdecl;
  external LUA_LIB name 'luaL_unref';
function luaL_loadfile(L: Plua_State; const filename: PAnsiChar): Integer; cdecl;
  external LUA_LIB name 'luaL_loadfile';
function luaL_loadbuffer(L: Plua_State; const buff: PAnsiChar; sz: size_t; const name: PAnsiChar)
  : Integer; cdecl; external LUA_LIB name 'luaL_loadbuffer';
function luaL_loadstring(L: Plua_State; const s: PAnsiChar): Integer; cdecl;
  external LUA_LIB name 'luaL_loadstring';
function luaL_newstate: Plua_State; cdecl; external LUA_LIB name 'luaL_newstate:';
function luaL_gsub(L: Plua_State; const s: PAnsiChar; const p: PAnsiChar; const r: PAnsiChar)
  : PAnsiChar; cdecl; external LUA_LIB name 'luaL_gsub';
function luaL_findtable(L: Plua_State; idx: Integer; const fname: PAnsiChar; szhint: Integer)
  : PAnsiChar; cdecl; external LUA_LIB name 'luaL_findtable';

(*
  ** ===============================================================
  ** some useful macros
  ** ===============================================================
*)

function luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; const extramsg: PAnsiChar)
  : Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_checkstring(L: Plua_State; n: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_optstring(L: Plua_State; n: Integer; d: PAnsiChar): PAnsiChar;

{$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_checkint(L: Plua_State; n: Integer): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_optint(L: Plua_State; n: Integer; d: lua_Integer): Integer; {$IFDEF HAVEINLINE}inline;

{$ENDIF}

function luaL_checklong(L: Plua_State; n: Integer): LongInt; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_optlong(L: Plua_State; n: Integer; d: lua_Integer): LongInt;

{$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_typename(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_dofile(L: Plua_State; fn: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function luaL_dostring(L: Plua_State; s: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


procedure luaL_getmetatable(L: Plua_State; n: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}
// #define luaL_opt(L,f,n,d) (lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))

(*
  ** {======================================================
  ** Generic Buffer manipulation
  ** =======================================================
*)

type
  PluaL_Buffer = ^luaL_Buffer;

  luaL_Buffer = record
    p: PAnsiChar; (* current position in buffer *)
    lvl: Integer; (* number of strings in the stack (level) *)
    L: Plua_State;
    buffer: array [0 .. LUAL_BUFFERSIZE - 1] of AnsiChar;
  end;

  // #define luaL_addchar(B,c) \
  // ((void)((B)->p < ((B)->buffer+LUAL_BUFFERSIZE) || luaL_prepbuffer(B)), \
  // (*(B)->p++ = (char)(c)))
  //
  // (* compatibility only *)
  // #define luaL_putchar(B,c) luaL_addchar(B,c)
  //
  // #define luaL_addsize(B,n) ((B)->p += (n))

procedure luaL_buffinit(L: Plua_State; b: PluaL_Buffer); cdecl;
  external LUA_LIB name 'luaL_buffinit';
function luaL_prepbuffer(b: PluaL_Buffer): PAnsiChar; cdecl;
  external LUA_LIB name 'luaL_prepbuffer';
procedure luaL_addlstring(b: PluaL_Buffer; const s: PAnsiChar; L: size_t); cdecl;
  external LUA_LIB name 'luaL_addlstring';
procedure luaL_addstring(b: PluaL_Buffer; const s: PAnsiChar); cdecl;
  external LUA_LIB name 'luaL_addstring';
procedure luaL_addvalue(b: PluaL_Buffer); cdecl; external LUA_LIB name 'luaL_addvalue';
procedure luaL_pushresult(b: PluaL_Buffer); cdecl; external LUA_LIB name 'luaL_pushresult';

(* }====================================================== *)

(* compatibility with ref system *)

(* pre-defined references *)
const
  LUA_NOREF = - 2;
  LUA_REFNIL = - 1;

  // #define lua_ref(L,lock) ((lock) ? luaL_ref(L, LUA_REGISTRYINDEX) : \
  // (lua_pushstring(L, "unlocked references are obsolete"), lua_error(L), 0))
  //
  // #define lua_unref(L,ref)        luaL_unref(L, LUA_REGISTRYINDEX, (ref))
  //
  // #define lua_getref(L,ref)       lua_rawgeti(L, LUA_REGISTRYINDEX, (ref))

  (*
    ** $Id: lualib.h,v 1.36.1.1 2007/12/27 13:02:25 roberto Exp $
    ** Lua standard libraries
    ** See Copyright Notice in lua.h
  *)

  (* Key to file-handle type *)
const
  LUA_FILEHANDLE = 'FILE*';
  LUA_COLIBNAME = 'coroutine';
  LUA_TABLIBNAME = 'table';
  LUA_IOLIBNAME = 'io';
  LUA_OSLIBNAME = 'os';
  LUA_STRLIBNAME = 'string';
  LUA_MATHLIBNAME = 'math';
  LUA_DBLIBNAME = 'debug';
  LUA_LOADLIBNAME = 'package';

function luaopen_base(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_base';
function luaopen_table(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_table';
function luaopen_io(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_io';
function luaopen_os(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_os';
function luaopen_string(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_string';
function luaopen_math(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_math';
function luaopen_debug(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_debug';
function luaopen_package(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_package';

(* open all previous libraries *)
procedure luaL_openlibs(L: Plua_State); cdecl; external LUA_LIB name 'luaL_openlibs';

// usefull
function lua_app_alloc(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;
function lua_processsor_loadstream(L: Plua_State; stream: TStream; chunkname: PAnsiChar): Integer;
function lua_processsor_loadfile(L: Plua_State; const filename: string;
  chunkname: PAnsiChar): Integer;
function lua_processsor_dofile(L: Plua_State; const filename: string; chunkname: PAnsiChar)
  : Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


function lua_processsor_dostream(L: Plua_State; stream: TStream; chunkname: PAnsiChar): Boolean;

{$IFDEF HAVEINLINE}inline; {$ENDIF}


implementation

uses
  sysutils, ansistrings;

procedure lua_pop(L: Plua_State; n: Integer); {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  lua_settop(L, - (n) - 1)
end;

procedure lua_newtable(L: Plua_State); {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  lua_createtable(L, 0, 0)
end;

procedure lua_register(L: Plua_State; n: PAnsiChar; f: lua_CFunction); {$IFDEF HAVEINLINE}inline;

{$ENDIF}

begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction); {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  lua_pushcclosure(L, f, 0)
end;

function lua_isfunction(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;

{$ENDIF}

begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_setglobal(L: Plua_State; s: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  lua_setfield(L, LUA_GLOBALSINDEX, s)
end;

procedure lua_getglobal(L: Plua_State; const s: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  lua_getfield(L, LUA_GLOBALSINDEX, s);
end;

function lua_tostring(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_tolstring(L, i, nil);
end;

procedure lua_pushliteral(L: Plua_State; const s: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}
begin
  lua_pushlstring(L, s, AnsiStrings.StrLen(s))
end;

procedure lua_getregistry(L: Plua_State); {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

function lua_getgccount(L: Plua_State): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_gc(L, LUA_GCCOUNT, 0);
end;

function luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; const extramsg: PAnsiChar)
  : Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := cond or (luaL_argerror(L, numarg, extramsg) <> 0)
end;

function luaL_checkstring(L: Plua_State; n: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := luaL_checklstring(L, n, nil);
end;

function luaL_optstring(L: Plua_State; n: Integer; d: PAnsiChar): PAnsiChar;

{$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := luaL_optlstring(L, n, d, nil)
end;

function luaL_getn(L: Plua_State; idx: Integer): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_objlen(L, idx);
end;

function luaL_checkint(L: Plua_State; n: Integer): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := luaL_checkinteger(L, n);
end;

function luaL_optint(L: Plua_State; n: Integer; d: lua_Integer): Integer; {$IFDEF HAVEINLINE}inline;

{$ENDIF}

begin
  Result := luaL_optinteger(L, n, d);
end;

function luaL_checklong(L: Plua_State; n: Integer): LongInt; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := luaL_checkinteger(L, n);
end;

function luaL_optlong(L: Plua_State; n: Integer; d: lua_Integer): LongInt;

{$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := luaL_optinteger(L, n, d);
end;

function luaL_typename(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := lua_typename(L, lua_type(L, i));
end;

function luaL_dofile(L: Plua_State; fn: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := (luaL_loadfile(L, fn) <> 0) or (lua_pcall(L, 0, LUA_MULTRET, 0) <> 0);
end;

function luaL_dostring(L: Plua_State; s: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}

begin
  Result := (luaL_loadstring(L, s) <> 0) or (lua_pcall(L, 0, LUA_MULTRET, 0) <> 0);
end;

procedure luaL_getmetatable(L: Plua_State; n: PAnsiChar); {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

function lua_upvalueindex(i: Integer): Integer; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := (LUA_GLOBALSINDEX - (i));
end;

function lua_app_alloc(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;
begin
  if (nsize > 0) then
  begin
    if ptr = nil then
      GetMem(Result, nsize)
    else
    begin
      ReallocMem(ptr, nsize);
      Result := ptr;
    end;
  end
  else
  begin
    if ptr <> nil then
      FreeMem(ptr);
    Result := nil;
  end;
end;

type
  TLuaState = (
    lsStart,
    lsLessThan,
    lsEscape,
    lsEscapeClose,
    lsLua,
    lsLuaBody,
    lsLuaBodyEnd,
    lsLuaEqual,
    lsLuaEqualEnd
    );
  PLuaTextProcessor = ^TLuaTextProcessor;

  TLuaTextProcessor = record
    stream: TStream;
    state: TLuaState;
    outbuffer: array [0 .. 1023] of AnsiChar;
  end;

function lua_stream_reader(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;
var
  inbuffer     : array [0 .. 1023] of AnsiChar;
  inlen, outlen: Integer;
  pr           : PLuaTextProcessor;
  pin, pout    : PAnsiChar;
  c            : AnsiChar;
  function Append(const str: PAnsiChar; L: Integer): Boolean;
  begin
    if outlen + L <= sizeof(pr.outbuffer) then
    begin
      move(str^, pout^, L);
      inc(pout, L);
      inc(outlen, L);
      Result := true;
    end
    else
      Result := false;
  end;

label
  redo,
  needspace;
begin
  pr := PLuaTextProcessor(ud);
  inlen := pr.stream.Read(inbuffer, sizeof(inbuffer));
  outlen := 0;
  pin := @inbuffer;
  pout := @pr.outbuffer;
  while (inlen > 0) do
  begin
    c := pin^;
  redo:
    case pr.state of
      lsStart:
        begin
          case c of
            '<':
              pr.state := lsLessThan;
            else
              if not Append('print("', 7) then
                goto needspace;
              pr.state := lsEscape;
              goto redo;
          end;
        end;
      lsLessThan:
        begin
          case c of
            '%':
              pr.state := lsLua;
            else
              if not Append('print("<', 8) then
                goto needspace;
              pr.state := lsEscape;
              goto redo;
          end;
        end;
      lsEscape:
        begin
          case c of
            '<':
              pr.state := lsEscapeClose;
            else
              case c of
                #7:
                  if not Append('\a', 2) then
                    goto needspace;
                #8:
                  if not Append('\b', 2) then
                    goto needspace;
                #9:
                  if not Append('\t', 2) then
                    goto needspace;
                #10:
                  if not Append('\n")'#10'print("', 12) then
                    goto needspace;
                #11:
                  if not Append('\v', 2) then
                    goto needspace;
                #13:
                  if not Append('\r', 2) then
                    goto needspace;
                '\':
                  if not Append('\\', 2) then
                    goto needspace;
                '"':
                  if not Append('\"', 2) then
                    goto needspace;
                '''':
                  if not Append('\''', 2) then
                    goto needspace;
                else
                  if not Append(pin, 1) then
                    goto needspace;
              end;
          end;
        end;
      lsEscapeClose:
        begin
          case c of
            '%':
              begin
                if not Append('");', 3) then
                  goto needspace;
                pr.state := lsLua;
              end;
            else
              if not Append('<', 1) then
                goto needspace;
              pr.state := lsEscape;
              goto redo;
          end;
        end;
      lsLua:
        begin
          case c of
            '=':
              begin
                if not Append('print((', 7) then
                  goto needspace;
                pr.state := lsLuaEqual;
              end;
            '%':
              pr.state := lsLuaBodyEnd;
            else
              if not Append(pin, 1) then
                goto needspace;
              pr.state := lsLuaBody;
          end;
        end;
      lsLuaBody:
        begin
          case c of
            '%':
              pr.state := lsLuaBodyEnd;
            else
              if not Append(pin, 1) then
                goto needspace;
          end;
        end;
      lsLuaBodyEnd:
        begin
          case c of
            '>':
              pr.state := lsStart;
            else
              if not Append('%', 1) then
                goto needspace;
              pr.state := lsLuaBody;
              goto redo;
          end;
        end;
      lsLuaEqual:
        begin
          case c of
            '%':
              pr.state := lsLuaEqualEnd;
            else
              if not Append(pin, 1) then
                goto needspace;
          end;
        end;
      lsLuaEqualEnd:
        begin
          case c of
            '>':
              begin
                if not Append('));', 3) then
                  goto needspace;
                pr.state := lsStart;
              end;
            else
              if not Append('%', 1) then
                goto needspace;
              pr.state := lsLuaEqual;
              goto redo;
          end;
        end;
    end;
    inc(pin);
    dec(inlen);
  end;
  if outlen = 0 then
  begin
    case pr.state of
      lsLessThan:
        Append('print("<")', 10);
      lsEscape:
        Append('")', 2);
      lsEscapeClose:
        Append('%")', 3);
    end;
    pr.state := lsStart;
  end;
  Result := @pr.outbuffer;
  sz^ := outlen;
  Exit;
needspace:
  pr.stream.Seek( - inlen, soFromCurrent);
  Result := @pr.outbuffer;
  sz^ := outlen;
end;

function lua_processsor_loadstream(L: Plua_State; stream: TStream; chunkname: PAnsiChar): Integer;
var
  bom      : array [0 .. 2] of Byte;
  processor: TLuaTextProcessor;
begin
  processor.stream := stream;
  processor.state := lsStart;
  // skip utf8 bom
  stream.Seek(0, soFromBeginning);
  if not ((stream.Read(bom, 3) = 3) and (bom[0] = $EF) and (bom[1] = $BB) and (bom[2] = $BF)) then
    stream.Seek(0, soFromBeginning);
  Result := lua_load(L, @lua_stream_reader, @processor, chunkname);
end;

function lua_processsor_loadfile(L: Plua_State; const filename: string;
  chunkname: PAnsiChar): Integer;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    Result := lua_processsor_loadstream(L, stream, chunkname);
  finally
    stream.Free;
  end;
end;

function lua_processsor_dofile(L: Plua_State; const filename: string; chunkname: PAnsiChar)
  : Boolean; {$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := (lua_processsor_loadfile(L, filename, chunkname) = 0) and
    (lua_pcall(L, 0, LUA_MULTRET, 0) = 0);
end;

function lua_processsor_dostream(L: Plua_State; stream: TStream; chunkname: PAnsiChar): Boolean;

{$IFDEF HAVEINLINE}inline; {$ENDIF}


begin
  Result := (lua_processsor_loadstream(L, stream, chunkname) = 0) and
    (lua_pcall(L, 0, LUA_MULTRET, 0) = 0);
end;

end.
