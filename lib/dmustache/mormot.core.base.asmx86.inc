{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   x86 32-bit assembly used by mormot.core.base.pas
}

{$ifdef FPC}
  // disabled some FPC paranoid warnings
  {$WARN 7102 off : Use of +offset(%ebp) for parameters invalid here }
  {$WARN 7104 off : Use of -offset(%ebp) is not recommended for local variable access }
  {$WARN 7121 off : Check size of XMM memory operand }
  {$WARN 7122 off : Check size of XMM memory operand }
{$endif FPC}

{$ifdef ASMX86} // i386 asm with global - disabled on PIC targets

{
 FillCharFast/MoveFast implementation notes:
 - use SS2 and not FPU fld/fst which may trigger unexpected exceptions (Agner)
 - use dedicated branchless sub-functions for small buffers of 0..32 bytes
 - use simple SSE2 loop for 33..255 bytes
 - assume ERBMS is available (cpuid flag may not be propagated within VMs)
 - use "rep movsb" for 256..512K input (should work on all CPUs, even older with
 no ERBMS), or as fallback if no SSE2 CPU is used
 - use non volatile SSE2 loop when >= 512KB (to not pollute the CPU cache)
 - don't use backward "std rep movsb" since it is not involved by ERMBS (slow)
 - note: Delphi Win32 x87 RTL code by John O'Harrow seems deprecated
}

{$define WITH_ERMS} // we need it as fallback on old CPU without SSE2

{$ifdef WITH_ERMS}
var
  // "rep stosb/movsb" enabled for len >= 4096 on ERMSB CPUs
  // it has been reported that "on 32-bit strings have to be at least 4KB"
  // see https://stackoverflow.com/a/43837564/458259 for explanations and timing
  ERMSB_MIN_SIZE_FWD: integer = maxInt; // maxInt = disabled by default
  {$ifndef FPC_X86}
  ERMSB_MIN_SIZE_BWD: integer = maxInt; // used for no-SSE2, not for ERMSB
  {$endif FPC_X86}
  {$define WITH_ERMSASM} // include "rep movsb" asm blocks
{$endif WITH_ERMS}

{$ifdef HASNOSSE2}
  {$define WITH_ERMSASM} // "rep movsd" is used as fallback on oldest CPU
{$endif HASNOSSE2}

const
  // non-temporal writes should bypass the cache when the size is bigger than
  // half the size of the largest level cache - we assume low 1MB cache here
  NONTEMPORALSIZE = 1 shl 20;

// fast SSE2 version - force define HASNOSSE2 when run any very old CPU
procedure FillcharFast(var dst; cnt: PtrInt; value: byte);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=Dest edx=Count cl=Value
        movzx   ecx, cl
        imul    ecx, ecx, $01010101
        cmp     edx, 32
        jg      @32up
        test    edx, edx
        jle     @00        
        mov     [eax + edx - 1], cl     // fill last byte
        shr     edx, 1                  // how many words to fill
        jmp     dword ptr [edx * 4 + @jmptab]
@00:    ret
@32up:
{$ifndef HASNOSSE2}
        {$ifdef WITH_ERMS}
        cmp     edx, NONTEMPORALSIZE
        jae     @noerms // movntdq was reported to be faster than ERMSB
        cmp     edx, ERMSB_MIN_SIZE_FWD
        ja      @ermsb
@noerms:{$endif WITH_ERMS}
        sub     edx, 16
        movd    xmm0, ecx
        mov     ecx, eax
        pshufd  xmm0, xmm0, 0
        and     ecx, 15                      // 16-byte align writes
        movups  dqword ptr [eax], xmm0       // fill first 16 bytes
        movups  dqword ptr [eax + edx], xmm0 // fill last 16 bytes
        sub     ecx, 16
        sub     eax, ecx
        add     edx, ecx
        add     eax, edx
        neg     edx
        cmp     edx, -NONTEMPORALSIZE        // assume > 512KB bypass the cache
        jl      @nv
{$ifdef FPC} align 16 {$else} {$ifdef HASALIGN} .align 16 {$endif}{$endif}
@loop:  movaps  dqword ptr [eax + edx], xmm0 // fill 16 bytes per loop
        add     edx, 16
        jl      @loop
        ret
{$ifdef FPC} align 16 {$else} {$ifdef HASALIGN} .align 16 {$endif}{$endif}
@nv:    movntdq dqword ptr [eax + edx], xmm0 // non-temporal fill 16 bytes
        add     edx, 16
        jl      @nv
        ret
{$endif HASNOSSE2}
{$ifdef WITH_ERMSASM}
@ermsb: push    edi
        cld
{$ifdef HASNOSSE2}
@ermsa: test    al, 3   // aligned stosd is better on old CPUs
        jz      @erms3
        mov     byte ptr [eax], cl
        inc     eax
        dec     edx
        jmp     @ermsa
@erms3: mov     edi, eax
        mov     eax, ecx
        mov     ecx, edx
        shr     ecx, 2
        jz      @erms2
        rep     stosd    // no SSE2 version
@erms2: test    dl, 2
        jz      @erms1
        mov     word ptr [edi], ax
        add     edi, 2
@erms1: test    dl, 1
        jz      @erms0
        mov     byte ptr [edi], al
{$else} // ERMSB favors stosb and will properly align writes
        mov     edi, eax
        mov     eax, ecx
        mov     ecx, edx
        rep     stosb
{$endif HASNOSSE2}
@erms0: pop     edi
        ret
{$endif WITH_ERMSASM}
{$ifdef FPC} align 4 {$else} nop {$ifdef HASNOSSE2} nop; nop {$endif} {$endif}
@jmptab:dd      @00, @02, @04, @06, @08, @10, @12, @14, @16
        dd      @18, @20, @22, @24, @26, @28, @30, @32
        // Delphi RTL uses 16-bit "mov [eax + ...], cx" which are slower
@32:    mov     [eax + 28], ecx
@28:    mov     [eax + 24], ecx
@24:    mov     [eax + 20], ecx
@20:    mov     [eax + 16], ecx
@16:    mov     [eax + 12], ecx
@12:    mov     [eax + 8], ecx
@08:    mov     [eax + 4], ecx
@04:    mov     [eax], ecx
        ret
@30:    mov     [eax + 26], ecx
@26:    mov     [eax + 22], ecx
@22:    mov     [eax + 18], ecx
@18:    mov     [eax + 14], ecx
@14:    mov     [eax + 10], ecx
@10:    mov     [eax + 6], ecx
@06:    mov     [eax + 2], ecx
@02:    mov     word ptr [eax], cx
end;

{$ifndef FPC_X86} // FPC RTL has fastmove.inc -> our SSE2/ERMS asm is slower

// fast SSE2 version - force define HASNOSSE2 when run any very old CPU
procedure MoveFast(const src; var dst; cnt: PtrInt);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=source edx=dest ecx=count
        cmp     ecx, 32
        ja      @lrg                   // count > 32 or count < 0
        sub     ecx, 8
        jg      @sml                   // 9..32 byte move
        jmp     dword ptr [@table + 32 + ecx * 4]   // 0..8 byte move
{$ifdef HASNOSSE2}
@sml:   fild    qword ptr [eax + ecx]   // last 8
        fild    qword ptr [eax]         // first 8
        cmp     ecx, 8
        jle     @sml16
        fild    qword ptr [eax + 8]     // second 8
        cmp     ecx, 16
        jle     @sml24
        fild    qword ptr [eax + 16]    // third 8
        fistp   qword ptr [edx + 16]    // third 8
@sml24: fistp   qword ptr [edx + 8]     // second 8
@sml16: fistp   qword ptr [edx]         // first 8
        fistp   qword ptr [edx + ecx]   // last 8
{$else}
@sml:   movq    xmm0, qword ptr [eax + ecx]   // last 8
        movq    xmm1, qword ptr [eax]         // first 8
        cmp     ecx, 8
        jle     @sml16
        movq    xmm2, qword ptr [eax + 8]     // second 8
        cmp     ecx, 16
        jle     @sml24
        movq    xmm3, qword ptr [eax + 16]    // third 8
        movq    qword ptr [edx + 16], xmm3    // third 8
@sml24: movq    qword ptr [edx + 8], xmm2     // second 8
@sml16: movq    qword ptr [edx], xmm1         // first 8
        movq    qword ptr [edx + ecx], xmm0   // last 8
        ret
{$endif HASNOSSE2}
@exit:  rep     ret
{$ifdef FPC} align 4 {$else} {$ifdef HASALIGN} .align 4 {$endif}{$endif}
@table: dd      @exit, @m01, @m02, @m03, @m04, @m05, @m06, @m07, @m08
{$ifdef WITH_ERMSASM}
@ermsf: push    esi
        push    edi
        mov     esi, eax
        mov     edi, edx
        cld
        rep     movsb  // ERMSB forward move
        pop     edi
        pop     esi
        ret
@ermsb: push    esi
        push    edi
        lea     esi, [eax + ecx - 1]
        lea     edi, [edx + ecx - 1]
        std
        rep     movsb  // backward move is slow even if ERMSB is set
        pop     edi
        pop     esi
        cld            // FPC requires this
        ret
{$endif WITH_ERMSASM}
{$ifdef HASNOSSE2}
@lrg:   jng    @exit   // count < 0
        sub    edx, eax
        jz     @exit
        cmp    edx, ecx
        lea    edx, [edx + eax]
        jb     @ermsb  // move backwards if unsigned(dest-source) < count
        jmp    @ermsf
{$else}
@lrgfwd:// large forward move
        {$ifdef WITH_ERMS}
        cmp     ecx, NONTEMPORALSIZE
        jae     @noermf // movntdq was reported to be faster than ERMSB
        cmp     ecx, ERMSB_MIN_SIZE_FWD
        ja      @ermsf
@noermf:{$endif WITH_ERMS}
        push    edx
        movups  xmm2, dqword ptr [eax]       // first 16
        lea     eax, [eax + ecx - 16]
        lea     ecx, [ecx + edx - 16]
        movups  xmm1, dqword ptr [eax]       // last 16
        push    ecx
        neg     ecx
        and     edx, -16                     // 16-byte align writes
        lea     ecx, [ecx + edx + 16]
        pop     edx
        cmp     ecx, -NONTEMPORALSIZE        // assume > 512KB bypass the cache
        jl      @fwnv
{$ifdef FPC} align 16 {$else} {$ifdef HASALIGN} .align 16 {$endif}{$endif}
@fwd:   movups  xmm0, dqword ptr [eax + ecx] // move by 16 bytes
        movaps  dqword ptr [edx + ecx], xmm0
        add     ecx, 16
        jl      @fwd
@fwde:  movups  dqword ptr [edx], xmm1       // last 16
        pop     edx
        movups  dqword ptr [edx], xmm2       // first 16
        ret
{$ifdef FPC} align 16 {$else} {$ifdef HASALIGN} .align 16 {$endif}{$endif}
@fwnv:  movups  xmm0, dqword ptr [eax + ecx]
        movntdq dqword ptr [edx + ecx], xmm0 // non-temporal move by 16 bytes
        add     ecx, 16
        jl      @fwnv
        sfence
        jmp     @fwde
@lrg:   jng     @exit      // count < 0
        sub     edx, eax
        jz      @exit      // source=dest
        cmp     edx, ecx
        lea     edx, [edx + eax]
        jae     @lrgfwd   // move backwards if unsigned(dest-source) < count
        // large backward/overlapping move
@lrgbwd:{$ifdef WITH_ERMS}
        cmp     ecx, ERMSB_MIN_SIZE_BWD // enabled for no-SSE2, not for ERMSB
        ja      @ermsb
        {$endif WITH_ERMS}
        sub     ecx, 16
        push    ecx
        movups  xmm2, dqword ptr [eax + ecx] // last 16
        movups  xmm1, dqword ptr [eax]       // first 16
        add     ecx, edx
        and     ecx, -16              // 16-byte align writes
        sub     ecx, edx
        // non-volatile backward is not efficient since we overlap data
{$ifdef FPC} align 16 {$else} {$ifdef HASALIGN} .align 16 {$endif}{$endif}
@bwd:   movups  xmm0, dqword ptr [eax + ecx]
        movaps  dqword ptr [edx + ecx], xmm0
        sub     ecx, 16
        jg      @bwd
        pop     ecx
        movups  dqword ptr [edx], xmm1       // first 16
        movups  dqword ptr [edx + ecx], xmm2 // last 16
        ret
{$endif HASNOSSE2}
@m01:   mov     al, [eax]
        mov     [edx], al
        ret
@m02:   movzx   eax, word ptr [eax]
        mov     [edx], ax
        ret
@m03:   movzx   ecx, word ptr [eax]
        mov     al, [eax + 2]
        mov     [edx], cx
        mov     [edx + 2], al
        ret
@m04:   mov     ecx, [eax]
        mov     [edx], ecx
        ret
@m05:   mov     ecx, [eax]
        mov     al, [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], al
        ret
@m06:   mov     ecx, [eax]
        movzx   eax, word ptr [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], ax
        ret
@m07:   mov     ecx, [eax]
        mov     eax, [eax + 3]
        mov     [edx], ecx
        mov     [edx + 3], eax
        ret
@m08:   mov     ecx, [eax]
        mov     eax, [eax + 4]
        mov     [edx], ecx
        mov     [edx + 4], eax
end;

{$endif FPC_X86}

function CompareMem(P1, P2: Pointer; Length: PtrInt): boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P1 edx=P2 ecx=Length
        cmp     eax, edx
        je      @0                 // P1=P2
        sub     ecx, 8
        jl      @small
        push    ebx
        mov     ebx, [eax]         // Compare First 4 Bytes
        cmp     ebx, [edx]
        jne     @setbig
        lea     ebx, [eax + ecx]   // Compare Last 8 Bytes
        add     edx, ecx
        mov     eax, [ebx]
        cmp     eax, [edx]
        jne     @setbig
        mov     eax, [ebx + 4]
        cmp     eax, [edx + 4]
        jne     @setbig
        sub     ecx, 4
        jle     @true              // All Bytes already Compared
        neg     ecx                // ecx=-(Length-12)
        add     ecx, ebx           // DWORD Align Reads
        and     ecx, -4
        sub     ecx, ebx
@loop:  mov     eax, [ebx + ecx]   // Compare 8 Bytes per Loop
        cmp     eax, [edx + ecx]
        jne     @setbig
        mov     eax, [ebx + ecx + 4]
        cmp     eax, [edx + ecx + 4]
        jne     @setbig
        add     ecx, 8
        jl      @loop
@true:  pop     ebx
@0:     mov     al, 1
        ret
@setbig:pop     ebx
        setz    al
        ret
@small: add     ecx, 8             // ecx=0..7
        jle     @0                 // Length <= 0
        neg     ecx                // ecx=-1..-7
        lea     ecx, [@1 + ecx * 8 + 8]   // each @#: block below = 8 bytes
        jmp     ecx
@7:     mov     cl, [eax + 6]
        cmp     cl, [edx + 6]
        jne     @setsml
@6:     mov     ch, [eax + 5]
        cmp     ch, [edx + 5]
        jne     @setsml
@5:     mov     cl, [eax + 4]
        cmp     cl, [edx + 4]
        jne     @setsml
@4:     mov     ch, [eax + 3]
        cmp     ch, [edx + 3]
        jne     @setsml
@3:     mov     cl, [eax + 2]
        cmp     cl, [edx + 2]
        jne     @setsml
@2:     mov     ch, [eax + 1]
        cmp     ch, [edx + 1]
        jne     @setsml
@1:     mov     al, [eax]
        cmp     al, [edx]
@setsml:setz    al
end;

function crc32fasttab(crc: cardinal; buf: PAnsiChar; len: cardinal;
  tab: PCrc32tab): cardinal;
asm
        // adapted from Aleksandr Sharahov code and Maxim Masiutin remarks
        push    ebx
        push    ebp
        mov     ebp, tab
        not     eax
        neg     ecx       // eax=crc edx=buf ecx=-len ebp=tab
        jz      @z
        test    edx, edx
        jz      @z
@head:  test    dl, 3
        jz      @align
        movzx   ebx, byte ptr [edx]
        inc     edx
        xor     bl, al
        shr     eax, 8
        xor     eax, dword ptr [ebx * 4 + ebp]
        inc     ecx
        jnz     @head
@z:     jmp     @e
@align: sub     edx, ecx
        add     ecx, 8
        jg      @done
        push    esi
        push    edi
        mov     edi, edx
        {$ifdef FPC} align   8 {$endif FPC}
@by8:   mov     edx, eax
        mov     ebx, [edi + ecx - 4]
        xor     edx, [edi + ecx - 8]
        movzx   esi, bl
        mov     eax, dword ptr [esi * 4 + ebp + 1024 * 3]
        movzx   esi, bh
        xor     eax, dword ptr [esi * 4 + ebp + 1024 * 2]
        shr     ebx, 16
        movzx   esi, bl
        xor     eax, dword ptr [esi * 4 + ebp + 1024 * 1]
        movzx   esi, bh
        xor     eax, dword ptr [esi * 4 + ebp + 1024 * 0]
        movzx   esi, dl
        xor     eax, dword ptr [esi * 4 + ebp + 1024 * 7]
        movzx   esi, dh
        xor     eax, dword ptr [esi * 4 + ebp + 1024 * 6]
        shr     edx, 16
        movzx   esi, dl
        xor     eax, dword ptr [esi * 4 + ebp + 1024 * 5]
        movzx   esi, dh
        xor     eax, dword ptr [esi * 4 + ebp + 1024 * 4]
        add     ecx, 8
        jle     @by8
        mov     edx, edi
        pop     edi
        pop     esi
@done:  sub     ecx, 8
        jge     @e
@tail:  movzx   ebx, byte[edx + ecx]
        xor     bl, al
        shr     eax, 8
        xor     eax, dword ptr [ebx * 4 + ebp]
        inc     ecx
        jnz     @tail
@e:     pop     ebp
        pop     ebx
        not     eax
end;

procedure crcblockfast(crc128, data128: PBlock128);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // Delphi is not efficient about corresponding pascal code
        push    ebp
        push    edi
        push    esi
        mov     ebp, eax  // ebp=crc128 edi=data128
        mov     edi, edx
        mov     edx, dword ptr [eax]
        mov     ecx, dword ptr [eax + 4]
        xor     edx, dword ptr [edi]
        xor     ecx, dword ptr [edi + 4]
        movzx   esi, dl
        // note: since we have "+ 1024 * n" offsets, crc32ctab is left immediate
        mov     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, dh
        shr     edx, 16
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, dl
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, dh
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 0]
        mov     edx, dword ptr [ebp + 8]
        xor     edx, dword ptr [edi + 8]
        mov     dword ptr [ebp], eax
        movzx   esi, cl
        mov     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, ch
        shr     ecx, 16
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, cl
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, ch
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 0]
        mov     dword ptr [ebp + 4], eax
        mov     ecx, dword ptr [ebp + 12]
        xor     ecx, dword ptr [edi + 12]
        movzx   esi, dl
        mov     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, dh
        shr     edx, 16
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, dl
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, dh
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 0]
        mov     dword ptr [ebp + 8], eax
        movzx   esi, cl
        mov     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 3]
        movzx   esi, ch
        shr     ecx, 16
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 2]
        movzx   esi, cl
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 1]
        movzx   esi, ch
        xor     eax, dword ptr [esi * 4 + crc32ctab + 1024 * 0]
        mov     dword ptr [ebp + 12], eax
        pop     esi
        pop     edi
        pop     ebp
end;

function fnv32(crc: cardinal; buf: PAnsiChar; len: PtrInt): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=crc, edx=buf, ecx=len
        push    ebx
        test    edx, edx
        jz      @0
        neg     ecx
        jz      @0
        sub     edx, ecx
@1:     movzx   ebx, byte ptr [edx + ecx]
        xor     eax, ebx
        imul    eax, eax, 16777619
        inc     ecx
        jnz     @1
@0:     pop     ebx
end; // we tried an unrolled version, but it was slower on our Core i7!

function kr32(crc: cardinal; buf: PAnsiChar; len: PtrInt): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=crc, edx=buf, ecx=len
        test    ecx, ecx
        push    edi
        push    esi
        push    ebx
        push    ebp
        jz      @z
        cmp     ecx, 4
        jb      @s
@8:     mov     ebx, [edx] // unrolled version reading per dword
        add     edx, 4
        mov     esi, eax
        movzx   edi, bl
        movzx   ebp, bh
        shr     ebx, 16
        shl     eax, 5
        sub     eax, esi
        add     eax, edi
        mov     esi, eax
        shl     eax, 5
        sub     eax, esi
        lea     esi, [eax + ebp]
        add     eax, ebp
        movzx   edi, bl
        movzx   ebx, bh
        shl     eax, 5
        sub     eax, esi
        lea     ebp, [eax + edi]
        add     eax, edi
        shl     eax, 5
        sub     eax, ebp
        add     eax, ebx
        cmp     ecx, 8
        lea     ecx, [ecx - 4]
        jae     @8
        test    ecx, ecx
        jz      @z
@s:     mov     esi, eax
@1:     shl     eax, 5
        movzx   ebx, byte ptr [edx]
        inc     edx
        sub     eax, esi
        lea     esi, [eax + ebx]
        add     eax, ebx
        dec     ecx
        jnz     @1
@z:     pop     ebp
        pop     ebx
        pop     esi
        pop     edi
end;

function StrLenSSE2(S: pointer): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // from GPL strlen32.asm by Agner Fog - www.agner.org/optimize
        mov     ecx, eax            // copy pointer
        test    eax, eax
        jz      @null               // returns 0 if S=nil
        push    eax                 // save start address
        pxor    xmm0, xmm0          // set to zero
        and     ecx, 15             // lower 4 bits indicate misalignment
        and     eax, -16            // align pointer by 16
        // will never read outside a memory page boundary, so won't trigger GPF
        movaps  xmm1, [eax]         // read from nearest preceding boundary
        pcmpeqb xmm1, xmm0          // compare 16 bytes with zero
        pmovmskb edx, xmm1          // get one bit for each byte result
        shr     edx, cl             // shift out false bits
        shl     edx, cl             // shift back again
        bsf     edx, edx            // find first 1-bit
        jnz     @A200               // found
        // Main loop, search 16 bytes at a time
@A100:  add     eax, 10H            // increment pointer by 16
        movaps  xmm1, [eax]         // read 16 bytes aligned
        pcmpeqb xmm1, xmm0          // compare 16 bytes with zero
        pmovmskb edx, xmm1          // get one bit for each byte result
        bsf     edx, edx            // find first 1-bit
        // (moving the bsf out of the loop and using test here would be faster
        // for long strings on old processors, but we are assuming that most
        // strings are short, and newer processors have higher priority)
        jz      @A100               // loop if not found
@A200:  // Zero-byte found. Compute string length
        pop     ecx                 // restore start address
        sub     eax, ecx            // subtract start address
        add     eax, edx            // add byte index
@null:
end;

function StrInt32(P: PAnsiChar; val: PtrInt): PAnsiChar;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P, edx=val
        mov     ecx, edx
        sar     ecx, 31         // 0 if val>=0 or -1 if val<0
        push    ecx
        xor     edx, ecx
        sub     edx, ecx        // edx=abs(val)
        cmp     edx, 10
        jb      @3  // direct process of common val<10
        push    edi
        mov     edi, eax
        mov     eax, edx
@s:     sub     edi, 2
        cmp     eax, 100
        jb      @2
        mov     ecx, eax
        mov     edx, 1374389535 // use power of two reciprocal to avoid division
        mul     edx
        shr     edx, 5          // now edx=eax div 100
        mov     eax, edx
        imul    edx, -200
        movzx   edx, word ptr [TwoDigitLookup + ecx * 2 + edx]
        mov     [edi], dx
        cmp     eax, 10
        jae     @s
@1:     dec     edi
        or      al, '0'
        mov     byte ptr [edi - 1], '-'
        mov     [edi], al
        mov     eax, edi
        pop     edi
        pop     ecx
        add     eax, ecx // includes '-' if val<0
        ret
@2:     movzx   eax, word ptr [TwoDigitLookup + eax * 2]
        mov     byte ptr [edi - 1], '-'
        mov     [edi], ax
        mov     eax, edi
        pop     edi
        pop     ecx
        add     eax, ecx // includes '-' if val<0
        ret
@3:     dec     eax
        pop     ecx
        or      dl, '0'
        mov     byte ptr [eax - 1], '-'
        mov     [eax], dl
        add     eax, ecx // includes '-' if val<0
end;

function StrUInt32(P: PAnsiChar; val: PtrUInt): PAnsiChar;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P, edx=val
        cmp     edx, 10
        jb      @3  // direct process of common val=0 (or val<10)
        push    edi
        mov     edi, eax
        mov     eax, edx
@s:     sub     edi, 2
        cmp     eax, 100
        jb      @2
        mov     ecx, eax
        mov     edx, 1374389535 // use power of two reciprocal to avoid division
        mul     edx
        shr     edx, 5          // now edx=eax div 100
        mov     eax, edx
        imul    edx, -200
        movzx   edx, word ptr [TwoDigitLookup + ecx * 2 + edx]
        mov     [edi], dx
        cmp     eax, 10
        jae     @s
@1:     dec     edi
        or      al, '0'
        mov     [edi], al
        mov     eax, edi
        pop     edi
        ret
@2:     movzx   eax, word ptr [TwoDigitLookup + eax * 2]
        mov     [edi], ax
        mov     eax, edi
        pop     edi
        ret
@3:     dec     eax
        or      dl, '0'
        mov     [eax], dl
end;

procedure YearToPChar(Y: PtrUInt; P: PUtf8Char);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=Y, edx=P
        push    edx
        mov     ecx, eax
        mov     edx, 1374389535 // use power of two reciprocal to avoid division
        mul     edx
        shr     edx, 5          // now edx=Y div 100
        movzx   eax, word ptr [TwoDigitLookup + edx * 2]
        imul    edx, -200
        movzx   edx, word ptr [TwoDigitLookup + ecx * 2 + edx]
        pop     ecx
        shl     edx, 16
        or      eax, edx
        mov     [ecx], eax
end;

{$endif ASMX86}

// functions below are always available, even on DARWIN

function Hash32(Data: PCardinalArray; Len: integer): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=Data edx=Len
        push    esi
        push    edi
        mov     cl, dl
        mov     ch, dl
        xor     esi, esi
        xor     edi, edi
        test    eax, eax
        jz      @z
        shr     edx, 4
        jz      @by4
        nop
@by16:  add     esi, dword ptr [eax]
        add     edi, esi
        add     esi, dword ptr [eax+4]
        add     edi, esi
        add     esi, dword ptr [eax+8]
        add     edi, esi
        add     esi, dword ptr [eax+12]
        add     edi, esi
        add     eax, 16
        dec     edx
        jnz     @by16
@by4:   and     cl, 15
        jz      @0
        shr     cl, 2
        jz      @rem
@4:     add     esi, dword ptr [eax]
        add     edi, esi
        add     eax, 4
        dec     cl
        jnz     @4
@rem:   and     ch, 3
        jz      @0
        dec     ch
        jz      @1
        dec     ch
        jz      @2
        mov     eax, dword ptr [eax]
        and     eax, $ffffff
        jmp     @e
@2:     movzx   eax, word ptr [eax]
        jmp     @e
@1:     movzx   eax, byte ptr [eax]
@e:     add     esi, eax
@0:     add     edi, esi
        mov     eax, esi
        shl     edi, 16
        xor     eax, edi
@z:     pop     edi
        pop     esi
end;

function xxHash32(crc: cardinal; P: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        xchg    edx, ecx
        push    ebp
        push    edi
        lea     ebp, [ecx+edx]
        push    esi
        push    ebx
        sub     esp, 8
        mov     ebx, eax
        mov     dword ptr [esp], edx
        lea     eax, [ebx+165667B1H]
        cmp     edx, 15
        jbe     @2
        lea     eax, [ebp-10H]
        lea     edi, [ebx+24234428H]
        lea     esi, [ebx-7A143589H]
        mov     dword ptr [esp+4H], ebp
        mov     edx, eax
        lea     eax, [ebx+61C8864FH]
        mov     ebp, edx
@1:     mov     edx, dword ptr [ecx]
        imul    edx, -2048144777
        add     edi, edx
        rol     edi, 13
        imul    edi, -1640531535
        mov     edx, dword ptr [ecx+4]
        imul    edx, -2048144777
        add     esi, edx
        rol     esi, 13
        imul    esi, -1640531535
        mov     edx, dword ptr [ecx+8]
        imul    edx, -2048144777
        add     ebx, edx
        rol     ebx, 13
        imul    ebx, -1640531535
        mov     edx, dword ptr [ecx+12]
        lea     ecx, [ecx+16]
        imul    edx, -2048144777
        add     eax, edx
        rol     eax, 13
        imul    eax, -1640531535
        cmp     ebp, ecx
        jnc     @1
        rol     edi, 1
        rol     esi, 7
        rol     ebx, 12
        add     esi, edi
        mov     ebp, dword ptr [esp+4H]
        ror     eax, 14
        add     ebx, esi
        add     eax, ebx
@2:     lea     esi, [ecx+4H]
        add     eax, dword ptr [esp]
        cmp     ebp, esi
        jc      @4
        mov     ebx, esi
        nop
@3:     imul    edx, dword ptr [ebx-4H], -1028477379
        add     ebx, 4
        add     eax, edx
        ror     eax, 15
        imul    eax, 668265263
        cmp     ebp, ebx
        jnc     @3
        lea     edx, [ebp-4H]
        sub     edx, ecx
        mov     ecx, edx
        and     ecx, 0FFFFFFFCH
        add     ecx, esi
@4:     cmp     ebp, ecx
        jbe     @6
@5:     movzx   edx, byte ptr [ecx]
        add     ecx, 1
        imul    edx, 374761393
        add     eax, edx
        rol     eax, 11
        imul    eax, -1640531535
        cmp     ebp, ecx
        jnz     @5
        nop
@6:     mov     edx, eax
        add     esp, 8
        shr     edx, 15
        xor     eax, edx
        imul    eax, -2048144777
        pop     ebx
        pop     esi
        mov     edx, eax
        shr     edx, 13
        xor     eax, edx
        imul    eax, -1028477379
        pop     edi
        pop     ebp
        mov     edx, eax
        shr     edx, 16
        xor     eax, edx
end;

function GetBitsCountPas(value: PtrInt): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // branchless Wilkes-Wheeler-Gill i386 asm implementation
        mov     edx, eax
        shr     eax, 1
        and     eax, $55555555
        sub     edx, eax
        mov     eax, edx
        shr     edx, 2
        and     eax, $33333333
        and     edx, $33333333
        add     eax, edx
        mov     edx, eax
        shr     eax, 4
        add     eax, edx
        and     eax, $0f0f0f0f
        mov     edx, eax
        shr     edx, 8
        add     eax, edx
        mov     edx, eax
        shr     edx, 16
        add     eax, edx
        and     eax, $3f
end;

{$ifdef HASNOSSE2} // fallback to simple pascal code if no SSE2 available

function ByteScanIndex(P: PByteArray; Count: PtrInt; Value: byte): PtrInt;
begin
  result := 0;
  if P <> nil then
    repeat
      if result >= Count then
        break;
      if P^[result] = Value then
        exit;
      inc(result);
    until false;
  result := -1;
end;

function WordScanIndex(P: PWordArray; Count: PtrInt; Value: word): PtrInt;
begin
  result := 0;
  if P <> nil then
    repeat
      if result >= Count then
        break;
      if P^[result] = Value then
        exit;
      inc(result);
    until false;
  result := -1;
end;

function IntegerScanIndex(P: PCardinalArray; Count: PtrInt; Value: cardinal): PtrInt;
begin
  result := PtrUInt(IntegerScan(P, Count, Value));
  if result = 0 then
    dec(result)
  else
  begin
    dec(result, PtrUInt(P));
    result := result shr 2;
  end;
end;

function IntegerScan(P: PCardinalArray; Count: PtrInt; Value: cardinal): PCardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P, edx=Count, Value=ecx
        test    eax, eax
        jz      @ok0 // avoid GPF
        cmp     edx, 8
        jb      @s2
        nop
        nop
        nop // @s1 loop align
@s1:    sub     edx, 8
        cmp     [eax], ecx
        je      @ok0
        cmp     [eax + 4], ecx
        je      @ok4
        cmp     [eax + 8], ecx
        je      @ok8
        cmp     [eax + 12], ecx
        je      @ok12
        cmp     [eax + 16], ecx
        je      @ok16
        cmp     [eax + 20], ecx
        je      @ok20
        cmp     [eax + 24], ecx
        je      @ok24
        cmp     [eax + 28], ecx
        je      @ok28
        add     eax, 32
        cmp     edx, 8
        jae     @s1
@s2:    test    edx, edx
        jz      @z
        cmp     [eax], ecx
        je      @ok0
        dec     edx
        jz      @z
        cmp     [eax + 4], ecx
        je      @ok4
        dec     edx
        jz      @z
        cmp     [eax + 8], ecx
        je      @ok8
        dec     edx
        jz      @z
        cmp     [eax + 12], ecx
        je      @ok12
        dec     edx
        jz      @z
        cmp     [eax + 16], ecx
        je      @ok16
        dec     edx
        jz      @z
        cmp     [eax + 20], ecx
        je      @ok20
        dec     edx
        jz      @z
        cmp     [eax + 24], ecx
        je      @ok24
@z:     xor     eax, eax // return nil if not found
        ret
@ok0:   rep     ret
@ok28:  add     eax, 28
        ret
@ok24:  add     eax, 24
        ret
@ok20:  add     eax, 20
        ret
@ok16:  add     eax, 16
        ret
@ok12:  add     eax, 12
        ret
@ok8:   add     eax, 8
        ret
@ok4:   add     eax, 4
end;

function IntegerScanExists(P: PCardinalArray; Count: PtrInt; Value: cardinal): boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P, edx=Count, Value=ecx
        test    eax, eax
        jz      @z // avoid GPF
        cmp     edx, 8
        jae     @s1
        jmp     dword ptr [edx * 4 + @Table]
{$ifdef FPC} align   4 {$endif FPC}
@Table: dd      @z, @1, @2, @3, @4, @5, @6, @7
@s1:    // fast search by 8 integers (pipelined instructions)
        sub     edx, 8
        cmp     [eax], ecx
        je      @ok
        cmp     [eax + 4], ecx
        je      @ok
        cmp     [eax + 8], ecx
        je      @ok
        cmp     [eax + 12], ecx
        je      @ok
        cmp     [eax + 16], ecx
        je      @ok
        cmp     [eax + 20], ecx
        je      @ok
        cmp     [eax + 24], ecx
        je      @ok
        cmp     [eax + 28], ecx
        je      @ok
        add     eax, 32
        cmp     edx, 8
        jae     @s1
        jmp     dword ptr [edx * 4 + @Table]
@7:     cmp     [eax + 24], ecx
        je      @ok
@6:     cmp     [eax + 20], ecx
        je      @ok
@5:     cmp     [eax + 16], ecx
        je      @ok
@4:     cmp     [eax + 12], ecx
        je      @ok
@3:     cmp     [eax + 8], ecx
        je      @ok
@2:     cmp     [eax + 4], ecx
        je      @ok
@1:     cmp     [eax], ecx
        je      @ok
@z:     xor     eax, eax
        ret
@ok:    mov     al, 1
end;

{$else}

function ByteScanIndex(P: PByteArray; Count: PtrInt; Value: byte): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P, edx=Count, cl=Value
        push    ebx
        mov     ebx, eax
        movzx   eax, cl
        mov     ecx, ebx
        test    edx, edx
        jbe     @no
        // eax=Value, ecx=P edx=Count
        imul    eax, $01010101
        and     ecx, -16
        movd    xmm1, eax
        movaps  xmm0, [ecx]
        add     ecx, 16
        pshufd  xmm1, xmm1, 0
        sub     ecx, ebx
        pcmpeqb xmm0, xmm1
        pmovmskb eax, xmm0
        shl     eax, cl
        and     eax, $FFFF0000
        shr     eax, cl
        jnz     @fnd
        cmp     edx, ecx
        jbe     @no
{$ifdef FPC} align 16 {$else} {$ifdef HASALIGN} .align 16 {$endif}{$endif}
@by16:  movaps  xmm0, [ebx + ecx]
        add     ecx, 16
        pcmpeqb xmm0, xmm1
        pmovmskb eax, xmm0
        bsf     eax, eax
        jnz     @fnd2
        cmp     edx, ecx
        ja      @by16
@no:    mov     eax, -1
        pop     ebx
        ret
@fnd:   bsf     eax, eax
@fnd2:  lea     eax, [ecx + eax - 16]
        cmp     edx, eax
        jbe     @no
        pop     ebx
end;

function WordScanIndex(P: PWordArray; Count: PtrInt; Value: word): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P, edx=Count, cx=Value
        push    ebx
        mov     ebx, eax
        movzx   eax, cx
        mov     ecx, ebx
        test    edx, edx
        jbe     @no
        test    cl, 1
        jnz     @unal
        // eax=Value, ecx=P edx=Count
        movd    xmm1, eax
        and     ecx, -16
        punpcklwd xmm1, xmm1
        movaps  xmm0, [ecx]
        add     ecx, 16
        pshufd  xmm1, xmm1, 0
        sub     ecx, ebx
        pcmpeqw xmm0, xmm1
        pmovmskb eax, xmm0
        shl     eax, cl
        and     eax, $FFFF0000
        shr     eax, cl
        shr     ecx, 1
        test    eax, eax
        jz      @nxt
        bsf     eax, eax
@fnd:   shr     eax, 1
        lea     eax, [ecx + eax - 8]
        cmp     edx, eax
        jbe     @no
        pop     ebx
        ret
        nop // manual loop alignment
@by16:  movaps  xmm0, [ebx + ecx * 2]
        add     ecx, 8
        pcmpeqw xmm0, xmm1
        pmovmskb eax, xmm0
        bsf     eax, eax
        jnz     @fnd
@nxt:   cmp     edx, ecx
        ja      @by16
@no:    mov     eax, -1
        pop     ebx
        ret
        // 16bit-unaligned loop (seldom called)
@unal:  lea     ecx, [ecx + edx * 2]
        neg     edx
@unals: cmp     word ptr [ecx + edx * 2], ax
        jz      @unale
        inc     edx
        jnz     @unals
        jmp     @no
@unale: lea     eax, [ecx + edx * 2]
        sub     eax, ebx
        shr     eax, 1
        pop     ebx
end;

function IntegerScanIndex(P: PCardinalArray; Count: PtrInt; Value: cardinal): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P, edx=Count, ecx=Value
        push    ebx
        movd    xmm1, ecx
        mov     ebx, eax
        mov     eax, ecx
        mov     ecx, ebx
        test    edx, edx
        jbe     @no
        test    cl, 3
        jnz     @unal
        // eax=Value, ecx=P edx=Count
        and     ecx, -16
        movaps  xmm0, [ecx]
        add     ecx, 16
        pshufd  xmm1, xmm1, 0
        sub     ecx, ebx
        pcmpeqd xmm0, xmm1
        pmovmskb eax, xmm0
        shl     eax, cl
        and     eax, $FFFF0000
        shr     eax, cl
        shr     ecx, 2
        bsf     eax, eax
        jz      @nxt
@fnd:   shr     eax, 2
        lea     eax, [ecx + eax - 4]
        cmp     edx, eax
        jbe     @no
        pop     ebx
        ret
{$ifdef FPC} align 16 {$else} nop;nop;nop;nop;nop;nop {$endif FPC}
@by16:  movaps  xmm0, [ebx + ecx * 4]
        add     ecx, 4
        pcmpeqd xmm0, xmm1
        pmovmskb eax, xmm0
        bsf     eax, eax
        jnz     @fnd
@nxt:   cmp     edx, ecx
        ja      @by16
@no:    mov     eax, -1
        pop     ebx
        ret
        // 32bit-unaligned loop (seldom called)
@unal:  lea     ecx, [ecx + edx * 4]
        neg     edx
@unals: cmp     dword ptr [ecx + edx * 4], eax
        jz      @unale
        inc     edx
        jnz     @unals
        jmp     @no
@unale: lea     eax, [ecx + edx * 4]
        sub     eax, ebx
        shr     eax, 2
        pop     ebx
end;

{$endif HASNOSSE2}

procedure mul64x64(const left, right: QWord; out product: THash128Rec);
{$ifdef FPC}nostackframe; assembler; {$endif}
asm
        // adapted from FPC compiler output, which is much better than Delphi's here
        {$ifdef FPC}
        push    ebp
        mov     ebp, esp
        {$endif FPC}
        mov     ecx, eax
        mov     eax, dword ptr [ebp+8H]
        mul     dword ptr [ebp+10H]
        mov     dword ptr [ecx], eax
        mov     dword ptr [ebp-4H], edx
        mov     eax, dword ptr [ebp+8H]
        mul     dword ptr [ebp+14H]
        add     eax, dword ptr [ebp-4H]
        adc     edx, 0
        mov     dword ptr [ebp-10H], eax
        mov     dword ptr [ebp-0CH], edx
        mov     eax, dword ptr [ebp+0CH]
        mul     dword ptr [ebp+10H]
        add     eax, dword ptr [ebp-10H]
        adc     edx, 0
        mov     dword ptr [ecx+4H], eax
        mov     dword ptr [ebp-14H], edx
        mov     eax, dword ptr [ebp+0CH]
        mul     dword ptr [ebp+14H]
        add     eax, dword ptr [ebp-0CH]
        adc     edx, 0
        add     eax, dword ptr [ebp-14H]
        adc     edx, 0
        mov     dword ptr [ecx+8H], eax
        mov     dword ptr [ecx+0CH], edx
        {$ifdef FPC}
        pop     ebp
        {$endif FPC}
end;

function bswap32(a: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        bswap eax
end;

function bswap64({$ifdef FPC}constref{$else}const{$endif} a: QWord): QWord;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        {$ifdef FPC}
        mov     edx, dword ptr [eax]
        mov     eax, dword ptr [eax + 4]
        {$else}
        mov     edx, a.TQWordRec.L
        mov     eax, a.TQWordRec.H
        {$endif FPC}
        bswap   edx
        bswap   eax
end;

procedure bswap64array(A, B: PQWordArray; n: PtrInt);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebx
        push    esi
@1:     mov     ebx, dword ptr [eax]
        mov     esi, dword ptr [eax + 4]
        bswap   ebx
        bswap   esi
        mov     dword ptr [edx + 4], ebx
        mov     dword ptr [edx], esi
        add     eax, 8
        add     edx, 8
        dec     ecx
        jnz     @1
        pop     esi
        pop     ebx
end;

procedure LockedInc32(int32: PInteger);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock inc     dword ptr [int32]
end;

procedure LockedDec32(int32: PInteger);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock dec     dword ptr [int32]
end;

procedure LockedInc64(int64: PInt64);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock inc     dword ptr [int64]
        jz      @h
        ret
@h:lock inc     dword ptr [int64 + 4] // collision is very unlikely
end;

function InterlockedIncrement(var I: integer): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        mov     edx, 1
        xchg    eax, edx
   lock xadd    [edx], eax
        inc     eax
end;

function InterlockedDecrement(var I: integer): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        mov     edx, -1
        xchg    eax, edx
   lock xadd    [edx], eax
        dec     eax
end;

function StrLenSafe(S: pointer): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // slower than StrLenSSE2(), but won't read any byte beyond the page
        test  eax, eax
        jz    @z
        cmp   byte ptr [eax], 0
        je @0
        cmp   byte ptr [eax + 1], 0
        je @1
        cmp   byte ptr [eax + 2], 0
        je @2
        cmp   byte ptr [eax + 3], 0
        je @3
        push  eax
        and   eax, -4                // dword align reads
        nop                          // @s loop code alignment
@s:     add   eax, 4
        mov   edx, dword ptr [eax]   // 4 chars per loop
        lea   ecx, [edx - $01010101]
        not   edx
        and   edx, ecx
        and   edx, $80808080         // set byte to $80 at each #0 position
        jz    @s                     // loop until any #0 found
@set:   pop   ecx
        bsf   edx, edx               // find first #0 position
        shr   edx, 3                 // byte offset of first #0
        add   eax, edx               // address of first #0
        sub   eax, ecx               // length
@z:     ret
@0:     xor   eax, eax
        ret
@1:     mov   eax, 1
        ret
@2:     mov   eax, 2
        ret
@3:     mov   eax, 3
end;

function PosEx(const SubStr, S: RawUtf8; Offset: PtrUInt): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=SubStr, edx=S, ecx=Offset
        push    ebx
        push    esi
        push    edx
        test    eax, eax
        jz      @notfnd            // exit if SubStr=''
        test    edx, edx
        jz      @notfnd            // exit if S=''
        mov     esi, ecx
        mov     ecx, [edx - 4]     // length(S)
        mov     ebx, [eax - 4]     // length(SubStr)
        add     ecx, edx
        sub     ecx, ebx              // ecx = max start pos for full match
        lea     edx, [edx + esi - 1]  // edx = start position
        cmp     edx, ecx
        jg      @notfnd            // startpos > max start pos
        cmp     ebx, 1
        jle     @onec              // optimized loop for length(SubStr)<=1
        push    edi
        push    ebp
        lea     edi, [ebx - 2]     // edi = length(SubStr)-2
        mov     esi, eax           // esi = SubStr
        movzx   ebx, byte ptr [eax] // bl = search character
        nop; nop
@l:     cmp     bl, [edx]          // compare 2 characters per @l
        je      @c1fnd
@notc1: cmp     bl, [edx + 1]
        je      @c2fnd
@notc2: add     edx, 2
        cmp     edx, ecx            // next start position <= max start position
        jle     @l
        pop     ebp
        pop     edi
@notfnd:xor     eax, eax            // returns 0 if not fnd
        pop     edx
        pop     esi
        pop     ebx
        ret
@c1fnd: mov     ebp, edi            // ebp = length(SubStr)-2
@c1l:   movzx   eax, word ptr [esi + ebp]
        cmp     ax, [edx + ebp]     // compare 2 chars per @c1l (may include #0)
        jne     @notc1
        sub     ebp, 2
        jnc     @c1l
        pop     ebp
        pop     edi
        jmp     @setres
@c2fnd: mov     ebp, edi            // ebp = length(SubStr)-2
@c2l:   movzx   eax, word ptr [esi + ebp]
        cmp     ax, [edx + ebp + 1] // compare 2 chars per @c2l (may include #0)
        jne     @notc2
        sub     ebp, 2
        jnc     @c2l
        pop     ebp
        pop     edi
        jmp     @chkres
@onec:  jl      @notfnd             // needed for zero-length non-nil strings
        movzx   eax, byte ptr [eax]  // search character
@charl: cmp     al, [edx]
        je      @setres
        cmp     al, [edx + 1]
        je      @chkres
        add     edx, 2
        cmp     edx, ecx
        jle     @charl
        jmp     @notfnd
@chkres:cmp     edx, ecx           // check within ansistring
        jge     @notfnd
        add     edx, 1
@setres:pop     ecx                // ecx = S
        pop     esi
        pop     ebx
        neg     ecx
        lea     eax, [edx + ecx + 1]
end;

function StrComp(Str1, Str2: pointer): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // no branch taken in case of not equal first char
        cmp     eax, edx
        je      @zero  // same string or both nil
        test    eax, edx
        jz      @maynil
@1:     mov     cl, byte ptr [eax]
        mov     ch, byte ptr [edx]
        inc     eax
        inc     edx
        test    cl, cl
        jz      @exit
        cmp     cl, ch
        je      @1
@exit:  movzx   eax, cl
        movzx   edx, ch
        sub     eax, edx
        ret
@maynil:test    eax, eax  // Str1='' ?
        jz      @max
        test    edx, edx  // Str2='' ?
        jnz     @1
        mov     eax, 1
        ret
@max:   dec     eax
        ret
@zero:  xor     eax, eax
end;

function SortDynArrayInteger(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        mov     ecx, dword ptr [eax]
        mov     edx, dword ptr[edx]
        xor     eax, eax
        cmp     ecx, edx
        setl    cl
        setg    al
        movzx   ecx, cl
        sub     eax, ecx
end;

function SortDynArrayCardinal(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        mov     ecx, [eax]
        mov     edx, [edx]
        xor     eax, eax
        cmp     ecx, edx
        seta    al
        sbb     eax,0
end;

function SortDynArrayPointer(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        mov     ecx, [eax]
        mov     edx, [edx]
        xor     eax, eax
        cmp     ecx, edx
        seta    al
        sbb     eax,0
end;

function SortDynArrayInt64(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // Delphi x86 compiler is not efficient at compiling Int64 comparisons
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
        jnb     @p
@n:     mov     eax, -1
        ret
@0:     xor     eax, eax
        ret
@nz:    jl      @n
@p:     mov     eax, 1
end;

function SortDynArrayQWord(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // Delphi x86 compiler is not efficient, and oldest even incorrect
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
@nz:    jnb     @p
        mov     eax, -1
        ret
@0:     xor     eax, eax
        ret
@p:     mov     eax, 1
end;

function SortDynArrayAnsiString(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // x86 version optimized for AnsiString/RawUtf8/RawByteString types
        mov     eax, dword ptr [eax]
        mov     edx, dword ptr [edx]
        cmp     eax, edx
        je      @0
        test    eax, edx
        jz      @maynil
@f:     mov     cl, byte ptr [eax] // first char comparison (quicksort speedup)
        mov     ch, byte ptr [edx]
        cmp     cl, ch
        je      @s
        movzx   eax, cl
        movzx   edx, ch
        sub     eax, edx            // branchless execution on Quicksort/ReHash
        ret
@0:     xor     eax, eax
        ret
@maynil:test    edx, edx       // A or B may be ''
        jz      @1
        test    eax, eax
        jnz     @f
        dec     eax
        ret
@s:     push    ebx
        xor     ecx, ecx
        mov     ebx, dword ptr [eax - 4]
        sub     ebx, dword ptr [edx - 4]   // ebx = length(A)-length(B)
        push    ebx
        adc     ecx, -1
        and     ecx, ebx
        sub     ecx, dword ptr [eax - 4]   // ecx = -min(length(A),length(B))
        sub     eax, ecx
        sub     edx, ecx
{$ifdef FPC} align 16 {$endif} // is naturally aligned anyway
@by4:   mov     ebx, dword ptr [eax + ecx]  // compare 4 bytes per iteration
        xor     ebx, dword ptr [edx + ecx]
        jnz     @d
        add     ecx, 4
        js      @by4
@eq:    pop     eax            // all chars equal -> returns length(A)-length(B)
        pop     ebx
        ret
@d:     bsf     ebx, ebx       // char differs -> returns pbyte(A)^-pbyte(B)^
        shr     ebx, 3
        add     ecx, ebx
        jns     @eq
        movzx   eax, byte ptr [eax + ecx]
        movzx   edx, byte ptr [edx + ecx]
        pop     ebx
        pop     ebx
        sub     eax, edx
        ret
@1:     mov     eax, 1
end;

function SortDynArrayDouble(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        fld     qword ptr [eax]
        fcomp   qword ptr [edx]
        fstsw   ax
        sahf
        jz      @0
@nz:    jnb     @p
        mov     eax, -1
        ret
@0:     xor     eax, eax
        ret
@p:     mov     eax, 1
end;

function SortDynArraySingle(const A, B): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        fld     dword ptr [eax]
        fcomp   dword ptr [edx]
        fstsw   ax
        sahf
        jz      @0
@nz:    jnb     @p
        mov     eax, -1
        ret
@0:     xor     eax, eax
        ret
@p:     mov     eax, 1
end;

procedure DynArrayHashTableAdjust(P: PIntegerArray; deleted: integer; count: PtrInt);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P edx=deleted ecx=count
        push    ebx
        mov     ebx, eax
        xor     eax, eax
@by8:   sub     ecx, 8
        cmp     edx, dword ptr [ebx]
        setl    al
        sub     dword ptr [ebx], eax
        cmp     edx, dword ptr [ebx + 4H]
        setl    al
        sub     dword ptr [ebx + 4H], eax
        cmp     edx, dword ptr [ebx + 8H]
        setl    al
        sub     dword ptr [ebx + 8H], eax
        cmp     edx, dword ptr [ebx + 0CH]
        setl    al
        sub     dword ptr [ebx + 0CH], eax
        cmp     edx, dword ptr [ebx + 10H]
        setl    al
        sub     dword ptr [ebx + 10H], eax
        cmp     edx, dword ptr [ebx + 14H]
        setl    al
        sub     dword ptr [ebx + 14H], eax
        cmp     edx, dword ptr [ebx + 18H]
        setl    al
        sub     dword ptr [ebx + 18H], eax
        cmp     edx, dword ptr [ebx + 1CH]
        setl    al
        sub     dword ptr [ebx + 1CH], eax
        add     ebx, 32
        cmp     ecx, 8
        jge     @by8
        test    ecx, ecx
        jz      @ok
        // trailing indexes (never appearing within DYNARRAYHASH_PO2 range)
@by1:   cmp     edx, dword ptr [ebx]
        setl    al
        sub     dword ptr [ebx], eax
        add     ebx, 4
        dec     ecx
        jnz     @by1
@ok:    pop     ebx
end;

procedure DynArrayHashTableAdjust16(P: PWordArray; deleted: cardinal; count: PtrInt);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=P dx=deleted ecx=count
        push    ebx
        mov     ebx, eax
        xor     eax, eax
@by8:   sub     ecx, 8
        cmp     dx, word ptr [ebx]
        setl    al
        sub     word ptr [ebx], ax
        cmp     dx, word ptr [ebx + 2]
        setl    al
        sub     word ptr [ebx + 2], ax
        cmp     dx, word ptr [ebx + 4]
        setl    al
        sub     word ptr [ebx + 4], ax
        cmp     dx, word ptr [ebx + 6]
        setl    al
        sub     word ptr [ebx + 6], ax
        cmp     dx, word ptr [ebx + 8]
        setl    al
        sub     word ptr [ebx + 8], ax
        cmp     dx, word ptr [ebx + 10]
        setl    al
        sub     word ptr [ebx + 10], ax
        cmp     dx, word ptr [ebx + 12]
        setl    al
        sub     word ptr [ebx + 12], ax
        cmp     dx, word ptr [ebx + 14]
        setl    al
        sub     word ptr [ebx + 14], ax
        add     ebx, 16
        cmp     ecx, 8
        jge     @by8
        test    ecx, ecx
        jz      @ok
        // trailing indexes (never appearing within DYNARRAYHASH_PO2 range)
@by1:   cmp     dx, word ptr [ebx]
        setl    al
        sub     word ptr [ebx], ax
        add     ebx, 2
        dec     ecx
        jnz     @by1
@ok:    pop     ebx
end;

function GetBitsCountSse42(value: PtrInt): PtrInt;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=value
        {$ifdef HASAESNI}
        popcnt  eax, eax
        {$else} // oldest Delphi don't support this opcode
        db      $f3,$0f,$B8,$c0
        {$endif HASAESNI}
end;

function crc32cby4sse42(crc, value: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=crc, edx=value
        {$ifdef HASAESNI}
        crc32   eax, edx
        {$else} // oldest Delphi don't support this opcode
        db      $F2, $0F, $38, $F1, $C2
        {$endif HASAESNI}
end;

procedure crcblocksse42(crc128, data128: PBlock128);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=crc128, edx=data128
        mov     ecx, eax
        {$ifdef HASAESNI}
        mov     eax, dword ptr [ecx]
        crc32   eax, dword ptr [edx]
        mov     dword ptr [ecx], eax
        mov     eax, dword ptr [ecx + 4]
        crc32   eax, dword ptr [edx + 4]
        mov     dword ptr [ecx + 4], eax
        mov     eax, dword ptr [ecx + 8]
        crc32   eax, dword ptr [edx + 8]
        mov     dword ptr [ecx + 8], eax
        mov     eax, dword ptr [ecx + 12]
        crc32   eax, dword ptr [edx + 12]
        mov     dword ptr [ecx + 12], eax
        {$else} // oldest Delphi don't support these opcodes
        mov     eax, dword ptr [ecx]
        db      $F2, $0F, $38, $F1, $02
        mov     dword ptr [ecx], eax
        mov     eax, dword ptr [ecx + 4]
        db      $F2, $0F, $38, $F1, $42, $04
        mov     dword ptr [ecx + 4], eax
        mov     eax, dword ptr [ecx + 8]
        db      $F2, $0F, $38, $F1, $42, $08
        mov     dword ptr [ecx + 8], eax
        mov     eax, dword ptr [ecx + 12]
        db      $F2, $0F, $38, $F1, $42, $0C
        mov     dword ptr [ecx + 12], eax
        {$endif HASAESNI}
end;

procedure crcblockssse42(crc128, data128: PBlock128; count: integer);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
          // eax=crc128 edx=data128 ecx=count
          push    ebx
          push    esi
          push    edi
          push    ebp
          test    count, count
          jle     @z
          mov     ebp, count
          mov     esi, crc128
          mov     edi, data128
          mov     eax, dword ptr [esi]
          mov     ebx, dword ptr [esi + 4]
          mov     ecx, dword ptr [esi + 8]
          mov     edx, dword ptr [esi + 12]
          {$ifdef HASAESNI}
{$ifdef FPC} align 16 {$else} {$ifdef HASALIGN} .align 16 {$endif}{$endif}
@s:       crc32   eax, dword ptr [edi]
          crc32   ebx, dword ptr [edi + 4]
          crc32   ecx, dword ptr [edi + 8]
          crc32   edx, dword ptr [edi + 12]
          {$else}  // oldest Delphi don't support these opcodes
@s:       db $F2, $0F, $38, $F1, $07
          db $F2, $0F, $38, $F1, $5F, $04
          db $F2, $0F, $38, $F1, $4F, $08
          db $F2, $0F, $38, $F1, $57, $0C
          {$endif HASAESNI}
          add     edi, 16
          dec     ebp
          jnz     @s
          mov     dword ptr [esi], eax
          mov     dword ptr [esi + 4], ebx
          mov     dword ptr [esi + 8], ecx
          mov     dword ptr [esi + 12], edx
@z:       pop     ebp
          pop     edi
          pop     esi
          pop     ebx
end;

function crc32csse42(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // eax=crc, edx=buf, ecx=len
        not     eax
        test    ecx, ecx
        jz      @0
        test    edx, edx
        jz      @0
        jmp     @align
@a:     {$ifdef HASAESNI}
        crc32   eax, byte ptr [edx]
        {$else} // oldest Delphi don't support these opcodes
        db      $F2, $0F, $38, $F0, $02
        {$endif HASAESNI}
        inc     edx
        dec     ecx
        jz      @0
@align: test    dl, 3
        jnz     @a
        push    ecx
        shr     ecx, 3
        jnz     @by8
@rem:   pop     ecx
        test    cl, 4
        jz      @4
        {$ifdef HASAESNI}
        crc32   eax, dword ptr [edx]
        {$else}
        db      $F2, $0F, $38, $F1, $02
        {$endif HASAESNI}
        add     edx, 4
@4:     test    cl, 2
        jz      @2
        {$ifdef HASAESNI}
        crc32   eax, word ptr [edx]
        {$else}
        db      $66, $F2, $0F, $38, $F1, $02
        {$endif HASAESNI}
        add     edx, 2
@2:     test    cl, 1
        jz      @0
        {$ifdef HASAESNI}
        crc32   eax, byte ptr [edx]
        {$else}
        db      $F2, $0F, $38, $F0, $02
        {$endif HASAESNI}
@0:     not     eax
        ret
        {$ifdef HASAESNI}
@by8:   crc32   eax, dword ptr [edx]
        crc32   eax, dword ptr [edx + 4]
        {$else}
@by8:   db      $F2, $0F, $38, $F1, $02
        db      $F2, $0F, $38, $F1, $42, $04
        {$endif HASAESNI}
        add     edx, 8
        dec     ecx
        jnz     @by8
        jmp     @rem
end;

function SynLZdecompress1(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -24
        mov     esi, ecx
        mov     ebx, eax
        add     edx, eax
        mov     [esp+8H], esi
        mov     [esp+10H], edx
        movzx   eax, word ptr [ebx]
        mov     [esp], eax
        or      eax,eax
        je      @@0917
        add     ebx, 2
        mov     eax, [esp]
        test    ah, 80H
        jz      @@0907
        and     eax, 7FFFH
        movzx   edx, word ptr [ebx]
        shl     edx, 15
        or      eax, edx
        mov     [esp], eax
        add     ebx, 2
@@0907: lea     ebp, [esi - 1]
@@0908: mov     ecx, [ebx]
        add     ebx, 4
        mov     [esp+14H], ecx
        mov     edi, 1             // edi=CWbit
        cmp     ebx, [esp+10H]
        jnc     @@0917
@@0909: mov     ecx, [esp+14H]
@@090A: test    ecx, edi
        jnz     @@0911
        mov     al, [ebx]
        inc     ebx
        mov     [esi], al
        inc     esi
        cmp     ebx, [esp+10H]
        lea     eax, [esi-3]
        jnc     @@0917
        cmp     eax, ebp
        jbe     @@0910
        inc     ebp
        mov     eax, [ebp]
        mov     edx, eax
        shr     eax, 12
        xor     eax, edx
        and     eax, 0FFFH
        mov     [esp+1CH + eax * 4], ebp
@@0910: add     edi, edi
        jnz     @@090A
        jmp     @@0908
@@0911: movzx   edx, word ptr [ebx]
        add     ebx, 2
        mov     eax, edx
        and     edx, 0FH
        add     edx, 2
        shr     eax, 4
        cmp     edx,2
        jnz     @@0912
        movzx   edx, byte ptr [ebx]
        inc     ebx
        add     edx, 18
@@0912: mov     eax, [esp+1CH + eax * 4]
        mov     ecx, esi
        mov     [esp+18H], edx
        sub     ecx, eax
        // inlined optimized move()
        cmp     ecx, edx
        jl      @@ovlap   // overlapping content requires per-byte copy
        cmp     edx, 32
        ja      @large
        sub     edx, 8
        jg      @9_32
{$ifdef HASNOSSE2}
        // slowest x87 FPU code on very old CPU with no SSE2 support
        mov     ecx, [eax]
        mov     eax, [eax + 4]     // always copy 8 bytes for 0..8
        mov     [esi], ecx         // safe since src_endmatch := src_end-(6+5)
        mov     [esi + 4], eax
        jmp     @movend
@9_32:  fild    qword ptr [eax + edx]
        fild    qword ptr [eax]
        cmp     edx, 8
        jle     @16
        fild    qword ptr [eax + 8]
        cmp     edx, 16
        jle     @24
        fild    qword ptr [eax + 16]
        fistp   qword ptr [esi + 16]
@24:    fistp   qword ptr [esi + 8]
@16:    fistp   qword ptr [esi]
        fistp   qword ptr [esi + edx]
        jmp     @movend
        nop
@large: push    esi
        fild    qword ptr [eax]
        lea     eax, [eax + edx - 8]
        lea     edx, [esi + edx - 8]
        fild    qword ptr [eax]
        push    edx
        neg     edx
        and     esi, -8
        lea     edx, [edx + esi + 8]
        pop     esi
@lrgnxt:fild    qword ptr [eax + edx]
        fistp   qword ptr [esi + edx]
        add     edx, 8
        jl      @lrgnxt
        fistp   qword ptr [esi]
        pop     esi
        fistp   qword ptr [esi]
{$else} // inlined SSE2 move
        movq    xmm0, qword ptr [eax]
        movq    qword ptr [esi], xmm0
        jmp     @movend
@9_32:  movq    xmm0, qword ptr [eax + edx]
        movq    xmm1, qword ptr [eax]
        cmp     edx, 8
        jle     @16
        movq    xmm2, qword ptr [eax + 8]
        cmp     edx, 16
        jle     @24
        movq    xmm3, qword ptr [eax + 16]
        movq    qword ptr [esi + 16], xmm3
@24:    movq    qword ptr [esi + 8], xmm2
@16:    movq    qword ptr [esi], xmm1
        movq    qword ptr [esi + edx], xmm0
        jmp     @movend
@large: push    esi
        movups  xmm2, dqword ptr [eax]
        lea     eax, [eax + edx - 16]
        lea     edx, [esi + edx - 16]
        movups  xmm1, dqword ptr [eax]
        push    edx
        neg     edx
        and     esi, -16
        lea     edx, [edx + esi + 16]
        pop     esi
@lrgnxt:movups  xmm0, dqword ptr [eax + edx]
        movaps  dqword ptr [esi + edx], xmm0
        add     edx, 16
        jl      @lrgnxt
        movups  dqword ptr [esi], xmm1
        pop     esi
        movups  dqword ptr [esi], xmm2
{$endif HASNOSSE2}
@movend:cmp     esi, ebp
        jbe     @@0916
@@0915: inc     ebp
        mov     edx, [ebp]
        mov     eax, edx
        shr     edx, 12
        xor     eax, edx
        and     eax, 0FFFH
        mov     [esp+1CH + eax * 4], ebp
        cmp     esi, ebp
        ja      @@0915
@@0916: add     esi, [esp+18H]
        cmp     ebx, [esp+10H]
        jnc     @@0917
        add     edi, edi
        lea     ebp, [esi - 1]
        jz      @@0908
        jmp     @@0909
@@ovlap:push    ebx
        push    esi
        lea     ebx, [eax + edx]
        add     esi, edx
        neg     edx
@s:     mov     al, [ebx + edx]
        mov     [esi + edx], al
        inc     edx
        jnz     @s
        pop     esi
        pop     ebx
        jmp     @movend
@@0917: mov     eax, [esp]
        add     esp, 16412
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
end;

function SynLZcompress1(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    ebp
        push    ebx
        push    esi
        push    edi
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -4092
        push    eax
        add     esp, -32
        mov     esi, eax // esi=src
        mov     edi, ecx // edi=dst
        mov     [esp+08H], ecx
        mov     eax, edx
        cmp     eax, 32768
        jl      @@0889
        or      ax, 8000H
        mov     [edi], eax
        mov     eax, edx
        shr     eax, 15
        mov     [edi + 2], eax
        add     edi, 4
        jmp     @@0891
@@0890: mov     eax, 2
        jmp     @@0904
@@0889: mov     [edi], eax
        test    eax, eax
        jz      @@0890
        add     edi, 2
@@0891: lea     eax, [edx + esi]
        mov     [esp+18H], edi
        mov     [esp+0CH], eax
        sub     eax, 11
        mov     [esp+4], eax
        xor     eax, eax
        lea     ebx, [esp+24H] // reset offsets lookup table
        {$ifdef HASNOSSE2}
        mov     ecx, 1024
@@089I: mov     [ebx], eax
        mov     [ebx + 4], eax
        mov     [ebx + 8], eax
        mov     [ebx + 12], eax
        add     ebx, 16
        {$else}
        pxor    xmm0, xmm0
        mov     ecx, 256
@@089I: movups  dqword ptr [ebx], xmm0
        movups  dqword ptr [ebx + 16], xmm0
        movups  dqword ptr [ebx + 32], xmm0
        movups  dqword ptr [ebx + 48], xmm0
        add     ebx, 64
        {$endif HASNOSSE2}
        dec     ecx
        jnz     @@089I
        mov     [edi], eax
        add     edi, 4
        mov     ebx, 1 // ebx=1 shl CWbit
        // main loop:
        cmp     esi, [esp+4]
        ja      @@0900
@@0892: mov     edx, [esi]
        mov     eax, edx
        shr     edx, 12
        xor     edx, eax
        and     edx, 0FFFH
        mov     ebp, [esp+24H + edx * 4]
        mov     ecx, [esp+4024H + edx * 4]
        mov     [esp+24H + edx * 4], esi
        xor     ecx, eax
        test    ecx, 0FFFFFFH
        mov     [esp+4024H + edx * 4], eax
        jnz     @@0897
        mov     eax, esi
        or      ebp, ebp
        jz      @@0897
        sub     eax, ebp
        mov     ecx, [esp+18H]
        cmp     eax, 2
        jle     @@0897
        add     esi, 2
        or      dword ptr [ecx], ebx
        mov     ecx, [esp+0CH]
        add     ebp, 2
        mov     eax, 1
        sub     ecx, esi
        dec     ecx
        mov     [esp], ecx
        cmp     ecx, 271
        jl      @@0894
        mov     dword ptr [esp], 271
        jmp     @@0894
@@0893: inc     eax
@@0894: mov     ecx, [ebp + eax]
        cmp     cl, [esi + eax]
        jnz     @@0895
        cmp     eax, [esp]
        jge     @@0895
        inc     eax
        cmp     ch, [esi + eax]
        jnz     @@0895
        shr     ecx, 16
        cmp     eax, [esp]
        jge     @@0895
        inc     eax
        cmp     cl, [esi + eax]
        jnz     @@0895
        cmp     eax, [esp]
        jge     @@0895
        inc     eax
        cmp     ch, [esi + eax]
        jnz     @@0895
        cmp     eax, [esp]
        jl      @@0893
@@0895: add     esi, eax
        shl     edx, 4
        cmp     eax, 15
        jg      @@0896
        or      eax, edx
        mov     word ptr [edi], ax
        add     edi, 2
        jmp     @@0898
@@0896: sub     eax, 16
        mov     [edi], dx
        mov     [edi + 2H], al
        add     edi, 3
        jmp     @@0898
@@0897: mov     al, [esi] // movsb is actually slower!
        mov     [edi], al
        inc     esi
        inc     edi
@@0898: add     ebx, ebx
        jz      @@0899
        cmp     esi, [esp+4]
        jbe     @@0892
        jmp     @@0900
@@0899: mov     [esp+18H], edi
        mov     [edi], ebx
        inc     ebx
        add     edi, 4
        cmp     esi, [esp+4]
        jbe     @@0892
@@0900: cmp     esi, [esp+0CH]
        jnc     @@0903
@@0901: mov     al, [esi]
        mov     [edi], al
        inc     esi
        inc     edi
        add     ebx, ebx
        jz      @@0902
        cmp     esi, [esp+0CH]
        jc      @@0901
        jmp     @@0903
@@0902: mov     [edi], ebx
        inc     ebx
        add     edi, 4
        cmp     esi, [esp+0CH]
        jc      @@0901
@@0903: mov     eax, edi
        sub     eax, [esp+08H]
@@0904: add     esp, 32804
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
end;

function RdRand32: cardinal;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // rdrand eax: same opcodes for x86 and x64
        db $0f, $c7, $f0
        // returns in eax, ignore carry flag (eax=0 won't hurt)
end;

function Rdtsc: Int64;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        // returns the TSC in EDX:EAX
        rdtsc
end;

function StrCntDecFree(var refcnt: TStrCnt): boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock dec dword ptr [refcnt]  // =longint on Delphi and FPC 32-bit
        setbe   al
end; // don't check for ismultithread global since lock is cheaper on new CPUs

function DACntDecFree(var refcnt: TDACnt): boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock dec dword ptr [refcnt]  // =longint on Delphi and FPC 32-bit
        setbe   al
end; // don't check for ismultithread global since lock is cheaper on new CPUs

function LockedExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt): boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        xchg    eax, ecx
   lock cmpxchg dword ptr [ecx], edx
        setz    al
end;

procedure LockedAdd(var Target: PtrUInt; Increment: PtrUInt);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock add     dword ptr [Target], Increment
end;

procedure LockedAdd32(var Target: cardinal; Increment: cardinal);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock add     dword ptr [Target], Increment
end;

procedure LockedDec(var Target: PtrUInt; Decrement: PtrUInt);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
   lock sub     dword ptr [Target], Decrement
end;

function IsXmmYmmOSEnabled: boolean;
{$ifdef FPC}nostackframe; assembler;{$endif}
asm // see https://software.intel.com/en-us/blogs/2011/04/14/is-avx-enabled
        xor     ecx, ecx  // get control register XCR0 = XFEATURE_ENABLED_MASK
        db  $0f, $01, $d0 // XGETBV reads XCR0 into EDX:EAX
        and     eax, 6    // check OS enabled both XMM (bit 1) and YMM (bit 2)
        cmp     al, 6
        sete    al
end;

procedure GetCpuid(cpueax, cpuecx: cardinal; var Registers: TIntelRegisters);
{$ifdef FPC}nostackframe; assembler;{$endif}
asm
        push    esi
        push    edi
        push    ebx
        mov     edi, cpueax
        mov     ebx, cpuecx
        mov     esi, Registers
        pushfd
        pop     eax
        mov     edx, eax
        xor     eax, $200000
        push    eax
        popfd
        pushfd
        pop     eax
        xor     eax, edx // does this CPU support the cpuid opcode?
        jz      @nocpu
        mov     eax, edi
        mov     ecx, ebx
        cpuid
        mov     TIntelRegisters(esi).&eax, eax
        mov     TIntelRegisters(esi).&ebx, ebx
        mov     TIntelRegisters(esi).&ecx, ecx
        mov     TIntelRegisters(esi).&edx, edx
@nocpu: pop     ebx
        pop     edi
        pop     esi
end;

{$ifdef CPU32DELPHI}

// inspired by ValExt_JOH_PAS_8_a by John O'Harrow - calls Delphi System.@Pow10
function GetExtended(P: PUtf8Char; out err: integer): TSynExtended;
const
  Ten: double = 10.0;
asm
        // in: eax=text, edx=@err  out: st(0)=result
        push    ebx                 // save used registers
        push    esi
        push    edi
        mov     esi, eax            // string pointer
        push    eax                 // save for error condition
        xor     ebx, ebx
        push    eax                 // allocate local storage for loading fpu
        test    esi, esi
        jz      @nil                // nil string
@trim:  movzx   ebx, byte ptr [esi] // strip leading spaces
        inc     esi
        cmp     bl, ' '
        je      @trim
        xor     ecx, ecx            // clear sign flag
        fld     qword[Ten]          // load 10 into fpu
        xor     eax, eax            // zero number of decimal places
        fldz                        // zero result in fpu
        cmp     bl, '0'
        jl      @chksig             // check for sign character
@dig1:  xor     edi, edi            // zero exponent value
@digl:  sub     bl, '0'
        cmp     bl, 9
        ja      @frac               // non-digit
        mov     cl, 1               // set digit found flag
        mov     [esp], ebx          // store for fpu use
        fmul    st(0), st(1)        // multply by 10
        fiadd   dword ptr [esp]     // add next digit
        movzx   ebx, byte ptr [esi] // get next char
        inc     esi
        test    bl, bl              // end reached?
        jnz     @digl               // no,get next digit
        jmp     @finish             // yes,finished
@chksig:cmp     bl, '-'
        je      @minus
        cmp     bl, '+'
        je      @sigset
@gdig1: test    bl, bl
        jz      @error              // no digits found
        jmp     @dig1
@minus: mov     ch, 1               // set sign flag
@sigset:movzx   ebx, byte ptr [esi] // get next char
        inc     esi
        jmp     @gdig1
@frac:  cmp     bl, '.' - '0'
        jne     @exp                // no decimal point
        movzx   ebx, byte ptr [esi] // get next char
        test    bl, bl
        jz      @dotend             // string ends with '.'
        inc     esi
@fracl: sub     bl, '0'
        cmp     bl, 9
        ja      @exp                // non-digit
        mov     [esp], ebx
        dec     eax                 // -(number of decimal places)
        fmul    st(0), st(1)        // multply by 10
        fiadd   dword ptr [esp]     // add next digit
        movzx   ebx, byte ptr [esi] // get next char
        inc     esi
        test    bl, bl              // end reached?
        jnz     @fracl              // no, get next digit
        jmp     @finish             // yes, finished (no exponent)
@dotend:test    cl, cl              // any digits found before '.'?
        jnz     @finish             // yes, valid
        jmp     @error              // no,invalid
@exp:   or      bl, $20
        cmp     bl, 'e' - '0'
        jne     @error              // not 'e' or 'e'
        movzx   ebx, byte ptr [esi] // get next char
        inc     esi
        mov     cl, 0               // clear exponent sign flag
        cmp     bl, '-'
        je      @minexp
        cmp     bl, '+'
        je      @expset
        jmp     @expl
@minexp:mov     cl, 1               // set exponent sign flag
@expset:movzx   ebx, byte ptr [esi] // get next char
        inc     esi
@expl:  sub     bl, '0'
        cmp     bl, 9
        ja      @error              // non-digit
        lea     edi, [edi + edi * 4]// multiply by 10
        add     edi, edi
        add     edi, ebx            // add next digit
        movzx   ebx, byte ptr [esi] // get next char
        inc     esi
        test    bl, bl              // end reached?
        jnz     @expl               // no, get next digit
@endexp:test    cl, cl              // positive exponent?
        jz      @finish             // yes, keep exponent value
        neg     edi                 // no, negate exponent value
@finish:add     eax, edi            // exponent value - number of decimal places
        mov     [edx], ebx          // result code = 0
        jz      @pow                // no call to _pow10 needed
        cmp     eax, 308
        jge     @oor                // limit to < 1.7 x 10^308 double range
        cmp     eax, -324
        jle     @oor                // limit to > 5.0 x 10^-324 range
@oors:  mov     edi, ecx            // save decimal sign flag
        call    System.@Pow10       // raise to power of 10
        mov     ecx, edi            // restore decimal sign flag
@pow:   test    ch, ch              // decimal sign flag set?
        jnz     @negate             // yes, negate value
@ok:    add     esp, 8              // dump local storage and string pointer
@exit:  ffree   st(1)               // remove ten value from fpu
        pop     edi                 // restore used registers
        pop     esi
        pop     ebx
        ret                         // finished
@negate:fchs                        // negate result in fpu
        jmp     @ok
@oor:   inc     esi                 // force result code = 1
        mov     [edx], esi          // set result code
        xor     eax, eax            // set exponent = 0 (as pure pascal version)
        jmp     @oors
@nil:   inc     esi                 // force result code = 1
        fldz                        // result value = 0
@error: pop     ebx                 // dump local storage
        pop     eax                 // string pointer
        sub     esi, eax            // error offset
        mov     [edx], esi          // set result code
        test    ch, ch              // decimal sign flag set?
        jz      @exit               // no,exit
        fchs                        // yes. negate result in fpu
        jmp     @exit               // exit setting result code
end;

// FPC will properly inline multiplication by reciprocal
procedure Div100(Y: cardinal; var res: TDiv100Rec);
asm
        mov     dword ptr [edx].TDiv100Rec.M, eax
        mov     ecx, edx
        mov     edx, eax
        mov     eax, 1374389535
        mul     edx
        shr     edx, 5
        mov     dword ptr [ecx].TDiv100Rec.D, edx
        imul    eax, edx, 100
        sub     dword ptr [ecx].TDiv100Rec.M, eax
end;

// those functions are intrinsics with FPC :)

function BSRdword(c: cardinal): cardinal;
asm
        bsr     eax, eax
        jnz     @nz
        mov     eax, 255
@nz:
end;

function BSRqword(const q: qword): cardinal;
asm
        bsr     eax, [esp + 8]
        jz      @1
        add     eax, 32
        ret
@1:     bsr     eax, [esp + 4]
        jnz     @2
        mov     eax, 255
@2:
end;

{$endif CPU32DELPHI}


