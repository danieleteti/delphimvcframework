{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   x86_64 assembly used by mormot.core.base.pas
}

{$ifdef FPC}
  // disabled some FPC paranoid warnings
  {$WARN 7119 off : Exported/global symbols should be accessed via the GOT }
  {$WARN 7121 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits]" }
  {$WARN 7122 off : Check size of memory operand "$1: memory-operand-size is $2 bits, but expected [$3 bits + $4 byte offset]" }
  {$WARN 7123 off : Check "$1: offset of memory operand is negative "$2 byte" }
{$endif FPC}

{$ifdef ASMX64}

{
 FillCharFast/MoveFast implementation notes:
 - assume ERBMS is available (cpuid flag may not be propagated within VMs)
 - use branchless sub-functions for smallest buffers, then SSE2 up to 255 bytes
 - use "rep movsb" for 256..512K input (should work on all CPUs, even oldest)
 - don't use backward "std rep movsb" since it is not involved by ERMBS (slow)
 - use non-temporal SSE2 or AVX loop >= 512KB (to not pollute the CPU cache)
 - will check X64CpuFeatures global to adjust the algorithm if cpuAVX is set
 - regarding benchmark, run TTestLowLevelCommon.CustomRTL on x86_64
  -> FillCharFast/MoveFast seems faster, especially for small lengths (strings)
  -> Delphi RTL is slower than FPC's, and it does not support AVX assembly yet
}

const
  // non-temporal writes should bypass the cache when the size is bigger than
  // half the size of the largest level cache = 512KB, assuming a low 1MB cache
  // - today CPUs could have 144MB of (3D) cache (!) so we favor a fixed value
  // and rely on the CPU hardware cache prefetch when accessing the data
  NONTEMPORALSIZE = 512 * 1024;

  {$ifdef NO_ERMS}
    {$undef WITH_ERMS}
  {$else}
    {$define WITH_ERMS}
    // we enable forward rep movsb/stosb over SSE2MAXSIZE=256 bytes on x86_64
    // and we don't try to detect it because CPUID is unset in some VMs
  {$endif NO_ERMS}

  // minimum size triggering ASMX64AVXNOCONST or WITH_ERMS optimized asm
  // - pre-ERMS expects at least 144/256 bytes, IvyBridge+ with ERMS is good
  // from 64 - copy_user_enhanced_fast_string() in recent Linux kernel uses 64
  // see https://stackoverflow.com/a/43837564/458259 for explanations and timing
  // - see also mormot.core.fpcx64mm.pas as reference
  SSE2MAXSIZE = 256;

  // identify Intel/AMD AVX2+BMI support at Haswell level
  CPUAVX2HASWELL = [cfAVX2, cfSSE42, cfBMI1, cfBMI2, cfCLMUL];

procedure MoveFast(const src; var dst; cnt: PtrInt);
{$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif} // rcx/rdi=src rdx/rsi=dst r8/rdx=cnt
        mov     rax, cnt             // rax=r8/rdx=cnt
        lea     r10, [rip + @jmptab] // 0..16 dedicated sub-functions
        sub     rax, 16
        ja      @up16                // >16 or <0
        {$ifdef WIN64ABI}    // circumvent FPC asm bug and adapt to xmm ABI
        jmp     qword ptr [r10 + r8 * 8]
@up16:  // >16 or <0
        jng     @z  // <0
        movups  xmm0, oword ptr [src + rax]    // last 16   = xmm0
        movups  xmm1, oword ptr [src]          // first 16  = xmm1
        cmp     rax, 96 - 16
        {$else}
        jmp     qword ptr [r10 + rdx * 8]
@neg:   ret
@up16:  // >16 or <0
        jng     @neg  // <0
        mov     r8, rdx
        movups  xmm0, oword ptr [src + rax]    // last 16   = xmm0
        movups  xmm1, oword ptr [src]          // first 16  = xmm1
        cmp     rdx, 144  // more volatile xmm registers on SystemV ABI
        {$endif WIN64ABI}
        ja      @lrg         // >96/144
        // cnt = 17..96/144
        cmp     al, $10
        jbe     @sml10
        movups  xmm2, oword ptr [src + $10]    // second 16
        cmp     al, $20
        jbe     @sml20
        movups  xmm3, oword ptr [src + $20]    // third 16
        cmp     al, $30
        jbe     @sml30
        movups  xmm4, oword ptr [src + $30]    // fourth 16
        cmp     al, $40
        jbe     @sml40
        movups  xmm5, oword ptr [src + $40]    // fifth 16
        // xmm0..xmm5 are volatile on both Win64 and SystemV ABI
        // xmm6 and up are also volatile on SystemV ABI so allow more bytes
        {$ifdef SYSVABI}
        cmp     al, $50
        jbe     @sml50
        movups  xmm6, oword ptr [src + $50]
        cmp     al, $60
        jbe     @sml60
        movups  xmm7, oword ptr [src + $60]
        cmp     al, $70
        jbe     @sml70
        movups  xmm8, oword ptr [src + $70]
        // more registers increases code size ([dst+$80]) so are not used
        movups  oword ptr [dst + $70], xmm8
@sml70: movups  oword ptr [dst + $60], xmm7
@sml60: movups  oword ptr [dst + $50], xmm6
@sml50: {$endif SYSVABI}
        movups  oword ptr [dst + $40], xmm5    // fifth 16
@sml40: movups  oword ptr [dst + $30], xmm4    // fourth 16
@sml30: movups  oword ptr [dst + $20], xmm3    // third 16
@sml20: movups  oword ptr [dst + $10], xmm2    // second 16
@sml10: movups  oword ptr [dst],       xmm1    // first 16
        movups  oword ptr [dst + rax], xmm0    // last 16 (may be overlapping)
@z:     ret
@lrg:   // cnt > 96/144 or cnt < 0
        mov     r11d, NONTEMPORALSIZE
        mov     r10, dst
        add     rax, 16       // restore rax=cnt as expected below
        jl      @z            // cnt < 0
        sub     r10, src
        jz      @z            // src=dst
        cmp     r10, cnt      // move backwards if unsigned(dst-src) < cnt
        jb      @lrgbwd
        // forward ERMSB/SSE2/AVX move for cnt > 96/144 bytes
        mov     r9, dst       // dst will be 16/32 bytes aligned for writes
        {$ifdef WITH_ERMS}
        {$ifdef WIN64ABI}   // 145 bytes seems good enough for ERMSB on a server
        cmp     rax, SSE2MAXSIZE
        jb      @fsse2      // 97..255 bytes may be not enough for ERMSB nor AVX
        {$endif WIN64ABI}
        cmp     rax, r11
        jae     @lrgfwd       // non-temporal move > 512KB is better than ERMSB
        // 256/145..512K could use the "rep movsb" ERMSB pattern on all CPUs
        cld
        {$ifdef WIN64ABI}
        push    rsi
        push    rdi
        mov     rsi, src
        mov     rdi, dst
        mov     rcx, r8
        rep movsb
        pop     rdi
        pop     rsi
        {$else}
        xchg    rsi, rdi // dst=rsi and src=rdi -> swap
        mov     rcx, r8
        rep movsb
        {$endif WIN64ABI}
        ret
        {$else}
        jmp     @lrgfwd
        {$endif WITH_ERMS}
        {$ifdef ASMX64AVXNOCONST} // limited AVX asm on Delphi 11
@lrgbwd:// backward SSE2/AVX move
        cmp     rax, SSE2MAXSIZE
        jb      @bsse2 // 97/129..255 bytes is not worth AVX context transition
        test    byte ptr [rip + X64CpuFeatures], 1 shl cpuAVX
        jz      @bsse2
        jmp     @bavx
@lrgfwd:// forward SSE2/AVX move
        test    byte ptr [rip + X64CpuFeatures], 1 shl cpuAVX
        jnz     @favx
        {$else}
@lrgfwd:{$endif ASMX64AVXNOCONST}
@fsse2: // forward SSE2 move
        lea     src, [src + rax - 16]
        lea     rax, [rax + dst - 16]
        mov     r10, rax
        neg     rax
        and     dst, -16                     // 16-byte aligned writes
        lea     rax, [rax + dst + 16]
        cmp     r8, r11
        jb      @fwd                         // bypass cache for cnt>512KB
        jmp     @fwdnt
        // backward SSE2/AVX move for cnt > 96/144 bytes
        // note: ERMSB is not available on "std rep move" which is slower
        {$ifndef ASMX64AVXNOCONST}
@lrgbwd:{$endif ASMX64AVXNOCONST}
@bsse2: // backward SSE2 move
        sub     rax, 16
        mov     r9, rax
        add     rax, dst
        and     rax, -16                     // 16-byte aligned writes
        sub     rax, dst
        cmp     r8, r11
        jae     @bwdnt                       // bypass cache for cnt>512KB
        jmp     @bwd
        {$ifdef ASMX64AVXNOCONST}
@bavx:  // backward AVX move
        sub     rax, 32
        mov     r9, rax
        vmovups ymm2, yword ptr [src + rax]  // last 32
        vmovups ymm1, yword ptr [src]        // first 32
        add     rax, dst
        and     rax, -32                     // 32-byte aligned writes
        sub     rax, dst
        cmp     r8, r11
        jae     @bavxn                       // bypass cache for cnt>512KB
        jmp     @bavxr
@favx:  // forward AVX move
        vmovups ymm2, yword ptr [src]        // first 32
        lea     src, [src + rax - 32]
        lea     dst, [dst + rax - 32]
        vmovups ymm1, yword ptr [src]        // last 32
        neg     rax
        add     rax, dst
        and     rax, -32                     // 32-byte aligned writes
        sub     rax, dst
        add     rax, 64
        cmp     r8, r11
        jb      @favxr                       // bypass cache for cnt>512KB
        jmp     @favxn
        // forward temporal AVX loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@favxr: vmovups ymm0, yword ptr [src + rax]
        vmovaps yword ptr [dst + rax], ymm0  // most CPUs have one store unit
        add     rax, 32
        jl      @favxr
@favxe: vmovups yword ptr [dst], ymm1        // last 32
        vmovups yword ptr [r9], ymm2         // first 32
// https://software.intel.com/en-us/articles/avoiding-avx-sse-transition-penalties
        vzeroupper
        ret
        // forward non-temporal AVX loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@favxn: vmovups ymm0, yword ptr [src + rax]
        // circumvent FPC inline asm bug with vmovntps [dst + rax], ymm0
        {$ifdef WIN64ABI}
        vmovntps [rdx + rax], ymm0           // rdx=dst on Win64
        {$else}
        vmovntps [rsi + rax], ymm0           // rsi=dst on POSIX
        {$endif WIN64ABI}
        add     rax, 32
        jl      @favxn
        sfence
        jmp     @favxe
        {$endif ASMX64AVXNOCONST}
        // forward temporal SSE2 loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@fwd:   movups  xmm2, oword ptr [src + rax]
        movaps  [r10 + rax], xmm2
        add     rax, 16
        jl      @fwd
        movups  oword ptr [r10], xmm0        // last 16
        movups  oword ptr [r9], xmm1         // first 16
        ret
        // forward non-temporal SSE2 loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@fwdnt: movups  xmm2, oword ptr [src + rax]
        movntdq [r10 + rax], xmm2
        add     rax, 16
        jl      @fwdnt
        sfence
        movups  oword ptr [r10], xmm0        // last 16
        movups  oword ptr [r9], xmm1         // first 16
        ret
        // backward temporal SSE2 loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bwd:   movups  xmm2, oword ptr [src + rax]
        movaps  oword ptr [dst + rax], xmm2
        sub     rax, 16
        jg      @bwd
        movups  oword ptr [dst], xmm1        // first 16
        movups  oword ptr [dst + r9], xmm0   // last 16
        ret
        // backward non-temporal SSE2 loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bwdnt: movups  xmm2, oword ptr [src + rax]
        movntdq oword ptr [dst + rax], xmm2
        sub     rax, 16
        jg      @bwdnt
        sfence
        movups  oword ptr [dst], xmm1        // first 16
        movups  oword ptr [dst + r9], xmm0   // last 16
        ret
        {$ifdef ASMX64AVXNOCONST}
        // backward temporal AVX loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bavxr: vmovups ymm0, yword ptr [src + rax]
        vmovaps yword ptr [dst + rax], ymm0
        sub     rax, 32
        jg      @bavxr
@bavxe: vmovups yword ptr [dst], ymm1        // first 32
        vmovups yword ptr [dst + r9], ymm2   // last 32
        vzeroupper
        ret
        // backward non-temporal AVX loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@bavxn: vmovups ymm0, yword ptr [src + rax]
        // circumvent FPC inline asm bug with vmovntps [dst + rax], ymm0
        {$ifdef WIN64ABI}
        vmovntps [rdx + rax], ymm0           // rdx=dst on Win64
        {$else}
        vmovntps [rsi + rax], ymm0           // rsi=dst on POSIX
        {$endif WIN64ABI}
        sub     rax, 32
        jg      @bavxn
        sfence
        jmp     @bavxe
        {$endif ASMX64AVXNOCONST}
        // dedicated branchless sub-functions for 0..16 bytes
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@jmptab:dq      @00, @01, @02, @03, @04, @05, @06, @07
        dq      @08, @09, @10, @11, @12, @13, @14, @15, @16
@01:    mov     al, byte ptr [src]
        mov     byte ptr [dst], al
@00:    ret
@02:    movzx   eax, word ptr [src]
        mov     word ptr [dst], ax
        ret
@03:    movzx   eax, word ptr [src]
        mov     cl, byte ptr [src + 2]
        mov     word ptr [dst], ax
        mov     byte ptr [dst + 2], cl
        ret
@04:    mov     eax, dword ptr [src]
        mov     dword ptr [dst], eax
        ret
@05:    mov     eax, dword ptr [src]
        mov     cl, byte ptr [src + 4]
        mov     dword ptr [dst], eax
        mov     byte ptr [dst + 4], cl
        ret
@06:    mov     eax, dword ptr [src]
        movzx   ecx, word ptr [src + 4]
        mov     dword ptr [dst], eax
        mov     word ptr [dst + 4], cx
        ret
@07:    mov     r8d, dword ptr [src]         // faster with no overlapping
        movzx   eax, word ptr [src + 4]
        mov     cl, byte ptr [src + 6]
        mov     dword ptr [dst], r8d
        mov     word ptr [dst + 4], ax
        mov     byte ptr [dst + 6], cl
        ret
@08:    mov     rax, qword ptr [src]
        mov     [dst], rax
        ret
@09:    mov     rax, qword ptr [src]
        mov     cl, byte ptr [src + 8]
        mov     [dst], rax
        mov     byte ptr [dst + 8], cl
        ret
@10:    mov     rax, qword ptr [src]
        movzx   ecx, word ptr [src + 8]
        mov     [dst], rax
        mov     word ptr [dst + 8], cx
        ret
@11:    mov     r8, qword ptr [src]
        movzx   eax, word ptr [src + 8]
        mov     cl, byte ptr [src + 10]
        mov     [dst], r8
        mov     word ptr [dst + 8], ax
        mov     byte ptr [dst + 10], cl
        ret
@12:    mov     rax, qword ptr [src]
        mov     ecx, dword ptr [src + 8]
        mov     [dst], rax
        mov     dword ptr [dst + 8], ecx
        ret
@13:    mov     r8, qword ptr [src]
        mov     eax, dword ptr [src + 8]
        mov     cl, byte ptr [src + 12]
        mov     [dst], r8
        mov     dword ptr [dst + 8], eax
        mov     byte ptr [dst + 12], cl
        ret
@14:    mov     r8, qword ptr [src]
        mov     eax, dword ptr [src + 8]
        movzx   ecx, word ptr [src + 12]
        mov     [dst], r8
        mov     dword ptr [dst + 8], eax
        mov     word ptr [dst + 12], cx
        ret
@15:    mov     r8, qword ptr [src]
        mov     rax, qword ptr [src + 7] // overlap is the easiest solution
        mov     [dst], r8
        mov     [dst + 7], rax
        ret
@16:    movups  xmm0, oword ptr [src]
        movups  oword [dst], xmm0
end;

procedure FillCharFast(var dst; cnt: PtrInt; value: byte);
{$ifdef FPC}nostackframe; assembler;
asm {$else} asm .noframe {$endif} // rcx/rdi=dst rdx/rsi=cnt r8b/dl=val
        mov     r9, $0101010101010101
        lea     r10, [rip + @jmptab]
        {$ifdef WIN64ABI}
        movzx   eax, r8b
        {$else}
        movzx   eax, dl
        mov     rdx, rsi // rdx=cnt
        {$endif WIN64ABI}
        imul    rax, r9  // broadcast value into all bytes of rax (in 1 cycle)
        cmp     cnt, 32
        ja      @abv32   // >32 or <0
        sub     rdx, 8
        jg      @sml32   // 9..32
        jmp     qword ptr [r10 + 64 + rdx * 8] // tinest 0..8 bytes
@sml32: cmp     dl, 8  // 9..32 bytes
        jle     @sml16
        cmp     dl, 16
        jle     @sml24
        mov     qword ptr [dst + 16], rax
@sml24: mov     qword ptr [dst + 8], rax
@sml16: mov     qword ptr [dst + rdx], rax // last 8 (may be overlapping)
@08:    mov     qword ptr [dst], rax
@00:    ret
@abv32: jng     @00  // < 0
        // cnt > 32 bytes
        movd    xmm0, eax
        lea     r8, [dst + cnt]        // r8 point to end
        mov     r9d, NONTEMPORALSIZE
        pshufd  xmm0, xmm0, 0          // broadcast value into all bytes of xmm0
        mov     r10, rdx               // save rdx=cnt
        cmp     rdx, 255 // = SSE2MAXSIZE-1, but hardcoded in move below
        ja      @abv255
        // 33..255 bytes is not good for ERMSB or AVX, and need no alignment
        test    dl, $80
        jz      @sml80
        movups  oword ptr [dst], xmm0
        movups  oword ptr [dst + $10], xmm0
        movups  oword ptr [dst + $20], xmm0
        movups  oword ptr [dst + $30], xmm0
        movups  oword ptr [dst + $40], xmm0
        movups  oword ptr [dst + $50], xmm0
        movups  oword ptr [dst + $60], xmm0
        movups  oword ptr [dst + $70], xmm0
        add     dst, $80
@sml80: test    dl, $40
        jz      @sml40
        movups  oword ptr [dst], xmm0
        movups  oword ptr [dst + $10], xmm0
        movups  oword ptr [dst + $20], xmm0
        movups  oword ptr [dst + $30], xmm0
        add     dst, $40
@sml40: test    dl, $20
        jz      @sml20
        movups  oword ptr [dst], xmm0
        movups  oword ptr [dst + $10], xmm0
        add     dst, $20
@sml20: test    dl, $10
        jz      @sml10
        movups  oword ptr [dst], xmm0
@sml10: movups  oword ptr [r8 - 16], xmm0 // last 16 bytes (may be overlapping)
        ret
@abv255:{$ifdef WITH_ERMS}
        cmp     rax, r9                   // non-temporal move > 512KB
        {$ifdef ASMX64AVXNOCONST}
        jae     @avx
        {$else}
        jae     @sse2
        {$endif ASMX64AVXNOCONST}
        // 256..512K could use the "rep stosb" ERMSB pattern on all CPUs
        cld
        {$ifdef WIN64ABI}
        mov     r8, rdi
        mov     rdi, dst
        mov     rcx, cnt
        rep     stosb
        mov     rdi, r8
        {$else}
        mov     rcx, cnt
        rep stosb
        {$endif WIN64ABI}
        ret
        {$endif WITH_ERMS}
@sse2:  movups  oword ptr [dst], xmm0  // first unaligned 16 bytes
        lea     rdx, [dst + rdx - 1]
        and     rdx, -16
        add     dst, 16
        and     dst, -16               // dst is 16-bytes aligned
        sub     dst, rdx
        jnb     @last
        cmp     r10, r9
        jae     @sse2nt                // bypass cache for cnt>512KB
        jmp     @reg
        {$ifdef ASMX64AVXNOCONST}
@avx:   test    byte ptr [rip + X64CpuFeatures], 1 shl cpuAVX
        jz      @sse2
        movups  oword ptr [dst], xmm0  // first unaligned 1..16 bytes
        add     dst, 16
        and     dst, -16
        movaps  oword ptr [dst], xmm0  // aligned 17..32 bytes
        vinsertf128 ymm0, ymm0, xmm0, 1
        add     dst, 16
        and     dst, -32               // dst is 32-bytes aligned
        mov     rdx, r8
        and     rdx, -32
        sub     dst, rdx
        cmp     r10, r9
        jb      @avxreg
        jmp     @avxnt
        {$endif ASMX64AVXNOCONST}
        // temporal SSE2 loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@reg:   movaps  oword ptr [rdx + dst], xmm0  // regular loop
        add     dst, 16
        jnz     @reg
@last:  movups  oword ptr [r8 - 16], xmm0 // last unaligned 16 bytes
        ret
        // non-temporal SSE2 loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@sse2nt:movntdq [rdx + dst], xmm0      // non-temporal loop
        add     dst, 16
        jnz     @sse2nt
        sfence
        movups  oword ptr [r8 - 16], xmm0
        ret
        {$ifdef ASMX64AVXNOCONST}
        // temporal AVX loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@avxreg:vmovaps yword ptr [rdx + dst], ymm0  // regular loop
        add     dst, 32
        jnz     @avxreg
@avxok: vmovups yword ptr [r8 - 32], ymm0    // last unaligned 32 bytes
        vzeroupper
        ret
        // non-temporal AVX loop
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
        {$ifdef WIN64}
@avxnt: vmovntps [rdx + rcx], ymm0 // non-temporal loop - rcx=dst on Win64
        {$else}
@avxnt: vmovntps [rdx + rdi], ymm0 // non-temporal loop - rdi=dst on POSIX
        {$endif WIN64}
        add      dst, 32
        jnz      @avxnt
        sfence
        jmp      @avxok
        {$endif ASMX64AVXNOCONST}
        // dedicated branchless sub-functions for 0..8 bytes
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@jmptab:dq @00, @01, @02, @03, @04, @05, @06, @07, @08
@07:    mov     dword ptr [dst + 3], eax
@03:    mov     word ptr [dst + 1], ax
@01:    mov     byte ptr [dst], al
        ret
@06:    mov     dword ptr [dst + 2], eax
@02:    mov     word ptr [dst], ax
        ret
@05:    mov     byte ptr [dst + 4], al
@04:    mov     dword ptr [dst], eax
end;

function crc32fasttab(crc: cardinal; buf: PAnsiChar; len: cardinal;
  tab: PCrc32tab): cardinal;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifdef SYSVABI}
        mov     r8, rdx
        mov     r9, tab
        {$endif SYSVABI}
        mov     eax, crc
        xor     ecx, ecx
        test    buf, buf // crc=eax buf=rdx/rsi len=r8 tab=r9
        jz      @z
        neg     r8
        jz      @z
        not     eax
        cmp     r8, -8
        jb      @head
@sml:   mov     cl, byte ptr [buf]
        add     buf, 1
        xor     cl, al
        shr     eax, 8
        xor     eax, dword ptr [rcx * 4 + r9]
        add     r8, 1
        jnz     @sml
@0:     not     eax
@z:     ret
@head:  {$ifdef SYSVABI}
        test    sil, 7
        {$else}
        test    dl, 7
        {$endif SYSVABI}
        jz      @align
        mov     cl, byte ptr [buf]
        add     buf, 1
        xor     cl, al
        shr     eax, 8
        xor     eax, dword ptr [rcx * 4 + r9]
        add     r8, 1
        jnz     @head
        not     eax
        ret
@align: sub     buf, r8
        add     r8, 8
        jg      @done
        push    rbx
@by8:   mov     r10d, eax
        mov     rcx, qword ptr [buf + r8 - 8]
        xor     r10, rcx
        shr     rcx, 32
        movzx   ebx, cl
        mov     eax, dword ptr [rbx * 4 + r9 + 1024 * 3]
        movzx   ebx, ch
        shr     ecx, 16
        xor     eax, dword ptr [rbx * 4 + r9 + 1024 * 2]
        movzx   ebx, cl
        xor     eax, dword ptr [rbx * 4 + r9 + 1024 * 1]
        movzx   ebx, ch
        xor     eax, dword ptr [rbx * 4 + r9 + 1024 * 0]
        mov     rcx, r10
        movzx   ebx, cl
        xor     eax, dword ptr [rbx * 4 + r9 + 1024 * 7]
        movzx   ebx, ch
        shr     ecx, 16
        xor     eax, dword ptr [rbx * 4 + r9 + 1024 * 6]
        movzx   ebx, cl
        xor     eax, dword ptr [rbx * 4 + r9 + 1024 * 5]
        movzx   ebx, ch
        xor     eax, dword ptr [rbx * 4 + r9 + 1024 * 4]
        add     r8, 8
        jle     @by8
        xor     ecx, ecx
        pop     rbx
@done:  sub     r8, 8
        jge     @e
@tail:  mov     cl, byte ptr [buf + r8]
        xor     cl, al
        shr     eax, 8
        xor     eax, dword ptr [rcx * 4 + r9]
        add     r8, 1
        jnz     @tail
@e:     not     eax
end;

function StrInt32(P: PAnsiChar; val: PtrInt): PAnsiChar;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     r10, val
        sar     r10, 63         // r10=0 if val>=0 or -1 if val<0
        xor     val, r10
        sub     val, r10        // val=abs(val)
        mov     rax, val
        cmp     val, 10
        jb      @1              // direct process of common val<10
        lea     r8, [rip + TwoDigitLookup]
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     lea     P, [P - 2]
        cmp     rax, 100
        jb      @2
        lea     r9, [rax * 2]
        shr     rax, 2
        mov     rdx, 2951479051793528259
        mul     rdx  // use power of two reciprocal to avoid division
        shr     rdx, 2
        mov     rax, rdx
        imul    rdx, -200
        lea     rdx, [rdx + r8]
        movzx   edx, word ptr [rdx + r9]
        mov     word ptr [P], dx
        cmp     rax, 10
        jae     @s
@1:     or      al, '0'
        mov     byte ptr [P - 2], '-'
        mov     byte ptr [P - 1], al
        lea     rax, [P + r10 - 1]       // includes '-' if val<0
        ret
        {$ifdef FPC} align 8 {$else} .align 8 {$endif}
@2:     movzx   eax, word ptr [r8 + rax * 2]
        mov     byte ptr [P - 1], '-'
        mov     word ptr [P], ax
        lea     rax, [P + r10]           // includes '-' if val<0
end;

function StrUInt32(P: PAnsiChar; val: PtrUInt): PAnsiChar;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rax, val
        cmp     val, 10
        jb      @1  // direct process of common val<10
        lea     r8, [rip + TwoDigitLookup]
@s:     lea     P, [P - 2]
        cmp     rax, 100
        jb      @2
        lea     r9, [rax * 2]
        shr     rax, 2
        mov     rdx, 2951479051793528259
        mul     rdx  // use power of two reciprocal to avoid division
        shr     rdx, 2
        mov     rax, rdx
        imul    rdx, -200
        add     rdx, r8
        movzx   rdx, word ptr [rdx + r9]
        mov     word ptr [P], dx
        cmp     rax, 10
        jae     @s
@1:     sub     P, 1
        or      al, '0'
        mov     byte ptr [P], al
@0:     mov     rax, P
        ret
@2:     movzx   eax, word ptr [r8 + rax * 2]
        mov     word ptr [P], ax
        mov     rax, P
end;

{$endif ASMX64}

{$ifdef CPUX64ASM} /// proper compilation on FPC and Delphi XE7+

{$ifdef FPC}

procedure fpc_freemem; external name 'FPC_FREEMEM'; // access to RTL from asm

procedure FastAssignNew(var d; s: pointer); nostackframe; assembler;
asm
        mov     rax, qword ptr [d]
        mov     qword ptr [d], s
        test    rax, rax
        jz      @z
        lea     d, qword ptr [rax - _STRRECSIZE] // for fpc_freemem() below
        {$ifdef STRCNT32}
        cmp     dword ptr [rax - _STRCNT], 0 // refcnt=-1 for const
        jl      @z
   lock dec     dword ptr [rax - _STRCNT]
        {$else}
        cmp     qword ptr [rax - _STRCNT], 0 // refcnt=-1 for const
        jl      @z
   lock dec     qword ptr [rax - _STRCNT]
        {$endif STRCNT32}
        jbe     fpc_freemem
@z:
end;

procedure FastAssignNewNotVoid(var d; s: pointer);
nostackframe; assembler;
asm
        mov     rax, qword ptr [d]
        mov     qword ptr [d], s
        lea     d, qword ptr [rax - _STRRECSIZE] // for fpc_freemem() below
        {$ifdef STRCNT32}
        cmp     dword ptr [rax - _STRCNT], 0 // refcnt=-1 for const
        jl      @z
   lock dec     dword ptr [rax - _STRCNT]
        {$else}
        cmp     qword ptr [rax - _STRCNT], 0 // refcnt=-1 for const
        jl      @z
   lock dec     qword ptr [rax - _STRCNT]
        {$endif STRCNT32}
        jbe     fpc_freemem
@z:
end;

{$endif FPC}

{
  Some numbers, with CITIES_MAX=200000, deleting 1/128 entries
  first column (3..23) is the max number of indexes[] chunk to rehash
  #abc is the number of slots in the hash table
  adjust=.. match DynArrayHashTableAdjust() time fixing the indexes
  hash=ms is the time needed to hash input (not impacted by adjusting algorithm)
  -> TDynArray.Delete move() now takes more time than the HashTable update :)
  1. naive loop
    for i := 0 to HashTableSize-1 do
      if HashTable[i]>aArrayIndex then
        dec(HashTable[i]);
    3 #257  adjust=7.95ms 191.7MB/s hash=8us
    23 #195075  adjust=4.27s 548.6MB/s hash=2.47ms
  2. branchless pure pascal code is about 10x faster!
    3 #257  adjust=670us 2.2GB hash=8us
    23 #195075  adjust=520.85ms 4.3GB/s hash=2.45ms
  3. SSE2 simd assembly makes about 3x improvement
    3 #257  adjust=290us 5.1GB hash=8us
    23 #195075  adjust=201.53ms 11.3GB/s hash=2.44ms
  4. AVX2 simd assembly gives some additional 40% (on my iCore3 cpu)
    3 #257  adjust=262us 5.6GB hash=8us
    23 #195075  adjust=161.73ms 14.1GB/s hash=2.57ms
}

// brute force O(n) indexes fix after deletion (much faster than full ReHash)
procedure DynArrayHashTableAdjust(P: PIntegerArray; deleted: integer; count: PtrInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
{$else}  {$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     r8, rdx
        mov     rcx, rdi
        mov     rdx, rsi
{$endif WIN64ABI}
        xor     eax, eax    // reset eax high bits for setg al below
        movq    xmm0, rdx   // xmm0 = 128-bit of quad deleted
        pshufd  xmm0, xmm0, 0
        test    cl, 3
        jnz     @1  // paranoid: a dword dynamic array is always dword-aligned
        // ensure P is 256-bit aligned (for avx2)
@align: test    cl, 31
        jz      @ok
        cmp     dword ptr [rcx], edx
        setg    al                     // P[]>deleted -> al=1, 0 otherwise
        sub     dword ptr [rcx], eax   // branchless dec(P[])
        add     rcx, 4
        sub     r8, 1
        jmp     @align
@ok:    {$ifdef ASMX64AVXNOCONST}
        test    byte ptr [rip + X64CpuFeatures], 1 shl cpuAVX2
        jz      @sse2
        vpshufd ymm0, ymm0, 0          // shuffle to ymm0 128-bit low lane
        vperm2f128 ymm0, ymm0, ymm0, 0 // copy to ymm0 128-bit high lane
        // avx process of 128 bytes (32 indexes) per loop iteration
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@avx2:  sub      r8, 32
        vmovdqa  ymm1, [rcx]           // 4 x 256-bit process = 4 x 8 integers
        vmovdqa  ymm3, [rcx + 32]
        vmovdqa  ymm5, [rcx + 64]
        vmovdqa  ymm7, [rcx + 96]
        vpcmpgtd ymm2, ymm1, ymm0      // compare P[]>deleted -> -1, 0 otherwise
        vpcmpgtd ymm4, ymm3, ymm0
        vpcmpgtd ymm6, ymm5, ymm0
        vpcmpgtd ymm8, ymm7, ymm0
        vpaddd   ymm1, ymm1, ymm2      // adjust by adding -1 / 0
        vpaddd   ymm3, ymm3, ymm4
        vpaddd   ymm5, ymm5, ymm6
        vpaddd   ymm7, ymm7, ymm8
        vmovdqa  [rcx], ymm1
        vmovdqa  [rcx + 32], ymm3
        vmovdqa  [rcx + 64], ymm5
        vmovdqa  [rcx + 96], ymm7
        add      rcx, 128
        cmp      r8, 32
        jae      @avx2
        vzeroupper
        jmp      @2
        {$endif ASMX64AVXNOCONST}
        // SSE2 process of 64 bytes (16 indexes) per loop iteration
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@sse2:  sub     r8, 16
        movaps  xmm1, dqword [rcx]     // 4 x 128-bit process = 4 x 4 integers
        movaps  xmm3, dqword [rcx + 16]
        movaps  xmm5, dqword [rcx + 32]
        movaps  xmm7, dqword [rcx + 48]
        movaps  xmm2, xmm1             // keep copy for paddd below
        movaps  xmm4, xmm3
        movaps  xmm6, xmm5
        movaps  xmm8, xmm7
        pcmpgtd xmm1, xmm0             // quad compare P[]>deleted -> -1 / 0
        pcmpgtd xmm3, xmm0
        pcmpgtd xmm5, xmm0
        pcmpgtd xmm7, xmm0
        paddd   xmm1, xmm2             // quad adjust by adding -1 / 0
        paddd   xmm3, xmm4
        paddd   xmm5, xmm6
        paddd   xmm7, xmm8
        movaps  dqword [rcx], xmm1     // quad store back
        movaps  dqword [rcx + 16], xmm3
        movaps  dqword [rcx + 32], xmm5
        movaps  dqword [rcx + 48], xmm7
        add     rcx, 64
        cmp     r8, 16
        jae     @sse2
        jmp     @2
        // trailing indexes (never appearing within DYNARRAYHASH_PO2 range)
@1:     sub     r8, 1
        cmp     dword ptr [rcx + r8 * 4], edx
        setg    al
        sub     dword ptr [rcx + r8 * 4], eax
@2:     test    r8, r8
        jnz     @1
{$ifdef WIN64ABI}
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
{$endif WIN64ABI}
end;

// DYNARRAYHASH_16BIT version for 16-bit HashTable[] - no AVX2 since count < 64K
procedure DynArrayHashTableAdjust16(P: PWordArray; deleted: cardinal; count: PtrInt);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
{$else} {$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     r8, rdx
        mov     rcx, rdi
        mov     rdx, rsi
{$endif WIN64ABI}
        mov     eax, deleted
        shl     eax, 16     // for pshufd (inline asm doesn't know about pshufw)
        or      edx, eax               // edx = 32-bit of 2x 16-bit deleted
        movq    xmm0, rdx              // xmm0 = 128-bit of 8x deleted
        pshufd  xmm0, xmm0, 0
        xor     eax, eax               // reset eax high bits for setg al below
        test    cl, 1
        jnz     @1  // paranoid: a dword dynamic array is always word-aligned
        // ensure P is 128-bit aligned (for movaps)
@align: test    cl, 15
        jz      @sse2
        cmp     word ptr [rcx], dx
        setg    al                     // P[]>deleted -> al=1, 0 otherwise
        sub     word ptr [rcx], ax     // branchless dec(P[])
        add     rcx, 2
        sub     r8, 1
        jmp     @align
        // SSE2 process of 64 bytes (32 indexes) per loop iteration
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@sse2:  sub     r8, 32
        movaps  xmm1, dqword [rcx]     // 4 x 128-bit process = 4 x 8 words
        movaps  xmm3, dqword [rcx + 16]
        movaps  xmm5, dqword [rcx + 32]
        movaps  xmm7, dqword [rcx + 48]
        movaps  xmm2, xmm1             // keep copy for paddd below
        movaps  xmm4, xmm3
        movaps  xmm6, xmm5
        movaps  xmm8, xmm7
        pcmpgtw xmm1, xmm0             // 8x compare P[]>deleted -> -1 / 0
        pcmpgtw xmm3, xmm0
        pcmpgtw xmm5, xmm0
        pcmpgtw xmm7, xmm0
        paddw   xmm1, xmm2             // 8x adjust by adding -1 / 0
        paddw   xmm3, xmm4
        paddw   xmm5, xmm6
        paddw   xmm7, xmm8
        movaps  dqword [rcx], xmm1     // 8x store back
        movaps  dqword [rcx + 16], xmm3
        movaps  dqword [rcx + 32], xmm5
        movaps  dqword [rcx + 48], xmm7
        add     rcx, 64
        cmp     r8, 32
        jae     @sse2
        jmp     @2
        // trailing indexes (never appearing within DYNARRAYHASH_PO2 range)
@1:     sub     r8, 1
        cmp     word ptr [rcx + r8 * 4], dx
        setg    al
        sub     word ptr [rcx + r8 * 4], ax
@2:     test    r8, r8
        jnz     @1
{$ifdef WIN64ABI}
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
{$endif WIN64ABI}
end;

{$ifdef ASMX64AVXNOCONST}
// AVX2 ASM .align 32 for const is not available on Delphi :(

// adapted from https://github.com/simdjson/simdjson - Apache License 2.0
function IsValidUtf8Avx2(source: PUtf8Char; sourcelen: PtrInt): boolean;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        test    source, source
        jz      @ok
        test    sourcelen, sourcelen
        jle     @ok
        {$ifdef WIN64ABI} // Win64 ABI doesn't consider rsi/rdi as volatile
        push    rsi
        push    rdi
        {$endif WIN64ABI}
        push    rbp
        mov     r8, source
        mov     rdx, sourcelen
        mov     rsi, r8
        mov     ecx, 64
        mov     rax, rsi
        mov     rdi, rdx
        mov     rbp, rsp
        and     rsp, 0FFFFFFFFFFFFFFE0H // align stack at 32 bytes
        sub     rsp, 160
        {$ifdef WIN64ABI}
        movaps  dqword ptr [rsp + 00H], xmm6
        movaps  dqword ptr [rsp + 10H], xmm7
        movaps  dqword ptr [rsp + 20H], xmm8
        movaps  dqword ptr [rsp + 30H], xmm9
        movaps  dqword ptr [rsp + 40H], xmm10
        movaps  dqword ptr [rsp + 50H], xmm11
        movaps  dqword ptr [rsp + 60H], xmm12
        movaps  dqword ptr [rsp + 70H], xmm13
        movaps  dqword ptr [rsp + 80H], xmm14
        movaps  dqword ptr [rsp + 90H], xmm15
        sub     rsp, 100H
        {$endif WIN64ABI}
        cmp     rdx, 64
        cmovnc  rcx, rdx
        sub     rcx, 64
        je      @small
        vpxor   xmm3, xmm3, xmm3
        xor     esi, esi
        vmovdqu ymm7,  yword ptr [rip + @0f]
        vmovdqu ymm15, yword ptr [rip + @_6]
        vmovdqu ymm14, yword ptr [rip + @_7]
        vmovdqu ymm13, yword ptr [rip + @_8]
        vmovdqa ymm5, ymm3
        vmovdqa ymm2, ymm3
        // main processing loop, 64 bytes per iteration
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@loop:  vmovdqu xmm6, oword ptr [rax + rsi]
        vinserti128 ymm0, ymm6, [rax + rsi + 10H], 01H
        vmovdqu xmm6, oword ptr [rax + rsi + 20H]
        vinserti128 ymm1, ymm6, [rax + rsi + 30H], 01H
        add     rsi, 64
        vpor    ymm4, ymm1, ymm0
        vpmovmskb rdx, ymm4 // check set MSB of each 64 bytes
        test    edx, edx
        jne     @check
        vpor    ymm2, ymm5, ymm2
        vmovdqa ymm4, ymm2
        cmp     rcx, rsi
        ja      @loop
        // process trailing 0..63 bytes
@trail: sub     rdi, rsi
        jz      @ended
        add     rsi, rax
        vmovdqu xmm0, oword ptr [rip + @20]
        lea     rdx, qword ptr [rsp + 60H] // copy on stack with space padding
        sub     rsi, rdx
        vmovdqa oword ptr [rdx], xmm0
        vmovdqa oword ptr [rdx + 10H], xmm0
        vmovdqa oword ptr [rdx + 20H], xmm0
        vmovdqa oword ptr [rdx + 30H], xmm0
@by8:   sub     rdi, 8
        jb      @by1
        mov     rax, qword ptr [rsi + rdx]
        mov     qword ptr [rdx], rax
        add     rdx, 8 // in-order copy to preserve UTF-8 encoding
        jmp     @by8
@by1:   add     rdi, 8
        jz      @0
@sml:   mov     al, byte ptr [rsi + rdx]
        mov     byte ptr [rdx], al
        add     rdx, 1
        sub     rdi, 1
        jnz     @sml
@0:     vmovdqa ymm1, yword ptr [rsp + 60H]
        vmovdqa ymm2, yword ptr [rsp + 80H]
        vpor    ymm0, ymm1, ymm2
        vpmovmskb rax, ymm0 // check any set MSB
        test    eax, eax
        jne     @last
@ended: vpor    ymm5, ymm5, ymm4
@final: vptest  ymm5, ymm5
        sete    al
        vzeroupper
        {$ifdef WIN64ABI}
        add     rsp, 100H
        movaps  xmm6, dqword ptr [rsp + 00H]
        movaps  xmm7, dqword ptr [rsp + 10H]
        movaps  xmm8, dqword ptr [rsp + 20H]
        movaps  xmm9, dqword ptr [rsp + 30H]
        movaps  xmm10, dqword ptr [rsp + 40H]
        movaps  xmm11, dqword ptr [rsp + 50H]
        movaps  xmm12, dqword ptr [rsp + 60H]
        movaps  xmm13, dqword ptr [rsp + 70H]
        movaps  xmm14, dqword ptr [rsp + 80H]
        movaps  xmm15, dqword ptr [rsp + 90H]
        leave      // mov rsp,rbp + pop rbp
        pop     rdi
        pop     rsi
        {$else}
        leave
        {$endif WIN64ABI}
        ret
@ok:    mov     al, 1
        ret
@small: vpxor   xmm4, xmm4, xmm4
        xor     esi, esi
        vmovdqa ymm3, ymm4
        vmovdqa ymm5, ymm4
        jmp     @trail
        // validate UTF-8 extra bytes from main loop
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@check: vpsrlw  ymm9, ymm0, 4
        vpsrlw  ymm12, ymm1, 4
        vperm2i128 ymm3, ymm3, ymm0, 21H
        vpalignr ymm5, ymm0, ymm3, 0FH
        vpalignr ymm11, ymm0, ymm3, 0EH
        vpsubusb ymm11, ymm11, yword ptr [rip + @_9]
        vpalignr ymm3, ymm0, ymm3, 0DH
        vperm2i128 ymm0, ymm0, ymm1, 21H
        vpsubusb ymm3, ymm3, yword ptr [rip + @_10]
        vpalignr ymm8, ymm1, ymm0, 0FH
        vpsrlw  ymm10, ymm5, 4
        vpand   ymm5, ymm7, ymm5
        vpsrlw  ymm6, ymm8, 4
        vpalignr ymm4, ymm1, ymm0, 0EH
        vpsubusb ymm4, ymm4, yword ptr [rip + @_9]
        vpalignr ymm0, ymm1, ymm0, 0DH
        vpsubusb ymm0, ymm0, yword ptr [rip + @_10]
        vpand   ymm10, ymm10, ymm7
        vpand   ymm6, ymm6, ymm7
        vpand   ymm8, ymm7, ymm8
        vpor    ymm3, ymm3, ymm11
        vpor    ymm0, ymm4, ymm0
        vpxor   xmm11, xmm11, xmm11
        vpshufb ymm10, ymm15, ymm10
        vpshufb ymm5, ymm14, ymm5
        vpand   ymm9, ymm9, ymm7
        vpshufb ymm6, ymm15, ymm6
        vpshufb ymm8, ymm14, ymm8
        vpand   ymm12, ymm12, ymm7
        vpand   ymm5, ymm5, ymm10
        vpcmpgtb ymm3, ymm3, ymm11
        vpcmpgtb ymm0, ymm0, ymm11
        vpshufb ymm9, ymm13, ymm9
        vpand   ymm3, ymm3, yword ptr [rip + @_11]
        vpand   ymm0, ymm0, yword ptr [rip + @_11]
        vpshufb ymm12, ymm13, ymm12
        vpand   ymm6, ymm6, ymm8
        vpand   ymm9, ymm5, ymm9
        vpsubusb ymm5, ymm1, yword ptr [rip + @_12]
        vpand   ymm12, ymm6, ymm12
        vpxor   ymm9, ymm3, ymm9
        vmovdqa ymm3, ymm1
        vpxor   ymm12, ymm0, ymm12
        vpor    ymm9, ymm9, ymm12
        vpor    ymm2, ymm9, ymm2
        vmovdqa ymm4, ymm2
        cmp     rcx, rsi
        ja      @loop
        jmp     @trail
        // validate UTF-8 extra bytes from input ending
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@last:  vmovdqu ymm5, yword ptr [rip + @0f]
        vperm2i128 ymm3, ymm3, ymm1, 21H
        vmovdqu ymm9, yword ptr [rip + @_7]
        vpsrlw  ymm11, ymm1, 4
        vpalignr ymm0, ymm1, ymm3, 0FH
        vmovdqu ymm13, yword ptr [rip + @_10]
        vmovdqu ymm14, yword ptr [rip + @_9]
        vpsrlw  ymm6, ymm0, 4
        vpand   ymm0, ymm5, ymm0
        vpand   ymm11, ymm11, ymm5
        vmovdqu ymm7, yword ptr [rip + @_6]
        vpshufb ymm10, ymm9, ymm0
        vpalignr ymm0, ymm1, ymm3, 0EH
        vpand   ymm6, ymm6, ymm5
        vmovdqu ymm8, yword ptr [rip + @_8]
        vpalignr ymm3, ymm1, ymm3, 0DH
        vperm2i128 ymm1, ymm1, ymm2, 21H
        vpsubusb ymm0, ymm0, ymm14
        vpsubusb ymm12, ymm3, ymm13
        vpalignr ymm3, ymm2, ymm1, 0FH
        vpshufb ymm6, ymm7, ymm6
        vpsrlw  ymm15, ymm3, 4
        vpand   ymm3, ymm5, ymm3
        vpor    ymm0, ymm0, ymm12
        vpshufb ymm9, ymm9, ymm3
        vpsrlw  ymm3, ymm2, 4
        vpand   ymm15, ymm15, ymm5
        vpand   ymm5, ymm3, ymm5
        vpalignr ymm3, ymm2, ymm1, 0EH
        vpxor   xmm12, xmm12, xmm12
        vpalignr ymm1, ymm2, ymm1, 0DH
        vpsubusb ymm3, ymm3, ymm14
        vpshufb ymm11, ymm8, ymm11
        vpsubusb ymm1, ymm1, ymm13
        vpcmpgtb ymm0, ymm0, ymm12
        vpshufb ymm7, ymm7, ymm15
        vpor    ymm1, ymm3, ymm1
        vpshufb ymm8, ymm8, ymm5
        vpsubusb ymm5, ymm2, yword ptr [rip + @_12]
        vmovdqu ymm2, yword ptr [rip + @_11]
        vpcmpgtb ymm1, ymm1, ymm12
        vpand   ymm6, ymm6, ymm10
        vpand   ymm7, ymm7, ymm9
        vpand   ymm0, ymm0, ymm2
        vpand   ymm11, ymm6, ymm11
        vpand   ymm8, ymm7, ymm8
        vpxor   ymm0, ymm0, ymm11
        vpor    ymm5, ymm4, ymm5
        vpand   ymm1, ymm1, ymm2
        vpxor   ymm1, ymm1, ymm8
        vpor    ymm0, ymm0, ymm1
        vpor    ymm5, ymm0, ymm5
        jmp     @final
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@20:    dq 2020202020202020H
        dq 2020202020202020H
{$ifdef FPC} align 32 {$else} .align 16 {Delphi doesn't allow 32 :(} {$endif}
@0f:    dq 0F0F0F0F0F0F0F0FH
        dq 0F0F0F0F0F0F0F0FH
        dq 0F0F0F0F0F0F0F0FH
        dq 0F0F0F0F0F0F0F0FH
@_6:    dq 0202020202020202H
        dq 4915012180808080H
        dq 0202020202020202H
        dq 4915012180808080H
@_7:    dq 0CBCBCB8B8383A3E7H
        dq 0CBCBDBCBCBCBCBCBH
        dq 0CBCBCB8B8383A3E7H
        dq 0CBCBDBCBCBCBCBCBH
@_8:    dq 0101010101010101H
        dq 01010101BABAAEE6H
        dq 0101010101010101H
        dq 01010101BABAAEE6H
@_9:    dq 0DFDFDFDFDFDFDFDFH
        dq 0DFDFDFDFDFDFDFDFH
        dq 0DFDFDFDFDFDFDFDFH
        dq 0DFDFDFDFDFDFDFDFH
@_10:   dq 0EFEFEFEFEFEFEFEFH
        dq 0EFEFEFEFEFEFEFEFH
        dq 0EFEFEFEFEFEFEFEFH
        dq 0EFEFEFEFEFEFEFEFH
@_11:   dq 8080808080808080H
        dq 8080808080808080H
        dq 8080808080808080H
        dq 8080808080808080H
@_12:   db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0EFH, 0DFH, 0BFH
end;

// inspired by https://github.com/aklomp/base64 - BSD-2-Clause License
// - less unrolled, but (much) faster thanks to manually tuned asm

procedure Base64EncodeAvx2(var b: PAnsiChar; var blen: PtrUInt;
  var b64: PAnsiChar);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12, bak13, bak14, bak15: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        movups  dqword ptr [bak13], xmm13
        movups  dqword ptr [bak14], xmm14
        movups  dqword ptr [bak15], xmm15
        push    rsi         // Win64 ABI doesn't consider rsi/rdi as volatile
        push    rdi
        mov     rsi, r8     // rsi = b64
        mov     r8, rdx     // r8 = blen
        mov     rdi, rcx    // rdi = b
{$else} {$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
        mov     r8, rsi     // r8 = blen
        mov     rsi, rdx    // rsi = b64   rdi = b
{$endif WIN64ABI}
        mov     rcx, qword ptr [r8]
        cmp     rcx, 31
        jbe     @done
        lea     rdx, qword ptr [rcx - 4]
        vmovdqu ymm0,  yword ptr [rip + @c9]
        mov     r10, 0AAAAAAAAAAAAAAABH
        vmovdqu ymm7,  yword ptr [rip + @c10]
        mov     rax, rdx
        vmovdqu ymm5,  yword ptr [rip + @c11]
        vmovdqu ymm8,  yword ptr [rip + @c12]
        mul     r10
        vmovdqu ymm9,  yword ptr [rip + @c13]
        vmovdqu ymm10, yword ptr [rip + @c14]
        vmovdqu ymm6,  yword ptr [rip + @c16]
        vmovdqu ymm4,  yword ptr [rip + @c15]
        vmovdqu ymm11, yword ptr [rip + @c17]
        shr     rdx, 4                  // rdx = rounds = (blen - 4) / 24
        lea     rax, qword ptr [rdx + rdx * 2]
        shl     rax, 3
        sub     rcx, rax
        mov     qword ptr [r8], rcx     // blen = rounds * 24
        mov     rcx, qword ptr [rdi]    // rcx = [rdi] = b
        mov     rax, qword ptr [rsi]    // rax = [rsi] = b64
        // initial 24 bytes output process
        vmovdqu xmm3, oword ptr [rcx]
        vinserti128 ymm1, ymm3, oword ptr [rcx + 16], 1
        vpermd  ymm1, ymm0, ymm1
        vpshufb ymm1, ymm1, ymm7
        vpand   ymm0, ymm5, ymm1
        vpmulhuw ymm2, ymm0, ymm8
        vpand   ymm0, ymm9, ymm1
        vpmullw ymm0, ymm10, ymm0
        vpor    ymm0, ymm0, ymm2
        vpcmpgtb ymm2, ymm0, ymm6
        vpsubusb ymm1, ymm0, ymm4
        vpsubb  ymm1, ymm1, ymm2
        vpshufb ymm1, ymm11, ymm1
        vpaddb  ymm0, ymm1, ymm0
        vmovdqu oword ptr [rax], xmm0
        vextracti128 oword ptr [rax + 16], ymm0, 1
        add     rax, 32
        add     rcx, 20
        sub     rdx, 1
        je      @10
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
        // process 48 input bytes per loop iteration into 64 encoded bytes
@9:     cmp     rdx, 1
        je      @12
        // whole loop logic is fully interlaced to unleash future CPU potential
        vmovdqu xmm1, oword ptr [rcx]
        vmovdqu xmm3, oword ptr [rcx + 24]
        vinserti128 ymm1, ymm1, oword ptr [rcx + 16], 1
        vinserti128 ymm3, ymm3, oword ptr [rcx + 24 + 16], 1
        vpshufb ymm1, ymm1, ymm7
        vpshufb ymm3, ymm3, ymm7
        vpand   ymm0, ymm5, ymm1
        vpand   ymm12, ymm5, ymm3
        vpmulhuw ymm2, ymm0, ymm8
        vpmulhuw ymm14, ymm12, ymm8
        vpand   ymm0, ymm9, ymm1
        vpand   ymm12, ymm9, ymm3
        vpmullw ymm0, ymm10, ymm0
        vpmullw ymm12, ymm10, ymm12
        vpor    ymm0, ymm0, ymm2
        vpor    ymm12, ymm12, ymm14
        vpcmpgtb ymm2, ymm0, ymm6
        vpcmpgtb ymm15, ymm12, ymm6
        vpsubusb ymm1, ymm0, ymm4
        vpsubusb ymm14, ymm12, ymm4
        vpsubb  ymm1, ymm1, ymm2
        vpsubb  ymm14, ymm14, ymm15
        vpshufb ymm1, ymm11, ymm1
        vpshufb ymm14, ymm11, ymm14
        vpaddb  ymm0, ymm1, ymm0
        vpaddb  ymm12, ymm14, ymm12
        vmovdqu oword ptr [rax], xmm0
        vextracti128 oword ptr [rax + 16], ymm0, 1
        vmovdqu oword ptr [rax + 32], xmm12
        vextracti128 oword ptr [rax + 48], ymm12, 1
        add     rcx, 48
        add     rax, 64
        sub     rdx, 2
        jne     @9
@10:    add     rcx, 4
        mov     qword ptr [rsi], rax
        mov     qword ptr [rdi], rcx
        vzeroupper
        {$ifdef WIN64ABI}
        jmp     @done
        {$else}
        ret
        {$endif WIN64ABI}
        // trailing 24 bytes
@12:    vmovdqu xmm3, oword ptr [rcx]
        vinserti128 ymm1, ymm3, oword ptr [rcx + 16], 1
        vpshufb ymm1, ymm1, ymm7
        vpand   ymm0, ymm5, ymm1
        vpmulhuw ymm8, ymm0, ymm8
        vpand   ymm0, ymm9, ymm1
        vpmullw ymm0, ymm10, ymm0
        vpor    ymm0, ymm0, ymm8
        vpcmpgtb ymm6, ymm0, ymm6
        vpsubusb ymm4, ymm0, ymm4
        vpsubb  ymm4, ymm4, ymm6
        vpshufb ymm11, ymm11, ymm4
        vpaddb  ymm0, ymm11, ymm0
        vmovdqu oword ptr [rax], xmm0
        vextracti128 oword ptr [rax + 16], ymm0, 1
        add     rcx, 24
        add     rax, 32
        jmp     @10
{$ifdef FPC} align 32 {$else} .align 16 {Delphi doesn't allow 32 :(} {$endif}
@c9:    dq 0000000000000000H
        dq 0000000200000001H
        dq 0000000400000003H
        dq 0000000600000005H
@c10:   dq 0809070805060405H
        dq 0E0F0D0E0B0C0A0BH
        dq 0405030401020001H
        dq 0A0B090A07080607H
@c11:   dq 0FC0FC000FC0FC00H
        dq 0FC0FC000FC0FC00H
        dq 0FC0FC000FC0FC00H
        dq 0FC0FC000FC0FC00H
@c12:   dq 0400004004000040H
        dq 0400004004000040H
        dq 0400004004000040H
        dq 0400004004000040H
@c13:   dq 003F03F0003F03F0H
        dq 003F03F0003F03F0H
        dq 003F03F0003F03F0H
        dq 003F03F0003F03F0H
@c14:   dq 0100001001000010H
        dq 0100001001000010H
        dq 0100001001000010H
        dq 0100001001000010H
@c15:   dq 3333333333333333H
        dq 3333333333333333H
        dq 3333333333333333H
        dq 3333333333333333H
@c16:   dq 1919191919191919H
        dq 1919191919191919H
        dq 1919191919191919H
        dq 1919191919191919H
@c17:   dq 0FCFCFCFCFCFC4741H
        dq 0000F0EDFCFCFCFCH
        dq 0FCFCFCFCFCFC4741H
        dq 0000F0EDFCFCFCFCH
@done:  {$ifdef WIN64ABI}
        pop     rdi
        pop     rsi
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        movups  xmm13, dqword ptr [bak13]
        movups  xmm14, dqword ptr [bak14]
        movups  xmm15, dqword ptr [bak15]
        {$endif WIN64ABI}
end;

procedure Base64DecodeAvx2(var b64: PAnsiChar; var b64len: PtrInt;
  var b: PAnsiChar);
{$ifdef WIN64ABI}
var
  bak6, bak7, bak8, bak9, bak10, bak11, bak12: THash128;
asm     // Windows x64 calling convention expects to preserve XMM6-XMM15
        movups  dqword ptr [bak6], xmm6
        movups  dqword ptr [bak7], xmm7
        movups  dqword ptr [bak8], xmm8
        movups  dqword ptr [bak9], xmm9
        movups  dqword ptr [bak10], xmm10
        movups  dqword ptr [bak11], xmm11
        movups  dqword ptr [bak12], xmm12
        push    rsi         // Win64 ABI doesn't consider rsi/rdi as volatile
        push    rdi
        mov     rsi, rdx
        mov     rdx, r8
        mov     rdi, rcx
{$else} {$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
{$endif WIN64ABI}
        // rcx/rdi=b64 rdx/rsi=b64len r8/rdx=b
        // on decoding error, b64 will point to the faulty input
        mov     r8, qword ptr [rsi]
        cmp     r8, 44
        jbe     @done
        lea     r9, qword ptr [r8 - 0DH]
        vmovdqu ymm1, yword ptr [rip + @c0]
        vmovdqu ymm5, yword ptr [rip + @c1]
        mov     rax, r9
        and     r9, 0FFFFFFFFFFFFFFE0H
        vmovdqu ymm4, yword ptr [rip + @c2]
        vmovdqu ymm9, yword ptr [rip + @c3]
        sub     r8, r9
        shr     rax, 5                         // rax = rounds
        vmovdqu ymm8, yword ptr [rip + @c4]
        vmovdqu ymm3, yword ptr [rip + @c5]
        mov     qword ptr [rsi], r8            // set final b64len
        vmovdqu ymm2, yword ptr [rip + @c6]
        vmovdqu ymm7, yword ptr [rip + @c7]
        vmovdqu ymm6, yword ptr [rip + @c8]
        mov     r8, qword ptr [rdi]            // r8 = [rdi] = b64
        mov     r9, qword ptr [rdx]            // r9 = [rdx] = b
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
        // decode 32 bytes on input into 24 binary bytes per loop iteration
@1:     vmovdqu xmm0, oword ptr [r8]
        vinserti128 ymm10, ymm0, oword ptr [r8 + 16], 1
        vpsrld  ymm0, ymm10, 4
        vpand   ymm11, ymm1, ymm0
        vpand   ymm0, ymm1, ymm10
        vpshufb ymm12, ymm5, ymm11
        vpshufb ymm0, ymm4, ymm0
        vptest  ymm0, ymm12
        jnz     @err
        add     r8, 32
        vpcmpeqb ymm0, ymm10, ymm9
        vpaddb  ymm0, ymm0, ymm11
        vpshufb ymm0, ymm8, ymm0
        vpaddb  ymm0, ymm0, ymm10
        vpmaddubsw ymm0, ymm0, ymm3
        vpmaddwd ymm0, ymm0, ymm2
        vpshufb ymm0, ymm0, ymm7
        vpermd  ymm0, ymm6, ymm0
        vmovdqu oword ptr [r9], xmm0
        vextracti128 oword ptr [r9 + 16], ymm0, 1
        add     r9, 24
        sub     rax, 1
        jne     @1
        jmp     @8
@err:   shl     rax, 5
        add     qword ptr [rsi], rax // restore proper b64len on error
@8:     mov     qword ptr [rdi], r8
        mov     qword ptr [rdx], r9
        vzeroupper
        {$ifdef WIN64ABI}
        jmp     @done
        {$else}
        ret
        {$endif WIN64ABI}
{$ifdef FPC} align 32 {$else} .align 16 {Delphi doesn't allow 32 :(} {$endif}
@c0:    dq 2F2F2F2F2F2F2F2FH
        dq 2F2F2F2F2F2F2F2FH
        dq 2F2F2F2F2F2F2F2FH
        dq 2F2F2F2F2F2F2F2FH
@c1:    dq 0804080402011010H
        dq 1010101010101010H
        dq 0804080402011010H
        dq 1010101010101010H
@c2:    dq 1111111111111115H
        dq 1A1B1B1B1A131111H
        dq 1111111111111115H
        dq 1A1B1B1B1A131111H
@c3:    dq 2F2F2F2F2F2F2F2FH
        dq 2F2F2F2F2F2F2F2FH
        dq 2F2F2F2F2F2F2F2FH
        dq 2F2F2F2F2F2F2F2FH
@c4:    dq 0B9B9BFBF04131000H
        dq 0000000000000000H
        dq 0B9B9BFBF04131000H
        dq 0000000000000000H
@c5:    dq 0140014001400140H
        dq 0140014001400140H
        dq 0140014001400140H
        dq 0140014001400140H
@c6:    dq 0001100000011000H
        dq 0001100000011000H
        dq 0001100000011000H
        dq 0001100000011000H
@c7:    dq 090A040506000102H
        dq 0FFFFFFFF0C0D0E08H
        dq 090A040506000102H
        dq 0FFFFFFFF0C0D0E08H
@c8:    dq 0000000100000000H
        dq 0000000400000002H
        dq 0000000600000005H
        dq 0FFFFFFFFFFFFFFFFH
@done:  {$ifdef WIN64ABI}
        pop     rdi
        pop     rsi
        movups  xmm6, dqword ptr [bak6]
        movups  xmm7, dqword ptr [bak7]
        movups  xmm8, dqword ptr [bak8]
        movups  xmm9, dqword ptr [bak9]
        movups  xmm10, dqword ptr [bak10]
        movups  xmm11, dqword ptr [bak11]
        movups  xmm12, dqword ptr [bak12]
        {$endif WIN64ABI}
end;

{$endif ASMX64AVXNOCONST}

{$endif CPUX64ASM} // SSE2 asm is invalid prior to Delphi XE7 (to be refined)


// functions below are always available, even on DARWIN

function SortDynArrayInteger(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     r8d, dword ptr [A]
        mov     edx, dword ptr [B]
        xor     eax, eax
        xor     ecx, ecx
        cmp     r8d, edx
        setl    cl
        setg    al
        sub     eax, ecx
end;

function SortDynArrayCardinal(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     ecx, dword ptr [A]
        mov     edx, dword ptr [B]
        xor     eax, eax
        cmp     ecx, edx
        seta    al
        sbb     eax, 0
end;

function SortDynArrayInt64(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     r8,  qword ptr [A]
        mov     rdx, qword ptr [B]
        xor     eax, eax
        xor     ecx, ecx
        cmp     r8, rdx
        setl    cl
        setg    al
        sub     eax, ecx
end;

function SortDynArrayQWord(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rcx, qword ptr [A]
        mov     rdx, qword ptr [B]
        xor     eax, eax
        cmp     rcx, rdx
        seta    al
        sbb     eax, 0
end;

function SortDynArrayPointer(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rcx, qword ptr [A]
        mov     rdx, qword ptr [B]
        xor     eax, eax
        cmp     rcx, rdx
        seta    al
        sbb     eax, 0
end;

function SortDynArrayDouble(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        movsd   xmm0, qword ptr [A]
        movsd   xmm1, qword ptr [B]
        xor     eax, eax
        xor     edx, edx
        comisd  xmm0, xmm1
        seta    al
        setb    dl
        sub     eax, edx
end;

function SortDynArraySingle(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        movss   xmm0, dword ptr [A]
        movss   xmm1, dword ptr [B]
        xor     eax, eax
        xor     edx, edx
        comiss  xmm0, xmm1
        seta    al
        setb    dl
        sub     eax, edx
end;

function SortDynArrayAnsiString(const A, B): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        // x86_64 version optimized for AnsiString/RawUtf8/RawByteString types
        mov     rax, qword ptr [A]
        mov     rdx, qword ptr [B]
        cmp     rax, rdx
        je      @z
        test    rax, rdx
        jz      @maynil
@f:     movzx   ecx, byte ptr [rax]    // first char comparison
        movzx   r8d, byte ptr [rdx]    // 32-bit to avoid overflow
        sub     ecx, r8d
        je      @s
        mov     eax, ecx              // branchless execution on Quicksort
        ret
@1:     mov     eax, 1
        ret
@z:     xor     eax, eax
        ret
@maynil:test    rdx, rdx       // A or B may be ''
        jz      @1
        test    rax, rax
        jnz     @f
        dec     eax
        ret
        {$ifdef FPC}
@s:     mov     r9, qword ptr [rax - _STRLEN]   // TStrLen=SizeInt on FPC
        mov     r8, r9
        sub     r9, qword ptr [rdx - _STRLEN]   // r9 = length(A)-length(B)
        {$else}
        .align 16
@s:     mov     r9d, dword ptr [rax - _STRLEN]  // TStrLen=integer on Delphi
        mov     r8, r9
        mov     r10d, dword ptr [rdx - _STRLEN]
        sub     r9, r10                         // r9 = length(A)-length(B)
        {$endif FPC}
        adc     rcx, -1
        and     rcx, r9
        sub     rcx, r8                  // rcx = -min(length(A),length(B))
        sub     rax, rcx
        sub     rdx, rcx
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@by8:   mov     r10, qword ptr [rax + rcx]  // compare 8 bytes at once
        xor     r10, qword ptr [rdx + rcx]
        jnz     @d
        add     rcx, 8
        js      @by8
@eq:    mov     eax, r9d    // all chars equal -> returns length(A)-length(B)
        ret
@d:     bsf     r10, r10    // char differs -> returns pbyte(A)^-pbyte(B)^
        shr     r10, 3
        add     rcx, r10
        jns     @eq
        movzx   eax, byte ptr [rax + rcx]
        movzx   edx, byte ptr [rdx + rcx]
        sub     eax, edx
end; // note: SSE4.2 read up to 16 bytes after buffer, this version won't

function Hash32(Data: PCardinalArray; Len: integer): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
        xor     eax, eax
        xor     r9d, r9d
        test    Data, Data
        jz      @z
        {$ifdef WIN64ABI}  // rcx/rdi=Data edx/esi=Len
        mov     r8, rdx
        shr     r8, 4
        {$else}
        mov     edx, esi
        shr     esi, 4
        {$endif WIN64ABI}
        jz      @by4
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@by16:  add     eax, dword ptr [Data]
        add     r9d, eax
        add     eax, dword ptr [Data + 4]
        add     r9d, eax
        add     eax, dword ptr [Data + 8]
        add     r9d, eax
        add     eax, dword ptr [Data + 12]
        add     r9d, eax
        add     Data, 16
        {$ifdef WIN64ABI}
        sub     r8d, 1
        {$else}
        sub     esi, 1
        {$endif WIN64ABI}
        jnz     @by16
@by4:   mov     dh, dl
        and     dl, 15
        jz      @0
        shr     dl, 2
        jz      @rem
@4:     add     eax, dword ptr [Data]
        add     r9d, eax
        add     Data, 4
        dec     dl
        jnz     @4
@rem:   and     dh, 3
        jz      @0
        dec     dh
        jz      @1
        dec     dh
        jz      @2
        mov     ecx, dword ptr [Data]
        and     ecx, $ffffff
        jmp     @e
@2:     movzx   ecx, word ptr [Data]
        jmp     @e
@1:     movzx   ecx, byte ptr [Data]
@e:     add     eax, ecx
@0:     add     r9d, eax
        shl     r9d, 16
        xor     eax, r9d
@z:
end;

function xxHash32(crc: cardinal; P: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe{$endif}
        {$ifdef SYSVABI} // crc=rdi P=rsi len=rdx
        mov     r8, rdi
        mov     rcx, rsi
        {$else} // crc=r8 P=rcx len=rdx
        mov     r10, r8
        mov     r8, rcx
        mov     rcx, rdx
        mov     rdx, r10
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        {$endif SYSVABI}
        // P=r8 len=rcx crc=rdx
        push    r12
        push    rbx
        mov     r12d, -1640531535
        lea     r10, [rcx + rdx]
        lea     eax, [r8 + 165667B1H]
        cmp     rdx, 15
        jbe     @2
        lea     rsi, [r10 - 16]
        lea     ebx, [r8 + 24234428H]
        lea     edi, [r8 - 7A143589H]
        lea     eax, [r8 + 61C8864FH]
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@1:     imul    r9d, dword ptr [rcx], -2048144777
        add     rcx, 16
        imul    r11d, dword ptr [rcx - 12], -2048144777
        add     ebx, r9d
        lea     r9d, [r11 + rdi]
        rol     ebx, 13
        rol     r9d, 13
        imul    ebx, r12d
        imul    edi, r9d, -1640531535
        imul    r9d, dword ptr [rcx - 8], -2048144777
        add     r8d, r9d
        imul    r9d, dword ptr [rcx - 4], -2048144777
        rol     r8d, 13
        imul    r8d, r12d
        add     eax, r9d
        rol     eax, 13
        imul    eax, r12d
        cmp     rsi, rcx
        jnc     @1
        rol     edi, 7
        rol     ebx, 1
        rol     r8d, 12
        mov     r9d, edi
        ror     eax, 14
        add     r9d, ebx
        add     r8d, r9d
        add     eax, r8d
@2:     lea     r9, [rcx+4H]
        add     eax, edx
        cmp     r10, r9
        jc      @4
        mov     r8, r9
@3:     imul    edx, dword ptr [r8 - 4], -1028477379
        add     r8, 4
        add     eax, edx
        ror     eax, 15
        imul    eax, 668265263
        cmp     r10, r8
        jnc     @3
        lea     rdx, [r10 - 4]
        sub     rdx, rcx
        mov     rcx, rdx
        and     rcx, 0FFFFFFFFFFFFFFFCH
        add     rcx, r9
@4:     cmp     r10, rcx
        jbe     @6
@5:     movzx   edx, byte ptr [rcx]
        add     rcx, 1
        imul    edx, 374761393
        add     eax, edx
        rol     eax, 11
        imul    eax, r12d
        cmp     r10, rcx
        jnz     @5
@6:     mov     edx, eax
        shr     edx, 15
        xor     eax, edx
        imul    eax, -2048144777
        mov     edx, eax
        shr     edx, 13
        xor     eax, edx
        imul    eax, -1028477379
        mov     edx, eax
        shr     edx, 16
        xor     eax, edx
        pop     rbx
        pop     r12
        {$ifndef SYSVABI}
        pop     rdi
        pop     rsi
        {$endif SYSVABI}
end;

function GetBitsCountPas(value: PtrInt): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        mov     rax, value
        mov     rdx, value
        shr     rax, 1
        mov     rcx, $5555555555555555
        mov     r8,  $3333333333333333
        mov     r10, $0f0f0f0f0f0f0f0f
        mov     r11, $0101010101010101
        and     rax, rcx
        sub     rdx, rax
        mov     rax, rdx
        shr     rdx, 2
        and     rax, r8
        and     rdx, r8
        add     rax, rdx
        mov     rdx, rax
        shr     rax, 4
        add     rax, rdx
        and     rax, r10
        imul    rax, r11
        shr     rax, 56
end;

procedure mul64x64(const left, right: QWord; out product: THash128Rec);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif}
        // uses built-in 64-bit -> 128-bit multiplication
        {$ifdef WIN64ABI}
        mov     rax, rcx
        mul     rdx
        {$else}
        mov     r8, rdx
        mov     rax, rdi
        mul     rsi
        {$endif WIN64ABI}
        mov     qword ptr [r8], rax
        mov     qword ptr [r8 + 8], rdx
end;

function bswap32(a: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, a
        bswap   eax
end;

function bswap64(const a: QWord): QWord;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rax, a
        bswap   rax
end;

procedure bswap64array(a, b: PQWordArray; n: PtrInt);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
@1:     mov     rax, qword ptr [a]
        bswap   rax
        mov     qword ptr [b], rax
        add     a, 8
        add     b, 8
        sub     n, 1
        jnz @1
end;

function StrLen(S: pointer): PtrInt;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        // from GPL strlen64.asm by Agner Fog - www.agner.org/optimize
        {$ifdef WIN64ABI}
        mov     rax, rcx             // get pointer to string from rcx
        mov     r8,  rcx             // copy pointer
        test    rcx, rcx
        {$else}
        mov     rax, rdi
        mov     ecx, edi
        test    rdi, rdi
        {$endif WIN64ABI}
        jz      @null                // returns 0 if S=nil
        // rax=s,ecx=32-bit of s
        pxor    xmm0, xmm0           // set to zero
        and     ecx, 15              // lower 4 bits indicate misalignment
        and     rax, -16             // align pointer by 16
        // will never read outside a memory page boundary, so won't trigger GPF
        movaps  xmm1, [rax]          // read from nearest preceding boundary
        pcmpeqb xmm1, xmm0           // compare 16 bytes with zero
        pmovmskb edx, xmm1           // get one bit for each byte result
        shr     edx, cl              // shift out false bits
        shl     edx, cl              // shift back again
        bsf     edx, edx             // find first 1-bit
        jnz     @L2                  // found
        // Main loop, search 16 bytes at a time
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@L1:    add     rax, 10H             // increment pointer by 16
        movaps  xmm1, [rax]          // read 16 bytes aligned
        pcmpeqb xmm1, xmm0           // compare 16 bytes with zero
        pmovmskb edx, xmm1           // get one bit for each byte result
        bsf     edx, edx             // find first 1-bit
        // (moving the bsf out of the loop and using test here would be faster
        // for long strings on old processors, but we are assuming that most
        // strings are short, and newer processors have higher priority)
        jz      @L1                  // loop if not found
@L2:    // Zero-byte found. Compute string length
        {$ifdef WIN64ABI}
        sub     rax, r8              // subtract start address
        {$else}
        sub     rax, rdi
        {$endif WIN64ABI}
        add     rax, rdx             // add byte index
@null:
end;

function PosChar(Str: PUtf8Char; Chr: AnsiChar): PUtf8Char;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        // fast SSE2 version searching for both Chr and #0 over 16 bytes at once
        {$ifdef WIN64ABI}
        movzx   edx, dl
        mov     rax, rcx             // get pointer to string from rcx
        imul    edx, $01010101
        test    rcx, rcx
        {$else}
        imul    edx, esi, $01010101
        mov     rax, rdi
        mov     ecx, edi
        test    rdi, rdi
        {$endif WIN64ABI}
        jz      @null                // returns 0 if S=nil
        movd    xmm1, edx
        // rax=Str, ecx=32-bit of Str, xmm1=Chr
        pxor    xmm0, xmm0           // set xmm0 to zero
        pshufd  xmm1, xmm1, 0        // set Chr into all bytes of xmm1
        and     ecx, 15              // lower 4 bits indicate misalignment
        and     rax, -16             // align pointer by 16
        // will never read outside a memory page boundary, so won't trigger GPF
        movaps  xmm2, [rax]          // read from nearest preceding boundary
        movaps  xmm3, xmm2
        pcmpeqb xmm2, xmm0           // compare 16 bytes with zero
        pcmpeqb xmm3, xmm1           // compare 16 bytes with Chr
        por     xmm2, xmm3
        pmovmskb edx, xmm2           // get one bit for each byte result
        shr     edx, cl              // shift out false bits
        shl     edx, cl              // shift back again
        bsf     edx, edx             // find first 1-bit
        jnz     @L2                  // found
        // Main loop, search 16 bytes at a time
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@L1:    add     rax, 10H             // increment pointer by 16
        movaps  xmm2, [rax]          // read 16 bytes aligned
        movaps  xmm3, xmm2
        pcmpeqb xmm2, xmm0           // compare 16 bytes with zero
        pcmpeqb xmm3, xmm1           // compare 16 bytes with Chr
        por     xmm2, xmm3
        pmovmskb edx, xmm2           // get one bit for each byte result
        bsf     edx, edx             // find first 1-bit
        // (moving the bsf out of the loop and using test here would be faster
        // for long strings on old processors, but we are assuming that most
        // strings are short, and newer processors have higher priority)
        jz      @L1                  // loop if not found
@L2:    // Zero or Chr byte found
        add     rax, rdx             // add byte index for rax = match address
        cmp     byte ptr [rax], 0
        jz      @z                   // return nil if zero was reached
@null:  ret
@z:     xor     eax, eax
end;

function BufferLineLength(Text, TextEnd: PUtf8Char): PtrInt;
{$ifdef FPC} nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifdef WIN64ABI} // Win64 ABI to System-V ABI
        push    rsi
        push    rdi
        mov     rdi, rcx
        mov     rsi, rdx
        {$endif WIN64ABI}
        mov     r8, rsi
        sub     r8, rdi            // rdi=Text, rsi=TextEnd, r8=TextLen
        jz      @fail
        mov     ecx, edi
        movaps  xmm0, [rip + @for10]
        movaps  xmm1, [rip + @for13]
        and     rdi, -16           // check first aligned 16 bytes
        and     ecx, 15            // lower cl 4 bits indicate misalignment
        movaps  xmm2, [rdi]
        movaps  xmm3, xmm2
        pcmpeqb xmm2, xmm0
        pcmpeqb xmm3, xmm1
        por     xmm3, xmm2
        pmovmskb eax, xmm3
        shr     eax, cl            // shift out unaligned bytes
        bsf     eax, eax
        jz      @main
        add     rax, rcx
        add     rax, rdi
        sub     rax, rsi
        jae     @fail              // don't exceed TextEnd
        add     rax, r8 // rax = TextFound - TextEnd + (TextEnd - Text) = offset
        {$ifdef WIN64ABI}
        pop     rdi
        pop     rsi
        {$endif WIN64ABI}
        ret
@main:  add     rdi, 16
        sub     rdi, rsi
        jae     @fail
        jmp     @by16
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@for10: dq      $0a0a0a0a0a0a0a0a
        dq      $0a0a0a0a0a0a0a0a
@for13: dq      $0d0d0d0d0d0d0d0d
        dq      $0d0d0d0d0d0d0d0d
@by16:  movaps  xmm2, [rdi + rsi]  // check 16 bytes per loop
        movaps  xmm3, xmm2
        pcmpeqb xmm2, xmm0
        pcmpeqb xmm3, xmm1
        por     xmm3, xmm2
        pmovmskb eax, xmm3
        test    eax, eax
        jnz     @found
        add     rdi, 16
        jnc     @by16
@fail:  mov     rax, r8            // returns TextLen if no CR/LF found
        {$ifdef WIN64ABI}
        pop     rdi
        pop     rsi
        {$endif WIN64ABI}
        ret
@found: bsf     eax, eax
        add     rax, rdi
        jc      @fail
        add     rax, r8
        {$ifdef WIN64ABI}
        pop     rdi
        pop     rsi
        {$endif WIN64ABI}
end;

function ByteScanIndex(P: PByteArray; Count: PtrInt; Value: byte): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        movzx   eax, r8b
        mov     r8, rcx
        test    rdx, rdx
        {$else}
        movzx   eax, dl
        mov     rcx, rdi
        mov     r8, rdi
        mov     rdx, rsi
        test    rsi, rsi
        {$endif WIN64ABI}
        jbe     @no
        // eax=Value, rcx=P rdx=Count
        imul    eax, $01010101
        and     rcx, -16
        movd    xmm1, eax
        movaps  xmm0, [rcx]      // check first aligned 16 bytes
        add     rcx, 16
        pshufd  xmm1, xmm1, 0
        sub     rcx, r8
        pcmpeqb xmm0, xmm1
        pmovmskb eax, xmm0
        shl     eax, cl
        and     eax, $FFFF0000
        shr     eax, cl
        jnz     @fnd
        cmp     rdx, rcx
        jbe     @no
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@by16:  movaps  xmm0, [r8 + rcx]  // next 16 bytes
        add     rcx, 16
        pcmpeqb xmm0, xmm1
        pmovmskb eax, xmm0
        test    eax, eax
        jnz     @fnd
        cmp     rdx, rcx
        ja      @by16
@no:    mov     rax, -1
        ret
@fnd:   bsf     eax, eax
        lea     rax, qword ptr [rcx + rax - 16]
        cmp     rdx, rax
        jbe     @no
end;

function WordScanIndex(P: PWordArray; Count: PtrInt; Value: word): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        movzx   eax, r8w
        mov     r8, rcx
        test    rdx, rdx
        {$else}
        movzx   eax, dx
        mov     rcx, rdi
        mov     r8, rdi
        mov     rdx, rsi
        test    rsi, rsi
        {$endif WIN64ABI}
        jbe     @no
        test    rcx, 1
        jnz     @unal
        // eax=Value, rcx=P rdx=Count
        movd    xmm1, eax
        and     rcx, -16
        punpcklwd xmm1, xmm1
        movaps  xmm0, [rcx]      // check first aligned 16 bytes
        add     rcx, 16
        pshufd  xmm1, xmm1, 0
        sub     rcx, r8
        pcmpeqw xmm0, xmm1
        pmovmskb eax, xmm0
        shl     eax, cl
        and     eax, $FFFF0000
        shr     eax, cl
        shr     ecx, 1
        bsf     eax, eax
        jnz     @fnd
        cmp     rdx, rcx
        jbe     @no
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@by16:  movaps  xmm0, [r8 + rcx * 2]  // next 16 bytes
        add     rcx, 8
        pcmpeqw xmm0, xmm1
        pmovmskb eax, xmm0
        bsf     eax, eax
        jnz     @fnd
        cmp     rdx, rcx
        ja      @by16
@no:    mov     rax, -1
        ret
        bsf     eax, eax
@fnd:   shr     eax, 1
        lea     rax, qword ptr [rcx + rax - 8]
        cmp     rdx, rax
        jbe     @no
        ret
        // 16bit-unaligned loop (seldom called)
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@unal:  lea     rcx, [rcx + rdx * 2]
        neg     rdx
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@unals: cmp     word ptr [rcx + rdx * 2], ax
        jz      @unale
        add     rdx, 1
        jnz     @unals
        jmp     @no
@unale: lea     rax, [rcx + rdx * 2]
        sub     rax, r8
        shr     rax, 1
end;

function IntegerScanIndex(P: PCardinalArray; Count: PtrInt; Value: cardinal): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        mov     eax, r8d
        movd    xmm1, r8d
        mov     r8, rcx
        test    rdx, rdx
        {$else}
        mov     eax, edx
        movd    xmm1, edx
        mov     rcx, rdi
        mov     r8, rdi
        mov     rdx, rsi
        test    rsi, rsi
        {$endif WIN64ABI}
        jbe     @no
        test    rcx, 3
        jnz     @unal
        // eax=Value, rcx=P rdx=Count
        and     rcx, -16
        movaps  xmm0, [rcx]      // check first aligned 16 bytes
        add     rcx, 16
        pshufd  xmm1, xmm1, 0
        sub     rcx, r8
        pcmpeqd xmm0, xmm1
        pmovmskb eax, xmm0
        shl     eax, cl
        and     eax, $FFFF0000
        shr     eax, cl
        shr     ecx, 2
        bsf     eax, eax
        jnz     @fnd
        cmp     rdx, rcx
        jbe     @no
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@by16:  movaps  xmm0, [r8 + rcx * 4]  // next 16 bytes
        add     rcx, 4
        pcmpeqd xmm0, xmm1
        pmovmskb eax, xmm0
        bsf     eax, eax
        jnz     @fnd
        cmp     rdx, rcx
        ja      @by16
@no:    mov     rax, -1
        ret
        bsf     eax, eax
@fnd:   shr     eax, 2
        lea     rax, qword ptr [rcx + rax - 4]
        cmp     rdx, rax
        jbe     @no
        ret
        // 32bit-unaligned loop (seldom called)
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@unal:  lea     rcx, [rcx + rdx * 4]
        neg     rdx
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@unals: cmp     dword ptr [rcx + rdx * 4], eax
        jz      @unale
        add     rdx, 1
        jnz     @unals
        jmp     @no
@unale: lea     rax, [rcx + rdx * 4]
        sub     rax, r8
        shr     rax, 2
end;

function MemCmp(P1, P2: PByteArray; L: PtrInt): integer;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
// from GPL memcmp64.asm by Agner Fog - www.agner.org/optimize
          add     P1, L    // use negative index from end of memory block
          add     P2, L
          neg     L
          jge     @eq
          mov     r9d, $FFFF             // 16 bits mask = 16 bytes
          cmp     L, -16
          ja      @sml
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@by16:    // loop comparing 16 bytes
          movups  xmm1, oword ptr [P1 + L]
          movups  xmm2, oword ptr [P2 + L]
          pcmpeqb xmm1, xmm2             // compare 16 bytes
          pmovmskb eax, xmm1             // get byte mask
          xor     eax, r9d               // not ax
          jnz     @diff                  // difference found
          add     L, 16
          jz      @eq                    // finished, equal
          cmp     L, -16
          jna     @by16                  // next 16 bytes
          jmp     @sml
@diff:    // difference found: find position
          bsf     eax, eax
          add     L, rax
@last:    movzx   eax, byte ptr [P1 + L]
          movzx   r9d, byte ptr [P2 + L]
          sub     eax, r9d               // return result
          ret
@eq:      // equal
          xor     eax, eax
          ret
@sml:     // less than 16 bytes left
          cmp     L, -8
          ja      @less8
          // compare 8 bytes
          movq    xmm1, qword ptr [P1 + L]
          movq    xmm2, qword ptr [P2 + L]
          pcmpeqb xmm1, xmm2             // compare 8 bytes
          pmovmskb eax, xmm1             // get byte mask
          xor     eax, r9d               // not ax
          jnz     @diff                  // difference found
          add     L, 8
          jz      @eq
@less8:   // less than 8 bytes left
          cmp     L, -4
          ja      @less4
          // compare 4 bytes
          movd    xmm1, dword ptr [P1 + L]
          movd    xmm2, dword ptr [P2 + L]
          pcmpeqb xmm1, xmm2             // compare 4 bytes
          pmovmskb eax, xmm1             // get byte mask
          xor     eax, r9d               // not ax
          jnz     @diff                  // difference found
          add     L, 4
          jz      @eq
@less4:   // less than 4 bytes left
          cmp     L, -2
          ja      @less2
          movzx   eax, word ptr [P1 + L]
          movzx   r9d, word ptr [P2 + L]
          sub     eax, r9d
          jnz     @last2                 // difference in byte 0 or 1
          add     L, 2
          jz      @eq
@less2:   // less than 2 bytes left
          test    L, L
          jz      @eq                    // no bytes left
          jmp     @last                  // one byte left
@last2:   // difference in byte 0 or 1
          neg     al
          sbb     L, -1                  // add 1 to L if al == 0
          jmp     @last
end;

function FastFindWordSorted(P: PWordArray; R: PtrInt; Value: Word): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        {$ifdef WIN64ABI}
        push    rdi
        mov     rdi, P  // rdi=P - we use ecx to read the word ptr value
        {$endif WIN64ABI}
        xor     r9, r9  // r9=L rax=result
        test    R, R
        jl      @ko
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     lea     rax, [r9 + R]
        shr     rax, 1
        lea     r10, qword ptr [rax - 1] // branchless loop
        lea     r11, qword ptr [rax + 1]
        movzx   ecx, word ptr [rdi + rax * 2]
        {$ifdef WIN64ABI}
        cmp     ecx, r8d
        {$else}
        cmp     ecx, edx // 'cmp cx,Value' is silently rejected by Darwin asm
        {$endif WIN64ABI}
        je      @ok
        cmovg   R, r10
        cmovl   r9, r11
        cmp     r9, R
        jle     @s
@ko:    mov     rax, -1
@ok:    {$ifdef WIN64ABI}
        pop     rdi
        {$endif WIN64ABI}
end;

function FastFindIntegerSorted(P: PIntegerArray; R: PtrInt; Value: integer): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        xor     r9, r9  // r9=L rax=result
        test    R, R
        jl      @ko
        lea     rax, [r9 + R]
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     shr     rax, 1
        lea     r10, qword ptr [rax - 1]  // efficient branchless binary search
        lea     r11, qword ptr [rax + 1]
        cmp     Value, dword ptr [P + rax * 4]
        je      @ok
        cmovl   R, r10
        cmovg   r9, r11
        lea     rax, [r9 + R]
        cmp     r9, R
        jle     @s
@ko:    mov     rax, -1
@ok:
end;

function FastFindInt64Sorted(P: PInt64Array; R: PtrInt; const Value: Int64): PtrInt;
{$ifdef FPC} assembler; nostackframe; asm {$else} asm .noframe {$endif}
        xor     r9, r9  // r9=L rax=result
        test    R, R
        jl      @ko
        lea     rax, [r9 + R]
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@s:     shr     rax, 1
        lea     r10, qword ptr [rax - 1]  // efficient branchless binary search
        lea     r11, qword ptr [rax + 1]
        cmp     Value, qword ptr [P + rax * 8]
        je      @ok
        cmovl   R, r10
        cmovg   r9, r11
        lea     rax, [r9 + R]
        cmp     r9, R
        jle     @s
@ko:    mov     rax, -1
@ok:
end;

function GetBitsCountSse42(value: PtrInt): PtrInt;
{$ifdef FPC} assembler; nostackframe;
asm
        popcnt  rax, value
{$else} // oldest Delphi don't support this opcode
asm     .noframe
        {$ifdef WIN64ABI}
        db $f3,$48,$0f,$B8,$c1
        {$else}
        db $f3,$48,$0f,$B8,$c7
        {$endif WIN64ABI}
{$endif FPC}
end;

function crc32cby4sse42(crc, value: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, crc
        crc32   eax, value
end;

procedure crcblocksse42(crc128, data128: PBlock128);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax,  dword ptr [crc128] // we can't use two qword ptr here
        mov     r8d,  dword ptr [crc128 + 4]
        mov     r9d,  dword ptr [crc128 + 8]
        mov     r10d, dword ptr [crc128 + 12]
        crc32   eax,  dword ptr [data128]
        crc32   r8d,  dword ptr [data128 + 4]
        crc32   r9d,  dword ptr [data128 + 8]
        crc32   r10d, dword ptr [data128 + 12]
        mov     dword ptr [crc128],      eax
        mov     dword ptr [crc128 + 4],  r8d
        mov     dword ptr [crc128 + 8],  r9d
        mov     dword ptr [crc128 + 12], r10d
end;

procedure crcblockssse42(crc128, data128: PBlock128; count: integer);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        test    count, count
        jle     @z
        mov     rax, data128
        {$ifdef WIN64ABI}
        mov     rdx, rcx
        mov     ecx, r8d
        {$else}
        mov     ecx, edx
        mov     rdx, rdi
        {$endif WIN64ABI}
        mov     r8d,  dword ptr [rdx] // we can't use qword ptr here
        mov     r9d,  dword ptr [rdx + 4]
        mov     r10d, dword ptr [rdx + 8]
        mov     r11d, dword ptr [rdx + 12]
{$ifdef FPC} align 16 {$else} .align 16 {$endif}
@s:     crc32   r8d,  dword ptr [rax]
        crc32   r9d,  dword ptr [rax + 4]
        crc32   r10d, dword ptr [rax + 8]
        crc32   r11d, dword ptr [rax + 12]
        add     rax, 16
        sub     ecx, 1
        jnz     @s
        mov     dword ptr [rdx], r8d
        mov     dword ptr [rdx + 4], r9d
        mov     dword ptr [rdx + 8], r10d
        mov     dword ptr [rdx + 12], r11d
@z:
end;

function crc32csse42(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, crc
        test    len, len
        jz      @z
        test    buf, buf
        jz      @z
        not     eax
        mov     ecx, len
        shr     len, 3
        jnz     @by8 // no read alignment care here - but in crypto.core
@0:     test    cl, 4
        jz      @4
        crc32   eax, dword ptr [buf]
        add     buf, 4
@4:     test    cl, 2
        jz      @2
        crc32   eax, word ptr [buf]
        add     buf, 2
@2:     test    cl, 1
        jz      @1
        crc32   eax, byte ptr [buf]
@1:     not     eax
@z:     ret
        {$ifdef FPC}
        align 16
@by8:   crc32   rax, qword ptr [buf] // hash 8 bytes per loop
        {$else}
        .align 16
@by8:   db $F2,$48,$0F,$38,$F1,$02 // circumvent Delphi inline asm compiler bug
        {$endif FPC}
        add     buf, 8
        sub     len, 1
        jnz     @by8
        jmp     @0
end;

function SynLZcompress1(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
var
  off: TOffsets;
  cache: array[0..4095] of cardinal; // uses 32KB+16KB=48KB on stack
asm // rcx=src, edx=size, r8=dest
        {$ifdef WIN64ABI} // additional registers to preserve
        push    rdi
        push    rsi
        {$else} // Linux 64-bit ABI
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif WIN64ABI}
        push    rbx
        push    r12
        push    r13
        push    r14
        push    r15
        mov     r15, r8            // r8=dest r15=dst_beg
        mov     rbx, rcx           // rbx=src
        cmp     edx, 32768
        jc      @03
        mov     eax, edx
        and     eax, 7FFFH
        or      eax, 8000H
        mov     word ptr [r8], ax
        mov     eax, edx
        shr     eax, 15
        mov     word ptr [r8 + 2], ax
        add     r8, 4
        jmp     @05
@03:    mov     word ptr [r8], dx
        test    edx, edx
        jnz     @04
        mov     r15d, 2
        jmp     @19
        nop
@04:    add     r8, 2
@05:    lea     r9, [rdx + rbx]    // r9=src_end
        lea     r10, [r9 - 11]     // r10=src_endmatch
        mov     ecx, 1             // ecx=CWBits
        mov     r11, r8            // r11=CWpoint
        mov     dword ptr [r8], 0
        add     r8, 4
        pxor    xmm0, xmm0
        mov     eax, 32768 - 64
@06:    movaps  dqword ptr [off + rax - 48], xmm0 // stack is 16 bytes aligned
        movaps  dqword ptr [off + rax - 32], xmm0
        movaps  dqword ptr [off + rax - 16], xmm0
        movaps  dqword ptr [off + rax], xmm0
        sub     eax, 64
        jae     @06
        cmp     rbx, r10
        ja      @15
@07:    mov     edx, dword ptr [rbx]
        mov     rax, rdx
        mov     r12, rdx
        shr     rax, 12
        xor     rax, rdx
        and     rax, 0FFFH                     // rax=h
        mov     r14, qword ptr [off + rax * 8] // r14=o
        mov     edx, dword ptr [cache + rax * 4]
        mov     qword ptr [off + rax * 8], rbx
        mov     dword ptr [cache + rax * 4], r12d
        xor     rdx, r12
        test    r14, r14
        lea     rdi, [r9-1]
        je      @12
        and     rdx, 0FFFFFFH
        jne     @12
        mov     rdx, rbx
        sub     rdx, r14
        cmp     rdx, 2
        jbe     @12
        or      dword ptr [r11], ecx
        add     rbx, 2
        add     r14, 2
        mov     esi, 1
        sub     rdi, rbx
        cmp     rdi, 271
        jc      @09
        mov     edi, 271
        jmp     @09
@08:    add     rsi, 1
@09:    mov     edx, dword ptr [r14 + rsi]
        cmp     dl, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dh, byte ptr [rbx + rsi]
        jnz     @10
        shr     edx, 16
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dl, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jge     @10
        add     rsi, 1
        cmp     dh, byte ptr [rbx + rsi]
        jnz     @10
        cmp     rsi, rdi
        jc      @08
@10:    add     rbx, rsi
        shl     rax, 4
        cmp     rsi, 15
        ja      @11
        or      rax, rsi
        mov     word ptr [r8], ax
        add     r8, 2
        jmp     @13
@11:    sub     rsi, 16
        mov     word ptr [r8], ax
        mov     byte ptr [r8 + 2], sil
        add     r8, 3
        jmp     @13
@12:    mov     al, byte ptr [rbx]
        mov     byte ptr [r8], al
        add     rbx, 1
        add     r8, 1
@13:    add     ecx, ecx
        jnz     @14
        mov     r11, r8
        mov     [r8], ecx
        add     r8, 4
        add     ecx, 1
@14:    cmp     rbx, r10
        jbe     @07
@15:    cmp     rbx, r9
        jnc     @18
@16:    mov     al, byte ptr [rbx]
        mov     byte ptr [r8], al
        add     rbx, 1
        add     r8, 1
        add     ecx, ecx
        jnz     @17
        mov     [r8], ecx
        add     r8, 4
        add     ecx, 1
@17:    cmp     rbx, r9
        jc      @16
@18:    sub     r8, r15
        mov     r15, r8
@19:    mov     rax, r15
        pop     r15
        pop     r14
        pop     r13
        pop     r12
        pop     rbx
        {$ifdef WIN64ABI} // additional registers to preserve
        pop     rsi
        pop     rdi
        {$endif WIN64ABI}
end;

function SynLZdecompress1(src: PAnsiChar; size: integer; dst: PAnsiChar): integer;
var
  off: TOffsets;
asm     // rcx=src, edx=size, r8=dest
        {$ifdef WIN64ABI} // additional registers to preserve
        push    rsi
        push    rdi
        {$else} // Linux 64-bit ABI
        mov     r8, rdx
        mov     rdx, rsi
        mov     rcx, rdi
        {$endif WIN64ABI}
        push    rbx
        push    r12
        push    r13
        push    r14
        push    r15
        movzx   eax, word ptr [rcx]    // rcx=src   eax=result
        lea     r9, [rcx + rdx]        // r9=src_end
        test    eax, eax
        je      @35
        add     rcx, 2
        mov     r10d, eax
        and     r10d, 8000H
        jz      @21
        movzx   ebx, word ptr [rcx]
        shl     ebx, 15
        mov     r10d, eax
        and     r10d, 7FFFH
        or      r10d, ebx
        mov     eax, r10d
        add     rcx, 2
@21:    lea     r10, [r8 - 1]          // r10=last_hashed  r8=dest
@22:    mov     edi, dword ptr [rcx]   // edi=CW
        add     rcx, 4
        mov     r13d, 1                // r13d=CWBit
        cmp     rcx, r9
        jnc     @35
@23:    test    r13d, edi
        jnz     @25
        mov     bl, byte ptr [rcx]
        mov     byte ptr [r8], bl
        add     rcx, 1
        lea     rbx, [r8 - 2]
        add     r8, 1
        cmp     rcx, r9
        jnc     @35
        cmp     rbx, r10
        jbe     @24
        add     r10, 1
        mov     esi, dword ptr [r10]
        mov     rbx, rsi
        shr     esi, 12
        xor     ebx, esi
        and     ebx, 0FFFH
        mov     qword ptr [off + rbx * 8], r10
@24:    shl     r13d, 1
        jnz     @23
        jmp     @22
@25:    movzx   r11, word ptr [rcx]    // r11=t
        add     rcx, 2
        mov     ebx, r11d              // ebx=h
        shr     ebx, 4
        and     r11, 0FH
        lea     r11, [r11 + 2]
        jnz     @26
        movzx   r11, byte ptr [rcx]
        add     rcx, 1
        lea     r11, [r11 + 12H]
@26:    mov     r14, qword ptr [off + rbx * 8] // r14=o
        mov     rbx, r8
        xor     rsi, rsi
        sub     rbx, r14
        mov     r12, r11
        mov     r15, r11
        cmp     rbx, r11
        jc      @29
        shr     r12, 3
        jz      @30
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@27:    mov     rbx, qword ptr [r14 + rsi] // inline move by 8 bytes
        mov     qword ptr [r8 + rsi], rbx
        add     rsi, 8
        sub     r12, 1
        jnz     @27
        mov     rbx, qword ptr [r14 + rsi] // 1..7 remaining bytes
        and     r15, 7
        jz      @31
@28:    mov     byte ptr [r8 + rsi], bl
        shr     rbx, 8
        add     rsi, 1
        sub     r15, 1
        jnz     @28
        jmp     @31
{$ifdef FPC} align 8 {$else} .align 8 {$endif}
@29:    mov     bl, byte ptr [r14 + rsi] // overlaping move
        mov     byte ptr [r8 + rsi], bl
        add     rsi, 1
        sub     r12, 1
        jnz     @29
        cmp     rcx, r9
        jnz     @33
        jmp     @35
@30:    mov     rbx, qword ptr [r14]
        mov     qword ptr [r8], rbx
@31:    cmp     rcx, r9
        jz      @35
        cmp     r10, r8
        jnc     @34
@32:    add     r10, 1
        mov     ebx, dword ptr [r10]
        mov     rsi, rbx
        shr     ebx, 12
        xor     esi, ebx
        and     esi, 0FFFH
        mov     qword ptr [off + rsi * 8], r10
@33:    cmp     r10, r8
        jc      @32
@34:    add     r8, r11
        lea     r10, [r8 - 1]
        shl     r13d, 1
        jnz     @23
        jmp     @22
@35:    pop     r15
        pop     r14
        pop     r13
        pop     r12
        pop     rbx
        {$ifdef WIN64ABI} // additional registers to preserve
        pop     rdi
        pop     rsi
        {$endif WIN64ABI}
end;

function RdRand32: cardinal;
{$ifdef FPC}nostackframe; assembler; asm{$else} asm .noframe {$endif FPC}
        // rdrand eax: same opcodes for x86 and x64
        db $0f, $c7, $f0
        // we ignore the carry flag (handled once in TestIntelCpuFeatures)
end;

function Rdtsc: Int64;
{$ifdef FPC}nostackframe; assembler; asm{$else} asm .noframe {$endif FPC}
        rdtsc // returns the TSC in EDX:EAX
        shl     rdx, 32
        or      rax, rdx
end;

procedure LockedInc32(int32: PInteger);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
   lock inc     dword ptr [int32]
end;

procedure LockedDec32(int32: PInteger);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
   lock dec     dword ptr [int32]
end;

procedure LockedInc64(int64: PInt64);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
   lock inc     qword ptr [int64]
end;

function InterlockedIncrement(var I: integer): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, 1
   lock xadd    dword ptr [I], eax // atomic eax=[I] + [I]:=[I]+eax
        add     eax, 1
end;

function InterlockedDecrement(var I: integer): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, -1
   lock xadd    dword ptr [I], eax // atomic eax=[I] + [I]:=[I]+eax
        sub     eax, 1
end;

function StrCntDecFree(var refcnt: TStrCnt): boolean;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifdef STRCNT32}
   lock dec dword ptr [refcnt]  // TStrCnt=longint on Delphi Win64 and FPC>=3.4
        {$else}
   lock dec qword ptr [refcnt]  // on FPC<3.4
        {$endif STRCNT32}
        setbe   al
end; // don't check for ismultithread global since lock is cheaper on new CPUs

function DACntDecFree(var refcnt: TDACnt): boolean;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        {$ifdef DACNT32}
   lock dec dword ptr [refcnt]  // TDACnt=longint on Delphi
        {$else}
   lock dec qword ptr [refcnt]
        {$endif DACNT32}
        setbe   al
end;

function LockedExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt): boolean;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rax, Comperand
   lock cmpxchg qword ptr [Target], NewValue
        setz    al
end;

procedure LockedAdd(var Target: PtrUInt; Increment: PtrUInt);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
   lock add     qword ptr [Target], Increment
end;

procedure LockedAdd32(var Target: cardinal; Increment: cardinal);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
   lock add     dword ptr [Target], Increment
end;

procedure LockedDec(var Target: PtrUInt; Decrement: PtrUInt);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
   lock sub     qword ptr [Target], Decrement
end;

{$ifdef ISDELPHI}

// those functions are intrinsics with FPC :)

function BSRdword(c: cardinal): cardinal;
asm
        .noframe
        mov     eax, c
        bsr     eax, eax
        jnz     @nz
        mov     eax, 255
@nz:
end;

function BSRqword(const q: qword): cardinal;
asm
        .noframe
        mov     rax, q
        bsr     rax, rax
        jnz     @nz
        mov     eax, 255
@nz:
end;

// FPC will properly inline multiplication by reciprocal
procedure Div100(Y: cardinal; var res: TDiv100Rec);
asm
        .noframe
        mov     r8, res
        mov     edx, Y
        mov     dword ptr [r8].TDiv100Rec.M,edx
        mov     eax, 1374389535
        mul     edx
        shr     edx, 5
        mov     dword ptr [r8].TDiv100Rec.D, edx
        imul    eax, edx, 100
        sub     dword ptr [r8].TDiv100Rec.M, eax
end;

{$endif ISDELPHI}

function IsXmmYmmOSEnabled: boolean;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
// see https://software.intel.com/en-us/blogs/2011/04/14/is-avx-enabled
        xor     ecx, ecx  // specify control register XCR0=XFEATURE_ENABLED_MASK
        db $0f, $01, $d0  // XGETBV opcode reads XCR0 into EDX:EAX
        and     eax, 6
        cmp     al, 6     // check XMM (bit 1=2) and YMM (bit 2=4)
        sete    al        // true if OS enabled both XMM and YMM
end;

procedure GetCpuid(cpueax, cpuecx: cardinal; var regs: TIntelRegisters);
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     eax, cpueax
        mov     ecx, cpuecx
        mov     r9, regs
        mov     r10, rbx  // preserve rbx
        xor     ebx, ebx
        xor     edx, edx
        cpuid
        mov     TIntelRegisters(r9).&eax, eax
        mov     TIntelRegisters(r9).&ebx, ebx
        mov     TIntelRegisters(r9).&ecx, ecx
        mov     TIntelRegisters(r9).&edx, edx
        mov     rbx, r10
end;

