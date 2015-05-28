/* 
 * Copyright (c) 2006, Opera Software ASA 
 * All rights reserved. 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met: 
 * 
 *     * Redistributions of source code must retain the above copyright 
 *       notice, this list of conditions and the following disclaimer. 
 *     * Redistributions in binary form must reproduce the above copyright 
 *       notice, this list of conditions and the following disclaimer in the 
 *       documentation and/or other materials provided with the distribution. 
 *     * Neither the name of Opera Software ASA nor the 
 *       names of its contributors may be used to endorse or promote products 
 *       derived from this software without specific prior written permission. 
 * 
 * THIS SOFTWARE IS PROVIDED BY OPERA SOFTWARE ASA AND CONTRIBUTORS ``AS IS'' AND ANY 
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
 * DISCLAIMED. IN NO EVENT SHALL OPERA SOFTWARE ASA AND CONTRIBUTORS BE LIABLE FOR ANY 
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */ 
function convertStringToBase64(string) 
{ 
  var out='', charCode=0, i=0, length=string.length; 
  var puffer=[];
  var base64EncodeChars ="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
  var dig1, dig2, dig3, dig4;

  var i = 0; 
  while(i < string.length) 
  { 
    charCode = string.charCodeAt(i++);
    if(charCode<0x80)  
    { 
      puffer[puffer.length]=charCode; 
    } 
    else if(charCode<0x800) 
    { 
      puffer[puffer.length]=0xc0|(charCode>>6); 
      puffer[puffer.length]=0x80|(charCode&0x3f); 
    }  
    else if(charCode<0x10000) 
    { 
      puffer[puffer.length]=0xe0|(charCode>>12); 
      puffer[puffer.length]=0x80|((charCode>>6)&0x3f); 
      puffer[puffer.length]=0x80|(charCode&0x3f); 
    }  
    else 
    { 
      puffer[puffer.length]=0xf0|(charCode>>18); 
      puffer[puffer.length]=0x80|((charCode>>12)&0x3f); 
      puffer[puffer.length]=0x80|((charCode>>6)&0x3f); 
      puffer[puffer.length]=0x80|(charCode&0x3f); 
    }  
    if(i==length) 
    { 
      while(puffer.length%3)  
      { 
        puffer[puffer.length]=NaN;
      } 
    } 
    if(puffer.length>2) 
    { 
      dig1 = puffer[0]>>2;
      dig2 = ((puffer[0]&3)<<4)|(puffer[1]>>4);
      dig3 = ((puffer[1]&15)<<2)|(puffer[2]>>6);
      dig4 = puffer[2]&63;

      if (isNaN(puffer[1])) {
        dig3 = dig4 = 64;
      } else if (isNaN(puffer[2])) {
        dig4 = 64;
      }

      puffer.shift();
      puffer.shift();
      puffer.shift();

      out+=base64EncodeChars.charAt(dig1); 
      out+=base64EncodeChars.charAt(dig2); 
      out+=base64EncodeChars.charAt(dig3); 
      out+=base64EncodeChars.charAt(dig4);
    } 
  } 
  return out; 
}
