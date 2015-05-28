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
function convertStringToBase64(a){var b='',charCode=0,i=0,length=a.length;var c=[];var d="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";var e,dig2,dig3,dig4;var i=0;while(i<a.length){charCode=a.charCodeAt(i++);if(charCode<0x80){c[c.length]=charCode}else if(charCode<0x800){c[c.length]=0xc0|(charCode>>6);c[c.length]=0x80|(charCode&0x3f)}else if(charCode<0x10000){c[c.length]=0xe0|(charCode>>12);c[c.length]=0x80|((charCode>>6)&0x3f);c[c.length]=0x80|(charCode&0x3f)}else{c[c.length]=0xf0|(charCode>>18);c[c.length]=0x80|((charCode>>12)&0x3f);c[c.length]=0x80|((charCode>>6)&0x3f);c[c.length]=0x80|(charCode&0x3f)}if(i==length){while(c.length%3){c[c.length]=NaN}}if(c.length>2){e=c[0]>>2;dig2=((c[0]&3)<<4)|(c[1]>>4);dig3=((c[1]&15)<<2)|(c[2]>>6);dig4=c[2]&63;if(isNaN(c[1])){dig3=dig4=64}else if(isNaN(c[2])){dig4=64}c.shift();c.shift();c.shift();b+=d.charAt(e);b+=d.charAt(dig2);b+=d.charAt(dig3);b+=d.charAt(dig4)}}return b}
