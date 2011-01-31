/*
 * IDA SPU Module - IBM Cell Synergistic Processor Unit (SPU)
 *
 * Copyright (c) 2011 by respective authors.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#ifndef _SPU_HPP
#define _SPU_HPP

#include "../idaidp.hpp"
#include "ins.hpp"

#define UAS_NOSPA        0x0001         // no space after comma

//------------------------------------------------------------------------
#define IAS_FRGPR        0x0001         // Friendly GPR registers. For ex. $LP, $SP

#define LSLR		 0x0003ffff

//------------------------------------------------------------------
enum RegNo
{
      // 128 general-purpose registers (GPRs)

     gpr0,     gpr1,     gpr2,     gpr3,     gpr4,     gpr5,     gpr6,     gpr7,
     gpr8,     gpr9,    gpr10,    gpr11,    gpr12,    gpr13,    gpr14,    gpr15,
    gpr16,    gpr17,    gpr18,    gpr19,    gpr20,    gpr21,    gpr22,    gpr23,
    gpr24,    gpr25,    gpr26,    gpr27,    gpr28,    gpr29,    gpr30,    gpr31,
    gpr32,    gpr33,    gpr34,    gpr35,    gpr36,    gpr37,    gpr38,    gpr39,
    gpr40,    gpr41,    gpr42,    gpr43,    gpr44,    gpr45,    gpr46,    gpr47,
    gpr48,    gpr49,    gpr50,    gpr51,    gpr52,    gpr53,    gpr54,    gpr55,
    gpr56,    gpr57,    gpr58,    gpr59,    gpr60,    gpr61,    gpr62,    gpr63,
    gpr64,    gpr65,    gpr66,    gpr67,    gpr68,    gpr69,    gpr70,    gpr71,
    gpr72,    gpr73,    gpr74,    gpr75,    gpr76,    gpr77,    gpr78,    gpr79,
    gpr80,    gpr81,    gpr82,    gpr83,    gpr84,    gpr85,    gpr86,    gpr87,
    gpr88,    gpr89,    gpr90,    gpr91,    gpr92,    gpr93,    gpr94,    gpr95,
    gpr96,    gpr97,    gpr98,    gpr99,   gpr100,   gpr101,   gpr102,   gpr103,
   gpr104,   gpr105,   gpr106,   gpr107,   gpr108,   gpr109,   gpr110,   gpr111,
   gpr112,   gpr113,   gpr114,   gpr115,   gpr116,   gpr117,   gpr118,   gpr119,
   gpr120,   gpr121,   gpr122,   gpr123,   gpr124,   gpr125,   gpr126,   gpr127,

   // 128 channels registers

   rch0,   rch1,   rch2,   rch3,   rch4,   rch5,   rch6,   rch7,
   rch8,   rch9,  rch10,  rch11,  rch12,  rch13,  rch14,  rch15,
  rch16,  rch17,  rch18,  rch19,  rch20,  rch21,  rch22,  rch23,
  rch24,  rch25,  rch26,  rch27,  rch28,  rch29,  rch30,  rch31,
  rch32,  rch33,  rch34,  rch35,  rch36,  rch37,  rch38,  rch39,
  rch40,  rch41,  rch42,  rch43,  rch44,  rch45,  rch46,  rch47,
  rch48,  rch49,  rch50,  rch51,  rch52,  rch53,  rch54,  rch55,
  rch56,  rch57,  rch58,  rch59,  rch60,  rch61,  rch62,  rch63,
  rch64,  rch65,  rch66,  rch67,  rch68,  rch69,  rch70,  rch71,
  rch72,  rch73,  rch74,  rch75,  rch76,  rch77,  rch78,  rch79,
  rch80,  rch81,  rch82,  rch83,  rch84,  rch85,  rch86,  rch87,
  rch88,  rch89,  rch90,  rch91,  rch92,  rch93,  rch94,  rch95,
  rch96,  rch97,  rch98,  rch99, rch100, rch101, rch102, rch103,
 rch104, rch105, rch106, rch107, rch108, rch109, rch110, rch111,
 rch112, rch113, rch114, rch115, rch116, rch117, rch118, rch119,
 rch120, rch121, rch122, rch123, rch124, rch125, rch126, rch127,

 // Save-and-Restore Register 0 holds the address used by the Interrupt Return instruction

 srr0,

 // 128 special-purpose registers (SPRs)
   spr0,   spr1,   spr2,   spr3,   spr4,   spr5,   spr6,   spr7,
   spr8,   spr9,  spr10,  spr11,  spr12,  spr13,  spr14,  spr15,
  spr16,  spr17,  spr18,  spr19,  spr20,  spr21,  spr22,  spr23,
  spr24,  spr25,  spr26,  spr27,  spr28,  spr29,  spr30,  spr31,
  spr32,  spr33,  spr34,  spr35,  spr36,  spr37,  spr38,  spr39,
  spr40,  spr41,  spr42,  spr43,  spr44,  spr45,  spr46,  spr47,
  spr48,  spr49,  spr50,  spr51,  spr52,  spr53,  spr54,  spr55,
  spr56,  spr57,  spr58,  spr59,  spr60,  spr61,  spr62,  spr63,
  spr64,  spr65,  spr66,  spr67,  spr68,  spr69,  spr70,  spr71,
  spr72,  spr73,  spr74,  spr75,  spr76,  spr77,  spr78,  spr79,
  spr80,  spr81,  spr82,  spr83,  spr84,  spr85,  spr86,  spr87,
  spr88,  spr89,  spr90,  spr91,  spr92,  spr93,  spr94,  spr95,
  spr96,  spr97,  spr98,  spr99, spr100, spr101, spr102, spr103,
 spr104, spr105, spr106, spr107, spr108, spr109, spr110, spr111,
 spr112, spr113, spr114, spr115, spr116, spr117, spr118, spr119,
 spr120, spr121, spr122, spr123, spr124, spr125, spr126, spr127,

 // virtual registers for code and data segments

 rVcs,
 rVds,

};

//------------------------------------------------------------------
// specific device name

extern char device[MAXSTR];

//------------------------------------------------------------------
// processor types

typedef uchar proctype_t;
const proctype_t SONY_PS3 = 0;
extern proctype_t ptype;    // contains processor type

extern netnode helper;
extern ushort idpflags;

//------------------------------------------------------------------
void header(void);
void footer(void);

void segstart(ea_t ea);
void segend(ea_t ea);
void assumes(ea_t ea);         // function to produce assume directives

void out(void);
int  outspec(ea_t ea,uchar segtype);

int  ana(void);
int  emu(void);
bool outop(op_t &op);
void data(ea_t ea);

int  is_align_insn(ea_t ea);

#endif // _SPU_HPP

