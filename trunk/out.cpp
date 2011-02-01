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

#include "spu.hpp"
#include <entry.hpp>
#include <diskio.hpp>

//------------------------------------------------------------------
// channel definitions

const ioport_t *find_channel(int channel);

//----------------------------------------------------------------------
inline void OutReg(int r)
{
  if ( r > rVds )
    warning("%a: outreg: illegal reg %d", cmd.ea, r);
  else
  {
     char *reg;
     static char* frgpr[] = { "$LR", "$SP" };

     if ((idpflags & IAS_FRGPR) && r >= gpr0 && r <= gpr1)
     {
       reg = frgpr[r];
     }
     else if (r >= rch0 && r <= rch30)
     {
       const ioport_t *channel = find_channel(r - rch0);
       if (channel)
       {
          reg = channel->name;
          set_cmt(cmd.ea, channel->cmt, false);
       }
       else
           reg = ph.regNames[r];
     }
     else
       reg = ph.regNames[r];

     out_register(reg);
  }
}

//----------------------------------------------------------------------
static void out_bad_address(ea_t addr)
{
  out_tagon(COLOR_ERROR);
  OutLong(addr, 16);
  out_tagoff(COLOR_ERROR);
  QueueMark(Q_noName, cmd.ea);
}

//----------------------------------------------------------------------
bool outop(op_t &x)
{
  switch ( x.type )
  {

    case o_void:
      return 0;

    case o_reg:
      OutReg(x.reg);
      break;

    case o_imm:
      OutValue(x, OOFS_IFSIGN|OOF_SIGNED|OOFW_32);
      break;

    case o_mem:
      {
        ea_t ea = toEA(dataSeg(), x.addr);
        if ( !out_name_expr(x, ea, x.addr) )
          out_bad_address(x.addr);
      }
      break;

    case o_near:
      {
        ea_t ea = toEA(cmd.cs, x.addr);
        if ( !out_name_expr(x, ea, x.addr) )
          out_bad_address(x.addr);
      }
      break;

    case o_phrase:
      {
        OutValue(x, OOFS_IFSIGN|OOF_SIGNED|OOFW_32);
        out_symbol('(');
        OutReg(x.phrase);
        out_symbol(')');
      }
      break;

    default:
      warning("out: %a: bad optype %d", cmd.ea, x.type);
      break;
  }
  return 1;
}

//----------------------------------------------------------------------
void out(void)
{
  char buf[MAXSTR];

   init_output_buffer(buf, sizeof(buf));       // setup the output pointer
   OutMnem();

   out_one_operand(0);                   // output the first operand

   if (cmd.Op2.type != o_void)
   {
     out_symbol(',');
     if(!(ash.uflag & UAS_NOSPA)) OutChar(' ');
     out_one_operand(1);
   }

   if (cmd.Op3.type != o_void)
   {
     out_symbol(',');
     if(!(ash.uflag & UAS_NOSPA)) OutChar(' ');
     out_one_operand(2);
   }

   if(cmd.Op4.type != o_void)
   {
     out_symbol(',');
     if(!(ash.uflag & UAS_NOSPA)) OutChar(' ');
     out_one_operand(3);
   }

   if(isVoid(cmd.ea, uFlag, 0)) OutImmChar(cmd.Op1);
   if(isVoid(cmd.ea, uFlag, 1)) OutImmChar(cmd.Op2);
   if(isVoid(cmd.ea, uFlag, 2)) OutImmChar(cmd.Op3);
   if(isVoid(cmd.ea, uFlag, 3)) OutImmChar(cmd.Op4);

   term_output_buffer();
   gl_comm = 1;
   MakeLine(buf);

}

//--------------------------------------------------------------------------
void segstart(ea_t ea)
{
  segment_t *Sarea = getseg(ea);
  if ( is_spec_segm(Sarea->type) ) return;
  char sname[MAXNAMELEN];
  char sclas[MAXNAMELEN];
  get_segm_name(Sarea, sname, sizeof(sname));
  get_segm_class(Sarea, sclas, sizeof(sclas));
  printf_line(0, COLSTR("%s", SCOLOR_ASMDIR)
                 " "
                 COLSTR("%s %s", SCOLOR_AUTOCMT),
                 strcmp(sclas,"CODE") == 0
                    ? ".CSEG"
                    : strcmp(sclas,"DATA") == 0
                         ? ".DSEG"
                         : ".ESEG",
                 ash.cmnt,
                 sname);
  if ( Sarea->orgbase != 0 )
  {
    char buf[MAX_NUMBUF];
    btoa(buf, sizeof(buf), Sarea->orgbase);
    printf_line(inf.indent, COLSTR("%s %s", SCOLOR_ASMDIR), ash.origin, buf);
  }
}

//--------------------------------------------------------------------------
void segend(ea_t)
{
}

//--------------------------------------------------------------------------
void header(void)
{
  gen_cmt_line("Processor       : %-8.8s [%s]", inf.procName, device);
  gen_cmt_line("Target assembler: %s", ash.name);
  gen_cmt_line("Byte sex        : %s", inf.mf ? "Big endian" : "Little endian");
  if ( ash.header != NULL )
    for ( const char **ptr=ash.header; *ptr != NULL; ptr++ )
      printf_line(0,COLSTR("%s",SCOLOR_ASMDIR),*ptr);
}

//--------------------------------------------------------------------------
void footer(void)
{
  char name[MAXSTR];
  get_name(BADADDR, inf.beginEA, name, sizeof(name));
  printf_line(inf.indent,COLSTR("%s",SCOLOR_ASMDIR)
                " "
                COLSTR("%s %s",SCOLOR_AUTOCMT), ash.end, ash.cmnt, name);
}

