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

static int flow;

//----------------------------------------------------------------------
static void TouchArg(op_t &x,int,int isload)
{
  switch ( x.type )
  {
    case o_reg:
    case o_void:
    case o_phrase:
      // TODO
      break;
    case o_imm:
      // TODO
      if ( isOff(uFlag, x.n) )
        ua_add_off_drefs2(x, dr_O, OOFS_IFSIGN|OOF_SIGNED|OOFW_32);
      break;
    case o_near:
      {
        cref_t ftype = fl_JN;
        ea_t ea = toEA(cmd.cs, x.addr);
        switch(cmd.itype)
        {
		case SPU_hbr:
		case SPU_hbrp:
		case SPU_hbra:
		case SPU_hbrr:
			ftype = fl_F;
			break;
        default:
	        if ( InstrIsSet(cmd.itype, CF_CALL) )
	        {
	          if ( !func_does_return(ea) )
	            flow = false;
	          ftype = fl_CN;
	        }
	        break;
	    }
        ua_add_cref(x.offb, ea, ftype);
      }
      case o_mem:
      {
        ea_t ea = toEA(dataSeg(), x.addr);
        ua_add_dref(x.offb, ea, isload ? dr_R : dr_W);
      }
      break;
    default:
      warning("%a: %s,%d: bad optype %d", cmd.ea, cmd.get_canon_mnem(), x.n, x.type);
      break;
  }
}

//----------------------------------------------------------------------
static bool may_be_skipped(void)
{
  return false;
}

//----------------------------------------------------------------------
int emu(void)
{
  uint32 Feature = cmd.get_canon_feature();
  int flag1 = is_forced_operand(cmd.ea, 0);
  int flag2 = is_forced_operand(cmd.ea, 1);
  int flag3 = is_forced_operand(cmd.ea, 2);
  int flag4 = is_forced_operand(cmd.ea, 3);

  flow = ((Feature & CF_STOP) == 0);

  if ( Feature & CF_USE1 ) TouchArg(cmd.Op1, flag1, 1);
  if ( Feature & CF_USE2 ) TouchArg(cmd.Op2, flag2, 1);
  if ( Feature & CF_USE3 ) TouchArg(cmd.Op3, flag3, 1);
  if ( Feature & CF_USE4 ) TouchArg(cmd.Op4, flag4, 1);

  if ( Feature & CF_CHG1 ) TouchArg(cmd.Op1, flag1, 0);
  if ( Feature & CF_CHG2 ) TouchArg(cmd.Op2, flag2, 0);
  if ( Feature & CF_CHG3 ) TouchArg(cmd.Op3, flag3, 0);
  if ( Feature & CF_CHG4 ) TouchArg(cmd.Op4, flag4, 0);

//
//      Determine if the next instruction should be executed
//
  if ( !flow ) flow = may_be_skipped();
  if ( segtype(cmd.ea) == SEG_XTRN ) flow = 0;
  if ( flow ) ua_add_cref(0,cmd.ea+cmd.size,fl_F);

  return 1;
}

//----------------------------------------------------------------------
int is_align_insn(ea_t ea)
{
  decode_insn(ea);
  switch ( cmd.itype ) {
    default:
      return 0;
    case SPU_nop:
    case SPU_lnop:
      break;
  }
  return cmd.size;
}
