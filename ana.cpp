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

//--------------------------------------------------------------------------
/*

00000000000 X                    stop
00000000001 X X X                lnop
00000000010 C X                  sync
00000000011 X X X                dsync
00000001100 X SA RT              mfspr    rt, sa
00000001101 X CA RT              rdch     rt, ca
00000001111 X CA RT              rchcnt   rt, ca
00000100 I10 RA RT               ori      rt, ra, value
00000101 I10 RA RT               orhi     rt, ra, value
00000110 I10 RA RT               orbi     rt, ra, value
00001000000 RB RA RT             sf       rt, ra, rb
00001000001 RB RA RT             or       rt, ra, rb
00001000010 RB RA RT             bg       rt, ra, rb
00001001000 RB RA RT             sfh      rt, ra, rb
00001001001 RB RA RT             nor      rt, ra, rb
00001010011 RB RA RT             absdb    rt, ra, rb
00001011000 RB RA RT             rot      rt, ra, rb
00001011001 RB RA RT             rotm     rt, ra, rb
00001011010 RB RA RT             rotma    rt, ra, rb
00001011011 RB RA RT             shl      rt, ra, rb
00001011100 RB RA RT             roth     rt, ra, rb
00001011101 RB RA RT             rothm    rt, ra, rb
00001011110 RB RA RT             rotmah   rt, ra, rb
00001011111 RB RA RT             shlh     rt, ra, rb
00001100 I10 RA RT               sfi      rt, ra, value
00001101 I10 RA RT               sfhi     rt, ra, value
00001111000 I7 RA RT             roti     rt, ra, value
00001111001 I7 RA RT             rotmi    rt, ra, value
00001111010 I7 RA RT             rotmai   rt, ra, value
00001111011 I7 RA RT             shli     rt, ra, value
00001111100 I7 RA RT             rothi    rt, ra, value
00001111101 I7 RA RT             rothmi   rt, ra, value
00001111110 I7 RA RT             rotmahi  rt, ra, value
00001111111 I7 RA RT             shlhi    rt, ra, value
0001000 R0H I16 R0L              hbra     brinst, brtarg
0001001 R0H I16 R0L              hbrr     brinst, brtarg
00010100 I10 RA RT               andi     rt, ra, value
00010101 I10 RA RT               andhi    rt, ra, value
00010110 I10 RA RT               andbi    rt, ra, value
00011000000 RB RA RT             a        rt, ra, rb
00011000001 RB RA RT             and      rt, ra, rb
00011000010 RB RA RT             cg       rt, ra, rb
00011001000 RB RA RT             ah       rt, ra, rb
00011001001 RB RA RT             nand     rt, ra, rb
00011010011 RB RA RT             avgb     rt, ra, rb
00011100 I10 RA RT               ai       rt, ra, value
00011101 I10 RA RT               ahi      rt, ra, value
001000000 I16 RT                 brz      rt, symbol
001000001 I16 RT                 stqa     rt, symbol
001000010 I16 RT                 brnz     rt, symbol
00100001100 X SA RT              mtspr    sa, rt
00100001101 X CA RT              wrch     ca, rt
001000100 I16 RT                 brhz     rt, symbol
001000110 I16 RT                 brhnz    rt, symbol
001000111 I16 RT                 stqr     rt, symbol
00100100 I10 RA RT               stqd     rt, symbol(ra)
00100101000 x D E x x x x RA RT  biz      rt, ra
00100101001 x D E x x x x RA RT  binz     rt, ra
00100101010 x D E x x x x RA RT  bihz     rt, ra
00100101011 x D E x x x x RA RT  bihnz    rt, ra
00101000000 RB RA RC             stopd    
00101000100 RB RA RT             stqx     rt, ra, rb
001100000 I16 X                  bra      symbol
001100001 I16 RT                 lqa      rt, symbol
001100010 I16 RT                 brasl    rt, symbol
001100100 I16 X                  br       symbol
001100101 I16 RT                 fsmbi    rt, symbol
001100110 I16 RT                 brsl     rt, symbol
001100111 I16 RT                 lqr      rt, symbol
00110100 I10 RA RT               lqd      rt, symbol(ra)
00110101000 x D E x x x x RA X   bi       ra
00110101001 x D E x x x x RA RT  bisl     rt, ra
00110101010 x D E x x x x RA X   iret     ra
00110101011 x D E x x x x RA RT  bisled   rt, ra
00110101100 P X ROH RA ROL       hbr      brinst, brtarg
00110110000 X RA RT              gb       rt, ra
00110110001 X RA RT              gbh      rt, ra
00110110010 X RA RT              gbb      rt, ra
00110110100 X RA RT              fsm      rt, ra
00110110101 X RA RT              fsmh     rt, ra
00110110110 X RA RT              fsmb     rt, ra
00110111000 X RA RT              frest    rt, ra
00110111001 X RA RT              frsqest  rt, ra
00111000100 RB RA RT             lqx      rt, ra, rb
00111001100 RB RA RT             rotqbybi rt, ra, rb
00111001101 RB RA RT             rotqmbyb rt, ra, rb
00111001111 RB RA RT             shlqbybi rt, ra, rb
00111010100 RB RA RT             cbx      rt, ra, rb
00111010101 RB RA RT             chx      rt, ra, rb
00111010110 RB RA RT             cwx      rt, ra, rb
00111010111 RB RA RT             cdx      rt, ra, rb
00111011000 RB RA RT             rotqbi   rt, ra, rb
00111011001 RB RA RT             rotqmbi  rt, ra, rb
00111011011 RB RA RT             shlqbi   rt, ra, rb
00111011100 RB RA RT             rotqby   rt, ra, rb
00111011101 RB RA RT             rotqmby  rt, ra, rb
00111011111 RB RA RT             shlqby   rt, ra, rb
00111110000 X RA RT              orx      rt, ra
00111110100 I7 RA RT             cbd      rt, symbol(ra)
00111110101 I7 RA RT             chd      rt, symbol(ra)
00111110110 I7 RA RT             cwd      rt, symbol(ra)
00111110111 I7 RA RT             cdd      rt, symbol(ra)
00111111000 I7 RA RT             rotqbii  rt, ra, value
00111111001 I7 RA RT             rotqmbii rt, ra, value
00111111011 I7 RA RT             shlqbii  rt, ra, value
00111111100 I7 RA RT             rotqbyi  rt, ra, value
00111111101 I7 RA RT             rotqmbyi rt, ra, value
00111111111 I7 RA RT             shlqbyi  rt, ra, value
01000000001 X X RT               nop      
010000001 I16 RT                 il       rt, symbol
010000010 I16 RT                 ilhu     rt, symbol
010000011 I16 RT                 ilh      rt, symbol
0100001 I18 RT                   ila      rt, symbol
01000100 I10 RA RT               xori     rt, ra, value
01000101 I10 RA RT               xorhi    rt, ra, value
01000110 I10 RA RT               xorbi    rt, ra, value
01001000000 RB RA RT             cgt      rt, ra, rb
01001000001 RB RA RT             xor      rt, ra, rb
01001001000 RB RA RT             cgth     rt, ra, rb
01001001001 RB RA RT             eqv      rt, ra, rb
01001010000 RB RA RT             cgtb     rt, ra, rb
01001010011 RB RA RT             sumb     rt, ra, rb
01001011000 RB RA RT             hgt      ra, rb
01001100 I10 RA RT               cgti     rt, ra, value
01001101 I10 RA RT               cgthi    rt, ra, value
01001110 I10 RA RT               cgtbi    rt, ra, rb
01001111 I10 RA RT               hgti     ra, symbol
01010100101 X RA RT              clz      rt, ra
01010100110 X RA RT              xswd     rt, ra
01010101110 X RA RT              xshw     rt, ra
01010110100 X RA RT              cntb     rt, ra
01010110110 X RA RT              xsbh     rt, ra
01011000000 RB RA RT             clgt     rt, ra, rb
01011000001 RB RA RT             andc     rt, ra, rb
01011000010 RB RA RT             fcgt     rt, ra, rb
01011000011 RB RA RT             dfcgt    rt, ra, rb
01011000100 RB RA RT             fa       rt, ra, rb
01011000101 RB RA RT             fs       rt, ra, rb
01011000110 RB RA RT             fm       rt, ra, rb
01011001000 RB RA RT             clgth    rt, ra, rb
01011001001 RB RA RT             orc      rt, ra, rb
01011001010 RB RA RT             fcmgt    rt, ra, rb
01011001011 RB RA RT             dfcmgt   rt, ra, rb
01011001100 RB RA RT             dfa      rt, ra, rb
01011001101 RB RA RT             dfs      rt, ra, rb
01011001110 RB RA RT             dfm      rt, ra, rb
01011010000 RB RA RT             clgtb    rt, ra, rb
01011011000 RB RA RT             hlgt     ra, rb
01011100 I10 RA RT               clgti    rt, ra, value
01011101 I10 RA RT               clgthi   rt, ra, value
01011110 I10 RA RT               clgtbi   rt, ra, value
01011111 I10 RA RT               hlgti    ra, symbol
011000001 I16 RT                 iohl     rt, symbol
01101000000 RB RA RT             addx     rt, ra, rb
01101000001 RB RA RT             sfx      rt, ra, rb
01101000010 RB RA RT             cgx      rt, ra, rb
01101000011 RB RA RT             bgx      rt, ra, rb
01101000110 RB RA RT             mpyhha   rt, ra, rb
01101001110 RB RA RT             mpyhhau  rt, ra, rb
01101011100 RB RA RT             dfma     rt, ra, rb
01101011101 RB RA RT             dfms     rt, ra, rb
01101011110 RB RA RT             dfnms    rt, ra, rb
01101011111 RB RA RT             dfnma    rt, ra, rb
01110011000 X X RT               fscrrd   rt
01110100 I10 RA RT               mpyi     rt, ra, value
01110101 I10 RA RT               mpyui    rt, ra, value
0111011000 I8 RA RT              cflts    rt, ra, scale
0111011001 I8 RA RT              cfltu    rt, ra, scale
0111011010 I8 RA RT              csflt    rt, ra, scale
0111011011 I8 RA RT              cuflt    rt, ra, scale
01110111000 X RA RT              fesd     rt, ra
01110111001 X RA RT              frds     rt, ra
01110111010 X RA RT              fscrwr   ra
01110111111 I7 RA RT             dftsv    rt, ra, value
01111000000 RB RA RT             ceq      rt, ra, rb
01111000010 RB RA RT             fceq     rt, ra, rb
01111000011 RB RA RT             dfceq    rt, ra, rb
01111000100 RB RA RT             mpy      rt, ra, rb
01111000101 RB RA RT             mpyh     rt, ra, rb
01111000110 RB RA RT             mpyhh    rt, ra, rb
01111000111 RB RA RT             mpys     rt, ra, rb
01111001000 RB RA RT             ceqh     rt, ra, rb
01111001010 RB RA RT             fcmeq    rt, ra, rb
01111001011 RB RA RT             dfcmeq   rt, ra, rb
01111001100 RB RA RT             mpyu     rt, ra, rb
01111001110 RB RA RT             mpyhhu   rt, ra, rb
01111010000 RB RA RT             ceqb     rt, ra, rb
01111010100 RB RA RT             fi       rt, ra, rb
01111011000 RB RA RT             heq      ra, rb
01111100 I10 RA RT               ceqi     rt, ra, value
01111101 I10 RA RT               ceqhi    rt, ra, value
01111110 I10 RA RT               ceqbi    rt, ra, value
01111111 I10 RA RT               heqi     ra, symbol
1000 RT RB RA RC                 selb     rt, ra, rb, rc
1011 RT RB RA RC                 shufb    rt, ra, rb, rc
1100 RT RB RA RC                 mpya     rt, ra, rb, rc
1101 RT RB RA RC                 fnms     rt, ra, rb, rc
1110 RT RB RA RC                 fma      rt, ra, rb, rc
1111 RT RB RA RC                 fms      rt, ra, rb, rc
*/

//--------------------------------------------------------------------------
inline void opreg(op_t &x, uint16 n)
{
	x.type = o_reg;
	x.dtyp = dt_byte16;
	x.reg  = n;
}

//--------------------------------------------------------------------------
inline void opimm(op_t &x, int32 value)
{
	x.type   = o_imm;
	x.dtyp   = dt_dword;
	x.value  = value;
}

//--------------------------------------------------------------------------
inline void opnear(op_t &x, ea_t addr)
{
	x.type   = o_near;
	x.dtyp   = dt_code;
	x.addr   = addr;
}

//--------------------------------------------------------------------------
inline void opphr(op_t &x, int32 value, int16 phrase)
{
	x.type   = o_phrase;
	x.value  = value;
        x.phrase = phrase;
	x.dtyp   = dt_dword;
}

//--------------------------------------------------------------------------
inline void opreg_sa(op_t &x, uint32 code)
{
    opreg(x, spr0 + ((code >> 7) & 0x7f));
}

//--------------------------------------------------------------------------
inline void opreg_rt(op_t &x, uint32 code)
{
    opreg(x, gpr0 + (code & 0x7f));
}

//--------------------------------------------------------------------------
inline void opreg_ca(op_t &x, uint32 code)
{
    opreg(x, rch0 + ((code >> 7) & 0x7f));
}

//--------------------------------------------------------------------------
inline void opreg_ra(op_t &x, uint32 code)
{
    opreg(x, gpr0 + ((code >> 7) & 0x7f));
}

//--------------------------------------------------------------------------
inline void opreg_rb(op_t &x, uint32 code)
{
    opreg(x, gpr0 + ((code >> 14) & 0x7f));
}

//--------------------------------------------------------------------------
inline void opreg_rc(op_t &x, uint32 code)
{
    opreg(x, gpr0 + ((code >> 21) & 0x7f));
}

//--------------------------------------------------------------------------
inline void opreg_rt_ra_rb_rc(int16 itype, uint32 code)
{
    cmd.itype = itype;
    opreg(cmd.Op1, gpr0 + ((code >> 21) & 0x7f));
    opreg_ra(cmd.Op2, code);
    opreg_rb(cmd.Op3, code);
    opreg(cmd.Op4, gpr0 + (code & 0x7f));
}

//--------------------------------------------------------------------------
inline void opnear_roh_rol_repl_left_bit(op_t &x, uint32 code)
{
    int32 addr = (code << 2) & 0x3fc;
    if ((code >> 8) & 1)
    {
          addr |= 0xfc00;
    }
    addr += cmd.ea;
    addr &= lslr_size & 0xfffffffc;
    opnear(x, addr);
}

//--------------------------------------------------------------------------
inline void opimm_i7_repl_left_bit(op_t &x, uint32 code)
{
    int8 imm = ((code >> 14) & 0x3f);
    if ((code >> 20) & 1)
    {
          imm |= 3 << 6;
    }
    opimm(x, imm);
}

//--------------------------------------------------------------------------
inline void opimm_i7(op_t &x, uint32 code)
{
    opimm(x, ((code >> 14) & 0x7f));
}

//--------------------------------------------------------------------------
inline void opphr_i7_symbol_ra(op_t &x, uint32 code)
{
    int8 symbol = ((code >> 14) & 0x3f);
    if ((code >> 20) & 1)
    {
          symbol |= 3 << 6;
    }
    int8 ra = gpr0 + ((code >> 7) & 0x7f);
    opphr(x, symbol, ra);
}

//--------------------------------------------------------------------------
inline void opphr_i10_symbol_ra(op_t &x, uint32 code)
{
    int16 symbol = (code >> 10) & 0x1ff0;
    if ((code >> 23) & 1)
    {
        symbol |= 0xe000;
    }
    int8 ra = gpr0 + ((code >> 7) & 0x7f);
    opphr(x, symbol, ra);
}

//--------------------------------------------------------------------------
inline void opimm_i10_repl_left_bit(op_t &x, uint32 code)
{
    int16 value = (code >> 14) & 0x1ff;
    if ((code >> 23) & 1)
    {
        value |= 0xfe00;
    }
    opimm(x, value);
}

//--------------------------------------------------------------------------
inline void opimm_i10(op_t &x, uint32 code)
{
    opimm(x, (code >> 14) & 0x3ff);
}

//--------------------------------------------------------------------------
inline void opnear_i16(op_t &x, uint32 code, uint32 ea)
{
    int32 addr = (code >> 5) & 0x1fffc;
    if (((code >> 22) & 1))
    {
        addr |= 0x7fff << 17;
    }
    addr += ea;
    addr &= lslr_size & 0xfffffffc;
    opnear(x, ea_t(addr));
}

//--------------------------------------------------------------------------
inline void opimm_i16(op_t &x, uint32 code)
{
    opimm(x, (code >> 7) & 0xffff);
}

//--------------------------------------------------------------------------
inline void opnear_i18(op_t &x, uint32 code)
{
    opnear(x, ea_t((code >> 7) & 0x3ffff));
}

//--------------------------------------------------------------------------
int ana(void)
{
	uint32 code = get_full_long(cmd.ea);
	cmd.size = 4;

	switch ( code >> 28 )
	{
	case 0:
		// 0000 0000 000 X                    stop
		// 0000 0000 001 X X X                lnop
		// 0000 0000 010 C X                  sync
		// 0000 0000 011 X X X                dsync
		// 0000 0001 100 X SA RT              mfspr    rt, sa
		// 0000 0001 101 X CA RT              rdch     rt, ca
		// 0000 0001 111 X CA RT              rchcnt   rt, ca
		// 0000 0100     I10 RA RT            ori      rt, ra, value
		// 0000 0101     I10 RA RT            orhi     rt, ra, value
		// 0000 0110     I10 RA RT            orbi     rt, ra, value
		// 0000 1000 000 RB RA RT             sf       rt, ra, rb
		// 0000 1000 001 RB RA RT             or       rt, ra, rb
		// 0000 1000 010 RB RA RT             bg       rt, ra, rb
		// 0000 1001 000 RB RA RT             sfh      rt, ra, rb
		// 0000 1001 001 RB RA RT             nor      rt, ra, rb
		// 0000 1010 011 RB RA RT             absdb    rt, ra, rb
		// 0000 1011 000 RB RA RT             rot      rt, ra, rb
		// 0000 1011 001 RB RA RT             rotm     rt, ra, rb
		// 0000 1011 010 RB RA RT             rotma    rt, ra, rb
		// 0000 1011 011 RB RA RT             shl      rt, ra, rb
		// 0000 1011 100 RB RA RT             roth     rt, ra, rb
		// 0000 1011 101 RB RA RT             rothm    rt, ra, rb
		// 0000 1011 110 RB RA RT             rotmah   rt, ra, rb
		// 0000 1011 111 RB RA RT             shlh     rt, ra, rb
		// 0000 1100     I10 RA RT            sfi      rt, ra, value
		// 0000 1101     I10 RA RT            sfhi     rt, ra, value
		// 0000 1111 000 I7 RA RT             roti     rt, ra, value
		// 0000 1111 001 I7 RA RT             rotmi    rt, ra, value
		// 0000 1111 010 I7 RA RT             rotmai   rt, ra, value
		// 0000 1111 011 I7 RA RT             shli     rt, ra, value
		// 0000 1111 100 I7 RA RT             rothi    rt, ra, value
		// 0000 1111 101 I7 RA RT             rothmi   rt, ra, value
		// 0000 1111 110 I7 RA RT             rotmahi  rt, ra, value
		// 0000 1111 111 I7 RA RT             shlhi    rt, ra, value
		{
			switch ( (code >> 24) & 15 )
			{
			case 0:
				// 0000 0000 000 X                    stop
				// 0000 0000 001 X X X                lnop
				// 0000 0000 010 C X                  sync
				// 0000 0000 011 X X X                dsync
				{
					int idx = ((code >> 21) & 7);
					static const uchar itypes[] =
					{ SPU_stop, SPU_lnop, SPU_sync, SPU_dsync, 0, 0, 0, 0 };
					cmd.itype = itypes[idx];
				}
				cmd.Op1.type = cmd.Op2.type = cmd.Op3.type = cmd.Op3.type = cmd.Op4.type = o_void;
				break;
			case 1:
				switch ( (code >> 21) & 7 )
				{
				case 4:
					// 0000 0001 100 X SA RT              mfspr    rt, sa
                                        cmd.itype = SPU_mfspr;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_sa(cmd.Op2, code);
					break;
				case 5:
					// 0000 0001 101 X CA RT              rdch     rt, ca
					cmd.itype = SPU_rdch;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ca(cmd.Op2, code);
					break;
				case 7:
					// 0000 0001 111 X CA RT              rchcnt   rt, ca
					cmd.itype = SPU_rchcnt;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ca(cmd.Op2, code);
					break;
				default:
					return 0;
				}          	  
				break;
			case 4:
				// 0000 0100     I10 RA RT            ori      rt, ra, value
                                cmd.itype = SPU_ori;
                                opreg_rt(cmd.Op1, code);
                                opreg_ra(cmd.Op2, code);
                                opimm_i10_repl_left_bit(cmd.Op3, code);
				break;
			case 5:
				// 0000 0101     I10 RA RT            orhi     rt, ra, value
                                cmd.itype = SPU_orhi;
                                opreg_rt(cmd.Op1, code);
                                opreg_ra(cmd.Op2, code);
                                opimm_i10_repl_left_bit(cmd.Op3, code);
				break;
			case 6:
				// 0000 0110     I10 RA RT            orbi     rt, ra, value
                                cmd.itype = SPU_orbi;
                                opreg_rt(cmd.Op1, code);
                                opreg_ra(cmd.Op2, code);
                                opimm_i10(cmd.Op3, code);
				break;

			case 8:
				// 0000 1000 000 RB RA RT             sf       rt, ra, rb
				// 0000 1000 001 RB RA RT             or       rt, ra, rb
				// 0000 1000 010 RB RA RT             bg       rt, ra, rb
				{
					int idx = ((code >> 21) & 7);
					static const uchar itypes[] =
					{ SPU_sf, SPU_or, SPU_bg, 0, 0, 0, 0, 0 };
					if (itypes[idx] == 0)
						return 0;
                                        cmd.itype = itypes[idx];
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opreg_rb(cmd.Op3, code);
				}
				break;
			case 9:
				// 0000 1001 000 RB RA RT             sfh      rt, ra, rb
				// 0000 1001 001 RB RA RT             nor      rt, ra, rb
				{
					int idx = ((code >> 21) & 7);
					static const uchar itypes[] =
					{ SPU_sfh, SPU_nor, 0, 0, 0, 0, 0, 0 };
					if (itypes[idx] == 0)
						return 0;
                                        cmd.itype = itypes[idx];
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opreg_rb(cmd.Op3, code);
				}
				break;
			case 10:
				// 0000 1010 011 RB RA RT             absdb    rt, ra, rb
				{
					int idx = ((code >> 21) & 7);
					static const uchar itypes[] =
					{ 0, 0, 0, SPU_absdb, 0, 0, 0, 0 };
					if (itypes[idx] == 0)
						return 0;
                                        cmd.itype = itypes[idx];
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opreg_rb(cmd.Op3, code);
				}
				break;
			case 11:
				// 0000 1011 000 RB RA RT             rot      rt, ra, rb
				// 0000 1011 001 RB RA RT             rotm     rt, ra, rb
				// 0000 1011 010 RB RA RT             rotma    rt, ra, rb
				// 0000 1011 011 RB RA RT             shl      rt, ra, rb
				// 0000 1011 100 RB RA RT             roth     rt, ra, rb
				// 0000 1011 101 RB RA RT             rothm    rt, ra, rb
				// 0000 1011 110 RB RA RT             rotmah   rt, ra, rb
				// 0000 1011 111 RB RA RT             shlh     rt, ra, rb
				{
					int idx = ((code >> 21) & 7);
					static const uchar itypes[] =
					{ SPU_rot,  SPU_rotm,  SPU_rotma,  SPU_shl
					, SPU_roth, SPU_rothm, SPU_rotmah, SPU_shlh };
					if (itypes[idx] == 0)
						return 0;
                                        cmd.itype = itypes[idx];
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opreg_rb(cmd.Op3, code);
				}
				break;
			case 12:
				// 0000 1100     I10 RA RT            sfi      rt, ra, value
                                cmd.itype = SPU_sfi;
                                opreg_rt(cmd.Op1, code);
                                opreg_ra(cmd.Op2, code);
                                opimm_i10_repl_left_bit(cmd.Op3, code);
				break;
			case 13:
				// 0000 1101     I10 RA RT            sfhi     rt, ra, value
                                cmd.itype = SPU_sfhi;
                                opreg_rt(cmd.Op1, code);
                                opreg_ra(cmd.Op2, code);
                                opimm_i10_repl_left_bit(cmd.Op3, code);
				break;
			case 15:
				// 0000 1111 000 I7 RA RT             roti     rt, ra, value
				// 0000 1111 001 I7 RA RT             rotmi    rt, ra, value
				// 0000 1111 010 I7 RA RT             rotmai   rt, ra, value
				// 0000 1111 011 I7 RA RT             shli     rt, ra, value
				// 0000 1111 100 I7 RA RT             rothi    rt, ra, value
				// 0000 1111 101 I7 RA RT             rothmi   rt, ra, value
				// 0000 1111 110 I7 RA RT             rotmahi  rt, ra, value
				// 0000 1111 111 I7 RA RT             shlhi    rt, ra, value
				{
					int idx = ((code >> 21) & 7);
					static const uchar itypes[] =
					{ SPU_roti,  SPU_rotmi,  SPU_rotmai,  SPU_shli
					, SPU_rothi, SPU_rothmi, SPU_rotmahi, SPU_shlhi };
					if (itypes[idx] == 0)
						return 0;
                                        cmd.itype = itypes[idx];
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i7_repl_left_bit(cmd.Op3, code);
				}
				break;
			default:
				return 0;
			}
		}
		break;
	case 1:
		// 0001 000      R0H I16 R0L         hbra    brinst, brtarg
		// 0001 001      R0H I16 R0L         hbrr    brinst, brtarg
		// 0001 0100     I10 RA RT           andi    rt, ra, value
		// 0001 0101     I10 RA RT           andhi   rt, ra, value
		// 0001 0110     I10 RA RT           andbi   rt, ra, value
		// 0001 1000 000 RB RA RT            a       rt, ra, rb
		// 0001 1000 001 RB RA RT            and     rt, ra, rb
		// 0001 1000 010 RB RA RT            cg      rt, ra, rb
		// 0001 1001 000 RB RA RT            ah      rt, ra, rb
		// 0001 1001 001 RB RA RT            nand    rt, ra, rb
		// 0001 1010 011 RB RA RT            avgb    rt, ra, rb
		// 0001 1100     I10 RA RT           ai      rt, ra, value
		// 0001 1101     I10 RA RT           ahi     rt, ra, value
		{
			int idx = (code >> 25) & 7;
			if (idx == 0)
			{
				// 0001 000      R0H I16 R0L         hbra    brinst, brtarg
				cmd.itype = SPU_hbra;
                                opnear_roh_rol_repl_left_bit(cmd.Op1, (int16) (((code >> 16) & 0x180) | (code & 0x7f)));
                                opnear_i16(cmd.Op2, code, 0);
			}
			else if (idx == 1)
			{
				// 0001 001      R0H I16 R0L         hbrr    brinst, brtarg
				cmd.itype = SPU_hbrr;
                                opnear_roh_rol_repl_left_bit(cmd.Op1, (int16) (((code >> 16) & 0x180) | (code & 0x7f)));
                                opnear_i16(cmd.Op2, code, cmd.ea);
			}
			else
			{
				 idx = (code >> 24) & 15;
				 switch (idx)
				 {
				 case 4:
					// 0001 0100     I10 RA RT           andi    rt, ra, value
                                        cmd.itype = SPU_andi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
				  break;
				 case 5:
					// 0001 0101     I10 RA RT           andhi   rt, ra, value
                                        cmd.itype = SPU_andhi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				 case 6:
				  // 0001 0110     I10 RA RT           andbi   rt, ra, value
                                        cmd.itype = SPU_andbi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10(cmd.Op3, code);
				  break;
				 case 8:
					// 0001 1000 000 RB RA RT            a       rt, ra, rb
					// 0001 1000 001 RB RA RT            and     rt, ra, rb
					// 0001 1000 010 RB RA RT            cg      rt, ra, rb
					{
						int idx = ((code >> 21) & 3);
						static const uchar itypes[] =
						{ SPU_a, SPU_and, SPU_cg, 0 };
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
				  break;
				 case 9:
					// 0001 1001 000 RB RA RT            ah      rt, ra, rb
					// 0001 1001 001 RB RA RT            nand    rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ SPU_ah, SPU_nand, 0, 0, 0, 0, 0, 0 };
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
				 	break;
				 case 10:
				  // 0001 1010 011 RB RA RT            avgb    rt, ra, rb
					if (((code >> 21) & 7) != 3)
						return 0;
                                        cmd.itype = SPU_avgb;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opreg_rb(cmd.Op3, code);
				  break;
				 case 12:
					// 0001 1100     I10 RA RT           ai      rt, ra, value
                                        cmd.itype = SPU_ai;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
				 	break;
				 case 13:
					// 0001 1101     I10 RA RT           ahi     rt, ra, value
                                        cmd.itype = SPU_ahi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				 default:
				 	 	return 0;
				 }
			}
		}
		break;
	case 2:
		// 0010 0000 0   I16 RT               brz     rt, symbol
		// 0010 0000 1   I16 RT               stqa    rt, symbol
		// 0010 0001 0   I16 RT               brnz    rt, symbol
		// 0010 0001 100 XXX SA RT            mtspr   sa, rt
		// 0010 0001 101 XXX CA RT            wrch    ca, rt
		// 0010 0010 0   I16 RT               brhz    rt, symbol
		// 0010 0011 0   I16 RT               brhnz   rt, symbol
		// 0010 0011 1   I16 RT               stqr    rt, symbol
		// 0010 0100     I10 RA RT            stqd    rt, symbol(ra)
		// 0010 0101 000 x D E x x x x RA RT  biz     rt, ra
		// 0010 0101 001 x D E x x x x RA RT  binz    rt, ra
		// 0010 0101 010 x D E x x x x RA RT  bihz    rt, ra
		// 0010 0101 011 x D E x x x x RA RT  bihnz   rt, ra
		// 0010 1000 000 RB RA RC             stopd
		// 0010 1000 100 RB RA RT             stqx    rt, ra, rb
		{
			int idx = (code >> 24) & 15;
			switch ( idx )
			{
			case 0:
				if (((code >> 23) & 1) == 0)
				{
					// 0010 0000 0   I16 RT               brz     rt, symbol
                                        cmd.itype = SPU_brz;
                                        opreg_rt(cmd.Op1, code);                                        
                                        opnear_i16(cmd.Op2, code, cmd.ea);
				}
				else
				{
					// 0010 0000 1   I16 RT               stqa    rt, symbol
                                        cmd.itype = SPU_stqa;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i16(cmd.Op2, code, 0);
				}
				break;
			case 1:
				switch ((code >> 21) & 7)
				{
					case 0:
					case 1:
					case 2:
					case 3:
						// 0010 0001 0   I16 RT               brnz    rt, symbol
                                                cmd.itype = SPU_brnz;
                                                opreg_rt(cmd.Op1, code);
                                                opnear_i16(cmd.Op2, code, cmd.ea);
						break;
					case 4:
						// 0010 0001 100 XXX SA RT            mtspr   sa, rt
                                                cmd.itype = SPU_mtspr;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_sa(cmd.Op2, code);
						break;
					case 5:
						// 0010 0001 101 XXX CA RT            wrch    ca, rt
                                                cmd.itype = SPU_wrch;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ca(cmd.Op2, code);
						break;
					default:
						return 0;
				}
				break;
			case 2:
				// 0010 0010 0   I16 RT               brhz    rt, symbol
                                cmd.itype = SPU_brhz;
                                opreg_rt(cmd.Op1, code);
                                opnear_i16(cmd.Op2, code, cmd.ea);
				break;
			case 3:
				if (((code >> 23) & 1) == 0)
				{
					// 0010 0011 0   I16 RT               brhnz   rt, symbol
                                        cmd.itype = SPU_brhnz;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i16(cmd.Op2, code, cmd.ea);
				}
				else
				{
					// 0010 0011 1   I16 RT               stqr    rt, symbol
                                        cmd.itype = SPU_stqr;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i16(cmd.Op2, code, cmd.ea);
				}
				break;
			case 4:
				// 0010 0100     I10 RA RT            stqd    rt, symbol(ra)
                                cmd.itype = SPU_stqd;
                                opreg_rt(cmd.Op1, code);
                                opphr_i10_symbol_ra(cmd.Op2, code);
				break;
			case 5:
				// 0010 0101 000 x D E x x x x RA RT  biz     rt, ra
				// 0010 0101 001 x D E x x x x RA RT  binz    rt, ra
				// 0010 0101 010 x D E x x x x RA RT  bihz    rt, ra
				// 0010 0101 011 x D E x x x x RA RT  bihnz   rt, ra
				{
					int idx = ((code >> 21) & 3);
					static const uchar itypes[] =
					{ SPU_biz, SPU_binz, SPU_bihz, SPU_bihnz};
					if (itypes[idx] == 0)
						return 0;
                                        cmd.itype = itypes[idx];
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
			 	}
				break;
			case 8:
				int idx = (code >> 21) & 7;
				if (idx == 0)
				{
					// 0010 1000 000 RB RA RC             stopd
                                        cmd.itype = SPU_stopd;
				} 
				else if (idx == 4)
				{
					// 0010 1000 100 RB RA RT             stqx    rt, ra, rb
                                        cmd.itype = SPU_stqx;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opreg_rb(cmd.Op3, code);
				}
				else
				{
					return 0;
				}
				break;
		  }
		}
                break;
	case 3:
		// 0011 0000 0   I16 X                bra     symbol
		// 0011 0000 1   I16 RT               lqa     rt, symbol
		// 0011 0001 0   I16 RT               brasl   rt, symbol
		// 0011 0010 0   I16 X                br      symbol
		// 0011 0010 1   I16 RT               fsmbi   rt, symbol
		// 0011 0011 0   I16 RT               brsl    rt, symbol
		// 0011 0011 1   I16 RT               lqr     rt, symbol
		// 0011 0100     I10 RA RT            lqd     rt, symbol(ra)
		// 0011 0101 000 x D E x x x x RA X   bi      ra
		// 0011 0101 001 x D E x x x x RA RT  bisl    rt, ra
		// 0011 0101 010 x D E x x x x RA X   iret    ra
		// 0011 0101 011 x D E x x x x RA RT  bisled  rt, ra
		// 0011 0101 100 P X ROH RA ROL       hbr     brinst, brtarg
		// 0011 0110 000 X RA RT              gb      rt, ra
		// 0011 0110 001 X RA RT              gbh     rt, ra
		// 0011 0110 010 X RA RT              gbb     rt, ra
		// 0011 0110 100 X RA RT              fsm     rt, ra
		// 0011 0110 101 X RA RT              fsmh    rt, ra
		// 0011 0110 110 X RA RT              fsmb    rt, ra
		// 0011 0111 000 X RA RT              frest   rt, ra
		// 0011 0111 001 X RA RT              frsqest rt, ra
		// 0011 1000 100 RB RA RT             lqx     rt, ra, rb
		// 0011 1001 100 RB RA RT             rotqbybirt, ra, rb
		// 0011 1001 101 RB RA RT             rotqmbybrt, ra, rb
		// 0011 1001 111 RB RA RT             shlqbybirt, ra, rb
		// 0011 1010 100 RB RA RT             cbx     rt, ra, rb
		// 0011 1010 101 RB RA RT             chx     rt, ra, rb
		// 0011 1010 110 RB RA RT             cwx     rt, ra, rb
		// 0011 1010 111 RB RA RT             cdx     rt, ra, rb
		// 0011 1011 000 RB RA RT             rotqbi  rt, ra, rb
		// 0011 1011 001 RB RA RT             rotqmbi rt, ra, rb
		// 0011 1011 011 RB RA RT             shlqbi  rt, ra, rb
		// 0011 1011 100 RB RA RT             rotqby  rt, ra, rb
		// 0011 1011 101 RB RA RT             rotqmby rt, ra, rb
		// 0011 1011 111 RB RA RT             shlqby  rt, ra, rb
		// 0011 1110 000 X RA RT              orx     rt, ra
		// 0011 1110 100 I7 RA RT             cbd     rt, symbol(ra)
		// 0011 1110 101 I7 RA RT             chd     rt, symbol(ra)
		// 0011 1110 110 I7 RA RT             cwd     rt, symbol(ra)
		// 0011 1110 111 I7 RA RT             cdd     rt, symbol(ra)
		// 0011 1111 000 I7 RA RT             rotqbii rt, ra, value
		// 0011 1111 001 I7 RA RT             rotqmbii rt, ra, value
		// 0011 1111 011 I7 RA RT             shlqbii  rt, ra, value
		// 0011 1111 100 I7 RA RT             rotqbyi rt, ra, value
		// 0011 1111 101 I7 RA RT             rotqmbyirt, ra, value
		// 0011 1111 111 I7 RA RT             shlqbyi rt, ra, value
		{
			int idx = (code >> 23) & 0x1f;
			switch ( idx )
			{
				case 0:
					// 0011 0000 0   I16 X                bra     symbol
                                        cmd.itype = SPU_bra;
                                        opnear_i16(cmd.Op1, code, 0);
					break;
				case 1:
					// 0011 0000 1   I16 RT               lqa     rt, symbol
                                        cmd.itype = SPU_lqa;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i16(cmd.Op2, code, 0);
					break;
				case 2:
					// 0011 0001 0   I16 RT               brasl   rt, symbol
                                        cmd.itype = SPU_brasl;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i16(cmd.Op2, code, 0);
					break;
				case 4:
					// 0011 0010 0   I16 X                br      symbol
                                        cmd.itype = SPU_br;
                                        opnear_i16(cmd.Op1, code, cmd.ea);
					break;
				case 5:
					// 0011 0010 1   I16 RT               fsmbi   rt, symbol
                                        cmd.itype = SPU_fsmbi;
                                        opreg_rt(cmd.Op1, code);
                                        opimm_i16(cmd.Op2, code);
					break;
				case 6:
					// 0011 0011 0   I16 RT               brsl    rt, symbol
                                        cmd.itype = SPU_brsl;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i16(cmd.Op2, code, cmd.ea);
					break;
				case 7:
					// 0011 0011 1   I16 RT               lqr     rt, symbol
                                        cmd.itype = SPU_lqr;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i16(cmd.Op2, code, cmd.ea);
					break;
				case 8:
				case 9:
					// 0011 0100     I10 RA RT            lqd     rt, symbol(ra)
                                        cmd.itype = SPU_lqd;
                                        opreg_rt(cmd.Op1, code);
                                        opphr_i10_symbol_ra(cmd.Op2, code);
					break;
				default:
					{
						int idx = (code >> 21) & 127;
						switch( idx )
						{
							case 40:
								// 0011 0101 000 x D E x x x x RA X   bi      ra
                                                                cmd.itype = SPU_bi;
                                                                opreg_ra(cmd.Op1, code);
								break;
							case 41:
								// 0011 0101 001 x D E x x x x RA RT  bisl    rt, ra
                                                                cmd.itype = SPU_bisl;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 42:
								// 0011 0101 010 x D E x x x x RA X   iret    ra
                                                                cmd.itype = SPU_iret;
                                                                opreg_ra(cmd.Op1, code);
								break;
							case 43:
								// 0011 0101 011 x D E x x x x RA RT  bisled  rt, ra
                                                                cmd.itype = SPU_bisled;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 44:
								// 0011 0101 100 P X ROH RA ROL       hbr     brinst, brtarg
                                                                cmd.itype = ((code >> 20) & 1) ? SPU_hbrp : SPU_hbr;
                                                                opnear_roh_rol_repl_left_bit(cmd.Op1, (int16) (((code >> 7) & 0x180) | (code & 0x7f)));
								break;
							case 48:
								// 0011 0110 000 X RA RT              gb      rt, ra
                                                                cmd.itype = SPU_gb;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 49:
								// 0011 0110 001 X RA RT              gbh     rt, ra
                                                                cmd.itype = SPU_gbh;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 50:
								// 0011 0110 010 X RA RT              gbb     rt, ra
                                                                cmd.itype = SPU_gbb;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 52:
								// 0011 0110 100 X RA RT              fsm     rt, ra
                                                                cmd.itype = SPU_fsm;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 53:
								// 0011 0110 101 X RA RT              fsmh    rt, ra
                                                                cmd.itype = SPU_fsmh;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 54:
								// 0011 0110 110 X RA RT              fsmb    rt, ra
                                                                cmd.itype = SPU_fsmb;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 56:
								// 0011 0111 000 X RA RT              frest   rt, ra
                                                                cmd.itype = SPU_frest;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 57:
								// 0011 0111 001 X RA RT              frsqest rt, ra
                                                                cmd.itype = SPU_frsqest;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 68:
								// 0011 1000 100 RB RA RT             lqx     rt, ra, rb
                                                                cmd.itype = SPU_lqx;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 76:
								// 0011 1001 100 RB RA RT             rotqbybi rt, ra, rb
                                                                cmd.itype = SPU_rotqbybi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 77:
								// 0011 1001 101 RB RA RT             rotqmbybi rt, ra, rb
                                                                cmd.itype = SPU_rotqmbybi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 79:
								// 0011 1001 111 RB RA RT             shlqbybi rt, ra, rb
                                                                cmd.itype = SPU_shlqbybi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 84:
								// 0011 1010 100 RB RA RT             cbx     rt, ra, rb
                                                                cmd.itype = SPU_cbx;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 85:
								// 0011 1010 101 RB RA RT             chx     rt, ra, rb
                                                                cmd.itype = SPU_chx;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 86:
								// 0011 1010 110 RB RA RT             cwx     rt, ra, rb
                                                                cmd.itype = SPU_cwx;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 87:
								// 0011 1010 111 RB RA RT             cdx     rt, ra, rb
                                                                cmd.itype = SPU_cdx;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 88:
								// 0011 1011 000 RB RA RT             rotqbi  rt, ra, rb
                                                                cmd.itype = SPU_rotqbi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 89:
								// 0011 1011 001 RB RA RT             rotqmbi rt, ra, rb
                                                                cmd.itype = SPU_rotqmbi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 91:
								// 0011 1011 011 RB RA RT             shlqbi  rt, ra, rb
                                                                cmd.itype = SPU_shlqbi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 92:
								// 0011 1011 100 RB RA RT             rotqby  rt, ra, rb
                                                                cmd.itype = SPU_rotqby;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 93:
								// 0011 1011 101 RB RA RT             rotqmby rt, ra, rb
                                                                cmd.itype = SPU_rotqmby;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 95:
								// 0011 1011 111 RB RA RT             shlqby  rt, ra, rb
                                                                cmd.itype = SPU_shlqby;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opreg_rb(cmd.Op3, code);
								break;
							case 112:
								// 0011 1110 000 X RA RT              orx     rt, ra
                                                                cmd.itype = SPU_orx;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
								break;
							case 116:
								// 0011 1110 100 I7 RA RT             cbd     rt, symbol(ra)
                                                                cmd.itype = SPU_cbd;
                                                                opreg_rt(cmd.Op1, code);
                                                                opphr_i7_symbol_ra(cmd.Op2, code);
								break;
							case 117:
								// 0011 1110 101 I7 RA RT             chd     rt, symbol(ra)
                                                                cmd.itype = SPU_chd;
                                                                opreg_rt(cmd.Op1, code);
                                                                opphr_i7_symbol_ra(cmd.Op2, code);
								break;
							case 118:
								// 0011 1110 110 I7 RA RT             cwd     rt, symbol(ra)
                                                                cmd.itype = SPU_cwd;
                                                                opreg_rt(cmd.Op1, code);
                                                                opphr_i7_symbol_ra(cmd.Op2, code);
								break;
							case 119:
								// 0011 1110 111 I7 RA RT             cdd     rt, symbol(ra)
                                                                cmd.itype = SPU_cdd;
                                                                opreg_rt(cmd.Op1, code);
                                                                opphr_i7_symbol_ra(cmd.Op2, code);
								break;
							case 120:
								// 0011 1111 000 I7 RA RT             rotqbii rt, ra, value
                                                                cmd.itype = SPU_rotqbii;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opimm_i7(cmd.Op3, code);
								break;
							case 121:
								// 0011 1111 001 I7 RA RT             rotqmbii rt, ra, value
                                                                cmd.itype = SPU_rotqmbii;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opimm_i7(cmd.Op3, code);
								break;
							case 123:
								// 0011 1111 011 I7 RA RT             shlqbii rt, ra, value
                                                                cmd.itype = SPU_shlqbii;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opimm_i7(cmd.Op3, code);
								break;
							case 124:
								// 0011 1111 100 I7 RA RT             rotqbyi rt, ra, value
                                                                cmd.itype = SPU_rotqbyi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opimm_i7(cmd.Op3, code);
								break;
							case 125:
								// 0011 1111 101 I7 RA RT             rotqmbyi rt, ra, value
                                                                cmd.itype = SPU_rotqmbyi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opimm_i7(cmd.Op3, code);
								break;
							case 127:
								// 0011 1111 111 I7 RA RT             shlqbyi rt, ra, value
                                                                cmd.itype = SPU_shlqbyi;
                                                                opreg_rt(cmd.Op1, code);
                                                                opreg_ra(cmd.Op2, code);
                                                                opimm_i7(cmd.Op3, code);
								break;
							default:
								return 0;
						}
					}
					break;
			}
		}
		break;
	case 4:
		// 0100 0000 001 X X RT               nop
		// 0100 0000 1   I16 RT               il      rt, symbol
		// 0100 0001 0   I16 RT               ilhu    rt, symbol
		// 0100 0001 1   I16 RT               ilh     rt, symbol
		// 0100 001      I18 RT               ila     rt, symbol
		// 0100 0100     I10 RA RT            xori    rt, ra, value
		// 0100 0101     I10 RA RT            xorhi   rt, ra, value
		// 0100 0110     I10 RA RT            xorbi   rt, ra, value
		// 0100 1000 000 RB RA RT             cgt     rt, ra, rb
		// 0100 1000 001 RB RA RT             xor     rt, ra, rb
		// 0100 1001 000 RB RA RT             cgth    rt, ra, rb
		// 0100 1001 001 RB RA RT             eqv     rt, ra, rb
		// 0100 1010 000 RB RA RT             cgtb    rt, ra, rb
		// 0100 1010 011 RB RA RT             sumb    rt, ra, rb
		// 0100 1011 000 RB RA RT             hgt     ra, rb
		// 0100 1100     I10 RA RT            cgti    rt, ra, value
		// 0100 1101     I10 RA RT            cgthi   rt, ra, value
		// 0100 1110     I10 RA RT            cgtbi   rt, ra, rb
		// 0100 1111     I10 RA RT            hgti    ra, symbol
		{
			int idx = (code >> 24) & 15;
			switch ( idx )
			{
				case 0:
					if (((code >> 21) & 7) == 1)
					{
                                              // 0100 0000 001 X X RT               nop
                                              cmd.itype = SPU_nop;
                                              opreg_rt(cmd.Op1, code);
					}
					else if (((code >> 23) & 1) == 1)
					{
                                              // 0100 0000 1   I16 RT               il      rt, symbol
                                              cmd.itype = SPU_il;
                                              opreg_rt(cmd.Op1, code);
                                              opimm(cmd.Op2, (int16) ((code >> 7) & 0xffff));
					}
					else
					{
                                              return 0;
					}
					break;
				case 1:
					if (((code >> 23) & 1) == 0)
					{
						// 0100 0001 0   I16 RT               ilhu    rt, symbol
                                                cmd.itype = SPU_ilhu;
                                                opreg_rt(cmd.Op1, code);
                                                opimm_i16(cmd.Op2, code);
					}
					else
					{
						// 0100 0001 1   I16 RT               ilh     rt, symbol
                                                cmd.itype = SPU_ilh;
                                                opreg_rt(cmd.Op1, code);
                                                opimm_i16(cmd.Op2, code);
					}
					break;
				case 2:
				case 3:
					// 0100 001      I18 RT               ila     rt, symbol
                                        cmd.itype = SPU_ila;
                                        opreg_rt(cmd.Op1, code);
                                        opnear_i18(cmd.Op2, code);
					break;
				case 4:
					// 0100 0100     I10 RA RT            xori    rt, ra, value
                                        cmd.itype = SPU_xori;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 5:
					// 0100 0101     I10 RA RT            xorhi   rt, ra, value
                                        cmd.itype = SPU_xorhi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 6:
					// 0100 0110     I10 RA RT            xorbi   rt, ra, value
                                        cmd.itype = SPU_xorbi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10(cmd.Op3, code);
					break;
				case 8:
					if (((code >> 21) & 7) == 0)
					{
						// 0100 1000 000 RB RA RT             cgt     rt, ra, rb
                                                cmd.itype = SPU_cgt;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);

					}
					else if (((code >> 21) & 7) == 1)
					{
						// 0100 1000 001 RB RA RT             xor     rt, ra, rb
                                                cmd.itype = SPU_xor;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
					}
					else
					{
						return 0;
					}
					break;
				case 9:
					if (((code >> 21) & 7) == 0)
					{
						// 0100 1001 000 RB RA RT             cgth    rt, ra, rb
                                                cmd.itype = SPU_cgth;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
					}
					else if (((code >> 21) & 7) == 1)

					{
						// 0100 1001 001 RB RA RT             eqv     rt, ra, rb
                                                cmd.itype = SPU_eqv;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
					}
					else
					{
						return 0;
					}
					break;
				case 10:
					if (((code >> 21) & 7) == 0)
					{
						// 0100 1010 000 RB RA RT             cgtb    rt, ra, rb
                                                cmd.itype = SPU_cgtb;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
					}
					else if (((code >> 21) & 7) == 3)
					{
						// 0100 1010 011 RB RA RT             sumb    rt, ra, rb
                                                cmd.itype = SPU_sumb;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
					}
					else
					{
						return 0;
					}
				case 11:
					if (((code >> 21) & 7) == 0)
					{
						// 0100 1011 000 RB RA RT             hgt     ra, rb
                                                cmd.itype = SPU_hgt;
                                                opreg_ra(cmd.Op1, code);
                                                opreg_rb(cmd.Op2, code);
					} 
					else
					{
						return 0;
					}					
					break;
				case 12:
					// 0100 1100     I10 RA RT            cgti    rt, ra, value
                                        cmd.itype = SPU_cgti;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 13:
					// 0100 1101     I10 RA RT            cgthi   rt, ra, value
                                        cmd.itype = SPU_cgthi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 14:
					// 0100 1110     I10 RA RT            cgtbi   rt, ra, rb
                                        cmd.itype = SPU_cgtbi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10(cmd.Op3, code);
					break;
				case 15:
					// 0100 1111     I10 RA RT            hgti    ra, symbol
                                        cmd.itype = SPU_hgti;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				default:
					return 0;
			}
		}
                break;
	case 5:
		{
			// 0101 0100 101 X RA RT              clz     rt, ra
			// 0101 0100 110 X RA RT              xswd    rt, ra
			// 0101 0101 110 X RA RT              xshw    rt, ra
			// 0101 0110 100 X RA RT              cntb    rt, ra
			// 0101 0110 110 X RA RT              xsbh    rt, ra
			// 0101 1000 000 RB RA RT             clgt    rt, ra, rb
			// 0101 1000 001 RB RA RT             andc    rt, ra, rb
			// 0101 1000 010 RB RA RT             fcgt    rt, ra, rb
			// 0101 1000 011 RB RA RT             dfcgt   rt, ra, rb
			// 0101 1000 100 RB RA RT             fa      rt, ra, rb
			// 0101 1000 101 RB RA RT             fs      rt, ra, rb
			// 0101 1000 110 RB RA RT             fm      rt, ra, rb
			// 0101 1001 000 RB RA RT             clgth   rt, ra, rb
			// 0101 1001 001 RB RA RT             orc     rt, ra, rb
			// 0101 1001 010 RB RA RT             fcmgt   rt, ra, rb
			// 0101 1001 011 RB RA RT             dfcmgt  rt, ra, rb
			// 0101 1001 100 RB RA RT             dfa     rt, ra, rb
			// 0101 1001 101 RB RA RT             dfs     rt, ra, rb
			// 0101 1001 110 RB RA RT             dfm     rt, ra, rb
			// 0101 1010 000 RB RA RT             clgtb   rt, ra, rb
			// 0101 1011 000 RB RA RT             hlgt    ra, rb
			// 0101 1100     I10 RA RT            clgti   rt, ra, value
			// 0101 1101     I10 RA RT            clgthi  rt, ra, value
			// 0101 1110     I10 RA RT            clgtbi  rt, ra, value
			// 0101 1111     I10 RA RT            hlgti   ra, symbol
			int idx = (code >> 24) & 15;
			switch ( idx )
			{
				case 4:
					if (((code >> 21) & 7) == 5)
					{
						// 0101 0100 101 X RA RT              clz     rt, ra
                                                cmd.itype = SPU_clz;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
					}
					else if (((code >> 21) & 7) == 6)
					{
						// 0101 0100 110 X RA RT              xswd    rt, ra
                                                cmd.itype = SPU_xswd;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
					}
					else
					{
						return 0;
					}
					break;
				case 5:
					if (((code >> 21) & 7) == 6)
					{
						// 0101 0101 110 X RA RT              xshw    rt, ra
                                                cmd.itype = SPU_xshw;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
					}
					else
					{
						return 0;
					}					
					break;
				case 6:
					if (((code >> 21) & 7) == 4)
					{
						// 0101 0110 100 X RA RT              cntb    rt, ra
                                                cmd.itype = SPU_cntb;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
					}
					else if (((code >> 21) & 7) == 6)
					{
						// 0101 0110 110 X RA RT              xsbh    rt, ra
                                                cmd.itype = SPU_xsbh;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
					}
					else
					{
						return 0;
					}
					break;					
				case 8:					
					// 0101 1000 000 RB RA RT             clgt    rt, ra, rb
					// 0101 1000 001 RB RA RT             andc    rt, ra, rb
					// 0101 1000 010 RB RA RT             fcgt    rt, ra, rb
					// 0101 1000 011 RB RA RT             dfcgt   rt, ra, rb
					// 0101 1000 100 RB RA RT             fa      rt, ra, rb
					// 0101 1000 101 RB RA RT             fs      rt, ra, rb
					// 0101 1000 110 RB RA RT             fm      rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ SPU_clgt, SPU_andc, SPU_fcgt, SPU_dfcgt, SPU_fa, SPU_fs, SPU_fm, 0	};
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
					break;
				case 9:
					// 0101 1001 000 RB RA RT             clgth   rt, ra, rb
					// 0101 1001 001 RB RA RT             orc     rt, ra, rb
					// 0101 1001 010 RB RA RT             fcmgt   rt, ra, rb
					// 0101 1001 011 RB RA RT             dfcmgt  rt, ra, rb
					// 0101 1001 100 RB RA RT             dfa     rt, ra, rb
					// 0101 1001 101 RB RA RT             dfs     rt, ra, rb
					// 0101 1001 110 RB RA RT             dfm     rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ SPU_clgth, SPU_orc, SPU_fcmgt, SPU_dfcmgt, SPU_dfa, SPU_dfs, SPU_dfm, 0	};
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
					break;
				case 10:
					if (((code >> 21) & 7) == 0)
					{
						// 0101 1010 000 RB RA RT             clgtb   rt, ra, rb
                                                cmd.itype = SPU_clgtb;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
					}
					else
					{
						return 0;
					}					
					break;
				case 11:
					if (((code >> 21) & 7) == 0)
					{
						// 0101 1011 000 RB RA RT             hlgt    ra, rb
                                                cmd.itype = SPU_hlgt;
                                                opreg_ra(cmd.Op1, code);
                                                opreg_rb(cmd.Op2, code);
					} 
					else
					{
						return 0;
					}
					break;
				case 12:
					// 0101 1100     I10 RA RT            clgti   rt, ra, value
                                        cmd.itype = SPU_clgti;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 13:
					// 0101 1101     I10 RA RT            clgthi  rt, ra, value
                                        cmd.itype = SPU_clgthi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 14:
					// 0101 1110     I10 RA RT            clgtbi  rt, ra, value
                                        cmd.itype = SPU_clgtbi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10(cmd.Op3, code);
					break;
				case 15:
					// 0101 1111     I10 RA RT            hlgti   ra, symbol
                                        cmd.itype = SPU_hlgti;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				default:
					return 0;
			}
		}
                break;
	case 6:
		{
			// 0110 0000 1   I16 RT               iohl    rt, symbol
			// 0110 1000 000 RB RA RT             addx    rt, ra, rb
			// 0110 1000 001 RB RA RT             sfx     rt, ra, rb
			// 0110 1000 010 RB RA RT             cgx     rt, ra, rb
			// 0110 1000 011 RB RA RT             bgx     rt, ra, rb
			// 0110 1000 110 RB RA RT             mpyhha  rt, ra, rb
			// 0110 1001 110 RB RA RT             mpyhhau rt, ra, rb
			// 0110 1011 100 RB RA RT             dfma    rt, ra, rb
			// 0110 1011 101 RB RA RT             dfms    rt, ra, rb
			// 0110 1011 110 RB RA RT             dfnms   rt, ra, rb
			// 0110 1011 111 RB RA RT             dfnma   rt, ra, rb
			int idx = (code >> 24) & 15;
			switch ( idx )
			{
				case 0:
					if (((code >> 23) & 1) == 1)
					{
						// 0110 0000 1   I16 RT               iohl    rt, symbol
                                                cmd.itype = SPU_iohl;
                                                opreg_rt(cmd.Op1, code);
                                                opimm_i16(cmd.Op2, code);
					}
					else
					{
						return 0;
					}
					break;
				case 8:
					// 0110 1000 000 RB RA RT             addx    rt, ra, rb
					// 0110 1000 001 RB RA RT             sfx     rt, ra, rb
					// 0110 1000 010 RB RA RT             cgx     rt, ra, rb
					// 0110 1000 011 RB RA RT             bgx     rt, ra, rb
					// 0110 1000 110 RB RA RT             mpyhha  rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ SPU_addx, SPU_sfx, SPU_cgx, SPU_bgx, 0, 0, SPU_mpyhha, 0	};
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
					break;
				case 9:
					if (((code >> 21) & 7) == 6)
					{
						// 0110 1001 110 RB RA RT             mpyhhau rt, ra, rb
                                                cmd.itype = SPU_mpyhhau;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
					}
					else
					{
						return 0;
					}
					break;
				case 11:
					// 0110 1011 100 RB RA RT             dfma    rt, ra, rb
					// 0110 1011 101 RB RA RT             dfms    rt, ra, rb
					// 0110 1011 110 RB RA RT             dfnms   rt, ra, rb
					// 0110 1011 111 RB RA RT             dfnma   rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ 0, 0, 0, 0, SPU_dfma, SPU_dfms, SPU_dfnms, SPU_dfnma	};
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
					break;
				default:
					return 0;
			}
		}
                break;
	case 7:
		// 0111 0011 000 X X RT               fscrrd  rt
		// 0111 0100     I10 RA RT            mpyi    rt, ra, value
		// 0111 0101     I10 RA RT            mpyui   rt, ra, value
		// 0111 0110 00  I8 RA RT             cflts   rt, ra, scale
		// 0111 0110 01  I8 RA RT             cfltu   rt, ra, scale
		// 0111 0110 10  I8 RA RT             csflt   rt, ra, scale
		// 0111 0110 11  I8 RA RT             cuflt   rt, ra, scale
		// 0111 0111 000 X RA RT              fesd    rt, ra
		// 0111 0111 001 X RA RT              frds    rt, ra
		// 0111 0111 010 X RA RT              fscrwr  ra
		// 0111 0111 111 I7 RA RT             dftsv   rt, ra, value
		// 0111 1000 000 RB RA RT             ceq     rt, ra, rb
		// 0111 1000 010 RB RA RT             fceq    rt, ra, rb
		// 0111 1000 011 RB RA RT             dfceq   rt, ra, rb
		// 0111 1000 100 RB RA RT             mpy     rt, ra, rb
		// 0111 1000 101 RB RA RT             mpyh    rt, ra, rb
		// 0111 1000 110 RB RA RT             mpyhh   rt, ra, rb
		// 0111 1000 111 RB RA RT             mpys    rt, ra, rb
		// 0111 1001 000 RB RA RT             ceqh    rt, ra, rb
		// 0111 1001 010 RB RA RT             fcmeq   rt, ra, rb
		// 0111 1001 011 RB RA RT             dfcmeq  rt, ra, rb
		// 0111 1001 100 RB RA RT             mpyu    rt, ra, rb
		// 0111 1001 110 RB RA RT             mpyhhu  rt, ra, rb
		// 0111 1010 000 RB RA RT             ceqb    rt, ra, rb
		// 0111 1010 100 RB RA RT             fi      rt, ra, rb
		// 0111 1011 000 RB RA RT             heq     ra, rb
		// 0111 1100     I10 RA RT            ceqi    rt, ra, value
		// 0111 1101     I10 RA RT            ceqhi   rt, ra, value
		// 0111 1110     I10 RA RT            ceqbi   rt, ra, value
		// 0111 1111     I10 RA RT            heqi    ra, symbol
		{
			int idx = (code >> 24) & 15;
			switch ( idx )
			{
				case 3:
					if (((code >> 21) & 7) == 0)
					{
						// 0111 0011 000 X X RT               fscrrd  rt
                                                cmd.itype = SPU_fscrrd;
                                                opreg_rt(cmd.Op1, code);
					}
					else
					{
						return 0;
					}					
					break;
				case 4:
					// 0111 0100     I10 RA RT            mpyi    rt, ra, value
                                        cmd.itype = SPU_mpyi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 5:
					// 0111 0101     I10 RA RT            mpyui   rt, ra, value
                                        cmd.itype = SPU_mpyui;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 6:
					// 0111 0110 00  I8 RA RT             cflts   rt, ra, scale
					// 0111 0110 01  I8 RA RT             cfltu   rt, ra, scale
					// 0111 0110 10  I8 RA RT             csflt   rt, ra, scale
					// 0111 0110 11  I8 RA RT             cuflt   rt, ra, scale
					{
						int idx = ((code >> 22) & 3);
						static const uchar itypes[] =
						{ SPU_cflts, SPU_cfltu, SPU_csflt, SPU_cuflt };
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opimm(cmd.Op3, (code >> 14) & 0xff);
				 	}
					break;
				case 7:
                                        switch (((code >> 21) & 7))
                                        {
                                        case 0:
                                                // 0111 0111 000 X RA RT              fesd    rt, ra
                                                cmd.itype = SPU_fesd;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                break;
                                        case 1:
                                                // 0111 0111 001 X RA RT              frds    rt, ra
                                                cmd.itype = SPU_frds;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                break;
                                        case 2:
                                                // 0111 0111 010 X RA                 fscrwr  ra
                                                cmd.itype = SPU_fscrwr;
                                                opreg_ra(cmd.Op1, code);
                                                break;
                                        case 7:
                                                // 0111 0111 111 I7 RA RT             dftsv   rt, ra, value
                                                cmd.itype = SPU_dftsv;
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opimm_i7(cmd.Op3, code);
                                                break;
                                        default:
                                                return 0;
                                        }
					break;
				case 8:
					// 0111 1000 000 RB RA RT             ceq     rt, ra, rb
					// 0111 1000 010 RB RA RT             fceq    rt, ra, rb
					// 0111 1000 011 RB RA RT             dfceq   rt, ra, rb
					// 0111 1000 100 RB RA RT             mpy     rt, ra, rb
					// 0111 1000 101 RB RA RT             mpyh    rt, ra, rb
					// 0111 1000 110 RB RA RT             mpyhh   rt, ra, rb
					// 0111 1000 111 RB RA RT             mpys    rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ SPU_ceq, 0, SPU_fceq, SPU_dfceq 
						, SPU_mpy, SPU_mpyh, SPU_mpyhh, SPU_mpys	};
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
					break;
				case 9:
					// 0111 1001 000 RB RA RT             ceqh    rt, ra, rb
					// 0111 1001 010 RB RA RT             fcmeq   rt, ra, rb
					// 0111 1001 011 RB RA RT             dfcmeq  rt, ra, rb
					// 0111 1001 100 RB RA RT             mpyu    rt, ra, rb
					// 0111 1001 110 RB RA RT             mpyhhu  rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ SPU_ceqh, 0, SPU_fcmeq, SPU_dfcmeq, SPU_mpyu, 0, SPU_mpyhhu, 0	};
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
					break;
				case 10:
					// 0111 1010 000 RB RA RT             ceqb    rt, ra, rb
					// 0111 1010 100 RB RA RT             fi      rt, ra, rb
					{
						int idx = ((code >> 21) & 7);
						static const uchar itypes[] =
						{ SPU_ceqb, 0, 0, 0, SPU_dfma, 0, 0, 0	};
						if (itypes[idx] == 0)
							return 0;
                                                cmd.itype = itypes[idx];
                                                opreg_rt(cmd.Op1, code);
                                                opreg_ra(cmd.Op2, code);
                                                opreg_rb(cmd.Op3, code);
				 	}
					break;
				case 11:
					if (((code >> 21) & 7) == 0)
					{
						// 0111 1011 000 RB RA RT             heq     ra, rb
                                                cmd.itype = SPU_heq;
                                                opreg_ra(cmd.Op1, code);
                                                opreg_rb(cmd.Op2, code);
					}
					else
					{
						return 0;
					}					
					break;
				case 12:
					// 0111 1100     I10 RA RT            ceqi    rt, ra, value
                                        cmd.itype = SPU_ceqi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 13:
					// 0111 1101     I10 RA RT            ceqhi   rt, ra, value
                                        cmd.itype = SPU_ceqhi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				case 14:
					// 0111 1110     I10 RA RT            ceqbi   rt, ra, value
                                        cmd.itype = SPU_ceqbi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10(cmd.Op3, code);
					break;
				case 15:
					// 0111 1111     I10 RA RT            heqi    ra, symbol
                                        cmd.itype = SPU_heqi;
                                        opreg_rt(cmd.Op1, code);
                                        opreg_ra(cmd.Op2, code);
                                        opimm_i10_repl_left_bit(cmd.Op3, code);
					break;
				default:
					return 0;
			}
		}
		break;
	case 8:
		// 1000 RT RB RA RC                 selb    rt, ra, rb, rc
                opreg_rt_ra_rb_rc(SPU_selb, code);
		break;
	case 11:
		// 1011 RT RB RA RC                 shufb   rt, ra, rb, rc
                opreg_rt_ra_rb_rc(SPU_shufb, code);
		break;
	case 12:
		// 1100 RT RB RA RC                 mpya    rt, ra, rb, rc
                opreg_rt_ra_rb_rc(SPU_mpya, code);
		break;
	case 13:
		// 1101 RT RB RA RC                 fnms    rt, ra, rb, rc
                opreg_rt_ra_rb_rc(SPU_fnms, code);
		break;
	case 14:
		// 1110 RT RB RA RC                 fma     rt, ra, rb, rc
                opreg_rt_ra_rb_rc(SPU_fma, code);
		break;
	case 15:
		// 1111 RT RB RA RC                 fms     rt, ra, rb, rc
                opreg_rt_ra_rb_rc(SPU_fms, code);
		break;
	}

	if ( cmd.itype == SPU_null ) return 0;
	return cmd.size;
}

