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

#ifndef __INSTRS_HPP
#define __INSTRS_HPP

#include "../idaidp.hpp"

extern instruc_t Instructions[];

enum nameNum ENUM_SIZE(uint16)
{

SPU_null = 0,           // Unknown Operation

// Channel Instructions

SPU_rchcnt,             // Read Channel Count
SPU_rdch,               // Read Channel
SPU_wrch,               // Write Channel

// Compare, Branch, and Halt Instructions

SPU_bi,                 // Branch Indirect
SPU_bihnz,              // Branch Indirect If Not Zero Halfword
SPU_bihz,               // Branch Indirect If Zero Halfword
SPU_binz,               // Branch Indirect If Not Zero
SPU_bisl,               // Branch Indirect and Set Link
SPU_bisled,             // Branch Indirect and Set Link if External Data
SPU_biz,                // Branch Indirect If Zero
SPU_br,                 // Branch Relative
SPU_bra,                // Branch Absolute
SPU_brasl,              // Branch Absolute and Set Link
SPU_brhnz,              // Branch If Not Zero Halfword
SPU_brhz,               // Branch If Zero Halfword
SPU_brnz,               // Branch If Not Zero Word
SPU_brsl,               // Branch Relative and Set Link
SPU_brz,                // Branch If Zero Word
SPU_ceq,                // Compare Equal Word
SPU_ceqb,               // Compare Equal Byte
SPU_ceqbi,              // Compare Equal Byte Immediate
SPU_ceqh,               // Compare Equal Halfword
SPU_ceqhi,              // Compare Equal Halfword Immediate
SPU_ceqi,               // Compare Equal Word Immediate
SPU_cgt,                // Compare Greater Than Word
SPU_cgtb,               // Compare Greater Than Byte
SPU_cgtbi,              // Compare Greater Than Byte Immediate
SPU_cgth,               // Compare Greater Than Halfword
SPU_cgthi,              // Compare Greater Than Halfword Immediate
SPU_cgti,               // Compare Greater Than Word Immediate
SPU_clgt,               // Compare Logical Greater Than Word
SPU_clgtb,              // Compare Logical Greater Than Byte
SPU_clgtbi,             // Compare Logical Greater Than Byte Immediate
SPU_clgth,              // Compare Logical Greater Than Halfword
SPU_clgthi,             // Compare Logical Greater Than Halfword Immediate
SPU_clgti,              // Compare Logical Greater Than Word Immediate
SPU_heq,                // Halt If Equal
SPU_heqi,               // Halt If Equal Immediate
SPU_hgt,                // Halt If Greater Than
SPU_hgti,               // Halt If Greater Than Immediate
SPU_hlgt,               // Halt If Logically Greater Than
SPU_hlgti,              // Halt If Logically Greater Than Immediate
SPU_iret,               // Interrupt Return

// Constant-Formation Instructions

SPU_fsmbi,              // Form Select Mask for Bytes Immediate
SPU_il,                 // Immediate Load Word
SPU_ila,                // Immediate Load Address
SPU_ilh,                // Immediate Load Halfword
SPU_ilhu,               // Immediate Load Halfword Upper
SPU_iohl,               // Immediate Or Halfword Lower

// Control Instructions

SPU_dsync,              // Synchronize Data
SPU_lnop,               // No Operation (Load)
SPU_mfspr,              // Move from Special-Purpose Register
SPU_mtspr,              // Move to Special-Purpose Register
SPU_nop,                // No Operation (Execute)
SPU_stop,               // Stop and Signal
SPU_stopd,              // Stop and Signal with Dependencies
SPU_sync,               // Synchronize

// Floating-Point Status and Control Register Instructions

SPU_cfltu,              // Convert Floating to Unsigned Integer
SPU_csflt,              // Convert Signed Integer to Floating
SPU_cuflt,              // Convert Unsigned Integer to Floating
SPU_dfa,                // Double Floating Add
SPU_dfceq,              // Double Floating Compare Equal
SPU_dfcgt,              // Double Floating Compare Greater Than
SPU_dfcmeq,             // Double Floating Compare Magnitude Equal
SPU_dfcmgt,             // Double Floating Compare Magnitude Greater Than
SPU_dfm,                // Double Floating Multiply
SPU_dfma,               // Double Floating Multiply and Add
SPU_dfms,               // Double Floating Multiply and Subtract
SPU_dfnma,              // Double Floating Negative Multiply and Add
SPU_dfnms,              // Double Floating Multiply and Subtract
SPU_dfs,                // Double Floating Subtract
SPU_dftsv,              // Double Floating Test Special Value
SPU_fa,                 // Floating Add
SPU_fceq,               // Floating Compare Equal
SPU_fcgt,               // Floating Compare Greater Than
SPU_fcmeq,              // Floating Compare Magnitude Equal
SPU_fcmgt,              // Floating Compare Magnitude Greater Than
SPU_fesd,               // Floating Extend Single to Double
SPU_fi,                 // Floating Interpolate
SPU_fm,                 // Floating Multiply
SPU_fma,                // Floating Multiply and Add
SPU_fms,                // Floating Multiply and Subtract
SPU_fnms,               // Floating Negative Multiply and Subtract
SPU_frds,               // Floating Round Double to Single
SPU_frest,              // Floating Reciprocal Estimate
SPU_frsqest,            // Floating Reciprocal Absolute Square Root Estimate
SPU_fs,                 // Floating Subtract
SPU_fscrrd,             // Floating-Point Status and Control Register Write
SPU_fscrwr,             // Floating-Point Status and Control Register Read

// Hint-for-Branch Instructions

SPU_cflts,              // Convert Floating to Signed Integer
SPU_hbr,                // Hint for Branch (r-form)
SPU_hbrp,		// Hint for Branch (r-form) p field..
SPU_hbra,               // Hint for Branch (a-form)
SPU_hbrr,               // Hint for Branch Relative

// Integer and Logical Instructions

SPU_a,                  // Add Word
SPU_absdb,              // Absolute Differences of Bytes
SPU_addx,               // Add Extended
SPU_ah,                 // Add Halfword
SPU_ahi,                // Add Halfword Immediate
SPU_ai,                 // Add Word Immediate
SPU_and,                // And
SPU_andbi,              // And Byte Immediate
SPU_andc,               // And with Complement
SPU_andhi,              // And Halfword Immediate
SPU_andi,               // And Word Immediate
SPU_avgb,               // Average Bytes
SPU_bg,                 // Borrow Generate
SPU_bgx,                // Borrow Generate Extended
SPU_cg,                 // Carry Generate
SPU_clz,                // Count Leading Zeros
SPU_cntb,               // Count Ones in Bytes
SPU_eqv,                // Equivalent
SPU_fsm,                // Form Select Mask for Words
SPU_fsmb,               // Form Select Mask for Bytes
SPU_fsmh,               // Form Select Mask for Halfwords
SPU_gb,                 // Gather Bits from Words
SPU_gbb,                // Gather Bits from Bytes
SPU_gbh,                // Gather Bits from Halfwords
SPU_mpy,                // Multiply
SPU_mpya,               // Multiply and Add
SPU_mpyh,               // Multiply High
SPU_mpyhh,              // Multiply High High
SPU_mpyhha,             // Multiply High High and Add
SPU_mpyhhau,            // Multiply High High Unsigned and Add
SPU_mpyhhu,             // Multiply High High Unsigned
SPU_mpyi,               // Multiply Immediate
SPU_mpys,               // Multiply and Shift Right
SPU_mpyu,               // Multiply Unsigned
SPU_mpyui,              // Multiply Unsigned Immediate
SPU_nand,               // Nand
SPU_nor,                // Nor
SPU_or,                 // Or
SPU_orbi,               // Or Byte Immediate
SPU_orc,                // Or with Complement
SPU_orhi,               // Or Halfword Immediate
SPU_ori,                // Or Word Immediate
SPU_orx,                // Or Across
SPU_selb,               // Select Bits
SPU_sf,                 // Subtract from Word
SPU_sfh,                // Subtract from Halfword
SPU_sfhi,               // Subtract from Halfword Immediate
SPU_sfi,                // Subtract from Word Immediate
SPU_sfx,                // Subtract from Extended
SPU_shufb,              // Shuffle Bytes
SPU_sumb,               // Sum Bytes into Halfwords
SPU_xor,                // Exclusive Or
SPU_xorbi,              // Exclusive Or Byte Immediate
SPU_xorhi,              // Exclusive Or Halfword Immediate
SPU_xori,               // Exclusive Or Word Immediate
SPU_xsbh,               // Extend Sign Byte to Halfword
SPU_xshw,               // Extend Sign Halfword to Word
SPU_xswd,               // Extend Sign Word to Doubleword

// Memory—Load/Store Instructions

SPU_cbd,                // Generate Controls for Byte Insertion (d-form)
SPU_cbx,                // Generate Controls for Byte Insertion (x-form)
SPU_cdd,                // Generate Controls for Doubleword Insertion (d-form)
SPU_cdx,                // Generate Controls for Doubleword Insertion (x-form)
SPU_cgx,                // Carry Generate Extended
SPU_chd,                // Generate Controls for Halfword Insertion (d-form)
SPU_chx,                // Generate Controls for Halfword Insertion (x-form)
SPU_cwd,                // Generate Controls for Word Insertion (d-form)
SPU_cwx,                // Generate Controls for Word Insertion (x-form)
SPU_lqa,                // Load Quadword (a-form)
SPU_lqd,                // Load Quadword (d-form)
SPU_lqr,                // Load Quadword Instruction Relative (a-form)
SPU_lqx,                // Load Quadword (x-form)
SPU_stqa,               // Store Quadword (a-form)
SPU_stqd,               // Store Quadword (d-form)
SPU_stqr,               // Store Quadword Instruction Relative (a-form)
SPU_stqx,               // Store Quadword (x-form)

// Shift and Rotate Instructions

SPU_rot,                // Rotate Word
SPU_roth,               // Rotate Halfword
SPU_rothi,              // Rotate Halfword Immediate
SPU_rothm,              // Rotate and Mask Halfword
SPU_rothmi,             // Rotate and Mask Halfword Immediate
SPU_roti,               // Rotate Word Immediate
SPU_rotm,               // Rotate and Mask Word
SPU_rotma,              // Rotate and Mask Algebraic Word
SPU_rotmah,             // Rotate and Mask Algebraic Halfword
SPU_rotmahi,            // Rotate and Mask Algebraic Halfword Immediate
SPU_rotmai,             // Rotate and Mask Algebraic Word Immediate
SPU_rotmi,              // Rotate and Mask Word Immediate
SPU_rotqbi,             // Rotate Quadword by Bits
SPU_rotqbii,            // Rotate Quadword by Bits Immediate
SPU_rotqby,             // Rotate Quadword by Bytes
SPU_rotqbybi,           // Rotate Quadword by Bytes from Bit Shift Count
SPU_rotqbyi,            // Rotate Quadword by Bytes Immediate
SPU_rotqmbi,            // Rotate and Mask Quadword by Bits
SPU_rotqmbii,           // Rotate and Mask Quadword by Bits Immediate
SPU_rotqmby,            // Rotate and Mask Quadword by Bytes
SPU_rotqmbybi,          // Rotate and Mask Quadword Bytes from Bit Shift Count
SPU_rotqmbyi,           // Rotate and Mask Quadword by Bytes Immediate
SPU_shl,                // Shift Left Word
SPU_shlh,               // Shift Left Halfword
SPU_shlhi,              // Shift Left Halfword Immediate
SPU_shli,               // Shift Left Word Immediate
SPU_shlqbi,             // Shift Left Quadword by Bits
SPU_shlqbii,            // Shift Left Quadword by Bits Immediate
SPU_shlqby,             // Shift Left Quadword by Bytes
SPU_shlqbybi,           // Shift Left Quadword by Bytes from Bit Shift Count
SPU_shlqbyi,            // Shift Left Quadword by Bytes Immediate

SPU_last,

};

#endif

