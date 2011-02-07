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

#include "ins.hpp"

instruc_t Instructions[] =
{

{ "",          0                                                 },      // Unknown Operation

// Channel Instructions

{ "rchcnt",    CF_USE2|CF_CHG1                                   },      // Read Channel Count                                 
{ "rdch",      CF_USE2|CF_CHG1                                   },      // Read Channel                                       
{ "wrch",      CF_USE2|CF_CHG1                                   },      // Write Channel                                      

// Compare, Branch, and Halt Instructions

{ "bi",        CF_USE1|CF_STOP|CF_JUMP                           },      // Branch Indirect                                    
{ "bihnz",     CF_USE2|CF_USE1|CF_JUMP                           },      // Branch Indirect If Not Zero Halfword               
{ "bihz",      CF_USE2|CF_USE1|CF_JUMP                           },      // Branch Indirect If Zero Halfword                   
{ "binz",      CF_USE2|CF_USE1|CF_JUMP                           },      // Branch Indirect If Not Zero                        
{ "bisl",      CF_USE2|CF_CHG1|CF_CALL                           },      // Branch Indirect and Set Link                       
{ "bisled",    CF_USE2|CF_CHG1|CF_CALL                           },      // Branch Indirect and Set Link if External Data      
{ "biz",       CF_USE2|CF_USE1|CF_JUMP                           },      // Branch Indirect If Zero                            
{ "br",        CF_USE1|CF_JUMP                                   },      // Branch Relative                                    
{ "bra",       CF_USE1|CF_JUMP                                   },      // Branch Absolute                                    
{ "brasl",     CF_USE2|CF_CHG1|CF_CALL                           },      // Branch Absolute and Set Link                       
{ "brhnz",     CF_USE2|CF_USE1|CF_JUMP                           },      // Branch If Not Zero Halfword                        
{ "brhz",      CF_USE2|CF_USE1|CF_JUMP                           },      // Branch If Zero Halfword                            
{ "brnz",      CF_USE2|CF_USE1|CF_JUMP                           },      // Branch If Not Zero Word                            
{ "brsl",      CF_USE2|CF_CHG1|CF_CALL                           },      // Branch Relative and Set Link                       
{ "brz",       CF_USE2|CF_USE1|CF_JUMP                           },      // Branch If Zero Word                                
{ "ceq",       CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Equal Word                                 
{ "ceqb",      CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Equal Byte                                 
{ "ceqbi",     CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Equal Byte Immediate                       
{ "ceqh",      CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Equal Halfword                             
{ "ceqhi",     CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Equal Halfword Immediate                   
{ "ceqi",      CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Equal Word Immediate                       
{ "cgt",       CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Greater Than Word                          
{ "cgtb",      CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Greater Than Byte                          
{ "cgtbi",     CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Greater Than Byte Immediate                
{ "cgth",      CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Greater Than Halfword                      
{ "cgthi",     CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Greater Than Halfword Immediate            
{ "cgti",      CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Greater Than Word Immediate                
{ "clgt",      CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Logical Greater Than Word                  
{ "clgtb",     CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Logical Greater Than Byte                  
{ "clgtbi",    CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Logical Greater Than Byte Immediate        
{ "clgth",     CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Logical Greater Than Halfword              
{ "clgthi",    CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Logical Greater Than Halfword Immediate    
{ "clgti",     CF_USE3|CF_USE2|CF_CHG1                           },      // Compare Logical Greater Than Word Immediate        
{ "heq",       CF_USE2|CF_USE1                                   },      // Halt If Equal                                      
{ "heqi",      CF_USE2|CF_USE1                                   },      // Halt If Equal Immediate                            
{ "hgt",       CF_USE2|CF_USE1                                   },      // Halt If Greater Than                               
{ "hgti",      CF_USE2|CF_USE1                                   },      // Halt If Greater Than Immediate                     
{ "hlgt",      CF_USE2|CF_USE1                                   },      // Halt If Logically Greater Than                     
{ "hlgti",     CF_USE2|CF_USE1                                   },      // Halt If Logically Greater Than Immediate           
{ "iret",      CF_USE1                                           },      // Interrupt Return                                   

// Constant-Formation Instructions

{ "fsmbi",     CF_USE2|CF_CHG1                                   },      // Form Select Mask for Bytes Immediate               
{ "il",        CF_USE2|CF_CHG1                                   },      // Immediate Load Word                                
{ "ila",       CF_USE2|CF_CHG1                                   },      // Immediate Load Address                             
{ "ilh",       CF_USE2|CF_CHG1                                   },      // Immediate Load Halfword                            
{ "ilhu",      CF_USE2|CF_CHG1                                   },      // Immediate Load Halfword Upper                      
{ "iohl",      CF_USE2|CF_CHG1                                   },      // Immediate Or Halfword Lower                        

// Control Instructions

{ "dsync",     0                                                 },      // Synchronize Data                                   
{ "lnop",      0                                                 },      // No Operation (Load)                                
{ "mfspr",     CF_USE2|CF_CHG1                                   },      // Move from Special-Purpose Register                 
{ "mtspr",     CF_USE2|CF_CHG1                                   },      // Move to Special-Purpose Register                   
{ "nop",       0                                                 },      // No Operation (Execute)                             
{ "stop",      CF_USE1                                           },      // Stop and Signal
{ "stopd",     CF_USE3|CF_USE2|CF_USE1                           },      // Stop and Signal with Dependencies                  
{ "sync",      0                                                 },      // Synchronize                                        

// Floating-Point Status and Control Register Instructions

{ "cfltu",     CF_USE3|CF_USE2|CF_CHG1                           },      // Convert Floating to Unsigned Integer               
{ "csflt",     CF_USE3|CF_USE2|CF_CHG1                           },      // Convert Signed Integer to Floating                 
{ "cuflt",     CF_USE3|CF_USE2|CF_CHG1                           },      // Convert Unsigned Integer to Floating               
{ "dfa",       CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Add                                
{ "dfceq",     CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Compare Equal                      
{ "dfcgt",     CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Compare Greater Than               
{ "dfcmeq",    CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Compare Magnitude Equal            
{ "dfcmgt",    CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Compare Magnitude Greater Than     
{ "dfm",       CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Multiply                           
{ "dfma",      CF_USE4|CF_USE3|CF_USE2|CF_CHG1                   },      // Double Floating Multiply and Add                   
{ "dfms",      CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Multiply and Subtract              
{ "dfnma",     CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Negative Multiply and Add          
{ "dfnms",     CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Multiply and Subtract              
{ "dfs",       CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Subtract                           
{ "dftsv",     CF_USE3|CF_USE2|CF_CHG1                           },      // Double Floating Test Special Value                 
{ "fa",        CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Add                                       
{ "fceq",      CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Compare Equal                             
{ "fcgt",      CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Compare Greater Than                      
{ "fcmeq",     CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Compare Magnitude Equal                   
{ "fcmgt",     CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Compare Magnitude Greater Than            
{ "fesd",      CF_USE2|CF_CHG1                                   },      // Floating Extend Single to Double                   
{ "fi",        CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Interpolate                               
{ "fm",        CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Multiply                                  
{ "fma",       CF_USE4|CF_USE3|CF_USE2|CF_CHG1                   },      // Floating Multiply and Add                          
{ "fms",       CF_USE4|CF_USE3|CF_USE2|CF_CHG1                   },      // Floating Multiply and Subtract                     
{ "fnms",      CF_USE4|CF_USE3|CF_USE2|CF_CHG1                   },      // Floating Negative Multiply and Subtract            
{ "frds",      CF_USE2|CF_CHG1                                   },      // Floating Round Double to Single                    
{ "frest",     CF_USE2|CF_CHG1                                   },      // Floating Reciprocal Estimate                       
{ "frsqest",   CF_USE2|CF_CHG1                                   },      // Floating Reciprocal Absolute Square Root Estimate  
{ "fs",        CF_USE3|CF_USE2|CF_CHG1                           },      // Floating Subtract                                  
{ "fscrrd",    CF_USE2|CF_CHG1                                   },      // Floating-Point Status and Control Register Write   
{ "fscrwr",    CF_USE2|CF_CHG1                                   },      // Floating-Point Status and Control Register Read    

// Hint-for-Branch Instructions

{ "cflts",     CF_USE3|CF_USE2|CF_CHG1                           },      // Convert Floating to Signed Integer                 
{ "hbr",       CF_USE2|CF_USE1                                   },      // Hint for Branch (r-form)                           
{ "hbrp",      CF_USE2|CF_USE1                                   },      // Hint for Branch (r-form) p field..                           
{ "hbra",      CF_USE2|CF_USE1                                   },      // Hint for Branch (a-form)
{ "hbrr",      CF_USE2|CF_USE1                                   },      // Hint for Branch Relative                           

// Integer and Logical Instructions

{ "a",         CF_USE3|CF_USE2|CF_CHG1                           },      // Add Word                                           
{ "absdb",     CF_USE3|CF_USE2|CF_CHG1                           },      // Absolute Differences of Bytes                      
{ "addx",      CF_USE3|CF_USE2|CF_CHG1                           },      // Add Extended                                       
{ "ah",        CF_USE3|CF_USE2|CF_CHG1                           },      // Add Halfword                                       
{ "ahi",       CF_USE3|CF_USE2|CF_CHG1                           },      // Add Halfword Immediate                             
{ "ai",        CF_USE3|CF_USE2|CF_CHG1                           },      // Add Word Immediate                                 
{ "and",       CF_USE3|CF_USE2|CF_CHG1                           },      // And                                                
{ "andbi",     CF_USE3|CF_USE2|CF_CHG1                           },      // And Byte Immediate                                 
{ "andc",      CF_USE3|CF_USE2|CF_CHG1                           },      // And with Complement                                
{ "andhi",     CF_USE3|CF_USE2|CF_CHG1                           },      // And Halfword Immediate                             
{ "andi",      CF_USE3|CF_USE2|CF_CHG1                           },      // And Word Immediate                                 
{ "avgb",      CF_USE3|CF_USE2|CF_CHG1                           },      // Average Bytes                                      
{ "bg",        CF_USE3|CF_USE2|CF_CHG1                           },      // Borrow Generate                                    
{ "bgx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Borrow Generate Extended                           
{ "cg",        CF_USE3|CF_USE2|CF_CHG1                           },      // Carry Generate                                     
{ "clz",       CF_USE2|CF_CHG1                                   },      // Count Leading Zeros                                
{ "cntb",      CF_USE2|CF_CHG1                                   },      // Count Ones in Bytes                                
{ "eqv",       CF_USE3|CF_USE2|CF_CHG1                           },      // Equivalent                                         
{ "fsm",       CF_USE2|CF_CHG1                                   },      // Form Select Mask for Words                         
{ "fsmb",      CF_USE2|CF_CHG1                                   },      // Form Select Mask for Bytes                         
{ "fsmh",      CF_USE2|CF_CHG1                                   },      // Form Select Mask for Halfwords                     
{ "gb",        CF_USE2|CF_CHG1                                   },      // Gather Bits from Words                             
{ "gbb",       CF_USE2|CF_CHG1                                   },      // Gather Bits from Bytes                             
{ "gbh",       CF_USE2|CF_CHG1                                   },      // Gather Bits from Halfwords                         
{ "mpy",       CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply                                           
{ "mpya",      CF_USE4|CF_USE3|CF_USE2|CF_CHG1                   },      // Multiply and Add                                   
{ "mpyh",      CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply High                                      
{ "mpyhh",     CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply High High                                 
{ "mpyhha",    CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply High High and Add                         
{ "mpyhhau",   CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply High High Unsigned and Add                
{ "mpyhhu",    CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply High High Unsigned                        
{ "mpyi",      CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply Immediate                                 
{ "mpys",      CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply and Shift Right                           
{ "mpyu",      CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply Unsigned                                  
{ "mpyui",     CF_USE3|CF_USE2|CF_CHG1                           },      // Multiply Unsigned Immediate                        
{ "nand",      CF_USE3|CF_USE2|CF_CHG1                           },      // Nand                                               
{ "nor",       CF_USE3|CF_USE2|CF_CHG1                           },      // Nor                                                
{ "or",        CF_USE3|CF_USE2|CF_CHG1                           },      // Or                                                 
{ "orbi",      CF_USE3|CF_USE2|CF_CHG1                           },      // Or Byte Immediate                                  
{ "orc",       CF_USE3|CF_USE2|CF_CHG1                           },      // Or with Complement                                 
{ "orhi",      CF_USE3|CF_USE2|CF_CHG1                           },      // Or Halfword Immediate                              
{ "ori",       CF_USE3|CF_USE2|CF_CHG1                           },      // Or Word Immediate                                  
{ "orx",       CF_USE2|CF_CHG1                                   },      // Or Across                                          
{ "selb",      CF_USE4|CF_USE3|CF_USE2|CF_CHG1                   },      // Select Bits                                        
{ "sf",        CF_USE3|CF_USE2|CF_CHG1                           },      // Subtract from Word                                 
{ "sfh",       CF_USE3|CF_USE2|CF_CHG1                           },      // Subtract from Halfword                             
{ "sfhi",      CF_USE3|CF_USE2|CF_CHG1                           },      // Subtract from Halfword Immediate                   
{ "sfi",       CF_USE3|CF_USE2|CF_CHG1                           },      // Subtract from Word Immediate                       
{ "sfx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Subtract from Extended                             
{ "shufb",     CF_USE4|CF_USE3|CF_USE2|CF_CHG1                   },      // Shuffle Bytes                                      
{ "sumb",      CF_USE3|CF_USE2|CF_CHG1                           },      // Sum Bytes into Halfwords                           
{ "xor",       CF_USE3|CF_USE2|CF_CHG1                           },      // Exclusive Or                                       
{ "xorbi",     CF_USE3|CF_USE2|CF_CHG1                           },      // Exclusive Or Byte Immediate                        
{ "xorhi",     CF_USE3|CF_USE2|CF_CHG1                           },      // Exclusive Or Halfword Immediate                    
{ "xori",      CF_USE3|CF_USE2|CF_CHG1                           },      // Exclusive Or Word Immediate                        
{ "xsbh",      CF_USE2|CF_CHG1                                   },      // Extend Sign Byte to Halfword                       
{ "xshw",      CF_USE2|CF_CHG1                                   },      // Extend Sign Halfword to Word                       
{ "xswd",      CF_USE2|CF_CHG1                                   },      // Extend Sign Word to Doubleword                     

// Memory—Load/Store Instructions

{ "cbd",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Byte Insertion (d-form)      
{ "cbx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Byte Insertion (x-form)      
{ "cdd",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Doubleword Insertion (d-form)
{ "cdx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Doubleword Insertion (x-form)
{ "cgx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Carry Generate Extended                            
{ "chd",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Halfword Insertion (d-form)  
{ "chx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Halfword Insertion (x-form)  
{ "cwd",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Word Insertion (d-form)      
{ "cwx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Generate Controls for Word Insertion (x-form)      
{ "lqa",       CF_USE2|CF_CHG1                                   },      // Load Quadword (a-form)                             
{ "lqd",       CF_USE3|CF_USE2|CF_CHG1                           },      // Load Quadword (d-form)                             
{ "lqr",       CF_USE2|CF_CHG1                                   },      // Load Quadword Instruction Relative (a-form)        
{ "lqx",       CF_USE3|CF_USE2|CF_CHG1                           },      // Load Quadword (x-form)                             
{ "stqa",      CF_CHG2|CF_USE1                                   },      // Store Quadword (a-form)
{ "stqd",      CF_USE3|CF_USE2|CF_CHG1                           },      // Store Quadword (d-form)
{ "stqr",      CF_CHG2|CF_USE1                                   },      // Store Quadword Instruction Relative (a-form)
{ "stqx",      CF_USE3|CF_USE2|CF_CHG1                           },      // Store Quadword (x-form)

// Shift and Rotate Instructions

{ "rot",       CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Word
{ "roth",      CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Halfword
{ "rothi",     CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Halfword Immediate
{ "rothm",     CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Halfword
{ "rothmi",    CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Halfword Immediate
{ "roti",      CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Word Immediate
{ "rotm",      CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Word
{ "rotma",     CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Algebraic Word
{ "rotmah",    CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Algebraic Halfword
{ "rotmahi",   CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Algebraic Halfword Immediate
{ "rotmai",    CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Algebraic Word Immediate
{ "rotmi",     CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Word Immediate
{ "rotqbi",    CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Quadword by Bits
{ "rotqbii",   CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Quadword by Bits Immediate
{ "rotqby",    CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Quadword by Bytes
{ "rotqbybi",  CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Quadword by Bytes from Bit Shift Count
{ "rotqbyi",   CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate Quadword by Bytes Immediate
{ "rotqmbi",   CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Quadword by Bits
{ "rotqmbii",  CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Quadword by Bits Immediate
{ "rotqmby",   CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Quadword by Bytes
{ "rotqmbybi", CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Quadword Bytes from Bit Shift Count
{ "rotqmbyi",  CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Rotate and Mask Quadword by Bytes Immediate
{ "shl",       CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Word
{ "shlh",      CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Halfword
{ "shlhi",     CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Halfword Immediate
{ "shli",      CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Word Immediate
{ "shlqbi",    CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Quadword by Bits
{ "shlqbii",   CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Quadword by Bits Immediate
{ "shlqby",    CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Quadword by Bytes
{ "shlqbybi",  CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Quadword by Bytes from Bit Shift Count
{ "shlqbyi",   CF_USE3|CF_USE2|CF_CHG1|CF_SHFT                   },      // Shift Left Quadword by Bytes Immediate

};

#ifdef __BORLANDC__
#if sizeof(Instructions)/sizeof(Instructions[0]) != SPU_last
#error          No match:  sizeof(InstrNames) !!!
#endif
#endif
