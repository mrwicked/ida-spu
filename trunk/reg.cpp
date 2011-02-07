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
#include <srarea.hpp>
#include <diskio.hpp>
#include <loader.hpp>
#include <entry.hpp>
#include <fpro.h>
#include <ctype.h>

//--------------------------------------------------------------------------

static char *register_names[] =
{
    // 128 general-purpose registers (GPRs)

     "$0",     "$1",     "$2",     "$3",     "$4",     "$5",     "$6",     "$7",
     "$8",     "$9",    "$10",    "$11",    "$12",    "$13",    "$14",    "$15",
    "$16",    "$17",    "$18",    "$19",    "$20",    "$21",    "$22",    "$23",
    "$24",    "$25",    "$26",    "$27",    "$28",    "$29",    "$30",    "$31",
    "$32",    "$33",    "$34",    "$35",    "$36",    "$37",    "$38",    "$39",
    "$40",    "$41",    "$42",    "$43",    "$44",    "$45",    "$46",    "$47",
    "$48",    "$49",    "$50",    "$51",    "$52",    "$53",    "$54",    "$55",
    "$56",    "$57",    "$58",    "$59",    "$60",    "$61",    "$62",    "$63",
    "$64",    "$65",    "$66",    "$67",    "$68",    "$69",    "$70",    "$71",
    "$72",    "$73",    "$74",    "$75",    "$76",    "$77",    "$78",    "$79",
    "$80",    "$81",    "$82",    "$83",    "$84",    "$85",    "$86",    "$87",
    "$88",    "$89",    "$90",    "$91",    "$92",    "$93",    "$94",    "$95",
    "$96",    "$97",    "$98",    "$99",   "$100",   "$101",   "$102",   "$103",
   "$104",   "$105",   "$106",   "$107",   "$108",   "$109",   "$110",   "$111",
   "$112",   "$113",   "$114",   "$115",   "$116",   "$117",   "$118",   "$119",
   "$120",   "$121",   "$122",   "$123",   "$124",   "$125",   "$126",   "$127",

   // 128 channels registers

   "$ch0",   "$ch1",   "$ch2",   "$ch3",   "$ch4",   "$ch5",   "$ch6",   "$ch7",
   "$ch8",   "$ch9",  "$ch10",  "$ch11",  "$ch12",  "$ch13",  "$ch14",  "$ch15",
  "$ch16",  "$ch17",  "$ch18",  "$ch19",  "$ch20",  "$ch21",  "$ch22",  "$ch23",
  "$ch24",  "$ch25",  "$ch26",  "$ch27",  "$ch28",  "$ch29",  "$ch30",  "$ch31",
  "$ch32",  "$ch33",  "$ch34",  "$ch35",  "$ch36",  "$ch37",  "$ch38",  "$ch39",
  "$ch40",  "$ch41",  "$ch42",  "$ch43",  "$ch44",  "$ch45",  "$ch46",  "$ch47",
  "$ch48",  "$ch49",  "$ch50",  "$ch51",  "$ch52",  "$ch53",  "$ch54",  "$ch55",
  "$ch56",  "$ch57",  "$ch58",  "$ch59",  "$ch60",  "$ch61",  "$ch62",  "$ch63",
  "$ch64",  "$ch65",  "$ch66",  "$ch67",  "$ch68",  "$ch69",  "$ch70",  "$ch71",
  "$ch72",  "$ch73",  "$ch74",  "$ch75",  "$ch76",  "$ch77",  "$ch78",  "$ch79",
  "$ch80",  "$ch81",  "$ch82",  "$ch83",  "$ch84",  "$ch85",  "$ch86",  "$ch87",
  "$ch88",  "$ch89",  "$ch90",  "$ch91",  "$ch92",  "$ch93",  "$ch94",  "$ch95",
  "$ch96",  "$ch97",  "$ch98",  "$ch99", "$ch100", "$ch101", "$ch102", "$ch103",
 "$ch104", "$ch105", "$ch106", "$ch107", "$ch108", "$ch109", "$ch110", "$ch111",
 "$ch112", "$ch113", "$ch114", "$ch115", "$ch116", "$ch117", "$ch118", "$ch119",
 "$ch120", "$ch121", "$ch122", "$ch123", "$ch124", "$ch125", "$ch126", "$ch127",

 // Save-and-Restore Register 0 holds the address used by the Interrupt Return instruction

 "srr0",

 // 128 special-purpose registers (SPRs)

   "spr0",   "spr1",   "spr2",   "spr3",   "spr4",   "spr5",   "spr6",   "spr7",
   "spr8",   "spr9",  "spr10",  "spr11",  "spr12",  "spr13",  "spr14",  "spr15",
  "spr16",  "spr17",  "spr18",  "spr19",  "spr20",  "spr21",  "spr22",  "spr23",
  "spr24",  "spr25",  "spr26",  "spr27",  "spr28",  "spr29",  "spr30",  "spr31",
  "spr32",  "spr33",  "spr34",  "spr35",  "spr36",  "spr37",  "spr38",  "spr39",
  "spr40",  "spr41",  "spr42",  "spr43",  "spr44",  "spr45",  "spr46",  "spr47",
  "spr48",  "spr49",  "spr50",  "spr51",  "spr52",  "spr53",  "spr54",  "spr55",
  "spr56",  "spr57",  "spr58",  "spr59",  "spr60",  "spr61",  "spr62",  "spr63",
  "spr64",  "spr65",  "spr66",  "spr67",  "spr68",  "spr69",  "spr70",  "spr71",
  "spr72",  "spr73",  "spr74",  "spr75",  "spr76",  "spr77",  "spr78",  "spr79",
  "spr80",  "spr81",  "spr82",  "spr83",  "spr84",  "spr85",  "spr86",  "spr87",
  "spr88",  "spr89",  "spr90",  "spr91",  "spr92",  "spr93",  "spr94",  "spr95",
  "spr96",  "spr97",  "spr98",  "spr99", "spr100", "spr101", "spr102", "spr103",
 "spr104", "spr105", "spr106", "spr107", "spr108", "spr109", "spr110", "spr111",
 "spr112", "spr113", "spr114", "spr115", "spr116", "spr117", "spr118", "spr119",
 "spr120", "spr121", "spr122", "spr123", "spr124", "spr125", "spr126", "spr127",

 // virtual registers for code and data segments

 "rVcs",
 "rVds",
};

//-----------------------------------------------------------------------
//           APU assembler
//-----------------------------------------------------------------------

static asm_t spuasm =
{
  AS_COLON|AS_N2CHR|ASH_HEXF3|ASD_DECF0|ASB_BINF3|ASO_OCTF0|AS_ONEDUP,
  0,
  "GNU Assembler",
  0,
  NULL,         // header lines
  NULL,         // no bad instructions
  ".org",       // org
  ".exit",      // end

  ";",          // comment string
  '"',          // string delimiter
  '\'',         // char delimiter
  "\"'",        // special symbols in char and string constants

  ".ascii",     // ascii string directive
  ".byte",      // byte directive
  ".word",      // word directive
  ".dword",  	// double words
  ".qword",  	// qwords
  ".octa",      // oword  (16 bytes)
  NULL,         // float  (4 bytes)
  NULL,         // double (8 bytes)
  NULL,         // tbyte  (10/12 bytes)
  NULL,         // packed decimal real
  NULL,         // arrays (#h,#d,#v,#s(...)
  ".long %s",   // uninited arrays
  ".equ",       // equ
  NULL,         // 'seg' prefix (example: push seg seg001)
  NULL,         // Pointer to checkarg_preline() function.
  NULL,         // char *(*checkarg_atomprefix)(char *operand,void *res); // if !NULL, is called before each atom
  NULL,         // const char **checkarg_operations;
  NULL,         // translation to use in character and string constants.
  NULL,         // current IP (instruction pointer)
  NULL,         // func_header
  NULL,         // func_footer
  NULL,         // "public" name keyword
  NULL,         // "weak"   name keyword
  NULL,         // "extrn"  name keyword
  NULL,         // "comm" (communal variable)
  NULL,         // get_type_name
  NULL,         // "align" keyword
  '(', ')',	// lbrace, rbrace
  NULL,         // mod
  "&",          // and
  "|",          // or
  "^",          // xor
  "~",          // not
  "<<",         // shl
  ">>",         // shr
  NULL,         // sizeof
};

static asm_t *asms[] = { &spuasm, NULL };

//--------------------------------------------------------------------------
static ioport_t *ports = NULL;
static size_t numports = 0;
char device[MAXSTR] = "";
static const char cfgname[] = "spu.cfg";

netnode helper;
proctype_t ptype = SONY_PS3;
ushort idpflags = IAS_FRGPR;
uint32 lslr_size = DEFAULT_LSLR;

static proctype_t ptypes[] =
{
  SONY_PS3
};

const ioport_t *find_channel(int channel)
{
  return find_ioport(ports, numports, ea_t(channel));
}

static const char *spu_callback(const ioport_t *ports, size_t numports, const char *line);
#define callback spu_callback
#include "../iocommon.cpp"

static void load_symbols(void)
{
  free_ioports(ports, numports);
  ports = read_ioports(&numports, cfgname, device, sizeof(device), callback);
}

//--------------------------------------------------------------------------
static const char *spu_callback(const ioport_t *ports, size_t numports, const char *line)
{
  char word[MAXSTR];
  int value, len;

  if ( sscanf(line, "%[^=] = %" FMT_EA "i", word, &value) == 2 )
  {
    if ( strcmp(word, "LSLR") == 0 )
    {
      lslr_size = value;
      return NULL;
    }
  }
  /*if ( sscanf(line, "channel %s %i%n", word, &value, &len) == 2 )
  {
    const char *ptr = &line[len];
    ptr = skipSpaces(ptr);

    return NULL;
  }*/
  return standard_callback(ports, numports, line);
}

//--------------------------------------------------------------------------
static void choose_device(TView *[],int)
{
  if ( choose_ioport_device(cfgname, device, sizeof(device), NULL) )
  {
    load_symbols();
  }
}

//--------------------------------------------------------------------------
static const char *set_idp_options(const char *keyword,int value_type,const void *value)
{
  if ( keyword == NULL )
  {
    static const char form[] =
"HELP\n"
"IBM Cell SPU specific options _\n"
" ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯\n"
"\n"
" Friendly GPR definitions\n"
"\n"
"       If this option is on, IDA will use friendly\n"
"       GPR register names like $LR and $SP.\n"
"\n"
"ENDHELP\n"
"IBM Cell SPU specific options\n"
"\n"
" <~F~riendly GPR definitions:C>>\n"
"\n"
" <~C~hoose device name:B:0:::>\n"
"\n"
"\n";
    AskUsingForm_c(form, &idpflags, choose_device);
    return IDPOPT_OK;
  }
  else
  {
    if ( strcmp(keyword, "IBMSPU_FRGPR") == 0 )
    {
      setflag(idpflags,IAS_FRGPR,*(int*)value);
      return IDPOPT_OK;
    }
    if ( value_type != IDPOPT_BIT ) return IDPOPT_BADTYPE;
    return IDPOPT_BADKEY;
  }
}

//--------------------------------------------------------------------------
inline void set_device_name(const char *dev)
{
  if ( dev != NULL )
    qstrncpy(device, dev, sizeof(device));
}

//----------------------------------------------------------------------
static int notify(processor_t::idp_notify msgid, ...)
{
	va_list va;
	va_start(va, msgid);

	// A well behaving processor module should call invoke_callbacks()
	// in his notify() function. If this function returns 0, then
	// the processor module should process the notification itself
	// Otherwise the code should be returned to the caller:

	int code = invoke_callbacks(HT_IDP, msgid, va);
	if ( code ) return code;

	switch(msgid)
	{
        case processor_t::init:
          helper.create("$ spu");
          {
            char buf[MAXSTR];
            if ( helper.supval(0, buf, sizeof(buf)) > 0 )
              set_device_name(buf);
          }
          inf.mf = 1; // MSB first
          break;

        case processor_t::term:
          free_ioports(ports, numports);
          break;

        case processor_t::newfile:   // new file loaded
          break;

        case processor_t::oldfile:   // old file loaded
          {
            char buf[MAXSTR];
            if ( helper.supval(-1, buf, sizeof(buf)) > 0 )
              set_device_name(buf, IORESP_NONE);
          }
          break;

        case processor_t::newasm:    // new assembler type
          break;

      case processor_t::closebase:
      case processor_t::savebase:
        helper.altset(-1, idpflags);
        helper.supset(0,  device);
        break;

      case processor_t::newprc:    // new processor type
        {
          ptype = ptypes[va_arg(va, int)];
          switch ( ptype )
          {
            case SONY_PS3:
              break;
            default:
              error("interr: setprc");
              break;
          }
          device[0] = '\0';
          load_symbols();
        }
        break;
	case processor_t::is_ret_insn:
        {
            bool strict = va_arg(va, bool);
            switch (cmd.itype)
            {
                case SPU_bi:
                      return (cmd.Op1.reg == 0 ? 2 : 1);
                case SPU_iret:
                case SPU_stop:
                case SPU_stopd:
                      return strict ? 1 : 2;
            }
        }
        break;
	default:
             break;
	}
	va_end(va);

	return(1);
}

//--------------------------------------------------------------------------
static uchar retcode_0[] = { 0x35, 0x00, 0x00, 0x00 };  // bi

static bytes_t retcodes[] =
{
 { sizeof(retcode_0), retcode_0 },
 { 0, NULL }
};

//-----------------------------------------------------------------------
static char *shnames[] = { "SPU", NULL };
static char *lnames[] = {
  "IBM Cell Synergistic Processor Unit (SPU)",
  NULL
};

//-----------------------------------------------------------------------
//      Processor Definition
//-----------------------------------------------------------------------
processor_t LPH =
{
  IDP_INTERFACE_VERSION,        // version
  -1,                           // id
  PRN_HEX|PR_RNAMESOK,
  8,                    // 16 bits in a byte for code segments
  8,                    // 8 bits in a byte for other segments

  shnames,
  lnames,

  asms,

  notify,

  header,
  footer,

  segstart,
  segend,

  NULL,                 // generate "assume" directives

  ana,                  // analyze instruction
  emu,                  // emulate instruction

  out,                  // generate text representation of instruction
  outop,                // generate ...                    operand
  intel_data,           // generate ...                    data directive
  NULL,                 // compare operands
  NULL,                 // can have type

  qnumber(register_names), // Number of registers
  register_names,       // Register names
  NULL,                 // get abstract register

  0,                    // Number of register files
  NULL,                 // Register file names
  NULL,                 // Register descriptions
  NULL,                 // Pointer to CPU registers

  rVcs,                 // first
  rVds,                 // last
  0,                    // size of a segment register
  rVcs, rVds,

  NULL,                 // No known code start sequences
  retcodes,             // Array of 'return' instruction opcodes

  SPU_null,
  SPU_last,
  Instructions,

  NULL,                 // int  (*is_far_jump)(int icode);
  NULL,                 // Translation function for offsets
  0,                    // int tbyte_size;  -- doesn't exist
  NULL,                 // int (*realcvt)(void *m, ushort *e, ushort swt);
  { 0, },               // char real_width[4];
                        // number of symbols after decimal point
                        // 2byte float (0-does not exist)
                        // normal float
                        // normal double
                        // long double
  NULL,                 // int (*is_switch)(switch_info_t *si);
  NULL,                 // int32 (*gen_map_file)(FILE *fp);
  NULL,                 // ea_t (*extract_address)(ea_t ea,const char *string,int x);
  NULL,                 // int (*is_sp_based)(op_t &x); -- always, so leave it NULL
  NULL,                 // int (*create_func_frame)(func_t *pfn);
  NULL,                 // int (*get_frame_retsize(func_t *pfn)
  NULL,                 // void (*gen_stkvar_def)(char *buf,const member_t *mptr,int32 v);
  gen_spcdef,           // Generate text representation of an item in a special segment
  SPU_bi,               // Icode of return instruction. It is ok to give any of possible return instructions
  set_idp_options,      // const char *(*set_idp_options)(const char *keyword,int value_type,const void *value);
  is_align_insn,        // int (*is_align_insn)(ea_t ea);
  NULL,                 // mvm_t *mvm;
};
