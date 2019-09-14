PROGRAM pascalP6interpreter(input,output);

  LABEL 1;

  CONST

      {

      Program object sizes and characteristics, sync with pint. These define
      the machine specific characteristics of the target.

      This configuration is for a 32 bit machine as follows:

      integer               32  bits
      real                  64  bits ( this is also known as 'Double' )
      char                  8   bits ( even though ASCII is a 7-bit code )
      boolean               8   bits ( 'PACKED' might make this 1 bit )
      set                   256 bits ( maximum, not the typical )
      pointers              32  bits
         ( data addresses, but not necessarily code addresses )
      marks                 20  bytes
      File logical number   8   bits

      Both endian types are supported. There is no alignment needed, but you
      may wish to use alignment to tune the runtime speed.

      The machine characteristics dependent on byte accessable machines. This
      table is all you should need to adapt to any byte addressable machine.

      }

      wordsize    =        4;  { size of word, the default integer }
      wordsizeM1   =       3;
      intal       =        4;  { alignment of integer }
      dublsize    =        8;  { size of double, the default real }
      dublsizeM1  =        7;
      realal      =        4;  { alignment of real }
      charsize    =        1;  { size of char }
      charal      =        1;  { alignment of char }
      boolsize    =        1;  { size of boolean }
      boolal      =        1;  { alignment of boolean }
      adrsize     =        4;  { size of data address }
      adrsizeM1   =        3;
      adral       =        4;  { alignment of data address }
      labsize     =        2;  { size of codeAddr (different from data address) }
      setsize     =       32;  { size of set }
      setsizeM1   =       31;
      setal       =        1;  { alignment of set }
      filesize    =        1;  { required runtime space for file (lfn) }
      fileidsize  =        1;  { size of the lfn only }
      stackal     =        4;  { alignment of stack }
      stackelsize =        4;  { stack element size }
      maxsize     =       32;  { this is the largest operand on the stack }
      { Heap alignment should be either the natural word alignment of the
        machine, or the largest object needing alignment that will be allocated.
        It can also be used to enforce minimum block allocation policy. }
      heapal      =        4;  { alignment for each heap block }
      sethigh     =      255;  { Sets are 256 values }
      setlow      =        0;
      ordmaxchar  =      255;  { Characters are 8 bit ISO/IEC 8859-1 }
      ordminchar  =        0;
      maxresult   = dublsize;  { maximum size of function result }
      marksize    =       20;  { maxresult+3*adrsize }
      { Value of nil is 1 because this allows checks for pointers that were
        initialized, which would be zero (since we clear all space to zero).
        In the new unified code/data space scheme, 0 and 1 are always invalid
        addresses, since the startup code is at least that long. }
      nilval      =        1;  { value of 'nil' }

      { end of pcom and pint common parameters }

      { internal constants }

      t8          = 256;          { 2**8      }
      t8m1        = 255;          { 2**8  - 1 }
      t16         = 65536;        { 2**16     }
      t16m1       = 65535;        { 2**16 - 1 }
      t29         = 536870912;    { 2**29     }
      t29m1       = 536870911;    { 2**29 - 1 }
      lo_enc      = -t29;
      hi_enc      = t29m1;        { 2**29 - 1 }
      t30         = 1073741824;   { 2**30     }
      { code space will be 64K, because the compiler will fit there }
      maxCS       = t16m1;        { 2**16 - 1 } { currently can't be bigger }
      { data space will be 128K, but it can be made much larger if needed }
      maxDS       = 262143;       { 2**18 - 1 } { make this as big as you want }
      maxdigh     = 5;    { number of digits in hex representation of maxDS }
      maxdigd     = 6;    { number of digits in dec representation of maxDS }

      entryPoint  = 12{2+9+1};{ beginning of code, offset by program preamble:
                                  2: mst  (op + p)
                                  9: cal  (op + wordsize + adrsize)
                                  1: stop (op)
                              }
      { the header files have a logical no. followed by a buffer var }
      inputoff    = 24;      { 'input'  file address, marksize + 4 }
      outputoff   = 26;      { 'output' file address, marksize + 6 }

      { assigned logical channels for header files }
      inputfn     = 1;        { 'input' file no. }
      outputfn    = 2;        { 'output' file no. }

      { Mark element offsets

        Mark format is:

        0:  Function return value, 64 bits, enables a full real result.
        8:  Static link.
        12: Dynamic link.
        16: Return address

      }
      {markfv     = 0;}         { function value }
      marksl     = 8;         { static link }
      markdl     = 12;        { dynamic link }
      markra     = 16;        { return address }

      stringlgth  = {1000;}  { longest string length we can buffer } 200;
      maxsp       = 36;      { number of predefined procedures/functions }
      maxfil      = 100;     { maximum number of general (temp) files }
      maxalfa     = 4;       { maximum number of characters in alfa type }
      bralen      = labsize; { length of case table entry ( = sizeof(codeAddr)) }

      { debugging flags: turn these on for various dumps and traces }

      dotrasmb    = false;    { trace the assembly }
      dodmpsto    = false;    { dump storage area specs }
      dotrcins    = false;    { trace instruction executions }
      minpctrc    = 0;     { only trace ex w/ pc > this }
      doTraceSource = false;  { trace source line executions }
      startdebugat  = 1647;   { source line no. to begin exec trace }
      dodmpspc    = false;    { dump heap space after execution }
      dynaStats   = false;    { show dynamic frequency (time) of each p-code }
      statStats   = false;    { show static frequency (space)  "   "    "    }
      constPoolOpt = false;   { re-use constant pool entries & brag about it }
      monStackHeap = false;   { test for stack/heap collision }

      { version numbers }

      majorver   = 1; { major version number }
      minorver   = 0; { minor version number }

  TYPE            { this is (almost) the same declaration as in pcom }
    opcodes   = (invd, { 0 should always be an invalid opcode }
                 logb, logw, logd, logm, lolb, lolw, lold, lolm,
                 loib, loiw, loid, loim, locw, locr, locs, loca,
                 stgb, stgw, stgd, stgm, stlb, stlw, stld, stlm,
                 stib, stiw, stid, stim, iand, ior,  flt2, inot,
                 addi, subi, muli, divi, negi, absi, sqri, modi,
                 addr, subr, mulr, divr, negr, absr, sqrr, flt,
                 trc,  rnd,  incw, decw, icgw, iclw, dcgw, dclw,
                 equ,  neq,  leq,  geq,  les,  gtr,  inn,  dif,
                 equm, neqm, leqm, geqm, lesm, gtrm, sst1, sst2,
                 tjp,  ujc,  xjp,  sjp,  cas,  chk0, chk1, chk2,
                 lag,  lal,  lpa,  lip,  ixa,  ixca,  iodd, fjp,
                 eofp, sgs,  rgs,  bra,  ipj,  dmp,  dup,  swp,
                 cal,  cali, csp,  mst,  ent,  ret0, ret1, ret2,
                 mov,  copy, fbv,  fvb,  efb,  pack, unpk, pop0,
                 cmln, asgn, srcl, fatl, stop,
                 { up to here, it's the same; now here's a few opcodes
                   only used in the assembler/interpreter: }
                 locq, loc0, loc1,
                 loib0, loiw0, loid0, loim0,
                 stib0, stiw0, stid0, stim0,
                 lolw0, stlw0, { same as lolw/stlw except w/ lex=0 }
                 { that's all folks }
                 lastop);

    opset     = SET OF opcodes;

        byte        = PACKED 0..t8m1;  { the familiar 8-bit unsigned datum }
        GNU_Int     = Integer attribute (size = 32);
        lvltyp      = byte;     { procedure/function level }
        dataAddr    = 0..maxDS;
        codeAddr    = PACKED 0..maxCS; { ShortCard, effectively }
        encodable   = lo_enc..hi_enc;
        offset      = -maxDS..maxDS; { can't be full integer range; }
                                { otherwise, it wouldn't be encodable }
        jmpoff      = PACKED -32768..32767; { for branches }
        beta        = PACKED ARRAY [1..36] OF char; (*error message*)
        settype     = SET OF setlow..sethigh; { for set constructers }
        alfa        = PACKED ARRAY [1..maxalfa] OF char;
        bytfil      = FILE OF byte; { untyped file of bytes }
        fileno      = 0..maxfil; { logical file number }
        { strong type-smashing "trix" records }
        trixInt     = RECORD CASE Boolean OF
                        true:  (i: GNU_Int);
                        false: (b: PACKED ARRAY [0..wordsizeM1] OF byte);
                      END;
        trixReal    = RECORD CASE Boolean OF
                        true:  (r: Double); { double-precision (64-bit) real }
                        false: (b: PACKED ARRAY [0..dublsizeM1] OF byte);
                      END;
        trixSet     = RECORD CASE Boolean OF
                        true:  (s: settype);
                        false: (b: PACKED ARRAY [0..setsizeM1] OF byte);
                      END;
        trixCAddr   = RECORD CASE Boolean OF
                        true:  (a: codeAddr);
                        false: (b: PACKED ARRAY [0..1] OF byte);
                      END;
        trixDAddr   = RECORD CASE Boolean OF
                        true:  (a: dataAddr); { can handle ds, cs, or labels }
                        false: (b: PACKED ARRAY [0..adrsizeM1] OF byte);
                      END;
        trixOff     = RECORD CASE Boolean OF
                        true:  (j: jmpoff);
                        false: (b: PACKED ARRAY [0..1] OF byte);
                      END;
        p2word      = ^integer;
        str255      = string(255);

  VAR   ch          : char;       { reads on prd generally uses this }
        pc          : codeAddr;   { index into code space: instruction pointer }
        pctop       : codeAddr;   { size of code in code space }
        op, op1, op2: opcodes;
        p           : lvltyp;  (* opcode & (optional) static level field *)
        q,q1        : dataAddr; { (optional) addresses into data space }
        prevSL      : codeAddr;
        str         : str255;
        constPool   : dataAddr;  (* pointer to next free constant position *)
        fp,sp,np    : dataAddr;  (* addressing registers *)
        (*fp  points to beginning of the current stack mark (stack "frame")
          sp  points to top of the stack
          np  points to the bottom of the heap ( 'dynamic' variables ) *)

        prd         : text;

        mn          : ARRAY [opcodes] OF alfa;
        stdp        : ARRAY [0..maxsp] OF alfa; (*standard functions and procedures*)
        sourceLine  : integer; { current source line executing }

        { general (temp) textfile holders }
        filtable    : ARRAY [3..maxfil] OF text;
        { general (temp) binary file holders }
        bfiltable   : ARRAY [3..maxfil] OF bytfil;
        { file state holding }
        filstate    : ARRAY [3..maxfil] OF (fsentinel, fclosed, fread, fwrite);
        { file buffer full status }
        filbuff     : ARRAY [3..maxfil] OF Boolean;

        (*locally used for interpreting one instruction*)
        ad          : dataAddr;
        b           : Boolean;
        i,j,i1,i2   : integer;
        i3, i4      : integer;
        r1, r2      : Double;
        s1, s2      : settype;
        a1          : dataAddr;
        fn          : fileno;
        oldpc,ca    : codeAddr;
        dystats: ARRAY [opcodes] OF RECORD inx: opcodes; tot: integer END;
        total,minStackHeap: integer;
        searchByte: byte;
        {debug       : Boolean;}
        opNeedsSize,
        opNeedsLevel,
        opNeedsOffset: opset;
        data        : PACKED ARRAY [dataAddr] OF byte; { data space, 2**24 }
        code        : PACKED ARRAY [codeAddr] OF byte; { code space, 2**18 }

  (*--------------------------------------------------------------------*)

  { Accessor functions

    These translate store variables to internal, and convert to and from store
    BAM formats.

    The accessors are fairly machine independent, they rely here on the machine
    being byte addressable. The endian format is inherent to the machine.

    The exception are the get/put int16/32/64/128-bit routines, which are
    dependent on the endian mode of the machine.

  }

  FUNCTION getByte(a: dataAddr): byte;{ attribute (inline); }
  BEGIN
    getByte := data[a];
  END;

  PROCEDURE putByte(a: dataAddr; b: byte);{ attribute (inline); }
  BEGIN
    data[a] := b
  END;

  PROCEDURE loadWord(a: dataAddr); { optimized version of }
  BEGIN                            { pushWord(getWord(a)) }
    data[sp  ] := data[a  ];
    data[sp+1] := data[a+1];
    data[sp+2] := data[a+2];
    data[sp+3] := data[a+3];
    sp := sp + 4;
  END;

  FUNCTION getWord(a: dataAddr): GNU_Int;{ attribute (inline); }
    VAR   r: trixInt; { guaranteed to be 32 bits }
{         i: integer;}
  BEGIN
    { put check in here to make sure 'a' is 4-byte aligned }
{$ifdef __GPC__}
{ warning: this assumes wordsize = 4 }
    r.b[0] := data[a  ];
    r.b[1] := data[a+1];
    r.b[2] := data[a+2];
    r.b[3] := data[a+3];
{$else}
    FOR i := 0 TO wordsizeM1 DO
      r.b[i] := data[a+i];
{$endif}
    getWord := r.i;
  END;

  PROCEDURE putWord(a: dataAddr; x: GNU_Int);{ attribute (inline); }
    VAR   r: trixInt;
  BEGIN
    r.i := x;
{$ifdef __GPC__}
{ warning: this assumes wordsize = 4 }
    data[a  ] := r.b[0];
    data[a+1] := r.b[1];
    data[a+2] := r.b[2];
    data[a+3] := r.b[3];
{$else}
    FOR i := 0 TO wordsizeM1 DO
      data[a+i] := r.b[i];
{$endif}
  END;

  PROCEDURE incWord(a: dataAddr; i: GNU_Int);{ attribute (inline); }
  BEGIN
    putWord(a, getWord(a) + i)
  END;
  
  PROCEDURE decWord(a: dataAddr; i: GNU_Int);{ attribute (inline); }
  BEGIN
    putWord(a, getWord(a) - i)
  END;
  
  { Since data addresses are 32-bit, they are loaded and stored using the  }
  { 'getWord' and 'putWord' routines. But if they were to become 64-bits, }
  { then the 'getDubl' and 'putDubl' routines would be used. }
  FUNCTION getDubl(a: dataAddr): Double;{ attribute (inline); }
    VAR   r: trixReal;
          i: integer;
  BEGIN
    FOR i := 0 TO dublsizeM1 DO
      r.b[i] := data[a+i];
    getDubl := r.r
  END;

  PROCEDURE putDubl(a: dataAddr; f: Double);{ attribute (inline); }
    VAR   r: trixReal;
          i: integer;
  BEGIN
    r.r := f;
    FOR i := 0 TO dublsizeM1 DO
      data[a+i] := r.b[i]
  END;

  PROCEDURE getSet(a: dataAddr; VAR s: settype; size: integer);
    VAR   r: trixSet;
          i: integer;
  BEGIN
    FOR i := 0 TO size - 1 DO
      r.b[i] := data[a+i];
    FOR i := size TO setsize-1 DO
      r.b[i] := 0;
    s := r.s
  END;

  PROCEDURE putSet(a: dataAddr; s: settype; size: integer);
    VAR   r: trixSet;
          i: integer;
  BEGIN
    r.s := s;
    searchByte := r.b[0];
    FOR i := 0 TO size - 1 DO
      data[a+i] := r.b[i]
  END;

  { Swap word on TOS with the operand underneath (the size of which is 'lgth') }

  PROCEDURE swapTop(lgth: integer);
    VAR   i,j:  integer;
  BEGIN
    { get the word on TOS }
    j := getWord(sp-adrsize);
    { move up 2nd thing on stack }
    FOR i := lgth-1 DOWNTO 0 DO
      data[sp-lgth+i] := data[sp-lgth-adrsize+i];
    { put former TOS word underneath it }
    putWord(sp-lgth-adrsize, j);
  END;

  { that does for fetching from and storing to data space }
  { now we need some similar ones for code space }

  PROCEDURE putIntRev(x: GNU_Int);
  { This switches the endian-ness: puts the most significant bytes }
  { in the lower addresses. Needed for the NS32000 encoding }
    VAR   r: trixInt;
  BEGIN
    r.i := x;
{ warning: this assumes wordsize = 4 }
    code[pc  ] := r.b[3];
    code[pc+1] := r.b[2];
    code[pc+2] := r.b[1];
    code[pc+3] := r.b[0];
    pc := pc + wordsize
  END;

  FUNCTION getCodeInt(a: codeAddr): GNU_Int;
    VAR   r: trixInt;
{          i: integer;}
  BEGIN
{$ifdef __GPC__}
{ warning: this assumes wordsize = 4 }
    r.b[0] := code[a  ];
    r.b[1] := code[a+1];
    r.b[2] := code[a+2];
    r.b[3] := code[a+3];
{$else}
    FOR i := 0 TO wordsizeM1 DO
      r.b[i] := code[a+i];
{$endif}
    getCodeInt := r.i;
  END;

  PROCEDURE putCodeInt(x: GNU_Int);
    VAR   r: trixInt;
  BEGIN
    r.i := x;
{$ifdef __GPC__}
{ warning: this assumes wordsize = 4 }
    code[pc  ] := r.b[0];
    code[pc+1] := r.b[1];
    code[pc+2] := r.b[2];
    code[pc+3] := r.b[3];
{$else}
    FOR i := 0 TO wordsizeM1 DO
      code[pc+i] := r.b[i];
{$endif}
    pc := pc + wordsize
  END;

  FUNCTION getCode2byte(a: codeAddr): jmpoff;{ attribute (inline); }
    VAR   r: trixOff;
  BEGIN
    r.b[0] := code[a  ];
    r.b[1] := code[a+1];
    getCode2byte := r.j;
  END;

  PROCEDURE putCode2byte(a: codeAddr; i: jmpoff);
    VAR   r: trixOff;
  BEGIN
    r.j := i;
    code[a  ] := r.b[0];
    code[a+1] := r.b[1];
  END;

  FUNCTION getCodeAddr(a: codeAddr): codeAddr;
    VAR   r: trixCAddr;
  BEGIN
    r.b[0] := code[a  ];
    r.b[1] := code[a+1];
    getCodeAddr := r.a
  END;

  PROCEDURE putCodeAddr(a: codeAddr; ad: codeAddr);
    VAR   r: trixCAddr;
  BEGIN
    r.a := ad;
    code[a  ] := r.b[0];
    code[a+1] := r.b[1];
  END;

  { end of accessor functions }

  (*--------------------------------------------------------------------*)

  {   Routines for pushing and popping words and doubles.  }

  PROCEDURE popWord(VAR i: integer);{ attribute (inline); }
  BEGIN sp := sp - wordsize; i := getWord(sp) END;
  
  PROCEDURE pushWord(i: integer);{ attribute (inline); }
  BEGIN
    putWord(sp, i);
    sp := sp + wordsize;
    IF monStackHeap THEN
      IF np - sp < minStackHeap THEN
        minStackHeap := np - sp
  END;
  
  PROCEDURE popDubl(VAR r: Double);
  BEGIN sp := sp - dublsize; r := getDubl(sp) END;
  
  PROCEDURE pushDubl(r: Double);
  BEGIN putDubl(sp, r); sp := sp + dublsize;
    IF monStackHeap THEN
      IF np - sp < minStackHeap THEN
        minStackHeap := np - sp
  END;

  PROCEDURE popSet(VAR s: settype; len: integer);
    VAR   resid: 0..stackal;
  BEGIN
    resid := bytecard(len) MOD stackal;
    IF resid > 0 THEN
      sp := sp - (wordsize - resid); { pop any padded zeros }
    sp := sp - len;
    getSet(sp, s, len); { s := mem[sp] for len, and then padded w/ zeros }
  END;
  
  PROCEDURE alignstack;
    VAR   resid: 0..stackal;
  BEGIN
    resid := sp MOD stackal;
    IF resid > 0 THEN { mis-aligned sp }
      REPEAT
        data[sp] := 0; { pad stack with zeros }
        sp := sp + 1;
        resid := resid + 1
      UNTIL resid = stackal; { all better }
  END;
  
  PROCEDURE pushSet(s: settype; VAR len: integer);
  BEGIN
    putSet(sp, s, len); { mem[sp] := s for len; }
    sp := sp + len;
    { what if the stack is now mis-aligned? }
    alignstack;
    IF monStackHeap THEN
      IF np - sp < minStackHeap THEN
        minStackHeap := np - sp
  END;
  
  PROCEDURE writeHex(v: integer; { value } f: integer { field });
    VAR   i: integer;
          d,p: 0..maxint;
  BEGIN
    p := 1;
    FOR i := 1 TO f-1 DO
      p := p*16;
    WHILE p > 0 DO
      BEGIN
        d := v DIV p MOD 16; { extract digit }
        IF d < 10 THEN write(chr(d+ord('0')))
                  ELSE write(chr(d-10+ord('A')));
        p := p DIV 16
      END
  END;

  { align dataAddr, downwards }

  PROCEDURE alignDown(algn: integer; VAR flc: dataAddr);
  BEGIN { used to keep the heap aligned }
    IF algn <> 1 THEN
      flc := flc - (flc MOD algn);
  END (*alignDown*);

  (*--------------------------------------------------------------------*)

  PROCEDURE load;
    CONST maxlabel = 5000;           { hopefully, that's enough }
    TYPE  labelrg  = 0..maxlabel;         (*label range*)
          labelVal = codeAddr;
          trixLab  = RECORD CASE Boolean OF
                       true:  (a: labelVal); { specifically for labels }
                       false: (b: PACKED ARRAY [0..1] OF byte);
                     END;
    VAR   len,iline: integer; { line number of intermediate file }
          lastXjpPc,lastSjpPc: codeAddr;
          labeltab: ARRAY [labelrg] OF RECORD
                                         val: labelVal;
                                         defined,equate: Boolean
                                       END;
          stats: ARRAY [opcodes] OF RECORD inx: opcodes; tot: integer END;
          op1,op2: opcodes;
saveC1,saveC2,saveC3: integer;
          freqoff     : ARRAY [-16..15] OF integer;
          offStats,
          saveoffStats: Boolean;

    FUNCTION getLabel(a: codeAddr): labelVal;
      VAR   r: trixLab;
    BEGIN
      r.b[0] := code[a  ];
      r.b[1] := code[a+1];
      getLabel := r.a
    END;

    PROCEDURE putLabel(finx: codeAddr; ad: labelVal);
      VAR   r: trixLab;
    BEGIN
      r.a := ad;
      code[finx  ] := r.b[0];
      code[finx+1] := r.b[1];
    END;

    PROCEDURE init;
      VAR   i: integer;
            op: opcodes;
    BEGIN { see my comments starting at about line 300 }
      
      mn[logb]:='logb'; mn[logw]:='logw'; mn[logd]:='logd'; mn[logm]:='logm';
      mn[lolb]:='lolb'; mn[lolw]:='lolw'; mn[lold]:='lold'; mn[lolm]:='lolm';
      mn[loib]:='loib'; mn[loiw]:='loiw'; mn[loid]:='loid'; mn[loim]:='loim';
      mn[locw]:='locw';
      
      mn[stgb]:='stgb'; mn[stgw]:='stgw'; mn[stgd]:='stgd'; mn[stgm]:='stgm';
      mn[stlb]:='stlb'; mn[stlw]:='stlw'; mn[stld]:='stld'; mn[stlm]:='stlm';
      mn[stib]:='stib'; mn[stiw]:='stiw'; mn[stid]:='stid'; mn[stim]:='stim';
      mn[iand]:='iand'; mn[ior ]:='ior '; mn[flt2]:='flt2'; mn[inot]:='inot';
      
      mn[addi]:='addi'; mn[subi]:='subi'; mn[muli]:='muli'; mn[divi]:='divi';
      mn[negi]:='negi'; mn[absi]:='absi'; mn[sqri]:='sqri'; mn[modi]:='modi';
      mn[addr]:='addr'; mn[subr]:='subr'; mn[mulr]:='mulr'; mn[divr]:='divr';
      mn[negr]:='negr'; mn[absr]:='absr'; mn[sqrr]:='sqrr'; mn[flt ]:='flt ';
      
      mn[trc ]:='trc '; mn[rnd ]:='rnd '; mn[incw]:='incw'; mn[decw]:='decw';
      mn[icgw]:='icgw'; mn[iclw]:='iclw'; mn[dcgw]:='dcgw'; mn[dclw]:='dclw';
      mn[equ ]:='equ '; mn[neq ]:='neq '; mn[leq ]:='leq '; mn[geq ]:='geq ';
      mn[les ]:='les '; mn[gtr ]:='gtr '; mn[inn ]:='inn '; mn[fjp ]:='fjp ';
      mn[equm]:='equm'; mn[neqm]:='neqm'; mn[leqm]:='leqm'; mn[geqm]:='geqm';
      mn[lesm]:='lesm'; mn[gtrm]:='gtrm'; mn[sst1]:='sst1'; mn[sst2]:='sst2';
      
      mn[tjp ]:='tjp '; mn[ujc ]:='ujc '; mn[xjp ]:='xjp '; mn[sjp ]:='sjp ';
      mn[cas ]:='cas '; mn[chk0]:='chk0'; mn[chk1]:='chk1'; mn[chk2]:='chk2';
      mn[lag ]:='lag '; mn[lal ]:='lal '; mn[loca]:='loca'; mn[lpa ]:='lpa ';
      mn[lip ]:='lip '; mn[ixa ]:='ixa '; mn[ixca]:='ixca'; mn[iodd]:='iodd';
      
      mn[eofp]:='eofp'; mn[dif ]:='dif '; mn[sgs ]:='sgs '; mn[rgs ]:='rgs ';
      mn[bra ]:='bra '; mn[ipj ]:='ipj '; mn[dmp ]:='dmp '; mn[dup ]:='dup ';
      mn[swp ]:='swp '; mn[cal ]:='cal '; mn[cali]:='cali'; mn[csp ]:='csp ';
      mn[mst ]:='mst '; mn[ent ]:='ent '; mn[ret0]:='ret0'; mn[ret1]:='ret1';
      
      mn[ret2]:='ret2'; mn[mov ]:='mov '; mn[copy]:='copy'; mn[fbv ]:='fbv ';
      mn[fvb ]:='fvb '; mn[efb ]:='efb '; mn[pack]:='pack'; mn[unpk]:='unpk';
      mn[pop0]:='pop0'; mn[cmln]:='cmln'; mn[asgn]:='asgn'; mn[locr]:='locr';
      mn[locs]:='locs'; mn[srcl]:='srcl'; mn[fatl]:='fatl'; mn[stop]:='stop';
      
      mn[locq]:='locq'; mn[loc0]:='loc0'; mn[loc1]:='loc1';
      mn[loib0]:='lib0';mn[loiw0]:='liw0';mn[loid0]:='lid0';mn[loim0]:='lim0';
      mn[stib0]:='sib0';mn[stiw0]:='siw0';mn[stid0]:='sid0';mn[stim0]:='sim0';

      opNeedsSize := [logm,lolm,loim,stgm,stlm,stim,iand,ior,equ,neq,
                      leq,geq,les,gtr,inn,equm,neqm,leqm,geqm,lesm,gtrm,
                      dif,sgs,rgs,ent,mov,locs];
      opNeedsLevel := [lolb, lolw, lold, lolm, stlb, stlw, stld, stlm,
                       iclw, dclw, lal, lpa, lip, ipj, mst];
      opNeedsOffset := [logb, logw, logd, logm, lolb, lolw, lold, lolm,
                        loib, loiw, loid, loim, locw, locq, stgb, stgw,
                        stgd, stgm, stlb, stlw, stld, stlm, stib, stiw,
                        stid, stim, incw, decw, icgw, iclw, dcgw, dclw,
                        lag,  lal,  cas,  chk2, loca, lip,  ixa,  swp,
                        cal,  csp,  pack, unpk, locr, locs, srcl,
                        lolw0, stlw0];
      
      stdp[ 0]:='get '; stdp[ 1]:='put '; stdp[ 2]:='rds '; stdp[ 3]:='rln ';
      stdp[ 4]:='new '; stdp[ 5]:='wln '; stdp[ 6]:='wrs '; stdp[ 7]:='eln ';
      stdp[ 8]:='wri '; stdp[ 9]:='wrr '; stdp[10]:='wrc '; stdp[11]:='rdi ';
      stdp[12]:='rdr '; stdp[13]:='rdc '; stdp[14]:='sin '; stdp[15]:='cos ';
      stdp[16]:='exp '; stdp[17]:='log '; stdp[18]:='sqt '; stdp[19]:='atn ';
      stdp[20]:='--- '; stdp[21]:='pag '; stdp[22]:='rsf '; stdp[23]:='rwf ';
      stdp[24]:='wrb '; stdp[25]:='wrf '; stdp[26]:='dsp '; stdp[27]:='wbf ';
      stdp[28]:='wbi '; stdp[29]:='wbr '; stdp[30]:='wbc '; stdp[31]:='wbb ';
      stdp[32]:='rbf '; stdp[33]:='rsb '; stdp[34]:='rwb '; stdp[35]:='gbf ';
      stdp[36]:='pbf ';
      
      constPool := maxDS; { start constant pool at top of data seg }
      { initialize label table }
      FOR i := 0 TO maxlabel DO
        WITH labeltab[i] DO
          BEGIN val := maxCS; defined := false; equate := false END;
      { initialize file state }
      FOR i := 3 TO maxfil-1 DO
        filstate[i] := fclosed;
      filstate[maxfil] := fsentinel;
      
      iline := 1; { set 1st line of intermediate }
      prevSL := 0;
      IF statStats THEN
        BEGIN
          FOR op := succ(invd) TO pred(lastop) DO
            BEGIN stats[op].inx := op; stats[op].tot := 0 END;
          total := 0;
        END;
      {debug := false;}
offStats := false;
FOR i := -15 TO 15 DO
  freqoff[i] := 0;
saveC1 := 0; { re-use reals }
saveC2 := 0; { re-use sets }
saveC3 := 0; { re-use bound pairs }
    END;(*init*)

    PROCEDURE loadError(streng: beta); (*error in loading*)
    BEGIN
      writeln;
      writeln('*** Program load error: [', iline:1, '] ', streng);
      GOTO 1
    END; (*loadError*)

    PROCEDURE getLine; { get to beginning of new line }
    BEGIN
      readln(prd);
      IF dotrasmb THEN
        writeln;
      iline := iline + 1 { next intermediate line }
    END;

    PROCEDURE getnxt;
    BEGIN
      IF eof(prd) THEN
        loadError('Unexpected EOF on p-code file.')
      ELSE
        BEGIN
          IF eoln(prd) THEN
            BEGIN
              getLine;
              ch := chr(10) { end of token }
            END
          ELSE
            BEGIN
              read(prd, ch);
              IF dotrasmb THEN
                write(ch)
            END;
        END;
    END;

    PROCEDURE skipSpaces; { get next non-blank character }
    BEGIN
      WHILE ch = ' ' DO getnxt
    END;

    FUNCTION readInt: integer;
      VAR   Int: integer;
            neg: Boolean;
    BEGIN
      skipSpaces;
      IF ch IN ['-','+'] THEN
        BEGIN neg := ch = '-'; getnxt END
      ELSE
        neg := false;
      Int := 0;
      WHILE ch IN ['0'..'9'] DO
        BEGIN Int := 10*Int + (ord(ch) - ord('0')); getnxt END;
      IF neg THEN readInt := -Int
             ELSE readInt := Int;
    END;

    PROCEDURE storeop(fop: opcodes);
    BEGIN
      code[pc] := ord(fop); pc := pc + 1
    END;

    PROCEDURE storebyte(b: byte);
    BEGIN
      code[pc] := b; pc := pc + 1
    END;

    PROCEDURE store_enc(q: encodable);
      VAR   t: cardinal;
    BEGIN
      IF (q >= -64) AND (q <= 63) THEN { 1-byte is enough }
        IF q < 0 THEN
          storebyte(128+q)
        ELSE
          storebyte(q)
      ELSE { needs 2 bytes, at least }
      IF (q >= -8192) AND (q <= 8191) THEN
        BEGIN
          IF q >= 0 THEN q := q - 32768 { turn on bits 15 through 31}
                    ELSE q := q - 16384 { turn off bit 14 };
          { q = 11111111 11111111 10sxxxxx xxxxxxxx }
          t := q + t16;
          { q = 00000000 00000000 10sxxxxx xxxxxxxx }
          storebyte(t DIV 256); storebyte(t MOD 256);
        END
      ELSE { needs 4 bytes } { both bits 30 and 31 must be "on" }
        IF q < 0 THEN
          putIntRev(q)
        ELSE { q >= 0 }
          putIntRev(integer(q)-t30);
    END;

    FUNCTION log2(i: integer): integer;
      VAR j: integer;
    BEGIN
      j := 0;
      IF i >= 0 THEN
        WHILE i <> 0 DO BEGIN i := i DIV 2; j := j + 1 END
      ELSE
        WHILE i <> 0 DO BEGIN i := i DIV 2; j := j - 1 END;
      log2 := j;
    END;

    FUNCTION makeOffset(target,jumper: labelVal): jmpoff;
      VAR   i,j: integer; { turn the difference between two absAddrs into an }
    BEGIN               { offset from the jumper (actually the next instr) } 
      i := target - jumper;                                { to the target }
      IF i < -32768 THEN
        i := i + t16
      ELSE
      IF i > 32767 THEN
        i := i - t16;
      IF offStats THEN
        BEGIN
          j := log2(i);
          freqoff[j] := freqoff[j] + 1;
        END;
      makeOffset := i;
    END;

    PROCEDURE labelLookup(fpc: codeAddr; ldaCalling: Boolean);
       VAR   i: labelrg;
             absAddr: labelVal;
    BEGIN
      WHILE (ch <> 'L') DO getnxt;
      getnxt; { eat the 'L' }
      i := readInt;
      absAddr := labeltab[i].val; { absAddr of Li (defined), or prev ref to Li }
      IF labeltab[i].defined THEN
        IF ldaCalling THEN { lda instr needs an absolute address }
          putLabel(pc, absAddr)
        ELSE
          putCode2byte(pc, makeOffset(absAddr, fpc+labsize))
      ELSE
        BEGIN
          labeltab[i].val := pc;
          putCodeAddr(pc, absAddr); { chain gets one more ref }
        END;
      pc := pc + labsize
    END; (* labelLookup *)

    PROCEDURE getname(VAR fname: alfa);
      VAR   i: integer;
    BEGIN
      i := 1; { set 1st character of word }
      WHILE ch IN ['a'..'z','0','1','2'] DO
        BEGIN
          IF i > maxalfa THEN loadError('Opcode name is too long');
          fname[i] := ch;
          i := i + 1;
          getnxt;
        END;
      WHILE i <= maxalfa DO
        BEGIN fname[i] := ' '; i := i + 1 END;
    END; (*getname*)

    PROCEDURE lbub2constpool(lb,ub: integer; VAR q: encodable);
      VAR   q1: dataAddr;
    BEGIN
      constPool := constPool - wordsize;
      alignDown(intal, constPool);
      IF constPool - wordsize <= 0 THEN
        loadError('constant table overflow');
      putWord(constPool, ub);
      constPool := constPool - wordsize;
      putWord(constPool, lb); q := constPool;
      IF constPoolOpt THEN
        BEGIN
          q1 := maxDS - wordsize;
          alignDown(intal, q1); { top of consts (adjusted) }
          WHILE (getWord(q1) <> ub) OR (getWord(q1-wordsize) <> lb) DO
            q1 := q1 - intal;
          IF q1-wordsize*2 >= q THEN { found an older copy }
            BEGIN
              saveC3 := saveC3 + wordsize*2;
              q := q1 - wordsize;
              constPool := constPool + wordsize*2;
            END;
        END;
    END;  

    PROCEDURE assemble; (*translate symbolic code into machine code and store*)
      CONST apos = '''';
      VAR   opname: alfa;
            r: Double;
            s,s2: settype;
            i,j,lb,ub: integer;
            s1: integer;
            q: encodable;
            q1: dataAddr;
    BEGIN { assemble }
      { make sure there's room left in the code space for the biggest instr: }
      IF pc+3*(adrsize+1) > maxCS THEN
        loadError('Program code overflow');

      p := 0;  q := 0;
      getname(opname);
      { do linear search for opcode: }
      mn[invd] := opname; { put the sentinel in the slot for invd }
      op := lastop;
      REPEAT
        op := pred(op)
      UNTIL mn[op] = opname;

      CASE op OF
        invd:
          BEGIN
            write('Problem with ',opname,': ');
            loadError('illegal instruction');
          END;
        logb,logw,logd,
        stgb,stgw,stgd,
        iand,ior,dif,
        incw,decw,icgw,dcgw,
        equ,neq,leq,geq,les,gtr,inn,
        equm,neqm,leqm,geqm,lesm,gtrm,sst1,sst2,
        lag,ixa,sgs,rgs,swp,mov:
          BEGIN
            storeop(op);
            store_enc(readInt)  { offset (or length) }
          END;
        loib:
          BEGIN
            i := readint;
            IF i = 0 THEN
              storeop(loib0)
            ELSE
              BEGIN
                storeop(op);
                store_enc(i)  { offset }
              END;
          END;
        loiw:
          BEGIN
            i := readint;
            IF i = 0 THEN
              storeop(loiw0)
            ELSE
              BEGIN
                storeop(op);
                store_enc(i)  { offset }
              END;
          END;
        loid:
          BEGIN
            i := readint;
            IF i = 0 THEN
              storeop(loid0)
            ELSE
              BEGIN
                storeop(op);
                store_enc(i)  { offset }
              END;
          END;
        loim:  
          BEGIN
            j := readint; { length }
            i := readint; { offset }
            IF i = 0 THEN
              BEGIN
                storeop(loim0);
                store_enc(j)
              END
            ELSE
              BEGIN
                storeop(op);
                store_enc(j);
                store_enc(i)
              END;
          END;
        logm,stgm,
        pack,unpk:
          BEGIN
            storeop(op);
            store_enc(readInt); { length }
            store_enc(readInt)  { offset }
          END;
        lolb,lold,
        stlb,stld,
        iclw,dclw,lal,lip:
          BEGIN
            storeop(op);
            storebyte(readInt); { lex level }
            store_enc(readInt)  { offset }
          END;
        lolw:
          BEGIN
            i := readInt; { lex level }
            IF i = 0 THEN { which it is more often than not }
              storeop(lolw0)
            ELSE
              BEGIN
                storeop(lolw);
                storebyte(i); { lex level }
              END;
            store_enc(readInt)  { offset }
          END;
        stlw:
          BEGIN
            i := readInt; { lex level }
            IF i = 0 THEN { which it is more often than not }
              storeop(stlw0)
            ELSE
              BEGIN
                storeop(stlw);
                storebyte(i); { lex level }
              END;
            store_enc(readInt)  { offset }
          END;
        lolm,stlm:
          BEGIN
            storeop(op);
            store_enc(readInt); { length }
            storebyte(readInt); { lex level }
            store_enc(readInt)  { offset }
          END;
        locw:
          BEGIN
            i := readInt;
            IF (i < lo_enc) OR (i > hi_enc) THEN
              BEGIN
                storeop(locw);
                putCodeInt(i)
              END
            ELSE
            IF i = 0 THEN
              storeop(loc0)
            ELSE
            IF i = 1 THEN
              storeop(loc1)
            ELSE
              BEGIN storeop(locq); store_enc(i) END
          END;
        addi,subi,muli,divi,negi,absi,sqri,modi,
        addr,subr,mulr,divr,negr,absr,sqrr,inot,
        flt,flt2,
        trc,rnd,
        chk0,chk1,ixca,
        iodd,eofp,dmp,dup,ret0,ret1,ret2,copy,
        fbv,fvb,efb,pop0,cmln,asgn,fatl,stop:
          storeop(op);
        stib:
          BEGIN
            i := readint;
            IF i = 0 THEN
              storeop(stib0)
            ELSE
              BEGIN
                storeop(op);
                store_enc(i)  { offset }
              END;
          END;
        stiw:
          BEGIN
            i := readint;
            IF i = 0 THEN
              storeop(stiw0)
            ELSE
              BEGIN
                storeop(op);
                store_enc(i)  { offset }
              END;
          END;
        stid:
          BEGIN
            i := readint;
            IF i = 0 THEN
              storeop(stid0)
            ELSE
              BEGIN
                storeop(op);
                store_enc(i)  { offset }
              END;
          END;
        stim:
          BEGIN
            j := readint; { length }
            i := readint; { offset }
            IF i = 0 THEN
              BEGIN
                storeop(stim0);
                store_enc(j)
              END
            ELSE
              BEGIN
                storeop(op);
                store_enc(j);
                store_enc(i)
              END;
          END;
        fjp,tjp,bra,
        ent:
          BEGIN storeop(op); labelLookup(pc, false) END;
        ujc: BEGIN storeop(op); putCodeAddr(pc, 0); pc := pc + labsize END;
        xjp: BEGIN storeop(op); lastXjpPc := pc END;
        sjp: BEGIN storeop(op); labelLookup(pc, false); lastSjpPc := pc END;
        cas:
          BEGIN
            lb := readInt; ub := readInt;
            lbub2constpool(lb, ub, q);
            storeop(op); store_enc(q);
            labelLookup(pc, false);
          END;
        chk2:
          BEGIN
            lb := readInt; ub := readInt;
            lbub2constpool(lb, ub, q);
            storeop(op); store_enc(q);
          END;
        loca:
          BEGIN
            len := readInt;
            constPool := constPool - len;
            IF constPool <= 0 THEN loadError('constant table overflow');
            skipSpaces; { postcondition: ch <> ' ' }
            IF ch <> '|' THEN
              loadError('bad string format');
            { characters from the loca string go directly into the const pool }
            getnxt; { 'Just eat it!' --Weird Al Yankovich }
            FOR j := 0 TO len-1 DO
              BEGIN
                putByte(constPool + j, ord(ch));
                getnxt;
              END;
            IF ch <> '|' THEN loadError('bad string format: type 2');
            getnxt; { eat final '|', so ch can't be 'q', stopping generate }
            q := constPool;
            storeop(op); store_enc(q)
          END;
        lpa:
          BEGIN
            storeop(op); storebyte(readInt);
            labelLookup(pc, true); { 'true' means drop a 4-byte abs address }
          END;                     { instead of 2-byte signed offset }
        ipj:
          BEGIN
            storeop(op);
            storebyte(readInt);      { lex level }
            labelLookup(pc, false);  { offset to target }
          END;
        cal:
          BEGIN { note: 1st q is encoded (# of args); but 2nd one is label }
            q := readInt;
            saveoffStats := offStats; offStats := false; { don't count cups }
            storeop(op); store_enc(q); labelLookup(pc, false);
            offStats := saveoffStats; { restore }
          END;
        cali,
        mst:
          BEGIN p := readInt; storeop(op); storebyte(p) END;
        csp:
          BEGIN { call standard procedure }
            skipSpaces;
            getname(opname);
            q := 0;
            WHILE opname <> stdp[q] DO
              BEGIN
                q := q + 1;
                IF q > maxsp THEN
                  loadError('std proc/func not found')
              END;
            storeop(op); store_enc(q)
          END;
        locr:
          BEGIN
            {$I-}
            read(prd, r);
            IF IOResult <> 0 THEN
              BEGIN
                writeln('Error processing real constant: abort');
                GOTO 1;
              END;
            {$I+}
            alignDown(realal, constPool);
            constPool := constPool - dublsize;
            IF constPool <= 0 THEN
              loadError('constant table overflow');
            putDubl(constPool, r);
            q := constPool;
            IF constPoolOpt THEN
              BEGIN
                q1 := maxDS-dublsize;
                alignDown(realal, q1); { top of consts (adjusted) }
                WHILE getDubl(q1) <> r DO
                  q1 := q1 - realal;
                IF q1-dublsize >= q THEN { found an older copy }
                  BEGIN
                    saveC1 := saveC1 + dublsize;
                    q := q1;
                    constPool := constPool + dublsize;
                  END;
              END;
            storeop(op); store_enc(q)
          END;
        locs:
          BEGIN
            skipSpaces;
            len := readInt;
            skipSpaces;
            IF ch <> '(' THEN loadError('ldc <n> (...) expected');
            s := [ ];  getnxt; { eat the '(' }
            skipSpaces;
            WHILE ch <> ')' DO
              BEGIN
                s1 := readInt;
                {s := s + [s1]} include(s, s1);
                skipSpaces
              END;
            getnxt; { eat the ')' }
            constPool := constPool - len;
            alignDown(setal, constPool);
            IF constPool <= 0 THEN
              loadError('constant table overflow');
            putSet(constPool, s, len); { sets searchByte }
            q := constPool;
            IF constPoolOpt THEN
              BEGIN
                q1 := maxDS - len + 1;
                alignDown(setal, q1); { top of consts (adjusted) }
                WHILE data[q1] <> searchByte DO
                  q1 := q1 - setal;
                getSet(q1, s2, len);
                WHILE s2 <> s DO
                  BEGIN
                    q1 := q1 - setal;
                    WHILE data[q1] <> searchByte DO
                      q1 := q1 - setal; { speeds things up a bit }
                    getSet(q1, s2, len)
                  END;
                IF q1-len >= q THEN { found an older copy }
                  BEGIN
                    saveC2 := saveC2 + len;
                    q := q1;
                    constPool := constPool + len;
                  END;
              END;
            storeop(op); store_enc(len); store_enc(q)
          END;
        otherwise writeln('OOPS: forgot to include ',ord(op):1);
      END; (*CASE*)
      IF statStats THEN
        BEGIN
          stats[op].tot := stats[op].tot + 1;
          total := total + 1;
        END;
    END; (*assemble*)

    PROCEDURE generate; (* generate segment of code *)
      VAR   firstchar: char;
            labval: labelVal;
            i: labelrg; (* label number *)
            j: offset; { big enough for a source line number, surely? }
            inx, newinx: labelVal;
    BEGIN
      IF dotrasmb THEN
        writeln('ORG ',pc:1);
      REPEAT
        WHILE eoln(prd) DO
          getLine; { if previous processing left us at end-of-line, then fix }
        getnxt;(* first character of non-empty line*)
        firstchar := ch;
        getnxt;
        IF NOT (firstchar IN ['L', 'C', 'S', 'q', ' ', '!', 'i', ':']) THEN
          loadError('unexpected line start');
        IF firstchar <> ':' THEN
          prevSL := 0;
        CASE firstchar OF
          'i',
          '!': WHILE ch <> chr(10) DO getnxt;
          'L': BEGIN
                 i := readInt;
                 IF ch = '=' THEN
                   BEGIN getnxt; labval := readInt; labeltab[i].equate := true END
                 ELSE
                   labval := pc;
                 IF labeltab[i].defined THEN loadError('duplicated label');
                 { now that this label is defined, let's fixup any previous }
                 { references; in any case, store the value in the label table }
                 inx := labeltab[i].val; { head of list of labels to be fixed up }
                 WHILE inx <> maxCS DO
                   BEGIN
                     newinx := getLabel(inx); { get next index in chain }
                     IF labeltab[i].equate THEN
                       putLabel(inx, labval) { fix up the 'q' field }
                     ELSE
                       putCode2byte(inx, makeOffset(labval, inx+labsize));
                     inx := newinx;
                   END;
                 labeltab[i].defined := true;  { backward-looking refs to i }
                 labeltab[i].val := labval; { will be fixed up immediately }
                 WHILE ch <> chr(10) DO getnxt;
               END;
          { CASE table entry: now limited to a two-byte offset }
          'C': labelLookup(lastXjpPc-labsize, false);
          'S': BEGIN { sparse CASE table entry: 2 x 2-byte entries }
                 putCode2byte(pc, readInt); { case label constant }
                 pc := pc + labsize;
                 labelLookup(lastSjpPc-labsize, false);
               END;  { offset from first sparse CASE table entry to L<i> }
          'q': ; { quittin' time (beer:30?) }
          ' ': assemble;
          ':': BEGIN { source line }
                 { get source line number: }
                 j := readInt;
                 IF doTraceSource THEN
                   BEGIN
                     { pass source line register instruction }
                     IF code[prevSL] = ord(srcl) THEN { previous guy was source line }
                       pc := prevSL { write over previous guy }
                     ELSE
                       prevSL := pc;
                     storeop(srcl);
                     store_enc(j)
                   END;
                 WHILE ch <> chr(10) DO getnxt;
               END;
        END { CASE };
      UNTIL firstchar = 'q';
      IF dotrasmb THEN
        writeln;
    END; (*generate*)

  BEGIN (*load*)
    init;
    pc := entryPoint; { where to start the generated code }
    generate;
    pctop := pc; { save top of code store }
    writeln('Total code size = ',pctop:1);
    pc := 0;     { put the final mst/cup/stp at the beginning }
    generate;
    alignDown(heapal, constPool); { align the start of constPool for heap top }
    IF statStats THEN
      BEGIN
        FOR op2 := succ(invd) TO pred(lastop) DO
          FOR op1 := succ(invd) TO pred(lastop) DO
            IF stats[op1].tot < stats[succ(op1)].tot THEN
              BEGIN
                stats[invd] := stats[op1];
                stats[op1] := stats[succ(op1)];
                stats[succ(op1)] := stats[invd];
              END;
        
        FOR op1 := succ(invd) TO pred(lastop) DO
          WITH stats[op1] DO
{         IF tot <> 0 THEN}
            writeln(ord(op1):3,' ',mn[inx], tot:9, tot*100/total:6:2, '%');
      END;
    IF dodmpsto THEN
      BEGIN { dump storage overview }
        writeln;
        writeln('Storage areas occupied');
        writeln;
        write('Program     '); writeHex(0, maxdigh); write('-');
                               writeHex(pctop-1, maxdigh);
        writeln(' (',pctop:maxdigd,')');
        write('Stack/Heap  '); writeHex(pctop, maxdigh); write('-');
                               writeHex(constPool-1, maxdigh);
        writeln(' (',constPool-pctop+1:maxdigd,')');
        write('Constants   '); writeHex(constPool, maxdigh); write('-');
                               writeHex(maxDS, maxdigh);
        writeln(' (',maxDS-(constPool):maxdigd,')');
        writeln
      END;
    IF saveC1 > 0 THEN
      writeln('We saved ',saveC1:1,' bytes by re-using reals in the constant pool.');
    IF saveC2 > 0 THEN
      writeln('We saved ',saveC2:1,' bytes by re-using set constants.');
    IF saveC3 > 0 THEN
      writeln('We saved ',saveC3:1,' bytes by re-using range-checking constants.');
  IF offStats THEN
    FOR i := -15 TO 15 DO
      writeln('freq[',i:3,'] = ',freqoff[i]);
  END; (*load*)

  (*------------------------------------------------------------------------*)

  PROCEDURE interpError(streng: beta);
  BEGIN writeln; write('*** Runtime error');
      IF sourceLine > 0 THEN write(' [', sourceLine:1, ']');
      writeln(': ', streng);
      GOTO 1
  END;(*interpError*)

  FUNCTION base(ld: integer): dataAddr;
    VAR   ad: dataAddr;
  BEGIN
    ad := fp;
    WHILE ld > 0 DO
      BEGIN ad := getWord(ad+marksl); ld := ld - 1 END;
    base := ad
  END; (*base*)

  PROCEDURE compare;
  (*comparing is only correct if result by comparing integers will be*)
  BEGIN
    popWord(i2);
    popWord(i1);
    i := 0; b := true;
    WHILE b AND (i <> q) DO
      IF data[i1+i] = data[i2+i] THEN i := i+1
      ELSE b := false
  END; (*compare*)

  PROCEDURE valfil(fa: dataAddr); { attach file to file entry }
    VAR   i: integer;
  BEGIN
    IF data[fa] = 0 THEN
      IF fa = inputoff THEN
        data[fa] := inputfn
      ELSE
      IF fa = outputoff THEN
        data[fa] := outputfn
      ELSE
        BEGIN
          i := 3; { skip over files open for reading & writing }
          WHILE filstate[i] > fclosed DO i := i + 1;
          { assert( i < maxfil ^ filstate[i] = fclosed |
                    i = maxfil ^ filstate[i] = fsentinel ) }
          IF i = maxfil THEN
            interpError('Max files exceeded       ')
          ELSE
            data[fa] := i;
        END;
  END;

  PROCEDURE valWrite(fn: integer); { validate file write mode }
  BEGIN
    IF fn = inputfn THEN
      interpError('input not in write mode  ')
    ELSE
    IF fn = outputfn THEN
      {filstate[fn] := fwrite}
    ELSE
    IF filstate[fn] = fclosed THEN
      filstate[fn] := fwrite
    ELSE
    IF filstate[fn] <> fwrite THEN { = fread? }
      interpError('File not in write mode   ')
  END;

  PROCEDURE valRead(fn: integer); { validate file read mode }
  BEGIN
    IF fn = inputfn THEN
      {filstate[fn] := fread}
    ELSE
    IF fn = outputfn THEN
      interpError('output not in read mode  ')
    ELSE { fn >= 3, right? }
      BEGIN
        IF filstate[fn] = fclosed THEN
          filstate[fn] := fread
        ELSE
        IF filstate[fn] <> fread THEN
          interpError('File not in read mode    ');
        {IF eof(filtable[fn]) THEN
          interpError('End of file              ');}
      END;
  END;

  FUNCTION getOp: opcodes;{ attribute (inline); }
  BEGIN
    getOp := opcodes(code[pc]); pc := pc + 1
  END;

  FUNCTION getCodeByte: byte;{ attribute (inline); }
  BEGIN
    getCodeByte := code[pc]; pc := pc + 1
  END;

  FUNCTION decode: encodable; { next 1, 2, or 4 bytes are encoded }
    VAR   b: byte;
          q: encodable;
  BEGIN
    b := getCodeByte;
    IF b < 128 THEN { bit 7 = 0 }
      IF b >= 64 THEN { bit 6 is on, however }
        q := integer(b) - 128
      ELSE { q in 0..63: already done }
        q := integer(b)
    ELSE
    IF b < 192 THEN
      BEGIN
        q := integer(b-128)*256 + getCodeByte;
        { q = 00sxxxxx xxxxxxxx }
        IF q >= 8192 THEN { that s bit is on }
          { q is in [8192..16383]; must trans to [-8192..-1] }
          q := q - 16384
        ELSE
          { q = 000xxxxx xxxxxxxx: DONE }
      END
    ELSE { b >= 192: both bits 6 and 7 are "on" }
      BEGIN
        IF b >= 224 THEN { b in 111xxxxx: it's a negative number }
          q := integer(b) - 256 { q in 11111111 11111111 11111111 111xxxxx }
        ELSE { b in 110xxxxx (192..223) }
          q := integer(b-192); { q in [0..31] }
        shl(q, 8); q := q + getCodeByte;
        shl(q, 8); q := q + getCodeByte;
        shl(q, 8); q := q + getCodeByte;
      END;
    decode := q
  END;

  { list single instruction at code[pc] }
  PROCEDURE listInstr;
    VAR   op: opcodes;
          q: integer;
          lo,hi: integer;  (*instruction register*)
  BEGIN
    op := getOp;
    write(' ', mn[op], '  ');
    IF op IN opNeedsSize THEN
      writeHex(decode, 2);
    IF op IN opNeedsLevel THEN
      writeHex(getCodeByte, 2);
    IF op IN opNeedsOffset THEN
      BEGIN
        IF op IN opNeedsLevel THEN write(',')
                              ELSE write('   ');
        IF (op = locw) OR (op = lpa) THEN
          q := getCodeInt(pc)
        ELSE
        IF op IN [fjp, bra, ent, ipj, tjp] THEN
          BEGIN q := getCode2byte(pc); pc := pc + labsize END { unencoded q fields }
        ELSE
          q := decode;
        IF op IN [chk2, cas] THEN
          BEGIN { go to the constant pool and get the lower & upper bounds }
            lo := getWord(q);
            hi := getWord(q+wordsize);
            write('[',lo:1,',',hi:1,']');
          END
        ELSE
        IF op = csp THEN
          write(stdp[q])
        ELSE
        IF (op = locw) OR (op = locq) THEN
          writeHex(q, 8) { full 32 bits }
        ELSE
          writeHex(q, maxdigh);
        IF op IN [cal,cas] THEN { the new 'cal', 'cas' }
          BEGIN
            q := getCode2byte(pc);
            pc := pc + labsize; { unencoded q fields }
            write(' ');
            writeHex(q, maxdigh);
          END;
      END;
  END; { listInstr }

  FUNCTION getStr(inx, lgth: integer): str255;
    VAR   i: integer;
          s: str255;
  BEGIN
    s := '';
    FOR i := 0 TO lgth-1 DO
      s := s + chr(data[inx + i]);
    getStr := s;
  END;
  
  PROCEDURE dx(p: dataAddr);
  BEGIN
    write('address ('); writeHex(p, maxdigh);
    write(') is outside the heap: ['); writeHex(np, maxdigh);
    write(','); writeHex(constPool-adrsize, maxdigh); writeln(']');
  END;
  
  { report all space in heap }

  PROCEDURE repspc;
    VAR len: integer;
        ad: dataAddr;
        i: integer;
        b: byte;
  BEGIN
    ad := np; { index the bottom of heap }
    len := getWord(ad); { get next block length }
    IF len = 0 THEN writeln('Heap empty');
    WHILE len <> 0 DO
      BEGIN
        b := data[ad+4];
        IF (len = -20) AND (b >= 97) AND (b <= 122) THEN
          BEGIN
            write('addr: '); writeHex(ad, maxdigh); write(': ', len:6, ': ');
            i := 4;
            b := data[ad+i];
            WHILE (i < 14) AND ((b >= 97) OR (b = 32)) AND (b <= 122) DO
              BEGIN
                write(chr(b));
                i := i + 1;
                b := data[ad+i]
              END;
            writeln;
          END;
        IF len > 0 THEN ad := ad + len
                   ELSE ad := ad - len;
        len := getWord(ad); { get next block length }
      END
  END;

  { find free block using length }

  PROCEDURE skipAllocated(VAR faddr: dataAddr; VAR fsize: integer);
  BEGIN { we might already be at the sentinel, in which case fsize = 0 }
    WHILE fsize < 0 DO { allocated block has size > 0 }
      BEGIN { faddr will get higher }
        faddr := faddr - fsize; { jump over allocated blocks }
        fsize := getWord(faddr);
      END;
    { obviously, fsize >= 0, i.e. we're at the sentinel, or a free block }
  END;
    
  PROCEDURE findFree(adjustedLen: integer; VAR blk: dataAddr);
    VAR   list: dataAddr;
          fit,bestFit,size: integer;
          bestBlk: dataAddr;
  BEGIN { try to find something disposed, and therefore on the 'free' list }
    bestFit := maxDS;
    bestBlk := 0;
    blk := 0; { no block found, until we find it }
    list := np; { bottom of heap }
    size := getWord(list);
    skipAllocated(list, size); { get to a free (available) block }
    WHILE size <> 0 DO { not at the sentinel yet }
      BEGIN { assert: size > 0, i.e., we've got a free block }
        fit := size - adjustedLen;
        IF fit = 0 THEN { bingo! exact size }
          BEGIN blk := list; size := 0 { bail out of loop } END
        ELSE
          BEGIN
            IF fit > 0 THEN { find best fit }
              BEGIN
                IF fit < bestFit THEN
                  BEGIN
                    bestFit := fit;
                    bestBlk := list
                  END
              END;
            list := list + size;   { how to get from a free block to the next }
            size := getWord(list); { size of the next block (or the sentinel) }
          END;
        skipAllocated(list, size); { get to a free block (or the sentinel) }
      END;
    IF blk = 0 THEN { no exact fit }
      IF bestFit < maxDS THEN { but there IS a (smallest) free block }
        blk := bestBlk;
  END;

  { allocate space in heap }

  PROCEDURE newspc(len: integer; VAR blk: dataAddr);
    VAR   newBlockAddr,unalignedAddr: dataAddr;
  BEGIN
    len := len + wordsize; { the real len includes the size field }
    findFree(len, blk); { try finding an existing free block }
    IF blk = 0 THEN { findFree didn't find any, so create a new block }
      BEGIN { allocate from heap bottom }
        newBlockAddr := np - len; { find new heap bottom }
        unalignedAddr := newBlockAddr; { save address }
        alignDown(heapal, newBlockAddr);
        len := len + (unalignedAddr - newBlockAddr); { adjust length for alignment }
        np := newBlockAddr;
        IF monStackHeap THEN
          IF np - sp < minStackHeap THEN
            minStackHeap := np - sp;
        blk := newBlockAddr;
      END
    ELSE
      len := getWord(blk); { findFree found one, but the len might be different }
    putWord(blk, -len); { mark block as being allocated }
    blk := blk + wordsize { return address after size field }
  END;

  { coalesce space in heap }

  PROCEDURE cscspc;
    VAR   size: integer;
  BEGIN
    { 'forget' any free blocks that might be at the heap bottom }
    size := getWord(np);
    WHILE size > 0 DO { it's free (no longer allocated; been disposed) }
      BEGIN
        np := np + size;
        size := getWord(np);
      END;
  END;

  { dispose of space in heap }

  PROCEDURE dspspc(blk: dataAddr);
    VAR   blockAddr: dataAddr;
          size: integer;
  BEGIN
    IF blk = 0 THEN interpError('dispose uninit pointer   ')
    ELSE
    IF blk = nilval THEN interpError('dispose nil pointer      ')
    ELSE
      BEGIN
        blockAddr := blk - adrsize; { back up behind size field }
        IF (blockAddr < np) OR (blockAddr >= constPool-adrsize) THEN
          BEGIN
            dx(blk); { diagnose pointer problem }
            interpError('bad pointer value        ');
          END;

        size := getWord(blockAddr);
        IF size >= 0 THEN
          interpError('block already freed      ');
        putWord(blockAddr, abs(size)); { marks the block as being free }
        cscspc { coalesce free space }
      END;
  END;

  { check pointer indexes free entry }

  FUNCTION isfree(blk: dataAddr): Boolean;
  BEGIN
    isfree := getWord(blk-adrsize) >= 0
  END;

  PROCEDURE copyCmndLine;
    VAR   i,j,k,lgth,n: integer;
          s: str255;
  BEGIN
    n := ParamCount;
    i := 1; { skip over the first two p-code file names }
    k := 0;
    WHILE i <= n DO
      BEGIN
        s := ParamStr(i);
        lgth := Length(s);
        IF lgth + k < 254 THEN
          BEGIN
            FOR j := 0 TO lgth-1 DO
              data[ad+k+j] := ord(s[j+1]);
            {MoveLeft(s[1], data[ad+k], lgth);}
            k := k + lgth;
            data[ad+k] := ord(' ');
            k := k + 1;
          END
        ELSE
          interpError('Command line too long');
        i := i + 1;
      END;
    data[ad+k-1] := 0; { null terminator replaces final blank }
    WHILE k <= 254 DO
      BEGIN k := k + 1; data[ad+k] := ord(' ') END;
  END;

  { call standard procedure or function }

  PROCEDURE callsp;
    CONST FF = 12; { The ASCII form feed character for the 'page' intrinsic }
    VAR   line: Boolean;
          i3,j: integer;
          i, w, len, f: integer;
          c: char;
          b: Boolean;
          ad: dataAddr;
          r: Double;
          fn: fileno;

    PROCEDURE writestr(VAR f: text; ad: dataAddr; w: integer; len: integer);
      VAR   i: integer;
    BEGIN (* len and w are numbers of characters *)
      IF w > len THEN
        FOR i:= 1 TO w-len DO write(f,' ') { padding on the left }
      ELSE
        len := w;
      FOR i := 0 TO len-1 DO
        write(f, chr(getByte(ad+i)))
    END;(*writestr*)

    PROCEDURE readstr(VAR f: text; ad: dataAddr; len: integer);
      VAR   i: integer;
            ch: char;
    BEGIN (* len is length of the string @ ad *)
      FOR i := 0 TO len-1 DO
        BEGIN
          read(f, ch);
          putByte(ad+i, ord(ch));
        END;
    END;(*readstr*)

    PROCEDURE putfile(VAR f: text; VAR ad: dataAddr);
    BEGIN
      f^ := chr(getByte(ad+fileidsize));
      put(f)
    END;(*putfile*)

  BEGIN (*callsp*)
    IF q > maxsp THEN interpError('invalid std proc/func    ');

    CASE q OF
      0 (*get*): BEGIN
                   popWord(i); valfil(i); fn := data[i]; valRead(fn);
                   IF fn = inputfn  THEN get(input)
                                    ELSE get(filtable[fn])
                 END;
      1 (*put*): BEGIN
                   popWord(i); valfil(i); fn := data[i]; valWrite(fn);
                   IF fn = outputfn THEN putfile(output, ad)
                                    ELSE putfile(filtable[fn], ad)
                 END;
      
      2 (*rds*): BEGIN { P6 intrinsic procedure rds (read string from textfile) }
                   popWord(len); popWord(j);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valRead(fn);
                   IF fn = inputfn  THEN readstr(input, j, len)
                                    ELSE readstr(filtable[fn], j, len)
                 END;
      3 (*rln*): BEGIN
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valRead(fn);
                   IF fn = inputfn  THEN readln(input)
                                    ELSE readln(filtable[fn]);
                   sp := sp - adrsize; { pop top of stack, 'dmp' not needed } 
                 END;
      4 (*new*): BEGIN popWord(i); { i is # of bytes requested }
                   newspc(i, ad); { ad is now ptr into heap for i bytes }
                   IF ad < np THEN  { or is it outside the heap?? }
                     interpError('OOPS: dynamic alloc goof ');
                   popWord(i3);
                   putWord(i3, ad)
                 END;
      5 (*wln*): BEGIN
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   IF fn = outputfn THEN writeln(output)
                                    ELSE writeln(filtable[fn]);
                   sp := sp - adrsize; { pop top of stack, 'dmp' not needed }
                 END;
      6 (*wrs*): BEGIN
                   popWord(len); popWord(w); popWord(i);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   IF fn = outputfn THEN writestr(output, i, w, len)
                                    ELSE writestr(filtable[fn], i, w, len)
                 END;
      7 (*eln*): BEGIN popWord(i); valfil(i); fn := data[i]; valRead(fn);
                   IF fn = inputfn  THEN line:= eoln(input)
                                    ELSE line := eoln(filtable[fn]);
                   pushWord(ord(line))
                 END;
      8 (*wri*): BEGIN popWord(w); popWord(i);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   IF fn = outputfn THEN write(output, i:w)
                                    ELSE write(filtable[fn], i:w)
                 END;
      9 (*wrr*): BEGIN popWord(w); popDubl(r);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   IF fn = outputfn THEN write(output, r:w)
                                    ELSE write(filtable[fn], r:w)
                 END;
      10(*wrc*): BEGIN popWord(w); popWord(i); c := chr(i);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   IF fn = outputfn THEN write(output, c:w)
                                    ELSE write(filtable[fn], c:w)
                 END;
      11(*rdi*): BEGIN popWord(j);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valRead(fn);
                   IF fn = inputfn  THEN read(input, i)
                                    ELSE read(filtable[fn], i);
                   putWord(j, i)
                 END;
      12(*rdr*): BEGIN popWord(j);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valRead(fn);
                   IF fn = inputfn  THEN read(input, r)
                                    ELSE read(filtable[fn], r);
                   putDubl(j, r)
                 END;
      13(*rdc*): BEGIN popWord(j);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valRead(fn);
                   IF fn = inputfn  THEN read(input, c)
                                    ELSE read(filtable[fn], c);
                   putByte(j, ord(c))
                 END;
      14(*sin*): BEGIN popDubl(r1); pushDubl(sin(r1)) END;
      15(*cos*): BEGIN popDubl(r1); pushDubl(cos(r1)) END;
      16(*exp*): BEGIN popDubl(r1); pushDubl(exp(r1)) END;
      17(*log*): BEGIN popDubl(r1); pushDubl(ln(r1)) END;
      18(*sqt*): BEGIN popDubl(r1); pushDubl(sqrt(r1)) END;
      19(*atn*): BEGIN popDubl(r1); pushDubl(arctan(r1)) END;
      { placeholder for "mark" }
      20(*sav*): interpError('invalid std proc/func    ');
      21(*pag*): BEGIN popWord(i); valfil(i); fn := data[i]; valWrite(fn);
                   IF fn = outputfn THEN write(output, chr(FF))
                                    ELSE write(filtable[fn], chr(FF))
                 END;
      22(*rsf*): BEGIN popWord(i); valfil(i); fn := data[i];
                   IF filstate[fn] = fwrite THEN
                     filstate[fn] := fread { switch }
                   ELSE
                     valRead(fn);
                   IF fn >= 3 THEN { ignore a reset of input (or output) }
                     reset(filtable[fn]); { it's up to the Pascal compiler that }
                 END;                     { compiles this interpreter }
      23(*rwf*): BEGIN popWord(i); valfil(i); fn := data[i];
                   IF filstate[fn] = fread THEN
                     filstate[fn] := fwrite { switch}
                   ELSE
                     valWrite(fn);
                   IF fn >= 3 THEN { ignore a rewrite of output (or input) }
                     rewrite(filtable[fn]); { what I said above }
                 END;
      24(*wrb*): BEGIN popWord(w); popWord(i); b := i <> 0;
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   IF fn = outputfn THEN write(output, b:w)
                                    ELSE write(filtable[fn], b:w)
                 END;
      25(*wrf*): BEGIN popWord(f); popWord(w); popDubl(r);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   IF fn = outputfn THEN write(output, r:w:f)
                                    ELSE write(filtable[fn], r:w:f)
                 END;
      26(*dsp*): BEGIN
                   popWord(i); popWord(j); dspspc(getWord(j))
                 END;
      27(*wbf*): BEGIN popWord(len); popWord(j);
                   popWord(f); valfil(f);
                   fn := data[f]; valWrite(fn);
                   FOR i := 1 TO len DO
                     BEGIN
                       write(bfiltable[fn], data[j]);
                       j := j + 1
                     END
                 END;
      28(*wbi*): BEGIN
                   ad := getWord(sp-adrsize-wordsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   FOR i := 0 TO wordsize-1 DO
                     write(bfiltable[fn], data[sp-wordsize+i]);
                   popWord(i)
                 END;
      29(*wbr*): BEGIN
                   ad := getWord(sp-adrsize-dublsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   FOR i := 0 TO dublsize-1 DO
                     write(bfiltable[fn], data[sp-dublsize+i]);
                   popDubl(r)
                 END;
      30(*wbc*): BEGIN
                   ad := getWord(sp-adrsize-wordsize); valfil(ad);
                   c := chr(getWord(sp-wordsize));
                   fn := data[ad]; valWrite(fn);
                   FOR i := 0 TO charsize-1 DO
                     write(bfiltable[fn], data[sp-wordsize+i]);
                   popWord(i)
                 END;
      31(*wbb*): BEGIN
                   ad := getWord(sp-adrsize-wordsize); valfil(ad);
                   fn := data[ad]; valWrite(fn);
                   FOR i := 0 TO boolsize-1 DO
                     write(bfiltable[fn], data[sp-wordsize+i]);
                   popWord(i)
                 END;
      32(*rbf*): BEGIN popWord(len); popWord(j);
                   ad := getWord(sp-adrsize); valfil(ad);
                   fn := data[ad]; valRead(fn);
                   IF filbuff[fn] THEN { buffer data exists }
                     FOR i := 0 TO len-1 DO
                       data[j+i] := data[ad+fileidsize+i]
                   ELSE
                     FOR i := 0 TO len-1 DO
                       read(bfiltable[fn], data[j+i]);
                 END;
      33(*rsb*): BEGIN
                   popWord(i); valfil(i); fn := data[i];
                   filstate[fn] := fread;
                   reset(bfiltable[fn]);
                   filbuff[fn] := false
                 END;
      34(*rwb*): BEGIN
                   popWord(i); valfil(i); fn := data[i];
                   filstate[fn] := fwrite;
                   rewrite(bfiltable[fn]);
                   filbuff[fn] := false
                 END;
      35(*gbf*): BEGIN popWord(i);
                   popWord(w); valfil(w);
                   fn := data[w]; valRead(fn);
                   IF filbuff[fn] THEN
                     filbuff[fn] := false
                   ELSE
                     FOR j := 0 TO i-1 DO
                       read(bfiltable[fn], data[w+fileidsize+j])
                 END;
      36(*pbf*): BEGIN popWord(i);
                   popWord(w); valfil(w);
                   fn := data[w]; valWrite(fn);
                   FOR j := 0 TO i-1 DO
                     write(bfiltable[fn], data[w+fileidsize+j]);
                 END;
    END;(*CASE q*)
  END;(*callsp*)

BEGIN (* main *)

  writeln('Pascal P6 interpreter v', majorver:1, '.', minorver:1);

  IF ParamCount >= 1 THEN
    BEGIN
      assign(prd, ParamStr(1));
      reset(prd);
      IF IOResult <> 0 THEN
        BEGIN
          writeln('Input file not found: ',ParamStr(1));
          halt(IOResult);
        END;
    END
  ELSE { ParamCount = 0 }
    BEGIN
      writeln('This is an interpreter for the P6 compiler; you need to specify');
      writeln('the <ProgramName>.p6 as the first command-line parameter.');
      halt(1);
    END;
    
  writeln('Assembling/loading program');
  load; (* assembles and stores code *)
  pc := 0; sp := 0; fp := 0;
  alignDown(intal, constPool);
  np := constPool; sourceLine := 0;
  np := np - wordsize;
  putWord(np, 0); { put a sentinel size (= 0) for the null block at the top }
  minStackHeap := np;
  IF dynaStats THEN
    BEGIN
      FOR op1 := succ(invd) TO pred(lastop) DO
        BEGIN dystats[op1].inx := op1; dystats[op1].tot := 0 END;
      total := 0;
    END;
  writeln('Running program');
  writeln;
  REPEAT
    { trace executed instructions }
    IF dotrcins {AND debug} AND (pc > minpctrc) THEN
      BEGIN
        oldpc := pc;
        writeHex(oldpc, maxdigh);
        write('/');
        writeHex(sp,    maxdigh);
        listInstr;
        pc := oldpc;
      END;

    { fetch opcode from code space }
    op := getOp;

    IF dynaStats THEN
      BEGIN
        dystats[op].tot := dystats[op].tot + 1;
        total := total + 1;
      END;

    (*execute*)

    CASE op OF
      invd: { 0 should always be an invalid opcode };
      logb: pushWord(getByte(decode));
      logw: loadWord(decode);
      logd: pushDubl(getDubl(decode));
      logm: BEGIN
              i1 := decode; { length }
              i2 := decode; { src }
              FOR i3 := 0 TO i1-1 DO data[sp+i3] := data[i2+i3];
              sp := sp + i1;
              alignstack;
              IF monStackHeap THEN
                IF np - sp < minStackHeap THEN
                  minStackHeap := np - sp
            END;
      lolb: BEGIN p := getCodeByte; pushWord(getByte(base(p) + decode)) END;
      lolw: BEGIN p := getCodeByte;
              loadWord(base(p) + decode);
            END;
     lolw0: loadWord(fp + decode);
      lold: BEGIN p := getCodeByte; pushDubl(getDubl(base(p) + decode)) END;
      lolm: BEGIN
              i1 := decode;
              p := getCodeByte;
              i2 := base(p) + decode; { src }
              FOR i3 := 0 TO i1-1 DO data[sp+i3] := data[i2+i3];
              sp := sp + i1;
              alignstack;
              IF monStackHeap THEN
                IF np - sp < minStackHeap THEN
                  minStackHeap := np - sp
            END;
      loib: BEGIN popWord(j); pushWord(getByte(j + decode)) END;
      loib0: BEGIN popWord(j); pushWord(getByte(j)) END;
      loiw: BEGIN popWord(j); loadWord(j + decode) END;
      loiw0: BEGIN popWord(j); loadWord(j) END;
      loid: BEGIN popWord(j); pushDubl(getDubl(j + decode)) END;
      loid0: BEGIN popWord(j); pushDubl(getDubl(j)) END;
      loim: BEGIN
              i1 := decode; { # of bytes }
              popWord(i2);  { src ptr }
              i2 := i2 + decode; { src = src ptr + offset }
              FOR i3 := 0 TO i1-1 DO data[sp+i3] := data[i2+i3];
              sp := sp + i1;
              alignstack;
              IF monStackHeap THEN
                IF np - sp < minStackHeap THEN
                  minStackHeap := np - sp
            END;
      loim0: BEGIN
               i1 := decode; { # of bytes }
               popWord(i2);  { src ptr }
               FOR i3 := 0 TO i1-1 DO data[sp+i3] := data[i2+i3];
               sp := sp + i1;
               alignstack;
              IF monStackHeap THEN
                IF np - sp < minStackHeap THEN
                  minStackHeap := np - sp
             END;
      locw: BEGIN pushWord(getCodeInt(pc)); pc := pc + wordsize END;
      locq: pushWord(decode);
      loc0: pushWord(0);
      loc1: pushWord(1);
      stgb: BEGIN popWord(i1); putByte(decode, i1) END;
      stgw: BEGIN popWord(i); putWord(decode, i) END;
      stgd: BEGIN popDubl(r1); putDubl(decode, r1) END;
      stgm: BEGIN
              i1 := decode;   { length }
              i2 := decode;   { destination }
              j := bytecard(i1) MOD stackal;
              IF j > 0 THEN
                sp := sp - (wordsize - j); { pop any padded zeros }
              sp := sp - i1;
              FOR i3 := 0 TO i1-1 DO data[i2+i3] := data[sp+i3];
            END;
      stlb: BEGIN
              p := getCodeByte;
              popWord(i);
              IF (i < 0) OR (i > 255) THEN
                InterpError('bad Boolean or char value');
              j := decode;
              IF j = 0 THEN { storing function return byte }
                putWord(base(p), i) { (Boolean or char); zero rest of word }
              ELSE
                putByte(base(p) + j, i)
            END;
      stlw: BEGIN p := getCodeByte; popWord(i); putWord(base(p) + decode, i) END;
     stlw0: BEGIN popWord(i); putWord(fp + decode, i) END; 
      stld: BEGIN p := getCodeByte; popDubl(r1); putDubl(base(p) + decode, r1) END;
      stlm: BEGIN
              i1 := decode;   { length }
              p := getCodeByte;
              i2 := base(p) + decode; { destination }
              j := bytecard(i1) MOD stackal;
              IF j > 0 THEN
                sp := sp - (wordsize - j); { pop any padded zeros }
              sp := sp - i1;
              FOR i3 := 0 TO i1-1 DO data[i2+i3] := data[sp+i3];
            END;
      stib: BEGIN i := decode; popWord(i1); popWord(j); putByte(j+i, i1) END;
      stib0:BEGIN              popWord(i1); popWord(j); putByte(j  , i1) END;
      stiw: BEGIN i := decode; popWord(i1); popWord(j); putWord(j+i, i1) END;
      stiw0:BEGIN              popWord(i1); popWord(j); putWord(j  , i1) END;
      stid: BEGIN i := decode; popDubl(r1); popWord(j); putDubl(j+i, r1) END;
      stid0:BEGIN              popDubl(r1); popWord(j); putDubl(j  , r1) END;
      stim: BEGIN
              i1 := decode;
              { we need to get destination (i2) from UNDERNEATH the set on TOS }
              j := bytecard(i1) MOD stackal;
              IF j > 0 THEN
                sp := sp - (wordsize - j); { pop any padded zeros }
              sp := sp - i1 - wordsize;
              i2 := getWord(sp);       { address of final destination }
              i2 := i2 + decode;       { offset }
              FOR i3 := 0 TO i1-1 DO data[i2+i3] := data[sp+wordsize+i3];
            END;
      stim0:BEGIN
              i1 := decode;
              { we need to get destination (i2) from UNDERNEATH the set on TOS }
              j := bytecard(i1) MOD stackal;
              IF j > 0 THEN
                sp := sp - (wordsize - j); { pop any padded zeros }
              sp := sp - i1 - wordsize;
              i2 := getWord(sp);       { address of final destination }
              FOR i3 := 0 TO i1-1 DO data[i2+i3] := data[sp+wordsize+i3];
            END;
      iand: BEGIN
              i1 := decode; { # of bytes in each operand }
              i4 := i1;     { save a copy }
              j := bytecard(i1) MOD stackal;
              IF j > 0 THEN
                i1 := i1 + wordsize - j; { i1, aligned upward }
              sp := sp - i1; { "pop" the rhs set (sp now points to it) }
              i2 := sp - i1; { i2 now points at lhs set }
              FOR i3 := 0 TO i4-1 DO { lhs := lhs * rhs }
                putByte(i2+i3, getByte(i2+i3) AND getByte(sp+i3));
            END;
      ior:  BEGIN
              i1 := decode; { # of bytes in each operand }
              i4 := i1;     { save a copy }
              j := bytecard(i1) MOD stackal;
              IF j > 0 THEN
                i1 := i1 + wordsize - j; { i1, aligned upward }
              sp := sp - i1; { "pop" the rhs set (sp now points to it) }
              i2 := sp - i1; { i2 now points at lhs set }
              FOR i3 := 0 TO i4-1 DO { lhs := lhs + rhs }
                putByte(i2+i3, getByte(i2+i3) OR getByte(sp+i3));
            END;
      flt2: BEGIN popDubl(r1); popWord(i1); pushDubl(i1); pushDubl(r1) END;
      inot: BEGIN popWord(i1);  pushWord(ord(i1 = 0)) END;
      addi: BEGIN popWord(i2); popWord(i1); pushWord(i1+i2) END;
      subi: BEGIN popWord(i2); popWord(i1); pushWord(i1-i2) END;
      muli: BEGIN popWord(i2); popWord(i1); pushWord(i1*i2) END;
      divi: BEGIN popWord(i2); popWord(i1);
              IF i2 = 0 THEN interpError('Zero divide              ');
              pushWord(i1 DIV i2)
            END;
      negi: BEGIN popWord(i1);  pushWord(-i1) END;
      absi: BEGIN popWord(i1);  pushWord(abs(i1)) END;
      sqri: BEGIN popWord(i1);  pushWord(sqr(i1)) END;
      modi: BEGIN popWord(i2); popWord(i1); pushWord(i1 MOD i2) END;
      addr: BEGIN popDubl(r2); popDubl(r1); pushDubl(r1+r2) END;
      subr: BEGIN popDubl(r2); popDubl(r1); pushDubl(r1-r2) END;
      mulr: BEGIN popDubl(r2); popDubl(r1); pushDubl(r1*r2) END;
      divr: BEGIN popDubl(r2); popDubl(r1);
              IF r2 = 0.0 THEN interpError('Zero divide              ');
              pushDubl(r1/r2)
            END;
      negr: BEGIN popDubl(r1); pushDubl(-r1) END;
      absr: BEGIN popDubl(r1); pushDubl(abs(r1)) END;
      sqrr: BEGIN popDubl(r1); pushDubl(sqr(r1)) END;
      flt:  BEGIN popWord(i1); pushDubl(i1) END;
      trc:  BEGIN popDubl(r1); pushWord(trunc(r1)) END;
      rnd:  BEGIN popDubl(r1); pushWord(round(r1)) END;
      incw: incWord(sp - wordsize, decode);
      decw: decWord(sp - wordsize, decode);
      icgw: BEGIN q := decode; putWord(q, getWord(q)+1) END;
      iclw: BEGIN p := getCodeByte; incWord(base(p) + decode, 1) END;
      dcgw: BEGIN q := decode; putWord(q, getWord(q)-1) END;
      dclw: BEGIN p := getCodeByte; decWord(base(p) + decode, 1) END;
      { the following six p-codes are for comparing scalars (and reals) and sets
        (for equal and not equal; other relationals can be subset tests) }
      equ:  BEGIN
              q := decode;
              IF q <= 4 THEN
                BEGIN popWord(j); popWord(i); pushWord(ord(i=j)) END
              ELSE
              IF q = 8 THEN
                BEGIN popDubl(r2); popDubl(r1); pushWord(ord(r1=r2)) END
              ELSE
                BEGIN
                  i1 := q;
                  popSet(s2, i1); popSet(s1, i1);
                  pushWord(ord(s1=s2))
                END
            END;
      neq:  BEGIN
              q := decode;
              IF q <= 4 THEN
                BEGIN popWord(j); popWord(i); pushWord(ord(i<>j)) END
              ELSE
              IF q = 8 THEN
                BEGIN popDubl(r2); popDubl(r1); pushWord(ord(r1<>r2)) END
              ELSE
                BEGIN
                  i1 := q;
                  popSet(s2, i1); popSet(s1, i1);
                  pushWord(ord(s1<>s2))
                END
            END;
      leq:  BEGIN
              q := decode;
              IF q <= 4 THEN
                BEGIN popWord(i2); popWord(i1); pushWord(ord(i1<=i2)) END
              ELSE
              IF q = 8 THEN
                BEGIN popDubl(r2); popDubl(r1); pushWord(ord(r1<=r2)) END;
            END;
      geq:  BEGIN
              q := decode;
              IF q <= 4 THEN
                BEGIN popWord(i2); popWord(i1); pushWord(ord(i1>=i2)) END
              ELSE
              IF q = 8 THEN
                BEGIN popDubl(r2); popDubl(r1); pushWord(ord(r1>=r2)) END;
            END;
      les:  BEGIN
              q := decode;
              IF q <= 4 THEN
                BEGIN popWord(j); popWord(i); pushWord(ord(i<j)) END
              ELSE
              IF q = 8 THEN
                BEGIN popDubl(r2); popDubl(r1); pushWord(ord(r1<r2)) END
            END;
      gtr:  BEGIN
              q := decode;
              IF q <= 4 THEN
                BEGIN popWord(j); popWord(i); pushWord(ord(i>j)) END
              ELSE
              IF q = 8 THEN
                BEGIN popDubl(r2); popDubl(r1); pushWord(ord(r1>r2)) END
            END;
      inn:  BEGIN
              i2 := decode;
              popSet(s1, i2);
              popWord(i1);
              pushWord(ord(i1 IN s1))
            END;
      fjp:  BEGIN
              j := getCode2byte(pc);
              pc := pc + labsize;
              popWord(i);
              IF i = 0 THEN pc := pc + j;
            END;
      { the following six p-codes are for comparing arrays (including strings)
        and records (for equal and not equal) }
      equm: BEGIN q := decode; compare; pushWord(ord(b)) END;
      neqm: BEGIN q := decode; compare; pushWord(ord(NOT b)) END;
      geqm: BEGIN
              q := decode; compare;
              pushWord(ord(b OR (data[i1+i] >= data[i2+i])))
            END;
      leqm: BEGIN
              q := decode; compare;
              pushWord(ord(b OR (data[i1+i] <= data[i2+i])))
            END;
      gtrm: BEGIN
              q := decode; compare;
              pushWord(ord(NOT b AND (data[i1+i] > data[i2+i])))
            END;
      lesm: BEGIN
              q := decode; compare;
              pushWord(ord(NOT b AND (data[i1+i] < data[i2+i])))
            END;
      tjp:  BEGIN
              j := getCode2byte(pc);
              pc := pc + labsize;
              popWord(i);
              IF i <> 0 THEN pc := pc + j;
            END;
      ujc:  interpError('CASE - error             ');
      xjp:  BEGIN
              popWord(i1); { CASE expression value }
              pc := pc + getCode2byte(pc + i1*bralen)
            END;
      sjp:  BEGIN
              j := getCode2byte(pc); pc := pc + labsize;
              popWord(i2); { length of sparse CASE table }
              popWord(i1); { CASE expression value }
              ca := pc;
              i3 := getCode2byte(ca); { 1st table entry }
              WHILE (i2 > 0) AND_THEN (i3 <> i1) DO
                BEGIN
                  i2 := i2 - 1;
                  ca := ca + 4;
                  i3 := getCode2byte(ca);
                END;
              IF i2 = 0 THEN pc := pc + j { error/otherwise }
                        ELSE pc := pc + getCode2byte(ca+2)
            END;
      cas:  BEGIN
              q := decode;
              j := getCode2byte(pc); pc := pc + labsize;
              i1 := getWord(sp-wordsize); { CASE expr value on TOS }
              IF (i1 < getWord(q)) OR (i1 > getWord(q+wordsize)) THEN
                pc := pc + j; { error/otherwise branch }
            END;
      chk0: BEGIN
              a1 := getWord(sp-adrsize); { ptr on TOS }
              IF (a1 >= np) AND (a1 < constPool) THEN { in the heap }
                IF isfree(a1) THEN
                  { attempt to dereference or assign a freed block }
                  interpError('Ptr used after dispose')
                ELSE { pointer is in heap and not dispose'd: OK }
              ELSE
              IF a1 = nilval THEN
              ELSE
              IF a1 = 0 THEN { ptr never been assigned }
                interpError('uninitialized pointer    ')
              ELSE { leftovers: corrupted for some reason }
                BEGIN
                  dx(a1); { diagnose before crapping out }
                  interpError('chka: bad pointer value  ')
                END
            END;
      chk1: BEGIN
              a1 := getWord(sp-adrsize); { ptr on TOS }
              IF (a1 >= np) AND (a1 < constPool) THEN { in the heap }
                IF isfree(a1) THEN
                  { attempt to dereference or assign a freed block }
                  interpError('Ptr used after dispose')
                ELSE { pointer is in heap and not dispose'd: OK }
              ELSE
              IF a1 = nilval THEN
                interpError('Dereference of nil ptr   ')
              ELSE
              IF a1 = 0 THEN { ptr never been assigned }
                interpError('uninitialized pointer    ')
              ELSE { leftovers: corrupted for some reason }
                BEGIN
                  dx(a1); { diagnose before crapping out }
                  interpError('chka: bad pointer value  ')
                END
            END;
      chk2: BEGIN
              q := decode;
              i1 := getWord(sp-wordsize);
              IF (i1 < getWord(q)) OR (i1 > getWord(q+wordsize)) THEN
                BEGIN
                  writeln('val: ',i1:1,' not in (',getWord(q):1,',',
                          getWord(q+wordsize):1,'); pc = ',pc:1);
                  interpError('value out of range       ')
                END
            END;
      lag:  pushWord(decode);
      lal:  BEGIN
              p := getCodeByte;
              i1 := decode;
              pushWord(base(p) + i1)
            END;
      loca: pushWord(decode);
      lpa:  BEGIN { place procedure desc (entry & static link) on stack }
              p := getCodeByte;
              q := getCodeAddr(pc);
              pc := pc + labsize;
              pushWord(base(p));      { static link }
              pushWord(q);
            END;
      lip:  BEGIN p := getCodeByte; ad := base(p) + decode;
              i := getWord(ad); a1 := getWord(ad + adrsize);
              pushWord(i); pushWord(a1)
            END;
      ixa:  BEGIN popWord(i); popWord(j); pushWord(decode*i+j) END;
      ixca: BEGIN { index conformant array }
              popWord(i); { array descriptor }
              i1 := getWord(i);                   { lo bound }
              i2 := getWord(i+wordsize);          { hi bound }
              i3 := getWord(i+wordsize+wordsize); { row size }
              popWord(j); { subscript }
              IF (j < i1) OR (j > i2) THEN
                interpError('subscript outside bounds ');
              i4 := (j - i1) * i3;
              { replace TOS (addr of array) with addr(array[subscript]) }
              putWord(sp-wordsize, getWord(sp-wordsize)+i4)
            END;
      iodd: BEGIN popWord(i1); pushWord(ord(odd(i1))) END;
      eofp: BEGIN
              popWord(j); valfil(j); fn := data[j];
              IF fn = inputfn THEN
                pushWord(ord(eof(input)))
              ELSE
              IF fn = outputfn THEN
                interpError('eof test on output file  ')
              ELSE
                IF filstate[fn] = fwrite THEN
                  pushWord(ord(true))
                ELSE
                IF filstate[fn] = fread THEN
                  pushWord(ord(eof(filtable[fn]) AND NOT filbuff[fn]))
                ELSE
                  interpError('file is not open         ')
            END;
      dif:  BEGIN
              i1 := decode; { # of bytes in each operand }
              popSet(s2, i1); popSet(s1, i1);
              pushSet(s1-s2, i1)
            END;
      sgs:  BEGIN
              i1 := decode; { # of bytes in set }
              popWord(i3);  { value placed into singleton set }
              s1 := [ i3 ];
              pushSet(s1, i1);
            END;
      rgs:  BEGIN
              i1 := decode;  { # of bytes in set }
              popWord(i3); popWord(i2);
              s1 := [i2..i3];
              pushSet(s1, i1)
            END;
      bra:  pc := pc + labsize + getCode2byte(pc);
      ipj:  BEGIN
              a1 := fp; { save this off, before establishing new fp }
              p := getCodeByte; { static level of target GOTO label }
              fp := base(p); { find the static link for the target }
              REPEAT
                sp := a1;
                a1 := getWord(a1+markdl); { follow dynamic links }
              UNTIL a1 = fp; { a1 points to beginning of target stack frame }
                { however, sp now points to the beginning of NEXT stack frame }
              j := getCode2byte(pc);
              pc := pc + labsize + j; { AND jump . . . }
            END;
      dmp:  sp := sp - adrsize; { pop top of stack }
      dup:  BEGIN { make a second copy on the TOS }
              {pushWord(getWord(sp - wordsize))} {a.k.a. loadWord(sp-wordsize) }
              { caution: this optimization assumes wordsize = 4 }
              data[sp  ] := data[sp-4];
              data[sp+1] := data[sp-3];
              data[sp+2] := data[sp-2];
              data[sp+3] := data[sp-1];
              sp := sp + wordsize;
            END;
      swp:  swapTop(decode);
      cal:  BEGIN (* 1st q = # of bytes of params, 2nd q = entry point*)
              { # of bytes of args no longer limited to 255 }
              fp := sp-(decode+marksize); { pointer to base of mark }
              j := getCode2byte(pc); pc := pc + labsize;
              { we got the entry point; now store the ra & set the new pc }
              putWord(fp+markra, pc); { place return addr }
              IF pc + j < 0    THEN pc := pc + (j + t16)
              ELSE
              IF pc + j >= t16 THEN pc := pc + (j - t16)
                               ELSE pc := pc + j; { self-relative jump }
            END;
      cali: BEGIN   { call formal procedure }
              p := getCodeByte;
              popWord(j);  { address of the static link & entry point }
              fp := sp-(p+marksize); { there is a mark stack here }
              { replace next link fp with the one for the target }
              putWord(fp+marksl, getWord(j));  { store static link }
              putWord(fp+markra, pc);          { store return addr }
              pc := getWord(j + adrsize)       { and . . . jump }
            END;
      csp:  BEGIN q := decode; callsp END;
      mst:  BEGIN (*p=level of calling procedure minus level of called
                    procedure + 1;  set dl and sl, increment sp*)
              p := getCodeByte;
              { 0 for up one level; 1 for same level; 2 for down 1 level }
              ad := sp; { the new mark is being pushed on top of the stack }
              sp := sp + marksize; { 20 }
              putWord(ad + marksl, base(p)); { sl }  { store the static link }
              putWord(ad + markdl, fp); { dl }       { store the dynamic link }
            END;
      ent:  BEGIN
              q := getCodeAddr(pc);
              pc := pc + labsize; { unencoded q field }
              sp := fp + q; (* q = length of frame, gets fixed up *)
              IF monStackHeap THEN
                IF np - sp < minStackHeap THEN
                  minStackHeap := np - sp
            END;
      ret0: BEGIN
              sp := fp;
              pc := getWord(fp+markra); { get ra }
              fp := getWord(fp+markdl)  { get dl }
            END;
      ret1: BEGIN { function returns an integer-sized result }
              sp := fp+wordsize; { set stack above function result }
              pc := getWord(fp+markra);
              fp := getWord(fp+markdl)
            END;
      ret2: BEGIN { function returns an double-sized result }
              sp := fp+dublsize; { set stack above function result }
              pc := getWord(fp+markra);
              fp := getWord(fp+markdl)
            END;
      mov:  BEGIN
              q := decode; { # of bytes }
              popWord(i2); { src }
              popWord(i1); { dest }
              FOR i3 := 0 TO q-1 DO data[i1+i3] := data[i2+i3]
            END;
      copy: BEGIN
              { In particular, 'copy': }
              { 1. pops the address and then the size }
              { 2. de-references address to get the REAL start of actual array }
              { 3. copies the array from 'src' into SP for 'size' }
              { 4. copies SP into src }
              { 5. adds size into SP }
              popWord(i2); { src }
              popWord(i4); { # of bytes }
              a1 := getWord(i2); { de-reference i2 to get the real address }
              FOR i3 := 0 TO i4-1 DO data[sp+i3] := data[a1+i3];
              putWord(i2, sp); { address of copied array }
              sp := sp + i4; { update stack to include copy in local frame }
              alignstack;
              IF monStackHeap THEN
                IF np - sp < minStackHeap THEN
                  minStackHeap := np - sp
            END;
      fbv:  BEGIN
              ad := getWord(sp-adrsize); valfil(ad); fn := data[ad];
              IF fn = inputfn THEN
                putByte(ad+fileidsize, ord(input^))
              ELSE
              IF filstate[fn] = fread THEN
                putByte(ad+fileidsize, ord(filtable[fn]^))
            END;
      fvb:  BEGIN popWord(i);
              ad := getWord(sp-adrsize);
              valfil(ad); fn := data[ad];
              { load buffer only if in read mode, and buffer is empty }
              IF (filstate[fn] = fread) AND NOT filbuff[fn] THEN
                BEGIN
                  FOR j := 1 TO i DO
                    read(bfiltable[fn], data[ad+fileidsize+j-1]);
                  filbuff[fn] := true
                END
            END;
      efb:  BEGIN
              ad := getWord(sp-adrsize); valfil(ad);
              fn := data[ad];
              { eof is file eof, and buffer not full }
              pushWord(ord(eof(bfiltable[fn]) AND NOT filbuff[fn]))
            END;
      pack: BEGIN q := decode; q1 := decode; popWord(i3); popWord(i2); popWord(i1);
              IF i2 + q > q1 THEN interpError('pack elements out of bnds');
              FOR i4 := 0 TO q-1 DO
                BEGIN data[i3 + i4] := data[i1 + i2]; i2 := i2 + 1 END
            END;
      unpk: BEGIN q := decode; q1 := decode; popWord(i3); popWord(i2); popWord(i1);
              IF i3 + q > q1 THEN interpError('unpack elem out of bnds  ');
              FOR i4 := 0 TO q-1 DO
                BEGIN data[i2 + i3] := data[i1 + i4]; i3 := i3 + 1 END
            END;
      pop0: BEGIN
              popWord(i);
              IF i <> 0 THEN interpError('set elements lost');
            END;
      cmln: BEGIN { address of program param 'commandline' is TOS-1 }
              popWord(j);     { size is TOS } { (usually 255) }
              newspc(j, ad); { 2nd arg is addr of alloc'd space }
              popWord(j); putWord(j, ad);{ store into 'commandline' }
              copyCmndLine
            END;
      asgn: BEGIN popWord(j); popWord(i); str := getStr(j, i);
              popWord(i);
              valfil(i); fn := data[i];
              assign(filtable[fn], str+'.txt')
            END;
      locr: pushDubl(getDubl(decode));
      locs: BEGIN
              i1 := decode; { length }
              i4 := i1;     { save a copy }
              j := bytecard(i1) MOD stackal;
              IF j > 0 THEN
                i1 := i1 + wordsize - j; { i1, aligned upward }
              i2 := decode; { source address (in const pool) }
              FOR i3 := 0 TO i1-1 DO
                IF i3 < i4 THEN
                  data[sp+i3] := data[i2+i3]
                ELSE
                  data[sp+i3] := 0; { pad remainder of stack word w/ zeros }
              sp := sp + i1;
              IF monStackHeap THEN
                IF np - sp < minStackHeap THEN
                  minStackHeap := np - sp
            END;
      sst1: BEGIN
              i1 := decode;
              popSet(s2, i1); popSet(s1, i1);
              pushWord(ord(s1 <= s2))
            END;
      sst2: BEGIN
              i1 := decode;
              popSet(s2, i1); popSet(s1, i1);
              pushWord(ord(s1 >= s2))
            END;
      srcl: BEGIN
              sourceLine := decode; { and in case of run-time error }
{             IF sourceLine = startdebugat THEN
                debug := true;
}             IF doTraceSource AND NOT dotrcins THEN
                write('S l ex: ', sourceLine:1)
            END;
      fatl: writeln('Fatal error(s) prevents execution of this pcode file');
      stop: ;
      lastop: { yup, this is the last stop};
    END; (* CASE op *)
    IF dotrcins {AND debug} AND (pc > minpctrc) THEN
      BEGIN
        write(' TOS = ');
        writeHex(getWord(sp-wordsize), 8);
        writeln;
      END;
  UNTIL op = stop;

  1 : { abort run }

{  writeln;}
  writeln('intepreter finished');
  IF monStackHeap THEN
    writeln('Stack & heap space used: ',constPool-minStackHeap:1,
            '; smallest free area was ',minStackHeap:1);
  IF dynaStats THEN
    BEGIN
      FOR op1 := succ(invd) TO pred(lastop) DO
        FOR op2 := succ(invd) TO pred(lastop) DO
          IF dystats[op2].tot < dystats[succ(op2)].tot THEN
            BEGIN
              dystats[invd] := dystats[op2];
              dystats[op2] := dystats[succ(op2)];
              dystats[succ(op2)] := dystats[invd];
            END;
        
      FOR op1 := succ(invd) TO pred(lastop) DO
        WITH dystats[op1] DO
          IF tot <> 0 THEN
            writeln(ord(op1):3,' ',mn[inx], tot:9, (tot*100)/total:6:2, '%');
    END;
  IF dodmpspc THEN
    repspc;
END.
