(*$d-,l-,t-*)
PROGRAM pascalP6compiler(input, output, p6);

  LABEL 98,99; { terminate immediately: eof & internal error, respectively }

  CONST

    { see p4-p5.html & p5-p6.html for the derivation of these sources

    Program object sizes and characteristics, sync with pint. These define
    the machine specific characteristics of the target.

    This configuration is for a 32 bit machine as follows:

    integer               32  bits
    real                  64  bits
    char                  8   bits
    Boolean               8   bits
    set                   256 bits
    pointers              32  bits
    marks                 160 bits
    File logical number   8   bits

    Both endian types are supported. There is no alignment needed, but you
    may wish to use alignment to tune the runtime speed.

    The machine characteristics dependent on byte accessable machines. This
    table is all you should need to adapt to any byte addressable machine.

    }
    { and now we switch from bits to bytes: }
    intsize     =        4;  { size of integer }
    intal       =        4;  { memory alignment of integer }
    realsize    =        8;  { size of real }
    realal      =        4;  { memory alignment of real }
    charsize    =        1;  { size of char }
    charal      =        1;  { memory alignment of char }
    boolsize    =        1;  { size of Boolean }
    boolal      =        1;  { alignment of Boolean }
    ptrsize     =        4;  { size of pointer }
    adrsize     =        4;  { size of address }
    adral       =        4;  { alignment of address }
    setsize     =       32;  { max size of set, & size of constructers }
    setal       =        1;  { alignment of set }
    filesize    =        1;  { required runtime space for file (lfn) }
    fileidsize  =        1;  { size of the logical file number only }
    stackal     =        4;  { alignment of stack }
    stackelsize =        4;  { stack element size }
    heapal      =        4;  { alignment for each heap object }
    { miscellaneous sizes and limits }
    sethigh     =      255;  { Sets can have up to 256 elements }
    setlow      =        0;
    ordmaxchar  =      255;  { Characters are 8 bit ISO/IEC 8859-1 }
    ordminchar  =        0;  {  }
    EOFchar     = 127; { when eof(input) is detected, }
                       { nextch returns ch = chr(EOFchar), and }
                       { then a "bad" GOTO to label 98 }
    rbrace      = '}'; { so rbrace can be commented out, if desired (like this!)}
    {maxresult   = realsize;   maximum size of function result }
    lcaftermarkstack =  20;  { maxresult+3*ptrsize }
    { Value of nil is 1 because this allows checks for pointers that were
      initialized, which would be zero (since we clear all space to zero).
      In the new unified code/data space scheme, 0 and 1 are always invalid
      addresses, since the startup code is at least that long. }
    nilval      =        1;  { value of 'nil' }

    { end of pcom and pint common parameters }

    displimit   = 30;
    maxlevel    = displimit;
    fileal      = charal;
    parmal      = stackal;
    parmsize    = stackelsize;
    recal       = stackal;
    maxaddr     = maxint;
    maxsp       = 39;  { number of standard procedures/functions }
    maxstd      = 39;  { number of standard identifiers }
    varsqt      = 12;  { variable string quantum length }
    reslen      = 10;  { maximum length of reserved words }
    { increase to 10 so that string types match for comparisons }

    { version numbers }

    majorver    = 1; { major version number }
    minorver    = 0; { minor version number }

{$ifdef __GPC__}
    maxCondComp = 10; { max nesting level for conditional compilation }
{$endif}
    maxErrNo    = 200;
    minWarnRng  = 81;
    maxWarnRng  = 89;
    maxlabel    = 9999;  { ISO 7185 label value limit (clause 6.1.6) }
    sparseCase  = 4; { CASE is sparse if largest-smallest > totalCases*sparseCase }
    minCaseLab  = -32767; { These can be set higher, but it seems prudent }
    maxCaseLab  = 32767;  { to set some reasonable range }
    
    { default field sizes for text output: }
    defaultIntField = 1;
    defaultRealField = 22;
    defaultCharField = 1;
    defaultBoolField = 5;

  TYPE
    symbol   = (imul,rdiv,idiv,imod,andop,plus,minus,orop,ltop,leop,
          geop,gtop,neop,eqop,inop,ident,intconst,realconst,strngconst,notop,
          lparen,rparen,lbrack,rbrack,comma,semicolon,period,arrow,colon,becomes,
          range,labelsy,constsy,typesy,varsy,funcsy,progsy,procsy,setsy,packedsy,
       arraysy,recordsy,filesy,beginsy,ifsy,casesy,repeatsy,whilesy,forsy,withsy,
          gotosy,endsy,elsesy,untilsy,ofsy,dosy,tosy,downtosy,thensy,nilsy,
          cmtEndsy, { 61 } illegalsy);
    setofsys = SET OF symbol; { fits in 8 bytes }
{    relopsy  = ltop..eqop;}
    setofchr = SET OF char;
    quantum  = PACKED ARRAY [1..varsqt] OF char;
    strllp   = ^strll; { pointer to linked list of quanta for identifier }
    strll    = RECORD { a bunch (usually one) of strs make an identifier }
                 str:   quantum;
                 next:  strllp
               END;

    str4     = PACKED ARRAY [1..4] OF char;

    compRslt = (strLT, strEQ, strGT);
                { determine =, <, or > with a single compare }
     
                                                            (*constants*)
                                                            (*.........*)
    setty    = SET OF setlow..sethigh;
    cstclass = (reel,pset,strg);
    addrrange = 0..maxaddr;
    konstp   = ^konstant;    { info for 'big' (non-ordinal) constants }
    konstant = RECORD        { at each display level, these guys are linked }
                 next: konstp; { next entry link }
                 CASE cclass: cstclass OF
                   reel: (rval: strllp);
                   pset: (pval: setty; size: addrrange);
                   strg: (slgth: integer; sval: strllp)
               END;

    valu     = RECORD
                 CASE intval: Boolean OF
                    true:  (ival: integer);
                    false: (valp: konstp)
               END;

                                                           (*data structures*)
                                                           (*...............*)
    levrange = 0..maxlevel;
    structform = (scalar, subrange, confInx, pointer, powerset, arrays,
                  records, files, tagfld, variant);
    declkind = (predclrd, expdclrd);
    tIptr    = ^typeInfo;
    sTptr    = ^symbTabl;

    typeInfo = RECORD
                 next: tIptr; { next entry link }
                 size: addrrange;
                 packing: Boolean; { packing status }
                 CASE form: structform OF
                   scalar:   (CASE scalkind: declkind OF
                                expdclrd: (fenum: sTptr);
                                predclrd: ( )
                             );
                   subrange: (rangetype: tIptr; min,max: valu);
                   confInx:  (lobnd,hibnd: sTptr; TidOrd: tIptr);
                   pointer:  (eltype: tIptr);
                   powerset: (elset: tIptr; matchpack: Boolean);
                   arrays:   (aeltype,inxtype: tIptr);
                   records:  (fstfld: sTptr; recvar: tIptr; recyc: tIptr);
                   files:    (compType: tIptr);
                   tagfld:   (tagfieldp: sTptr; fstvar: tIptr);
                   variant:  (prvcon,subvar: tIptr; varval: integer)
               END;

                                                            (*names*)
                                                            (*.....*)

    idclass  = (types,konst,vars,field,proc,func);
    setofids = SET OF idclass;
    idkind   = (actual,formal);
    symbTabl = RECORD
                 name: strllp; llink, rlink: sTptr;
                 idtype: tIptr; next: sTptr; keep: Boolean;
                 addr: addrrange; { not used by types or konst, but }
                                  { field, vars, proc & func use it }
                 CASE klass: idclass OF
                   types: ();
                   field: (selectr: Boolean);
                   konst: (values: valu);
                   vars:  (vkind: idkind; vlev: levrange; 
                           vbound, readonly: Boolean);
                   proc,
                   func:  (params: sTptr; { param list }
                           CASE pfdeckind: declkind OF
                             predclrd: (key: 1..18);
                             expdclrd: (pflev: levrange;
                                        pfname: integer;
                                        result: Boolean;
                                        CASE pfkind: idkind OF
                                          actual: (forwdecl,
                                                   externl: Boolean);
                                          formal: ()
                                       )
                          )
               END;

    disprange = 0..displimit;
    where    = (blck,crec,vrec,rec);

                                                            (*expressions*)
                                                            (*...........*)
    attrkind = (cst,varbl,expr);
    vaccess  = (drct,indrct);

    attr     = RECORD
                 typtr: tIptr;
                 CASE kind: attrkind OF
                   cst:   (cval: valu);
                   varbl: (formantBound, loopvar, varsel, packedcompon: Boolean;
                           CASE access: vaccess OF
                             drct:   (vlevel: levrange; dplmt: addrrange);
                             indrct: (idplmt: addrrange); { ptr on TOS }
                          );
                   expr: ()
               END;

                                                                 (*labels*)
                                                                 (*......*)
    lbp      = ^labl;
    labl     = RECORD { 'goto' label }
                 next: lbp;         { next list link }
                 undeclared,
                 defined: Boolean;  { label defining point was seen }
                                    { where it attached to statement }
                 maxNesting: integer; { subsequent use in GOTO must be at }
                                    { level not exceeding this }
                 labval,            { numeric value of label }
                 labname: integer;  { internal sequental name of label }
                 vlevel: levrange;  { static level of definition }
               END;

    { case statement tracking entries } { can be used for sparse sets, as well }
    cip      = ^caseinfo;
    caseinfo = RECORD
                 next: cip;
                 csstart: integer;
                 cslab: integer
               END;

    prologp  = ^prolog;
    prolog   = RECORD offset: addrrange; id: strllp; next: prologp END;
    
    errnoType = 1..maxErrNo;
{$ifdef __GPC__}

    condCompRng = 0..maxCondComp;

    condMode  = (parsing, skipping);
    condCompT = RECORD
                  mode: condMode;
                  branch: (iffing, elsing);
                END;
{$endif}
                
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
                 lag,  lal,  lpa,  lip,  ixa,  ixca, iodd, fjp,
                 eofp, sgs,  rgs,  bra,  ipj,  dmp,  dup,  swp,
                 cal,  cali, csp,  mst,  ent,  ret0, ret1, ret2,
                 mov,  copy, fbv,  fvb,  efb,  pack, unpk, pop0,
                 cmln, asgn, srcl, fatl, stop, lastop);
    storerng   = stgb..stim; { global, local & indirect }
    comrelop   = equ..gtr;
(*-------------------------------------------------------------------------*)

  VAR
    sy: symbol;                     (* kind of token scanned *)
    val: valu;                      (* value of last constant *)
    lgth: integer;                  (* length of last string constant *)
    idll: strllp;                   (* id scanned by insymbol as a linked list *)
    kk: integer;                    (* nr of chars in last identifier *)
    ch: char;                       (* last character *)

    chcnt: integer;                 { column where an error occurred }
    lc: addrrange;                  (*data location counter*)
    linecount: integer;

    eolseen,                        { eoln(input): nextch positions to start of }
                                    { next line, but endofline not called yet }
    listing: Boolean;               (*output options for -- source listing *)
    debug: Boolean;                 { emitting run-time error-checking code }
    hash: ARRAY ['a'..'z'] OF integer; { hash values to detect reserved words }
    
    letter,digit,letordig: setofchr;
                                    (*pointers:*)
                                    (*.........*)
    parmptr,
    intptr,realptr,charptr,
    boolptr,nilptr,textptr: tIptr;  (*pointers to entries of predclrd ids*)
    utypptr,ucstptr,uvarptr,
    ufldptr,uprcptr,ufctptr,        (*pointers to entries for undeclared ids*)
    fwptr: sTptr;                   (*head of chain of forw decl type ids*)
    inputptr,outputptr: sTptr;      (*pointers to pre-declared textfiles*)
                                    (*bookkeeping of declaration levels:*)
                                    (*..................................*)

    level: levrange;                (*current static level*)
    statLevFnd: integer;            (*level where idll was found; or -1 if not*)
    top: disprange;                 (*top of display*)

    display:                        (*where:   means:*)
      ARRAY [disprange] OF
        RECORD                      (*=blck:   id is variable id*)
          fname: sTptr; flabel: lbp;(*=crec:   id is field id in record with*)
          fconst: konstp; ftype: tIptr;
          forwProcFunc: sTptr;      (* keep track of forward declared routines *)
          CASE occur: where OF      (*         constant address*)
            blck: (bname: sTptr);   (* block name *)
            crec: (clev: levrange;  (*=vrec:   id is field id in record with*)
                  cdspl: addrrange);(*         run-time address*)
            vrec: (vdspl,offset: addrrange); { offset accumulates field offsets}
            rec: ()
        END;                        (* --> procedure withstatement*)


                                    (*error messages:*)
                                    (*...............*)

    errinx:   0..10;                  (*nr of errors in current source line*)
    errlist:  ARRAY [1..10] OF
                RECORD
                  pos: integer;
                  nmr: errnoType
                END;

                                    (*expression compilation:*)
                                    (*.......................*)

    gattr: attr;                    (*describes the expr currently compiled*)
    setLimit: integer;              { impose this limit on set expressions }
       { OK for sets on stack to exceed this for stack alignment purposes; }
       { for 'el IN [expr1,...]', OK for constructer to be 256 }

                                    (*structured constants:*)
                                    (*.....................*)

    mulops,addops,relops,
    constbegsys,simptypebegsys,typebegsys,parambegsys,blockbegsys,selectsys,
    facbegsys,exprsys,statbegsys,typedels: setofsys;
    lcase: PACKED ARRAY [char] OF char;
    src: PACKED ARRAY [1..255] OF char;
    rw:  ARRAY [3..43] OF quantum;
    rsy: ARRAY [3..37] OF symbol;
    na:  ARRAY [1..maxstd] OF quantum;
    mn:  ARRAY [opcodes] OF str4;
    sna: ARRAY [1..maxsp] OF str4;
    tn:  ARRAY [symbol] OF quantum; { names of tokens, for error msg }
    relopinstr1,
    relopinstr2: ARRAY [ltop..eqop] OF opcodes;
    commute: ARRAY [imul..orop] OF Boolean; { whether the operator is commutative }
    comRel: ARRAY [comrelop] OF comrelop;
    corLoad: ARRAY [storerng] OF opcodes;
    load1wordops: SET OF opcodes; { only those loads that take up 1 stack position }

    intlabel,mxint10: integer;
    errtbl: ARRAY [errnoType] OF
              RECORD
                didit: Boolean; { error occurrence tracking }
                CASE integer OF
                  0: (sym: symbol);
                  1: (labnum: integer);
              END;
    totErr,totWarn: integer; { total errors in program }
    
    deadcode: Boolean; { after an unconditional branch, but before a branch }
                       { label (destination) is seen, any code generated is }
                       { "dead", i.e., there is no executable path to it }
    
    cmndLine: Boolean; { was 'commandline' seen in the program header? }
    prolist: prologp;

    { Set up free lists for all things that get new'd }
    strfl: strllp;
    reelcstfl,strgcstfl,psetcstfl: konstp;
    stcfl: tIptr;
    idfl: sTptr;
    lbpfl: lbp;
    cipfl: cip;
{$ifdef __GPC__}
    condComp: ARRAY [condCompRng] OF condCompT;
    condCompLev: condCompRng;
    p6: text; { this decl is for the compile by gpc; }
    trace: Boolean;
    indent: integer;
{$endif} { whereas the id "p6" appearing in the program header suffices for P6 }

  PROCEDURE error(ferrnr: integer);
  BEGIN
    IF ferrnr = 100 THEN { internal compiler error: stop the compilation }
      BEGIN
        writeln('Internal compiler error: please send bug report.');
        GOTO 99;
      END;
    WITH errtbl[ferrnr] DO
      BEGIN didit := true; sym := sy END;
    IF errinx = 10 THEN
      BEGIN
        errlist[10].nmr := 67; { overwrite the last err no }
        errtbl[67].didit := true;
      END
    ELSE
      BEGIN
        errinx := errinx + 1;
        errlist[errinx].nmr := ferrnr
      END;
    errlist[errinx].pos := chcnt - 3;
    IF (ferrnr >= minWarnRng) AND (ferrnr <= maxWarnRng) THEN
      totWarn := totWarn + 1
    ELSE
      totErr := totErr + 1
  END (*error*) ;

(*-------------------------------------------------------------------------*)

                           { recycling controls }

(*-------------------------------------------------------------------------*)

  { get string quantum }
  PROCEDURE getstr(VAR fp: strllp);
  BEGIN
    IF strfl = NIL THEN { free list is emptr; have to grab memory from heap }
      new(fp) { get new entry }
    ELSE { de-couple free list entry & return that }
      BEGIN fp := strfl; strfl := strfl^.next END;
    fp^.next := NIL
  END;
  
  { add quanta list to free list }
  PROCEDURE putstrs(VAR fp: strllp);
    VAR   llp: strllp;
  BEGIN
    IF fp <> NIL THEN
      BEGIN
        llp := fp;
        WHILE fp^.next <> NIL DO
          fp := fp^.next;
        { fp^.next = NIL ; fp points to last quantum }
        fp^.next := strfl; { add to front of free list }
        strfl := llp;
        fp := NIL;
      END;
  END;

  { get label entry: near clone of getstr }
  PROCEDURE getlab(VAR fp: lbp);
  BEGIN
    IF lbpfl = NIL THEN
      new(fp) { get new entry }
    ELSE { de-couple free list entry & return that }
      BEGIN fp := lbpfl; lbpfl := lbpfl^.next END;
  END;

  { recycle label entry }
  PROCEDURE putlab(fp: lbp);
  BEGIN
    fp^.next := lbpfl;
    lbpfl := fp;
  END;

  { get memory for reel constant & add it to display's list }
  PROCEDURE getreelcst(VAR fp: konstp);
  BEGIN
    IF reelcstfl = NIL THEN
      BEGIN new(fp, reel); fp^.cclass := reel END
    ELSE
      BEGIN fp := reelcstfl; reelcstfl := reelcstfl^.next END;
    { insert fp into display[top]'s fconst list }
    WITH display[top] DO
      BEGIN fp^.next := fconst; fconst := fp END;
  END;

  { get memory for strg constant & add it to display's list }
  PROCEDURE getstrgcst(VAR fp: konstp);
  BEGIN
    IF strgcstfl = NIL THEN
      BEGIN new(fp, strg); fp^.cclass := strg END
    ELSE
      BEGIN fp := strgcstfl; strgcstfl := strgcstfl^.next END;
    { insert fp into display[top]'s fconst list }
    WITH display[top] DO
      BEGIN fp^.next := fconst; fconst := fp END;
  END;

  { get memory for pset constant & add it to display's list }
  PROCEDURE getpsetcst(VAR fp: konstp);
  BEGIN
    IF psetcstfl = NIL THEN
      BEGIN new(fp, pset); fp^.cclass := pset END
    ELSE
      BEGIN fp := psetcstfl; psetcstfl := psetcstfl^.next END;
    { insert fp into display[top]'s fconst list }
    WITH display[top] DO
      BEGIN fp^.next := fconst; fconst := fp END;
  END;

  { recycle constant entry }
  PROCEDURE putcst(fp: konstp);
  BEGIN
    { recycle string if present }
    IF fp^.cclass = strg THEN
      BEGIN putstrs(fp^.sval); fp^.next := strgcstfl; strgcstfl := fp END
    ELSE
    IF fp^.cclass = reel THEN
      BEGIN putstrs(fp^.rval); fp^.next := reelcstfl; reelcstfl := fp END
    ELSE { fp^.cclass = pset }
      BEGIN fp^.next := psetcstfl; psetcstfl := fp END
  END;

  { get memory for typeInfo & add it to display's list }
  PROCEDURE getstc(VAR fp: tIptr);
  BEGIN
    IF stcfl = NIL THEN
      new(fp)
    ELSE
      BEGIN fp := stcfl; stcfl := stcfl^.next; fp^.next := NIL END;
    WITH display[top] DO
      BEGIN fp^.next := ftype; ftype := fp END;
  END;

  { recycle typeInfo entry }
  PROCEDURE putstc(fp: tIptr);
  BEGIN
    fp^.next := stcfl; stcfl := fp;
  END;

  { get memory for symbTabl entry }
  PROCEDURE getid(VAR fp: sTptr);
  BEGIN
    IF idfl = NIL THEN
      new(fp)
    ELSE
      BEGIN fp := idfl; idfl := idfl^.next END;
    fp^.next := NIL;
    fp^.keep := false;
  END;

  { recycle symbTabl entry }
  PROCEDURE putid(fp: sTptr);
    VAR p1: sTptr;
  BEGIN
    IF (fp^.klass = proc) OR (fp^.klass = func) THEN
      WHILE fp^.params <> NIL DO
        BEGIN
        { scavenge the parameter list }
          p1 := fp^.params; fp^.params := p1^.next;
          putid(p1) { release }
        END;
    putstrs(fp^.name); { release name string }
    fp^.next := idfl; idfl := fp;
  END;
  
  { scrub display level }
  PROCEDURE putdsp(lev: disprange);
    VAR   llp: lbp;
          lkonp: konstp;
          lsp: tIptr;

    { recycle symbTabl tree }
    PROCEDURE putSymTabTree(fp: sTptr);
    BEGIN
      IF fp <> NIL THEN
        BEGIN
          putSymTabTree(fp^.llink); { release left }
          putSymTabTree(fp^.rlink); { release right }
          { "keep" means it is a parameter and stays with it's procedure or
            function entry. }
          IF NOT fp^.keep THEN putid(fp) { release the id entry }
        END
    END;

    PROCEDURE putsub(fp: tIptr);     { release substructure }
      VAR p1: tIptr;
    BEGIN
      IF fp^.form = records THEN   { clear record recycle list }
        BEGIN
          WHILE fp^.recyc <> NIL DO { clear typeInfo list }
            BEGIN
              { remove top of list }
              p1 := fp^.recyc; fp^.recyc := p1^.next;
              putsub(p1) { release that element }
            END;
          putSymTabTree(fp^.fstfld) { clear id list }
        END
      ELSE
      IF fp^.form = tagfld THEN
        IF fp^.tagfieldp^.name = NIL THEN
          putid(fp^.tagfieldp);
      putstc(fp) { release head entry }
    END; (* putsub *)

  BEGIN { putdsp }
    putSymTabTree(display[lev].fname);
    WHILE display[lev].flabel <> NIL DO
      BEGIN
        llp := display[lev].flabel;
        display[lev].flabel := llp^.next;
        putlab(llp)
      END;

    WHILE display[lev].fconst <> NIL DO
      BEGIN
        lkonp := display[lev].fconst;
        display[lev].fconst := lkonp^.next;
        putcst(lkonp)
      END;
    WHILE display[lev].ftype <> NIL DO
      BEGIN
        lsp := display[lev].ftype;
        display[lev].ftype := lsp^.next;
        putsub(lsp)
      END
  END; { putdsp }

  { scrub all display levels until given }
  PROCEDURE putdsps(lev: disprange);
    VAR t: disprange;
  BEGIN
    IF lev > top THEN
      error(100);
    FOR t := top DOWNTO lev + 1 DO
      putdsp(t)
  END;

  { get case tracking entry }
  PROCEDURE getcas(VAR fp: cip);
  BEGIN
    IF cipfl = NIL THEN
      new(fp) { get new entry }
    ELSE
      BEGIN fp := cipfl; cipfl := cipfl^.next END;
    fp^.next := NIL;
  END;

  { recycle case tracking entry }
  PROCEDURE putcas(fp: cip);
  BEGIN
    fp^.next := cipfl; cipfl := fp;
  END;

(*-------------------------------------------------------------------------*)

                { character and string quanta functions }

(*-------------------------------------------------------------------------*)

  { write variable length id string to output }
  PROCEDURE writev(VAR f: text; a: strllp; fl: integer);
    VAR   i: integer;
  BEGIN
    { pre-condition: 0 <= fl }
    WHILE fl >= varsqt DO
      BEGIN
        write(f, a^.str);
        a := a^.next;
        fl := fl - varsqt;
      END;
    { 0 <= fl < varsqt }
    FOR i := 1 TO fl DO
      write(f, a^.str[i]);
  END;

  { find padded length of variable length id string }
  FUNCTION lenpv(s: strllp): integer;
    VAR   i, cnt: integer;
          blank: Boolean;
  BEGIN
    { pre-condition: s <> NIL }
    cnt := 0;
    WHILE s^.next <> NIL DO
      BEGIN
        cnt := cnt + varsqt;
        s := s^.next
      END;
    { s <> NIL, but s^.next = NIL }
    i := varsqt;
    REPEAT { 0 < i <= varsqt }
      blank := s^.str[i] = ' ';
      IF blank THEN
        i := i - 1;
    UNTIL (NOT blank) OR (i = 0);
    cnt := cnt + i;
    lenpv := cnt
  END;

  { assign reserved word to variable length string }
  PROCEDURE strassvr(VAR a: strllp; { CON } b: quantum);
  BEGIN
    getstr(a); a^.str := b;
  END;

  { compare variable length id strings }
  FUNCTION strcmpvv(a, b: strllp): compRslt;
    VAR   BothHaveExtensions: Boolean;
          rsltOfCompare: compRslt;

    FUNCTION strcmp(VAR a,b: quantum): compRslt;
    BEGIN
      IF a < b THEN strcmp := strLT ELSE
      IF a > b THEN strcmp := strGT ELSE
       { a = b }    strcmp := strEQ;
    END;

  BEGIN (* strcmpvv *)
    rsltOfCompare := strcmp(a^.str, b^.str);
    BothHaveExtensions := (a^.next <> NIL) AND (b^.next <> NIL);
    WHILE (rsltOfCompare = strEQ) AND BothHaveExtensions DO
      BEGIN { equal so far; bring up more quanta }
        a := a^.next; b := b^.next;
        rsltOfCompare := strcmp(a^.str, b^.str);
        BothHaveExtensions := (a^.next <> NIL) AND (b^.next <> NIL);
      END;
    IF rsltOfCompare <> strEQ THEN { it's been decided; }
      strcmpvv := rsltOfCompare    { either strLT or strGT }
    ELSE { rsltOfCompare = strEQ, so ergo NOT BothHaveExtensions; }
    IF a^.next <> NIL THEN { a has more non-blank, so it is greater }
      strcmpvv := strGT
    ELSE
    IF b^.next <> NIL THEN { b has more non-blank, so a is less }
      strcmpvv := strLT
    ELSE { neither side has more to compare, ergo: }
      strcmpvv := strEQ;
  END;

(*-------------------------------------------------------------------------*)

  PROCEDURE dumpname(flp: strllp);
  BEGIN
    writev(output, flp, lenpv(flp));
  END;
  
  PROCEDURE errMessage;
    VAR   prevTarg,colsUsed,targetCol,currnmr,f,k: integer;
  BEGIN
    IF errinx > 0 THEN   (*output error messages*)
      BEGIN
        IF NOT listing THEN
          BEGIN
            write(linecount:4,' ');
            FOR k := 1 TO chcnt DO write(src[k]);
            writeln;
          END;
        write(linecount:4,'#');
        prevTarg := -1; colsUsed := -1;
        FOR k := 1 TO errinx DO
          BEGIN
            WITH errlist[k] DO
              BEGIN targetCol := pos; currnmr := nmr END;
            IF targetCol = prevTarg THEN
              write(',')
            ELSE
              BEGIN
                WHILE colsUsed < targetCol DO
                  BEGIN write(' '); colsUsed := colsUsed + 1 END;
                write('^');
                prevTarg := targetCol
              END;
            IF currnmr <  10 THEN f := 1 ELSE
            IF currnmr < 100 THEN f := 2
                             ELSE f := 3;
            write(currnmr:1);
            colsUsed := colsUsed + f + 1
          END;
        writeln;
        errinx := 0;
      END;
  END;
      
  PROCEDURE endofline;
    VAR   k: integer;
  BEGIN
    errMessage;
    linecount := linecount + 1;
    IF listing THEN
      write(output, linecount:4, ' ');
    { output line marker in intermediate file }
    IF linecount > 1 THEN
      BEGIN
        write(p6, ':', linecount-1:4, ' ');
        IF listing THEN
          FOR k := 1 TO chcnt DO write(p6, src[k]);
        writeln(p6);
      END;
    chcnt := 0
  END (* endofline *);

  PROCEDURE errmsg(ferrnr: integer);
  
    PROCEDURE completer;
    BEGIN write(' expected; instead found ', tn[sy]) END;
    
    PROCEDURE notallowed;
    BEGIN write(' not allowed in ISO 7185 Pascal') END;
    
  BEGIN
    CASE ferrnr OF
      1: BEGIN write('Simple type (subrange, enumeration, type id, e.g.)'); completer END;
      2: BEGIN write('Identifier'); completer END;
      3: BEGIN write('"PROGRAM"'); completer END;
      4: BEGIN write('Right paren ")"'); completer END;
      5: BEGIN write('Colon ":"'); completer END;
      6: write('This character cannot appear outside of a comment or string');
      7: BEGIN write('"PROCEDURE", "FUNCTION", "VAR" or identifier'); completer END;
      8: BEGIN write('"OF"'); completer END;
      9: BEGIN write('Left paren "("'); completer END;
     10: BEGIN write('Beginning of type (ARRAY, RECORD, SET, FILE, e.g.)'); completer END;
     11: BEGIN write('Left bracket "["'); completer END;
     12: BEGIN write('Either a comma "," or right bracket "]"'); completer END;
     13: BEGIN write('"END"'); completer END;
     14: BEGIN write('Semicolon ";"'); completer END;
     15: BEGIN write('Label (0-9999)'); completer END;
     16: BEGIN write('Equals sign "="'); completer END;
     17: BEGIN write('"BEGIN"'); completer END;
     18: BEGIN write('Either a comma "," or right paren ")"'); completer END;
     19: BEGIN write('Record field identifier (or "CASE")'); completer END;
     20: BEGIN write('Comma ","'); completer END;
     21: BEGIN write('Period "."'); completer END;
     22: BEGIN write('"ARRAY"'); completer END;
     23: BEGIN write('".." (range)'); completer END;
     24: BEGIN write('Semicolon ";" or right bracket "]"'); completer END;
     25: BEGIN write('Record variant parts'); completer END;
     26: BEGIN write('Beginning of statement'); completer END;
     27: BEGIN write('Beginning of declaration'); completer END;
     28: BEGIN write('Underscores in identifiers'); notallowed END;
     29: BEGIN write('Semicolon ";" or right paren ")"'); completer END;
     30: BEGIN write('CONST parameters'); notallowed END;
     31: BEGIN write('Initialization of variables and types'); notallowed END;
     32: BEGIN write('Strings with double quotes'); notallowed END;
     33: BEGIN write('"bindable" is'); notallowed END;
     34: BEGIN write('"type of <name>" (type inquiry)'); notallowed END;
     35: BEGIN write('A "protected" parameter'); notallowed END;
     36: write('Non-Pascal style comment');
     37: BEGIN write('Array slices/substrings'); notallowed END;
     38: BEGIN write('Open array parameter'); notallowed END;
     39: BEGIN write('Schemata are'); notallowed END;

     50: BEGIN write('Integer, real, string, "+", "-", or id'); completer END;
     51: BEGIN write('":=" (assignment operator)'); completer END;
     52: BEGIN write('"THEN"'); completer END;
     53: BEGIN write('"UNTIL"'); completer END;
     54: BEGIN write('"DO"'); completer END;
     55: BEGIN write('Either "TO" or "DOWNTO"'); completer END;
     56: BEGIN write('ELSE/OTHERWISE case label'); notallowed END;
     57: BEGIN write('Case label ranges'); notallowed END;
     58: BEGIN write('Expression parts (constants, ids, operators, e.g.)'); completer END;
     59: BEGIN write('Qualifier missing: bracket, arrow or period'); completer END;
     60: write('More digits were expected to complete real constant');
     61: write('String constant cannot span more than one source line');
     62: write('Integer exceeds "maxint" ',maxint:1);
     63: BEGIN write('Null (zero length) strinq'); notallowed END;
     64: write('Too much nesting; (re-compile compiler with larger "displimit"');
     65: write('Read and write require a list of parameters');
     66: write('Argument to this standard function must be integer or real');
     67: write('Too many errors on this source line');
     68: write('Argument to this standard function must be real');
     69: write('Argument to this standard function must be integer');
     70: write('Argument to this standard function must be a non-real scalar type');
     71: write('Argument to pred/succ must be an ordinal type');
     72: write('Arguments to binary file read/write must match file component type');
     73: write('Left operand of "IN" must have ordinal type compatible with right operand''s base type');
     74: write('Implementation restriction: set elements must be in range ',
                      setlow:1, ' to ', sethigh:1);
     75: write('Already in use as a control variable in enclosing FOR');
     76: write('Operands of AND and OR must be of Boolean type');
     77: write('Operands must both be numeric or both be sets of compatible basetypes');
     78: write('End of file encountered while in a comment');
     79: write('Component of packed structure cannot be argument to variable parameter');
     80: write('Value parameter cannot have a file type or have a file component');
     
     82: write('UNTIL expr is false: looping not possible');
     83: write('UNTIL expr is true: infinite loop likely');
     84: write('WHILE false: the body of the WHILE loop cannot be reached');
     85: write('WHILE true: infinite loop is likely');
{$ifdef __GPC__}
     86: write('Nesting level exceeded for conditional compilation');
     87: write('No matching ''$if'' or ''$ifdef'', etc.');
     88: write('This is the second ''$else'' at this level');
{$endif}
     90: write('Mismatched PACKED attributes');     
     91: write('Subranges cannot use real constants');
     92: write('String cannot be used as variant selector constant');
     93: write('Argument to "read" cannot be read from textfile');
     94: write('Argument to "write" cannot be written to textfile');
     95: write('Function result value never assigned');
     96: write('Argument to variable parameter must have identical type (not just structural)');
     97: write('Index value not assignment compatible with array index type');

     98: write('Statement label referenced in this block, but not defined or declared: ',
               errtbl[98].labnum:1);
     99: write('Disjoint subranges; value cannot be assigned');
    100: write('Compiler internal error');
    101: write('Identifier declared twice in same scope');
    102: write('Low bound exceeds highbound');
    103: write('Identifier is not of appropriate class');
    104: write('Identifier not declared');
    105: write('Only integer or real constants can be signed');
    106: write('Array index type must not be "real"');
    107: write('Incompatible types of subrange bounds');
    108: BEGIN write('For statement with set iterator'); notallowed END;
    109: write('Array element types are not the same');
    110: write('Tag type of variant part of record must be scalar (but not "real")');
    111: write('Case constant incompatible with tag type');
    112: write('Variable of array type required here');
    113: write('Array index type must be scalar or subrange (but not "real")');
    114: write('Set element type must not be "real"');
    115: write('Set elements must be scalar or subrange (but not "real")');
    116: write('Variable of file type required here');
    117: write('Unsatisfied forward reference');
    118: write('All output field-width values must be of integer type (and > 0)');
    119: write('Forward declared; repetition of parameter list not allowed');
    120: write('Function result type must be scalar, subrange or pointer');
    121: write('Only textfiles can use readln or writeln');
    122: write('Forward declared function; repetition of result type not allowed');
    123: write('Missing result type in function declaration');
    124: write('A second output field-width value applies to type "real" only');
    125: write('Expression is not assignment compatible with variable');
    126: write('Number of parameters does not agree with declaration');
    127: write('"DIV" and "MOD" operate only on integer values');
    128: write('Result type of parameter function does not agree with declaration');
    129: write('Dissimilar operand types cannot be compared');
    130: write('Expression must evaluate to a set type');
    131: write('Pointers can only be compared for = or <>');
    132: write('Operators "<" and ">" not allowed on set types');
    133: write('Arrays (except strings), records and files cannot be compared');
    134: write('Only integers and reals can be divided');
    135: write('Type of operand must be Boolean');
    136: write('Set element type must be scalar nr subrange');
    137: write('Set element types not compatible');
    138: write('Indexed variable must be an array');
    139: write('Subscript type is not compatible with array index declaration');
    140: write('Variable must be a record');
    141: write('Variable must be a file or a pointer');
    142: write('Actual argument type not compatible with formal parameter');
    143: write('Control variable of FOR loop must be non-real, ordinal type');
    144: write('Expression must evaluate to a Boolean value (true or false)');
    145: write('Limit expressions must be compatible with control variable');
    146: write('Assignment of files not allowed');
    147: write('Label type incompatible with CASE expression');
    148: write('Subrange bound may not be a string');
    149: write('Array index type must not be "integer"');
    150: write('Assignment to standard function is not allowed');
    151: write('Assignment to formal function is not allowed');
    152: write('Field not declared in this record (misspelling?)');
    153: write('CASE expression must be scalar, non-real type');
    154: write('Argument to VAR parameter must be a variable');
    155: write('Variable of type pointer is required here');
    156: write('Case label already seen');
    157: write('Case label outside of range ', minCaseLab:1, '..', maxCaseLab:1);
    158: write('Constant doesn''t correspond to variant selector');
    159: write('Real or string tagfields not allowed');
    160: write('Previous declaration was not forward');
    161: write('Again forward declared');
    162: write('Record variant selector must be constant');
    163: write('"AND" and "OR" operate only on Boolean values');
    164: write('"*", "+" and "-" operate only on integers, reals, and sets');
    165: write('Statement label already used');
    166: write('Statement label previously declared');
    167: write('Statement label referenced in this block, but not declared');
    168: write('Statement label declared in this block, but not defined: ',
               errtbl[168].labnum:1);
    169: write('Argument must be of type "textfile"');
    170: write('Divide by zero');
    171: write('Control variable of FOR cannot be component of a structured type');
    172: write('Record variant selector cannot be argument to variable parameter');
    173: write('Unresolved FORWARD procedure/function declaration');
    174: write('Declarations out of order');
    175: write('The standard identifier "input" missing from PROGRAM header');
    176: write('The standard identifier "output" missing from PROGRAM header');
    177: write('RECORD variant selector already seen');
    178: write('Control variable of FOR loop must not be a formal parameter');
    179: write('Control variable of FOR must be declared locally');
    180: write('Argument has to be same type as previous arg');
    181: write('Argument non-conformant: fewer array dimensions than parameter');
    182: write('Argument index must lie within the ordinal type of the c.a.p. schema');
    183: write('Argument array has more dimensions than does parameter');
    184: write('Arrays of different types require their own conformant array schemas');
    185: write('Statement label must not exceed ',maxlabel:1);
    186: write('Cannot jump into a structured statement from outside of it');
    187: write('Control variable of FOR cannot be changed within the loop');
    188: write('Control variable of FOR cannot be argument to variable parameter');
    189: write('Incompatible profiles between formal and actual parameters');
    190: write('File component may not contain other files');
    191: write('Cannot assign any structure containing a file component');
    192: write('Assignment to function not in the call chain');
    193: write('Subscript out of range of index type');
    194: write('Constant out of range');
    195: ;
    196: write('Ordinal (but not real) type identifier expected');
    197: write('Conformant array bound cannot be changed');
    198: write('Conformant array bound cannot be argument to variable parameter');
    199: write('Packed conformant array cannot have conformant array element type');
    200: write('Packed conformant array can only have one index (dimension)');
{   maxErrNo = 200 }
    END
  END;

  PROCEDURE nextch;
  BEGIN
    IF eolseen THEN
      BEGIN { delay to make error handling more accurate }
        eolseen := false;
        endofline;
      END
    ELSE
    IF eof THEN
      ch := chr(EOFchar)
    ELSE
    IF eoln THEN
      BEGIN
        IF listing THEN writeln;
        readln;        { clear the end-of-line }
        ch := chr(10); { signal to insymbol: end of id, num, or string }
        eolseen := true;
      END
    ELSE
      BEGIN
        ch := input^;
        get(input);
        IF chcnt < 255 THEN
          BEGIN
            chcnt := chcnt + 1;
            src[chcnt] := ch;
          END;
        IF listing THEN
          write(ch);
      END;
  END; { nextch }

  PROCEDURE insymbol;
    (* read next basic symbol of source program and return its
       description in the global variables sy, op, id, val and lgth *)
    LABEL 1;
    VAR   oldch: char;
          i,k,j: integer;
          lkonp: konstp;
          EndOfString,underscoreSeen: Boolean;
          headp,lp: strllp; { for identifier, number, or string quanta }

{$ifdef __GPC__}
    FUNCTION constExpr: Boolean;
      VAR   saveMode: condMode;
    BEGIN
      saveMode := condComp[condCompLev].mode;
      condComp[condCompLev].mode := parsing;
      insymbol; { even in skip mode, need to come back to comment processing }
      condComp[condCompLev].mode := saveMode;
      IF sy <> ident THEN
        error(2)
      ELSE
        putstrs(idll);
      constExpr := false; { gpc sees "__GPC__" & says its defined, but }
    END;                  { P6 sees the same thing & says "no, it's not" }
{$endif}

    PROCEDURE options;
      VAR   i,j: integer;
            singleLetterID: char;
{$ifdef __GPC__}
            saveMode: condMode;
{$endif}
    BEGIN
      REPEAT                 { such as __GPC__ }
        nextch; { eat the $ or comma }
        IF ch in letter THEN
{$ifdef __GPC__}
          BEGIN
            saveMode := condComp[condCompLev].mode;
            condComp[condCompLev].mode := parsing;
{$endif}
          insymbol
{$ifdef __GPC__}
            ;condComp[condCompLev].mode := saveMode;
            IF (kk > 1) AND
               (sy IN [ident,ifsy,elsesy]) THEN { multi-letter word }
              BEGIN
                j := 0;
                IF sy = ident THEN
                  BEGIN
                    FOR i := 38 TO 43 DO { looking for 'elif', 'endif', }
                      IF rw[i] = idll^.str THEN { 'ifdef', 'error', 'define', }
                        j := i;                 { or 'defined' }
                    putstrs(idll);
                  END;
                IF (sy = ifsy) OR (j = 40 {ifdef}) THEN
                  BEGIN
                    IF condCompLev < maxCondComp THEN
                      condCompLev := condCompLev + 1
                    ELSE
                      error(86); { nesting limit exceeded }
                    WITH condComp[condCompLev] DO
                      BEGIN
                        IF constExpr THEN mode := parsing
                                     ELSE mode := skipping;
                        branch := iffing
                      END;
                  END                              
                ELSE
                IF j = 39 {endif} THEN
                  IF condCompLev > 0 THEN
                    condCompLev := condCompLev - 1
                  ELSE
                    error(87)  { no matching 'if' }
                ELSE
                IF sy = elsesy THEN
                  WITH condComp[condCompLev] DO
                    IF branch = elsing THEN { second 'else' }
                      error(88)
                    ELSE
                      BEGIN
                        branch := elsing;
                        IF mode = parsing THEN mode := skipping
                                          ELSE mode := parsing;
                      END;
              END;
          END
{$endif}
        ELSE
          sy := nilsy;
        IF (sy = ident) AND (kk = 1) THEN { single letter }
          BEGIN
            singleLetterID := idll^.str[1];
{$ifdef __GPC__}
            IF singleLetterID = 't' THEN
              trace := ch = '+'
            ELSE
{$endif}
            IF singleLetterID = 'l' THEN
              BEGIN listing := ch = '+';
                IF NOT listing THEN writeln(output)
              END
            ELSE
            IF singleLetterID = 'd' THEN
              debug := ch = '+';
            nextch; { eat the switch character }
            putstrs(idll);
          END;
      UNTIL ch <> ',';
    END (* options *) ;

    PROCEDURE heapitup(VAR i,k: integer; VAR lp: strllp; c: char);
    BEGIN
      k := k + 1;
      i := i + 1;
      IF i > varsqt THEN
        BEGIN getstr(lp^.next); lp := lp^.next; i := 1 END;
      lp^.str[i] := c
    END;

    PROCEDURE commentary;
      LABEL 2;
    BEGIN
      IF ch = '$' THEN
        options;
      WHILE ch <> rbrace DO
        BEGIN
          IF ch = chr(EOFchar) THEN
            BEGIN error(78); GOTO 98 END;
          IF (ch = '*') AND (input^ = ')') THEN { alternative comment ender }
            BEGIN get(input); GOTO 2 END;
          nextch;
        END;
2:    nextch; { eat the comment ender (unless eof) }
    END; { commentary }
    
  BEGIN (*insymbol*)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>insymbol ch=',ch);
  END;
{$endif}
  1:
    { skip over spaces and control characters (e.g., end-of-lines): }
    WHILE ch <= ' ' DO nextch;
    { postcondition: ch > ' ' }
    oldch := ch;
    IF oldch = chr(EOFchar) THEN
      GOTO 98;
    nextch;
    CASE oldch OF
      '!','#','$','%','&','`','?','\','|','~':
        BEGIN
          sy := illegalsy;
          error(6)
        END;
      '"':
        BEGIN
          error(32);
          WHILE ch <> '"' DO
            BEGIN
              IF ch = chr(EOFchar) THEN
                GOTO 98;
              nextch;
            END;
          nextch;
        END;
      '_', { non-(7185)standard, but a common mistake }
      'A','B','C','D','E','F','G','H','I','J','K','L','M',
      'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
      'a','b','c','d','e','f','g','h','i','j','k','l','m',
      'n','o','p','q','r','s','t','u','v','w','x','y','z':
        BEGIN { put together identifier; check against reserved words }
          underscoreSeen := false;
          IF oldch = '_' THEN
            underscoreSeen := true;
          i := 0; kk := 0;
          getstr(idll); lp := idll;
          heapitup(i, kk, lp, lcase[oldch]);
          WHILE ch IN letordig DO
            BEGIN
              IF ch = '_' THEN
                underscoreSeen := true;
              heapitup(i, kk, lp, lcase[ch]);
              nextch
            END;
          WHILE i < varsqt DO
            BEGIN i := i + 1; lp^.str[i] := ' ' END;
          IF underscoreSeen THEN
{$ifdef __GPC__}
           IF condCompLev = 0 THEN { make exception for __GPC__ }
{$endif}
            error(28);
          sy := ident;
          { look for reserved words using "perfect hash functions" }
          { the hash values were generated by an algorithm due to  }
          { Richard Cichelli, et al. }
          IF kk >= 2 THEN { this eliminates LOTS of cases }
            IF kk <= 9 THEN { this eliminates a few more }
              IF NOT underscoreSeen THEN
                BEGIN
                  oldch := idll^.str[kk]; { the last character in the id }
                  IF oldch >= 'a' THEN    { must not be a digit }
                    BEGIN
                      i := hash[idll^.str[1]] + hash[oldch] + kk;
                      IF i >= 3 THEN
                        IF i <= 37 THEN
                          IF idll^.str = rw[i] THEN
                            sy := rsy[i];
                    END;
                END;
          IF sy <> ident THEN
            putstrs(idll);
        END;
      '0','1','2','3','4','5','6','7','8','9':
      { put together unsigned integer, which might be unsigned real }
        BEGIN
          sy := intconst; { until a "." or "E", or "e" shows up }
          i := 0; k := 0;
          getstr(headp); lp := headp;
          heapitup(i, k, lp, '+'); { in case it's a real constant }
          heapitup(i, k, lp, oldch);
          WHILE (ch >= '0') AND (ch <= '9') DO
            BEGIN
              heapitup(i, k, lp, ch);
              nextch
            END;
          IF (ch = '.') AND (input^ <> '.') AND (input^ <> ')') THEN
            BEGIN
              heapitup(i, k, lp, ch); { save the decimal point }
              nextch;
              IF (ch < '0') OR (ch > '9') THEN
                error(60)
              ELSE
                REPEAT
                  heapitup(i, k, lp, ch);
                  nextch
                UNTIL (ch < '0') OR (ch > '9');
              sy := realconst;
            END;
          ch := lcase[ch]; { in case ch = 'E' }
          IF ch = 'e' THEN
            BEGIN
              heapitup(i, k, lp, ch);
              nextch;
              IF (ch = '+') OR (ch ='-') THEN
                BEGIN heapitup(i, k, lp, ch); nextch END;
              IF (ch < '0') OR (ch > '9') THEN
                error(60)
              ELSE
                REPEAT
                  heapitup(i, k, lp, ch);
                  nextch
                UNTIL (ch < '0') OR (ch > '9');
              sy := realconst
            END;
          lp^.next := NIL;
          IF sy = realconst THEN
            BEGIN
              WHILE i < varsqt DO
                heapitup(i, k, lp, ' ');
              getreelcst(lkonp);
              lkonp^.rval := headp;
              val.valp := lkonp
            END
          ELSE { sy = intconst }
            WITH val DO
              BEGIN
                ival := 0; lp := headp; i := 1;
                { start after the '+': }
                FOR j := 2 TO k DO
                  BEGIN
                    i := i + 1;
                    IF i > varsqt THEN { 00000000000125? could happen }
                      BEGIN lp := lp^.next; i := 1 END;
                    IF ival <= mxint10 THEN
                      ival := ival*10 + (ord(lp^.str[i])-ord('0'))
                    ELSE
                      BEGIN error(62); ival := 0 END
                  END;
                putstrs(headp);
              END
        END;
      '''':
        BEGIN
          lgth := 0;
          sy := strngconst;
          getstr(headp); lp := headp;
          i := 0; lgth := 0;
          EndOfString := false;
          REPEAT
            IF ch = '''' THEN { string ender, or 1st char of embedded quote }
              BEGIN
                nextch;
                EndOfString := ch <> ''''; { 2nd char of embedded quote? }
              END
            ELSE
            IF ch = chr(10) THEN
              BEGIN error(61); EndOfString := true END;
            IF NOT EndOfString THEN
              BEGIN
                heapitup(i, lgth, lp, ch);
                nextch;
              END;
          UNTIL EndOfString;
          lp^.next := NIL;
          IF lgth = 0 THEN
            BEGIN error(63); val.ival := 0; putstrs(headp); lgth := 1 END
          ELSE
          IF lgth = 1 THEN
            BEGIN val.ival := ord(headp^.str[1]); putstrs(headp) END
          ELSE
            BEGIN
              WHILE i < varsqt DO { blank out rest of final str }
                BEGIN i := i + 1; lp^.str[i] := ' ' END;
              getstrgcst(lkonp);
              WITH lkonp^ DO
                BEGIN cclass := strg; slgth := lgth; sval := headp END;
              val.valp := lkonp
            END;
        END;
      '(':
        IF ch = '*' THEN { digraph: alt for left brace }
          BEGIN
            nextch;
            commentary;
            GOTO 1
          END
        ELSE
        IF ch = '.' THEN { (. digraph (alt for "[" ) }
          BEGIN nextch; sy := lbrack END
        ELSE
          sy := lparen;
      ')': sy := rparen;
      '*':
        IF ch = ')' THEN { scanning from within conditional compile comment }
          BEGIN sy := cmtEndsy; nextch END
        ELSE
          sy := imul;
      '+': sy := plus;
      ',': sy := comma;
      '-': sy := minus;
      '.':
        IF ch = '.' THEN
          BEGIN sy := range; nextch END
        ELSE
        IF ch = ')' THEN { .) digraph (alt for "]") }
          BEGIN sy := rbrack; nextch END
        ELSE sy := period;
      '/':
        IF ch = '/' THEN { C++ style comment: // }
          BEGIN
            error(36);
            REPEAT nextch UNTIL eolseen OR eof;
            GOTO 1;
          END
        ELSE sy := rdiv;
      ':':
        IF ch = '=' THEN
          BEGIN sy := becomes; nextch END
        ELSE sy := colon;
      ';': sy := semicolon;
      '<':
        IF ch = '=' THEN { <= digraph }
          BEGIN sy := leop; nextch END
        ELSE
        IF ch = '>' THEN { <> digraph }
          BEGIN sy := neop; nextch END
        ELSE sy := ltop;
      '=': sy := eqop;
      '>':
        IF ch = '=' THEN { >= digraph }
          BEGIN sy := geop; nextch END
        ELSE sy := gtop;
      '@': sy := arrow;
      '[': sy := lbrack;
      ']': sy := rbrack;
      '^': sy := arrow; { the caret lacks a stick }
      '{':
        BEGIN
          commentary;
          GOTO 1
        END;
      '}':
        BEGIN sy := cmtEndsy { see above comment } END;
    END; (* CASE *)

{$ifdef __GPC__}
    IF condComp[condCompLev].mode = skipping THEN
      BEGIN
        IF sy = ident THEN
          putstrs(idll); { re-cycle identifier linked list }
        GOTO 1
      END;
      
IF trace THEN
  BEGIN
    writeln(' ':indent,'<insymbol found ',tn[sy]);
    indent := indent - 1;
  END;
{$endif}
  END (* insymbol *);

  PROCEDURE enterId(fname: sTptr);
    (*enter id pointed at by fname into the name-table,
     which on each declaration level is organised as
     an unbalanced binary tree*)
    VAR   nam: strllp;
          lname, lname1: sTptr;
          lleft: Boolean;
          strRslt: compRslt;
  BEGIN
    nam := fname^.name;
    lname := display[top].fname;
    IF lname = NIL THEN
      display[top].fname := fname
    ELSE
      BEGIN
        REPEAT
          strRslt := strcmpvv(lname^.name, nam);
          lname1 := lname;
          IF strRslt = strEQ THEN (*name conflict, follow right link*)
            BEGIN error(101); lname := lname^.rlink; lleft := false END
          ELSE
          IF strRslt = strLT THEN
            BEGIN lname := lname^.rlink; lleft := false END
          ELSE { strRslt = strGT }
            BEGIN lname := lname^.llink; lleft := true END
        UNTIL lname = NIL;
        IF lleft THEN lname1^.llink := fname
                 ELSE lname1^.rlink := fname
      END;
    fname^.llink := NIL; fname^.rlink := NIL; fname^.next := NIL;
  END (* enterId *);

  PROCEDURE searchOneLevel(fname: sTptr; VAR fndname: sTptr);
    (*to find record fields and forward declared procedure id's
     --> procedure procdeclaration
     --> procedure selector*)
    VAR  rslt: compRslt;
  BEGIN
    rslt := strLT;
    WHILE (fname <> NIL) AND (rslt <> strEQ) DO
      BEGIN
        rslt := strcmpvv(fname^.name, idll);
        IF rslt <> strEQ THEN
          IF rslt = strLT THEN fname := fname^.rlink
                          ELSE fname := fname^.llink;
      END;
    { assert: fname = NIL (not found) OR rslt = strEQ (found) }
    IF rslt = strEQ THEN { found in symbol table: keep only that copy of id }
      putstrs(idll);     { and re-cycle the one just built by insymbol }
    fndname := fname
  END (* searchOneLevel *);

  PROCEDURE searchid(fidcls: setofids; VAR fndname: sTptr; mustFind: Boolean);
  { searches name in idll from closest scope clear down to pre-declareds; }
  { if mustFind, but doesn't, issue an error; or if the identifier is of the }
  { wrong klass, issue a different error message (103). }
    LABEL 1,2;
    VAR   lname: sTptr;
          disxl: disprange;
          rslt: compRslt;
          notQuite: Boolean;
  BEGIN
    notQuite := false;
    FOR disxl := top DOWNTO 0 DO
      BEGIN
        lname := display[disxl].fname;
        WHILE lname <> NIL DO
          BEGIN
            rslt := strcmpvv(lname^.name, idll);
            IF rslt = strEQ THEN
              IF lname^.klass IN fidcls THEN
                BEGIN
                  statLevFnd := disxl;
                  putstrs(idll);
                  GOTO 2
                END
              ELSE { multiply-declared id; keep looking for the klass we want }
                BEGIN
                  notQuite := true;
                  lname := lname^.rlink;
                  IF lname = NIL THEN GOTO 1;
                END
            ELSE
              IF rslt = strLT THEN
                lname := lname^.rlink
              ELSE    { strGT }
                lname := lname^.llink
          END
      END;
1:  IF mustFind AND (lname = NIL) THEN
      BEGIN
        (*search not successful *)
        IF notQuite THEN error(103)
                    ELSE error(104);
          (*to avoid returning nil, reference an entry
           for an undeclared id of appropriate class
           --> procedure enterundecl*)
        IF types IN fidcls THEN lname := utypptr ELSE
        IF vars  IN fidcls THEN lname := uvarptr ELSE
        IF field IN fidcls THEN lname := ufldptr ELSE
        IF konst IN fidcls THEN lname := ucstptr ELSE
        IF proc  IN fidcls THEN lname := uprcptr
         { func }          ELSE lname := ufctptr;
      END;
    statLevFnd := -1;
2:
    fndname := lname
    { thus the returned VAR parameter "fndname" is never NIL }
  END (* searchid *);

  PROCEDURE getbounds(fsp: tIptr; VAR fmin,fmax: integer);
    (*get internal bounds of subrange or scalar type*)
    (*assume fsp<>realptr*)
  BEGIN
    fmin := 0; fmax := 0;
    IF fsp <> NIL THEN
    WITH fsp^ DO
      IF form = subrange THEN
        BEGIN fmin := min.ival; fmax := max.ival  END
      ELSE
      IF form = confInx THEN
        getbounds(TidOrd, fmin, fmax)
      ELSE
      IF fsp = charptr THEN
        BEGIN fmin := ordminchar; fmax := ordmaxchar END
      ELSE
      IF fsp = intptr THEN
        BEGIN fmin := -maxint; fmax := maxint END
      ELSE
      IF (form = scalar) AND (scalkind = expdclrd) THEN { includes boolean }
        IF fenum <> NIL THEN
          BEGIN fmin := 0; fmax := fenum^.values.ival END;
  END (* getbounds *);

  { alignment for general memory placement }
  FUNCTION alignquot(fsp: tIptr): integer;
  BEGIN
    alignquot := 1;
    IF fsp <> NIL THEN
      WITH fsp^ DO
        CASE form OF
          scalar:   IF fsp = intptr  THEN alignquot := intal ELSE
                    IF fsp = boolptr THEN alignquot := boolal ELSE
                    IF scalkind = expdclrd THEN alignquot := intal ELSE
                    IF fsp = charptr THEN alignquot := charal ELSE
                    IF fsp = realptr THEN alignquot := realal
                       (* parmptr *) ELSE alignquot := parmal;
          subrange: alignquot := alignquot(rangetype);
          confInx:  alignquot := intal;
          pointer:  alignquot := adral;
          powerset: alignquot := setal;
          files:    alignquot := fileal;
          arrays:   alignquot := alignquot(aeltype);
          records:  alignquot := recal;
          variant,tagfld: error(100)
        END
  END (*alignquot*);

  PROCEDURE align(fsp: tIptr; VAR flc: addrrange);
    VAR   k,llc: integer;
  BEGIN
    k := alignquot(fsp);
    llc := flc + k - 1;
    flc := llc - llc MOD k
  END (*align*);

  PROCEDURE genlabel(VAR nxtlab: integer);
  BEGIN
    intlabel := intlabel + 1;
    nxtlab := intlabel
  END;

  PROCEDURE searchlabel(VAR llp: lbp; flevel: disprange);
    LABEL 1;
  BEGIN
    llp := display[flevel].flabel; { index top of label list }
    WHILE llp <> NIL DO
      IF llp^.labval <> val.ival THEN
        llp := llp^.next { search some more }
      ELSE { llp^.labval = val.ival }
        { found! } GOTO 1;
    1: { either llp = NIL: WHILE got to the end of linked list, (not found)
         or the GOTO was taken }
  END; { in Extended Pascal (ISO 10206), the WHILE statement could be replaced
         by: 'WHILE (llp <> NIL) AND_THEN (llp^.labval <> val.ival) DO
                llp := llp^.next' }

  PROCEDURE newlabel(VAR llp: lbp);
  BEGIN
    WITH display[top] DO
      BEGIN
        getlab(llp);
        WITH llp^ DO
          BEGIN
            next := flabel;
            undeclared := true;
            defined := false;
            maxNesting := maxint;
            labval := val.ival;
            genlabel(labname);
            vlevel := level;
          END;
        flabel := llp
      END
  END;

  PROCEDURE skip(fsys: setofsys);
    (* recovering from error; try to re-synch parser: *)
    (* skip input string until relevant symbol found *)
    VAR   s: symbol; { debug }
  BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    write(' ':indent,'>skip [ ');
    FOR s := imul TO illegalsy DO
      IF s IN fsys THEN write(tn[s],' ');
    writeln(']');
  END;
{$endif}
    WHILE NOT (sy IN fsys) DO
      insymbol;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<skip');
    indent := indent - 1;
  END;
{$endif}
  END;

  PROCEDURE unknownParenConstruct(ferr: integer);
    VAR   plev: integer;
  BEGIN
    IF ferr <> 104 THEN error(ferr);
    plev := 1;
    REPEAT
      insymbol;
      IF sy = rparen THEN
        plev := plev - 1
      ELSE
      IF sy = lparen THEN
        plev := plev + 1;
    UNTIL (plev = 0) OR (sy = semicolon);
    IF sy = rparen THEN insymbol;
  END;
  
  PROCEDURE block(fsys: setofsys;
                  fsy: symbol;     { = procsy or funcsy, when fprocp <> NIL }
                  fprocp: sTptr);  { otherwise it's programme that called }
    LABEL 1;
    VAR   lsy: symbol;
          stalvl: integer; { statement nesting level }
          lsys: setofsys;

    PROCEDURE updatelist(fcp: sTptr; fsize: addrrange; fsp: tIptr);
    BEGIN { takes a linked-list of symbol table entries, and updates the }
      REPEAT { addr (if fsize <> 0), and/or the idtype (if fsp <> NIL) }
        WITH fcp^ DO
          BEGIN
            IF fsize <> 0 THEN { you gotta size? we'll update the addr }
              BEGIN addr := lc; lc := lc + fsize END;
            IF fsp <> NIL THEN { you gotta type? we'll update that }
              idtype := fsp;
          END;
        fcp := fcp^.next;
      UNTIL fcp = NIL;
    END; { updatelist }

    PROCEDURE add2list(VAR first,last: sTptr; { existing list }
                       fcp: sTptr);           { list to be added }
    BEGIN { used by fieldlist, vardeclaration & parameterlist to process idlists }
      { precondition: (first = NIL AND_THEN last = undefined) OR
                      (first <> NIL AND last <> NIL) }
      IF first = NIL THEN { no list built yet }
        first := fcp
      ELSE
        last^.next := fcp;
      last := fcp;
      WHILE last^.next <> NIL DO
        last := last^.next;
    END; { add2list }

    PROCEDURE constant(fsys: setofsys; VAR fsp: tIptr; VAR fvalu: valu);
      VAR   lsp: tIptr;
            lcp: sTptr;
            sign: (none,pos,neg);
            lkonp: konstp;
            oldlist,newlist: strllp;
    BEGIN
      lsp := NIL;
      fvalu.ival := 0;
      IF NOT (sy IN constbegsys) THEN
        BEGIN error(50); skip(fsys+constbegsys) END;
      IF sy IN constbegsys THEN
        BEGIN
          IF sy = strngconst THEN
            BEGIN
              IF lgth = 1 THEN
                lsp := charptr
              ELSE
                BEGIN
                  getstc(lsp);
                  WITH lsp^ DO
                    BEGIN
                      size := lgth*charsize;
                      packing := true;
                      form := arrays;
                      aeltype := charptr;
                      inxtype := NIL;
                    END
                END;
              fvalu := val;
              insymbol
            END
          ELSE { plus/minus sign, int, real, string }
            BEGIN
              sign := none;
              IF sy IN [plus,minus] THEN
                BEGIN
                  IF sy = plus THEN sign := pos ELSE sign := neg;
                  insymbol
                END;
              IF sy = ident THEN { <ident> or "+"<ident> or "-"<ident> }
                BEGIN
                  searchid([konst], lcp, true);
                  insymbol;
                  IF (lcp = ucstptr) AND (sy = lparen) THEN { function? }
                    unknownParenConstruct(104);
                  WITH lcp^ DO
                    BEGIN lsp := idtype; fvalu := values END;
                  IF sign <> none THEN
                    IF lsp = intptr THEN
                      IF sign = neg THEN
                        fvalu.ival := -fvalu.ival
                      ELSE
                    ELSE
                    IF lsp = realptr THEN
                      IF sign = neg THEN
                        BEGIN
                          { make a copy of the linked list for a new constant }
                          getreelcst(lkonp); { new constant }
                          oldlist := fvalu.valp^.rval; { ptr to old linkedlist }
                          getstr(lkonp^.rval); { first str of new list alloc'd }
                          newlist := lkonp^.rval;       { newlist points to it }
                          newlist^.str := oldlist^.str; { make the first copy }
                          oldlist := oldlist^.next;  { check for add'l quanta }
                          WHILE oldlist <> NIL DO    { if any, copy them too }
                            BEGIN
                              getstr(newlist^.next);
                              newlist := newlist^.next;
                              newlist^.str := oldlist^.str;
                              oldlist := oldlist^.next;
                            END;
                          { re-point to first strs: }
                          newlist := lkonp^.rval;
                          oldlist := fvalu.valp^.rval;
                          IF oldlist^.str[1] = '-' THEN newlist^.str[1] := '+'
                                                   ELSE newlist^.str[1] := '-';
                          fvalu.valp := lkonp;
                        END
                      ELSE
                    ELSE
                      error(105);
                  IF lcp = ucstptr THEN
                    fvalu.ival := maxint; { to prevent error(102) }
                END
              ELSE
              IF sy = intconst THEN
                BEGIN
                  IF sign = neg THEN val.ival := -val.ival;
                  lsp := intptr; fvalu := val;
                  insymbol
                END
              ELSE
              IF sy = realconst THEN
                BEGIN
                  IF sign = neg THEN
                    val.valp^.rval^.str[1] := '-';
                  lsp := realptr; fvalu := val; insymbol
                END
              ELSE
                error(50);
            END;
        END;
      fsp := lsp
    END (* constant *);

    FUNCTION string(fsp: tIptr): Boolean;
      VAR   fmin,fmax: integer;
    BEGIN
      string := false;
      IF fsp <> NIL THEN
        BEGIN
          fmin := 1; fmax := fsp^.size;
          IF (fsp^.form = arrays) AND fsp^.packing THEN
            BEGIN
              IF fsp^.inxtype <> NIL THEN
                IF fsp^.inxtype^.form <> confinx THEN
                  getbounds(fsp^.inxtype, fmin, fmax);
              IF (fsp^.aeltype = charptr) AND (fmin = 1) AND (fmax > 1) THEN
                string := true
            END
        END
    END (*string*) ;

    FUNCTION comptypes(fsp1,fsp2: tIptr): Boolean;
      (*decide whether structures pointed at by fsp1 and fsp2 are compatible*)
    BEGIN
      comptypes := false; { by default }
      IF fsp1 = fsp2 THEN { Check equal. Aliases of the same type }
        comptypes := true { will also be equal. }
      ELSE
        IF (fsp1 <> NIL) AND (fsp2 <> NIL) THEN { no errors so far }
          IF fsp1^.form = fsp2^.form THEN
            CASE fsp1^.form OF
              scalar: ;
              { Subranges are compatible if either type is a subrange of the
                other, or if the base type is the same. }
              subrange: comptypes := (fsp1^.rangetype = fsp2) OR
                                     (fsp2^.rangetype = fsp1) OR
                                     (fsp1^.rangetype = fsp2^.rangetype);
              { Sets are compatible if they have the same base types and packed/
                unpacked status, or one of them is the empty set. The empty set
                is indicated by a nil base type, which is identical to a base
                type in error. Either way, we treat them as compatible.

                Set types created for set constants have a flag that disables
                packing matches. This is because set constants can be packed or
                unpacked by context. }
              confInx: comptypes := comptypes(fsp1^.TidOrd, fsp2^.TidOrd);
              { Pointers, must either be the same type or aliases of the same
                type, or one must be nil. The nil pointer is indicated by a nil
                base type, which is identical to a base type in error. Either
                way, we treat them as compatible. }
              pointer:
                comptypes := (fsp1^.eltype = NIL) OR (fsp2^.eltype = NIL);
              powerset: comptypes := (comptypes(fsp1^.elset, fsp2^.elset) AND
                                    ((fsp1^.packing = fsp2^.packing) OR
                                     NOT fsp1^.matchpack OR
                                     NOT fsp2^.matchpack)) OR
                                  (fsp1^.elset = NIL) OR (fsp2^.elset = NIL);
              { Arrays are compatible if they are strings of the same size }
              { or when an array is passed to a conformant array parameter }
              { or to a parameter with identical dimensions and element types }
              arrays: comptypes := (string(fsp1) AND string(fsp2) AND
                                    (fsp1^.size = fsp2^.size ));
              { records and files must either be the same type or }
              { aliases of the same type }
              records: ;
              files:
            END (*CASE*)
          ELSE (* fsp1^.form <> fsp2^.form *)
            { subranges of a base type match the base type }
            IF fsp1^.form = subrange THEN
              comptypes := comptypes(fsp1^.rangetype, fsp2)
            ELSE
            IF fsp2^.form = subrange THEN
              comptypes := comptypes(fsp1, fsp2^.rangetype)
            ELSE
            IF fsp1^.form = confInx THEN
              comptypes := comptypes(fsp1^.TidOrd, fsp2)
            ELSE
            IF fsp2^.form = confInx THEN
              comptypes := comptypes(fsp1, fsp2^.TidOrd)
            ELSE
              comptypes := false
        ELSE { (fsp1 = NIL) OR (fsp2 = NIL): previous error }
          comptypes := true { don't add to it }
    END (* comptypes *);

    { check whether fsp is a file, or is a structure containing a file }
    FUNCTION filecomponent(fsp: tIptr): Boolean;

      FUNCTION filecomponentre(lcp: sTptr): Boolean;
      BEGIN
        IF lcp <> NIL THEN
          WITH lcp^ DO
            BEGIN
              IF filecomponent(idtype)  THEN filecomponentre := true
              ELSE
              IF filecomponentre(llink) THEN filecomponentre := true
              ELSE
              IF filecomponentre(rlink) THEN filecomponentre := true
                                        ELSE filecomponentre := false
            END
        ELSE
          filecomponentre := false;
      END; { filecomponentre }

    BEGIN
      IF fsp <> NIL THEN
        WITH fsp^ DO
          IF form = arrays THEN
            filecomponent := filecomponent(aeltype)
          ELSE
          IF form = records THEN
            filecomponent := filecomponentre(fstfld)
          ELSE
          IF form = files THEN
            filecomponent := true
          ELSE
            filecomponent := false
      ELSE
        filecomponent := false;
    END; { filecomponent }

    { resolve all pointer references in the forward list }
    PROCEDURE resolvep;
      VAR   lcp1, lcp2: sTptr;
            notQuite, fe: Boolean;
    BEGIN
      fe := true;
      WHILE fwptr <> NIL DO
        BEGIN
          lcp1 := fwptr;
          fwptr := fwptr^.next;
          idll := lcp1^.name;
          searchId([types], lcp2, false);
          IF lcp2 <> NIL THEN { type-id found }
            BEGIN
              lcp1^.name := NIL; { so we don't try to dispose again }
              lcp1^.idtype^.eltype := lcp2^.idtype;
              putid(lcp1);
            END
          ELSE
            BEGIN
              IF fe THEN
                BEGIN error(117); writeln(output) END;
              write('*** undefined type-id forward reference: ');
              dumpname(lcp1^.name);
              writeln;
              fe := false
            END;
        END;
    END;

    PROCEDURE typ(fsys: setofsys; VAR fTIptr: tIptr; VAR fsize: addrrange);
      VAR   lsp,lsp1,lsp2: tIptr;
            oldtop: disprange;
            lcp: sTptr;
            lsize,saveLc: addrrange;
            lmin,lmax: integer;
            commaSeen,andThen: Boolean;
            ispacked: Boolean;

      PROCEDURE simpTyp(fsys: setofsys; VAR fTIptr: tIptr; VAR fsize:addrrange);
        VAR   lsp,lsp1: tIptr;
              lcp,lcp1: sTptr;
              saveTop: disprange;
              lcnt: integer;
              lvalu: valu;
      BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>simpTyp');
  END;
{$endif}
        fsize := 1; lsp := NIL;
        IF NOT (sy IN simptypebegsys) THEN
          BEGIN error(1); skip(fsys + simptypebegsys) END;
        IF sy IN simptypebegsys THEN
          IF sy = lparen THEN
            BEGIN { enumeration type constants }
              saveTop := top;   (*decl. consts local to innermost block*)
              WHILE display[top].occur <> blck DO
                top := top - 1;
              getstc(lsp);
              WITH lsp^ DO
                BEGIN
                  size := intsize;
                  form := scalar;
                  scalkind := expdclrd
                END;
              lcp1 := NIL;
              lcnt := 0;
              REPEAT
                insymbol;
                IF sy = ident THEN
                  BEGIN
                    getid(lcp);
                    WITH lcp^ DO
                      BEGIN name := idll; idtype := lsp; next := lcp1;
                        klass := konst; values.ival := lcnt;
                      END;
                    enterId(lcp);
                    lcnt := lcnt + 1;
                    lcp1 := lcp;
                    insymbol
                  END
                ELSE
                  BEGIN error(2); skip(fsys + [comma,rparen]) END;
              UNTIL sy <> comma;
              lsp^.fenum := lcp1;
              top := saveTop;
              IF sy = rparen THEN insymbol ELSE error(18)
            END { enumerated type }
          ELSE
          IF sy = ident THEN
            BEGIN
              searchid([types,konst], lcp, true);
              insymbol;
              IF lcp = utypptr THEN { unknown id: neither a type nor konst }
                IF sy = lparen THEN { string, ord, chr? }
                  BEGIN { recover from previous error as best we can }
                    REPEAT
                      insymbol; { eat up the lparen (or comma) }
                      constant(fsys + [rparen,comma], lsp1, lvalu);
                    UNTIL sy <> comma;
                    IF sy = rparen THEN insymbol;
                    IF sy = range THEN
                      lcp := ucstptr; { so the next 'then' branch will be taken }
                  END
                ELSE
                IF lcp^.name^.str = 'bindable    ' THEN
                  BEGIN
                    error(33);
                    typ(fsys, ftIptr, fsize);
                  END
                ELSE
              ELSE
              IF lcp^.klass = konst THEN
                BEGIN
                  getstc(lsp);
                  WITH lsp^, lcp^ DO
                    BEGIN
                      form := subrange;
                      rangetype := idtype;
                      IF string(rangetype) THEN
                        BEGIN error(148); rangetype := NIL END;
                      min := values;
                      size := intsize
                    END;
                  IF sy = range THEN insymbol ELSE error(5);
                  constant(fsys, lsp1, lvalu);
                  lsp^.max := lvalu;
                  IF lsp1 <> NIL THEN
                    IF lsp^.rangetype <> lsp1 THEN
                      error(107)
                END
              ELSE
                BEGIN { type id, possible a subrange, but it would }
                  lsp := lcp^.idtype; { have been checked earlier }
                  IF lsp <> NIL THEN
                    fsize := lsp^.size
                END;
            END (*sy = ident*)
          ELSE { not lparen and not ident }
            BEGIN
              getstc(lsp);
              lsp^.form := subrange;
              constant(fsys + [range], lsp1, lvalu);
              IF string(lsp1) THEN
                BEGIN error(148); lsp1 := NIL END;
              WITH lsp^ DO
                BEGIN
                  rangetype := lsp1;
                  min := lvalu;
                  size := intsize
                END;
              IF sy = range THEN insymbol ELSE error(5);
              constant(fsys, lsp1, lvalu);
              lsp^.max := lvalu;
              WITH lsp^ DO
                IF rangetype <> NIL THEN
                  BEGIN
                    IF (lsp1 <> NIL) AND (lsp^.rangetype <> lsp1) THEN
                      error(107);
                    IF rangetype = realptr THEN
                      error(91)
                    ELSE
                    IF min.ival > max.ival THEN
                      error(102)
                  END
            END;
        fTIptr := lsp;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<simpTyp');
    indent := indent - 1;
  END;
{$endif}
      END (* simpTyp *);

      PROCEDURE fieldlist(fsys: setofsys;
                          VAR fRecordVariant: tIptr);
        VAR   lcp,selectorTypeId,first,last: sTptr;
              lsp,lsp1,lsp2,lsp3,lsp4: tIptr;
              minsize,maxsize,lsize: addrrange;
              lvalu: valu;
              commaSeen,semicolonSeen: Boolean;
              idllSave: strllp;
      BEGIN
        IF NOT (sy IN (fsys+[ident,casesy])) THEN
          BEGIN error(19); skip(fsys + [ident,casesy]) END;
        WHILE sy = ident DO
          BEGIN
            first := NIL;
            REPEAT
              IF sy = ident THEN
                BEGIN
                  getid(lcp);
                  WITH lcp^ DO
                    BEGIN name := idll; idtype := NIL;
                      klass := field; selectr := false;
                    END;
                  add2list(first, last, lcp);
                  enterId(lcp);
                  insymbol
                END
              ELSE
                error(2);
              IF NOT (sy IN [comma,colon]) THEN
                BEGIN
                  error(19);
                  skip(fsys + [comma,colon,semicolon,casesy])
                END;
              commaSeen := sy = comma;
              IF commaSeen THEN insymbol
            UNTIL NOT commaSeen;
            IF sy = colon THEN insymbol ELSE error(5);
            typ(fsys + [casesy,semicolon], lsp, lsize);
            align(lsp, lc);
            updatelist(first, lsize, lsp);
            WHILE sy = semicolon DO
              BEGIN
                insymbol;
                IF NOT (sy IN fsys + [ident,casesy,semicolon]) THEN
                  BEGIN error(19); skip(fsys + [ident,casesy]) END
              END
          END (* WHILE sy = ident *);
        IF sy = casesy THEN
          BEGIN
            getstc(lsp);
            WITH lsp^ DO
              BEGIN form := tagfld; tagfieldp := NIL; fstvar := NIL END;
            fRecordVariant := lsp;
            insymbol;
            IF sy = ident THEN { could be a field, or just a naked type }
              BEGIN
                idllSave := idll;
                insymbol; { scanning 'OF', e.g., will destroy idll }
                IF sy = colon THEN
                  BEGIN { it's a selector field, whether found or not }
                    searchId([types], selectorTypeId, false);
                    IF selectorTypeId = NIL THEN { new id: no problem }
                      BEGIN getid(lcp); lcp^.name := idll END
                    ELSE
                      lcp := selectorTypeId; { known id: redefine as field }
                    WITH lcp^ DO { first set it up for a selector }
                      BEGIN
                        idtype := NIL; klass := field;
                        addr := lc; selectr := true;
                      END;
                    enterId(lcp);
                    insymbol; { eat the colon }
                    IF sy = ident THEN { this should be a known type }
                      BEGIN
                        searchid([types], selectorTypeId, true);
                        insymbol
                      END
                    ELSE
                      BEGIN { recover best we can }
                        error(2); skip(fsys + [ofsy,lparen]);
                        selectorTypeId := utypptr;
                      END
                  END
                ELSE { no colon: must be type, else error }
                  BEGIN
                    idll := idllSave;
                    searchId([types], selectorTypeId, true);
                    lcp := NIL; { no field name }
                  END;
                lsp1 := selectorTypeId^.idtype;
                IF lsp1 <> NIL THEN { takes care of utypptr, or type w/ error }
                  BEGIN
                    IF lcp <> NIL THEN
                      BEGIN
                        align(lsp1, lc);
                        lcp^.addr := lc;
                        lc := lc + lsp1^.size;
                        lcp^.idtype := lsp1;
                      END;
                    IF string(lsp1) THEN
                      error(92)
                    ELSE
                    IF lsp1^.form > subrange THEN
                      error(110)
                    ELSE
                    IF comptypes(realptr, lsp1) THEN
                      error(110);
                    lsp^.tagfieldp := lcp;
                  END (* IF lsp1 <> NIL THEN *)
              END (* IF sy = ident *)
            ELSE
              BEGIN error(2); skip(fsys + [ofsy,lparen]) END;
            lsp^.size := lc;
            IF sy = ofsy THEN insymbol ELSE error(8);
            lsp1 := NIL;
            minsize := lc;
            maxsize := lc;
            REPEAT
              lsp2 := NIL;
              IF NOT (sy IN fsys + [semicolon]) THEN
                BEGIN
                  REPEAT
                    constant(fsys + [comma,colon,lparen], lsp3, lvalu);
                    IF selectorTypeId^.idtype <> NIL THEN
                      IF NOT comptypes(selectorTypeId^.idtype, lsp3) THEN
                        error(111);
                    getstc(lsp3);
                    WITH lsp3^ DO
                      BEGIN
                        form := variant;
                        prvcon := lsp1; subvar := lsp2; varval := lvalu.ival;
                      END;
                    lsp4 := lsp1;
                    WHILE lsp4 <> NIL DO
                      BEGIN
                        WITH lsp4^ DO
                          IF varval = lvalu.ival THEN
                            error(177);
                        lsp4 := lsp4^.prvcon
                      END;
                    lsp1 := lsp3;
                    lsp2 := lsp3;
                    commaSeen := sy = comma;
                    IF sy = range THEN
                      BEGIN error(20); commaseen := true END;
                    IF commaSeen THEN insymbol;
                  UNTIL NOT commaSeen;
                  IF sy = colon THEN insymbol ELSE error(5);
                  IF sy = lparen THEN insymbol ELSE error(9);
                  fieldlist(fsys + [rparen,semicolon], lsp2);
                  IF lc > maxsize THEN maxsize := lc;
                  WHILE lsp3 <> NIL DO
                    BEGIN lsp4 := lsp3^.subvar; lsp3^.subvar := lsp2;
                      lsp3^.size := lc;
                      lsp3 := lsp4
                    END;
                  IF sy = rparen THEN
                    BEGIN insymbol;
                      IF NOT (sy IN fsys + [semicolon]) THEN
                        BEGIN error(25); skip(fsys + [semicolon]) END
                    END
                  ELSE error(4);
                END;
              semicolonSeen := sy = semicolon;
              IF semicolonSeen THEN
                BEGIN
                  lc := minsize;
                  insymbol
                END
            UNTIL NOT semicolonSeen;
            lc := maxsize;
            lsp^.fstvar := lsp1;
          END
        ELSE { sy <> casesy }
          fRecordVariant := NIL
      END (* fieldlist *);

    BEGIN (* typ *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>typ');
  END;
{$endif}
      IF NOT (sy IN typebegsys) THEN
         BEGIN error(10); skip(fsys + typebegsys) END;
      IF sy IN typebegsys THEN
        IF sy IN simptypebegsys THEN
          simpTyp(fsys, fTIptr, fsize)
        ELSE
        IF sy = typesy THEN { type inquiry in Extended Pascal }
          BEGIN
            error(34); fTIptr := NIL;
            insymbol;
            IF sy = ofsy THEN insymbol ELSE error(8);
            IF sy = ident THEN insymbol;
            skip(fsys-[ident,period,lparen,lbrack]);
          END
        ELSE
{ ^ }   IF sy = arrow THEN
          BEGIN
            getstc(lsp);
            fTIptr := lsp;
            WITH lsp^ DO
              BEGIN
                form := pointer;
                eltype := NIL;
                size := ptrsize;
              END;
            insymbol;
            IF sy = ident THEN
              BEGIN { forward reference everything }
                getid(lcp);
                WITH lcp^ DO
                  BEGIN name := idll; idtype := lsp; next := fwptr;
                    klass := types
                  END;
                fwptr := lcp;
                insymbol;
              END
            ELSE
              error(2);
          END
        ELSE
          BEGIN
            IF sy = packedsy THEN
              BEGIN
                ispacked := true;
                insymbol;
                IF NOT (sy IN typedels) THEN
                  BEGIN error(10); skip(fsys + typedels) END
              END
            ELSE
              ispacked := false;  
            lsp := NIL;
{array}     IF sy = arraysy THEN
              BEGIN
                insymbol;
                IF sy = lbrack THEN insymbol ELSE error(11);
                lsp1 := NIL;
                REPEAT
                  getstc(lsp);
                  WITH lsp^ DO
                    BEGIN form := arrays; aeltype := lsp1; inxtype := NIL;
                      packing := ispacked
                    END;
                  lsp1 := lsp;
                  simpTyp(fsys + [comma,rbrack,ofsy], lsp2, lsize);
                  lsp1^.size := lsize;
                  IF lsp2 <> NIL THEN
                    IF lsp2^.form <= subrange THEN
                      BEGIN
                        IF lsp2 = realptr THEN
                          BEGIN error(106); lsp2 := NIL END
                        ELSE
                        IF lsp2 = intptr THEN
                          BEGIN error(149); lsp2 := NIL END;
                        lsp^.inxtype := lsp2
                      END
                    ELSE
                      BEGIN error(113); lsp2 := NIL END;
                  commaSeen := sy = comma;
                  IF commaSeen THEN insymbol
                UNTIL NOT commaSeen;
                IF sy = rbrack THEN insymbol ELSE error(12);
                IF sy = ofsy THEN insymbol ELSE error(8);
                typ(fsys, lsp, lsize);
                IF (lsp = charptr) AND NOT ispacked THEN
                  {error(87)}; { suggest using 'PACKED' }
                REPEAT
                  WITH lsp1^ DO
                    BEGIN
                      lsp2 := aeltype; aeltype := lsp;
                      IF inxtype <> NIL THEN
                        BEGIN
                          getbounds(inxtype, lmin, lmax);
                          lsize := lsize*(lmax - lmin + 1);
                          size := lsize
                        END
                    END;
                  lsp := lsp1; lsp1 := lsp2
                UNTIL lsp1 = NIL
              END
            ELSE
{record}    IF sy = recordsy THEN
              BEGIN
                insymbol;
                oldtop := top;
                IF top < displimit THEN
                  BEGIN
                    top := top + 1;
                    WITH display[top] DO
                      BEGIN
                        fname := NIL;
                        flabel := NIL;
                        fconst := NIL;
                        ftype := NIL;
                        forwProcFunc := NIL;
                        occur := rec
                      END
                  END
                ELSE
                  error(64);
                saveLc := lc;
                lc := 0;
                fieldlist(fsys-[semicolon]+[endsy], lsp1);
                getstc(lsp);
                WITH lsp^, display[top] DO
                  BEGIN
                    size := lc;
                    packing := ispacked;
                    form := records;
                    fstfld := fname; fname := NIL;
                    recvar := lsp1;
                    recyc := ftype; ftype := NIL
                  END;
                lc := saveLc;
                putdsps(oldtop);
                top := oldtop;
                IF sy = endsy THEN insymbol ELSE error(13)
              END
            ELSE
{set}       IF sy = setsy THEN
              BEGIN
                insymbol;
                IF sy = ofsy THEN insymbol ELSE error(8);
                simpTyp(fsys, lsp1, lsize);
                IF lsp1 <> NIL THEN
                  IF lsp1^.form > subrange THEN
                    BEGIN error(115); lsp1 := NIL END
                  ELSE
                    IF lsp1 = realptr THEN
                      BEGIN error(114); lsp1 := NIL END
                    ELSE
                    IF lsp1 = intptr THEN
                      BEGIN error(74); lsp1 := NIL END
                    ELSE
                      BEGIN
                        getbounds(lsp1, lmin, lmax);
                        IF (lmin < setlow) OR (lmax > sethigh) THEN
                          error(74);
                        lsize := lmax DIV 8 + 1; { bytes needed }
                      END;
                getstc(lsp);
                WITH lsp^ DO
                  BEGIN
                    size := lsize;
                    form := powerset;
                    packing := ispacked;
                    elset := lsp1;
                    matchpack := true
                  END;
              END
            ELSE
{file}      IF sy = filesy THEN
              BEGIN
                insymbol;
                IF sy <> ofsy THEN
                  BEGIN error(8); skip(fsys+[ofsy]) END;
                IF sy = ofsy THEN
                  insymbol;
                typ(fsys, lsp1, lsize);
                IF filecomponent(lsp1) THEN
                  error(190);
                getstc(lsp);
                WITH lsp^ DO
                  BEGIN
                    size := filesize + lsize;
                    form := files;
                    packing := ispacked;
                    compType := lsp1;
                  END
              END;
            IF lsp <> NIL THEN
              align(lsp, lsp^.size); { align once and for all }
            fTIptr := lsp
          END
      ELSE
        fTIptr := NIL;

      andThen := false;
      IF sy = ident THEN
        andThen := idll^.str = 'value       ';
      IF (sy = eqop) OR
         andThen {((sy = ident) AND_THEN (idll^.str = 'value       '))} THEN
        BEGIN error(31); skip(lsys + [semicolon]) END;
        
      IF fTIptr = NIL THEN
        fsize := 1
      ELSE
        fsize := fTIptr^.size;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<typ');
    indent := indent - 1;
  END;
{$endif}
    END (* typ *);

    PROCEDURE labeldeclaration;
            
      PROCEDURE handleLabel;
        VAR   llp: lbp;
      BEGIN
        IF sy = intconst THEN
          BEGIN
            IF val.ival > maxlabel THEN
              error(185);
            searchlabel(llp, top); { search preexisting label }
            IF llp <> NIL THEN error(166) { multideclared label }
            ELSE BEGIN newlabel(llp); llp^.undeclared := false END;
            insymbol
          END
        ELSE
          error(15);
      END;
      
    BEGIN
      lsys := lsys - [labelsy];
      handleLabel;
      WHILE sy = comma DO
        BEGIN
          insymbol;
          handleLabel
        END;
      IF sy = semicolon THEN
        insymbol
      ELSE
        BEGIN error(14); skip(lsys) END;
    END (* labeldeclaration *);

    PROCEDURE constdeclaration;
      VAR   lcp: sTptr;
    BEGIN
      lsys := lsys - [labelsy,constsy];
      IF sy <> ident THEN
        BEGIN error(2); skip(lsys + [ident]) END;
      WHILE sy = ident DO
        BEGIN
          getid(lcp);
          WITH lcp^ DO
            BEGIN name := idll; idtype := NIL; klass := konst END;
          insymbol;
          IF sy = eqop THEN insymbol ELSE error(16);
          WITH lcp^ DO
            constant(lsys + [semicolon], idtype, values);
          enterId(lcp);
          IF sy = semicolon THEN
            insymbol
          ELSE
            BEGIN
              error(14);
              skip(lsys + [semicolon]);
              IF sy = semicolon THEN insymbol
            END
        END;
    END (* constdeclaration *);

    PROCEDURE typedeclaration;
      VAR   newTypeName: sTptr;
            theNewType: tIptr;
            lsize: addrrange;
    BEGIN
      lsys := lsys - [labelsy,constsy,typesy];
      IF sy <> ident THEN
        BEGIN error(2); skip(lsys + [ident]) END;
      WHILE sy = ident DO
        BEGIN
          getid(newTypeName);
          WITH newTypeName^ DO
            BEGIN name := idll; idtype := NIL; klass := types END;
          insymbol;
          IF sy = lparen THEN
            unknownParenConstruct(39);
          IF sy = eqop THEN insymbol ELSE error(16);
          typ(lsys + [semicolon], theNewType, lsize);
          enterId(newTypeName);
          newTypeName^.idtype := theNewType;
          IF sy = semicolon THEN
            insymbol
          ELSE
            BEGIN error(14); skip(lsys + [ident]) END
        END;
      resolvep;
    END (* typedeclaration *);

    PROCEDURE vardeclaration;
      VAR   first,last: sTptr;
            lsp: tIptr;
            lsize: addrrange;
            
      PROCEDURE handleVar;
        VAR   lcp: sTptr;
      BEGIN
        IF sy = ident THEN
          BEGIN
            getid(lcp);
            WITH lcp^ DO
              BEGIN
                name := idll; idtype := NIL;
                klass := vars; vkind := actual; vlev := level;
                vbound := false; readonly := false;
              END;
            enterId(lcp);
            add2list(first, last, lcp);
            insymbol;
          END
        ELSE
          error(2);
      END;
      
    BEGIN
      lsys := lsys - [labelsy,constsy,typesy,varsy];
      REPEAT
        first := NIL;
        handleVar;
        WHILE sy = comma DO
          BEGIN
            insymbol;
            handleVar;
          END;
        IF sy = colon THEN insymbol ELSE error(5);
        typ(lsys + [semicolon], lsp, lsize);
        IF lsp = NIL THEN { error }
          IF (sy = procsy) OR (sy = funcsy) THEN
            skip(lsys + [semicolon] - [procsy,funcsy])
          ELSE
{           updatelist(first, intsize, utypptr)  to minimize problems later }
        ELSE
          BEGIN
            align(lsp, lc);
            updatelist(first, lsize, lsp); { updating lc & lsp }
          END;
        IF sy = semicolon THEN
          insymbol
        ELSE
          BEGIN error(14); skip(lsys + [ident]) END
      UNTIL (sy <> ident);
      resolvep; { because type-denoter can be ^<domain-type> }
    END (* vardeclaration *);

    PROCEDURE procdeclaration(fsy: symbol);
      VAR   oldlev: levrange;
            lcp,lcp1,lcp2: sTptr;
            lsp: tIptr;
            forw,               { proc id was forwar-declared previously }
            andThen: Boolean;   { the directive 'forward' was just seen }
            oldtop: disprange;
            saveLc,lcm: addrrange;

      PROCEDURE pushlvl(forw: Boolean; lcp: sTptr);
      BEGIN
        IF level < maxlevel THEN
          level := level + 1
        ELSE
          error(64);
        IF top < displimit THEN
          BEGIN
            top := top + 1;
            WITH display[top] DO
              BEGIN
                IF forw THEN fname := lcp^.params
                        ELSE fname := NIL;
                flabel := NIL; fconst := NIL; ftype := NIL; forwProcFunc := NIL;
                occur := blck; bname := lcp
              END
          END
        ELSE
          error(64);
      END;

      PROCEDURE parameterlist(fsys: setofsys; VAR fpar: sTptr);
        VAR   allinxes,allast,first,last,plast,lcp,lcp2: sTptr;
              lsp: tIptr;
              lkind: idkind;
              lsize: addrrange;
              oldlev: levrange;
              oldtop: disprange;
              saveLc: addrrange;
              commaSeen,protectedSeen: Boolean;
              lsy: symbol;

        PROCEDURE conformantArray(fsys: setofsys; VAR fsp: tIptr);
          LABEL 1;
          VAR   ispacked: Boolean;
                semicolonSeen: Boolean;
                lsize: addrrange;
                firstinx,last,lcp,lcp2,ordtyp: sTptr;
                lsp,lsp1,lsp2,lsp3: tIptr;
        BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>conformantArray');
  END;
{$endif}
          lsp1 := NIL;
          ispacked := sy = packedsy;
          IF ispacked THEN
            insymbol;
          IF sy = arraysy THEN insymbol
                          ELSE BEGIN error(22); skip(fsys+[arraysy]) END;
          IF sy = ofsy THEN { open array syntax: not in 7185 }
            BEGIN
              error(38);
              top := top - 1;
              getstc(lsp1);
              top := top + 1;
              WITH lsp1^ DO
                BEGIN form := arrays; aeltype := NIL; inxtype := NIL;
                  packing := ispacked
                END;
              GOTO 1;
            END;
          IF sy = lbrack THEN insymbol ELSE error(11);
          REPEAT
            firstinx := NIL;
            { the conformant array parameter spec is only place in the Pascal }
            { language where a parameter's type is not taken from a surrounding }
            { lexical level; to keep the type from being re-cycled prematurely, }
            { attach it to the display level one less than the current level: }
            top := top - 1;
            getstc(lsp);
            top := top + 1;
            WITH lsp^ DO
              BEGIN form := arrays; aeltype := lsp1; inxtype := NIL;
                packing := ispacked
              END;
            lsp1 := lsp;
            IF sy = ident THEN { low bound }
              BEGIN
                getid(lcp);
                WITH lcp^ DO
                  BEGIN
                    name := idll; idtype := NIL; keep := true;
                    klass := vars; vbound := true; readonly := false;
                    vkind := actual; vlev := level;
                  END;
                enterId(lcp);
                add2list(firstinx, last, lcp);
                insymbol;
              END
            ELSE  
              BEGIN error(2); skip(fsys+[range,ident,colon,rbrack]) END;  
            IF sy = range THEN
              insymbol
            ELSE
              BEGIN error(23); IF sy = comma THEN insymbol END;
            IF sy = ident THEN { high bound }
              BEGIN
                getid(lcp2);
                WITH lcp2^ DO
                  BEGIN
                    name := idll; idtype := NIL; keep := true;
                    klass := vars; vbound := true; readonly := false;
                    vkind := actual; vlev := level;
                  END;
                enterId(lcp2);
                add2list(firstinx, last, lcp2);
                insymbol;
              END
            ELSE
              error(2);
            top := top - 1; { see comment above }
            getstc(lsp3);
            top := top + 1;
            WITH lsp3^ DO
              BEGIN
                form := confInx;
                lobnd := lcp; hibnd := lcp2;
              END;
            lsp1^.inxtype := lsp3;
            IF sy = colon THEN insymbol ELSE error(5);
            IF sy = ident THEN
              BEGIN { should be an ordinal type identifier }
                searchid([types], ordtyp, true); { ordtyp never NIL }
                lsp2 := ordtyp^.idtype;
                IF lsp2 <> NIL THEN
                  IF (lsp2^.form > subrange) OR (lsp2 = realptr) THEN
                    error(196)
                  ELSE
                    BEGIN
                      lsize := lsp2^.size;
                      align(parmptr, lsize);
                      updatelist(firstinx, lsize, lsp2);
                      lc := lc + intsize; { make space for row size }
                      lsp3^.TidOrd := lsp2;
                    END;
                insymbol;
              END
            ELSE
              error(2);
            add2list(allinxes, allast, firstinx); { keep track of everyone }
            { tie the index spec lobnd & hibnd 'vars' into the list of params }
            semicolonSeen := sy = semicolon;
            IF semicolonSeen THEN
              BEGIN
                insymbol;
                IF ispacked THEN error(200);
              END;
          UNTIL NOT semicolonSeen;
          IF sy = rbrack THEN insymbol ELSE BEGIN error(24); skip(fsys) END;
1:        IF sy = ofsy THEN insymbol ELSE BEGIN error(8); skip(fsys) END;
          IF sy IN [packedsy,arraysy] THEN
            IF ispacked THEN
              BEGIN error(199); skip(fsys) END
            ELSE
              conformantArray(fsys, lsp)
          ELSE
          IF sy = ident THEN
            typ(fsys, lsp, lsize)
          ELSE
            BEGIN error(2); skip(fsys); lsp := NIL END;
          REPEAT
            WITH lsp1^ DO
              BEGIN
                lsp2 := aeltype; aeltype := lsp;
              END;
            lsp := lsp1; lsp1 := lsp2
          UNTIL lsp1 = NIL;
          fsp := lsp;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<conformantArray');
    indent := indent - 1;
  END;
{$endif}
        END; { conformantArray }
              
      BEGIN { parameterlist }
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>parameterlist');
  END;
{$endif}
        fpar := NIL;
        IF sy = lparen THEN
          BEGIN
            IF forw THEN error(119);
            insymbol;
            IF NOT (sy IN parambegsys) THEN
              BEGIN error(7); skip(fsys + parambegsys) END;
            WHILE sy IN parambegsys DO
              BEGIN
                first := NIL;
                lsy := sy;
                IF (lsy = procsy) OR (lsy = funcsy) THEN
                  BEGIN
                    insymbol;
                    IF sy = ident THEN
                      BEGIN
                        getid(lcp);
                        WITH lcp^ DO
                          BEGIN
                            IF lsy = funcsy THEN klass := func
                                            ELSE klass := proc;
                            name := idll; idtype := NIL;
                            pfdeckind := expdclrd;
                            pflev := level; pfkind := formal; keep := true
                          END;
                        enterId(lcp);
                        add2list(first, last, lcp);
                        align(parmptr, lc);
                        updatelist(first, ptrsize*2, NIL);
                        insymbol;
                      END
                    ELSE
                      BEGIN error(2); lcp := NIL END;
                    oldlev := level; oldtop := top; pushlvl(false, lcp);
                    saveLc := lc;
                    parameterlist(fsys + [colon,semicolon,rparen], lcp2);
                    lc := saveLc;
                    IF lcp <> NIL THEN lcp^.params := lcp2;
                    IF NOT (sy IN fsys+[semicolon,rparen,colon]) THEN
                      BEGIN error(7); skip(fsys+[semicolon,rparen,colon]) END;
                    IF lsy = funcsy THEN
                      IF sy = colon THEN
                        BEGIN
                          insymbol;
                          IF sy = ident THEN
                            BEGIN
                              searchid([types], lcp2, true);
                              lsp := lcp2^.idtype;
                              lcp^.idtype := lsp;
                              IF lsp <> NIL THEN
                                IF lsp^.form >= powerset THEN
                                  { i.e., set, record, array or file }
                                  BEGIN error(120); lsp := NIL END;
                              insymbol
                            END
                          ELSE
                            error(2);
                          IF NOT (sy IN fsys + [semicolon,rparen]) THEN
                            BEGIN error(7); skip(fsys+[semicolon,rparen]) END
                        END
                      ELSE
                        error(5);
                    level := oldlev; putdsps(oldtop); top := oldtop
                  END
                ELSE { sy not procsy or funcsy; must be varsy or ident }
                  BEGIN
                    protectedSeen := false;
                    IF sy = ident THEN
                      IF idll^.str = 'protected   ' THEN
                        BEGIN
                          protectedSeen := true; { but is it a legitimate id? }
                          WHILE ch <= ' ' DO nextch; { scan ahead }
                          IF ch IN [',',':'] THEN { it's a legit id }
                            protectedSeen := false;
                        END;
                    IF protectedSeen THEN
                      BEGIN
                        error(35);
                        insymbol;
                      END;
                    IF sy = constsy THEN
                      BEGIN error(30); lkind := formal; insymbol END;
                    IF sy = varsy THEN
                      BEGIN lkind := formal; insymbol END
                    ELSE
                      lkind := actual;
                    { parse comma-list of param ids }
                    REPEAT
                      IF sy = ident THEN
                        BEGIN
                          getid(lcp);
                          WITH lcp^ DO
                            BEGIN keep := true;
                              name := idll; idtype := NIL;
                              klass := vars; vkind := lkind; vbound := false;
                              vlev := level; readonly := false;
                            END;
                          enterId(lcp);
                          add2list(first, last, lcp);
                          insymbol;
                        END
                      ELSE
                        error(2);
                      IF sy = ident THEN { prior ident might be extension id }
                        BEGIN
                          error(20);
                          commaSeen := true { let's get past it & process id }
                        END
                      ELSE
                        BEGIN
                          IF NOT (sy IN [comma,colon] + fsys) THEN
                            BEGIN
                              error(7);
                              skip(fsys+[comma,semicolon,rparen])
                            END;
                          commaSeen := sy = comma;
                          IF commaSeen THEN insymbol
                        END
                    UNTIL NOT commaSeen;
                    IF sy = colon THEN
                      BEGIN
                        insymbol;
                        IF sy IN [arraysy,packedsy] THEN
                          BEGIN
                            align(parmptr, lc);
                            updatelist(first, ptrsize, NIL); { update the addrs }
                            allinxes := NIL;
                            conformantArray(fsys + [semicolon,rparen], lsp);
                            updatelist(first, 0, lsp); { only update the type }
                            IF allinxes <> NIL THEN
                              add2list(first, last, allinxes); { combine the lists }
                          END
                        ELSE
                        IF sy = ident THEN
                          BEGIN
                            searchid([types], lcp, true);
                            IF (lcp = utypptr) OR (lcp^.idtype = NIL) THEN
                              lsp := intptr
                            ELSE
                              lsp := lcp^.idtype;
                            IF lkind = formal THEN
                              lsize := ptrsize { reference }
                            ELSE
                              BEGIN
                                IF filecomponent(lsp) THEN
                                  error(80);
                                lsize := lsp^.size; { value }
                              END;
                            align(parmptr, lc);
                            updatelist(first, lsize, lsp);
                            insymbol
                          END
                        ELSE
                          error(2);
                        IF NOT (sy IN fsys + [semicolon,rparen,comma]) THEN
                          BEGIN error(7); skip(fsys+[semicolon,rparen]) END
                      END
                    ELSE { IF sy = colon }
                      error(5);
                  END;
                IF sy = semicolon THEN
                  BEGIN
                    insymbol;
                    IF NOT (sy IN fsys + parambegsys) THEN
                      BEGIN error(7); skip(fsys + parambegsys) END
                  END
                ELSE
                IF sy <> rparen THEN
                  BEGIN
                    error(29);
                    IF sy = comma THEN
                      insymbol;
                  END;
                add2list(fpar, plast, first)
              END (* WHILE sy IN parambegsys DO *);
            IF sy = rparen THEN insymbol ELSE error(4);
          END; { sy = lparen }
        IF NOT (sy IN fsys) THEN
          BEGIN error(7); skip(fsys) END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<parameterlist');
    indent := indent - 1;
  END;
{$endif}
      END (* parameterlist *);

    BEGIN (* procdeclaration *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>procdeclaration');
  END;
{$endif}
      saveLc := lc; { previous level's lc }
      lc := lcaftermarkstack;
      forw := false;
      IF sy = ident THEN
        BEGIN
          searchOneLevel(display[top].fname, lcp); (* decide whether forw. *)
          IF lcp <> NIL THEN { found: already declared: forward? }
            IF (lcp^.klass IN [proc,func]) AND {_THEN}
               (lcp^.pfdeckind = expdclrd) THEN
              BEGIN
                IF (lcp^.pfkind = actual) AND {_THEN} lcp^.forwdecl THEN
                  IF lcp^.klass = proc THEN forw := (fsy = procsy) ELSE
                  IF lcp^.klass = func THEN forw := (fsy = funcsy)
                                       ELSE forw := false;
                IF NOT forw THEN error(160)
              END
            ELSE
              error(101);
          IF forw THEN
            BEGIN
              lcp1 := display[top].forwProcFunc;
              lcp2 := NIL;
              WHILE lcp1 <> NIL DO
                IF lcp1 = lcp THEN
                  BEGIN { de-link it from fwptr list }
                    IF lcp2 = NIL THEN
                      display[top].forwProcFunc := lcp1^.next { de-link }
                    ELSE
                      lcp2^.next := lcp1^.next;
                    lcp1 := NIL
                  END
                ELSE
                  BEGIN lcp2 := lcp1; lcp1 := lcp1^.next END;
              lcp1 := lcp^.params;
              WHILE lcp1 <> NIL DO
                BEGIN
                  WITH lcp1^ DO
                    IF (klass = vars) AND (idtype <> NIL) THEN
                      BEGIN
                        lcm := addr + idtype^.size;
                        IF lcm > lc THEN
                          lc := lcm
                      END;
                  lcp1 := lcp1^.next
                END
            END
          ELSE
            BEGIN
              getid(lcp);
              WITH lcp^ DO
                BEGIN
                  IF idll = NIL THEN
                    strassvr(idll, 'dummy       ');
                  name := idll; idtype := NIL;
                  IF fsy = procsy THEN klass := proc
                                  ELSE klass := func;
                  pfdeckind := expdclrd;
                  pflev := level;
                  genlabel(pfname);
                  result := false;
                  pfkind := actual;
                  externl := false;
                  forwdecl := false;
                END;
              enterId(lcp)
            END;
          insymbol
        END
      ELSE
        BEGIN error(2); lcp := ufctptr END;
      oldlev := level;
      oldtop := top;
      pushlvl(forw, lcp);
      IF fsy = procsy THEN
        BEGIN
          parameterlist(fsys + [rparen,semicolon], lcp1);
          IF NOT forw THEN
            lcp^.params := lcp1
        END
      ELSE { fsy = funcsy }
        BEGIN
          parameterlist(fsys + [rparen,semicolon,colon], lcp1);
          IF NOT forw THEN
            lcp^.params := lcp1;
          IF sy = colon THEN
            BEGIN
              insymbol;
              IF sy = ident THEN
                BEGIN
                  IF forw THEN error(122);
                  searchid([types], lcp1, true);
                  lsp := lcp1^.idtype;
                  lcp^.idtype := lsp;
                  IF lsp <> NIL THEN
                    IF lsp^.form >= powerset THEN { set, record, array or file }
                      BEGIN error(120); lcp^.idtype := NIL END;
                  insymbol
                END
              ELSE
                BEGIN error(2); skip(fsys + [semicolon]) END
            END
          ELSE
            IF NOT forw THEN error(123)
        END;
      IF sy = semicolon THEN insymbol ELSE error(14);
      andThen := sy = ident;
      IF andThen THEN
        IF idll^.str <> 'forward     ' THEN
          andThen := false;
      IF andThen {(sy = ident) AND_THEN (idll^.str = 'forward     ')} THEN
        BEGIN                  { keep track of this forward declaration: }
          WITH display[top-1] DO { insert in forwProcFunc chain at this level }
            BEGIN lcp^.next := forwProcFunc; forwProcFunc := lcp END;
          putstrs(idll);
          IF forw THEN error(161)
                  ELSE lcp^.forwdecl := true;
          insymbol; { eat the 'forward' }
        END
      ELSE
        BEGIN
          lcp^.forwdecl := false;
          block(fsys, semicolon, lcp);
        END;
      IF sy = semicolon THEN
        insymbol
      ELSE
        error(14);
      level := oldlev;
      putdsps(oldtop);
      top := oldtop;
      lc := saveLc; { restore previous level's lc }
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<procdeclaration');
    indent := indent - 1;
  END;
{$endif}
    END (* procdeclaration *);

    PROCEDURE body(fsys: setofsys);
      CONST pmax = 8; pmaxm1 = 7;
            stringlgth  = 200; { same constant in pint.pas }
      TYPE  instr   =  RECORD
                         opcode: opcodes;
                         arg1, arg2, arg3: integer;
                         constText: PACKED ARRAY [1..stringlgth] OF char;
                       END;
            peeprng  = 0..pmaxm1;  { 0 to pmax-1 }
      VAR   i, len, entname, segsize: integer;
            lcmax: addrrange;
            llp: lbp;
            wereinafunc: Boolean;
            parm: sTptr;
            lparmTyp, linxTyp: tIptr;
            kp: konstp;

            { declarations for peephole optimizations }
            pin: peeprng; { next available slot in peep for gen'd instrs }
            pout: peeprng; { inx of oldest instr in peep, next to be emit'd }
            ps: peeprng; { # of instrs currently in peep, pout - pin MOD pmax }
            peep: ARRAY [peeprng] OF instr;

      PROCEDURE emit;
        VAR   i: integer;
      BEGIN
        WITH peep[pout] DO
          BEGIN
            write(p6, ' ', mn[opcode]);
            CASE opcode OF
              absi,absr,addi,addr,divi,divr,eofp,flt2,flt,
              modi,muli,mulr,negi,negr,inot,ixca,iodd,subi,subr,sqri,sqrr,
              trc,xjp,ujc,rnd,fbv,efb,fvb,dmp,cmln,chk0,chk1,
              asgn,dup,ret0,ret1,ret2,pop0,fatl,stop:;            { gen0 }
              loib,loiw,loid,stib,stiw,stid,
              logb,logw,logd,stgb,stgw,stgd,
              equ, neq, leq, geq, les, gtr, inn,
              equm,neqm,leqm,geqm,lesm,gtrm,sst1,sst2,
              dif,iand,ior,ixa,lag,mov,mst,sgs,rgs,cali,swp,
              decw,incw,locw,icgw,dcgw:
                write(p6, ' ', arg1:1);                          { gen1 }
              lal,pack,unpk,lip,iclw,dclw,chk2,loim,stim,
              lolb,lolw,lold,logm,stlb,stlw,stld,stgm: 
                write(p6, ' ', arg1:1, ' ', arg2:1);             { gen2 } 
              lolm,stlm:
                write(p6, ' ', arg1:1, ' ', arg2:1, ' ', arg3:1); { gen3 }  
              loca:
                BEGIN { genloca }
                  write(p6, ' ', arg1:1, ' |');
                  FOR i := 1 TO arg1 DO
                    write(p6, constText[i]);
                  write(p6, '|')                                 { genloca }
                END;
              locr:
                BEGIN
                  write(p6, ' ');
                  FOR i := 1 TO arg1 DO
                    write(p6, constText[i]);                     { genrcon }
                END;
              locs:  { gensetcon is handling this right now (temporarily?) }
                BEGIN
                  write(p6, ' ',arg1:1,' (');
                  FOR i := setlow TO sethigh DO
                    {IF i IN setttttt THEN}
                      write(p6, ' ', i:1);
                  write(p6,' )')
                END;
              csp:  { gencsp }  
                write(p6, ' ', sna[arg1]);
              ent,fjp,bra,sjp,tjp:            { gen1lab }  
                write(p6, ' L', arg1:1);
              cal,lpa:                        { gen2lab }
                write(p6, ' ', arg1:1, ' L', arg2:1);
              cas:                            { gen3lab }
                write(p6, ' ', arg1:1, ' ', arg2:1, ' L', arg3:1);
              ipj:  { genipj }  
                write(p6, ' ', arg1:1, ' L', arg2:1)
            END { CASE };
            writeln(p6);
          END; { WITH }
        IF pout = 0 THEN pout := pmaxm1
                    ELSE pout := pout - 1;
        ps := ps - 1; { one less instr in peep }
      END; { emit }

      PROCEDURE flushpeep;
      BEGIN
        WHILE ps <> 0 DO
          emit;
        pin := pmaxm1; pout := pmaxm1;
      END;

      PROCEDURE erase; { pin1 will be overwritten; therefore he is 'erased' }
      BEGIN
        IF pin = pmaxm1 THEN pin := 0 { wrap around }
                        ELSE pin := pin + 1;
        ps := ps - 1;
      END;
        
      PROCEDURE tryopt(pin1: peeprng);
        CONST loenc      = -536870912;     { -2**29 }
              hienc      =  536870911;     { 2**29 - 1 }
        VAR   pin2,pin3,pin4: peeprng;
              op1,op2,op3: opcodes;
              tmp: instr;
              arg,x,y: integer;
              
      BEGIN { tryopt }
        pin2 := (pin+2) MOD pmax; { index of 2nd youngest instr }
        op1 := peep[pin1].opcode;
        op2 := peep[pin2].opcode;
        arg := peep[pin2].arg1;
        CASE op1 OF
          logb,logw:
            IF op2 IN [stgb,stgw] THEN
              IF (corLoad[op2] = op1) AND
                 (arg = peep[pin1].arg1) THEN { loading a scalar that was just stored }
                BEGIN
                  peep[pin1].opcode := op2; { delay the store by 1 }
                  peep[pin2].opcode := dup; { duplicate TOS }
                END;      { and now no need to reload the value }
          lolb,lolw,lold:
            IF op2 IN [stlb,stlw,stld] THEN
              WITH peep[pin2] DO
                IF (corLoad[op2] = op1) AND
                   (arg1 = peep[pin1].arg1) AND
                   (arg2 = peep[pin1].arg2) THEN { loading a scalar that was just stored }
                  BEGIN
                    peep[pin1].opcode := op2; { delay the store by 1 }
                    peep[pin2].opcode := dup; { duplicate TOS }
                  END;      { and now no need to reload the value }
          locw:
            IF op2 = locw THEN { locw n, locw n -> locw n, dup }
              WITH peep[pin1] DO
                IF arg1 = arg THEN
                  opcode := dup;
          stgw:
            IF ps > 2 THEN
              IF (op2 = incw) OR (op2 = decw) THEN
                IF arg = 1 THEN
                  WITH peep[(pin+3) MOD pmax {pin3} ] DO
                    IF (opcode = logw) AND
                       (arg1 = peep[pin1].arg1) THEN { same address profile }
                      BEGIN
                        IF op2 = incw THEN opcode := icgw
                                      ELSE opcode := dcgw;
                        erase; erase;  
                      END;
          stlw:
            IF ps > 2 THEN
              IF ((op2 = incw) OR (op2 = decw)) AND (arg = 1) THEN
                WITH peep[(pin+3) MOD pmax {pin3} ] DO
                  IF (opcode = lolw) AND
                     (arg1 = peep[pin1].arg1) AND
                     (arg2 = peep[pin1].arg2) THEN { same address profile }
                    BEGIN
                      IF op2 = incw THEN opcode := iclw
                                    ELSE opcode := dclw;
                      erase; erase;  
                    END;
          iand:
            IF (op2 = locw) AND (arg = 1) THEN
              BEGIN erase; erase END; { locw 1, iand 1 -> ( P ^ true = P ) }
          addi:
            IF op2 = locw THEN
              IF (arg >= loenc) AND (arg <= hienc) THEN
                WITH peep[pin2] DO
                  BEGIN  { locw n, addi -> incw n }
                    opcode := incw;
                    erase;
                    tryopt(pin2); { is a further opt possible? }
                  END
              ELSE
            ELSE
            IF op2 = negi THEN
              BEGIN
                peep[pin1].opcode := subi;
                peep[pin2] := peep[pin1];
                erase;
              END
            ELSE
            IF (op2 = locw) AND (arg = 0) THEN
              BEGIN erase; erase END; { locw 0, addi -> ( POOF! ) }
          subi: 
            WITH peep[pin2] DO
              IF (op2 = locw) AND
                 (arg1 >= loenc) AND (arg1 <= hienc) THEN { locw n -> decw n }
                BEGIN
                  opcode := decw;
                  erase;
                  tryopt(pin2); { is a further opt possible? }
                END
              ELSE
              IF op2 = negi THEN
                BEGIN
                  peep[pin1].opcode := addi;
                  peep[pin2] := peep[pin1];
                  erase;
                END
              ELSE
              IF (op2 = locw) AND (arg = 0) THEN
                BEGIN erase; erase END; { locw 0, subi -> ( POOF! ) }
          divi:
            IF (op2 = locw) AND (arg = 1) THEN
              BEGIN erase; erase END;  { locw 1, divi -> ( POOF! ) }
          muli:
            IF op2 = locw THEN
              IF arg = 0 THEN   { locw 0, muli -> dmp, locw 0 }
                BEGIN peep[pin1] := peep[pin2]; peep[pin2].opcode := dmp END
              ELSE
              IF arg = 1 THEN   { locw 1, muli -> ( POOF!) }
                BEGIN erase; erase END
              ELSE
              IF arg = 2 THEN  { locw 2, muli -> dup, addi }
                BEGIN peep[pin2].opcode := dup; peep[pin1].opcode := addi END;
          addr:
            IF op2 = negr THEN
              BEGIN
                peep[pin1].opcode := subr;
                peep[pin2] := peep[pin1];
                erase;
              END;
          subr:
            IF op2 = negr THEN
              BEGIN
                peep[pin1].opcode := addr;
                peep[pin2] := peep[pin1];
                erase;
              END;
          incw:
            IF op2 = lag THEN { lag <n>, incw <p> -> lag <n+p>, (erase) }
              BEGIN
                WITH peep[pin2] DO
                  arg1 := arg1 + peep[pin1].arg1;
                erase;
              END
            ELSE
            IF op2 = decw THEN
              BEGIN { decw <x>, incw <y> -> decw <x-y> (or incw <y-x>) (, erase) }
                x := arg; y := peep[pin1].arg1;
                IF y > x THEN
                  BEGIN peep[pin2].arg1 := y - x; erase END
                ELSE
                IF y < x THEN
                  WITH peep[pin2] DO
                    BEGIN opcode := incw; arg1 := x - y; erase; tryopt(pin2) END
                ELSE { x = y }
                  BEGIN erase; erase { POOF! } END
              END;
          decw:
            IF op2 = locw THEN { locw <n>, decw <p> -> locw <n-p> (, erase) }
              WITH peep[pin2] DO
                BEGIN
                  arg1 := arg1 - peep[pin1].arg1;
                  erase;
                END
            ELSE
            IF op2 = incw THEN
              BEGIN { incw <x>, decw <y> -> incw <x-y> (or decw <y-x>) (, erase) }
                x := arg; y := peep[pin1].arg1;
                IF x > y THEN
                  BEGIN peep[pin2].arg1 := x - y; erase END
                ELSE
                IF x < y THEN
                  WITH peep[pin2] DO
                    BEGIN opcode := decw; arg1 := y - x; erase; tryopt(pin2) END
                ELSE { x = y }
                  BEGIN erase; erase { POOF! } END
              END;
          equ:  
            IF ps > 2 THEN { load, dup, equ 4 -> loc1 (,erase ,erase) }
              IF peep[pin1].arg1 = 4 THEN
                IF op2 = dup THEN
                  BEGIN
                    pin3 := (pin+3) MOD pmax;
                    WITH peep[pin3] DO
                      IF opcode IN load1wordops THEN
                        BEGIN
                          opcode := locw;
                          arg1 := 1; { true }
                          erase; erase; tryopt(pin3)
                        END;
                  END;
          fjp:
            IF op2 = inot THEN { inot, fjp -> tjp (,erase) }
              BEGIN
                peep[pin1].opcode := tjp;
                peep[pin2] := peep[pin1];
                erase;
              END
            ELSE
            IF op2 = locw THEN
              IF arg = 0 { false } THEN
                BEGIN
                  peep[pin1].opcode := bra;
                  peep[pin2] := peep[pin1];
                  erase;
                END
              ELSE
              IF arg = 1 { true } THEN
                BEGIN erase; erase END; { fall through }
          ixa:
            IF (op2 = locw) AND (arg = 0) THEN
              BEGIN erase; erase { POOF! } END
            ELSE
            IF ps > 2 THEN
              BEGIN
                pin3 := (pin+3) MOD pmax;
                op3 := peep[pin3].opcode;
                IF op3 = lag THEN
                  WITH peep[pin3] DO 
                    BEGIN
                      IF op2 = locw THEN
                        BEGIN
                          arg1 := arg1 + peep[pin1].arg1 * arg;
                          erase; erase;
                        END
                      ELSE
                      IF (op2 = locw) AND (arg1 = 1) THEN
                        BEGIN
                          arg1 := arg1 + peep[pin1].arg1;
                          erase; erase;
                        END
                      ELSE
                      IF (op2 IN load1wordops) AND (peep[pin1].arg1 = 1) THEN
                        BEGIN { lag <n>, load1word, ixa 1 -> load1word, incw <n> }
                          x := peep[pin3].arg1; { save }
                          peep[pin3] := peep[pin2]; { move up load }
                          peep[pin2].opcode := incw;
                          peep[pin2].arg1 := x;
                          erase;
                        END
                    END
                ELSE
                IF ps > 3 THEN
                  BEGIN { lag <n>, load1word, inc/dec <p>, ixa <q> -> lag <n+p*q>, }
                    pin4 := (pin+4) MOD pmax; { load1word, ixa <q> (, erase) }
                    WITH peep[pin4] DO 
                      IF (op2 IN [decw,incw]) AND (op3 IN load1wordops) AND
                         (opcode = lag) THEN
                        BEGIN x := arg * peep[pin1].arg1;
                          IF op2 = decw THEN arg1 := arg1 - x
                                        ELSE arg1 := arg1 + x;
                          { move the ixa up, overwriting the inc/dec: }
                          peep[pin2] := peep[pin1];
                          erase; tryopt(pin2);
                        END;
                  END;
              END;
          swp:
            IF ps > 2 THEN
              BEGIN
                pin3 := (pin+3) MOD pmax;
                IF (op2 = lal) OR (op2 = lag) AND
                   (peep[pin3].opcode IN load1wordops) THEN
                  BEGIN { swap peep[pin2] & peep[pin3] }
                    tmp := peep[pin3];
                    peep[pin3] := peep[pin2];
                    peep[pin2] := tmp;
                    erase; { so now we can get rid of swp }
                  END;  
              END;
         {logb, logw, logd,}logm,{lolb, lolw, lold,}lolm,
          loib, loiw, loid, loim,{locw,}
          stgb,{stgw,}stgd, stgm, stlb,{stlw,}stld, stlm,
          stib, stiw, stid, stim,{iand,}ior,  flt2, inot,
         {addi, subi, muli, divi,}negi, absi, sqri, modi,
         {addr, subr,}mulr, divr, negr, absr, sqrr, flt,
          trc,  rnd, {incw, decw,}icgw, iclw, dcgw, dclw,
         {equ,} neq,  leq,  geq,  les,  gtr,  inn, {fjp,}
          equm, neqm, leqm, geqm, lesm, gtrm, sst1, sst2,
          tjp,  ujc,  xjp,  sjp,  cas,  chk0, chk1, chk2,
          lag,  lal,  loca, lpa,  lip, {ixa,} ixca, iodd,
          eofp, dif,  sgs,  rgs,  bra,  ipj,  dmp,  dup,
         {swp,} cal,  cali, csp,  mst,  ent,  ret0, ret1,
          ret2, mov,  copy, fbv,  fvb,  efb,  pack, unpk,
          pop0, cmln, asgn, locr, locs, stop: ;
        END { CASE }
      END; { tryopt }
      
      PROCEDURE adjustpeep;
      BEGIN
        IF NOT deadcode THEN
          BEGIN
            { one of the gens just put an instr into peep[pin], so move pin down }
            IF pin = 0 THEN pin := pmaxm1
                       ELSE pin := pin - 1;
            { check for collision with pout, which would mean peep is full }
            IF pin = pout THEN
              emit; { bump the oldest instruction }
            ps := ps + 1;
            { OK, let's look for some code to improve }
            IF ps > 1 THEN { there are at least instrs in pin+1 & pin+2 (MOD pmax) }
              tryopt((pin+1) MOD pmax);
          END;
      END; { adjustpeep }
      
      PROCEDURE genloca(fkonst: konstp);
      { load_address of string constant, which assembler will put in const pool }
        VAR   i,j: integer;
              lp: strllp;
      BEGIN
        WITH peep[pin], fkonst^ DO
          BEGIN
            opcode := loca;
            arg1 := slgth;
            j := 1; lp := sval;
            FOR i := 1 TO arg1 DO
              BEGIN
                constText[i] := lp^.str[j];
                j := j + 1;
                IF j > varsqt THEN
                  IF lp^.next <> NIL THEN
                    BEGIN lp := lp^.next; j := 1 END;
              END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    write(mn[loca], ' ', arg1:1,' |');
    FOR i := 1 TO arg1 DO
      write(constText[i]);
    writeln('|');  
  END;
{$endif}
          END;
        adjustpeep;
      END;

      PROCEDURE genrcon(fkonst: konstp);
        VAR   len: integer;
      BEGIN
        flushpeep;
        len := lenpv(fkonst^.rval);
        write(p6, ' ', mn[locr], ' ');
        writev(p6, fkonst^.rval, len);
        writeln(p6);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    write(mn[locr], ' ');
    dumpname(fkonst^.rval);
    writeln
  END;
{$endif}
      END;
      
      PROCEDURE gensetcon(fkonst: konstp);
        VAR   k: integer;
      BEGIN
        flushpeep;
        write(p6, ' ', mn[locs], ' ', fkonst^.size:1, ' (');
        FOR k := setlow TO sethigh DO
          IF k IN fkonst^.pval THEN
            write(p6, ' ', k:1);
        writeln(p6,' )');
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    write(mn[locs], ' ', fkonst^.size, ' (');
    FOR k := setlow TO sethigh DO
      IF k IN fkonst^.pval THEN
        write(' ', k:1);
    writeln(' )')
  END;
{$endif}
      END;

      PROCEDURE gen0(fop: opcodes);
      BEGIN { loc0, loc1, chk0, chk1, fbv, fvb, dmp, dup }
            { subi, muli, absi, absr, sqri, sqrr, trc, rnd, ixca, iodd, eofp, efb }
            { flt, flt2, inot, mulr, divi, divr, modi, negi, negr, addi, addr }
            { subr, ujc, xjp, ret0, ret1, ret2, pop0, cmln, asgn, fatl, stop }
        peep[pin].opcode := fop;
        adjustpeep;
{$ifdef __GPC__}
IF trace THEN
  writeln(mn[fop])
{$endif}
      END (*gen0*) ;

      PROCEDURE gen1(fop: opcodes; fp1: integer);
      BEGIN { locw, logb, logw, logd, loib, loiw, loid, stgb, stgw, stgd, lag, }
            { stib, stiw, stid, incw, decw, swp, mst, cali, rgs, sgs, }
            { ior, iand, dif, inn }
            { equ, neq, les, leq, gtr, geq, equm, neqm, leqm, geqm, lesm, gtrm, }
            { sst1, sst2, ixa, mov }
        WITH peep[pin] DO
          BEGIN opcode := fop; arg1 := fp1 END;
        adjustpeep;
{$ifdef __GPC__}
IF trace THEN
  IF fop = csp THEN
    writeln(mn[fop], ' ', sna[fp1])
  ELSE
    writeln(mn[fop], ' ', fp1:1)
{$endif}
      END (*gen1*) ;

      PROCEDURE gen2(fop: opcodes; fp1,fp2: integer);
      BEGIN { lolb, lolw, lold, logm, loim, stgm, stlb, stlw, stld, lal, chk2 }
            { pack, unpk, lip, iclw, dclw }
        WITH peep[pin] DO
          BEGIN opcode := fop; arg1 := fp1; arg2 := fp2 END;
        adjustpeep;
{$ifdef __GPC__}
IF trace THEN
  writeln(mn[fop], ' ', fp1:1, ' ', fp2:1)
{$endif}
      END (*gen2*) ;

      PROCEDURE gen3(fop: opcodes; fp1,fp2,fp3: integer);
      BEGIN { lolm. stlm }
        WITH peep[pin] DO
          BEGIN opcode := fop; arg1 := fp1; arg2 := fp2; arg3 := fp3 END;
        adjustpeep;
{$ifdef __GPC__}
IF trace THEN
  writeln(mn[fop], ' ', fp1:1, ' ', fp2:1, ' ', fp3:1)
{$endif}
      END; (*gen3*)

      PROCEDURE gen1lab(fop: opcodes; flab: integer);
      BEGIN { bra, fjp, sjp, tjp, ent }
        WITH peep[pin] DO
          BEGIN opcode := fop; arg1 := flab END;
        adjustpeep;
        IF fop = bra THEN
          deadcode := true; { until the next putlabel }
{$ifdef __GPC__}
IF trace THEN
  writeln(mn[fop], ' L', flab:1)
{$endif}
      END (* gen1lab *);

      PROCEDURE gen2lab(fop: opcodes; fp1,flab: integer);
      BEGIN { lpa, cal }
        WITH peep[pin] DO
          BEGIN opcode := fop; arg1 := fp1; arg2 := flab END;
        adjustpeep;
{$ifdef __GPC__}
IF trace THEN
  writeln(mn[fop],' ',fp1:1,' L',flab:1)
{$endif}
      END; (* gen2lab *)

      PROCEDURE gen3lab(fop: opcodes; fp1,fp2,flab: integer);
      BEGIN { 'cas' }
        WITH peep[pin] DO
          BEGIN opcode := fop; arg1 := fp1; arg2 := fp2; arg3 := flab END;
        adjustpeep;
{$ifdef __GPC__}
IF trace THEN
  writeln(mn[fop],' ',fp1:1,' ',fp2:1,' L',flab:1)
{$endif}
      END (* gen3lab *);

      PROCEDURE putlabel(labname: integer; comment: quantum);
      BEGIN
        deadcode := false;
        IF ps > 0 THEN
          WITH peep[(pin+1) MOD pmax] DO
            IF (opcode = bra) AND (arg1 = labname) THEN
              erase;
        flushpeep; { target of branch instr; get any laggards out now }
        writeln(p6, 'L', labname:1, ' ', comment);
{$ifdef __GPC__}
IF trace THEN
  writeln('L', labname:1,' ',comment)
{$endif}
      END (*putlabel*);

      PROCEDURE putlabelFixup(labname, newvalue: integer);
      BEGIN { doesn't matter where this goes, doesn't affect sequences of instrs }
        writeln(p6, 'L', labname:1, '=', newvalue:1);
{$ifdef __GPC__}
IF trace THEN
  writeln('L', labname:1,'=',newvalue:1)
{$endif}
      END;
      
      FUNCTION rightSize(fsp: tIptr): integer;
      BEGIN
        IF fsp = intptr THEN
          rightSize := 1
        ELSE
        IF (fsp = boolptr) OR (fsp = charptr) THEN
          rightSize := 0
        ELSE
        IF fsp <> NIL THEN
          WITH fsp^ DO
            IF form = scalar THEN
              IF scalkind = expdclrd THEN
                rightSize := 1
              ELSE { only other scalar is real }
                rightSize := 2
            ELSE
            IF form >= powerset THEN
              rightSize := 3 { use opcode with an 'm' suffix }
            ELSE
            IF form = subrange THEN
              rightSize := rightSize(rangetype)
            ELSE
            IF form = pointer THEN
              rightSize := 1;
      END;

      FUNCTION rightOp(fop: opcodes; delt: integer): opcodes;
      BEGIN
        WHILE delt > 0 DO
          BEGIN delt := delt - 1; fop := succ(fop) END;
        rightOp := fop;
      END;

      PROCEDURE load(VAR fattr: attr);
        VAR   rsize: integer;
      BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>load');
  END;
{$endif}
        WITH fattr DO
          IF typtr <> NIL THEN
            BEGIN
              IF typtr^.form = subrange THEN
                typtr := typtr^.rangetype;
              IF kind = cst THEN
                BEGIN
                  IF typtr^.form = scalar THEN
                    IF typtr = realptr THEN
                      genrcon(cval.valp)
                    ELSE
                      gen1(locw, cval.ival)
                  ELSE
                  IF typtr = nilptr THEN
                    gen1(locw, nilval)
                  ELSE
                    gensetcon(cval.valp);
                END
              ELSE
              IF kind = varbl THEN
                BEGIN
                  rsize := rightSize(typtr);
                  IF access = drct THEN
                    IF vlevel <= 1 THEN { global }
                      IF rsize = 3 THEN
                        gen2(logm, typtr^.size, dplmt)
                      ELSE  
                        gen1(rightOp(logb, rsize), dplmt)
                    ELSE              { local & sub-local }
                      IF rsize = 3 THEN
                        gen3(lolm, typtr^.size, level-vlevel, dplmt)
                      ELSE  
                        gen2(rightOp(lolb, rsize), level-vlevel, dplmt)
                  ELSE
                  {IF access = indrct THEN}
                    IF rsize = 3 THEN
                      gen2(loim, typtr^.size, idplmt)
                    ELSE
                      gen1(rightOp(loib, rsize), idplmt);
                END;
              kind := expr
            END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<load');
    indent := indent - 1;
  END;
{$endif}
      END (*load*) ;

      PROCEDURE store(VAR fattr: attr);
        VAR   rsize: integer;
      BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>store');
  END;
{$endif}
        WITH fattr DO
          IF typtr <> NIL THEN
            BEGIN
              rsize := rightSize(typtr);
              IF access = drct THEN
                IF vlevel <= 1 THEN
                  IF rsize = 3 THEN { set }
                    gen2(stgm, typtr^.size, dplmt)
                  ELSE
                    gen1(rightOp(stgb, rsize), dplmt)
                ELSE
                  IF rsize = 3 THEN
                    gen3(stlm, typtr^.size, level-vlevel, dplmt)
                  ELSE  
                    gen2(rightOp(stlb, rsize), level-vlevel, dplmt)
              ELSE
              {IF access = indrct THEN}
                IF rsize = 3 THEN
                  gen2(stim, typtr^.size, idplmt)
                ELSE
                  gen1(rightOp(stib, rsize), idplmt)
            END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<store');
    indent := indent - 1;
  END;
{$endif}
      END (*store*) ;

      PROCEDURE loadaddress(applyOffset: Boolean);
      BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>loadaddress');
  END;
{$endif}
        WITH gattr DO
          IF typtr <> NIL THEN
            IF kind = cst THEN
              IF string(typtr) THEN
                BEGIN
                  genloca(cval.valp);
                  kind := varbl; access := indrct; idplmt := 0;
                  formantBound := false; loopvar := false;
                END
              ELSE
                error(100) { internal compiler error }
            ELSE
            IF kind = varbl THEN
              IF access = drct THEN
                BEGIN
                  IF vlevel <= 1 THEN
                    gen1(lag, dplmt)
                  ELSE
                    gen2(lal, level - vlevel, dplmt);
                  access := indrct;
                  idplmt := 0;
                END
              ELSE  { access = indrct }
                IF (idplmt <> 0) AND applyOffset THEN
                  BEGIN
                    gen1(incw, idplmt);
                    idplmt := 0;
                  END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<loadaddress');
    indent := indent - 1;
  END;
{$endif}
      END (*loadaddress*) ;

      PROCEDURE chkbounds(fsp: tIptr; ferrno: integer; VAR fattr: attr);
        VAR   kmin,kmax,lmin,lmax: integer;
      BEGIN
        IF fsp <> NIL THEN
          IF ((fsp^.form = scalar) AND
              (fsp^.scalkind = expdclrd) AND
              (fsp <> boolptr)) OR       { enumeration or }
             (fsp^.form = subrange) THEN { subrange }
            BEGIN
              getbounds(fsp, lmin, lmax);
              IF fattr.kind = cst THEN
                IF (fattr.cval.ival < lmin) OR
                   (fattr.cval.ival > lmax) THEN
                  error(ferrno)
                ELSE { no need to check at runtime }
              ELSE
              IF (fattr.kind = varbl) AND (fattr.typtr^.form = subrange) THEN
                BEGIN
                  getbounds(fattr.typtr, kmin, kmax);
                  IF (kmin > lmax) OR (kmax < lmin) THEN
                    error(99)
                END
              ELSE
                IF debug THEN
                  gen2(chk2, lmin, lmax)
            END
      END (*chkbounds*);

      FUNCTION isConfArrayParam(fpar: sTptr; VAR fparTyp,finxTyp: tIptr): Boolean;
        { called by callUserRoutine & body }
      BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>isCAP');
  END;
{$endif}
        isConfArrayParam := false;
        IF fpar <> NIL THEN
          BEGIN
            fparTyp := fpar^.idtype;
            IF fparTyp <> NIL THEN
              IF fparTyp^.form = arrays THEN
                BEGIN
                  finxTyp := fparTyp^.inxtype;
                  IF finxTyp <> NIL THEN
                    IF finxTyp^.form = confInx THEN
                      isConfArrayParam := true;
                END;
          END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<isCAP');
    indent := indent - 1;
  END;
{$endif}
      END; { isConfArrayParam }
            
      PROCEDURE statSeq(fsys: setofsys);
        
      PROCEDURE statement(fsys: setofsys); { called from statSeq }
        VAR   lcp: sTptr;
              llp: lbp;

        PROCEDURE call(fsys: setofsys; fcp: sTptr); FORWARD;
        
        PROCEDURE expression(fsys: setofsys); FORWARD;
        
        PROCEDURE selector(fsys: setofsys; fcp: sTptr);
          LABEL 1;
          VAR   lattr,savAttr: attr;
                lcp: sTptr;
                lmin,lmax: integer;
        BEGIN (* selector *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>selector');
  END;
{$endif}
          IF (fcp = uvarptr) AND (sy = lparen) THEN { unidentified func? }
            fcp := ufctptr;
          WITH fcp^, gattr DO
            BEGIN
              typtr := idtype;
              kind := varbl;
              formantBound := false;
              loopvar := false;
              varsel := false;
              packedcompon := false;
              CASE klass OF
                vars:
                  BEGIN
                    formantBound := vbound;
                    loopvar := readonly;
                    IF vkind = actual THEN
                      BEGIN
                        access := drct;
                        vlevel := vlev;
                        dplmt := addr
                      END
                    ELSE
                      BEGIN
                        gen2(lolw, level-vlev, addr);
                        access := indrct;
                        idplmt := 0
                      END;
                  END;  
                field:
                  WITH display[statLevFnd] DO
                    IF occur = crec THEN
                      BEGIN
                        access := drct;
                        vlevel := clev;
                        dplmt := cdspl + addr
                      END
                    ELSE { how do we know it's a vrec? }
                      BEGIN
                        IF level = 1 THEN gen1(logw, vdspl)
                                     ELSE gen2(lolw, 0, vdspl);
                        access := indrct;
                        idplmt := addr + offset
                      END;
                func:
                  IF sy = lparen THEN
                    call(fsys, fcp); { function call }
              END (*CASE*)
            END (*WITH*);
          IF NOT (sy IN selectsys + fsys) THEN
            BEGIN error(59); skip(selectsys + fsys) END;
          WHILE sy IN selectsys DO
            BEGIN
        (*[*) IF sy = lbrack THEN
                BEGIN
                  REPEAT
                    lattr := gattr; { lattr is now the array }
                    WITH lattr DO   { gattr will be the subscript }
                      IF typtr <> NIL THEN
                        IF typtr^.form <> arrays THEN
                          BEGIN error(138); typtr := NIL END;
                    loadaddress(true); { add in any offset }
                    insymbol; { eat the lbrack }
                    expression(fsys + [comma,rbrack]);
                    savAttr := gattr; { to preserve possible subrange info }
                    load(gattr);      { which load would tend to destroy }
                    IF gattr.typtr = NIL THEN GOTO 1; { previous error }
                    IF gattr.typtr^.form <> scalar THEN { subscript }
                      error(113);
                    IF lattr.typtr = NIL THEN GOTO 1; { likewise }
                    WITH lattr.typtr^ DO { the array }
                      BEGIN { inxtype/aeltype refer to the array (lattr) }
                        IF (inxtype = NIL) OR (aeltype = NIL) THEN GOTO 1;
                        IF inxtype^.form = confinx THEN { c.a.p. }
                          IF comptypes(inxtype^.TidOrd, gattr.typtr) THEN
                            BEGIN
                              WITH inxtype^ DO
                                gen2(lal, level-lobnd^.vlev, lobnd^.addr);
                              { this loads a ptr to the array descriptor }
                              gen0(ixca); { ixca subtracts the lobnd }
                            END           { then multiplies in the stride }
                          ELSE            { and then increments the array addr }
                            error(139)
                        ELSE { conventional array }
                          IF comptypes(inxtype, gattr.typtr) THEN
                            BEGIN
                              chkbounds(inxtype, 193, savAttr);
                              getbounds(inxtype, lmin, lmax);
                              IF lmin > 0 THEN
                                gen1(decw, lmin)
                              ELSE
                              IF lmin < 0 THEN
                                gen1(incw, -lmin);
                              gen1(ixa, aeltype^.size)
                            END
                          ELSE
                            error(139);
                        WITH gattr DO { make gattr the subscripted variable }
                          BEGIN
                            typtr := {lattr.typtr^} aeltype;
                            kind := varbl;
                            formantBound := false; loopvar := false;
                            packedcompon := { lattr.typtr^ } packing;
                            access := indrct;
                            idplmt := 0
                          END;
                      END;
1:                  IF sy = range THEN { to handle un-implemented extension }
                      BEGIN
                        error(37);
                        insymbol;
                        {savAttr := gattr;}
                        expression(fsys+[comma,rbrack]);
                        gattr := lattr; { restore array stuff }
                      END;
                  UNTIL sy <> comma;
                  IF sy = rbrack THEN insymbol ELSE error(12)
                END (*if sy = lbrack*)
              ELSE
        (*.*) IF sy = period THEN
                WITH gattr DO
                  BEGIN
                    IF typtr <> NIL THEN
                      IF typtr^.form <> records THEN
                        BEGIN error(140); typtr := NIL END;
                    insymbol;
                    IF sy = ident THEN
                      BEGIN
                        IF typtr <> NIL THEN
                          BEGIN
                            searchOneLevel(typtr^.fstfld, lcp);
                            IF lcp = NIL THEN
                              BEGIN error(152); typtr := NIL END
                            ELSE
                              WITH lcp^ DO
                                BEGIN
                                  varsel := (klass = field) AND{_THEN} selectr;
                                  typtr := idtype;
                                  IF access = drct THEN
                                    dplmt := dplmt + addr
                                  ELSE { access = indrct }
                                    idplmt := idplmt + addr;
                                END
                          END;
                        insymbol
                      END (*sy = ident*)
                    ELSE error(2)
                  END (*WITH gattr*)
              ELSE
        (*^*)   BEGIN  { sy = arrow }
                  IF gattr.typtr <> NIL THEN
                    IF gattr.typtr^.form = pointer THEN
                      BEGIN
                        load(gattr);
                        IF debug THEN gen0(chk1);
                        WITH gattr DO
                          BEGIN
                            typtr := typtr^.eltype;
                            kind := varbl;
                            formantBound := false; loopvar := false;
                            access := indrct;
                            idplmt := 0
                          END
                      END
                    ELSE
                    IF gattr.typtr^.form = files THEN
                      BEGIN
                        loadaddress(true); { add in any offset }
                        { generate buffer validate for file }
                        IF gattr.typtr = textptr THEN
                          gen0(fbv)
                        ELSE
                          BEGIN
                            gen1(locw, gattr.typtr^.compType^.size);
                            gen0(fvb)
                          END;
                        { index buffer }
                        gen1(incw, fileidsize);
                        gattr.typtr := gattr.typtr^.compType;
                      END
                    ELSE
                      error(141);
                  insymbol
                END;
            END; (*WHILE*)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<selector');
    indent := indent - 1;
  END;
{$endif}
        END (*selector*) ;

        PROCEDURE compatSize(VAR attr1,attr2: attr);
          VAR   wordSize1,wordSize2,dif: integer;
        BEGIN { assert: attr1.typtr^.form = powerset }
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>compatSize');
  END;
{$endif}
          { first see if adjusting constant sizes works: }
          IF attr1.kind = cst THEN
            IF attr1.typtr^.size < attr2.typtr^.size THEN
              BEGIN
                attr1.typtr^.size := attr2.typtr^.size;
                attr1.cval.valp^.size := attr1.typtr^.size;
{$ifdef __GPC__}
IF trace THEN
  writeln(' ':indent,' compatSize lhs size changed to that of rhs ',attr2.typtr^.size:1);
{$endif}

              END
            ELSE
          ELSE
          IF attr2.kind = cst THEN
            IF attr2.typtr^.size < attr1.typtr^.size THEN
              BEGIN
                attr2.typtr^.size := attr1.typtr^.size;
                attr2.cval.valp^.size := attr2.typtr^.size;
{$ifdef __GPC__}
IF trace THEN
  writeln(' ':indent,' compatSize rhs size changed to that of lhs: ',attr2.typtr^.size:1);
{$endif}
              END;
              
          { do more analysis to see if stack adjustment is needed }
          wordSize1 := (attr1.typtr^.size + (stackelsize - 1)) DIV stackelsize;
          wordSize2 := (attr2.typtr^.size + (stackelsize - 1)) DIV stackelsize;
          dif := wordSize1 - wordSize2;
          IF dif <> 0 THEN { yeah, it's needed }
            IF attr1.kind = expr THEN { called from (somewhere in) expression }
              IF attr2.kind = expr THEN { both operands on stack }
                IF dif > 0 THEN { pad attr2 with zero words }
                  REPEAT gen1(locw, 0);
                    dif := dif - 1
                  UNTIL dif = 0
                ELSE { dif < 0: need to pad attr1 (which is underneath att2) }
                  REPEAT gen1(locw, 0); gen1(swp, wordSize2*stackelsize);
                    dif := dif + 1
                  UNTIL dif = 0
              ELSE { attr2.kind must be a varbl that hasn't been loaded }
                IF dif > 0 THEN { pad att2 with zero words }
                  BEGIN
                    load(attr2); { but first get him on the TOS }
                    REPEAT gen1(locw, 0); dif := dif - 1 UNTIL dif = 0;
                    attr2.typtr^.size := attr1.typtr^.size;
                  END
                ELSE { dif < 0: need to pad attr1 B4 attr2 get loaded }
                  BEGIN
                    REPEAT gen1(locw, 0); dif := dif + 1 UNTIL dif = 0;
                    load(attr2);
                    attr1.typtr^.size := attr2.typtr^.size;
                  END
            ELSE { called from assignment or callUserRoutine }
              BEGIN { attr2 must be loaded and padded, or loaded and trimmed }
                IF attr2.kind <> expr THEN
                  load(attr2);
                IF dif > 0 THEN { pad att2 with zero words }
                  REPEAT gen1(locw, 0); dif := dif - 1 UNTIL dif = 0
                ELSE { dif < 0: r.h.s. must be trimmed to keep stack aligned }
                  REPEAT gen1(pop0, 0); dif := dif + 1 UNTIL dif = 0;
                { the l.h.s. determines how much is stored }
                attr2.typtr^.size := attr1.typtr^.size;
              END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<compatSize');
    indent := indent - 1;
  END;
{$endif}
        END;
        
        PROCEDURE call(*fsys: setofsys; fcp: sTptr*);
          VAR   lkey: 1..18;

          PROCEDURE variable(fsys: setofsys);
            VAR   lcp: sTptr;
          BEGIN
            IF sy = ident THEN
              BEGIN
                searchid([vars,field,func], lcp, true);
                insymbol
              END
            ELSE
              BEGIN error(2); lcp := uvarptr END;
            selector(fsys, lcp)
          END (*variable*) ;

          PROCEDURE getputresetrewriteprocedure;
          BEGIN
            variable(fsys + [rparen]);
            loadaddress(true); { add in any offset }
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr^.form <> files THEN error(116);
            IF lkey <= 2 THEN
              BEGIN
                IF gattr.typtr = textptr THEN
                  gen1(csp, lkey(*get,put*))
                ELSE
                  BEGIN
                    IF gattr.typtr <> NIL THEN
                      gen1(locw, gattr.typtr^.compType^.size);
                    IF lkey = 1 THEN gen1(csp, 38(*gbf*))
                                ELSE gen1(csp, 39(*pbf*))
                  END
              END
            ELSE
              IF gattr.typtr = textptr THEN
                BEGIN
                  IF lkey = 3 THEN gen1(csp, 25(*reset*))
                              ELSE gen1(csp, 26(*rewrite*))
                END
              ELSE
                BEGIN
                  IF lkey = 3 THEN gen1(csp, 36(*reset*))
                              ELSE gen1(csp, 37(*rewrite*))
                END
          END (*getputresetrewrite*) ;

          PROCEDURE dummyup(VAR stdtext: sTptr);
          BEGIN
            getid(stdtext);
            WITH stdtext^ DO
              BEGIN
                getstr(name);
                klass := vars; vbound := false; vkind := actual; vlev := 1;
                IF stdtext = outputptr THEN
                  BEGIN
                    addr := lcaftermarkstack + 6;
                    name^.str := 'output$     '
                  END
                ELSE
                  BEGIN
                    addr := lcaftermarkstack + 4;
                    name^.str := 'input$      '
                  END
              END;
            enterId(stdtext);
          END;
          
          PROCEDURE pageprocedure;
          BEGIN
            IF sy = lparen THEN
              BEGIN
                insymbol;
                variable(fsys + [rparen]);
                loadaddress(true); { add in any offset }
                IF gattr.typtr <> NIL THEN
                  IF gattr.typtr <> textptr THEN
                    error(169);
                IF sy = rparen THEN insymbol ELSE error(4)
              END
            ELSE
              BEGIN
                IF outputptr = NIL THEN
                  BEGIN error(176); dummyup(outputptr) END;
                gen2(lal, level-outputptr^.vlev, outputptr^.addr);
              END;
            gen1(csp, 24(*page*))
          END (*page*) ;

          PROCEDURE readprocedure;
            VAR lsp : tIptr;
                txt: Boolean; { = "is a text file" }
                need2loadDefaultFile: Boolean; { default file not yet loaded }
                arglist,commaSeen: Boolean;
          BEGIN
            txt := true; need2loadDefaultFile := true;
            IF sy = lparen THEN
              BEGIN
                insymbol;
                variable(fsys + [comma,rparen]);
                lsp := gattr.typtr;
                arglist := true; { if comma seen after a file name }
                IF lsp <> NIL THEN
                  IF lsp^.form = files THEN
                    BEGIN
                      txt := lsp = textptr;
                      IF NOT txt AND (lkey = 11 { readln } ) THEN
                        error(121);
                      loadaddress(true); { add in any offset }
                      need2loadDefaultFile := false;
                      IF sy = rparen THEN
                        BEGIN
                          IF lkey = 5 { read } THEN error(65);
                          arglist := false { just doing the readln }
                        END
                      ELSE
                      IF sy = comma THEN
                        BEGIN
                          insymbol;
                          variable(fsys + [comma,rparen])
                        END
                      ELSE { whatever sy is, it's wrong }
                        BEGIN
                          error(20);
                          skip(fsys + [comma,rparen]);
                          arglist := sy <> rparen
                        END;
                    END
                  ELSE
                    IF inputptr = NIL THEN
                      BEGIN error(175); dummyup(inputptr) END;
                IF arglist THEN
                  REPEAT
                    loadaddress(true); { add in any offset }
                    IF need2loadDefaultFile THEN
                      BEGIN
                        { file was not loaded, we load and swap }
                        { so that it ends up on the bottom.}
                        {gen2(lal, level-inputptr^.vlev, inputptr^.addr);}
                        IF inputptr = NIL THEN
                          BEGIN error(175); dummyup(inputptr) END;
                        gen1(lag, inputptr^.addr);
                        gen1(swp, ptrsize); { note 2nd is always pointer }
                        need2loadDefaultFile := false
                      END;
                    IF txt THEN
                      BEGIN
                        IF gattr.formantBound THEN
                          error(197); { conformant array bound can't be assigned }
                        IF gattr.loopvar THEN
                          error(187); { control variable of FOR can't be assigned }
                        IF gattr.typtr <> NIL THEN
                          IF gattr.typtr^.form <= subrange THEN
                            IF comptypes(intptr,gattr.typtr) THEN
                              gen1(csp, 3(*rdi*))
                            ELSE
                            IF comptypes(realptr,gattr.typtr) THEN
                              gen1(csp, 4(*rdr*))
                            ELSE
                            IF comptypes(charptr,gattr.typtr) THEN
                              gen1(csp, 5(*rdc*))
                            ELSE
                              error(93) { illegal read argument }
                          ELSE
                          IF string(gattr.typtr) THEN
                            BEGIN
                              gen1(locw, gattr.typtr^.size);
                              gen1(csp, 7(*rds*))
                            END
                          ELSE
                            error(93);
                      END
                    ELSE
                      BEGIN { binary file }
                        IF NOT comptypes(gattr.typtr, lsp^.compType) THEN
                          error(72);
                        IF lsp^.compType <> NIL THEN
                          gen1(locw, lsp^.compType^.size);
                        gen1(csp, 35(*rbf*))
                      END;
                    commaSeen := sy = comma;
                    IF commaSeen THEN
                      BEGIN
                        insymbol;
                        variable(fsys + [comma,rparen])
                      END
                  UNTIL NOT commaSeen;
                IF sy = rparen THEN insymbol ELSE error(18)
              END
            ELSE
              BEGIN
                IF inputptr = NIL THEN
                  BEGIN error(175); dummyup(inputptr) END;
                IF lkey = 5 THEN
                  error(65);
                gen1(lag, inputptr^.addr)
              END;
            IF lkey = 11 THEN
              gen1(csp, 21(*rln*)) { this removes the file pointer from stack }
            ELSE
              gen0(dmp);
          END; { readprocedure }

          PROCEDURE writeprocedure;
            VAR   lsp,lsp1: tIptr;
                  default, default1: Boolean;
                  llkey: 1..15;
                  len: addrrange;
                  txt: Boolean; { = "is a text file" }
                  need2loadDefaultFile: Boolean; { default file not yet loaded }
                  hasArgList,commaSeen: Boolean;
                  
            PROCEDURE handleFieldExpr;
              VAR   savAttr: attr;
            BEGIN
              insymbol;
              expression(fsys + [comma,colon,rparen]);
              savAttr := gattr;
              load(gattr);
              IF savAttr.typtr <> NIL THEN
                IF savAttr.typtr <> intptr THEN error(118);
              IF savAttr.kind = cst THEN
                IF savAttr.cval.ival <= 0 THEN error(118) ELSE
              ELSE
                IF debug THEN { generate run-time check }
                  gen2(chk2, 1, maxint); { error if zero or neg }
            END;
            
          BEGIN
            llkey := lkey; txt := true; need2loadDefaultFile := true;
            IF sy = lparen THEN
              BEGIN
                insymbol;
                expression(fsys + [comma,colon,rparen]);
                lsp := gattr.typtr; lsp1 := NIL;
                hasArgList := true;
                IF lsp <> NIL THEN
                  IF lsp^.form = files THEN
                    WITH gattr, lsp^ DO
                      BEGIN
                        lsp1 := lsp;
                        txt := lsp = textptr;
                        IF NOT txt AND (lkey = 12 { writeln } ) THEN
                          error(121);
                        loadaddress(true); { add in any offset }
                        need2loadDefaultFile := false;
                        IF sy = rparen THEN
                          BEGIN
                            IF llkey = 6 { write } THEN error(65);
                            hasArgList := false
                          END
                        ELSE
                        IF sy <> comma THEN
                          BEGIN error(20); skip(fsys+[comma,rparen]) END;
                        IF sy = comma THEN
                          BEGIN
                            insymbol;
                            expression(fsys+[comma,colon,rparen])
                          END
                        ELSE
                          hasArgList := false
                      END
                  ELSE { lsp^.form <> files, so 'output' not mentioned }
                    IF outputptr = NIL THEN { and may not be assumed }
                      BEGIN error(176); dummyup(outputptr) END;
                IF hasArgList THEN
                  REPEAT
                    IF gattr.typtr <> NIL THEN
                      IF gattr.typtr^.form <= subrange THEN
                        load(gattr)
                      ELSE
                        loadaddress(true); { add in any offset }
                    lsp := gattr.typtr;
                    IF need2loadDefaultFile THEN
                      BEGIN
                      { file was not loaded, we load and swap so that it ends up
                        on the bottom.}
                        IF outputptr = NIL THEN
                          BEGIN error(176); dummyup(outputptr) END;
                        gen1(lag, outputptr^.addr);
                        IF lsp <> NIL THEN
                          IF lsp^.form <= subrange THEN
                            IF lsp^.size < stackelsize THEN
                              gen1(swp, stackelsize) { char or Boolean }
                            ELSE
                              gen1(swp, lsp^.size)
                          ELSE
                            gen1(swp, ptrsize); { ptr to structure }
                        need2loadDefaultFile := false
                      END;
                    IF txt THEN
                      BEGIN
                        IF sy = colon THEN
                          BEGIN
                            handleFieldExpr;
                            default := false
                          END  
                        ELSE
                          default := true;
                        { so much fun, let's do that again }
                        IF sy = colon THEN
                          BEGIN
                            handleFieldExpr;
                            default1 := false
                          END
                        ELSE
                          default1 := true;
                        IF lsp = intptr THEN
                          BEGIN
                            IF default THEN
                              gen1(locw, defaultIntField);
                            gen1(csp, 6(*wri*))
                          END
                        ELSE
                        IF lsp = realptr THEN
                          BEGIN
                            IF default THEN
                              gen1(locw, defaultRealField);
                            IF default1 THEN
                              gen1(csp, 8(*wrr*))
                            ELSE
                              gen1(csp, 28(*wrf*))
                          END
                        ELSE
                        IF lsp = charptr THEN
                          BEGIN
                            IF default THEN
                              gen1(locw, defaultCharField);
                            gen1(csp, 9(*wrc*))
                          END
                        ELSE
                        IF lsp = boolptr THEN
                          BEGIN
                            IF default THEN
                              gen1(locw, defaultBoolField);
                            gen1(csp, 27(*wrb*))
                          END
                        ELSE
                        IF lsp <> NIL THEN
                          BEGIN
                            IF lsp^.form = scalar THEN
                              error(94)
                            ELSE
                            IF string(lsp) THEN
                              BEGIN
                                len := lsp^.size;
                                gen1(locw, len);
                                IF default THEN
                                  gen0(dup);
                                gen1(csp, 10(*wrs*))
                              END
                            ELSE
                              error(94)
                          END
                      END
                    ELSE { NOT txt }
                      BEGIN { binary file }
                        IF NOT comptypes(lsp1^.compType, lsp) THEN
                          error(72);
                        IF lsp = intptr THEN
                          gen1(csp, 31(*wbi*))
                        ELSE
                        IF lsp = realptr THEN
                          gen1(csp, 32(*wbr*))
                        ELSE
                        IF lsp = charptr THEN
                          gen1(csp, 33(*wbc*))
                        ELSE
                        IF lsp = boolptr THEN
                          gen1(csp, 34(*wbb*))
                        ELSE
                        IF lsp <> NIL THEN
                          IF lsp^.form <= subrange THEN
                            gen1(csp, 31(*wbi*))
                          ELSE
                            BEGIN
                              IF lsp1^.compType <> NIL THEN
                                gen1(locw, lsp1^.compType^.size);
                              gen1(csp, 30(*wbf*))
                            END
                      END;
                    commaSeen := sy = comma;
                    IF commaSeen THEN
                      BEGIN
                        insymbol;
                        expression(fsys + [comma,colon,rparen])
                      END
                  UNTIL NOT commaSeen;
                IF sy = rparen THEN insymbol ELSE error(18)
              END
            ELSE { sy <> lparen }
              BEGIN
                IF outputptr = NIL THEN
                  BEGIN error(176); dummyup(outputptr) END;
                IF lkey = 6 THEN
                  error(65);
                gen1(lag, outputptr^.addr)
              END;
            IF llkey = 12 THEN (*writeln*)
              gen1(csp, 22(*wln*)) { this removes the file pointer from stack }
            ELSE
              gen0(dmp);
          END; { writeprocedure }

          PROCEDURE packprocedure;
            VAR lsp,lsp1: tIptr; lb, bs: integer; lattr: attr;
          BEGIN
            variable(fsys + [comma,rparen]);
            loadaddress(true); { add in any offset }
            lsp := NIL; lsp1 := NIL; lb := 1; bs := 1;
            lattr := gattr;
            IF gattr.typtr <> NIL THEN
              WITH gattr.typtr^ DO
                IF form = arrays THEN
                  BEGIN
                    lsp := inxtype;
                    IF lsp <> NIL THEN
                      IF (lsp = charptr) OR (lsp = boolptr) THEN
                        lb := 0
                      ELSE
                      IF lsp^.form = subrange THEN
                        lb := lsp^.min.ival;
                    lsp1 := aeltype;
                    IF lsp1 <> NIL THEN
                      bs := lsp1^.size
                  END
                ELSE
                  error(112);
            IF sy = comma THEN insymbol ELSE error(20);
            expression(fsys + [comma,rparen]);
            load(gattr);
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr^.form <> scalar THEN
                error(113)
              ELSE
              IF NOT comptypes(lsp,gattr.typtr) THEN
                error(109);
            IF lb <> 0 THEN
              BEGIN
                gen1(locw, lb);
                gen0(subi);
              END;
            gen1(locw, bs);
            gen0(muli);
            IF sy = comma THEN insymbol ELSE error(20);
            variable(fsys + [rparen]);
            loadaddress(true); { add in any offset }
            IF gattr.typtr <> NIL THEN
              WITH gattr.typtr^ DO
                IF form = arrays THEN
                  BEGIN
                    IF aeltype <> NIL THEN
                      IF NOT comptypes(aeltype,lsp1) THEN error(109)
                  END
                ELSE
                  error(112);
            IF (gattr.typtr <> NIL) AND (lattr.typtr <> NIL) THEN
              gen2(pack, gattr.typtr^.size, lattr.typtr^.size)
          END (*pack*) ;

          PROCEDURE unpackprocedure;
            VAR lsp,lsp1: tIptr; lattr,lattr1: attr; lb, bs: integer;
          BEGIN
            variable(fsys + [comma,rparen]);
            loadaddress(true); { add in any offset }
            lsp := NIL; lsp1 := NIL; lb := 1; bs := 1;
            lattr := gattr;
            IF gattr.typtr <> NIL THEN
              WITH gattr.typtr^ DO
                IF form = arrays THEN
                  lsp1 := aeltype
                ELSE
                  error(112);
            IF sy = comma THEN insymbol ELSE error(20);
            variable(fsys + [comma,rparen]);
            loadaddress(true); { add in any offset }
            lattr1 := gattr;
            IF gattr.typtr <> NIL THEN
              WITH gattr.typtr^ DO
                IF form = arrays THEN
                  BEGIN
                    IF aeltype <> lsp1 THEN
                      error(109);
                    lsp := inxtype;
                    IF lsp <> NIL THEN
                      IF lsp = charptr THEN
                        lb := 0
                      ELSE
                      IF lsp^.form = subrange THEN
                        lb := lsp^.min.ival;
                    IF lsp1 <> NIL THEN
                      bs := lsp1^.size;
                  END
                ELSE
                  error(112);
            IF sy = comma THEN insymbol ELSE error(20);
            expression(fsys + [rparen]);
            IF gattr.kind = cst THEN
              BEGIN
                bs := (gattr.cval.ival - lb)*bs;
                IF bs < 0 THEN
                  error(97)
                ELSE
                  gen1(locw, bs);
              END
            ELSE
              BEGIN
                load(gattr);
                IF gattr.typtr <> NIL THEN
                  IF gattr.typtr^.form <> scalar THEN error(113)
                  ELSE
                    IF NOT comptypes(lsp, gattr.typtr) THEN error(109);
                IF lb <> 0 THEN
                  BEGIN
                    gen1(locw, lb);
                    gen0(subi);
                  END;
                gen1(locw, bs);
                gen0(muli);
              END;
            IF (gattr.typtr <> NIL) AND (lattr.typtr <> NIL) THEN
              gen2(unpk, lattr.typtr^.size, lattr1.typtr^.size)
          END (*unpack*) ;

          PROCEDURE newdisposeprocedure;
            LABEL 3;
            VAR   lsp,lsp1: tIptr;
                  lsize: addrrange;
                  lval: valu;
          BEGIN
            variable(fsys + [comma,rparen]);
            loadaddress(true); { add in any offset }
            lsp := NIL;
            lsize := 0;
            IF gattr.typtr <> NIL THEN
              WITH gattr.typtr^ DO
                IF form = pointer THEN
                  BEGIN
                    IF eltype <> NIL THEN
                      BEGIN
                        lsize := eltype^.size;
                        IF eltype^.form = records THEN
                          lsp := eltype^.recvar
                      END
                  END
                ELSE
                  error(155);
            WHILE sy = comma DO
              BEGIN
                insymbol;
                constant(fsys + [comma,rparen], lsp1, lval);
                IF lsp = NIL THEN
                  error(158)
                ELSE
                  IF lsp^.form <> tagfld THEN
                    error(162)
                  ELSE
                    IF lsp^.tagfieldp <> NIL THEN
                      IF string(lsp1) OR (lsp1 = realptr) THEN
                        error(159)
                      ELSE
                        IF comptypes(lsp^.tagfieldp^.idtype, lsp1) THEN
                          BEGIN
                            lsp1 := lsp^.fstvar;
                            WHILE lsp1 <> NIL DO
                              WITH lsp1^ DO
                                IF varval = lval.ival THEN
                                  BEGIN
                                    lsize := size; lsp := subvar; GOTO 3
                                  END
                                ELSE
                                  lsp1 := prvcon;
                            lsize := lsp^.size; lsp := NIL;
                          END
                        ELSE
                          error(158)
                    ELSE
                      error(158);
          3:  END (* WHILE sy = comma *);
            gen1(locw, lsize);
            IF lkey = 9 THEN gen1(csp, 12(*new*))
                        ELSE gen1(csp, 29(*dispose*))
          END (* new/dispose procedure *);

          PROCEDURE absfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr = intptr THEN
                IF gattr.kind = cst THEN
                  gattr.cval.ival := abs(gattr.cval.ival)
                ELSE
                  gen0(absi)
              ELSE
              IF gattr.typtr = realptr THEN
                IF gattr.kind = cst THEN
                  gattr.cval.valp^.rval^.str[1] := '+'
                ELSE
                  gen0(absr)
              ELSE
                BEGIN error(66); gattr.typtr := intptr END
          END (* abs function *);

          PROCEDURE sqrfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr = intptr THEN
                IF gattr.kind = cst THEN
                  gattr.cval.ival := sqr(gattr.cval.ival)
                ELSE
                  gen0(sqri)
              ELSE
              IF gattr.typtr = realptr THEN
                BEGIN
                  IF gattr.kind <> expr THEN
                    load(gattr);
                  gen0(sqrr)
                END
              ELSE
                BEGIN error(66); gattr.typtr := intptr END
          END (* sqr function *);

          PROCEDURE truncfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr <> realptr THEN error(68);
            IF gattr.kind <> expr THEN load(gattr);
            gen0(trc);
            gattr.typtr := intptr
          END (* trunc function *);

          PROCEDURE roundfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr <> realptr THEN error(68);
            IF gattr.kind <> expr THEN load(gattr);
            gen0(rnd);
            gattr.typtr := intptr
          END (* round function *);

          PROCEDURE oddfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr <> intptr THEN
                error(69);
            IF gattr.kind = cst THEN
              gattr.cval.ival := ord(odd(gattr.cval.ival))
            ELSE
              gen0(iodd);
            gattr.typtr := boolptr
          END (* odd function *);

          PROCEDURE ordfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr^.form >= powerset THEN
                BEGIN error(70); gattr.kind := expr END;
            gattr.typtr := intptr
          END (* ord function *);

          PROCEDURE chrfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr <> intptr THEN
                error(69);
            IF gattr.kind = cst THEN
              IF (gattr.cval.ival < ordminchar) OR
                 (gattr.cval.ival > ordmaxchar) THEN
                error(194);
            gattr.typtr := charptr
          END (* chr function *);

          PROCEDURE predsuccfunction;
          BEGIN
            IF gattr.typtr <> NIL THEN
              BEGIN
                IF gattr.typtr^.form <> scalar THEN
                  error(71)
                ELSE
                IF gattr.typtr = realptr THEN
                  error(70)
              END;
            IF lkey = 7 THEN
              IF gattr.kind = cst THEN
                gattr.cval.ival := pred(gattr.cval.ival)
              ELSE
                gen1(decw, 1)
            ELSE
              IF gattr.kind = cst THEN
                gattr.cval.ival := succ(gattr.cval.ival)
              ELSE
                gen1(incw, 1)
          END (* predsucc function *);

          PROCEDURE eofeolnfunction;
          BEGIN
            IF sy = lparen THEN
              BEGIN
                insymbol;
                variable(fsys + [rparen]); { sets up gattr }
                IF sy = rparen THEN insymbol ELSE error(4)
              END
            ELSE
              BEGIN
                IF inputptr = NIL THEN
                  BEGIN error(175); dummyup(inputptr) END;
                WITH gattr DO
                  BEGIN { even if not in the header, we know it's 'input' }
                    typtr := textptr;
                    kind := varbl; access := drct;
                    formantBound := false; loopvar := false;
                    vlevel := inputptr^.vlev; dplmt := inputptr^.addr
                  END
              END;
            loadaddress(true); { add in any offset }
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr^.form <> files THEN
                error(116)
              ELSE
              IF (lkey = 10 { eoln } ) AND (gattr.typtr <> textptr) THEN
                error(169);
            IF lkey = 9 THEN { eof }
              BEGIN
                IF gattr.typtr = textptr THEN gen0(eofp)
                                         ELSE gen0(efb)
              END
            ELSE { lkey = 10: eoln }
              gen1(csp, 14(*eln*));
            gattr.typtr := boolptr;
            gattr.kind := expr; { result is on the TOS }
          END (* eofeoln function *);

          PROCEDURE callUserRoutine;
            LABEL 1; { avoid excessive indentation while checking for errors }
            VAR   prevCAP,formalParam,lcp: sTptr;
                  paramTyp,prevArgTyp,dummy1,dummy2,inx: tIptr;
                  lkind: idkind;
                  locpar, saveOldLc, lsize: addrrange;
                  savAttr,lattr: attr;
                  lsys: setofsys;

            PROCEDURE compparam(pla, plb: sTptr);
            BEGIN
              WHILE (pla <> NIL) AND (plb <> NIL) DO
                BEGIN
                  IF pla^.idtype <> plb^.idtype THEN { must be same type }
                    error(189);                      { not just 'similar' }
                  pla := pla^.next; plb := plb^.next
                END;
              IF (pla <> NIL) OR (plb <> NIL) THEN { # args <> # params }
                error(189)
            END;

            FUNCTION isArray(f: tIptr): Boolean;
            BEGIN
              isArray := false;
              IF f <> NIL THEN
                IF f^.form = arrays THEN
                  isArray := true
            END;
              
            PROCEDURE conformanceTest(arg, param: tIptr);
              VAR   arglo, arghi, parmlo, parmhi: integer;
                    argIsArray, paramIsArray: Boolean;
                    
            BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>conformanceTest');
  END;
{$endif}
              IF arg = param THEN { identical (or no) types: also OK }
              ELSE
                BEGIN
                  argIsArray := isArray(arg);
                  paramIsArray := isArray(param);
                  IF argIsArray AND NOT paramIsArray THEN
                    error(183) { more arg than param }
                  ELSE
                  IF paramIsArray AND NOT argIsArray THEN
                    error(181) { more param than arg }
                  ELSE
                  IF arg^.packing <> param^.packing THEN
                    error(90)
                  ELSE { both not NIL, both are arrays, but not identical types }
                    BEGIN
                      IF arg^.inxtype = NIL THEN { string }
                        BEGIN arglo := 1; arghi := arg^.size END
                      ELSE
                        getbounds(arg^.inxtype, arglo, arghi);
                      getBounds(param^.inxtype^.TidOrd, parmlo, parmhi);
                      IF (arglo < parmlo) OR (arghi > parmhi) THEN
                        error(182);
                      conformanceTest(arg^.aeltype, param^.aeltype)
                    END;
                END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<conformanceTest');
    indent := indent - 1;
  END;
{$endif}
            END;
            
            FUNCTION isBound(fpar: sTptr): Boolean;
            BEGIN
              isBound := false;
              IF fpar <> NIL THEN
                IF fpar^.klass = vars THEN
                  isBound := fpar^.vbound;
            END; { isBound }
            
            PROCEDURE pushArrayDescriptor(fsp: tIptr);
              VAR   lo, hi: integer;
                    done: Boolean;
            BEGIN { push the array descriptor(s) for the previous c.a.p. }
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>pushArrayDescriptor');
  END;
{$endif}
              REPEAT
                IF fsp^.inxtype = NIL THEN { fsp is string literal }
                  BEGIN
                    gen1(locw, 1);                  { low bound }
                    gen1(locw, lgth);               { high bound }
                    gen1(locw, lgth);               { stride }
                  END
                ELSE
                IF fsp^.inxtype^.form = confinx THEN
                  WITH fsp^.inxtype^.lobnd^ DO
                    BEGIN { fsp is a c.a.p. passed into caller }
                      gen2(lolw, level-vlev, addr);           { low bound }
                      gen2(lolw, level-vlev, addr+intsize);   { high bound }
                      gen2(lolw, level-vlev, addr+2*intsize); { stride }
                    END
                ELSE { arg is actual array (with compile-time known bounds) }
                  BEGIN
                    getbounds(fsp^.inxtype, lo, hi);
                    gen1(locw, lo);                 { low bound }
                    gen1(locw, hi);                 { high bound }
                    IF fsp^.aeltype <> NIL THEN
                      gen1(locw, fsp^.aeltype^.size); { stride }
                  END;
                IF formalParam <> NIL THEN { get past low bound parameter }
                  formalParam := formalParam^.next;
                IF formalParam <> NIL THEN { get past hi bound parameter }
                  formalParam := formalParam^.next;
                locpar := locpar + 3*intsize;
                fsp := fsp^.aeltype;
              UNTIL (NOT isBound(formalParam)) OR (NOT isArray(fsp));
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<pushArrayDescriptor');
    indent := indent - 1;
  END;
{$endif}
            END; { pushArrayDescriptor }
            
          BEGIN (* callUserRoutine *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>callUserRoutine');
  END;
{$endif}
            locpar := 0;
            WITH fcp^ DO
              BEGIN
                formalParam := params; lkind := pfkind;
                IF lkind = actual THEN
                  BEGIN
                    IF NOT externl THEN
                      gen1(mst, level-pflev)
                  END
                ELSE
                  gen1(mst, level-pflev) { its an indirect }
              END;
            IF sy = lparen THEN
              BEGIN
                prevCAP := NIL;
                prevArgTyp := NIL; { there isn't one yet }
                lsys := fsys + [comma, rparen];
                saveOldLc := lc;
                REPEAT
                  insymbol; { eat the lparen or comma }
                  IF formalParam = NIL THEN
                    BEGIN
                      IF (fcp <> uprcptr) AND (fcp <> ufctptr) THEN
                        error(126); { don't issue error if fcp already flagged }
                      skip(lsys);
                      GOTO 1
                    END;
                  IF formalParam^.klass IN [proc,func] THEN
                    BEGIN { actual arg must be routine w/ full parameter list }
                      IF sy <> ident THEN
                        BEGIN error(2); skip(lsys); GOTO 1 END;
                      IF formalParam^.klass = proc THEN
                        searchid([proc], lcp, true)
                      ELSE          { klass = func }
                        BEGIN
                          searchid([func], lcp, true);
                          { compare result types }
                          IF lcp^.idtype <> formalParam^.idtype THEN
                            error(128)
                        END;
                      { compare parameter lists }
                      IF lcp^.klass IN [proc,func] THEN
                        compparam(formalParam^.params, lcp^.params);
                      IF lcp^.pfkind = actual THEN
                        gen2lab(lpa, level-lcp^.pflev, lcp^.pfname)
                      ELSE
                        gen2(lip, level-lcp^.pflev, lcp^.addr);
                      lsize := ptrsize*2;
                      insymbol;
                      prevCAP := NIL;
                      IF formalParam <> NIL THEN
                        formalParam := formalParam^.next
                    END
                  ELSE { callee Needs expr value or variable }
                    BEGIN
                      paramTyp := formalParam^.idtype;
                      IF paramTyp = NIL THEN
                        BEGIN skip(lsys); GOTO 1 END;
                      IF paramTyp^.form = powerset THEN
                        IF formalParam^.vkind = actual THEN
                          BEGIN { formal is call-by-value set }
                            setLimit := paramTyp^.size; { for 'expression' }
                            lattr.typtr := paramTyp; lattr.kind := varbl;
                          END;
                      expression(lsys);
                      IF gattr.typtr = NIL THEN { error in expression }
                        BEGIN skip(lsys); GOTO 1 END;
                      { call-by-value OR call-by-reference? }
                      IF formalParam^.vkind = actual THEN { value }
                        BEGIN
                          IF paramTyp^.form <= powerset THEN
                            BEGIN { can be loaded to TOS without a loim }
                              savAttr := gattr; { before load turns it }
                              IF paramTyp^.form = powerset THEN
                                compatSize(lattr, gattr);
                              load(gattr);          { into an expr }
                              chkbounds(paramTyp, 194, savAttr);
                              IF comptypes(realptr, paramTyp) AND
                                 (gattr.typtr = intptr) THEN
                                BEGIN
                                  gen0(flt);
                                  gattr.typtr := realptr
                                END;
                            END
                          ELSE { array/rec value parameter: copy to TOS }
                            BEGIN { dest is the stack }
                              { only the source addr needs to be pushed }
                              loadaddress(false); { load instr includes offset }
                              { OK move the source onto the stack }
                              IF NOT isConfArrayParam(formalParam, dummy1, dummy2) THEN
                                load(gattr);
                            END;
                          lsize := paramTyp^.size;
                        END
                      ELSE { VAR parameter: call-by-reference }
                        BEGIN
                          IF gattr.kind <> varbl THEN
                            error(154)
                          ELSE
                            BEGIN
                              IF gattr.formantBound THEN
                                error(198);
                              IF gattr.loopvar THEN
                                error(188);
                              IF gattr.varsel THEN
                                error(172);
                              IF gattr.packedcompon THEN
                                error(79);
                              loadaddress(true); { add in any offset }
                            END;  
                          lsize := ptrsize;
                        END;  
                      IF isConfArrayParam(formalParam, paramTyp, inx) THEN
                        BEGIN
                          { first, look for actual args passed to formal params }
                          { that share the same c.a.p. specification: those }
                          { are of the same type and the args must be also }
                          IF prevCAP <> NIL THEN
                            IF prevCAP^.idtype = paramTyp THEN
                              IF prevArgTyp <> NIL THEN
                                IF NOT comptypes(prevArgTyp, gattr.typtr) THEN
                                  error(184);
                          prevCAP := formalParam;
                          conformanceTest(gattr.typtr, ParamTyp);
                          IF formalParam <> NIL THEN
                            formalParam := formalParam^.next;
                          IF NOT isConfArrayParam(formalParam, dummy1, dummy2) THEN
                            { formalParam is the last conformant array id in }
                            { the id-list of a conformant array specification }
                            pushArrayDescriptor(gattr.typtr);
                          lsize := ptrsize; { override the array dimension size }
                        END
                      ELSE                     { any other kind of arg }
                        BEGIN
                          IF formalParam^.vkind = actual THEN { value }
                            IF NOT comptypes(paramTyp, gattr.typtr) THEN
                              error(142)
                            ELSE
                          ELSE { variable parameter }
                            IF paramTyp <> gattr.typtr THEN { have to be }
                              error(96);    { exact match, not just structural }
                          prevCAP := NIL;
                          IF formalParam <> NIL THEN
                            formalParam := formalParam^.next
                        END;
                      prevArgTyp := gattr.typtr;
                    END;
                  locpar := locpar + lsize;
                  align(parmptr, locpar);
1:                UNTIL sy <> comma;
                lc := saveOldLc; { back to before we pushed lots of arguments }
                IF sy = rparen THEN insymbol ELSE error(18)
              END; (*if lparen*)
            IF lkind = actual THEN
              BEGIN
                IF formalParam <> NIL THEN
                  error(126);
                WITH fcp^ DO
                  IF externl THEN gen1(csp, pfname)
                             ELSE gen2lab(cal, locpar, pfname);
              END
            ELSE
              BEGIN { call proc or func that is a formal parameter }
                gen2(lal, level-fcp^.pflev, fcp^.addr);
                gen1(cali, locpar)
              END;
            gattr.typtr := fcp^.idtype;
            gattr.kind := expr;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<callUserRoutine');
    indent := indent - 1;
  END;
{$endif}
          END (* callUserRoutine *);

        BEGIN (* call *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>call');
  END;
{$endif}
          IF fcp^.pfdeckind = predclrd THEN
            BEGIN
              lkey := fcp^.key;
              IF fcp^.klass = proc THEN
                BEGIN
                  IF NOT (lkey IN [5,6,11,12,17]) THEN
                    IF sy = lparen THEN insymbol ELSE error(9);
                  CASE lkey OF
                    1,2,
                    3,4:   getputresetrewriteprocedure;
                    17:    pageprocedure;
                    5,11:  readprocedure;
                    6,12:  writeprocedure;
                    7:     packprocedure;
                    8:     unpackprocedure;
                    9,18:  newdisposeprocedure;
                    10,13: error(100) { internal compiler error }
                  END;
                  IF NOT (lkey IN [5,6,11,12,17]) THEN
                    IF sy = rparen THEN insymbol ELSE error(4)
                END
              ELSE { fcp^.klass = func }
                BEGIN
                  IF lkey IN [1..8,16] THEN
                    BEGIN
                      IF sy = lparen THEN insymbol ELSE error(9);
                      expression(fsys+[rparen]);
                      IF gattr.kind = varbl THEN load(gattr)
                    END;
                  CASE lkey OF
                    1:    absfunction;
                    2:    sqrfunction;
                    3:    truncfunction;
                    16:   roundfunction;
                    4:    oddfunction;
                    5:    ordfunction;
                    6:    chrfunction;
                    7,8:  predsuccfunction;
                    9,10: eofeolnfunction
                  END;
                  IF (lkey <= 8) OR (lkey = 16) THEN
                    IF sy = rparen THEN
                      insymbol
                    ELSE
                      BEGIN
                        error(4);
                        WHILE sy = comma DO
                          BEGIN
                            insymbol;
                            expression(fsys+[comma,rparen]);
                          END;
                        IF sy = rparen THEN insymbol;
                      END
                END;
            END (*predclrd procedures and functions*)
          ELSE
            callUserRoutine;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<call');
    indent := indent - 1;
  END;
{$endif}
        END (* call *);

        PROCEDURE expression(*fsys: setofsys*);
          LABEL 4; { to minimize indentation }
          VAR   lattr: attr;
                lsy: symbol;
                savLimit: integer;
                leftRandTyp: tIptr;
                leftConst,rightConst: Boolean;

          FUNCTION loadableConst(VAR fattr: attr): Boolean;
          BEGIN
            loadableConst := false;
            IF fattr.kind = cst THEN
              IF fattr.typtr <> NIL THEN
                IF fattr.typtr^.form <= powerset THEN { scalars and even sets }
                  IF fattr.typtr <> realptr THEN      { but no reals }
                    loadableConst := true;
          END;

          PROCEDURE simpleexpression(fsys: setofsys);
            LABEL 3; { to minimize indentation }
            VAR   lattr: attr;
                  lsy: symbol;
                  signed: Boolean;

            PROCEDURE term(fsys: setofsys);
              LABEL 2; { to minimize indentation }
              VAR   lattr: attr;
                    lsy: symbol;

              PROCEDURE factor(fsys: setofsys);
                LABEL 1; { actually simplifies error checking }
                VAR   lcp: sTptr;
                      lkonp: konstp;
                      gattrNonCst,rattrNonCst,varpart: Boolean;
                      cstpart: setty;
                      biggest: integer; { in cstpart }
                      lsp: tIptr;
                      savAttr, rattr: attr;
                      commaSeen: Boolean;
              BEGIN
{$ifdef __GPC__}
IF trace AND debug THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>factor');
  END;
{$endif}
                IF NOT (sy IN facbegsys) THEN
                  BEGIN
                    error(58); skip(fsys + facbegsys);
                    gattr.typtr := NIL
                  END;
                IF sy IN facbegsys THEN
                  CASE sy OF
            (*id*)  ident:
                      BEGIN
                        searchid([konst,vars,field,func], lcp, true);
                        insymbol;
                        IF lcp^.klass = func THEN
                          call(fsys, lcp) { function call invocation }
                        ELSE
                        IF lcp^.klass = konst THEN
                          WITH gattr, lcp^ DO
                            BEGIN
                              typtr := idtype;
                              kind := cst;
                              cval := values
                            END
                        ELSE { klass = vars or field }
                          selector(fsys + [range], lcp);
                      END;
            (*cst*) intconst:
                      BEGIN
                        WITH gattr DO
                          BEGIN typtr := intptr; kind := cst; cval := val END;
                        insymbol
                      END;
                    realconst:
                      BEGIN
                        WITH gattr DO
                          BEGIN typtr := realptr; kind := cst; cval := val END;
                        insymbol
                      END;
                    strngconst:
                      BEGIN
                        WITH gattr DO
                          BEGIN
                            IF lgth = 1 THEN
                              typtr := charptr
                            ELSE
                              BEGIN
                                getstc(lsp);
                                WITH lsp^ DO
                                  BEGIN form := arrays; aeltype := charptr;
                                    packing := true;
                                    inxtype := NIL; size := lgth * charsize
                                  END;
                                typtr := lsp
                              END;
                            kind := cst; cval := val
                          END;
                        insymbol
                      END;
            (* ( *) lparen:
                      BEGIN
                        insymbol;
                        expression(fsys + [rparen]);
                        IF sy = rparen THEN insymbol ELSE error(4)
                      END;
          (* NOT *) notop:
                      BEGIN
                        insymbol;
                        factor(fsys);
                        IF gattr.kind = cst THEN
                          gattr.cval.ival := 1 - gattr.cval.ival
                        ELSE
                          BEGIN
                            load(gattr);
                            gen0(inot);
                          END;
                        IF gattr.typtr <> NIL THEN
                          IF gattr.typtr <> boolptr THEN
                            BEGIN error(135); gattr.typtr := NIL END;
                      END;
            (* [ *) lbrack:
                      BEGIN
{$ifdef __GPC__}
IF trace AND debug THEN
  BEGIN
    writeln(' ':indent,'factor: set constructer, setLimit=',setLimit:1);
  END;
{$endif}
                        insymbol;
                        cstpart := [ ];
                        biggest := 0;
                        varpart := false;
                        getstc(lsp);
                        WITH lsp^ DO
                          BEGIN
                            size := setLimit; form := powerset;
                            elset := NIL; packing := false; matchpack := false
                          END;
                        IF sy = rbrack THEN
                          BEGIN
                            WITH gattr DO
                              BEGIN typtr := lsp; kind := cst END;
                            insymbol
                          END
                        ELSE
                          BEGIN
                            REPEAT
                              expression(fsys + [comma,range,rbrack]);
                              gattrNonCst := false;
                              IF gattr.kind = cst THEN
                                IF (gattr.cval.ival < setlow) OR
                                   (gattr.cval.ival > sethigh) THEN
                                  BEGIN
                                    error(74); skip(fsys+[comma,rbrack]);
                                    GOTO 1
                                  END
                                ELSE
                                  gattrNonCst := true;
                              rattr.typtr := NIL; { in case there's no range }
                              IF sy = range THEN { expr1 .. expr2 }
                                BEGIN
                                  IF gattr.kind = varbl THEN
                                    load(gattr);
                                  savAttr := gattr;    { expr1 }
                                  insymbol;
                                  expression(fsys + [comma,rbrack]);
                                  rattrNonCst := true;
                                  IF gattr.kind = cst THEN
                                    IF (gattr.cval.ival < setlow) OR
                                       (gattr.cval.ival > sethigh) THEN
                                      BEGIN
                                        error(74); skip(fsys+[comma,rbrack]);
                                        GOTO 1
                                      END
                                    ELSE
                                      rattrNonCst := false;
                                  rattr := gattr;      { rattr -> expr2 }
                                  gattr := savAttr;    { gattr -> expr1 }
                                END;
                              IF gattr.typtr = NIL THEN GOTO 1; { previous error}
                              IF gattr.typtr^.form <> scalar THEN
                                BEGIN error(136); gattr.typtr := NIL; GOTO 1 END;
                              IF rattr.typtr <> NIL THEN
                                BEGIN { process set element range }
                                  IF rattr.typtr^.form <> scalar THEN
                                    BEGIN error(136); rattr.typtr := NIL; GOTO 1 END;
                                  IF NOT comptypes(gattr.typtr,rattr.typtr) THEN
                                    BEGIN error(137); GOTO 1 END;
                                  IF (gattr.kind = cst) AND (rattr.kind = cst) THEN
                                    BEGIN { make that a CONSTANT set element range }
                                      IF (rattr.cval.ival < setlow) OR
                                         (rattr.cval.ival > sethigh) THEN
                                        BEGIN error(74); GOTO 1 END;
                                      cstpart := cstpart +
                                             [gattr.cval.ival..rattr.cval.ival];
                                      IF (biggest < rattr.cval.ival) AND
                                         (rattr.cval.ival >=
                                          gattr.cval.ival) THEN
                                        biggest := rattr.cval.ival;  
                                    END
                                  ELSE { one or both are non-const exprs }
                                    BEGIN
                                      IF gattr.kind <> expr THEN
                                        IF rattr.kind = expr THEN { OOPS! }
                                          BEGIN
                                            load(gattr);
                                            IF debug AND gattrNonCst THEN
                                              gen2(chk2, 0, setLimit*8-1);
                                            gen1(swp, gattr.typtr^.size);
                                          END
                                        ELSE
                                          BEGIN
                                            load(gattr);
                                            IF debug AND gattrNonCst THEN
                                              gen2(chk2, 0, setLimit*8-1);
                                            load(rattr);
                                          END
                                      ELSE { gattr.kind = expr }
                                        IF rattr.kind <> expr THEN
                                          load(rattr);
                                      IF debug AND rattrNonCst THEN
                                        gen2(chk2, 0, setLimit*8-1);
                                      gen1(rgs, setLimit);
                                      IF varpart THEN
                                        gen1(ior, setLimit)
                                      ELSE
                                        varpart := true
                                    END
                                END
                              ELSE { rattr.typtr = NIL; process singleton }
                                IF gattr.kind = cst THEN
                                  BEGIN
                                    cstpart := cstpart + [gattr.cval.ival];
                                    IF biggest < gattr.cval.ival THEN
                                      biggest := gattr.cval.ival;
                                  END  
                                ELSE
                                  BEGIN
                                    load(gattr);
                                    IF debug THEN
                                      gen2(chk2, 0, setLimit*8-1);
                                    gen1(sgs, setLimit);
                                    IF varpart THEN
                                      gen1(ior, setLimit)
                                    ELSE
                                      varpart := true
                                  END;
1:                            lsp^.elset := gattr.typtr;
                              gattr.typtr := lsp;
                              commaSeen := sy = comma;
                              IF commaSeen THEN insymbol
                            UNTIL NOT commaSeen;
                            IF sy = rbrack THEN insymbol ELSE error(12)
                          END;
                        IF varpart THEN
                          BEGIN
                            IF cstpart <> [ ] THEN
                              BEGIN
                                getpsetcst(lkonp);
                                lkonp^.cclass := pset;
                                lkonp^.pval := cstpart;
                                lkonp^.size := setLimit;
                                gensetcon(lkonp);
                                gen1(ior, setLimit);
                                gattr.kind := expr
                              END
                          END
                        ELSE
                          BEGIN
                            getpsetcst(lkonp);
                            lkonp^.cclass := pset;
                            lkonp^.pval := cstpart;
                            lkonp^.size := biggest DIV 8 + 1;
                            gattr.cval.valp := lkonp;
                            gattr.typtr^.size := lkonp^.size;
                          END
                      END; { lbrack }
          (* NIL *) nilsy:
                      WITH gattr DO
                        BEGIN
                          typtr := nilptr;
                          kind := cst;
                          cval.ival := nilval;
                          insymbol
                        END
                  END (* CASE *) ;
{$ifdef __GPC__}
IF trace AND debug THEN
  BEGIN
    writeln(' ':indent,'<factor');
    indent := indent - 1;
  END;
{$endif}
              END (* factor *);

            BEGIN (* term *)
{$ifdef __GPC__}
IF trace AND debug THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>term');
  END;
{$endif}
              factor(fsys + mulops);
              WHILE sy IN [imul,rdiv,idiv,imod,andop] DO
                BEGIN
                  IF NOT loadableConst(gattr) THEN load(gattr);
                  lattr := gattr;
                  lsy := sy;
                  insymbol;
                  factor(fsys + mulops);
                  IF gattr.typtr = NIL THEN { previous error: get out }
                    GOTO 2;
                  IF lattr.typtr = NIL THEN { previous error: get out }
                    BEGIN gattr.typtr := NIL; GOTO 2 END;
                  IF gattr.typtr^.form = powerset THEN
                    compatSize(lattr, gattr);
                  IF gattr.kind = expr THEN { on the TOS }
                    IF lattr.kind <> expr THEN { but left operand not loaded }
                      BEGIN
                        load(lattr); { left operand is now TOS; right op under }
                        IF NOT commute[lsy] THEN { non-commutative }
                          gen1(swp, gattr.typtr^.size);
                        { now they're both expr }
                      END
                    ELSE { both expr; OK }
                  ELSE { gattr.kind <> expr }
                    IF lattr.kind = expr THEN
                      load(gattr) { both expr }
                    ELSE { lattr is a foldable constant; is gattr? }
                      IF loadableConst(gattr) AND (lsy <> rdiv) THEN
                        IF (lsy IN [idiv,imod]) AND
                           (gattr.cval.ival = 0) AND
                           NOT deadcode THEN
                          error(170)
                        ELSE { "You gotta know when to fold 'em" }
                      ELSE
                        BEGIN
                          load(lattr);
                          load(gattr);
                        END;
                  CASE lsy OF
          (* * *)   imul:
                      IF (lattr.typtr = intptr) AND (gattr.typtr = intptr) THEN
                        IF (gattr.kind = cst) AND (lattr.kind = cst) THEN
                          gattr.cval.ival := gattr.cval.ival * lattr.cval.ival
                        ELSE
                          gen0(muli)
                      ELSE
                      IF (gattr.typtr^.form = powerset) AND
                         (lattr.typtr^.form = powerset) THEN
                        IF comptypes(lattr.typtr, gattr.typtr) THEN
                          IF (gattr.kind = cst) AND (lattr.kind = cst) THEN
                            gattr.cval.valp^.pval := gattr.cval.valp^.pval *
                                                     lattr.cval.valp^.pval
                          ELSE
                            gen1(iand, lattr.typtr^.size) { set intersection }
                        ELSE
                          BEGIN error(77); gattr.typtr := NIL END
                      ELSE
                        BEGIN
                          IF lattr.typtr = intptr THEN
                            BEGIN
                              gen0(flt2); { convert to float }
                              lattr.typtr := realptr
                            END
                          ELSE
                            IF gattr.typtr = intptr THEN
                              BEGIN
                                gen0(flt); { convert the other guy }
                                gattr.typtr := realptr
                              END;
                          IF (lattr.typtr = realptr) AND
                             (gattr.typtr = realptr) THEN
                            gen0(mulr)
                          ELSE
                            BEGIN error(77); gattr.typtr := NIL END
                        END;
          (* / *)   rdiv:
                      BEGIN
                        IF gattr.typtr = intptr THEN
                          BEGIN
                            gen0(flt);
                            gattr.typtr := realptr
                          END;
                        IF lattr.typtr = intptr THEN
                          BEGIN
                            gen0(flt2);
                            lattr.typtr := realptr
                          END;
                        IF (lattr.typtr = realptr) AND
                           (gattr.typtr = realptr) THEN
                          gen0(divr)
                        ELSE
                          BEGIN error(134); gattr.typtr := NIL END
                      END;
          (*div*)   idiv:
                      IF (lattr.typtr = intptr) AND
                         (gattr.typtr = intptr) THEN
                        IF gattr.kind = cst THEN { lattr.kind also cst }
                          IF gattr.cval.ival = 0 THEN
                            IF NOT deadcode THEN
                              BEGIN error(170); gattr.cval.ival := maxint END
                            ELSE
                          ELSE
                            gattr.cval.ival := lattr.cval.ival DIV gattr.cval.ival
                        ELSE
                          gen0(divi)
                      ELSE
                        BEGIN error(127); gattr.typtr := NIL END;
          (*mod*)   imod:
                      IF (lattr.typtr = intptr) AND
                         (gattr.typtr = intptr) THEN
                        IF gattr.kind = cst THEN { lattr.kind also cst }
                          IF gattr.cval.ival = 0 THEN
                            IF NOT deadcode THEN
                              BEGIN error(170); gattr.cval.ival := maxint END
                            ELSE
                          ELSE
                            gattr.cval.ival := lattr.cval.ival MOD gattr.cval.ival
                        ELSE
                          gen0(modi)
                      ELSE
                        BEGIN error(127); gattr.typtr := NIL END;
          (*and*)   andop:
                      IF (lattr.typtr = boolptr) AND
                         (gattr.typtr = boolptr) THEN
                        IF gattr.kind = cst THEN { lattr.kind also cst }
                          gattr.cval.ival := ord((gattr.cval.ival = 1) AND
                                                 (lattr.cval.ival = 1))
                        ELSE
                          gen1(iand, boolsize)
                      ELSE
                        BEGIN error(76); gattr.typtr := NIL END
                  END (*CASE*);
2:              END; (* WHILE sy IN mulops *)
{$ifdef __GPC__}
IF trace AND debug THEN
  BEGIN
    writeln(' ':indent,'<term');
    indent := indent - 1;
  END;
{$endif}
            END (* term *);

          BEGIN (* simpleexpression *)
{$ifdef __GPC__}
IF trace AND debug THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>simpleexpression');
  END;
{$endif}
            signed := false;
            IF (sy IN [plus,minus]) THEN
              BEGIN signed := sy = minus; insymbol END;
            term(fsys + addops);
            IF (gattr.kind = cst) AND (gattr.typtr = intptr) AND signed THEN
              BEGIN
                gattr.cval.ival := -gattr.cval.ival;
                signed := false;
              END;
            IF signed THEN
              BEGIN
                load(gattr);
                IF gattr.typtr = intptr THEN
                  gen0(negi)
                ELSE
                IF gattr.typtr = realptr THEN
                  gen0(negr)
                ELSE
                  BEGIN error(77); gattr.typtr := NIL END
              END;
            WHILE sy IN addops DO
              BEGIN
                IF NOT loadableConst(gattr) THEN load(gattr);
                lattr := gattr;
                lsy := sy;
                insymbol;
                term(fsys + addops);
                IF gattr.typtr = NIL THEN { previous error: get out }
                  GOTO 3;
                IF lattr.typtr = NIL THEN { previous error: get out }
                  BEGIN gattr.typtr := NIL; GOTO 3 END;
                IF gattr.typtr^.form = powerset THEN
                  compatSize(lattr, gattr);
                IF gattr.kind = expr THEN { on the TOS }
                  IF lattr.kind <> expr THEN { but left operand not loaded }
                    BEGIN
                      load(lattr); { left operand is now TOS; right op under }
                      IF NOT commute[lsy] THEN { non-commutative }
                        gen1(swp, gattr.typtr^.size);
                      { now they're both expr }
                    END
                  ELSE { both expr; OK }
                ELSE { gattr.kind <> expr }
                  IF lattr.kind = expr THEN
                    load(gattr) { both expr }
                  ELSE { lattr is a foldable constant; is gattr? }
                    IF loadableConst(gattr) THEN { const folding below }
                    ELSE
                      BEGIN
                        load(lattr);
                        load(gattr);
                      END;
                CASE lsy OF
        (*+*)     plus:
                    IF (lattr.typtr = intptr) AND (gattr.typtr = intptr) THEN
                      IF (gattr.kind = cst) AND (lattr.kind = cst) THEN
                        gattr.cval.ival := gattr.cval.ival + lattr.cval.ival
                      ELSE
                        gen0(addi)
                    ELSE
                    IF (gattr.typtr^.form = powerset) AND
                       (lattr.typtr^.form = powerset) THEN
                      IF comptypes(lattr.typtr, gattr.typtr) THEN
                        IF (gattr.kind = cst) AND (lattr.kind = cst) THEN
                          gattr.cval.valp^.pval := gattr.cval.valp^.pval +
                                                   lattr.cval.valp^.pval
                        ELSE
                          gen1(ior, lattr.typtr^.size) { set union }
                      ELSE
                        BEGIN error(77); gattr.typtr := NIL END
                    ELSE
                      BEGIN
                        IF lattr.typtr = intptr THEN
                          BEGIN
                            gen0(flt2);
                            lattr.typtr := realptr
                          END
                        ELSE
                        IF gattr.typtr = intptr THEN
                          BEGIN
                            gen0(flt);
                            gattr.typtr := realptr
                          END;
                        IF (lattr.typtr = realptr) AND
                           (gattr.typtr = realptr) THEN
                          gen0(addr)
                        ELSE
                          BEGIN error(77); gattr.typtr := NIL END
                      END;
        (*-*)     minus:
                    IF (lattr.typtr = intptr) AND (gattr.typtr = intptr) THEN
                      IF (gattr.kind = cst) AND (lattr.kind = cst) THEN
                        gattr.cval.ival := lattr.cval.ival - gattr.cval.ival
                      ELSE
                        gen0(subi)
                    ELSE
                    IF (gattr.typtr^.form = powerset) AND
                       (lattr.typtr^.form = powerset) THEN
                      IF comptypes(lattr.typtr, gattr.typtr) THEN
                        IF (gattr.kind = cst) AND (lattr.kind = cst) THEN
                          gattr.cval.valp^.pval := lattr.cval.valp^.pval -
                                                   gattr.cval.valp^.pval
                        ELSE
                          gen1(dif, lattr.typtr^.size) { set difference }
                      ELSE
                        BEGIN error(77); gattr.typtr := NIL END
                    ELSE
                      BEGIN
                        IF lattr.typtr = intptr THEN
                          BEGIN
                            gen0(flt2);
                            lattr.typtr := realptr
                          END
                        ELSE
                          IF gattr.typtr = intptr THEN
                            BEGIN gen0(flt);
                              gattr.typtr := realptr
                            END;
                        IF (lattr.typtr = realptr) AND (gattr.typtr = realptr)
                          THEN gen0(subr)
                        ELSE
                          BEGIN error(77); gattr.typtr := NIL END
                      END;
        (*or*)    orop:
                    IF (lattr.typtr = boolptr) AND (gattr.typtr = boolptr) THEN
                      IF (gattr.kind = cst) AND (lattr.kind = cst) THEN
                        gattr.cval.ival := ord((gattr.cval.ival = 1) OR
                                               (lattr.cval.ival = 1))
                      ELSE
                        gen1(ior, boolsize)
                    ELSE
                      BEGIN error(76); gattr.typtr := NIL END
                END (*CASE*);
3:            END; (* WHILE sy IN addops *)
{$ifdef __GPC__}
IF trace AND debug THEN
  BEGIN
    writeln(' ':indent,'<simpleexpression');
    indent := indent - 1;
  END;
{$endif}
          END (* simpleexpression *);

        BEGIN (*expression*)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>expression');
  END;
{$endif}
          simpleexpression(fsys + relops);
          IF sy IN relops THEN
            BEGIN
              leftConst := loadableConst(gattr);
              IF gattr.typtr <> NIL THEN
                BEGIN
                  IF gattr.typtr^.form = subrange THEN
                    gattr.typtr := gattr.typtr^.rangetype;
                  leftRandTyp := gattr.typtr;
                  IF leftRandTyp^.form <= powerset THEN { loadable }
                    IF NOT leftConst THEN load(gattr) ELSE
                  ELSE
                    loadaddress(true); { add in any offset }
                END;
              lattr := gattr;
              lsy := sy;
              IF lsy = inop THEN
                BEGIN
                  savLimit := setLimit;
                  setLimit := setsize;
                  IF lattr.kind <> expr THEN
                    load(lattr);
                END;
              insymbol;
              simpleexpression(fsys);
              IF gattr.typtr = NIL THEN { previous error: get out }
                GOTO 4;
              IF lattr.typtr = NIL THEN { previous error: get out }
                GOTO 4;
              IF gattr.typtr^.form <= powerset THEN { load'able guy }
                BEGIN
                  rightConst := loadableConst(gattr);
                  IF gattr.typtr^.form = powerset THEN
                    IF lsy = inop THEN
                      IF (gattr.kind = cst) THEN
                        BEGIN
                          gattr.cval.valp^.size := setsize;
                          gattr.typtr^.size := setsize;
                        END
                      ELSE
                    ELSE
                      compatSize(lattr, gattr);
                  IF NOT (leftConst AND rightConst) THEN
                    BEGIN
                      IF lattr.kind <> expr THEN
                        BEGIN
                          load(lattr);
                          IF gattr.kind = expr THEN
                            IF lsy = leop THEN lsy := geop
                            ELSE
                            IF lsy = geop THEN lsy := leop;
                        END;
                      load(gattr)
                    END;
                END
              ELSE   { record, array, etc. }
                loadaddress(true); { add in any offset }
              IF lsy = inop THEN
                BEGIN
                  IF gattr.typtr^.form <> powerset THEN
                    BEGIN error(130); GOTO 4 END;
                  IF (NOT comptypes(leftRandTyp, gattr.typtr^.elset)) OR
                     comptypes(leftRandTyp, realptr) THEN
                    BEGIN error(73); GOTO 4 END;
                  IF leftConst AND rightConst THEN
                    gattr.cval.ival := ord(lattr.cval.ival IN gattr.cval.valp^.pval)
                  ELSE
                    gen1(inn, gattr.typtr^.size);
                  setLimit := savLimit; { back to status quo ante }
                END
              ELSE
                BEGIN { lsy <> inop }
                  IF leftRandTyp <> gattr.typtr THEN
                    IF leftRandTyp = intptr THEN
                      BEGIN
                        gen0(flt2);
                        leftRandTyp := realptr
                      END
                    ELSE
                    IF gattr.typtr = intptr THEN
                      BEGIN
                        gen0(flt);
                        gattr.typtr := realptr
                      END;
                  IF NOT comptypes(leftRandTyp, gattr.typtr) THEN
                    BEGIN error(129); GOTO 4 END;
                  IF leftRandTyp^.form = pointer THEN
                    IF lsy IN [ltop,leop,gtop,geop] THEN error(131) ELSE
                  ELSE
                  IF leftRandTyp^.form = powerset THEN
                    IF lsy IN [ltop,gtop] THEN
                      error(132)
                    ELSE
                  ELSE
                  IF leftRandTyp^.form = arrays THEN
                    IF NOT string(lattr.typtr) THEN error(133) ELSE
                  ELSE
                  IF leftRandTyp^.form = records THEN
                    error(133)
                  ELSE
                  IF leftRandTyp^.form = files THEN
                    error(133);
                  IF (leftRandTyp^.form = powerset) AND
                     (lsy IN [leop, geop]) THEN
                    IF leftConst AND rightConst THEN
                      IF lsy = leop THEN
                        gattr.cval.ival := ord(lattr.cval.valp^.pval <=
                                               gattr.cval.valp^.pval)
                      ELSE
                        gattr.cval.ival := ord(lattr.cval.valp^.pval >=
                                               gattr.cval.valp^.pval)
                    ELSE
                      IF lsy = leop THEN
                        gen1(sst1, leftRandTyp^.size)
                      ELSE
                        gen1(sst2, leftRandTyp^.size)
                  ELSE
                  IF leftRandTyp^.form <= powerset THEN
                    IF leftConst AND rightConst THEN
                      IF leftRandTyp^.form = powerset THEN
                        IF lsy = eqop THEN
                          gattr.cval.ival := ord(gattr.cval.valp^.pval =
                                                 lattr.cval.valp^.pval)
                        ELSE { lsy = neop }
                          gattr.cval.ival := ord(gattr.cval.valp^.pval <>
                                                 lattr.cval.valp^.pval)
                      ELSE
                        CASE lsy OF
                          ltop: gattr.cval.ival := ord(lattr.cval.ival < gattr.cval.ival);
                          leop: gattr.cval.ival := ord(lattr.cval.ival <= gattr.cval.ival);
                          geop: gattr.cval.ival := ord(lattr.cval.ival >= gattr.cval.ival);
                          gtop: gattr.cval.ival := ord(lattr.cval.ival > gattr.cval.ival);
                          neop: gattr.cval.ival := ord(lattr.cval.ival <> gattr.cval.ival);
                          eqop: gattr.cval.ival := ord(lattr.cval.ival = gattr.cval.ival);
                        END { CASE }
                    ELSE
                      gen1(relopinstr1[lsy], leftRandTyp^.size)
                  ELSE
                    BEGIN
                      gen1(relopinstr2[lsy], leftRandTyp^.size);
                      gattr.kind := expr
                    END;
                END; { lsy <> inop }
4:            gattr.typtr := boolptr;
            END; (* sy IN relops *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<expression');
    indent := indent - 1;
  END;
{$endif}
        END (* expression *);

        PROCEDURE assignment(fcp: sTptr);
          VAR   lattr, savAttr: attr;

          FUNCTION schblk(fcp: sTptr): Boolean;
            VAR   i: disprange;
                  f: Boolean;
          BEGIN
            i := level;
            f := display[i].bname = fcp;
            WHILE (NOT f) AND (i > 2) DO
              BEGIN
                i := i - 1;
                f := display[i].bname = fcp;
              END;
            schblk := f;
          END;

        BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>assignment');
  END;
{$endif}
          WITH fcp^, gattr DO
            IF klass = func THEN
            IF pfdeckind = predclrd THEN
              BEGIN error(150); typtr := NIL END
            ELSE       { = expdclrd }
              BEGIN
                IF pfkind = formal THEN
                  error(151)
                ELSE
                  IF NOT schblk(fcp) THEN
                    error(192);
                access := drct; vlevel := pflev + 1;
                dplmt := 0;
                result := true;
              END;
          selector(fsys + [becomes,range], fcp);
          WITH gattr DO
            BEGIN
              IF formantBound THEN
                error(197); { conformant array bound can't be assigned }
              IF loopvar THEN
                error(187); { control variable of FOR can't be assigned }
            END;
          IF sy = becomes THEN
            BEGIN
              IF gattr.typtr <> NIL THEN
                BEGIN
                  IF (gattr.access = indrct) OR { careful with offset . . . }
                     (gattr.typtr^.form > powerset) THEN { for arrays, records }
                    loadaddress(gattr.typtr^.form > powerset); { add it in }
                  IF gattr.typtr^.form = powerset THEN
                    setLimit := gattr.typtr^.size;
                  { if that is all that can be assigned; then that is all that }
                  { should be evaluated }
                END;
              lattr := gattr; { lattr now refers to the var being assigned }
              insymbol; { eat the becomes }
              expression(fsys);
              savAttr := gattr; { because 'load' makes gattr.kind = expr }
              IF gattr.typtr <> NIL THEN
                IF gattr.typtr^.form <= powerset THEN
                  BEGIN
                    IF gattr.typtr^.form = powerset THEN
                      IF lattr.typtr <> NIL THEN
                        compatSize(lattr, gattr);
                    load(gattr)
                  END  
                ELSE
                  loadaddress(true); { add in any offset }
              IF (lattr.typtr <> NIL) AND (gattr.typtr <> NIL) THEN
                BEGIN
                  IF comptypes(realptr, lattr.typtr) AND
                     (gattr.typtr = intptr) THEN
                    BEGIN
                      gen0(flt);
                      gattr.typtr := realptr
                    END;
                  IF comptypes(lattr.typtr, gattr.typtr) THEN
                    BEGIN
                      IF filecomponent(gattr.typtr) THEN
                        error(191);
                      CASE lattr.typtr^.form OF
                        scalar,
                        subrange: BEGIN
                                    chkbounds(lattr.typtr, 194, savAttr);
                                    store(lattr)
                                  END;
                        pointer:  BEGIN  { never do run-time check of NIL }
                                    IF debug AND (savAttr.kind <> cst) THEN
                                      gen0(chk0);
                                    store(lattr)
                                  END;
                        powerset: store(lattr);
                        arrays,
                        records:  gen1(mov, lattr.typtr^.size);
                        files:    error(146)
                      END
                    END
                  ELSE
                    error(125)
                END
            END (*sy = becomes*)
          ELSE
            error(51);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<assignment');
    indent := indent - 1;
  END;
{$endif}
        END (* assignment *);

        PROCEDURE gotostatement;
          VAR   llp: lbp;
                saveTop: disprange;
        BEGIN
          IF sy = intconst THEN
            BEGIN
              IF val.ival > maxlabel THEN
                error(185);
              saveTop := top;
              WHILE display[saveTop].occur <> blck DO
                saveTop := saveTop - 1; { find the closest-containing block }
              searchlabel(llp, saveTop); { look for label here }
              IF llp <> NIL THEN { declared in this block }
                WITH llp^ DO
                  BEGIN
                    gen1lab(bra, labname);
                    IF defined THEN
                      IF stalvl < maxNesting THEN
                        error(186) { attempt to jump into deeper nesting level }
                      ELSE
                    ELSE { when label does get defined, it can't be at a level }
                      IF maxNesting > stalvl THEN { greater than this }
                        maxNesting := stalvl;
                  END 
              ELSE
                BEGIN
                  IF saveTop > 1 THEN       { there is enclosing block }
                    REPEAT
                      saveTop := saveTop - 1;
                      searchlabel(llp, saveTop); { look for label here }
                    UNTIL (llp <> NIL) { found } OR (saveTop = 1) { exhausted };
                  IF llp = NIL THEN { not found in any active block }
                    BEGIN
                      error(167);   { undeclared label }
                      newlabel(llp) { create dummy label in current context }
                    END             { to forestall more syntax errors }
                  ELSE { found in enclosing block: do an inter-procedural jump }
                    WITH llp^ DO
                      BEGIN
                        gen2lab(ipj, level-vlevel, labname);
                        maxNesting := 0; { in destination block, label must not }
                      END;               { be defined at any nested level }
                END;
              insymbol
            END
          ELSE
            error(15)
        END (* gotostatement *);

        PROCEDURE compoundstatement;
        BEGIN
          statSeq(fsys + [semicolon,endsy]);
          IF sy = endsy THEN insymbol ELSE error(13);
        END (* compoundstatemenet *);

        PROCEDURE ifstatement;
          VAR   elsebranch,afterelse: integer;
                PutLabelElse: Boolean;
        BEGIN
          expression(fsys + [thensy]);
          IF gattr.typtr <> boolptr THEN error(144);
          IF sy = thensy THEN insymbol ELSE error(52);
          PutLabelElse := true;
          IF gattr.kind = cst THEN
            IF gattr.cval.ival = 0 THEN { Boolean expr evaluates to false }
              BEGIN                     { the THEN branch is dead }
                genlabel(elsebranch);
                gen1lab(bra, elsebranch); { beginning of dead code }
              END
            ELSE { Boolean expr evaluates to true: fall through; }
                 { don't gen the elsebranch label: 'else' statemnt is dead }
              PutLabelElse := false
          ELSE
            BEGIN
              load(gattr);
              genlabel(elsebranch);
              gen1lab(fjp, elsebranch);
            END;
          statement(fsys + [elsesy]); { the THEN branch }
          IF sy = elsesy THEN
            BEGIN
              genlabel(afterelse);
              gen1lab(bra, afterelse);
              IF putLabelElse THEN
                putlabel(elsebranch, 'else        ');
              insymbol;
              statement(fsys);
              putlabel(afterelse, 'endif       ')
            END
          ELSE
            IF putLabelElse THEN
              putlabel(elsebranch, 'endif       ')
        END (* ifstatement *);

        PROCEDURE casestatement;
          LABEL 2,4;
          VAR   exprTyp,caseLabTyp: tIptr;
                fstptr,lpt1,lpt2,prevGtrLab,latestCasLab: cip;
                lval: valu;
                otherLab,exitLab, lcix, lcix1: integer;
                lmin, lmax, totCases: integer;
                sparse,commaSeen,semicolonSeen: Boolean;
                
          PROCEDURE put2byteLab(flab: integer);
          BEGIN { to build table of jump offsets }
            writeln(p6, 'C L', flab:1)
          END (*put2byteLab*);

          PROCEDURE put4byteLab(fconst,flab: integer);
          BEGIN { to build sparse CASE offset table }
            writeln(p6, 'S ', fconst:1, ' L',flab:1)
          END (*put4byteLab*);

        BEGIN { casestatement }
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>casestatement');
  END;
{$endif}
          expression(fsys + [ofsy,comma,colon]);
          load(gattr);
          genlabel(lcix);
          exprTyp := gattr.typtr;
          IF exprTyp <> NIL THEN
            IF (exprTyp^.form <> scalar) OR (exprTyp = realptr) THEN
              BEGIN error(153); exprTyp := NIL END;
          gen1lab(bra, lcix);
          IF sy = ofsy THEN insymbol ELSE error(8);
          totCases := 0;
          fstptr := NIL;
          genlabel(exitLab);
          genlabel(otherLab);
          REPEAT
            latestCasLab := NIL;
            genlabel(lcix1);
            IF sy = elsesy THEN
              BEGIN error(56); insymbol; statSeq(fsys) END
            ELSE
            IF (sy = ident) AND {_THEN} (idll^.str = 'otherwise   ') THEN
              BEGIN error(56); putstrs(idll); insymbol; statSeq(fsys) END;
            IF (sy = endsy) AND (fstptr <> NIL) THEN
              GOTO 2;
            REPEAT { pick up comma-list of case labels; add to chain }
              constant(fsys + [comma,colon,elsesy,range,endsy], caseLabTyp, lval);
              IF exprTyp <> NIL THEN
                IF comptypes(exprTyp, caseLabTyp) THEN
                  BEGIN
                    IF (lval.ival < minCaseLab) OR
                       (lval.ival > maxCaseLab) THEN error(157);
                    totCases := totCases + 1;
                    lpt1 := fstptr;
                    prevGtrLab := NIL;
                    WHILE lpt1 <> NIL DO { go through chain }
                      BEGIN
                        WITH lpt1^ DO
                          BEGIN
                            IF cslab <= lval.ival THEN
                              BEGIN
                                IF cslab = lval.ival THEN error(156);
                                GOTO 4
                              END;
                            { lpt1^.cslab > lval.ival }
                            prevGtrLab := lpt1;
                          END;
                        lpt1 := lpt1^.next;  
                      END;
        4:          getcas(latestCasLab);
                    WITH latestCasLab^ DO
                      BEGIN
                        next := lpt1;
                        cslab := lval.ival;
                        csstart := lcix1
                      END;
                    IF prevGtrLab = NIL THEN
                      fstptr := latestCasLab
                    ELSE
                      prevGtrLab^.next := latestCasLab
                  END
                ELSE
                  error(147);
              commaSeen := sy = comma;
              IF sy = range THEN
                BEGIN error(57); commaSeen := true END;
              IF commaSeen THEN insymbol
            UNTIL NOT commaSeen;
            IF sy = colon THEN
              insymbol
            ELSE
              BEGIN
                error(5);
                WHILE sy IN exprsys DO insymbol;
                IF sy = colon THEN insymbol;
              END;
            putlabel(lcix1, 'case        ');
            statement(fsys + [semicolon]);
            IF latestCasLab <> NIL THEN
              gen1lab(bra, exitLab);
            semicolonSeen := sy = semicolon;
            IF semicolonSeen THEN insymbol
          UNTIL NOT semicolonSeen;
2:        putlabel(lcix, 'casetable   ');
          IF fstptr <> NIL THEN
            BEGIN
              lmax := fstptr^.cslab;
              (*reverse pointers*)
              lpt1 := fstptr; fstptr := NIL;
              REPEAT
                lpt2 := lpt1^.next; lpt1^.next := fstptr;
                fstptr := lpt1; lpt1 := lpt2
              UNTIL lpt1 = NIL;
              lmin := fstptr^.cslab;
              sparse := (lmax - lmin) > sparseCase * totCases;
              gen3lab(cas, lmin, lmax, otherLab);
              IF sparse THEN
                BEGIN
                  gen1(locw, totCases); (* # of sparse table entries *)
                  gen1lab(sjp, otherLab);
                  flushpeep;
                  REPEAT
                    WITH fstptr^ DO
                      BEGIN
                        put4byteLab(cslab, csstart);
                        lmin := lmin + 1;
                      END;  
                    lpt1 := fstptr;
                    fstptr := fstptr^.next;
                    putcas(lpt1);
                  UNTIL fstptr = NIL;
                END
              ELSE
                BEGIN
                  IF lmin <> 0 THEN
                    BEGIN
                      gen1(locw, lmin);
                      gen0(subi); { expr - lmin }
                    END;
                  gen0(xjp);
                  flushpeep;
                  REPEAT
                    WITH fstptr^ DO
                      BEGIN
                        WHILE cslab > lmin DO
                          BEGIN
                            put2byteLab(otherLab);
                            lmin := lmin + 1
                          END;
                        put2byteLab(csstart);
                        lmin := lmin + 1;
                      END;  
                    lpt1 := fstptr;
                    fstptr := fstptr^.next;
                    putcas(lpt1);
                  UNTIL fstptr = NIL;
                END;
              putlabel(otherLab, 'other/error ');
              gen0(ujc { otherwise/error } );
              putlabel(exitLab, 'endcase     ')
            END;
          IF sy = endsy THEN insymbol ELSE error(13);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<casestatement');
    indent := indent - 1;
  END;
{$endif}
        END (* casestatement *);

        PROCEDURE repeatstatement;
          VAR   loopTop: integer;
        BEGIN
          genlabel(loopTop);
          putlabel(loopTop, 'repeat top  ');
          statSeq(fsys + [untilsy]);
          IF sy = untilsy THEN
            BEGIN
              insymbol;
              expression(fsys);
              IF gattr.typtr <> boolptr THEN error(144);
              IF gattr.kind = cst THEN
                IF gattr.cval.ival = 0 THEN { Boolean expr evaluates to false }
                  error{warning}(82)
                ELSE
                  BEGIN
                    error{warning}(83);
                    gen1lab(bra, loopTop)
                  END
              ELSE
                BEGIN  
                  load(gattr);
                  gen1lab(fjp, loopTop)
                END;
            END
          ELSE
            error(53);
        END (* repeatstatement *);

        PROCEDURE whilestatement;
          VAR loopTop, exitLab: integer;
        BEGIN
          genlabel(loopTop);
          putlabel(loopTop, 'while top   ');
          expression(fsys + [dosy]);
          IF gattr.typtr <> boolptr THEN error(144);
          IF sy = dosy THEN insymbol ELSE error(54);
          genlabel(exitLab);
          IF gattr.kind = cst THEN
            IF gattr.cval.ival = 0 THEN { Boolean expr evaluates to false }
              BEGIN
                error{warning}(84); { WHILE false DO body is dead }
                gen1lab(bra, exitLab)
              END
            ELSE
              error{warning}(85)    { WHILE true DO infinite loop? }
          ELSE
            BEGIN  
              load(gattr);
              gen1lab(fjp, exitLab);
            END;
          statement(fsys);
          gen1lab(bra, loopTop);
          putlabel(exitLab, 'endwhile    ')
        END (* whilestatement *);

        PROCEDURE createTemp(VAR fattr: attr; fsp: tIptr);
        BEGIN
          WITH fattr DO
            BEGIN
              typtr := fsp;
              kind := varbl;
              formantBound := false; loopvar := false;
              access := drct;
              vlevel := level;
              align(fsp, lc);
              dplmt := lc;
              IF fsp <> NIL THEN
                BEGIN
                  lc := lc + fsp^.size;
                  IF lcmax < lc THEN
                    lcmax := lc
                END;
            END;
        END;

        PROCEDURE forstatement;
          LABEL 1;
        { for v := e1 to/downto e2 do <for body> }
        { CASE 1: both e1 and e2 are constants }
        {   1.a: ((e2 < e1) AND 'TO') OR ((e2 > e1) AND 'DOWNTO') -> tripcount = 0 }
        {   1.b: ((e2 >= e1) AND 'TO') OR ((e2 <= e1) AND 'DOWNTO') -> t.c. non-zero }
        { CASE 2: one or both are unknown at compile time -> need checking code }
          TYPE  mode = (zeroTripCount, nonZeroTripCount, needChecking);
                pattr = ^attr;
          VAR   direction: symbol; { to or downto? }
                m: mode;
                testop: opcodes;
                exitLab,loopTop: integer;
                ctrlVar: sTptr;   { ptr to control variable in symbol table }
                vAttr,tempAttr,e1Attr,e2Attr: pattr;
                lsys: setofsys;
                stack: ARRAY [0..3] OF pattr;
                TOS: integer;
                cvMin,cvMax: integer;
                e1Extremal,e2Extremal: Boolean;

          PROCEDURE pop(i: integer);
          BEGIN
            TOS := TOS - i;
          END;
          
          PROCEDURE push(fattr: pattr);
          BEGIN
            TOS := TOS + 1;
            stack[TOS] := fattr;
          END;
          
          PROCEDURE  loadpush(fattr: pattr);
          BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>loadpush');
  END;
{$endif}
            IF fattr <> NIL THEN
              gattr := fattr^; { don't destroy fattr's memory info }
            load(gattr);
            push(fattr);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<loadpush');
    indent := indent - 1;
  END;
{$endif}
          END;
          
          PROCEDURE incdecCV;
          BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>incdecCV');
  END;
{$endif}
            loadpush(vAttr);
            IF direction = tosy THEN gen1(incw, 1)
                                ELSE gen1(decw, 1);
            store(vAttr^);
            pop(1);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<incdecCV');
    indent := indent - 1;
  END;
{$endif}
          END;

          PROCEDURE condBranch(fop: opcodes; targetLab: integer);
          BEGIN { we assume vAttr & tempAttr have been are initialized or cst }
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>condBranch: TOS = ',TOS);
writeln('tempAttr^.kind=',ord(tempAttr^.kind));
  END;
{$endif}
            IF TOS = -1 THEN { nothing loaded, do control variable first }
              loadpush(vAttr);
            IF TOS = 0 THEN { one of them is already in the stack }
              IF stack[0{TOS}] = vAttr THEN { this is the vanilla case }
                loadpush(tempAttr)
              ELSE { reverse order, but we'll just commute the fop }
                loadpush(vAttr);
            IF TOS = 1 THEN { ready, but in what order? }
              IF (stack[1{TOS}] = vAttr) AND (stack[0{TOS-1}] = tempAttr) THEN
                fop := comRel[fop];
            gen1(fop, 4);
            pop(2); { restore TOS back to -1 }
            gen1(fjp, targetLab);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<condBranch');
    indent := indent - 1;
  END;
{$endif}
          END;
          
        BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>forstatement');
  END;
{$endif}
          new(vAttr); new(tempAttr); new(e1Attr); new(e2Attr);
          TOS := -1;
          lsys := fsys - [ident,period] + [becomes,tosy,downtosy,dosy];
          ctrlVar := NIL; { just in case }
          IF sy = ident THEN
            BEGIN
              searchid([vars,field], ctrlVar, true); { field is illegal, but will }
              WITH ctrlVar^, vAttr^ DO      { be caught below, where the error }
                BEGIN                       { message will make more sense }
                  typtr := idtype;
                  IF typtr <> NIL THEN
                    IF typtr^.form = subrange THEN
                      typtr := typtr^.rangetype;
                  kind := varbl;
                  IF klass = vars THEN
                    BEGIN
                      IF vbound THEN
                        error(197); { formant array bounds can't be FOR ctrl vars }
                      IF readonly THEN
                        error(75)  { already in use as a FOR ctrl var }
                      ELSE
                        readonly := true; { c.v. can't be changes within the loop }
                      loopvar := true;
                      IF vkind = formal THEN
                        error(178);
                      formantBound := vbound;
                      vlevel := vlev;
                      IF vlev <> level THEN
                        error(179);
                    END
                  ELSE { klass = field }
                    BEGIN error(171); loopvar := false; formantBound := false END;
                  access := drct;
                  dplmt := addr;
                  IF typtr = realptr THEN
                    BEGIN error(143); typtr := NIL END;
                  insymbol;
                  cvMin := -maxint-1; cvMax := maxint;
                  IF idtype <> NIL THEN
                    IF idtype^.form > subrange THEN
                      BEGIN
                        error(171);
                        { let's parse all we can: }
                        selector(lsys, ctrlVar);
                        typtr := gattr.typtr;
                        { to minimize further errors: }
                        readonly := false;
                      END
                    ELSE
                      getbounds(idtype, cvMin, cvMax);
                END;
            END
          ELSE
            BEGIN
              WITH vAttr^ DO { dummy up something to minimize further errors }
                BEGIN
                  typtr := intptr; kind := varbl; access := drct;
                  vlevel := level; dplmt := 0; formantBound := false;
                  loopvar := true;
                END;
              error(2); skip(lsys)
            END;
          IF sy = inop THEN
            BEGIN
              error(108);
              insymbol;
              expression(lsys);
              IF sy = dosy THEN insymbol ELSE error(54);
              statement(fsys);
              GOTO 1;
            END;
          IF sy = becomes THEN insymbol ELSE BEGIN error(51); skip(lsys) END;
          expression(lsys);        { initial value }
          e1Attr^ := gattr;
          IF e1Attr^.kind = expr THEN
            push(e1Attr);
          IF e1Attr^.typtr = NIL THEN { because of previous error }
            e1Attr^.typtr := vattr^.typtr;
          IF NOT comptypes(vAttr^.typtr, e1Attr^.typtr) THEN
            BEGIN chcnt := chcnt - 4; error(145); chcnt := chcnt + 4 END;
          IF sy IN [tosy,downtosy] THEN
            BEGIN direction := sy; insymbol END
          ELSE
            BEGIN error(55); skip(lsys); direction := tosy END;
          expression(lsys);        { final value }
          e2Attr^ := gattr;
          IF e2Attr^.kind = expr THEN
            push(e2Attr);
          IF e2Attr^.typtr = NIL THEN { because of previous error }
            e2Attr^.typtr := vAttr^.typtr;
  
          IF NOT comptypes(vAttr^.typtr, e2Attr^.typtr) THEN
            BEGIN chcnt := chcnt - 4; error(145); chcnt := chcnt + 4 END;
          IF sy = dosy THEN insymbol ELSE error(54);
          
          e1Extremal := true; { guilty until proven innocent }
          IF e1Attr^.kind = cst THEN
            IF direction = tosy THEN
              IF (vAttr^.typtr = charptr) OR (vAttr^.typtr = boolptr) THEN
                e1Extremal := e1Attr^.cval.ival = 0 { ordminchar or ord(false) }
              ELSE
                e1Extremal := e1Attr^.cval.ival = -maxint-1
            ELSE
              IF vAttr^.typtr = charptr THEN
                e1Extremal := e1Attr^.cval.ival = ordmaxchar
              ELSE
              IF vAttr^.typtr = boolptr THEN
                e1Extremal := e1Attr^.cval.ival = ord(true)
              ELSE
                e1Extremal := e1Attr^.cval.ival = maxint;
          e2Extremal := true; { guilty until proven innocent }
          IF e2Attr^.kind = cst THEN
            IF direction = tosy THEN
              IF vAttr^.typtr = charptr THEN
                e2Extremal := e2Attr^.cval.ival = ordmaxchar
              ELSE
              IF vAttr^.typtr = boolptr THEN
                e2Extremal := e2Attr^.cval.ival = ord(true)
              ELSE
                e2Extremal := e2Attr^.cval.ival = maxint
            ELSE
              IF (vAttr^.typtr = charptr) OR (vAttr^.typtr = boolptr) THEN
                e2Extremal := e2Attr^.cval.ival = 0 { ordmaxchar or ord(false) }
              ELSE
                e2Extremal := e2Attr^.cval.ival = -maxint-1;
          IF direction = tosy THEN testop := leq
                              ELSE testop := geq;
          IF e2Extremal AND { we mustn't inc/dec beyond the end of e2 }
             NOT e1Extremal THEN { we can adjust e1 and do the inc/dec first }
            WITH e1Attr^.cval DO
              IF direction = tosy THEN
                BEGIN ival := ival - 1; testop := les END
              ELSE { dir = downtosy }
                BEGIN ival := ival + 1; testop := gtr END;
          { time for a little mode analysis: }
          IF (e1Attr^.kind = cst) AND (e2Attr^.kind = cst) THEN
            BEGIN
              IF direction = tosy THEN
                IF e1Attr^.cval.ival > e2Attr^.cval.ival THEN
                  m := zeroTripCount
                ELSE
                  m := nonZeroTripCount
              ELSE { direction = downtosy }
                IF e2Attr^.cval.ival > e1Attr^.cval.ival THEN
                  m := zeroTripCount
                ELSE
                  m := nonZeroTripCount;
            END
          ELSE
            BEGIN
              m := needChecking; { need to generate checking code }
              IF e2Attr^.kind <> cst THEN { put e2 in a temp }
                BEGIN { before initializing the c.v.; it might be in e2! }
                  IF e2Attr^.kind = varbl THEN
                    loadpush(e2Attr);
                  gen0(dup); { stack[TOS] has to be e2Attr }
                  createTemp(tempAttr^, e2Attr^.typtr);
                  store(tempAttr^);   { tempAttr := e2 }
                  stack[TOS] := tempAttr;
                END
              ELSE { e2 is a constant }
                tempAttr^ := e2Attr^;
              IF e1Attr^.kind <> expr THEN { cst or varbl }
                loadpush(e1Attr)
              ELSE
              IF e2Attr^.kind = expr THEN { two expr in stack; e2 no top }
                BEGIN
                  gen1(swp, 4);
                  stack[TOS-1] := tempAttr;
                END;
              gen0(dup);
              store(vAttr^);          { vattr := e1 }
              stack[TOS] := vAttr;
              genlabel(exitLab);
              condBranch(testop, exitLab);
            END;
            
          IF m = zeroTripCount THEN { gen branch around for body }
            BEGIN
              genlabel(exitLab);
              gen1lab(bra, exitLab);
            END
          ELSE
          IF m = nonZeroTripCount THEN
            BEGIN { e1 ( and e2) is cst }
              loadpush(e1Attr);
              store(vAttr^);          { vAttr := e1 }
              pop(1);
              tempAttr^ := e2Attr^;   { when e2 is cst, don't create new temp }
            END;
            
          IF m <> zeroTripCount THEN
            BEGIN
              genlabel(loopTop);
              putlabel(loopTop, 'for loop top');
              IF e2Extremal AND NOT e1Extremal THEN
                incdecCV;
            END;
          
          statement(fsys);
          
          IF m <> zeroTripCount THEN
            BEGIN { again three cases: }
              IF NOT e2Extremal THEN
                BEGIN
                  incdecCV;
                  IF direction = tosy THEN
                    condBranch(gtr, loopTop)
                  ELSE
                    condBranch(les, loopTop)
                END
              ELSE { e2 could possibly be maxint (or -maxint-1 in downto case) }
              IF e1Extremal THEN { couldn't adjust e1, so: most general case }
                BEGIN { e.g. FOR i := m TO n DO ..., where i,m,n all integers }
                  IF m <> needChecking THEN { if not already gen'd }
                    genlabel(exitLab);
                  condBranch(neq, exitLab); { conditional branch out of middle }
                  incdecCV;
                  gen1lab(bra, loopTop);
                END
              ELSE { e1 was "adjusted" }
                condBranch(equ, loopTop)
            END;
          IF (m <> nonZeroTripCount) OR (e2Extremal AND e1Extremal) THEN
            putlabel(exitLab, 'end for     ');
1:        IF ctrlVar <> NIL THEN
            ctrlVar^.readonly := false;
          { thanks guys, your checks are in the mail: }
          dispose(vAttr); dispose(tempAttr); dispose(e1Attr); dispose(e2Attr);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<forstatement');
    indent := indent - 1;
  END;
{$endif}
        END (* forstatement *);

        PROCEDURE withstatement;
          VAR lcp: sTptr; oldtop: disprange; llc,accum: addrrange;
              commaSeen: Boolean;
        BEGIN
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>withstatement');
  END;
{$endif}
          oldtop := top;
          llc := lc;
          REPEAT
            IF sy = ident THEN
              BEGIN
                searchid([vars,field], lcp, true);
                insymbol
              END
            ELSE
              BEGIN error(2); lcp := uvarptr END;
            selector(fsys + [comma,dosy], lcp);
            IF gattr.typtr <> NIL THEN
              IF gattr.typtr^.form = records THEN
                IF top < displimit THEN
                  BEGIN
                    accum := 0;
                    IF display[top].occur = vrec THEN
                      accum := display[top].offset;
                    top := top + 1;
                    WITH display[top], gattr DO
                      BEGIN
                        fname := typtr^.fstfld;
                        flabel := NIL; fconst := NIL; ftype := NIL;
                        IF access = drct THEN
                          BEGIN
                            occur := crec;
                            clev := vlevel;
                            cdspl := dplmt
                          END
                        ELSE { access = indrct: ptr on TOS; offset in idplmt }
                          BEGIN
                            loadaddress(false); { store instr applies offset }
                            align(nilptr, lc);
                            IF level > 1 THEN gen2(stlw, 0, lc) { local temp }
                                         ELSE gen1(stgw, lc);   { global temp }
                            occur := vrec;
                            vdspl := lc;
                            offset := idplmt + accum; { from outer-level vrecs }
                            lc := lc + ptrsize;
                            IF lcmax < lc THEN
                              lcmax := lc
                          END
                      END
                  END
                ELSE
                  error(64)
              ELSE
                error(140);
            commaSeen := sy = comma;
            IF commaSeen THEN insymbol
          UNTIL NOT commaSeen;
          IF sy = dosy THEN insymbol ELSE error(54);
          statement(fsys);
          { purge display levels }
          WHILE top > oldtop DO
            BEGIN
              { don't recycle the record context }
              display[top].fname := NIL;
              putdsp(top); { purge }
              top := top - 1;
            END;
          lc := llc;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<withstatement');
    indent := indent - 1;
  END;
{$endif}
        END (* withstatement *);

      BEGIN (* statement *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>statement');
  END;
{$endif}
        IF sy = intconst THEN (* label is being 'defined' by being seen *)
          BEGIN               (* pre-fixing a statement ("applied reference") *)
            searchlabel(llp, level); { search label in this block }
            IF llp <> NIL THEN
              WITH llp^ DO
                BEGIN { found }
                  IF defined THEN error(165); { multidefined label }
                  defined := true; { set defined at "applied reference" }
                  IF stalvl > maxNesting THEN
                    error(186);
                  maxNesting := stalvl;
                  putlabel(labname, 'goto target '); { output label to intermediate }
                END
            ELSE { label must be declared in closest-containing block }
              BEGIN { not found }
                error(167); { undeclared label }
                newlabel(llp) { create a dummy label }
              END;
            insymbol;
            IF sy = colon THEN insymbol ELSE error(5)
          END;
        stalvl := stalvl + 1;
        setLimit := setsize;
        { for expression, in case there is a 'if S1 = S2 then' }
        IF sy IN statbegsys + [ident] THEN
          BEGIN
            CASE sy OF
              ident:
                BEGIN
                  searchid([vars,field,func,proc], lcp, true);
                  insymbol;
                  IF (lcp = uvarptr) AND { didn't find identifier }
                     (sy IN fsys + [lparen]) THEN  { it's acting like a proc }
                    lcp := uprcptr;      { so parse it like one }
                  IF (lcp = uprcptr) AND (sy = lparen) THEN
                    BEGIN
                      REPEAT
                        insymbol; { eat the lparen and any comma }
                        expression(fsys + [comma,rparen]);
                      UNTIL sy IN fsys + [rparen];
                      IF sy = rparen THEN insymbol ELSE error(4)
                    END
                  ELSE
                  IF (lcp^.klass = proc) AND (sy <> becomes) THEN
                    call(fsys,lcp) { procedure call invocation }
                  ELSE
                    assignment(lcp)
                END;
              beginsy:  BEGIN insymbol; compoundstatement END;
              gotosy:   BEGIN insymbol; gotostatement END;
              ifsy:     BEGIN insymbol; ifstatement END;
              casesy:   BEGIN insymbol; casestatement END;
              whilesy:  BEGIN insymbol; whilestatement END;
              repeatsy: BEGIN insymbol; repeatstatement END;
              forsy:    BEGIN insymbol; forstatement END;
              withsy:   BEGIN insymbol; withstatement END
            END; { CASE }
          END
        ELSE
        IF sy IN fsys THEN
          { empty statement }
        ELSE
          BEGIN error(26); skip(fsys) END;
        stalvl := stalvl - 1;
        llp := display[level].flabel;    { close off some labels }
        WHILE llp <> NIL DO WITH llp^ DO { from being GOTO-able }
          BEGIN
            IF defined THEN
              IF stalvl < maxNesting THEN
                maxNesting := maxint { NO MORE GOTOs to this label allowed }
              ELSE
            ELSE
            IF maxNesting < maxint THEN { at least one GOTO seen (but not ipj) }
              IF stalvl < maxNesting THEN
                maxNesting := stalvl;
            llp := next; { process each label in linked list }
          END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<statement');
    indent := indent - 1;
  END;
{$endif}
      END (* statement *);

      BEGIN   { statSeq }
        fsys := fsys + [semicolon];
        statement(fsys);
        WHILE (sy = semicolon) OR (sy IN (statbegsys + [ident])) DO
          BEGIN
            IF sy = semicolon THEN
              insymbol
            ELSE
              error(14);
            statement(fsys)
          END;
      END;    { statSeq }
      
    BEGIN (* body *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>body');
  END;
{$endif}
      pin := pmaxm1; pout := pmaxm1; ps := 0;
      wereinafunc := false;
      IF fprocp <> NIL THEN { called by procdeclaration }
        BEGIN
          entname := fprocp^.pfname;
          putlabel(entname, fprocp^.name^.str);
        END
      ELSE                  { called by programme }
        BEGIN
          genlabel(entname);
          putlabel(entname, 'main        ');
        END;
      genlabel(segsize);
      gen1lab(ent, segsize); { allocated local variables }
      IF fprocp = NIL THEN { called by programme }
        BEGIN
          IF cmndLine THEN
            BEGIN
              { load address of 'commandline' }
              gen2(lal, 0, lcaftermarkstack);
              gen1(locw, 255);
              gen0(cmln);
            END;
          i := 1;
          WHILE prolist <> NIL DO
            BEGIN
              WITH prolist^ DO
                BEGIN
                  gen2(lal, 0, offset);
                  len := lenpv(id);
                  gen1(locw, len);
                  new(kp, strg);
                  WITH kp^ DO
                    BEGIN cclass := strg; slgth := len; sval := id END;
                  genloca(kp);
                  gen0(asgn);
                END;
              prolist := prolist^.next;
            END;
        END
      ELSE { fprocp <> NIL, so called by procdeclaration }
        BEGIN
          IF fprocp^.idtype <> NIL THEN { typed: must be a func }
            wereinafunc := true;
          parm := fprocp^.params; { go through parameter list, looking for }
          WHILE parm <> NIL DO    { conformant array parameters that are }
            BEGIN                 { call-by-value: they must be copied }
              IF isConfArrayParam(parm, lparmTyp, linxTyp) AND
                 (parm^.vkind = actual) THEN { value conformant array parameter }
                BEGIN { despite its being 'value', the caller passed the address }
                  gen2(lolw, 0, linxTyp^.hibnd^.addr); { hi }
                  gen2(lolw, 0, linxTyp^.lobnd^.addr); { lo }
                  gen0(subi);                          { hi-lo }
                  gen1(incw, 1);                       { hi-lo+1: # of row els }
                  gen2(lolw, 0, linxTyp^.hibnd^.addr+4); { stride }
                  gen0(muli);               { size of caller's array }
                  gen2(lal, 0, parm^.addr); { address of addr(caller's array) }
                  gen0(copy); { shazaam! the c.a.p. is copied into local stack }
                  { In particular, execution of the 'copy' p-code: }
                  { 1. pops the address and then the size }
                  { 2. de-references address to get REAL start of actual array }
                  { 3. copies actual array into SP for 'size' }
                  { 4. stores SP into address (point formal param at copy) }
                  { 5. updates SP using 'size' to include copied array in frame }
                  parm^.vkind := formal; { now it's the address of the copy }
                END;                     { but it's still an address }
              parm := parm^.next;
            END;
        END;
      lcmax := lc;
      statSeq(fsys);
      IF sy = endsy THEN insymbol ELSE error(13);
      llp := display[top].flabel; (* look for labels declared that never *)
      WHILE llp <> NIL DO WITH llp^ DO (* showed *)
        BEGIN
          IF NOT defined THEN
            IF undeclared THEN
              BEGIN
                error(98);
                errtbl[98].labnum := labval; { overwrite "sym" field }
              END
            ELSE
              BEGIN
                error(168);
                errtbl[168].labnum := labval; { overwrite "sym" field }
              END;
          llp := next
        END;
      IF wereinafunc THEN
        BEGIN
          IF NOT fprocp^.result THEN { no result ever assigned }
            error(95);
          IF comptypes(fprocp^.idtype, realptr) THEN
            gen0(ret2)
          ELSE
            gen0(ret1) { funcs }
        END
      ELSE
        gen0(ret0); { procs and progs }
      flushpeep;
      align(parmptr, lcmax);
      putlabelFixup(segsize, lcmax);
      IF fprocp = NIL THEN { called by programme }
        BEGIN
          writeln(p6, 'q');
          deadcode := false; { In case, 'bra' was last thing statseq gen'd }
          IF totErr > 0 THEN
            gen0(fatl) { prevent execution of pcode file }
          ELSE
            BEGIN
              (*generate call of main program; note that this call
                must be loaded at address zero in the code space *)
              gen1(mst, 0);
              gen2lab(cal, 0, entname);
            END;
          gen0(stop);
          flushpeep;
          writeln(p6,'q'); { when to stop; a line beginning w/ 'q' is it }
        END;  
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<body');
    indent := indent - 1;
  END;
{$endif}
    END (*body*) ;

  BEGIN (* block *)
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>block');
  END;
{$endif}
    stalvl := 0; { clear statement nesting level }
1:  lsys := fsys + blockbegsys;
    IF NOT (sy IN lsys) THEN
      BEGIN error(27); skip(lsys) END;
    IF sy = labelsy THEN
      BEGIN insymbol; labeldeclaration END;
    IF sy = constsy THEN
      BEGIN insymbol; constdeclaration END;
    IF sy = typesy THEN
      BEGIN insymbol; typedeclaration END;
    IF sy = varsy THEN
      BEGIN insymbol; vardeclaration END;
    WHILE sy IN [procsy,funcsy] DO
      BEGIN lsy := sy; insymbol; procdeclaration(lsy) END;
    IF sy IN [labelsy,constsy,typesy,varsy] THEN
      BEGIN error(174); GOTO 1 END;
    WITH display[top] DO
      WHILE forwProcFunc <> NIL DO
        BEGIN
          error(173);
          forwProcFunc := forwProcFunc^.next;
        END;
    IF sy = beginsy THEN insymbol ELSE error(17);
    body(fsys+[endsy]);
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<block');
    indent := indent - 1;
  END;
{$endif}
  END (* block *);

  PROCEDURE programme(fsys: setofsys);
    VAR   progparam: sTptr;
          i: integer;
    
    PROCEDURE processCmndLine;
      CONST cmndLineSize = 255;
      VAR   ptr,ary,inx: tIptr;
    BEGIN
      getstc(inx);
      WITH inx^ DO
        BEGIN packing := false;
          form := subrange; rangetype := intptr; size := intsize;
          min.ival := 1; max.ival := cmndLineSize
        END;
      getstc(ary);
      WITH ary^ DO
        BEGIN packing := true; size := cmndLineSize;
          form := arrays; inxtype := inx; aeltype := charptr
        END;
      getstc(ptr);
      WITH ptr^ DO
        BEGIN size := ptrsize; packing := false;
          form := pointer; eltype := ary
        END;
      WITH progparam^ DO
        BEGIN
          idtype := ptr;
          addr := lcaftermarkstack { leave a space for a pointer before }
        END;                       { the textfiles appearing in the header }
      cmndLine := true; { generate some prologue p-code to access the cmd line }
    END;

    PROCEDURE processProgParam;
      VAR   p: prologp;
    BEGIN
      getid(progparam);
      WITH progparam^ DO
        BEGIN
          name := idll; klass := vars; vbound := false;
          vkind := actual; vlev := 1;
          IF idll^.str = 'commandline ' THEN
            processCmndLine 
          ELSE { additional textfiles that the interpreter can use for reading }
            BEGIN { and writing p-code files, as an example }
              idtype := textptr;
              IF idll^.str = 'input       ' THEN
                BEGIN
                  inputptr := progparam;
                  addr := lcaftermarkstack + 4
                END
              ELSE
              IF idll^.str = 'output      ' THEN
                BEGIN
                  outputptr := progparam;
                  addr := lcaftermarkstack + 6
                END
              ELSE
                BEGIN
                  addr := lc;
                  lc := lc + filesize + charsize; { file & buffer }
                  new(p);
                  WITH p^ DO
                    BEGIN offset := addr; id := name; next := prolist END;
                  prolist := p;
                END;
            END { textfile processing };
        END { WITH progParam^ DO };
      enterId(progparam);
    END { processProgParam };
    
  BEGIN { programme }
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    indent := indent + 1;
    writeln(' ':indent,'>programme');
  END;
{$endif}
    IF sy = progsy THEN
      BEGIN
        insymbol;
        IF sy = ident THEN
          BEGIN
            putstrs(idll); { program name has no significance in program }
            insymbol       { so get rid of it }
          END
        ELSE
          error(2);
        lc := lcaftermarkstack + 4 + ptrsize; { leave space for the required }
             { textfiles input & output (2 bytes apiece) & for commandline ptr }
        IF sy = lparen  THEN
          BEGIN
            i := 0;
            REPEAT
              insymbol; { to get rid of lparen, and then any commas }
              IF sy = ident THEN { but don't ignore program parameter names }
                BEGIN            { they are the gateway to the external world }
                  processProgParam;
                  insymbol;
                END
              ELSE
                error(2)
            UNTIL sy <> comma;
            IF sy = rparen THEN
              insymbol
            ELSE
              BEGIN
                error(18);
                skip(fsys + blockbegsys + [comma,rparen,semicolon])
              END;
          END;
        IF sy = semicolon THEN
          insymbol
        ELSE
          BEGIN error(14); skip(fsys + blockbegsys + [semicolon]) END;
      END
    ELSE
      error(3);
    block(fsys, period, NIL { <-- how block knows it's programme calling} );
    IF sy <> period THEN error(21);
    IF errinx > 0 THEN   (*output error messages*)
      BEGIN IF listing THEN writeln; errMessage END;
{$ifdef __GPC__}
IF trace THEN
  BEGIN
    writeln(' ':indent,'<programme');
    indent := indent - 1;
  END;
{$endif}
  END (* programme *);

  PROCEDURE stdnames;
  BEGIN
    { 'mark' and 'release' were removed and replaced with placeholders }
   na[ 1] := 'false       '; na[ 2] := 'true        '; na[ 3] := '---         ';
   na[ 4] := '---         '; na[ 5] := 'get         '; na[ 6] := 'put         ';
   na[ 7] := 'reset       '; na[ 8] := 'rewrite     '; na[ 9] := 'read        ';
   na[10] := 'write       '; na[11] := 'pack        '; na[12] := 'unpack      ';
   na[13] := 'new         '; na[14] := '---         '; na[15] := 'readln      ';
   na[16] := 'writeln     '; na[17] := 'abs         '; na[18] := 'sqr         ';
   na[19] := 'trunc       '; na[20] := 'odd         '; na[21] := 'ord         ';
   na[22] := 'chr         '; na[23] := 'pred        '; na[24] := 'succ        ';
   na[25] := 'eof         '; na[26] := 'eoln        '; na[27] := 'sin         ';
   na[28] := 'cos         '; na[29] := 'exp         '; na[30] := 'sqrt        ';
   na[31] := 'ln          '; na[32] := 'arctan      '; na[33] := '---         ';
   na[34] := '---         '; na[35] := '---         '; na[36] := 'maxint      ';
   na[37] := 'round       '; na[38] := 'page        '; na[39] := 'dispose     ';
  END (*stdnames*) ;

  PROCEDURE enterstdtypes;
  BEGIN                                                 (*type underlying:*)
    getstc(intptr);                                       (*integer*)
    WITH intptr^ DO
      BEGIN size := intsize; form := scalar; scalkind := predclrd END;
    getstc(realptr);                                      (*real*)
    WITH realptr^ DO
      BEGIN size := realsize; form := scalar; scalkind := predclrd END;
    getstc(charptr);                                      (*char*)
    WITH charptr^ DO
      BEGIN size := charsize; form := scalar; scalkind := predclrd END;
    { boolptr: 'expdclrd' tag is deliberate: this is so boolptr can use }
    { the 'fenum' field to link in the constants 'false' and 'true'; }
    { this allows getbounds to report an fmax value of '1' for boolean }
    getstc(boolptr);                                      (*Boolean*)
    WITH boolptr^ DO
      BEGIN size := boolsize; form := scalar; scalkind := expdclrd END;
    getstc(nilptr);                                       (*nil*)
    WITH nilptr^ DO
      BEGIN size := ptrsize; form := pointer; eltype := NIL END;
    (*for alignment of parameters*)
    getstc(parmptr);
    WITH parmptr^ DO
      BEGIN size := parmsize; form := scalar; scalkind := predclrd END;
    getstc(textptr);                                       (*text*)
    WITH textptr^ DO
      BEGIN size := filesize+charsize; form := files; compType := charptr END;
  END (*enterstdtypes*) ;

  PROCEDURE entstdnames;
    VAR cp,cp1: sTptr; i: integer;
  BEGIN                                                        (*name:*)
    getid(cp);                                 (*integer*)
    WITH cp^ DO
      BEGIN strassvr(name, 'integer     '); idtype := intptr; klass := types END;
    enterId(cp);
    getid(cp);                                 (*real*)
    WITH cp^ DO
      BEGIN strassvr(name, 'real        '); idtype := realptr; klass := types END;
    enterId(cp);
    getid(cp);                                 (*char*)
    WITH cp^ DO
      BEGIN strassvr(name, 'char        '); idtype := charptr; klass := types END;
    enterId(cp);
    getid(cp);                                 (*Boolean*)
    WITH cp^ DO
      BEGIN strassvr(name, 'boolean     '); idtype := boolptr; klass := types END;
    enterId(cp);
    getid(cp);                                 (*text*)
    WITH cp^ DO
      BEGIN strassvr(name, 'text        '); idtype := textptr; klass := types END;
    enterId(cp);
    cp1 := NIL;
    FOR i := 1 TO 2 DO
      BEGIN
        getid(cp);                         (*false,true*)
        WITH cp^ DO
          BEGIN strassvr(name, na[i]); idtype := boolptr;
            next := cp1; klass := konst; values.ival := i - 1
          END;
        enterId(cp); cp1 := cp
      END;
    boolptr^.fenum := cp;
    { kind of a kludge to link 'false' and 'true' w/ boolptr }
    
    FOR i := 5 TO 16 DO IF i <> 14 { 'release' } THEN { pre-declared procs: }
      BEGIN
        getid(cp);                                             (*get,put,reset*)
        WITH cp^ DO                                            (*rewrite,read*)
          BEGIN
            strassvr(name, na[i]); idtype := NIL;              (*write,pack*)
            klass := proc; pfdeckind := predclrd;              (*readln,writeln*)
            params := NIL; key := i - 4;                       (*unpack,new*)
          END;
        enterId(cp)
      END;
      
    FOR i := 17 TO 26 DO { predeclared funcs }
      BEGIN
        getid(cp);                                            (*abs,sqr,trunc*)
        WITH cp^ DO                                            (*odd,ord,chr*)
          BEGIN
            strassvr(name, na[i]); idtype := NIL;          (*pred,succ,eof*)
            klass := func; pfdeckind := predclrd;
            params := NIL; key := i - 16;
          END;
        enterId(cp)
      END;
      
    FOR i := 27 TO 32 DO
      BEGIN
        getid(cp);       (* dummy parameter for predeclared math functions*)
        WITH cp^ DO
          BEGIN
            strassvr(name, '            '); idtype := realptr; addr := 0;
            klass := vars; vkind := actual; vlev := 1; vbound := false;
          END;
          
        getid(cp1);                                           (*sin,cos,exp*)
        WITH cp1^ DO                                           (*sqrt,ln,arctan*)
          BEGIN
            strassvr(name, na[i]); idtype := realptr;
            klass := func; params := cp; addr := 0;
            pfdeckind := expdclrd; pflev := 0; pfname := i - 12;
            pfkind := actual; forwdecl := false; externl := true;
          END;
        enterId(cp1)
      END;
      
    getid(cp);                                                (*maxint*)
    WITH cp^ DO
      BEGIN
        strassvr(name, na[36]); idtype := intptr;
        klass := konst; values.ival := maxint
      END;
    enterId(cp);
      
    getid(cp);                                                (*round*)
    WITH cp^ DO
      BEGIN
        strassvr(name, na[37]); idtype := NIL;
        klass := func; pfdeckind := predclrd;
        params := NIL; key := 16;
      END;
    enterId(cp);
      
    getid(cp);                                                (*page*)
    WITH cp^ DO
      BEGIN
        strassvr(name, na[38]); idtype := NIL;
        klass := proc; pfdeckind := predclrd;
        params := NIL; key := 17;
      END;
    enterId(cp);
      
    getid(cp);                                                (*dispose*)
    WITH cp^ DO
      BEGIN
        strassvr(name, na[39]); idtype := NIL;
        klass := proc; pfdeckind := predclrd;
        params := NIL; key := 18;
      END;
    enterId(cp)
  END (* entstdnames *) ;

  PROCEDURE enterundecl;
  BEGIN
    getid(utypptr);
    WITH utypptr^ DO
      BEGIN strassvr(name, '            '); idtype := NIL; klass := types END;
    getid(ucstptr);
    WITH ucstptr^ DO
      BEGIN strassvr(name, '            '); idtype := NIL;
        klass := konst; values.ival := 0
      END;
    getid(uvarptr);
    WITH uvarptr^ DO
      BEGIN
        strassvr(name, '            '); idtype := NIL; addr := 0;
        klass := vars; vkind := actual; vlev := 0;
        vbound := false; readonly := false;
      END;
    getid(ufldptr);
    WITH ufldptr^ DO
      BEGIN
        strassvr(name, '            '); idtype := NIL; addr := 0;
        klass := field
      END;
    getid(uprcptr);
    WITH uprcptr^ DO
      BEGIN
        strassvr(name, '            '); idtype := NIL; addr := 0;
        klass := proc; params := NIL;
        pfdeckind := expdclrd; pflev := 0; genlabel(pfname);
        pfkind := actual; forwdecl := false; externl := false;
      END;
    getid(ufctptr);
    WITH ufctptr^ DO
      BEGIN
        strassvr(name, '            '); idtype := NIL; addr := 0;
        klass := func; params := NIL;
        pfdeckind := expdclrd; pflev := 0; genlabel(pfname);
        pfkind := actual; forwdecl := false; externl := false;
      END
  END (*enterundecl*) ;

  PROCEDURE initscalars;
    VAR   i: integer;
  BEGIN
{$ifdef __GPC__}
    condCompLev := 0;
    condComp[condCompLev].mode := parsing; { we start out parsing }
{$endif}
    idll := NIL; { either points to something, or is NIL, BUT NOT UNDEFINED }
    fwptr := NIL;
    eolseen := false; { mechanism to get position of error more accurate }
    listing := true; debug := true;
    errinx := 0;
    deadcode := false;
    intlabel := 0;
    (* note in the above reservation of buffer store for 2 text files *)
    linecount := 0;
    ch := ' '; chcnt := 0;
    mxint10 := maxint DIV 10;
    inputptr := NIL;
    outputptr := NIL;  { THIS is how to tell if they were in the header }
    FOR i := 1 TO maxErrNo DO
      errtbl[i].didit := false; { initialize error tracking }
    totErr := 0; totWarn := 0;
    { set up (empty) free lists for things that get new'd }
    strfl := NIL;
    reelcstfl := NIL; strgcstfl := NIL; psetcstfl := NIL;
    stcfl := NIL;
    idfl := NIL;
    lbpfl := NIL;
    cipfl := NIL;
    cmndLine := false;
    prolist := NIL;
{$ifdef __GPC__}
    trace := false;
    indent := 0;
{$endif}
  END (*initscalars*) ;

  PROCEDURE initsets;
  BEGIN
    letter := ['A','B','C','D','E','F','G','H','I','J','K','L','M',
               'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
               'a','b','c','d','e','f','g','h','i','j','k','l','m',
               'n','o','p','q','r','s','t','u','v','w','x','y','z'];
    digit  := ['0','1','2','3','4','5','6','7','8','9'];
    letordig := letter + digit + ['_'];
    mulops := [imul,rdiv,idiv,imod,andop];
    addops := [plus,minus,orop];
    relops := [ltop,leop,geop,gtop,neop,eqop,inop];
    load1wordops := [lag, lal, loca, logb, logw, lolb, lolw, locw];
  
    constbegsys := [plus,minus,intconst,realconst,strngconst,ident];
    simptypebegsys := [lparen] + constbegsys;
    typedels := [arraysy,recordsy,setsy,filesy];
    typebegsys := [arrow,packedsy,typesy] + typedels + simptypebegsys;
    blockbegsys := [labelsy,constsy,typesy,varsy,procsy,funcsy,beginsy];
    parambegsys := [ident,varsy,procsy,funcsy ,constsy { error(30) } ];
    selectsys := [arrow,period,lbrack];
    facbegsys := [intconst,realconst,strngconst,ident,lparen,lbrack,notop,nilsy];
    exprsys := facbegsys + mulops + addops + relops + selectsys + [rparen,rbrack];
    statbegsys := [beginsy,gotosy,ifsy,whilesy,repeatsy,forsy,withsy,casesy];
  END (*initsets*) ;

  PROCEDURE inittables;
    VAR   ch: char;

    PROCEDURE reswords;
    BEGIN
      hash['a'] := 15; hash['b'] := 14; hash['c'] :=  2; hash['d'] :=  0;
      hash['e'] :=  0; hash['f'] := 15; hash['g'] := 16; hash['h'] :=  6;
      hash['i'] := 14; hash['j'] :=  0; hash['k'] :=  0; hash['l'] := 15;
      hash['m'] := 14; hash['n'] := 10; hash['o'] :=  7; hash['p'] := 16;
      hash['q'] :=  0; hash['r'] := 14; hash['s'] :=  7; hash['t'] :=  1;
      hash['u'] := 14; hash['v'] := 13; hash['w'] :=  2; hash['x'] :=  0;
      hash['y'] := 16; hash['z'] :=  0;
      
  rw[ 3] := 'end         '; rw[ 4] := 'else        '; rw[ 5] := 'type        ';
  rw[ 6] := 'case        '; rw[ 7] := 'while       '; rw[ 8] := 'const       ';
  rw[ 9] := 'do          '; rw[10] := 'to          '; rw[11] := 'set         ';
  rw[12] := 'with        '; rw[13] := 'downto      '; rw[14] := 'not         ';
  rw[15] := 'then        '; rw[16] := 'div         '; rw[17] := 'mod         ';
  rw[18] := 'and         '; rw[19] := 'file        '; rw[20] := 'record      ';
  rw[21] := 'repeat      '; rw[22] := 'packed      '; rw[23] := 'or          ';
  rw[24] := 'of          '; rw[25] := 'procedure   '; rw[26] := 'in          ';
  rw[27] := 'goto        '; rw[28] := 'nil         '; rw[29] := 'begin       ';
  rw[30] := 'var         '; rw[31] := 'if          '; rw[32] := 'for         ';
  rw[33] := 'function    '; rw[34] := 'until       '; rw[35] := 'label       ';
  rw[36] := 'array       '; rw[37] := 'program     ';
  
  rw[38] := 'elif        '; rw[39] := 'endif       '; rw[40] := 'ifdef       ';
  rw[41] := 'error       '; rw[42] := 'define      '; rw[43] := 'defined     ';
    END (*reswords*) ;

    PROCEDURE symbols;
    BEGIN
      rsy[ 3] := endsy;     rsy[ 4] := elsesy;    rsy[ 5] := typesy;
      rsy[ 6] := casesy;    rsy[ 7] := whilesy;   rsy[ 8] := constsy;
      rsy[ 9] := dosy;      rsy[10] := tosy;      rsy[11] := setsy;
      rsy[12] := withsy;    rsy[13] := downtosy;  rsy[14] := notop;
      rsy[15] := thensy;    rsy[16] := idiv;      rsy[17] := imod;
      rsy[18] := andop;     rsy[19] := filesy;    rsy[20] := recordsy;
      rsy[21] := repeatsy;  rsy[22] := packedsy;  rsy[23] := orop;
      rsy[24] := ofsy;      rsy[25] := procsy;    rsy[26] := inop;
      rsy[27] := gotosy;    rsy[28] := nilsy;     rsy[29] := beginsy;
      rsy[30] := varsy;     rsy[31] := ifsy;      rsy[32] := forsy;
      rsy[33] := funcsy;    rsy[34] := untilsy;   rsy[35] := labelsy;
      rsy[36] := arraysy;   rsy[37] := progsy;
      
      { token names, for error messages }
      { "But I want YOU to give me the messAGE" --Inspector Clouseau }
      tn[imul]      := '*           '; tn[rdiv]       := '/           ';
      tn[idiv]      := '"DIV"       '; tn[imod]       := 'MOD         ';
      tn[andop]     := '"AND"       '; tn[plus]       := 'plus sign   ';
      tn[minus]     := 'minus sign  '; tn[orop]       := 'OR          ';
      tn[ltop]      := '<           '; tn[leop]       := '<=          ';
      tn[geop]      := '>=          '; tn[gtop]       := '>           ';
      tn[neop]      := '<>          '; tn[eqop]       := '=           ';
      tn[inop]      := '"IN"        '; tn[ident]      := 'identifier  ';
      tn[intconst]  := 'integer     '; tn[realconst]  := 'real number ';
      tn[strngconst]:= 'string      '; tn[notop]      := '"NOT"       ';
      tn[lparen]    := 'left paren  '; tn[rparen]     := 'right paren ';
      tn[lbrack]    := 'left bracket'; tn[rbrack]     := 'right brack ';
      tn[comma]     := 'comma       '; tn[semicolon]  := 'semicolon   ';
      tn[period]    := 'period      '; tn[arrow]      := 'arrow/caret ';
      tn[colon]     := 'colon       '; tn[becomes]    := ':=          ';
      tn[range]     := '..          '; tn[labelsy]    := 'LABEL       ';
      tn[constsy]   := 'CONST       '; tn[typesy]     := 'TYPE        ';
      tn[varsy]     := 'VAR         '; tn[funcsy]     := 'FUNCTION    ';
      tn[progsy]    := 'PROGRAM     '; tn[procsy]     := 'PROCEDURE   ';
      tn[setsy]     := 'SET         '; tn[packedsy]   := 'PACKED      ';
      tn[arraysy]   := 'ARRAY       '; tn[recordsy]   := 'RECORD      ';
      tn[filesy]    := 'FILE        '; tn[beginsy]    := 'BEGIN       ';
      tn[ifsy]      := 'IF          '; tn[casesy]     := 'CASE        ';
      tn[repeatsy]  := 'REPEAT      '; tn[whilesy]    := 'WHILE       ';
      tn[forsy]     := 'FOR         '; tn[withsy]     := 'WITH        ';
      tn[gotosy]    := 'GOTO (tsk!) '; tn[endsy]      := 'END         ';
      tn[elsesy]    := 'ELSE        '; tn[untilsy]    := 'UNTIL       ';
      tn[ofsy]      := 'OF          '; tn[dosy]       := 'DO          ';
      tn[tosy]      := 'TO          '; tn[downtosy]   := 'DOWNTO      ';
      tn[thensy]    := 'THEN        '; tn[nilsy]      := 'NIL         ';
      tn[cmtEndsy]  := '*) or }     '; tn[illegalsy]  := 'illegal char';
    END;

    PROCEDURE rators;
    BEGIN
      commute[imul]  := true;  commute[rdiv] := false;
      commute[andop] := true;  commute[idiv] := false;
      commute[imod]  := false; commute[plus] := true;
      commute[minus] := false; commute[orop] := true;
      
      comRel[equ] := equ; comRel[neq] := neq; comRel[leq] := geq;
      comRel[geq] := leq; comRel[les] := gtr; comRel[gtr] := les;
      
  relopinstr1[ltop] := les;  relopinstr1[leop] := leq;  relopinstr1[geop] := geq;
  relopinstr1[gtop] := gtr;  relopinstr1[neop] := neq;  relopinstr1[eqop] := equ;
  
  relopinstr2[ltop] := lesm; relopinstr2[leop] := leqm; relopinstr2[geop] := geqm;
  relopinstr2[gtop] := gtrm; relopinstr2[neop] := neqm; relopinstr2[eqop] := equm;
      
      corLoad[stgb] := logb; corLoad[stgw] := logw; corLoad[stgd] := logd;
      corLoad[stlb] := lolb; corLoad[stlw] := lolw; corLoad[stld] := lold;
      corLoad[stib] := loib; corLoad[stiw] := loiw; corLoad[stid] := loid;
    END (*rators*) ;

    PROCEDURE procmnemonics;
    BEGIN
      { There are two mnemonics that have no counterpart in the
        assembler/interpreter: wro, pak. I didn't find a generator for them,
        and suspect they are abandoned. [sam]}
      { Same with rst and sav. 'Read string' (rds) added. [rs]}
      sna[ 1] :=' get'; sna[ 2] :=' put'; sna[ 3] :=' rdi'; sna[ 4] :=' rdr';
      sna[ 5] :=' rdc'; sna[ 6] :=' wri'; sna[ 7] :=' rds'; sna[ 8] :=' wrr';
      sna[ 9] :=' wrc'; sna[10] :=' wrs'; sna[11] :=' ---'; sna[12] :=' new';
      sna[13] :=' ---'; sna[14] :=' eln'; sna[15] :=' sin'; sna[16] :=' cos';
      sna[17] :=' exp'; sna[18] :=' sqt'; sna[19] :=' log'; sna[20] :=' atn';
      sna[21] :=' rln'; sna[22] :=' wln'; sna[23] :=' ---';
      { new procedure/function memonics for P5 }
      sna[24] :=' pag'; sna[25] :=' rsf'; sna[26] :=' rwf'; sna[27] :=' wrb';
      sna[28] :=' wrf'; sna[29] :=' dsp'; sna[30] :=' wbf'; sna[31] :=' wbi';
      sna[32] :=' wbr'; sna[33] :=' wbc'; sna[34] :=' wbb'; sna[35] :=' rbf';
      sna[36] :=' rsb'; sna[37] :=' rwb'; sna[38] :=' gbf'; sna[39] :=' pbf';

    END (*procmnemonics*) ;

    PROCEDURE instrmnemonics;
    BEGIN
      mn[logb]:='logb'; mn[logw]:='logw'; mn[logd]:='logd'; mn[logm]:='logm';
      mn[lolb]:='lolb'; mn[lolw]:='lolw'; mn[lold]:='lold'; mn[lolm]:='lolm';
      mn[loib]:='loib'; mn[loiw]:='loiw'; mn[loid]:='loid'; mn[loim]:='loim';
      mn[locw]:='locw'; mn[locr]:='locr'; mn[locs]:='locs'; mn[loca]:='loca';
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
      mn[les ]:='les '; mn[gtr ]:='gtr '; mn[inn ]:='inn '; mn[dif ]:='dif ';
      mn[equm]:='equm'; mn[neqm]:='neqm'; mn[leqm]:='leqm'; mn[geqm]:='geqm';
      mn[lesm]:='lesm'; mn[gtrm]:='gtrm'; mn[sst1]:='sst1'; mn[sst2]:='sst2';
      mn[tjp ]:='tjp '; mn[ujc ]:='ujc '; mn[xjp ]:='xjp '; mn[sjp ]:='sjp ';
      mn[cas ]:='cas '; mn[chk0]:='chk0'; mn[chk1]:='chk1'; mn[chk2]:='chk2';
      mn[lag ]:='lag '; mn[lal ]:='lal '; mn[lpa ]:='lpa '; mn[lip ]:='lip ';
      mn[ixa ]:='ixa '; mn[ixca]:='ixca'; mn[iodd]:='iodd'; mn[fjp ]:='fjp ';
      mn[eofp]:='eofp'; mn[sgs ]:='sgs '; mn[rgs ]:='rgs '; mn[bra ]:='bra ';
      mn[ipj ]:='ipj '; mn[dmp ]:='dmp '; mn[dup ]:='dup '; mn[swp ]:='swp ';
      mn[cal ]:='cal '; mn[cali]:='cali'; mn[csp ]:='csp '; mn[mst ]:='mst ';
      mn[ent ]:='ent '; mn[ret0]:='ret0'; mn[ret1]:='ret1'; mn[ret2]:='ret2';
      mn[mov ]:='mov '; mn[copy]:='copy'; mn[fbv ]:='fbv '; mn[fvb ]:='fvb ';
      mn[efb ]:='efb '; mn[pack]:='pack'; mn[unpk]:='unpk'; mn[pop0]:='pop0';
      mn[cmln]:='cmln'; mn[asgn]:='asgn'; mn[srcl]:='srcl'; mn[fatl]:='fatl';
      mn[stop]:='stop';
    END (*instrmnemonics*) ;

  BEGIN (*inittables*)
    reswords; symbols; rators;
    instrmnemonics; procmnemonics;
    { initialize the lcase array }
    FOR ch := chr(0) TO chr(255) DO
      IF ch IN ['A'..'Z'] THEN
        lcase[ch] := chr(ord(ch)+(ord('a')-ord('A')))
      ELSE
        lcase[ch] := ch;
  END (*inittables*) ;

  PROCEDURE errorAccounting;
    VAR   i: errnoType; { index for error number tracking array }
  BEGIN
    writeln;
    write('Fatal errors: ', totErr);
    IF totWarn > 0 THEN
      write(' & warnings: ',totWarn);
    writeln;
    { output error report as required }
    IF totErr + totWarn > 0 THEN
      BEGIN
        writeln;
        writeln('Error numbers in listing:');
        writeln('-------------------------');
      END;
    FOR i := 1 TO maxErrNo DO
      IF errtbl[i].didit THEN
        BEGIN
          write(i:3, '  ');
          sy := errtbl[i].sym;
          errmsg(i);
          writeln
        END;
  END; { errorAccounting }

BEGIN { pascalcompiler main block }

{$ifdef __GPC__}
  IF ParamCount >= 1 THEN
    assign(p6, ParamStr(1)) { gpc doesn't bind p6 in the program header }
  ELSE                       { w/ external textfile like P6 does }
    BEGIN
      writeln('Output file should be the first command-line filename.');
      halt(1);
    END;
{$endif}

  write('Pascal P6');
{$ifdef __GPC__}
  write('a');
{$endif}
  writeln(' compiler v', majorver:1, '.', minorver:1);
  writeln;
  
  rewrite(p6);  { p6 in the program header does an "assign" to "p6.txt"; }
                { textfiles listed in the header can be reset or rewritten. }
                { This is my compiler, and that's the rule. }

  (*initialize*)
  (************)
  initscalars; initsets; inittables;
  
  (*enter pre-declared names and pre-declared types:*)
  (**************************************************)
  level := 0; top := 0;
  WITH display[0] DO
    BEGIN fname := NIL; flabel := NIL; fconst := NIL; ftype := NIL;
          forwProcFunc := NIL; occur := blck; bname := NIL END;
  enterstdtypes; stdnames; entstdnames; enterundecl;
  level := 1; top := 1;
  WITH display[1] DO
    BEGIN fname := NIL; flabel := NIL; fconst := NIL; ftype := NIL;
          forwProcFunc := NIL; occur := blck; bname := NIL END;

  writeln(p6, '!');
  write(p6, '! Pascal intermediate file Generated by Pascal P6');
{$ifdef __GPC__}
  write(p6, 'a');
{$endif}
  writeln(p6, ' compiler v', majorver:1, '.', minorver:1);
  writeln(p6, '!');
  endofline;
  insymbol;
  programme([period]);
  98:
  errorAccounting;

  99:
{$ifdef __GPC__}
  halt(totErr);
{$endif}

END.
