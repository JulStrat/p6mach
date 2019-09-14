This readme.txt file is for the purpose of explaining the contents of the
downloaded P6.zip file.

pcom.pas

pcom.pas is an expanded source of the Pascal P6 compiler, with conditionally-
compiled sections. It is not an ISO-compliant program, but it can be compiled by 
the GNU Pascal compiler (gpc.exe, not included). A stripped-down version of the
pcom.pas source (without the conditional-compile sections), p6com.pas, is ISO-
compliant, but, ironically, the GNU compiler cannot compile p6com.pas.

The reason for this is that the GNU Pascal compiler is not quite ISO-compliant
in the handling of identifiers in the program parameter-list, and so requires an
extension, the procedure "assign" to bind to an external file. So an inter-
mediate version of Pascal P6 must first be created which implements all features
of the ISO Standard, including the processing of the program parameter list, and
having the one extension of recognizing the "special" comments that use the
dollar-sign-delimited conditional-compile syntax.

So the first compilation by gpc of pcom.pas produces a Pascal "P6a" compiler
which understands ISO-compliant program headers, but also interprets "{$...}"
constructs as being GNU-oriented conditional-compile sections, which implement
GNU extensions within a special sandwich of Pascal comments. This intermediate
compiler (supplied in the downloaded P6.zip as "pcom.exe") is not quite ISO-
compliant (for the reasons stated above), but it is able to compile itself using
the pcom.pas source. If the provided batch file is used, then the P-code file
from the first compilation will be called "p6a.txt".

If one were to do a second compile, running p6a.txt on the interpreter, and
using pcom.pas again as the source input, it would produce a second P-code file,
this time called "p6.txt". Running "diff" on pcom.p6 and p6a.txt will show them
to be identical, verifying that the self-compilation is successful. But self.bat
doesn't do that. Instead it goes directly to the final step, below.

So now the final step is to produce a version of Pascal P6 that doesn't have the
conditional-compile extension implemented. This is accomplished by doing the
second compile as described above, except that the input source is p6com.pas,
which is pcom.pas with all the conditional-compile sections stripped out (which
includes all the code which (conditionally) compiles those sections. The
resulting P-code file, p6.txt, is the "real-McCoy" Pascal P6 compiler that is an
ISO-compliant program that compiles itself (using the p6com.pas source).

The batch file, self.bat, then renames the p6.txt intermediate file to
"prr.txt", and then proceeds to do a self-compile of p6com.pas (the "real-McCoy"
Pascal P6 source, produced by "strip.exe" from pcom.pas), on the interpreter
using the renamed prr.txt from the "final" step above, with "time" called to
show how fast it is. Last, but not least, a call to the utility "diff" shows
that prr.txt prr.txt and the freshly-created p6.txt are identical.

pcom.exe

pcom.exe (provided) is the executable produced by the GNU Pascal compiler (not
provided), operating under the dev-gnu IDE (also not provided), compiling
pcom.pas, the expanded source of the Pascal P6 compiler.

This executable, pcom.exe, is an intermediate version of the Pascal P6 compiler,
which implements all of ISO 7185, level 1, correctly, but also implements the
non-Standard conditional-compile facility. It announces itself as "P6a".

As already mentioned, the only implementation-defined feature that Pascal P6
relies on is declaring "prr" in the program header, and causing "rewrite(prr)"
to open the external textfile " prr.txt" for output.

pint.pas

This is an Pascal P6 assembler/interpreter for the intermediate code produced
by the Pascal P6 compiler. It is written in a dialect of Pascal that the GNU
Pascal compiler can compile. The executable produced by GPC is "pint.exe". One
of the extensions used by pint.pas is to read the command line. The first
argument on the command line is the name of the p-code file that pint.exe is
to assemble and then interpret. When that file is "p6.txt" (produced by the
gpc-compiled compiler) or "prr.txt" (produced by the P6 self-compiled
compiler), pint becomes a Pascal P6 (slightly non-Standard) compiler with the
conditional-compile facility ("p6.txt"), or else a (fully compliant, real
McCoy") Pascal P6 compiler without that facility ("prr.txt").

pint.exe

pint.exe (provided) is the executable produced by the GNU Pascal compiler (not
provided), operating under the dev-gnu IDE (also not provided), compiling
pint.pas, the (slightly non-standard) assembler/interpreter source.

self.bat

A sample batch file that compiles pcom.pas to p6a.txt, which is an intermediate
Pascal P6 compiler, "P6a", which then runs on the interpreter, pint, and
compiles p6com.pas to "p6.txt", the real-McCoy Pascal P6. It then performs the
compile of p6com.pas again to "p6.txt", with "time" called before and after, to
show how fast P6 compiles itself on an interpreter. The common utility, diff,
is then called to show that the self-compile worked.

strip.exe

A Free Pascal executable program that strips the conditional-compile sections
out of pcom.pas, yielding p6com.pas.

diff.exe

Admittedly, this is overkill for indicating that two files are identical. But
this is a hugely useful utility, which originated in Unix. This version is
targeted as a Windows console app, and is copylefted by the FSF. The source is
available from the GNU project. The self.bat batch file uses it.

p4-p5.html

At first, I added my own comments to the beginning of pcom.pas and pint.pas,
after Scott's. Then I thought, "Oops, this is getting a little too long." So I
took this commentary out of the .pas files and put them in this .html file.
This is an important piece of history which should always accompany the P
sources. Even though these sources and comments are "public domain", this
history should not get separated from the compiler sources in any re-
distribution. OK?

p5-p6.html

And this is where I put my own commentary, detailing the changes that I made to
the P5 sources.

manual.html

This is a concise manual for the P6 system, with two large appendices
documenting the overhauled P-codes, and the conformant array parameter
implementation. This is the "official" documentation accompanying the ISO
Standard-compliant processor, which details error-handling and processor-
defined and processor-dependent behaviors.

waiver.html

This is my explicit placing of the P6 sources (and certain accompanying files)
into the Public Domain.

readme.txt

This file.

P6-v2.html

Things I have in mind for a follow-up release, v2.0.

wd.html

A look at a Wirth-Dijkstra language proposal, which would implement Dijkstra's
notation in a wrapper provided by Wirth's latest Oberon language. This is the
reason I started the P6 project, as a stepping stone to get to "WD".

example.html

An coding example taken from Dijkstra's book "A Disciple of Programming". To
understand the syntax and semantics of his notation, read the book.

UPDATE HISTORY
==============

1.  This is the first update to the originally published p6.zip. The files
    diff.exe & example.html were added to the zip file, and this file,
    readme.txt, was updated.  --MARCH 29, 2017