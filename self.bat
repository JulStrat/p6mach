@echo off
if exist "p6com.pas" goto alreadystripped
strip
:alreadystripped
pcom p6a.txt   < pcom.pas
pint p6a.txt   < p6com.pas
rename p6.txt prr.txt
echo:| time
pint prr.txt   < p6com.pas
echo:| time
diff -s prr.txt p6.txt
del prr.txt p6.txt
