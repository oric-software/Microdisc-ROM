@echo off


SET ORICUTRON="..\..\..\..\oricutron\"


rem SET RELEASE="30"
rem SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%



rem cd src
%OSDK%\bin\xa.exe microdisc.asm

md5sums a.o65
md5sums MICRODIS.ROM

rem IF "
rem cd %ORICUTRON%
rem oricutronV4 -mt -d teledisks\stratsed.dsk --symbols "%ORIGIN_PATH%\src\xa_labels.txt"
:End
cd %ORIGIN_PATH%