#!/bin/csh -fb
#
# Unix shell script to check correct installation of AMR
# speech encoder and decoder
#
# $Id $

if ("$1" == "-vad2") then
    set vad=2;
    shift;
else if ("$1" == "-vad1") then
    set vad=1;
    shift;
else
    set vad=1;
endif

if ("$1" == "unix") then
    set BASEin=spch_unx;
    if ($vad == 1) then
        set BASEout = $BASEin;
    else
        set BASEout = spch_un2;
    endif
else if ("$1" == "dos") then
    set BASEin = spch_dos;
    if ($vad == 1) then
        set BASEout = $BASEin;
    else
        set BASEout = spch_do2;
    endif
else
    echo "Use:    $0 [-vad2] dos"
    echo "  or    $0 [-vad2] unix"
    exit -1;
endif
    
./encoder -dtx -modefile=allmodes.txt $BASEin.inp tmp.cod
echo ""
cmp tmp.cod $BASEout.cod
if ($status == 0) then
    echo "##################################################"
    echo "# AMR encoder executable installation successful #"
    echo "##################################################"
else
    echo "#########################################################"
    echo "# \!\!\! ERROR in AMR encoder installation verification \!\!\!#"
    echo "#########################################################"
    exit -1
endif

./decoder $BASEout.cod tmp.out
echo ""
cmp tmp.out $BASEout.out
if ($status == 0) then
    echo "##################################################"
    echo "# AMR decoder executable installation successful #"
    echo "##################################################"
else
    echo "#########################################################"
    echo "# \!\!\! ERROR in AMR decoder installation verification \!\!\!#"
    echo "#########################################################"
    exit -1
endif
