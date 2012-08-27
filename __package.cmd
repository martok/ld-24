@echo off
if exist __output (deltree /Y __output)
if not exist __output (mkdir __output)

mkdir __output\textures
mkdir __output\sounds
mkdir __output\maps


copy textures __output\textures\
copy sounds __output\sounds\
copy maps __output\maps\

copy *.exe __output\
copy *.dll __output\
copy *.glsl __output\