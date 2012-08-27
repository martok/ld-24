@echo off
if exist __output (deltree /Y __output)
if not exist __output (mkdir __output)

mkdir __output\textures
mkdir __output\sounds
mkdir __output\maps


copy textures __output\textures\
copy sounds __output\sounds\
copy maps __output\maps\

copy ld24.exe __output\CityLife.exe
copy *.dll __output\
copy *.glsl __output\
copy README.md __output\