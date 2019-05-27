@echo off
title Multiple Iterations of PeakfqSA


for /R %%A IN (*.spc) DO (
	"C:\Batch\PeakfqSA_win.exe" "%%A"
) 
	 
pause


