
 Engine Summary (as of Nov 2025)
 
 -   Written in COBOL-85 (GNUCOBOL) and COBOL-74 (TK4-)
 -   Two data files per adventure
     
     -   `rooms.dat` (128 bytes) first record with `ROOM-NAME = 'TITLE'` sets game title
     -   `objects.dat` (128 bytes) includes `OBJ-REQD-OBJ`, `OBJ-USE-ROOM`, `OBJ-IS-WIN`
     
 -   Win logic embedded in object records — no `sol.dat`
 -   CLIST (`RUNADV`) enables READY mode play on TK4-
 -   PowerShell tools (`dat2txt.ps1`, `txt2dat.ps1`) for easy editing
 -   Four adventures `forest`, `tower`, `temple`, `study`