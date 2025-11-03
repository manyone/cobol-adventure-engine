
### On TK4- / Hercules (OS/360 MVT, COBOL-74)

1. **Transfer data files using IND$FILE** from your PC to TK4-:  
   - In the IND$FILE transfer dialog, set **File Type = `FIXED`**  
   - Specify the **exact record length**:  
     - `rooms.txt` → `'USER.FOREST,ROOMS.DAT'` **(FIXED 128)**  
     - `objects.txt` → `'USER.FOREST,OBJECTS.DAT'` **(FIXED 128)**  
   > 💡 Do this **once per dataset** — the LRECL is stored in the catalog.

2. The engine reads `ROOMS` and `OBJECTS` as fixed-length sequential files.

3. Use the included **CLIST** for pseudo-interactive play in **READY mode**:

```clist
/* USER.RUN.CLIST(RUNADV) */
PROC 1
PROC 1 GAME                                               
ALLOC FILE(ROOMS) DA('USER.&GAME..ROOMS.DAT') SHR       
ALLOC FILE(OBJECTS) DA('USER.&GAME..OBJECTS.DAT') SHR   
ALLOC FILE(SYSOUT) DA(*)                                  
ALLOC FILE(SYSIN) DA(*)                                   
CALL 'USER.RUN.LOAD(ADVENT)'                            
```

Run in MVS READY mode:

`EX 'USER.RUN.CLIST(RUNADV)' 'FOREST'` or  `EX RUN(RUNADV) 'FOREST'`


> 💡 The CLIST pre-allocates files and invokes the load module directly, enabling a conversational feel despite OS/360’s batch nature.
