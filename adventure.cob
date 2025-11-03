       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEXT-ADVENTURE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
           SELECT ROOM-FILE
               ASSIGN TO "rooms.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-ROOMS.
            SELECT OBJ-FILE
               ASSIGN TO "objects.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-OBJECT.    
       DATA DIVISION.
       FILE SECTION.
       
       FD  ROOM-FILE
              RECORD CONTAINS 128 CHARACTERS
              BLOCK CONTAINS 0 RECORDS
              RECORDING MODE IS F.

       01  ROOM-RECORD.
           03 FILLER PIC X(08).
           03 ROOM-REC-NAME PIC X(20).
           03 ROOM-REC-DESC PIC X(100).
       FD OBJ-FILE
              RECORD CONTAINS 128 CHARACTERS
              BLOCK CONTAINS 0 RECORDS
              RECORDING MODE IS F.
       
       01  OBJ-RECORD.
           03 FILLER PIC X(128).

       WORKING-STORAGE SECTION.
       01 EOF-FLAGS.
           03 FILLER PIC X(01) VALUE SPACE.
               88 NO-MORE-ROOM VALUE 'Y'.
           03 FILLER PIC X(01) VALUE SPACE.
               88 NO-MORE-OBJ VALUE 'Y'.

       01 WS-FS-ROOMS   PIC X(02).
       01 WS-FS-object  PIC X(02).

       01  PLAYER-STATE.
           05 CURRENT-ROOM         PIC 9 VALUE 1.
           05 GAME-OVER            PIC X VALUE 'N'.


       01  ROOM-DATA.
           05 ROOM-COUNT           PIC 99 VALUE 0.
           05 ROOM-REC             OCCURS 4 TIMES.
              10 EXIT-NEWS-GRP.
                15 EXIT-NEWS PIC 99 OCCURS 4.
              10 ROOM-NAME         PIC X(20).
              10 ROOM-DESC         PIC X(100).

       01  OBJECT-DATA.
           05 OBJ-COUNT            PIC 99 VALUE 0.
           05 OBJ-REC             OCCURS 3 TIMES.
              10 OBJ-NAME          PIC X(10).
              10 OBJ-LOCATION      PIC 99 VALUE 0.
              10 OBJ-PORTABLE      PIC X VALUE 'Y'.
              10 OBJ-TAKEN          PIC X VALUE 'N'.
              10 OBJ-USED          PIC X VALUE 'N'.
              10 OBJ-REQD-OBJ      PIC X(10).   
              10 OBJ-USE-ROOM      PIC 99.     
              10 OBJ-IS-WIN        PIC X.  
              10 OBJ-DESC          PIC X(40).
              10 OBJ-USE-MSG       PIC X(60).


       01  INPUT-LINE              PIC X(40).
       01  VERB                   PIC X(10).
       01  NOUN                   PIC X(10).
       01  I                      PIC 99 VALUE 0.
       01  J                      PIC 99 VALUE 0.
       01  K                      PIC 99 VALUE 0.
       01  FOUND                  PIC X VALUE 'N'.
       01  DIR-VAL               PIC 99 VALUE 0.
       01 INVT-COUNT               PIC 99 VALUE 0.
       01 REQD-OBJ-USED             PIC X(1) VALUE 'N'.
       01 REQD-OBJ-NAME            PIC X(10) VALUE SPACES.
       01 GAME-TITLE           PIC X(40) VALUE 'ADVENTURE GAME'.


       PROCEDURE DIVISION.
       MAIN-PARA.
      * -------------------------
      * LOAD TABLES
      * -------------------------
     

           OPEN INPUT ROOM-FILE OBJ-FILE 
      *     PERFORM INIT-DATA.
           MOVE 0 TO ROOM-COUNT
           PERFORM READ-ROOM
           IF ROOM-REC-NAME = 'TITLE'
                   MOVE ROOM-REC-DESC TO GAME-TITLE
                   PERFORM READ-ROOM
           END-IF
           PERFORM UNTIL NO-MORE-ROOM

              ADD 1 TO ROOM-COUNT
               MOVE ROOM-RECORD TO ROOM-REC (ROOM-COUNT)
               PERFORM READ-ROOM
               
           END-PERFORM

           MOVE 0 TO OBJ-COUNT
           PERFORM READ-OBJ
           PERFORM UNTIL NO-MORE-OBJ
                   ADD 1 TO OBJ-COUNT
               MOVE OBJ-RECORD TO OBJ-REC (OBJ-COUNT)
               PERFORM READ-OBJ
           END-PERFORM

           CLOSE ROOM-FILE     OBJ-FILE 

           DISPLAY 'WELCOME TO ' GAME-TITLE
           DISPLAY 'TYPE "QUIT" TO EXIT.'.
           DISPLAY SPACE.

           PERFORM SHOW-ROOM.

           PERFORM UNTIL GAME-OVER = 'Y'
              DISPLAY SPACE
              ACCEPT INPUT-LINE
              PERFORM PARSE-COMMAND
              PERFORM EXECUTE-COMMAND
           END-PERFORM.

           DISPLAY 'THANKS FOR PLAYING!'.
           STOP RUN.
       
       READ-ROOM.
           READ ROOM-FILE AT END SET NO-MORE-ROOM TO TRUE
           END-READ
           CONTINUE. 
       READ-OBJ.
           READ OBJ-FILE AT END SET NO-MORE-OBJ TO TRUE
           END-READ
           CONTINUE. 



      * -------------------------
      * DISPLAY ROOM
      * -------------------------
       SHOW-ROOM.
           DISPLAY ROOM-NAME(CURRENT-ROOM)
           DISPLAY ROOM-DESC(CURRENT-ROOM)
           PERFORM SHOW-OBJECTS.

       SHOW-OBJECTS.
           MOVE 'N' TO FOUND
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > OBJ-COUNT
              IF OBJ-LOCATION(I) = CURRENT-ROOM
                 DISPLAY OBJ-DESC(I)
                 MOVE 'Y' TO FOUND
              END-IF
           END-PERFORM
           IF FOUND = 'N'
              DISPLAY 'There is nothing special here.'
           END-IF.

      * -------------------------
       SHOW-INVENTORY.
           MOVE 0 TO INVT-COUNT.
           PERFORM SHOW-INVT VARYING I FROM 1 BY 1 UNTIL I> OBJ-COUNT.
           DISPLAY "--Your inventory count is " INVT-COUNT.
       SHOW-INVT.
           IF OBJ-TAKEN(I) = 'Y'
               DISPLAY OBJ-DESC(I)
               ADD 1 TO INVT-COUNT
           END-IF.


      * PARSE INPUT
      * -------------------------
       PARSE-COMMAND.
           UNSTRING INPUT-LINE DELIMITED BY SPACE
              INTO VERB NOUN.

      * -------------------------
      * EXECUTE COMMAND
      * -------------------------
       EXECUTE-COMMAND.
           EVALUATE VERB
               WHEN 'NORTH'
                 MOVE 1 TO DIR-VAL
                 PERFORM PROCESS-MOVE
               WHEN 'EAST'  
                 MOVE 2 TO  DIR-VAL
                 PERFORM PROCESS-MOVE     
               WHEN 'WEST'
                  MOVE 3 TO DIR-VAL
                 PERFORM PROCESS-MOVE   
               WHEN 'SOUTH'
                  MOVE 4 TO DIR-VAL
                 PERFORM PROCESS-MOVE    
               WHEN 'LOOK'
                 PERFORM SHOW-ROOM
               WHEN 'INVENTORY'
                 PERFORM SHOW-INVENTORY
               WHEN 'INVT'
                 PERFORM SHOW-INVENTORY
               WHEN 'TAKE'
                 PERFORM TAKE-OBJECT
               WHEN 'USE'
                 PERFORM USE-OBJECT
               WHEN 'QUIT'
                 MOVE 'Y' TO GAME-OVER
               WHEN OTHER
                 DISPLAY 'I don"t understand.'
           END-EVALUATE.

      * -------------------------
      * MOVEMENT
      * -------------------------
       PROCESS-MOVE.
           IF EXIT-NEWS(CURRENT-ROOM, DIR-VAL) > 0
              MOVE EXIT-NEWS(CURRENT-ROOM, DIR-VAL) TO CURRENT-ROOM
              PERFORM SHOW-ROOM
           ELSE
              DISPLAY 'You can"t go that way.'
           END-IF.


      * -------------------------
      * TAKE OBJECT
      * -------------------------
       TAKE-OBJECT.
           MOVE 'N' TO FOUND
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > OBJ-COUNT
              IF OBJ-NAME(I) = NOUN AND OBJ-LOCATION(I) = CURRENT-ROOM

                 IF OBJ-PORTABLE(I) = 'Y'
                    MOVE 0 TO OBJ-LOCATION(I)
                    MOVE 'Y' TO OBJ-TAKEN(I)
                    DISPLAY 'Taken.'
                    MOVE 'Y' TO FOUND
    
                 ELSE
                    DISPLAY 'You can"t take that.'
                    MOVE 'Y' TO FOUND
                 END-IF
               END-IF

           END-PERFORM
           IF FOUND = 'N'
              DISPLAY 'You don"t see that here.'
           END-IF.

      * -------------------------
      * USE OBJECT
      * -------------------------
       USE-OBJECT.
           PERFORM FIND-OBJECT.
           IF OBJ-REQD-OBJ(J) NOT = SPACES
               PERFORM CHECK-REQD-OBJ-USED
           END-IF

           IF FOUND NOT = 'Y'  
               DISPLAY 'No such object.'
           ELSE              
               IF NOT ( OBJ-LOCATION(J) = CURRENT-ROOM
                  OR OBJ-TAKEN(J) = 'Y')
                       DISPLAY 'That object isn"t available.' 
               ELSE
                   IF (OBJ-USE-ROOM(J) = CURRENT-ROOM OR 0)
                       PERFORM CHECK-REQD-OBJ
                   ELSE
                       IF OBJ-USE-ROOM(J) NOT = CURRENT-ROOM 
                           DISPLAY 'You"re in the Wrong room.'
                       ELSE
                           DISPLAY 'Nothing Happens'.                
 
       CHECK-REQD-OBJ.
            IF (OBJ-REQD-OBJ(J) = SPACES 
            OR REQD-OBJ-USED = 'Y') 
               MOVE 'Y' TO OBJ-USED(J)   
               PERFORM CHECK-WIN
           ELSE
                  DISPLAY 'Use this item first: ' 
                  OBJ-REQD-OBJ(J).
                                   
       CHECK-WIN.
               IF OBJ-IS-WIN(J) = 'Y'
                    DISPLAY OBJ-USE-MSG(J)
                    DISPLAY '!!! YOU WIN !!!'
                    MOVE 'Y' TO GAME-OVER
                ELSE
                     DISPLAY OBJ-USE-MSG(J).

       FIND-OBJECT.
           MOVE 'N' TO FOUND

           PERFORM DO-NOTHING VARYING J FROM 1 BY 1 
           UNTIL  J > OBJ-COUNT OR OBJ-NAME(J) = NOUN

           IF OBJ-NAME(J) = NOUN
             MOVE 'Y' TO FOUND
             MOVE OBJ-REQD-OBJ(J) TO REQD-OBJ-NAME
           END-IF
           CONTINUE.
       CHECK-REQD-OBJ-USED.
           MOVE 'N' TO REQD-OBJ-USED.
           PERFORM DO-NOTHING VARYING K FROM 1 BY 1 
               UNTIL  K > OBJ-COUNT OR 
               (OBJ-NAME(K) = OBJ-REQD-OBJ(J) AND OBJ-USED(K)='Y')

           IF K > OBJ-COUNT
                CONTINUE
           ELSE
               MOVE 'Y' TO REQD-OBJ-USED
           END-IF
           CONTINUE.    
       DO-NOTHING.

           EXIT.

