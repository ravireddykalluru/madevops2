       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGZUNIT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
               COPY DFHAID.
               COPY CBZMAP.
         01 WS-COMMAREA PIC X(100).
        01 WS-ACCOUNT-NO-T PIC S9(18).
        01 WS-ACCOUNT-NAME PIC X(50).
        01 WS-PRINT PIC X(21) VALUE 'IS ALREADY REGISTERED'.
        01 WS-ACCOUNT-NAME1 PIC X(50).
        01 WS-PRINT1 PIC X(23) VALUE 'REGISTERED SUCCESSFULL'.
        01 WS-ACCOUNT-STATUS  PIC X(10).
        01 WS-MESSAGE PIC X(100).
        01 WS-MESSAGE1 PIC X(190).
        77 WS-ABS-DATE    PIC S9(10) COMP-3.
        01 WS-DATE.
           05 WS-MONTH   PIC 99.
           05 FILLER     PIC X(01).
           05 WS-DAY     PIC 99.
           05 FILLER     PIC X(01).
           05 WS-YEAR    PIC 99.
        01 WS-TIME.
           05 WS-HOUR    PIC 99.
           05 FILLER     PIC X(01).
           05 WS-MIN     PIC 99.
           05 FILLER     PIC X(01).
           05 WS-SEC     PIC 99.
       LINKAGE SECTION.
       01 DFHCOMMAREA PIC X(100).
       PROCEDURE DIVISION.
       MAIN-PARA.
            PERFORM EIB-PARA THRU EIB-EXIT.
             STOP RUN.
       EIB-PARA.
           IF EIBCALEN = 0
              PERFORM INIT-PARA THRU INIT-EXIT
           ELSE
             MOVE DFHCOMMAREA TO WS-COMMAREA
             EVALUATE WS-COMMAREA
             WHEN 'CREG'
                PERFORM KEY-VALID THRU KEY-VALID-EXIT
             WHEN OTHER
                MOVE LOW-VALUES TO MAPAGMO
                MOVE 'EXIT' TO MSGO
             END-EVALUATE
           END-IF.
       EIB-EXIT.
           EXIT.
       INIT-PARA.
           MOVE LOW-VALUES TO MAPAGMO
           PERFORM DATE-TIME THRU DATE-TIME-EXIT
           MOVE WS-DATE TO CDATEO
           MOVE WS-TIME TO CTIMEO
           PERFORM SEND-MAP THRU SEND-MAP-EXIT
           MOVE 'CREG' TO WS-COMMAREA
           PERFORM RETURN-CICS THRU RETURN-CICS-EXIT.
       INIT-EXIT.
           EXIT.
       DATE-TIME.
             EXEC CICS ASKTIME ABSTIME(WS-ABS-DATE)
             END-EXEC.
             EXEC CICS FORMATTIME ABSTIME(WS-ABS-DATE)
             DDMMYY(WS-DATE)
             DATESEP('-')
             TIME(WS-TIME)
             TIMESEP(':')
             END-EXEC.
       DATE-TIME-EXIT.
             EXIT.
       SEND-MAP.
             EXEC CICS
             SEND MAP('MAPAGM') MAPSET('CBZMAP')
             FROM(MAPAGMO)
             ERASE
      *      FREEKB
      *      RESP(WS-CICS-RESP)
             END-EXEC.
      *      PERFORM CICS-RESP THRU CICS-RESP-EXIT.
       SEND-MAP-EXIT.
             EXIT.
       RETURN-CICS.
             EXEC CICS
             RETURN TRANSID('ZC70')
             COMMAREA(WS-COMMAREA)
             END-EXEC.
       RETURN-CICS-EXIT.
             EXIT.
       KEY-VALID.
             EVALUATE EIBAID
             WHEN DFHENTER
               MOVE LOW-VALUES TO MAPAGMO
               PERFORM RECEIVE-PARA THRU RECEIVE-PARA-EXIT
               PERFORM PROCESS-PARA THRU PROCESS-PARA-EXIT
               PERFORM DATE-TIME THRU DATE-TIME-EXIT
               MOVE WS-DATE TO CDATEO
               MOVE WS-TIME TO CTIMEO
               PERFORM SEND-MAP THRU SEND-MAP-EXIT
               PERFORM RETURN-CICS THRU RETURN-CICS-EXIT
             WHEN DFHPF3
               EXEC CICS
                    SEND CONTROL FREEKB ERASE
               END-EXEC
               EXEC CICS
                    RETURN
               END-EXEC
             WHEN OTHER
                MOVE LOW-VALUES TO MAPAGMO
                MOVE 'INVALID OPTION' TO MSGO
                PERFORM SEND-ERROR-MSG THRU SEND-ERROR-EXIT
             END-EVALUATE.
       KEY-VALID-EXIT.
             EXIT.
       RECEIVE-PARA.
             EXEC CICS
             RECEIVE MAP('MAPAGM') MAPSET('CBZMAP')
             INTO (MAPAGMI)
             END-EXEC.
       RECEIVE-PARA-EXIT.
             EXIT.
       SEND-ERROR-MSG.
             PERFORM DATE-TIME THRU DATE-TIME-EXIT
             MOVE WS-DATE TO CDATEO
             MOVE WS-TIME TO CTIMEO
             PERFORM SEND-MAP THRU SEND-MAP-EXIT
             PERFORM RETURN-CICS THRU RETURN-CICS-EXIT.
       SEND-ERROR-EXIT.
            EXIT.

       PROCESS-PARA.
            MOVE ACCTI TO WS-ACCOUNT-NO-T.
            IF WS-ACCOUNT-NO-T EQUAL TO 1000001001 THEN
                   MOVE 'DHINESH' TO NAMEO
                   MOVE 78156 TO IDO
                   MOVE 'SUCESS' TO MSGO
            ELSE
                   MOVE "ENTER 100000001001 AS ACCT NO" TO MSGO.
       PROCESS-PARA-EXIT.
           EXIT.


