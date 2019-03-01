000100 IDENTIFICATION DIVISION.                                         00010025
000200                                                                  00020026
000300 PROGRAM-ID.                    PROGRAM2.                         00030027
000400 AUTHOR.                        HCL     .                         00040026
000500 DATE-WRITTEN.                  FEB 2019.                         00050027
000600                                                                  00060026
000700******************************************************************00070026
002300                                                                  00230001
002400 ENVIRONMENT DIVISION.                                            00240001
002500                                                                  00250001
002600***************************************************************** 00260001
002700**                                                                00270001
002800**   CONFIGURATION SECTION                                        00280001
002900**                                                                00290001
003000******************************************************************00300001
003100                                                                  00310001
003200 CONFIGURATION SECTION.                                           00320001
003300                                                                  00330001
003400 SPECIAL-NAMES.                                                   00340026
003500      DECIMAL-POINT IS COMMA.                                     00350026
003600                                                                  00360026
003700******************************************************************00370026
003800**                                                                00380026
003900**   INPUT-OUTPUT SECTION                                         00390026
004000**                                                                00400026
004100******************************************************************00410026
004200                                                                  00420026
004300 INPUT-OUTPUT SECTION.                                            00430026
004400                                                                  00440026
004500 FILE-CONTROL.                                                    00450026
004600******************************************************************00460026
004700**                                                                00470026
004800**   DATA DIVISION                                                00480026
004900**                                                                00490026
005000******************************************************************00500026
005100                                                                  00510026
005200 DATA DIVISION.                                                   00520026
005300                                                                  00530026
005400******************************************************************00540026
005500**                                                                00550026
005600**   FILE SECTION                                                 00560026
005700**                                                                00570026
005800******************************************************************00580026
005900                                                                  00590026
006000 FILE SECTION.                                                    00600026
006100                                                                  00610026
006200******************************************************************00620026
006300**                                                                00630026
006400**  WORKING-STORAGE SECTION                                       00640026
006500**                                                                00650026
006600******************************************************************00660026
006700                                                                  00670026
006800 WORKING-STORAGE SECTION.                                         00680026
006900                                                                  00690026
007000 01 I             PIC 9(2).                 
       01 J             PIC 9(2).                                       00700026
007100 01 WS-EQUAL-SW   PIC X(1) VALUE 'N' .                            00710026

007600 01 WS-INPUTFILE-1 .                                              00760026
007700      03  POLICYNUM  PIC 9(3).                                    00770026
007800      03  AEGON-NR1   PIC X(5).                                   00780026
007900      03  NAME1       PIC X(7).                                   00790026
008000      03  STREETNAME1 PIC X(3).                                   00800026
008000      03  TELEPHONE1  PIC X(7).                                   00800026
008100 01 WS-RECORDS1 .                                                 00810026
008200      03 FILLER  PIC X(25) VALUE '12345778DANAPALABC2345671'.     00820026
008300      03 FILLER  PIC X(25) VALUE '33344678DEEPA  AAE2545671'.     00830026
008400      03 FILLER  PIC X(25) VALUE '11335778LEE    ABD2645671'.     00840026
008500      03 FILLER  PIC X(25) VALUE '19336798LIPSA  CBD2785671'.     00850026
008600      03 FILLER  PIC X(25) VALUE '18556900LEEDA  ZCD6685671'.     00860026
008700      03 FILLER  PIC X(25) VALUE '66598798LISSY  QRS7785671'.     00870026

009020 01 FILLER REDEFINES WS-RECORDS1.                                 00902026
009030      03 WS-RECORDS3 PIC X(25) OCCURS 6.                          00903026

007600 01 WS-INPUTFILE-2 .    
007700                                                                   00770026                                          00760026
007800      03  AEGON-NR2   PIC X(5).                                    00780026
007900      03  NAME2       PIC X(7).                                    00790026
008000      03  STREETNAME2 PIC X(3).                                    00800026
008000      03  TELEPHONE2  PIC X(7).                                    00800026
008100 01 WS-RECORDS2 .                                                   00810026
008200      03 FILLER  PIC X(22) VALUE '45678zzzzzLABC2345771'.            00820026
008300      03 FILLER  PIC X(22) VALUE '44678AAAAA  AEE2545771'.           00830026
008400      03 FILLER  PIC X(22) VALUE '35778BBBBB  ACD2645771'.           00840026
008500      03 FILLER  PIC X(22) VALUE '36798CCCCC  CVD2785771'.           00850026
008600      03 FILLER  PIC X(22) VALUE '56898DDDDD  ZBD6688871'.           00860026
008700      03 FILLER  PIC X(22) VALUE '98798EEEEE  QQS7786671'.           00870026

009020 01 FILLER REDEFINES WS-RECORDS2.                                 00902026
009030      03 WS-RECORDS4 PIC X(22) OCCURS 6.                          00903026

007600 01 WS-OUTFILE-1. 
           
            03  POLICY-NO   PIC X(3).                                        
007800      03  AEGON-NR3   PIC X(5).                                    00780026
007900      03  NAME3       PIC X(7).                                    00790026
008000      03  STREETNAME3 PIC X(3).                                    00800026
008000      03  TELEPHONE3  PIC X(7).   
            
       01 WS-MATCHED        PIC X(1).
          88 MATCH          VALUE "Y".
          88 NOMATCH        VALUE "N".                                   00800026

009040*----------------------------------------------------------------*00904026
009050 PROCEDURE DIVISION .                                             00905026
009060*----------------------------------------------------------------*00906026
009070 A-MAIN                                      SECTION.             00907026
009080*----------------------------------------------------------------*00908026
009090 A-001.                                                           00909026
009100                                                                  00910026
009200            INITIALIZE I.                                         00920026
                                                                        0930026
                                                                        00990026
                                                                        01000026
010100            INITIALIZE WS-INPUTFILE-1                             01010026
                  INITIALIZE WS-INPUTFILE-2                             01010026
                  PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
                  MOVE WS-RECORDS3(I) TO WS-INPUTFILE-1
                    INITIALIZE J
                    INITIALIZE WS-MATCHED
		            PERFORM VARYING J FROM 1 BY 1 UNTIL J > 6	
                    MOVE WS-RECORDS4(J) TO WS-INPUTFILE-2
                     IF AEGON-NR1 = AEGON-NR2
                        MOVE "Y"         TO WS-MATCHED
                        MOVE POLICYNUM   TO POLICY-NO
010300                  MOVE AEGON-NR2   TO AEGON-NR3                    01030026
010300                  MOVE NAME2       TO NAME3                         01030026
010300                  MOVE STREETNAME2 TO STREETNAME3                   01030026
010300                  MOVE TELEPHONE2  TO TELEPHONE3
                    END-IF                                                                       01030026
                    END-PERFORM
                    
                    IF MATCH
                       CONTINUE
                    ELSE
                       MOVE WS-INPUTFILE-1 TO WS-OUTFILE-1
                    END-IF
                    
                    DISPLAY "POLICY-NO: " POLICY-NO
                    DISPLAY "AEGON-NR3: " AEGON-NR3
                    DISPLAY "NAME3 " NAME3
                    DISPLAY "STREETNAME3: " STREETNAME3
                    DISPLAY "TELEPHONE3: " TELEPHONE3
                    
010500            END-PERFORM
                  .                                                     1210026 
                                                                        1220026
012300 A-999.                                                           01230026
012400     GOBACK.                                                      01240026
012500                                                                  01250026
