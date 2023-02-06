*
* List Box in ABAP Report
*
* Written by : SAP Basis, ABAP Programming and Other IMG Stuff
*              https://www.erpgreat.com
*
REPORT ZLIST.

TYPE-POOLS: VRM.

DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

PARAMETERS: PS_PARM(10) AS LISTBOX VISIBLE LENGTH 10.

AT SELECTION-SCREEN OUTPUT.

NAME = 'PS_PARM'.
VALUE-KEY = '1'.

VALUE-TEXT = 'LINE 1'.
APPEND VALUE TO LIST. VALUE-KEY = '2'.

VALUE-TEXT = 'LINE 2'.
APPEND VALUE TO LIST.

CALL FUNCTION 'VRM_SET_VALUES' 
  EXPORTING 
    ID = NAME 
    VALUES = LIST.

START-OF-SELECTION.
WRITE: / 'PARAMETER:', PS_PARM.
