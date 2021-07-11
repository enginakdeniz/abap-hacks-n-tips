**********************************************************************
*  ZPROG_SELECTION_SCREEN                                            *
**********************************************************************
*                                                                    *
**********************************************************************

REPORT zprog_selection_screen.

TYPE-POOLS: icon. "To Add Icon to Button
TABLES sscrfields.
DATA activetab(6) TYPE c .

*-----------------------------------------------------------------------
*@SSL             S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------
"Radiobutton (inc desc) and entry field on same line of SAP selection
"screen
SELECTION-SCREEN BEGIN OF SCREEN 001 AS SUBSCREEN NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK out1 WITH FRAME TITLE s01.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_type RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 4(15) a01 FOR FIELD p_type.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_tinput LIKE p2001-awart.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_reason RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 4(17) a02 FOR FIELD p_reason.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_rinput LIKE p2001-umsch.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK out1.
SELECTION-SCREEN END OF SCREEN 001.

"Checkbox (inc desc) and entry field on same line of selection screen
SELECTION-SCREEN BEGIN OF SCREEN 002 AS SUBSCREEN NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK out2 WITH FRAME TITLE s02.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_filchk AS CHECKBOX.
SELECTION-SCREEN COMMENT 4(17) b01 FOR FIELD p_filchk.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_file LIKE rlgrap-filename.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK out2.
SELECTION-SCREEN END OF SCREEN 002.

"Two radios buttons on same line of selection screen
SELECTION-SCREEN BEGIN OF SCREEN 003 AS SUBSCREEN NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK out3 WITH FRAME TITLE s03.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_and RADIOBUTTON GROUP grp2.
SELECTION-SCREEN COMMENT 4(5) c01 FOR FIELD p_and.
PARAMETERS: p_or RADIOBUTTON GROUP grp2.
SELECTION-SCREEN COMMENT (5) c02 FOR FIELD p_or.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK out3.
SELECTION-SCREEN END OF SCREEN 003.

"Two entry fields plus comments on same line of selection screen
SELECTION-SCREEN BEGIN OF SCREEN 004 AS SUBSCREEN NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK out4 WITH FRAME TITLE s04.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(7) d01.  "su9 = Clerk
PARAMETERS: p_clerk1 LIKE knb1-busab OBLIGATORY DEFAULT '51'.
SELECTION-SCREEN COMMENT 15(19) d02. "su1 = Surname begins with
PARAMETERS: p_cstr1(20) TYPE c OBLIGATORY DEFAULT 'ABCD'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK out4.
SELECTION-SCREEN END OF SCREEN 004.

"Creating password field in ABAP selection screen
SELECTION-SCREEN BEGIN OF SCREEN 005 AS SUBSCREEN NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK out5 WITH FRAME TITLE s05.
PARAMETERS: p_name TYPE char10.
PARAMETERS: p_pass TYPE char10.
SELECTION-SCREEN END OF BLOCK out5.
SELECTION-SCREEN END OF SCREEN 005.

"Pushbutton on ABAP Selection Screen
SELECTION-SCREEN BEGIN OF SCREEN 006 AS SUBSCREEN NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK out6 WITH FRAME TITLE s06.
SELECTION-SCREEN PUSHBUTTON /2(35) button1 USER-COMMAND but1.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON /2(35) button2 USER-COMMAND but2.
SELECTION-SCREEN END OF BLOCK out6.
SELECTION-SCREEN END OF SCREEN 006.

"Mount-Year Selection Wizzard on ABAP Selection Screen
SELECTION-SCREEN BEGIN OF SCREEN 007 AS SUBSCREEN NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK out7 WITH FRAME TITLE s07.
PARAMETERS: p_month TYPE isellist-month.
SELECTION-SCREEN END OF BLOCK out7.
SELECTION-SCREEN END OF SCREEN 007.

SELECTION-SCREEN BEGIN OF TABBED BLOCK tabb1 FOR 5 LINES NO INTERVALS.

SELECTION-SCREEN TAB (15) tabs1 USER-COMMAND ucomm1 DEFAULT SCREEN 001.
SELECTION-SCREEN TAB (15) tabs2 USER-COMMAND ucomm2.
SELECTION-SCREEN TAB (15) tabs3 USER-COMMAND ucomm3.
SELECTION-SCREEN TAB (15) tabs4 USER-COMMAND ucomm4.
SELECTION-SCREEN TAB (15) tabs5 USER-COMMAND ucomm5.
SELECTION-SCREEN TAB (15) tabs6 USER-COMMAND ucomm6.
SELECTION-SCREEN TAB (15) tabs7 USER-COMMAND ucomm7.

SELECTION-SCREEN END OF BLOCK tabb1.

*-----------------------------------------------------------------------
*               A T   S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------

AT SELECTION-SCREEN.
  CASE sscrfields.
    WHEN 'BUT1'.
      MESSAGE 'Continue Button was clicked' TYPE 'I'.
    WHEN 'BUT2'.
      MESSAGE 'Exit Button was clicked' TYPE 'I'.
  ENDCASE.

  CASE sscrfields-ucomm.
    WHEN 'UCOMM1'.
      tabb1-prog = sy-repid.
      tabb1-dynnr = 001.
      tabb1-activetab = 'TABS1'.
      activetab = 'TABS1' .

    WHEN 'UCOMM2'.
      tabb1-prog = sy-repid.
      tabb1-dynnr = 002.
      tabb1-activetab = 'TABS2'.
      activetab = 'TABS2'.

    WHEN 'UCOMM3'.
      tabb1-prog = sy-repid.
      tabb1-dynnr = 003.
      tabb1-activetab = 'TABS3'.
      activetab = 'TABS3'.

    WHEN 'UCOMM4'.
      tabb1-prog = sy-repid.
      tabb1-dynnr = 004.
      tabb1-activetab = 'TABS4'.
      activetab = 'TABS4'.

    WHEN 'UCOMM5'.
      tabb1-prog = sy-repid.
      tabb1-dynnr = 005.
      tabb1-activetab = 'TABS5'.
      activetab = 'TABS5'.

    WHEN 'UCOMM6'.
      tabb1-prog = sy-repid.
      tabb1-dynnr = 006.
      tabb1-activetab = 'TABS6'.
      activetab = 'TABS6'.

    WHEN 'UCOMM7'.
      tabb1-prog = sy-repid.
      tabb1-dynnr = 007.
      tabb1-activetab = 'TABS7'.
      activetab = 'TABS7'.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month   = sy-datum(6)
    IMPORTING
      selected_month = p_month.

"Password Field Extention (Here is the Magic)
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_PASS'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*-----------------------------------------------------------------------
*@INI                 I N I T I A L I Z A T I O N.
*-----------------------------------------------------------------------  
INITIALIZATION.
  a01 = 'by Absence Type'.
  a02 = 'by Absence Reason'.
  b01 = 'Download to Excel'.
  c01 = 'AND'.
  c02 = 'OR'.
  d01 = 'Clerk'.
  d02 = 'Surname begins with'.
  s01 = 'Radiobutton (inc desc) and entry field on same line ' &&
        'of SAP selection screen'.
  s02 = 'Checkbox (inc desc) and entry field on same line of ' &&
        'selection screen'.
  s03 =   'Two radios buttons on same line of selection screen'.
  s04 =   'Two entry fields plus comments on same line of' &&
         'selection screen'.
  s05 =   'Creating password field in ABAP selection screen.'.
  s06 =   'Pushbutton on ABAP Selection Screen'.
*  BUTTON1 = 'Button 1'.
*  BUTTON2 = 'Button 2'.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_okay
      text   = 'Continue'
      info   = 'Click to Continue'
    IMPORTING
      result = button1
    EXCEPTIONS
      OTHERS = 0.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_cancel
      text   = 'Exit'
      info   = 'Click to Exit'
    IMPORTING
      result = button2
    EXCEPTIONS
      OTHERS = 0.

  tabs1 = '1'. tabs2 = '2'.
  tabs3 = '3'. tabs4 = '4'.
  tabs5 = '5'. tabs6 = '6'.
  tabs7 = '7'.
  activetab = 'TABS1'.

*-----------------------------------------------------------------------
*@SOS             S T A R T   O F   S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  IF p_name IS NOT INITIAL AND
     p_pass IS NOT INITIAL.
    WRITE:/ 'Name', ' : ', p_name.
    WRITE:/ 'Pass', ' : ', p_pass.
  ENDIF.