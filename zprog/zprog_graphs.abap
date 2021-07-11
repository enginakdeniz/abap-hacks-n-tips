**********************************************************************
* ZPROG_GRAPHS                                                       *
**********************************************************************

REPORT zprog_graphs.

*-----------------------------------------------------------------------
*@SSL             S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE blt.
PARAMETERS: rb1 RADIOBUTTON GROUP rg DEFAULT 'X'.
PARAMETERS: rb2 RADIOBUTTON GROUP rg.
PARAMETERS: rb3 RADIOBUTTON GROUP rg.
SELECTION-SCREEN END OF BLOCK bl.

*-----------------------------------------------------------------------
*@INI                 I N I T I A L I Z A T I O N.
*-----------------------------------------------------------------------
INITIALIZATION.
  blt = 'CALL FUNCTION'.
  %_rb1_%_app_%-text = 'GRAPH_2D'.
  %_rb2_%_app_%-text = 'GRAPH_MATRIX_2D'.
  %_rb3_%_app_%-text = 'GRAPH_MATRIX_3D'.


*-----------------------------------------------------------------------
*@SOS             S T A R T   O F   S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  CASE 'X'.
    WHEN rb1.
      PERFORM graph_2d.
    WHEN rb2.
      PERFORM graph_matrix_2d.
    WHEN rb3.
      PERFORM graph_matrix_3d.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  GRAPH_2D
*&---------------------------------------------------------------------*
FORM graph_2d .

  DATA: BEGIN OF lt_data OCCURS 1,
          text(25),
          value    TYPE p,
        END OF lt_data.

  DATA: tcol1(5)      VALUE '#1991'.
  DATA: title(25)      VALUE 'Sales'.

  lt_data-text = 'Product_1'.
  lt_data-value = 153470.
  APPEND lt_data.

  lt_data-text = 'Product_2'.
  lt_data-value = 253150.
  APPEND lt_data.

  lt_data-text = 'Product_3'.
  lt_data-value = 53470.
  APPEND lt_data.

  lt_data-text = 'Product_4'.
  lt_data-value = 182000.
  APPEND lt_data.

  lt_data-text = 'Product_5'.
  lt_data-value = 92410.
  APPEND lt_data.

  CALL FUNCTION 'GRAPH_2D'
    EXPORTING
      titl = title
    TABLES
      data = lt_data.

ENDFORM.                    " GRAPH_2D

*&---------------------------------------------------------------------*
*&      Form  GRAPH_MATRIX_2D
*&---------------------------------------------------------------------*
FORM graph_matrix_2d .
  DATA: BEGIN OF lt_data OCCURS 1 ,
          text(36),
          feld1    TYPE p,
        END OF lt_data ,

        BEGIN OF lt_opts OCCURS 1,
          C(80) TYPE c,
        END OF lt_opts,

        BEGIN OF lt_tyear OCCURS 1,
          C(20) TYPE c,
        END OF lt_tyear.

  lt_opts-c = 'P2TYPE = PI'.
  APPEND lt_opts.

  lt_data-text = 'RUSYA (%27.3)'.
  lt_data-feld1 = '27'.
  APPEND lt_data.

  lt_data-text = 'KANADA (%16)'.
  lt_data-feld1 = '16'.
  APPEND lt_data.

  lt_data-text = 'ÇİN (%15.4)'.
  lt_data-feld1 = '15'.
  APPEND lt_data.

  lt_data-text = 'ABD (%15.5)'.
  lt_data-feld1 = '15'.
  APPEND lt_data.

  lt_data-text = 'BREZILYA (%13.6)'.
  lt_data-feld1 = '13'.
  APPEND lt_data.

  lt_data-text = 'AVUSTRALYA (%12.3)'.
  lt_data-feld1 = '12'.
  APPEND lt_data.

  CALL FUNCTION 'GRAPH_MATRIX_2D'
    TABLES
      data        = lt_data
      opts        = lt_opts
      tcol        = lt_tyear
    EXCEPTIONS
      col_invalid = 1
      opt_invalid = 2
      OTHERS      = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GRAPH_MATRIX_2D

*&---------------------------------------------------------------------*
*&      Form  GRAPH_MATRIX_3D
*&---------------------------------------------------------------------*
FORM graph_matrix_3d .

  TYPES: BEGIN OF ttab_data,
           dataname(15),
           quantity1    TYPE i,
           quantity2    TYPE i,
           quantity3    TYPE i,
         END OF ttab_data.

  TYPES: BEGIN OF ttab_options,
           option(20),
         END OF ttab_options.

  DATA: itab_data TYPE TABLE OF ttab_data,
        xtab_data LIKE LINE OF  itab_data.

  DATA: itab_options TYPE TABLE OF ttab_options,
        xtab_options LIKE LINE OF  itab_options.


  xtab_data-dataname = 'Screws'.
  xtab_data-quantity1 = 5500.
  xtab_data-quantity2 = 6200.
  xtab_data-quantity3 = 5900.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Nails'.
  xtab_data-quantity1 = 3500.
  xtab_data-quantity2 = 5200.
  xtab_data-quantity3 = 4400.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Nuts'.
  xtab_data-quantity1 = 1800.
  xtab_data-quantity2 = 2200.
  xtab_data-quantity3 = 1900.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Fastners'.
  xtab_data-quantity1 = 5500.
  xtab_data-quantity2 = 6200.
  xtab_data-quantity3 = 5900.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Bolts'.
  xtab_data-quantity1 = 3500.
  xtab_data-quantity2 = 5200.
  xtab_data-quantity3 = 4400.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Clamps'.
  xtab_data-quantity1 = 1800.
  xtab_data-quantity2 = 2200.
  xtab_data-quantity3 = 1900.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Hand Tools'.
  xtab_data-quantity1 = 5500.
  xtab_data-quantity2 = 6200.
  xtab_data-quantity3 = 5900.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Saws'.
  xtab_data-quantity1 = 3500.
  xtab_data-quantity2 = 5200.
  xtab_data-quantity3 = 4400.
  APPEND xtab_data TO itab_data.

  xtab_data-dataname = 'Jigs'.
  xtab_data-quantity1 = 1800.
  xtab_data-quantity2 = 2200.
  xtab_data-quantity3 = 1900.
  APPEND xtab_data TO itab_data.

  CALL FUNCTION 'GRAPH_MATRIX_3D'
    EXPORTING
      titl = 'Usage in $'
      col1 = 'Materials'
    TABLES
      data = itab_data
      opts = itab_options.

ENDFORM.                    " GRAPH_MATRIX_3D