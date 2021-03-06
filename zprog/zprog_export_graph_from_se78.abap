**********************************************************************
*  ZPROG_EXPORT_GRAPH_FROM_SE78                                      *
**********************************************************************
*  This program extracts graphic logos from the BDS                  *
*  and saves it as a .bmp file on the pc.  Transaction               *
*  SE78 does not provide a way to export logos.                      *
**********************************************************************

REPORT zprog_export_graph_from_se78.

*-----------------------------------------------------------------------
*@DAT                         D A T A
*-----------------------------------------------------------------------
TYPE-POOLS: sbdst .

DATA : git_content TYPE sbdst_content.

DATA : git_rawdata     TYPE w3mime OCCURS 0,
       g_bitmaptypeout TYPE c. 

DATA : BEGIN OF git_bitmap OCCURS 0,
         line(1000),
       END   OF git_bitmap.
DATA : g_bytecount TYPE i.

*-----------------------------------------------------------------------
*@SSL             S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK b01 WITH FRAME TITLE b01.
PARAMETERS      : p_obj LIKE stxbitmaps-tdobject DEFAULT 'GRAPHICS',
                  p_nam LIKE stxbitmaps-tdname,
                  p_id  LIKE stxbitmaps-tdid DEFAULT 'BMAP',
                  p_ref LIKE stxbitmaps-tdbtype DEFAULT 'BCOL'.
SELECTION-SCREEN: END   OF BLOCK b01.
SELECTION-SCREEN: BEGIN OF BLOCK b02 WITH FRAME TITLE b02.
PARAMETERS      : p_file LIKE rlgrap-filename.
SELECTION-SCREEN: END   OF BLOCK b02.

*-----------------------------------------------------------------------
*@INI                 I N I T I A L I Z A T I O N.
*-----------------------------------------------------------------------
INITIALIZATION.

  CONCATENATE 'This program exports logos from SE78 to'
  'a pc file in .bmp format' INTO sy-title SEPARATED BY ' '.

  b01    = 'Selection Parameters'.
  %_p_obj_%_app_%-text   =  'Graphic Object'.
  %_p_nam_%_app_%-text   =  'Text Name'.
  %_p_id_%_app_%-text    =  'Graphic Identifier'.
  %_p_ref_%_app_%-text  =  'Graphic Type'.

  b02 = 'Output'.
  %_p_file_%_app_%-text  =  'File Name'.

*-----------------------------------------------------------------------
*@SOS             S T A R T   O F   S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM sapscript_get_graphic_bds.
  PERFORM sapscript_convert_bitmap.
  PERFORM ws_download.

*-----------------------------------------------------------------------
*@EOS                E N D   O F   S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.


*-----------------------------------------------------------------------
*@TOP                   T O P   O F   P A G E
*-----------------------------------------------------------------------
TOP-OF-PAGE.


*-----------------------------------------------------------------------
*               A T   S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_nam.

  DATA: l_return TYPE i.
  RANGES: r_obj FOR stxbitmaps-tdobject.
  DATA: l_bitmaps TYPE TABLE OF stxbitmaps WITH HEADER LINE.
  DATA: lit_scrfields TYPE TABLE OF dynpread WITH HEADER LINE.

  r_obj-sign   = 'I'.
  r_obj-option = 'EQ'.
  r_obj-low    = 'GRAPHICS'.
  APPEND r_obj.

  CALL FUNCTION 'SAPSCRIPT_SEARCH_GRAPHIC_BDS'
    EXPORTING
      selection_screen   = 'X'
      select_entry       = 'X'
      selection_show     = 'X'
    IMPORTING
      e_object           = p_obj
      e_id               = p_id
      e_name             = p_nam
      e_btype            = p_ref
    TABLES
      t_objects          = r_obj
      t_selections       = l_bitmaps
    EXCEPTIONS
      nothing_found      = 1
      selection_canceled = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE..
    lit_scrfields-fieldname = 'P_ID'.
    lit_scrfields-fieldvalue = p_id.
    APPEND lit_scrfields.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = 'Z_EXPORT_GRAPHIC_FROM_SE78'
        dynumb     = '1000'
      TABLES
        dynpfields = lit_scrfields.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.


*-----------------------------------------------------------------------
*@FOR                          F O R M S
*-----------------------------------------------------------------------


*&---------------------------------------------------------------------
*&      Form  SAPSCRIPT_GET_GRAPHIC_BDS
*&---------------------------------------------------------------------
FORM sapscript_get_graphic_bds.

  CALL FUNCTION 'SAPSCRIPT_GET_GRAPHIC_BDS'
    EXPORTING
      i_object       = p_obj
      i_name         = p_nam
      i_id           = p_id
      i_btype        = p_ref
    IMPORTING
      e_bytecount    = g_bytecount
    TABLES
      content        = git_content
    EXCEPTIONS
      not_found      = 1
      bds_get_failed = 2
      bds_no_content = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " SAPSCRIPT_GET_GRAPHIC_BDS

*&---------------------------------------------------------------------
*&      Form  SAPSCRIPT_CONVERT_BITMAP
*&---------------------------------------------------------------------
FORM sapscript_convert_bitmap.
*                                                assign
  CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP '
    EXPORTING
      old_format               = 'BDS'
      new_format               = 'BMP'
      bitmap_file_bytecount_in = g_bytecount
      itf_bitmap_type_in       = '*'
    IMPORTING
      bitmap_file_bytecount    = g_bytecount
      itf_bitmap_type_out      = g_bitmaptypeout
    TABLES
      bitmap_file              = git_rawdata
      bds_bitmap_file          = git_content
    EXCEPTIONS
      no_bitmap_file           = 1
      format_not_supported     = 2
      bitmap_file_not_type_x   = 3
      no_bmp_file              = 4
      bmperr_invalid_format    = 5
      bmperr_no_colortable     = 6
      bmperr_unsup_compression = 7
      bmperr_corrupt_rle_data  = 8
      bmperr_eof               = 9
      bdserr_invalid_format    = 10
      bdserr_eof               = 11
      OTHERS                   = 12.
  IF sy-subrc <> 0.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " SAPSCRIPT_CONVERT_BITMAP

*&---------------------------------------------------------------------
*&      Form  WS_DOWNLOAD
*&---------------------------------------------------------------------
FORM ws_download.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      bin_filesize            = g_bytecount
      filename                = p_file
      filetype                = 'BIN'
    TABLES
      data_tab                = git_rawdata
    EXCEPTIONS
      file_open_error         = 1
      file_write_error        = 2
      invalid_filesize        = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      OTHERS                  = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " WS_DOWNLOAD