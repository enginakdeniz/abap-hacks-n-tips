**********************************************************************
*  ZPROG_DYNAMIC_CODE                                                *
**********************************************************************
*                                                                    *
**********************************************************************

REPORT zprog_dynamic_code.

TABLES rlgrap.

*-----------------------------------------------------------------------
*@DAT                         D A T A
*-----------------------------------------------------------------------
DATA: it_tab TYPE filetable,
      gd_subrc TYPE i,
      answer TYPE c,
      str TYPE string.

DATA exp TYPE REF TO cx_root.

DATA: prog  TYPE syrepid,
      incl  TYPE syrepid,
      line  TYPE i.


TYPES: BEGIN OF t_abapcode,
  row(72) TYPE c,
 END OF t_abapcode.
DATA: it_abapcode TYPE STANDARD TABLE OF t_abapcode INITIAL SIZE 0,
      it_store    TYPE STANDARD TABLE OF t_abapcode INITIAL SIZE 0.

DATA: filename TYPE  string.

*-----------------------------------------------------------------------
*@SSL             S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------
PARAMETER: p_file(200) TYPE c OBLIGATORY,
           p_temp(30)  TYPE c DEFAULT 'ZTEMP_REPORT' OBLIGATORY,
           p_del AS CHECKBOX DEFAULT 'X',
           p_run AS CHECKBOX DEFAULT 'X'.

*-----------------------------------------------------------------------
*               A T   S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*Selecting a File, plus inserting default file extension
  REFRESH: it_tab.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select File'
      default_filename = '*.abap'
      multiselection   = ' '
    CHANGING
      file_table       = it_tab
      rc               = gd_subrc.

  LOOP AT it_tab INTO p_file.
  ENDLOOP.


*-----------------------------------------------------------------------
*@SOS             S T A R T   O F   S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.

*move file name into a field with type compatable with FM
  filename = p_file.

*upload file from PC
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = filename
    TABLES
      data_tab                = it_abapcode
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc EQ 0.
*   Check if report name being used fro temorary code already exists as
*   any code will be over written and lost
    READ REPORT p_temp INTO it_store.
    IF sy-subrc NE 0.
*     Please note any existing code in the program will be lost!!!!
      TRY .
          INSERT REPORT p_temp FROM it_abapcode.
          SUBMIT (p_temp) AND RETURN.
          DELETE REPORT p_temp.

        CATCH cx_root INTO exp.
          exp->get_source_position(
            IMPORTING
              include_name = incl
              program_name = prog
              source_line  = line ).

          MESSAGE s000(yrd) WITH 'ERROR ON LINE:' line.

      ENDTRY.
      INSERT REPORT p_temp FROM it_abapcode.
      SUBMIT (p_temp) AND RETURN.
      DELETE REPORT p_temp.
    ELSE.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
       text_question  = 'Report used to store temporary code ' &&
        'already exists!!! Do you want to overwrite it?'
       IMPORTING
         answer                      = answer.
      IF sy-subrc EQ 0.
        IF answer EQ '1'. "yes
*         Please note any existing code in the program will be lost!!!!
          INSERT REPORT p_temp FROM it_abapcode.
          TRY .
              SUBMIT (p_temp) AND RETURN.
              DELETE REPORT p_temp.
            CATCH cx_root INTO exp.
              exp->get_source_position(
                IMPORTING
                  include_name = incl
                  program_name = prog
                  source_line  = line ).
              MESSAGE s000(yrd) WITH 'ERROR ON LINE:' line.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
*@EOS                E N D   O F   S E L E C T I O N
*-----------------------------------------------------------------------
  LOOP AT it_abapcode INTO str.
    WRITE str.
  ENDLOOP.