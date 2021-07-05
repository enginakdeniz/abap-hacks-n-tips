**********************************************************************
* SICPA | TR     YRD_SAP_REQUEST_UPLOADER:V0.0                       *
**********************************************************************
* AUTHOR       : ENGINA                                              *
* CREATION DATE: 19.09.19                                            *
* DESCRIPTION  : SAP Request Uploader                                *
**********************************************************************
*                Modification Log                                    *
* Date    Developer Transport  Description                           *
*-------- --------- ---------- --------------------------------------*
**********************************************************************

REPORT yrd_sap_request_uploader.

SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE gv_blk1.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(20) gv_rqt FOR FIELD p_req MODIF ID rq.
PARAMETERS : p_req LIKE e071-trkorr MODIF ID rq OBLIGATORY.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(20) gv_fld FOR FIELD p_folder
MODIF ID fl.

PARAMETERS :
  p_folder(50) DEFAULT 'H:\SAP_TRANSPORT_FILES\' MODIF ID fl OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK s1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE gv_blk2.
PARAMETERS: p_up   TYPE c RADIOBUTTON GROUP r1.
PARAMETERS: p_down TYPE c RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK s2.

DATA : gv_dir_trans_path LIKE cst_rswatch01_alv-dirname,
       gv_slash ,gv_dotname_cofile(20), gv_dotname_data(20),
       gv_path_cofile LIKE sapb-sappfad,
       gv_path_data LIKE sapb-sappfad.

DATA gv_front_path_cofile LIKE sapb-sappfad.
DATA gv_front_path_data LIKE sapb-sappfad.
DATA gv_err.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  PERFORM f4_folder.

INITIALIZATION.

  gv_rqt  = 'SAP Request'. "'Transport Request :'.
  gv_fld  = 'Yerel Dosya'. "'Local Folder:'.
  gv_blk1 = 'Parametreler'.
  gv_blk2 = 'Operasyon'.


START-OF-SELECTION.

  " Path of DIR_TRANS folder :
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TRANS'
                     ID 'VALUE' FIELD gv_dir_trans_path.

  " Slash type :
  FIND '\' IN gv_dir_trans_path.
  IF sy-subrc EQ 0.
    gv_slash = '\'.
  ELSE.
    gv_slash = '/'.
  ENDIF.

  " Cofiles / Data File Paths :
  CONCATENATE :
    'K' p_req+4 '.' p_req(3) INTO gv_dotname_cofile,

    gv_dir_trans_path gv_slash 'cofiles'
    gv_slash gv_dotname_cofile
    INTO gv_path_cofile,

    'R' p_req+4 '.' p_req(3) INTO gv_dotname_data,

    gv_dir_trans_path gv_slash
    'data' gv_slash gv_dotname_data INTO gv_path_data.

  " Upload/Download Paths
  CONCATENATE p_folder gv_dotname_cofile
  INTO gv_front_path_cofile.

  CONCATENATE p_folder gv_dotname_data
  INTO gv_front_path_data.

  CASE abap_true.
    WHEN p_up.
      PERFORM check_front_file_exist
        USING gv_front_path_cofile
              gv_front_path_data.

      " Upload Cofile :
      PERFORM upload_file
      USING gv_path_cofile
            gv_front_path_cofile
            gv_err.

      CHECK gv_err IS INITIAL.

      " Upload Data :
      PERFORM upload_file
      USING gv_path_data
            gv_front_path_data
            gv_err.

    WHEN p_down.
      " Download Cofile :
      PERFORM download_file
      USING gv_path_cofile
            gv_front_path_cofile
            gv_err.

      CHECK gv_err IS INITIAL.

      " Download Data :
      PERFORM download_file
        USING gv_path_data
              gv_front_path_data
              gv_err.
    WHEN OTHERS.
  ENDCASE.

  IF gv_err EQ space.
    MESSAGE 'Request yüklendi' TYPE 'S'.
  ELSE.
    MESSAGE 'Request yüklenemedi' TYPE 'E'.
  ENDIF.

*&--------------------------------------------------------------------*
*&    Form  UPLOAD_FILE
*&--------------------------------------------------------------------*
FORM upload_file
USING ip_server_path
      ip_front_path
      ep_err.

  DATA lv_server_path LIKE sapb-sappfad.
  DATA lv_front_path  LIKE sapb-sappfad.

  CLEAR ep_err.

  lv_server_path = ip_server_path.
  lv_front_path  = ip_front_path.

  CALL FUNCTION 'ARCHIVFILE_CLIENT_TO_SERVER'
    EXPORTING
      path             = lv_front_path
      targetpath       = lv_server_path
    EXCEPTIONS
      error_file       = 1
      no_authorization = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    WRITE :/ 'Error uploading file'.
    ep_err = 'X'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_file
    USING ip_server_path
          ip_front_path
          ep_err.

  DATA lv_server_path LIKE sapb-sappfad.
  DATA lv_front_path  LIKE sapb-sappfad.

  CLEAR ep_err.

  lv_server_path = ip_server_path.
  lv_front_path  = ip_front_path.


  CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
    EXPORTING
      path                   = lv_server_path
      targetpath             = lv_front_path
   EXCEPTIONS
     error_file             = 1
     no_authorization       = 2
     OTHERS                 = 3.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DOWNLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  F4_FOLDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_folder .
  DATA: l_sel_dir TYPE string.

  "Browse the Directories
  CALL METHOD cl_gui_frontend_services=>directory_browse
*   EXPORTING
*     WINDOW_TITLE =
*     INITIAL_FOLDER =
    CHANGING
      selected_folder = l_sel_dir
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_folder = l_sel_dir.
  ENDIF.

ENDFORM.                    " F4_FOLDER

*&---------------------------------------------------------------------*
*&      Form  CHECK_FRONT_FILE_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_front_file_exist  USING    ip_front_path_cofile
                                ip_front_path_data.



ENDFORM.                    " CHECK_FRONT_FILE_EXIST