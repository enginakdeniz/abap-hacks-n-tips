*&---------------------------------------------------------------------*
*& Report ZSF_R_FIX_SRV_ORG_VARS
*&---------------------------------------------------------------------*
*& CRMS4D_SERV_H tablosundan “SERVICE_ORG_RESP” alanı boş olan ve
*& PROCESS_TYPE=ZRVR olan belgelerin çekilmesi gerekmektedir.
*& Sonrasında SERVICE_ORG_RESP alanına ilgili birimin atanması
*& gerekmektedir. (Yapılan bakım tablosundan kayıtlar çekilerek
*& update işlemi yapılacaktır.) Bu işlemi gerçekleştirmek
*& CRMS4D_SERV_H tablosunda selectte “SERVICE_ORG” değeri de
*& çekilerek, bakımlı tablodaki alanla karşılaştırılmalıdır.
*&---------------------------------------------------------------------*

REPORT zsf_r_fix_srv_org_vars.

TYPES:
  BEGIN OF ty_servh,
    header_guid      TYPE crms4d_serv_h-header_guid,
    process_type     TYPE crms4d_serv_h-process_type,
    service_org      TYPE crms4d_serv_h-service_org,
    service_org_resp TYPE crms4d_serv_h-service_org_resp,
  END OF ty_servh.

TYPES: tty_servh TYPE TABLE OF ty_servh.

DATA: gt_servh TYPE TABLE OF ty_servh.
DATA: gt_servh_selected TYPE tty_servh.

DATA: gt_srv_org_vars TYPE TABLE OF zsf_srv_org_vars.

CLASS lcl_alv DEFINITION DEFERRED.

START-OF-SELECTION.

  SELECT * FROM crms4d_serv_h
    WHERE service_org_resp EQ @space
      AND process_type EQ 'ZRVR'
      AND service_org NE @space
    INTO CORRESPONDING FIELDS OF TABLE @gt_servh.

  SELECT * FROM zsf_srv_org_vars
    INTO TABLE gt_srv_org_vars.

END-OF-SELECTION.
  IF sy-batch = abap_true.
    IF sy-dbcnt <> 0.
      PERFORM update CHANGING gt_servh[].
    ENDIF.
  ELSE.
    PERFORM display.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form update
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM update CHANGING pt_servh TYPE tty_servh.

  FIELD-SYMBOLS <fs_servh> like LINE OF pt_servh.

  DATA:
    lt_customer_h      TYPE crmt_customer_h_comt,
    ls_customer_h      TYPE crmt_customer_h_com,
    lt_exception       TYPE crmt_exception_t,
    lt_input_fields    TYPE crmt_input_field_tab,
    ls_input_fields    TYPE crmt_input_field,
    lt_field_names     TYPE crmt_input_field_names_tab,
    ls_field_names     TYPE crmt_input_field_names,
    lt_orderadm_h      TYPE crmt_orderadm_h_wrkt,
    ls_orderadm_h      TYPE crmt_orderadm_h_wrk,
    lt_objects_to_save TYPE crmt_object_guid_tab,
    ls_orgman          TYPE crmt_orgman_com,
    lt_orgman          TYPE crmt_orgman_comt.

  DATA: lt_guid_init TYPE crmt_object_guid_tab,
        lv_guid_init TYPE crmt_object_guid.

  DATA: lt_saved_objects TYPE crmt_return_objects,
        ls_saved_objects TYPE crmt_return_objects_struc.

* Fill in the order data
  LOOP AT pt_servh ASSIGNING <fs_servh>
    WHERE service_org_resp IS INITIAL.

    READ TABLE gt_srv_org_vars INTO DATA(ls_srv_org_vars)
      WITH KEY service_org = <fs_servh>-service_org.

    IF sy-subrc = 0.

      <fs_servh>-service_org_resp = ls_srv_org_vars-service_org_resp.

      ls_orgman = VALUE  crmt_orgman_com(
          ref_guid               = <fs_servh>-header_guid
          ref_kind               = 'A'
          ref_handle             = '0000000001'
*         service_org_resp       = ls_srv_org_vars-service_org_resp
          service_org_resp_short = ls_srv_org_vars-service_org_resp
          error_flag             = 'X'
          mode                   = 'B' ).

      ls_input_fields = VALUE crmt_input_field(
          ref_guid       = <fs_servh>-header_guid
          ref_kind       = 'A'
          objectname     = 'ORGMAN'
          ref_handle     = '0000000001'
          field_names = VALUE #(
*           ( fieldname = 'SERVICE_ORG_RESP' )
            ( fieldname = 'SERVICE_ORG_RESP_SHORT' ) ) ).

      INSERT ls_orgman INTO TABLE lt_orgman.
      INSERT ls_input_fields INTO TABLE lt_input_fields.
      INSERT <fs_servh>-header_guid INTO TABLE lt_objects_to_save.
    ENDIF.

  ENDLOOP.

  IF lt_objects_to_save[] IS NOT INITIAL.
    CALL FUNCTION 'CRM_ORDER_MAINTAIN'
      EXPORTING
        it_orgman         = lt_orgman
      IMPORTING
        et_exception      = lt_exception
      CHANGING
        ct_input_fields   = lt_input_fields
      EXCEPTIONS
        error_occurred    = 1
        document_locked   = 2
        no_change_allowed = 3
        no_authority      = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      "Implement suitable error handling here
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      "Save Leads
      CALL FUNCTION 'CRM_ORDER_SAVE'
        EXPORTING
          it_objects_to_save = lt_objects_to_save
        IMPORTING
          et_saved_objects   = lt_saved_objects
        EXCEPTIONS
          document_not_saved = 1
          OTHERS             = 2.

      "Commit Save
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      CLEAR: lt_guid_init[].
      LOOP AT lt_saved_objects INTO ls_saved_objects.
        lv_guid_init = ls_saved_objects-guid.
        APPEND lv_guid_init TO lt_guid_init.
      ENDLOOP.

      CALL FUNCTION 'CRM_ORDER_INITIALIZE'
        EXPORTING
          it_guids_to_init = lt_guid_init
        EXCEPTIONS
          error_occurred   = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDIF.
  ENDIF.

  LOOP AT pt_servh ASSIGNING <fs_servh>.
    READ TABLE lt_saved_objects INTO ls_saved_objects
      WITH KEY guid = <fs_servh>-header_guid.
    IF sy-subrc <> 0.
      CLEAR <fs_servh>-service_org_resp.
    ENDIF.
  ENDLOOP.

ENDFORM.

*--FAST-ALV-DEF------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_falv.
  PUBLIC SECTION.

  PROTECTED SECTION.
    "redefinition of event handler
    METHODS evf_user_command REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.

*--FAST-ALV-IMP------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD evf_user_command.
    CASE e_ucomm.
      WHEN zcl_falv_dynamic_status=>b_01.
        PERFORM change_settings.

      WHEN zcl_falv_dynamic_status=>b_02.
        me->check_changed_data( ).
        me->get_selected_rows(
           IMPORTING
             et_index_rows = DATA(lt_index_rows)  " Indexes of Selected Rows
*          et_row_no     =                      " Numeric IDs of Selected Rows
        ).

        REFRESH gt_servh_selected.
        LOOP AT lt_index_rows INTO DATA(ls_index).
          READ TABLE gt_servh INTO DATA(ls_servh) INDEX ls_index-index.
          IF sy-subrc = 0.
            APPEND ls_servh TO gt_servh_selected.
          ENDIF.
        ENDLOOP.

        PERFORM update CHANGING gt_servh_selected.

        LOOP AT gt_servh_selected INTO DATA(ls_servh_selected).
          READ TABLE gt_servh ASSIGNING FIELD-SYMBOL(<fs_servh>)
            WITH KEY header_guid = ls_servh_selected-header_guid.

          <fs_servh>-service_org_resp = ls_servh_selected-service_org_resp.

        ENDLOOP.

        me->check_changed_data( ).
        me->refresh_table_display( ).

      WHEN OTHERS.
        super->evf_user_command( e_ucomm ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Form display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display .
  DATA falv TYPE REF TO lcl_alv.
  falv ?= lcl_alv=>create( CHANGING ct_table = gt_servh ) .

  "Add title variable
  falv->title_v1 = sy-title.

  "Fcat
  falv->layout->set_zebra( abap_true  ).
  falv->layout->set_sel_mode( 'A' ).

  "Add button into GUI status at for function F01 (in partial dynamic GUI Status we can have up to 19 buttons)
  falv->gui_status->add_button(
    EXPORTING
      iv_button              = zcl_falv_dynamic_status=>b_01
      iv_text                = 'Settings'
      iv_icon                = icon_settings
  ).

  "Add button into GUI status at for function F01 (in partial dynamic GUI Status we can have up to 19 buttons)
  falv->gui_status->add_button(
    EXPORTING
      iv_button              = zcl_falv_dynamic_status=>b_02
      iv_text                = 'Run BAPI'
      iv_icon                = icon_complete
  ).

  "Display full screen grid
  falv->display( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form change_settings
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM change_settings .
  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action                       = 'U'
      view_name                    = 'ZSF_SRV_ORG_VARS'
    EXCEPTIONS
      client_reference             = 1
      foreign_lock                 = 2
      invalid_action               = 3
      no_clientindependent_auth    = 4
      no_database_function         = 5
      no_editor_function           = 6
      no_show_auth                 = 7
      no_tvdir_entry               = 8
      no_upd_auth                  = 9
      only_show_allowed            = 10
      system_failure               = 11
      unknown_field_in_dba_sellist = 12
      view_not_found               = 13
      maintenance_prohibited       = 14
      OTHERS                       = 15.

  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel = 'Error'   " Title
        msgid = '00'
        msgty = 'E'
        msgno = '001'
        msgv1 = 'Setting can not be opened.'.
  ENDIF.

ENDFORM.
