*&---------------------------------------------------------------------*
*& Report ZMM_DOC_FLOW_CHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_doc_flow_check.

INCLUDE zmm_i_3rdparty_create_po_cls.

TABLES: ekpo.

TYPES : BEGIN OF ty_data,
          ebeln_s       TYPE ekpo-ebeln,          "Ithalat SAS(Kaynak SAS)
          matnr         TYPE ekpo-matnr,          "Malzeme
          zz_purch_type TYPE ekpo-zz_purch_type,  "Alım Tipi
          vbeln         TYPE vbak-vbeln,          "Satış Siparişi
          werks         TYPE vbap-werks,          "Satış Siparişi UY
          banfn         TYPE eban-banfn,          "Yurtici SAT
          ebeln_t       TYPE eban-ebeln,          "Yurtici SAS (Hedef SAS)
          docnum        TYPE edids-docnum,            "IDOC
          docstatus     TYPE pfm_pap_doc_status_icon, "IDOC Status Icon
          docmessages   TYPE bapiret2_tab,            "IDOC Messages
          zmm180status  TYPE pfm_pap_doc_status_icon, "ZMM180 Status Icon
          zmm180msgs    TYPE bapiret2_tab,            "ZMM180 Messages
        END OF ty_data.

DATA: gt_data TYPE TABLE OF ty_data.
DATA: gs_data TYPE ty_data.

DATA: gv_percent TYPE p.

CLASS lcl_alv DEFINITION INHERITING FROM zcl_falv.
  PUBLIC SECTION.

  PROTECTED SECTION.
    "redefinition of event handler
    METHODS evf_hotspot_click REDEFINITION.
    METHODS evf_user_command REDEFINITION.
    METHODS evf_double_click REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD evf_double_click.
    READ TABLE gt_data INTO gs_data INDEX e_row-index.
    IF sy-subrc = 0.
      CASE e_column-fieldname.
        WHEN 'EBELN_S'.
          IF gs_data-ebeln_s IS NOT INITIAL.
            PERFORM call_tcode USING 'MM23' 'BES' gs_data-ebeln_s.
          ENDIF.
        WHEN 'MATNR'.
          IF gs_data-matnr IS NOT INITIAL.
            PERFORM call_tcode USING 'MM03' 'MAT' gs_data-matnr.
          ENDIF.
        WHEN 'VBELN'.
          IF gs_data-vbeln IS NOT INITIAL.
            PERFORM call_tcode USING 'VA03' 'AUN' gs_data-vbeln.
          ENDIF.
        WHEN 'BANFN'.
          IF gs_data-banfn IS NOT INITIAL.
            PERFORM call_tcode USING 'ME53' 'BAN' gs_data-banfn.
          ENDIF.
        WHEN 'EBELN_T'.
          IF gs_data-ebeln_s IS NOT INITIAL.
            PERFORM call_tcode USING 'MM23' 'BES' gs_data-ebeln_t.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD evf_hotspot_click.
    CASE e_column_id-fieldname.
      WHEN 'DOCSTATUS' OR 'ZMM180STATUS'.
        PERFORM display_messages USING es_row_no e_column_id-fieldname.
    ENDCASE.
  ENDMETHOD.

  METHOD evf_user_command.
    CASE e_ucomm.
      WHEN zcl_falv_dynamic_status=>b_01.
        DATA: lt_index_rows	TYPE lvc_t_row,
              lt_row_no	    TYPE lvc_t_roid.

        check_changed_data( ).
        get_selected_rows(
          IMPORTING
            et_index_rows = lt_index_rows
            et_row_no = lt_row_no
        ).

        IF lt_index_rows[] IS NOT INITIAL.
          READ TABLE lt_index_rows INTO DATA(ls_index_rows)
            INDEX 1.

          PERFORM call_zmm180 USING ls_index_rows-index.
          check_changed_data( ).
          refresh_table_display( ).

        ENDIF.

      WHEN OTHERS.
        super->evf_user_command( e_ucomm ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_ebeln FOR ekpo-ebeln.
SELECTION-SCREEN END OF BLOCK blk1.

START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.
  PERFORM diplay_report.

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_data .

  DATA: lv_objkey_docnum TYPE srrelroles-objkey,
        lv_objkey_ebeln  TYPE srrelroles-objkey.

  DATA: BEGIN OF ls_docnum_ebeln,
          ebeln  TYPE ekpo-ebeln,
          docnum TYPE edids-docnum,
        END OF ls_docnum_ebeln.

  DATA: lt_docnum_ebeln LIKE TABLE OF ls_docnum_ebeln.

  CLEAR: ls_docnum_ebeln, lt_docnum_ebeln[],
         gs_data, gt_data[].

  SELECT ebeln AS ebeln_s, matnr, zz_purch_type FROM ekpo INTO TABLE @DATA(lt_ekpo)
    WHERE ebeln IN @s_ebeln.

  SELECT srrelroles_idoc~objkey srrelroles_ekko~objkey
    INTO (lv_objkey_docnum, lv_objkey_ebeln)
    FROM srrelroles AS srrelroles_idoc
    JOIN idocrel
      ON idocrel~role_a                   = srrelroles_idoc~roleid
    JOIN srrelroles AS srrelroles_ekko
      ON srrelroles_ekko~roleid           = idocrel~role_b
    JOIN ekko ON ekko~ebeln               = srrelroles_ekko~objkey
    WHERE
      srrelroles_idoc~objtype         = 'IDOC'
      AND srrelroles_idoc~roletype    = 'INIDOC'
      AND srrelroles_ekko~objkey      IN s_ebeln.
    ls_docnum_ebeln-ebeln             = lv_objkey_ebeln.
    ls_docnum_ebeln-docnum            = lv_objkey_docnum.
    INSERT ls_docnum_ebeln INTO TABLE lt_docnum_ebeln.
  ENDSELECT.

  SORT lt_docnum_ebeln BY ebeln docnum ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_docnum_ebeln.

  SELECT DISTINCT status~* FROM edids AS status ##SHADOW
    INNER JOIN @lt_docnum_ebeln AS idocs ON idocs~docnum = status~docnum
      INTO TABLE @DATA(lt_edids).

  DESCRIBE TABLE lt_ekpo LINES DATA(lv_ekpo_size).

  LOOP AT lt_ekpo INTO DATA(ls_ekpo).

    PERFORM progress_bar USING '' sy-tabix lv_ekpo_size.

    CLEAR gs_data.
    gs_data-ebeln_s = ls_ekpo-ebeln_s.
    gs_data-matnr = ls_ekpo-matnr.
    gs_data-zz_purch_type = ls_ekpo-zz_purch_type.
    gs_data-docstatus = icon_light_out.

    CLEAR ls_docnum_ebeln.
    READ TABLE lt_docnum_ebeln INTO ls_docnum_ebeln
      WITH KEY ebeln = ls_ekpo-ebeln_s.
    IF sy-subrc = 0.
      gs_data-docnum = ls_docnum_ebeln-docnum.
      LOOP AT lt_edids INTO DATA(ls_edids)
          WHERE docnum = ls_docnum_ebeln-docnum
            AND countr IS NOT INITIAL.

        APPEND INITIAL LINE TO gs_data-docmessages ASSIGNING FIELD-SYMBOL(<fs_msg>).
        <fs_msg>-id = ls_edids-stamid.
        <fs_msg>-number = ls_edids-stamno.

        IF ls_edids-statyp IS NOT INITIAL.
          <fs_msg>-type = ls_edids-statyp.
        ELSE.
          <fs_msg>-type = 'W'.
        ENDIF.

        <fs_msg>-message = ls_edids-statxt.
        <fs_msg>-message_v1 = ls_edids-stapa1.
        <fs_msg>-message_v2 = ls_edids-stapa2.
        <fs_msg>-message_v3 = ls_edids-stapa3.
        <fs_msg>-message_v4 = ls_edids-stapa4.

        "Sipariş No
        IF ls_edids-status = '53' AND ls_edids-stamid = 'V1' AND ls_edids-stamno = '311'.
          gs_data-vbeln = |{ ls_edids-stapa2 ALPHA = IN }| .
          gs_data-docstatus = icon_green_light.

          PERFORM atc_skip01 CHANGING gs_data.


        ENDIF.

        IF gs_data-docstatus <> icon_green_light.
          CASE <fs_msg>-type.
            WHEN 'W'.
              gs_data-docstatus = icon_yellow_light.
            WHEN 'E'.
              gs_data-docstatus = icon_red_light.
          ENDCASE.
        ENDIF.

      ENDLOOP.

    ENDIF.

    APPEND gs_data TO gt_data.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form diplay_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM diplay_report .

  "creation of falv with local redefinition
  DATA falv TYPE REF TO lcl_alv.
  falv ?=  lcl_alv=>create( CHANGING ct_table = gt_data ).

  "Add hotspot to column 'DOCSTATUS'
  falv->column( 'DOCSTATUS' )->set_hotspot( abap_true ).
  falv->column( 'DOCSTATUS' )->set_just( 'C' ).

  falv->column( 'ZMM180STATUS' )->set_hotspot( abap_true ).
  falv->column( 'ZMM180STATUS' )->set_just( 'C' ).

  falv->layout->set_zebra( abap_true ).
  falv->layout->set_cwidth_opt( abap_true ).

  falv->gui_status->add_button(
      EXPORTING
        iv_button              = zcl_falv_dynamic_status=>b_01
        iv_text                = 'Test on ZMM180'(005)
        iv_icon                = icon_abc
    ).

  "Enable top_of_page and display Grid on full screen
  falv->display( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form dislplay_idoc_messages
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display_messages USING ps_row_no TYPE lvc_s_roid
                             pv_colnam.

  READ TABLE gt_data INTO gs_data INDEX ps_row_no-row_id.
  IF sy-subrc = 0 AND ( gs_data-docmessages[] IS NOT INITIAL OR
                        gs_data-zmm180msgs[] IS NOT INITIAL ).
    CASE pv_colnam.
      WHEN 'DOCSTATUS'.
        cl_rmsl_message=>display( gs_data-docmessages ).
      WHEN 'ZMM180STATUS'.
        cl_rmsl_message=>display( gs_data-zmm180msgs ).
      WHEN OTHERS.
    ENDCASE.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form call_zmm180
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_INDEX
*&---------------------------------------------------------------------*
FORM call_zmm180 USING u_index.

  CONSTANTS:
    BEGIN OF zmm180_check_status,
      success TYPE icon VALUE icon_message_uptodate,
      warning TYPE icon VALUE icon_message_outofdate,
      error   TYPE icon VALUE icon_message_orphaned,
    END OF zmm180_check_status.

  DATA: lo_zmm180 TYPE REF TO lcl_report.
  DATA: lr_banfn TYPE RANGE OF eban-banfn,
        lr_werks TYPE RANGE OF eban-werks.

  DATA: ls_banfn LIKE LINE OF lr_banfn,
        ls_werks LIKE LINE OF lr_werks.

  DATA: lt_return TYPE bapirettab.

  READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_data>)
    INDEX u_index.
  IF sy-subrc = 0.
    IF <fs_data>-banfn IS INITIAL.
      PERFORM add_message
        USING 'W' 'PR is requerd to create PO. (ZMM180)'(002)
        CHANGING lt_return.
    ENDIF.

    IF <fs_data>-ebeln_t IS NOT INITIAL.
      PERFORM add_message
        USING 'W' 'PO is already created. (ZMM180)'(003)
        CHANGING lt_return.
    ENDIF.

    IF lt_return[] IS INITIAL.
      CLEAR: lr_banfn, lr_werks, lt_return.

      ls_banfn-sign = ls_werks-sign = 'I'.
      ls_banfn-option = ls_werks-option = 'EQ'.
      ls_banfn-low = <fs_data>-banfn.
      ls_werks-low = <fs_data>-werks.

      APPEND:
        ls_banfn TO lr_banfn,
        ls_werks TO lr_werks.

      lo_zmm180 = NEW #( ).

      lt_return = lo_zmm180->get_data(
        EXPORTING
          r_banfn = lr_banfn[]
          r_werks = lr_werks[]
      ).

      IF lo_zmm180->gt_data[] IS NOT INITIAL.
        lt_return = lo_zmm180->run_bapi( abap_true ).
      ENDIF.
    ENDIF.

  ELSE.
    PERFORM add_message
      USING 'W' 'Select a row. (ZMM180)'(004)
      CHANGING lt_return.
  ENDIF.

  <fs_data>-zmm180status = zmm180_check_status-warning.
  <fs_data>-zmm180msgs = lt_return.
  cl_rmsl_message=>display( lt_return ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form add_message
*&---------------------------------------------------------------------*
*& Split ABAP string to internal table retaining whole words and
*& add as message
*&---------------------------------------------------------------------*
*&      --> U_TYPE
*&      --> U_MESSAGE
*&      <-- C_TABLES
*&---------------------------------------------------------------------*
FORM add_message  USING    u_type
                           u_message
                  CHANGING c_tables TYPE bapirettab.

  CONSTANTS:
    co_max TYPE i VALUE 50.

  DATA:
    lv_string       TYPE string,
    lv_sum          TYPE i,
    lv_txt1(co_max) TYPE c,
    lt_txt1         LIKE TABLE OF lv_txt1,
    lv_len1         TYPE i,
    lv_txt2(co_max) TYPE c,
    lt_txt2         LIKE TABLE OF lv_txt2,
    lv_len2         TYPE i.

  lv_string = u_message.

  SPLIT lv_string AT ' ' INTO TABLE lt_txt1.
  LOOP AT lt_txt1 INTO lv_txt1.
    lv_len1 = strlen( lv_txt1 ).
    ASSERT lv_len1 <= co_max.
    lv_len2 = strlen( lv_txt2 ).
    lv_sum = lv_len1 + lv_len2 + 1.
    IF lv_sum <= co_max.
      CONCATENATE lv_txt2 lv_txt1 INTO lv_txt2 SEPARATED BY ' '.
      CONDENSE lv_txt2.
    ELSE.
      APPEND lv_txt2 TO lt_txt2.
      lv_txt2 = lv_txt1.
    ENDIF.
  ENDLOOP.
  APPEND lv_txt2 TO lt_txt2.

  DATA: ls_message TYPE LINE OF bapirettab.

  ls_message-id = 'ZMM'.
  ls_message-number = 0.
  ls_message-type = u_type.
  ls_message-message = u_message.

  LOOP AT lt_txt2 INTO lv_txt2.
    CASE sy-tabix.
      WHEN 1.
        ls_message-message_v1 = lv_txt2.
      WHEN 2.
        ls_message-message_v2 = lv_txt2.
      WHEN 3.
        ls_message-message_v3 = lv_txt2.
      WHEN 4.
        ls_message-message_v4 = lv_txt2.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  APPEND ls_message TO c_tables.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form call_tcode
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> VALUE
*&---------------------------------------------------------------------*
FORM call_tcode  USING u_tcode  u_parid  u_parval.
  DATA: spa_tab TYPE STANDARD TABLE OF rfc_spagpa WITH HEADER LINE.

  "Parametere id of the screen field matnr on mm03 first screen
  spa_tab-parid = u_parid.
  spa_tab-parval = u_parval. " Material Name
  APPEND spa_tab.
  CLEAR spa_tab.

  CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
    EXPORTING
      tcode                   = u_tcode
      skip_screen             = 'X'
      mode_val                = 'A'
      update_val              = 'A'
*    IMPORTING
*     subrc                   =
    TABLES
*     using_tab               =
      spagpa_tab              = spa_tab
*     mess_tab                =
    EXCEPTIONS
      call_transaction_denied = 1
      tcode_invalid           = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0 AND sy-msgty IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
FORM progress_bar  USING    p_value
                            p_tabix
                            p_nlines.

  DATA: lv_text(40),
        lv_percentage      TYPE p,
        lv_percent_char(3).

  lv_percentage    = ( p_tabix / p_nlines ) * 100.
  lv_percent_char  = lv_percentage.

  SHIFT lv_percent_char LEFT DELETING LEADING ' '.
  CONCATENATE p_value lv_percent_char '% Complete'(006) INTO lv_text.

  IF lv_percentage GT gv_percent
      OR p_tabix  EQ 1.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_percentage
        text       = lv_text.

    gv_percent = lv_percentage.
  ENDIF.

ENDFORM.          " progress_bar

*&---------------------------------------------------------------------*
*& Form atc_skip01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM atc_skip01  CHANGING ps_data TYPE ty_data.
  SELECT SINGLE a~werks, e~banfn, n~ebeln FROM vbap AS a
    LEFT JOIN vbep AS e ON a~vbeln = e~vbeln
                       AND a~posnr = e~posnr
                       AND e~banfn IS NOT INITIAL
    LEFT JOIN eban AS n ON e~banfn = n~banfn
    WHERE a~vbeln = @ps_data-vbeln
      AND a~matnr = @ps_data-matnr
        INTO ( @ps_data-werks, @ps_data-banfn, @ps_data-ebeln_t ).
ENDFORM.
