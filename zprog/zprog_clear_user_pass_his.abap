**********************************************************************
* ZPROG_CLEAR_USER_PASS_HIS                                          *
**********************************************************************
* This program clears the user's password history,                   *
* so the user can use the same password again.                       *
**********************************************************************

REPORT zprog_clear_user_pass_his.

TABLES: usr02, usrpwdhistory.

*-----------------------------------------------------------------------
*@DAT                         D A T A
*-----------------------------------------------------------------------
DATA: gt_usr02 TYPE TABLE OF usr02.
DATA: gs_usr02 TYPE usr02.

*-----------------------------------------------------------------------
*@SSL             S E L E C T I O N   S C R E E N
*-----------------------------------------------------------------------
SELECT-OPTIONS:
  s_mandt FOR usrpwdhistory-mandt DEFAULT sy-mandt,
  s_user FOR usr02-bname DEFAULT sy-uname.

*-----------------------------------------------------------------------
*@INI                 I N I T I A L I Z A T I O N.
*-----------------------------------------------------------------------
INITIALIZATION.
  sy-title = 'Clear the users password history'.
  %_s_mandt_%_app_%-text = 'Client'.
  %_s_user_%_app_%-text  = 'Username'.

*-----------------------------------------------------------------------
*@SOS             S T A R T   O F   S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  SELECT * FROM usr02 INTO TABLE gt_usr02
    WHERE bname IN s_user.


END-OF-SELECTION.
  LOOP AT gt_usr02 INTO gs_usr02.

    usr02-ocod1 =
    usr02-ocod2 =
    usr02-ocod3 =
    usr02-ocod4 =
    usr02-ocod5 = usr02-bcode.

    MODIFY usr02 FROM gs_usr02.

    DELETE FROM usrpwdhistory CLIENT SPECIFIED
      WHERE bname = gs_usr02-bname AND
            mandt IN s_mandt.

  ENDLOOP.

*-----------------------------------------------------------------------
*@EOS                E N D   O F   S E L E C T I O N
*-----------------------------------------------------------------------
  IF sy-subrc = 0.
    WRITE: / 'User''s password history has been reset'.
    COMMIT WORK.
  ENDIF.