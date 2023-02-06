*
* Copy Materials from one Plant to Another
* First run   GETDATA AS CHECKBOX, "Tick to download materials data to local harddisk
* Second run  UPDDATA AS CHECKBOX. "Tick to update date to Materials Master
* Check data in Excel before Second run
* 
REPORT ZBAPIMM01 LINE-SIZE 255 NO STANDARD PAGE HEADING
                 LINE-COUNT 065(001).

TABLES: T001L, "Storage Locations
        MARA,  "General Material Data
        MAKT,  "Material Descriptions
        MBEW,  "Material Valuation
        MARC.  "Plant Data for Material

DATA: BAPI_HEAD   LIKE BAPIMATHEAD,
      BAPI_MAKT   LIKE BAPI_MAKT,    "Material Description
      BAPI_MARA1  LIKE BAPI_MARA,    "Client Data
      BAPI_MARAX  LIKE BAPI_MARAX,
      BAPI_MARC1  LIKE BAPI_MARC,    "Plant View
      BAPI_MARCX  LIKE BAPI_MARCX,
      BAPI_MBEW1  LIKE BAPI_MBEW,    "Accounting View
      BAPI_MBEWX  LIKE BAPI_MBEWX,
      BAPI_RETURN LIKE BAPIRET2.

DATA: BEGIN OF INT_MAKT OCCURS 100.
        INCLUDE STRUCTURE BAPI_MAKT.
DATA: END OF INT_MAKT.

DATA: BEGIN OF INT_MAT OCCURS 100,
         WERKS(4),     "Plant
         MTART(4),     "Material type
         MATNR(18),    "Material number
         MATKL(9) ,    "Material group
         MBRSH(1),     "Industry sector
         MEINS(3),     "Base unit of measure
         GEWEI(3),     "Weight Unit
         SPART(2),     "Division
         EKGRP(3),     "Purchasing group
         VPRSV(1),     "Price control indicator
         STPRS(12),    "Standard price
         PEINH(3),     "Price unit
         SPRAS(2),     "Language key
         MAKTX(40),     "Material description
       END OF INT_MAT.

SELECT-OPTIONS:
            PLANT    FOR  MARC-WERKS OBLIGATORY MEMORY ID PLT,
            MATERIAL FOR  MARA-MATNR MEMORY ID MAT,
            MATLTYPE FOR  MARA-MTART MEMORY ID MTY,
            DIVISION FOR  MARA-SPART MEMORY ID DIV.
PARAMETERS:  F_FILE LIKE RLGRAP-FILENAME
             DEFAULT 'C:\DATA\ZMATERIAL.XLS' MEMORY ID F_FILE,
             GETDATA AS CHECKBOX, "Tick to download materials data to local harddisk
             UPDDATA AS CHECKBOX. "Tick to update date to Materials Master

IF GETDATA = 'X'.
   PERFORM DOWNLOAD_DATA.
   PERFORM DOWNLOAD_FILE.
ENDIF.
IF UPDDATA = 'X'.
   PERFORM UPLOAD_FILE.
   PERFORM UPDATE_MM.
ENDIF.

FORM DOWNLOAD_DATA.
SELECT * FROM MARC  WHERE LVORM EQ ' '
                      AND WERKS IN PLANT
                      AND MATNR IN MATERIAL.
    CLEAR MARA.
    SELECT SINGLE * FROM MARA WHERE MATNR =  MARC-MATNR.
    CHECK MATLTYPE.
    CHECK DIVISION.

    CLEAR MBEW.
    SELECT SINGLE * FROM MBEW WHERE MATNR =  MARC-MATNR
                                AND BWKEY =  MARC-WERKS.

    CLEAR MAKT.
    SELECT SINGLE * FROM MAKT WHERE SPRAS =  'EN'
                                AND MATNR =  MARC-MATNR.

    WRITE:/ MARC-WERKS,    "Plant
            MARA-MTART,    "Material type
            MARA-MATNR,    "Material number
            MARA-MATKL,    "Material group
            MARA-MBRSH,    "Industry sector
            MARA-MEINS,    "Base unit of measure
            MARA-GEWEI,    "Weight Unit
            MARA-SPART,    "Division
            MARC-EKGRP,    "Purchasing group
            MBEW-VPRSV,    "Price control indicator
            MBEW-STPRS,    "Standard price
            MBEW-PEINH,    "Price unit
            MAKT-SPRAS,    "Language key
            MAKT-MAKTX.    "Material description

            INT_MAT-WERKS = MARC-WERKS.    "Plant
            INT_MAT-MTART = MARA-MTART.    "Material type
            INT_MAT-MATNR = MARA-MATNR.    "Material number
            INT_MAT-MATKL = MARA-MATKL.    "Material group
            INT_MAT-MBRSH = MARA-MBRSH.    "Industry sector
            INT_MAT-MEINS = MARA-MEINS.    "Base unit of measure
            INT_MAT-GEWEI = MARA-GEWEI.    "Weight Unit
            INT_MAT-SPART = MARA-SPART.    "Division
            INT_MAT-EKGRP = MARC-EKGRP.    "Purchasing group
            INT_MAT-VPRSV = MBEW-VPRSV.    "Price control indicator
            INT_MAT-STPRS = MBEW-STPRS.    "Standard price
            INT_MAT-PEINH = MBEW-PEINH.    "Price unit
            INT_MAT-SPRAS = MAKT-SPRAS.    "Language key
            INT_MAT-MAKTX = MAKT-MAKTX.    "Material description

            APPEND INT_MAT.
            CLEAR  INT_MAT.
ENDSELECT.
ENDFORM.

FORM DOWNLOAD_FILE.
call function 'WS_DOWNLOAD'
  EXPORTING
    FILENAME                      = F_FILE
    FILETYPE                      = 'DAT'
*   FILETYPE                      = 'WK1'
  tables
    data_tab                      = INT_MAT
  EXCEPTIONS
    FILE_OPEN_ERROR               = 1
    FILE_WRITE_ERROR              = 2
    INVALID_FILESIZE              = 3
    INVALID_TYPE                  = 4
    NO_BATCH                      = 5
    UNKNOWN_ERROR                 = 6
    INVALID_TABLE_WIDTH           = 7
    GUI_REFUSE_FILETRANSFER       = 8
    CUSTOMER_ERROR                = 9
    OTHERS                        = 10.

IF SY-SUBRC = 0.
   FORMAT COLOR COL_GROUP.
   WRITE:/ 'Data Download Successfully to your local harddisk'.
   SKIP.
ENDIF.
ENDFORM.

FORM UPLOAD_FILE.
call function 'WS_UPLOAD'
  EXPORTING
    FILENAME                      = F_FILE
    FILETYPE                      = 'DAT'
*   FILETYPE                      = 'WK1'
  tables
    data_tab                      = INT_MAT
  EXCEPTIONS
    FILE_OPEN_ERROR               = 1
    FILE_WRITE_ERROR              = 2
    INVALID_FILESIZE              = 3
    INVALID_TYPE                  = 4
    NO_BATCH                      = 5
    UNKNOWN_ERROR                 = 6
    INVALID_TABLE_WIDTH           = 7
    GUI_REFUSE_FILETRANSFER       = 8
    CUSTOMER_ERROR                = 9
    OTHERS                        = 10.

IF SY-SUBRC = 0.
   FORMAT COLOR COL_GROUP.
   WRITE:/ 'Data Upload Successfully from your local harddisk'.
   SKIP.
ENDIF.
ENDFORM.

FORM UPDATE_MM.
LOOP AT INT_MAT.
* Header
    BAPI_HEAD-MATERIAL        = INT_MAT-MATNR.
    BAPI_HEAD-IND_SECTOR      = INT_MAT-MBRSH.
    BAPI_HEAD-MATL_TYPE       = INT_MAT-MTART.
    BAPI_HEAD-BASIC_VIEW      = 'X'.
    BAPI_HEAD-PURCHASE_VIEW   = 'X'.
    BAPI_HEAD-ACCOUNT_VIEW    = 'X'.
* Material Description
    REFRESH INT_MAKT.
    INT_MAKT-LANGU           = INT_MAT-SPRAS.
    INT_MAKT-MATL_DESC       = INT_MAT-MAKTX.
    APPEND INT_MAKT.
* Client Data - Basic
    BAPI_MARA1-MATL_GROUP     = INT_MAT-MATKL.
    BAPI_MARA1-BASE_UOM       = INT_MAT-MEINS.
    BAPI_MARA1-UNIT_OF_WT     = INT_MAT-GEWEI.
    BAPI_MARA1-DIVISION       = INT_MAT-SPART.

    BAPI_MARAX-MATL_GROUP = 'X'.
    BAPI_MARAX-BASE_UOM   = 'X'.
    BAPI_MARAX-UNIT_OF_WT = 'X'.
    BAPI_MARAX-DIVISION   = 'X'.
* Plant - Purchasing
    BAPI_MARC1-PLANT      = INT_MAT-WERKS.
    BAPI_MARC1-PUR_GROUP  = INT_MAT-EKGRP.

    BAPI_MARCX-PLANT      = INT_MAT-WERKS.
    BAPI_MARCX-PUR_GROUP  = 'X'.
* Accounting
    BAPI_MBEW1-VAL_AREA   = INT_MAT-WERKS.
    BAPI_MBEW1-PRICE_CTRL = INT_MAT-VPRSV.
    BAPI_MBEW1-STD_PRICE  = INT_MAT-STPRS.
    BAPI_MBEW1-PRICE_UNIT = INT_MAT-PEINH.

    BAPI_MBEWX-VAL_AREA   = INT_MAT-WERKS.

    BAPI_MBEWX-PRICE_CTRL = 'X'.
    BAPI_MBEWX-STD_PRICE  = 'X'.
    BAPI_MBEWX-PRICE_UNIT = 'X'.

    WRITE:/ BAPI_HEAD, BAPI_MARC1.

    call function 'BAPI_MATERIAL_SAVEDATA'
      exporting
        HEADDATA                   = BAPI_HEAD
        CLIENTDATA                 = BAPI_MARA1
        CLIENTDATAX                = BAPI_MARAX
        PLANTDATA                  = BAPI_MARC1
        PLANTDATAX                 = BAPI_MARCX
*       FORECASTPARAMETERS         =
*       FORECASTPARAMETERSX        =
*       PLANNINGDATA               =
*       PLANNINGDATAX              =
*       STORAGELOCATIONDATA        =
*       STORAGELOCATIONDATAX       =
        VALUATIONDATA              = BAPI_MBEW1
        VALUATIONDATAX             = BAPI_MBEWX
*       WAREHOUSENUMBERDATA        =
*       WAREHOUSENUMBERDATAX       =
*       SALESDATA                  = BAPI_MVKE1
*       SALESDATAX                 = BAPI_MVKEX
*       STORAGETYPEDATA            =
*       STORAGETYPEDATAX           =
      IMPORTING
        RETURN                     = BAPI_RETURN
      TABLES
        MATERIALDESCRIPTION        = INT_MAKT
*       UNITSOFMEASURE             =
*       UNITSOFMEASUREX            =
*       INTERNATIONALARTNOS        =
*       MATERIALLONGTEXT           =
*       TAXCLASSIFICATIONS         =
*       RETURNMESSAGES             =
*       PRTDATA                    =
*       PRTDATAX                   =
*       EXTENSIONIN                =
*       EXTENSIONINX               =
          .

IF BAPI_RETURN-TYPE = 'E'.
   WRITE:/ 'Error Message ', BAPI_RETURN.
ENDIF.

ENDLOOP.
ENDFORM.
*---End of Program
