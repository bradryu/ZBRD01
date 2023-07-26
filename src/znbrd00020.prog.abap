*************************************************************************
* Modules / Sub Module : CM / AL
*&---------------------------------------------------------------------
* T_CODE             : ZNBRD00020
* WRITER             : BRAD.RYU
* DATE               : 2015.12.10
* TYPE               : Include Report
* Description        : 공통 sub routine, define include프로그램
*
*************************************************************************
*              Changed History
*&---------------------------------------------------------------------**
*Changed Number     Changed Date   Writer    Changed      Description
*&---------------------------------------------------------------------**
*      N            2015.12.10    BRAD.RYU   CREATE       CREATE       *
*************************************************************************

DEFINE _set_timestamp_create.
  CLEAR gs_create_timestamp.
  gs_create_timestamp-ernam = sy-uname.
  gs_create_timestamp-erdat = sy-datum.
  gs_create_timestamp-erzet = sy-uzeit.
  gs_create_timestamp-aenam = sy-uname.
  gs_create_timestamp-aedat = sy-datum.
  gs_create_timestamp-aezet = sy-uzeit.
  MOVE-CORRESPONDING gs_create_timestamp TO &1.
END-OF-DEFINITION.
DEFINE _set_timestamp_change.
  CLEAR gs_change_timestamp.
  gs_change_timestamp-aenam = sy-uname.
  gs_change_timestamp-aedat = sy-datum.
  gs_change_timestamp-aezet = sy-uzeit.
  MOVE-CORRESPONDING gs_change_timestamp TO &1.
END-OF-DEFINITION.
DEFINE _set_local_timestamp_create.
  &1-ernam = sy-uname.
  &1-erdat = sy-datlo.
  &1-erzet = sy-timlo.
  &1-aenam = sy-uname.
  &1-aedat = sy-datlo.
  &1-aezet = sy-timlo.
END-OF-DEFINITION.
DEFINE _set_local_timestamp_change.
  &1-aenam = sy-uname.
  &1-aedat = sy-datlo.
  &1-aezet = sy-timlo.
END-OF-DEFINITION.
DEFINE _set_toolbar.
  CLEAR &1.
  MOVE &2 TO &1-butn_type.
  MOVE &3 TO &1-function.
  MOVE &4 TO &1-icon.
  MOVE &5 TO &1-quickinfo.
  MOVE &6 TO &1-text.
  MOVE &7 TO &1-disabled.

END-OF-DEFINITION.
DEFINE _set_alv_cell.
  po_object->modify_cell(
    EXPORTING
      i_row_id    = &1
      i_fieldname = &2
      i_value     = &3 ).
END-OF-DEFINITION.
DEFINE _remove_comma.
  _remove_char &1 ','.
END-OF-DEFINITION.
DEFINE _remove_period.
  _remove_char &1 '.'.
END-OF-DEFINITION.
DEFINE _remove_char.
  REPLACE ALL OCCURRENCES OF &2 IN &1 WITH ''.
END-OF-DEFINITION.
DEFINE _set_range.
  CLEAR &1.
  &1-sign = &2.
  &1-option = &3.
  &1-low = &4.
  &1-high = &5.
  APPEND &1.
END-OF-DEFINITION.
DEFINE _set_eq_range.
  CLEAR &1.
  &1-sign = 'I'.
  &1-option = 'EQ'.
  &1-low = &2.
  IF &3 NE ''.
  &1-high = &3.
  ENDIF.
  APPEND &1.
END-OF-DEFINITION.
DEFINE _set_bt_range.
  CLEAR &1.
  &1-sign = 'I'.
  &1-option = 'BT'.
  &1-low = &2.
  IF &3 NE ''.
  &1-high = &3.
  ENDIF.
  APPEND &1.
END-OF-DEFINITION.
DEFINE _set_cp_range.
  CLEAR &1.
  &1-sign = 'I'.
  &1-option = 'CP'.
  &1-low = &2.
  IF &3 NE ''.
  &1-high = &3.
  ENDIF.
  APPEND &1.
END-OF-DEFINITION.
DEFINE _set_range_from_param.
  IF &1 IS NOT INITIAL.
    FIND '*' IN &1.
    IF sy-subrc EQ 0.
      _set_cp_range &2 &1 ''.
    ELSE.
      _set_eq_range &2 &1 ''.
  ENDIF.
ENDIF.
END-OF-DEFINITION.
DEFINE _set_last_day_range.
  &1-sign = 'I'.
  &1-option = 'BT'.
   CALL FUNCTION 'LAST_DAY_OF_MONTHS'
     EXPORTING
       day_in            = &1-low
     IMPORTING
       last_day_of_month = &1-high
     EXCEPTIONS
       day_in_no_date    = 1
       OTHERS            = 2.
  APPEND &1.
END-OF-DEFINITION.
DEFINE _clear.
  CLEAR : &1, &1[].
END-OF-DEFINITION.

DEFINE _set_date_format.
  CLEAR gv_date_format.
  gv_date_format = &1(4) && '.' && &1+4(2) && '.' && &1+6(2).
END-OF-DEFINITION.
DEFINE _set_time_format.
  CLEAR gv_time_format.
  gv_time_format = &1(2) && '.' && &1+2(2) && '.' && &1+4(2).
END-OF-DEFINITION.
DEFINE _set_number_format.
  CLEAR gv_number_format.

  CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
    EXPORTING
      betrg                   = &1
      new_decimal_separator   = '.'
      new_thousands_separator = ','
    IMPORTING
      string                  = &2.
  CONDENSE &2 NO-GAPS.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  ADD_FILE_OPEN_DIALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_FILE  text
*----------------------------------------------------------------------*
FORM add_excel_file_open_dialog  USING    pv_file.

  DATA: lt_file TYPE filetable,
        ls_file LIKE LINE OF lt_file,
        lv_rc   TYPE i.
  CLEAR : pv_file.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = 'Select Excel File'
      file_filter       = 'Excel files (*.XLSX)|*.XLSX| Excel files (*.XLS)|*.XLS|'
      default_extension = 'xlsx'
    CHANGING
      file_table        = lt_file
      rc                = lv_rc.

  IF  sy-subrc EQ 0.
    READ TABLE lt_file INTO ls_file INDEX 1.
    MOVE  ls_file-filename   TO  pv_file.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SMW0_FILE_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*      -->P_0028   text
*      -->P_0029   text
*----------------------------------------------------------------------*
FORM smw0_file_download  USING  pv_filename pv_fileid pv_ext   .


  DATA : lv_path TYPE string.
  DATA : ls_item         TYPE wwwdatatab,
         lv_file         TYPE localfile,
         lv_destinamtion TYPE localfile,
         lv_ext          TYPE string,
         lv_filename     TYPE string.
  DATA : rc TYPE i.
  DATA : lv_fullpath TYPE string,
         lv_result   TYPE i.

  TRY.

      lv_ext = pv_ext.
      lv_filename = pv_filename.
      lv_file = pv_fileid.
      IF lv_ext IS INITIAL.
        lv_ext = 'xls'.
      ENDIF.


      SELECT SINGLE relid objid srtf2 checkout checknew
        chname tdate ttime text clustr clustd
      FROM wwwdata
      INTO CORRESPONDING FIELDS OF ls_item
      WHERE objid = lv_file.

      IF ls_item-relid IS INITIAL.
        MESSAGE 'No File Found' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF lv_filename IS INITIAL.
        lv_filename = ls_item-text.
      ENDIF.

      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          default_extension = lv_ext
          default_file_name = lv_filename
          initial_directory = 'C:\'
        CHANGING
          filename          = lv_filename
          path              = lv_path
          fullpath          = lv_fullpath
          user_action       = lv_result.

      IF  sy-subrc EQ 0.
        lv_destinamtion  = lv_fullpath.
      ENDIF.

      CALL FUNCTION 'WS_FILE_DELETE'
        EXPORTING
*         FILE   = LV_FILE
          file   = lv_destinamtion
        IMPORTING
          return = rc.


      CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
        EXPORTING
          key         = ls_item
          destination = lv_destinamtion
        CHANGING
          temp        = lv_file.

  ENDTRY.
  IF sy-subrc NE 0.
    MESSAGE 'No File Found' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  itab_to_excel_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->
*----------------------------------------------------------------------*
FORM itab_to_excel_download TABLES pt_tab pt_head USING pv_filename pv_path .

  TYPES : BEGIN OF lty_header,
            fieldname TYPE fieldname,
          END OF lty_header.

  DATA: lo_gui    TYPE REF TO cl_gui_frontend_services,
        lv_title  TYPE string,
        lv_folder TYPE string,
        lv_dir    TYPE string.

  DATA lo_table_descr TYPE REF TO cl_abap_tabledescr.
  DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.
  DATA lt_columns TYPE abap_compdescr_tab.
  FIELD-SYMBOLS <ls_column> LIKE LINE OF lt_columns.
  DATA : lt_head TYPE TABLE OF lty_header,
         ls_head TYPE lty_header,
         lv_head TYPE string.
  DATA : lv_filename TYPE string.
**  1. FILE NAME
  IF pv_filename IS INITIAL.
    lv_filename = 'SAMPLE.XLS'.
  ELSE.
    CONCATENATE pv_filename '.XLS'  INTO lv_filename.
  ENDIF.

** 2. HEADER

  lo_table_descr ?= cl_abap_typedescr=>describe_by_data( pt_tab[] ).
  lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
  lt_columns = lo_struct_descr->components.

  IF pt_head[] IS INITIAL.
    LOOP AT lt_columns ASSIGNING <ls_column>.
      CLEAR  ls_head.
      ls_head = <ls_column>-name.
      APPEND ls_head TO lt_head.
    ENDLOOP.
  ELSE.
    LOOP AT pt_head INTO lv_head.
      APPEND lv_head TO lt_head.
    ENDLOOP.
  ENDIF.

  IF pv_path IS INITIAL.
    PERFORM get_file_save_dialog USING lv_filename lv_dir '' ''.
  ELSE.
    lv_dir = pv_path.
  ENDIF.
  IF lv_dir IS INITIAL.
    MESSAGE 'Operation Cancled.' TYPE 'I'.
  ELSE.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_dir
        filetype                = 'ASC'
        write_field_separator   = 'X'
      TABLES
        data_tab                = pt_tab
        fieldnames              = lt_head
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
      MESSAGE 'Error on generating sample file' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE 'File saved' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_save_dialog USING pv_filename pv_path pv_extension pv_title.
  DATA : lv_fullpath  TYPE string,
         lv_directory TYPE string,
         lv_title     TYPE string,
         lv_extension TYPE string.

  lv_directory = 'C:/'.

  IF pv_title IS INITIAL.
    lv_title = 'FILE SAVE'.
  ELSE.
    lv_title = pv_title.
  ENDIF.
  IF pv_extension IS INITIAL.
    lv_extension = 'xlsx'.
  ELSE.
    lv_extension = pv_extension.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = lv_title
      default_extension = lv_extension
      default_file_name = pv_filename
      initial_directory = lv_directory
    CHANGING
      filename          = pv_filename
      path              = pv_path
      fullpath          = lv_fullpath.

  IF sy-subrc EQ 0.
    pv_path  = lv_fullpath.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_layout_variant USING pv_alv_key
                               pv_save
                         CHANGING pv_vari .
  CLEAR : gs_alv_variant.

  gs_alv_variant-report   = sy-repid.
  gs_alv_variant-username = sy-uname.
  gs_alv_variant-handle   = pv_alv_key.

  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = pv_save
    CHANGING
      cs_variant    = gs_alv_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc = 0.
    pv_vari = gs_alv_variant-variant.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_layout_variant USING pv_alv_key
                              pv_save
                        CHANGING pv_vari .

  CLEAR : gs_alv_variant.

* ALV Layout variant
  IF NOT pv_vari IS INITIAL.
    MOVE : sy-repid   TO gs_alv_variant-report,
           sy-uname   TO gs_alv_variant-username,
           pv_vari    TO gs_alv_variant-variant,
           pv_alv_key TO gs_alv_variant-handle.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save        = pv_save
      CHANGING
        cs_variant    = gs_alv_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3.
  ELSE.

    PERFORM init_layout_variant USING pv_alv_key pv_save CHANGING pv_vari.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_LOCAL_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_BALV_ERDAT  text
*      -->P_GT_BALV_ERZET  text
*----------------------------------------------------------------------*
FORM get_local_time  USING    pv_date
                              pv_time.

  IF sy-zonlo IS NOT INITIAL AND c_tz_system <> sy-zonlo."local time zone이 세팅 된 경우만
    CONVERT DATE pv_date TIME pv_time
    INTO TIME STAMP gv_timestamp TIME ZONE c_tz_system.

    CONVERT TIME STAMP gv_timestamp TIME ZONE sy-zonlo
    INTO DATE pv_date TIME pv_time.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SYSTEM_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_BALV_ERDAT  text
*      -->P_GT_BALV_ERZET  text
*----------------------------------------------------------------------*
FORM get_system_time  USING    pv_date
                               pv_time.
  IF sy-zonlo IS NOT INITIAL AND c_tz_system <> sy-zonlo."local time zone이 세팅 된 경우만
    CONVERT DATE pv_date TIME pv_time
    INTO TIME STAMP gv_timestamp TIME ZONE sy-zonlo.

    CONVERT TIME STAMP gv_timestamp TIME ZONE c_tz_system
    INTO DATE pv_date TIME pv_time.
  ELSE.
    CONVERT DATE pv_date TIME pv_time
    INTO TIME STAMP gv_timestamp TIME ZONE c_tz_dhtc.

    CONVERT TIME STAMP gv_timestamp TIME ZONE c_tz_system
    INTO DATE pv_date TIME pv_time.
  ENDIF.
ENDFORM.
