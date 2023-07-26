CLASS zcl_brd_file DEFINITION
  PUBLIC
  INHERITING FROM zcl_brd_util
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS add_file_open_dialog
      IMPORTING
        !iv_filter      TYPE string DEFAULT '*.*'
        !iv_extension   TYPE string DEFAULT ''
        !iv_title       TYPE string DEFAULT 'Select File'
      CHANGING
        !cs_file        TYPE zsbrd00050
      RETURNING
        VALUE(rv_subrc) TYPE sy-subrc .
    CLASS-METHODS split_path
      CHANGING
        !cs_file_struc TYPE zsbrd00050 .
    CLASS-METHODS add_file_save_dialog
      IMPORTING
        !iv_extension   TYPE string DEFAULT ''
        !iv_title       TYPE string DEFAULT 'File save'
        !iv_directory   TYPE string DEFAULT 'C:/'
      CHANGING
        !cv_filename    TYPE string OPTIONAL
        !cv_path        TYPE string OPTIONAL
        !cv_fullpath    TYPE string
      RETURNING
        VALUE(rv_subrc) TYPE sy-subrc .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BRD_FILE IMPLEMENTATION.


  METHOD add_file_open_dialog.
    DATA: lt_file TYPE filetable,
          ls_file LIKE LINE OF lt_file,
          lv_rc   TYPE i.
    CLEAR : cs_file.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title      = iv_title
        file_filter       = iv_filter
        default_extension = iv_extension
      CHANGING
        file_table        = lt_file
        rc                = lv_rc.

    IF  sy-subrc EQ 0 AND lv_rc EQ 1.
      READ TABLE lt_file INTO ls_file INDEX 1.
      MOVE  ls_file-filename   TO  cs_file-fullname.

      split_path( CHANGING cs_file_struc = cs_file ).
      rv_subrc = 0.
    ELSE.
      rv_subrc = 8.
    ENDIF.
  ENDMETHOD.


  METHOD add_file_save_dialog.
    DATA: lv_fullpath TYPE string,
          lv_path     TYPE string,
          lv_rc       TYPE i.


    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = iv_title
        default_extension = iv_extension
        default_file_name = cv_filename
        initial_directory = iv_directory
      CHANGING
        filename          = cv_filename
        path              = lv_path
        fullpath          = lv_fullpath
        user_action       = lv_rc.


    IF  sy-subrc EQ 0 AND lv_rc EQ 0 AND lv_fullpath IS NOT INITIAL.
      cv_fullpath = lv_fullpath.
      cv_path = lv_path.
      rv_subrc = 0.
    ELSE.
      rv_subrc = 8.
    ENDIF.
  ENDMETHOD.


  METHOD split_path.


    DATA: lv_path(1024),
          lv_directory     TYPE c LENGTH 510,
          lv_filename      TYPE c LENGTH 510,
          lv_title         TYPE c LENGTH 510,
          lv_filenames     TYPE string,
          lv_extension(05),
          lv_size          TYPE i.

* File 명과 Directiroy 분리
    lv_path = cs_file_struc-fullname.
    CALL FUNCTION 'CV120_SPLIT_PATH'
      EXPORTING
        pf_path  = lv_path
      IMPORTING
        pfx_path = lv_directory
        pfx_file = lv_filename.

    cs_file_struc-directory = lv_directory.
    cs_file_struc-filename  = lv_filename.


*  확장자.
    CALL FUNCTION 'CV120_SPLIT_FILE'
      EXPORTING
        pf_file       = lv_filename
      IMPORTING
        pfx_file      = lv_title
        pfx_extension = lv_extension.

    cs_file_struc-title = lv_title.
    TRANSLATE lv_extension TO UPPER CASE.
    cs_file_struc-extension = lv_extension.

* File Size .

    lv_filenames = lv_path.
    CALL METHOD cl_gui_frontend_services=>file_get_size
      EXPORTING
        file_name            = lv_filenames
      IMPORTING
        file_size            = lv_size
      EXCEPTIONS
        file_get_size_failed = 1
        cntl_error           = 2
        error_no_gui         = 3
        not_supported_by_gui = 4
        OTHERS               = 5.

    cs_file_struc-doc_len = lv_size.

    CALL METHOD cl_gui_cfw=>flush.


  ENDMETHOD.
ENDCLASS.
