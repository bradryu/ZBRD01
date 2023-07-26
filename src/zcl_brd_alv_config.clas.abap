CLASS zcl_brd_alv_config DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    DATA:
      mr_prog     TYPE RANGE OF ztbrd00010-progid .
    DATA:
      mr_alv_key  TYPE RANGE OF ztbrd00010-alv_key .
    DATA:
      mr_fcat_key TYPE RANGE OF ztbrd00020-fcat_key .
    DATA:
      mv_copy_type(5) .
    DATA mc_sysid_dev TYPE sy-sysid VALUE 'BRD' ##NO_TEXT.
    DATA mc_sysid_qa TYPE sy-sysid VALUE 'QA' ##NO_TEXT.
    DATA mc_sysid_prd TYPE sy-sysid VALUE 'PRD' ##NO_TEXT.
    DATA mc_rfc_dest_qa TYPE rfcdest VALUE 'BRD' ##NO_TEXT.
    DATA mc_rfc_dest_prd TYPE rfcdest VALUE 'QA' ##NO_TEXT.
    DATA mc_rfc_dest_dev TYPE rfcdest VALUE 'PRD' ##NO_TEXT.

    METHODS clear_itab .
    METHODS constructor .
    METHODS copy_save_click .
    METHODS display_color .
    METHODS display_copy .
    METHODS display_header
      IMPORTING
        !io_container_header TYPE REF TO cl_gui_container .
    METHODS display_item
      IMPORTING
        !io_container_item TYPE REF TO cl_gui_container .
    METHODS display_source .
    METHODS display_template .
    METHODS excel_download .
    METHODS generate_source_fcat .
    METHODS get_header_data .
    METHODS get_item_data
      IMPORTING
        !iv_prog    TYPE cprog
        !iv_alv_key TYPE char10 .
    METHODS header_addr_click .
    METHODS header_canc_click .
    METHODS header_changed
      IMPORTING
        !iv_column TYPE fieldname
        !iv_value  TYPE any
        !iv_row_id TYPE int4 .
    METHODS header_chan_click .
    METHODS header_crea_click .
    METHODS header_dele_click .
    METHODS header_delr_click .
    METHODS header_double_click
      IMPORTING
        !is_row    TYPE lvc_s_row
        !is_column TYPE lvc_s_col
        !is_row_no TYPE lvc_s_roid .
    METHODS header_save_click .
    METHODS header_upld_click .
    METHODS item_ad10_click .
    METHODS item_addr_click .
    METHODS item_canc_click .
    METHODS item_changed
      IMPORTING
        !iv_column TYPE fieldname
        !iv_value  TYPE any
        !iv_row_id TYPE int4 .
    METHODS item_chan_click .
    METHODS item_crea_click .
    METHODS item_dele_click .
    METHODS item_delr_click .
    METHODS item_double_click
      IMPORTING
        !is_row    TYPE lvc_s_row
        !is_column TYPE lvc_s_col
        !is_row_no TYPE lvc_s_roid .
    METHODS item_prt_click .
    METHODS item_save_click .
    METHODS item_set_toolbar .
    METHODS item_upld_click .
    METHODS search_copy_source
      IMPORTING
        !iv_progid   TYPE char40
        !iv_alv_key  TYPE char10
        !iv_fcat_key TYPE char10 .
    METHODS search_structure
      IMPORTING
        !iv_strucnm TYPE tabname .
    METHODS set_item_color
      IMPORTING
        !is_row    TYPE lvc_s_row
        !is_column TYPE lvc_s_col
        !is_row_no TYPE lvc_s_roid .
    METHODS set_selected_item_row
      IMPORTING
        !is_row_no TYPE lvc_s_roid .
    METHODS source_save_click .
    METHODS template_save_click .
    METHODS unique_field_changed
      IMPORTING
        !iv_fieldname TYPE fieldname
        !iv_value     TYPE lvc_value
        !iv_row_id    TYPE i .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF gty_color,
        col    TYPE lvc_col,
        int    TYPE lvc_int,
        inv    TYPE lvc_inv,
        coltab TYPE lvc_t_scol,
      END OF gty_color .

    CONSTANTS:
      mc_default_key(10) VALUE 'DEFAULT' ##NO_TEXT.
    DATA mo_alv_header TYPE REF TO zcl_brd_alv .
    DATA mo_alv_item TYPE REF TO zcl_brd_alv .
    DATA mo_alv_template TYPE REF TO zcl_brd_alv .
    DATA mo_alv_copy TYPE REF TO zcl_brd_alv .
    DATA mo_alv_source TYPE REF TO zcl_brd_alv .
    DATA mo_alv_color TYPE REF TO zcl_brd_alv .
    DATA mo_abapedit TYPE REF TO cl_gui_abapedit .
    DATA mo_container_editor TYPE REF TO cl_gui_custom_container .
    DATA:
      mv_header_alv_key(10) .
    DATA:
      mv_item_alv_key(10) .
    DATA:
      mv_template_alv_key(10) .
    DATA:
      mv_copy_alv_key(10) .
    DATA:
      mv_source_alv_key(10) .
    DATA:
      mv_selected_alv_key(10) .
    DATA mv_selected_prog_id TYPE cprog .
    DATA mo_exception TYPE REF TO cx_root .
    DATA mt_rows TYPE lvc_t_row .
    DATA ms_rows TYPE lvc_s_row .
    DATA mt_row_ids TYPE lvc_t_roid .
    DATA ms_row_id TYPE lvc_s_roid .
    DATA ms_color TYPE gty_color .
    DATA:
      mt_color         TYPE TABLE OF gty_color .
    DATA ms_selected_item TYPE lvc_s_roid .
    DATA mt_ddl TYPE lvc_t_dral .
    DATA ms_ddl TYPE lvc_s_dral .
    DATA:
      BEGIN OF ms_edit_flag,
        crea_edit, chan_edit, upld_edit,
      END OF ms_edit_flag .

    METHODS set_header_data
      IMPORTING
        !it_tab TYPE REF TO data .
    METHODS set_item_data
      IMPORTING
        !it_tab TYPE REF TO data .
    METHODS generate_source
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .
    METHODS set_pretty_printer
      IMPORTING
        !iv_fieldname TYPE fieldname
      CHANGING
        !cs_fields    TYPE any .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BRD_ALV_CONFIG IMPLEMENTATION.


  METHOD clear_itab.
  ENDMETHOD.


  METHOD constructor.
    CLEAR : mv_header_alv_key,
            mv_item_alv_key,
            mv_template_alv_key,
            mv_copy_alv_key.
    ms_ddl-handle = 1.
    ms_ddl-value = 'A : A Mod'.
    ms_ddl-int_value = 'A'.
    APPEND ms_ddl TO mt_ddl.
    ms_ddl-handle = 1.
    ms_ddl-value = 'B : B Mod'.
    ms_ddl-int_value = 'B'.
    APPEND ms_ddl TO mt_ddl.
    ms_ddl-handle = 1.
    ms_ddl-value = 'C : C Mod'.
    ms_ddl-int_value = 'C'.
    APPEND ms_ddl TO mt_ddl.
    ms_ddl-handle = 1.
    ms_ddl-value = 'D : D Mod'.
    ms_ddl-int_value = 'D'.
    APPEND ms_ddl TO mt_ddl.


    ms_ddl-handle = 2.
    ms_ddl-value = 'C : Center'.
    ms_ddl-int_value = 'C'.
    APPEND ms_ddl TO mt_ddl.
    ms_ddl-handle = 2.
    ms_ddl-value = 'L : Left'.
    ms_ddl-int_value = 'L'.
    APPEND ms_ddl TO mt_ddl.
    ms_ddl-handle = 2.
    ms_ddl-value = 'R : Right'.
    ms_ddl-int_value = 'R'.
    APPEND ms_ddl TO mt_ddl.


    ms_ddl-handle = 3.
    ms_ddl-value = 'X : Grand total'.
    ms_ddl-int_value = abap_true.
    APPEND ms_ddl TO mt_ddl.
    ms_ddl-handle = 3.
    ms_ddl-value = 'C : Average'.
    ms_ddl-int_value = 'C'.
    APPEND ms_ddl TO mt_ddl.

    ms_ddl-handle = 4.
    ms_ddl-value = '0 : Single'.
    ms_ddl-int_value = 0.
    APPEND ms_ddl TO mt_ddl.
    ms_ddl-handle = 4.
    ms_ddl-value = '1 : Multi'.
    ms_ddl-int_value = 1.
    APPEND ms_ddl TO mt_ddl.
  ENDMETHOD.


  METHOD copy_save_click.
  ENDMETHOD.


  METHOD display_color.
    DATA : ls_scol  TYPE lvc_s_scol,
           lt_scol  TYPE lvc_t_scol,
           lv_index TYPE i.
    DATA lt_toolbar TYPE ui_functions.

    IF mt_color[] IS INITIAL.
      DO 7 TIMES.
        ls_scol-color-col = sy-index.
        DO 2 TIMES.
          lv_index = sy-index - 1.
          ls_scol-color-int = lv_index.
          DO 2 TIMES.
            lv_index = sy-index - 1.
            IF ls_scol-color-int = 1 AND ls_scol-color-inv = 1.
              CONTINUE.
            ENDIF.
            ls_scol-color-inv = lv_index.
            APPEND ls_scol TO lt_scol.
            ms_color-col   = ls_scol-color-col.
            ms_color-int   = ls_scol-color-int.
            ms_color-inv   = ls_scol-color-inv.
            ms_color-coltab = lt_scol.
            APPEND ms_color TO mt_color.
            CLEAR lt_scol.
          ENDDO.
        ENDDO.
      ENDDO.
    ENDIF.

    IF mo_alv_color IS INITIAL.
      CREATE OBJECT mo_alv_color
        EXPORTING
          iv_progid  = sy-cprog
          iv_alv_key = 'COLOR'
          iv_parent  = 'COLOR_CONTAINER'.
    ENDIF.

    lt_toolbar = mo_alv_color->get_all_exclude_toolbar( ).
    mo_alv_color->display( EXPORTING it_toolbar_excluding = lt_toolbar
                           CHANGING ct_data = mt_color[] ).
  ENDMETHOD.


  METHOD display_copy.
    IF mo_alv_copy IS INITIAL.
      CREATE OBJECT mo_alv_copy
        EXPORTING
          iv_parent   = 'GO_COPY'
          iv_progid   = sy-cprog
          iv_alv_key  = mv_copy_alv_key
          iv_fcat_key = mc_default_key.
    ENDIF.

  ENDMETHOD.


  METHOD display_header.
  ENDMETHOD.


  METHOD display_item.
  ENDMETHOD.


  METHOD display_source.
    DATA : lt_code   TYPE TABLE OF char255,
           lv_result TYPE i,
           lv_tabix  TYPE sy-tabix.

    IF mo_abapedit IS INITIAL.
      CREATE OBJECT mo_container_editor
        EXPORTING
          container_name = 'GO_EDITOR'.
      CREATE OBJECT mo_abapedit
        EXPORTING
          parent = mo_container_editor.
    ELSE.
      CLEAR lt_code[].
      mo_abapedit->get_text( IMPORTING table = lt_code[]
                                   is_modified = lv_result
                            ).
    ENDIF.
    IF lt_code[] IS INITIAL.
      APPEND 'DATA : BEGIN OF LT_TAB OCCURS 0,' TO lt_code.
      APPEND '**여기에 필드를 정의 하세요 ' TO lt_code.
      APPEND '* BELNR TYPE BSAK-BELNR,' TO lt_code.
      APPEND '* GJAHR   TYPE GJAHR,' TO lt_code.
      APPEND '* TXT(20),' TO lt_code.
      APPEND '* NODE_ID    TYPE C LENGTH 32,' TO lt_code.
      APPEND '* PNODE_ID   TYPE C LENGTH 32,' TO lt_code.
      APPEND '* NODE_TXT   TYPE C LENGTH 60,' TO lt_code.
      APPEND '* ZSORT      TYPE INT4,' TO lt_code.
      APPEND '         END OF LT_TAB.' TO lt_code.
    ENDIF.
    mo_abapedit->set_text( lt_code[] ).

    IF mo_alv_source IS INITIAL.
      CREATE OBJECT mo_alv_source
        EXPORTING
          iv_parent   = 'GO_SOURCE'
          iv_progid   = sy-cprog
          iv_alv_key  = mv_source_alv_key
          iv_fcat_key = mc_default_key.
    ENDIF.

  ENDMETHOD.


  METHOD display_template.
    IF mo_alv_template IS INITIAL.
      CREATE OBJECT mo_alv_template
        EXPORTING
          iv_parent   = 'GO_TEMPLATE'
          iv_progid   = sy-cprog
          iv_alv_key  = mv_template_alv_key
          iv_fcat_key = mc_default_key.
    ENDIF.
*    IF mo_alv_template IS INITIAL.
*      mo_alv_template = NEW ZCL_BRD_ALV(
*                                   iv_parent = 'GO_TEMPLATE'
*                                   iv_progid = sy-cprog
*                                   iv_alv_key = mv_template_alv_key
*                                   iv_fcat_key = mc_default_key
*                                  ).
*    ENDIF.
  ENDMETHOD.


  METHOD excel_download.
  ENDMETHOD.


  METHOD generate_source.
    DATA : lt_text   TYPE TABLE OF char255,
           lv_text   TYPE char255,
           lv_result TYPE i,
           lv_tabix  TYPE sy-tabix.
    DATA : lt_fcat TYPE lvc_t_fcat,
           ls_fcat TYPE lvc_s_fcat.
    DATA: lt_code    TYPE TABLE OF char72,
          lv_prog(8).
    DATA : lv_line     TYPE sy-index,
           lv_off      TYPE sy-tabix,
           lv_word(30).
    DATA: BEGIN OF ls_message,
            line1(72),
            line2(72),
            line3(72),
          END OF ls_message.
    FIELD-SYMBOLS <ls_fcat> TYPE lvc_s_fcat.
    mo_abapedit->get_text( IMPORTING table = lt_text[]
                                     is_modified = lv_result
       ).
    CHECK lt_text[] IS NOT INITIAL.

    APPEND 'PROGRAM YTMP_SUBPOOL.' TO lt_code.
    APPEND 'FORM get_itab_fcat TABLES pt_tab STRUCTURE lvc_s_fcat.'
    TO lt_code.
    LOOP AT lt_text INTO lv_text.
      APPEND lv_text TO lt_code.
      CLEAR lv_text.
    ENDLOOP.
    APPEND '  DATA:' TO lt_code.
    APPEND '    lo_columns      TYPE REF TO cl_salv_columns_table,' TO
lt_code.
    APPEND '    lo_aggregations TYPE REF TO cl_salv_aggregations,' TO
lt_code.
    APPEND '    lo_salv_table   TYPE REF TO cl_salv_table,' TO lt_code.
    APPEND '    lr_table        TYPE REF TO data.' TO lt_code.
    APPEND '  FIELD-SYMBOLS:' TO lt_code.
    APPEND '    <lt_table>         TYPE STANDARD TABLE.' TO lt_code.
    APPEND '  DATA lt_fcat         TYPE lvc_t_fcat.' TO lt_code.
    APPEND '  CREATE DATA lr_table like lt_tab[].' TO lt_code.
    APPEND '  ASSIGN lr_table->* TO <lt_table>.' TO lt_code.
    APPEND '  TRY.' TO lt_code.
    APPEND '      cl_salv_table=>factory(' TO lt_code.
    APPEND '        EXPORTING' TO lt_code.
    APPEND '          list_display = abap_false' TO lt_code.
    APPEND '        IMPORTING' TO lt_code.
    APPEND '          r_salv_table = lo_salv_table' TO lt_code.
    APPEND '        CHANGING' TO lt_code.
    APPEND '          t_table      = <lt_table> ).' TO lt_code.
    APPEND '    CATCH cx_salv_msg.' TO lt_code.
    APPEND '  ENDTRY.' TO lt_code.
    APPEND '  lo_columns  = lo_salv_table->get_columns( ).' TO lt_code.
    APPEND '  lo_aggregations = lo_salv_table->get_aggregations( ).'
TO lt_code.
    APPEND '  lt_fcat =' TO lt_code.
    APPEND '    cl_salv_controller_metadata=>get_lvc_fieldcatalog(' TO
lt_code.
    APPEND '      r_columns             = lo_columns' TO lt_code.
    APPEND '      r_aggregations        = lo_aggregations ).' TO
lt_code.
    APPEND '  APPEND LINES OF lt_fcat TO pt_tab.' TO lt_code.
    APPEND 'ENDFORM.' TO lt_code.


    CLEAR ls_message.
    SYNTAX-CHECK FOR lt_code
    MESSAGE     ls_message
    LINE         lv_line
    OFFSET       lv_off
    WORD         lv_word.

    IF ls_message IS NOT INITIAL.
      CLEAR lv_text.
      lv_line = lv_line - 3.
      lv_text = lv_line.
      CONCATENATE lv_text '째 라인 오류=>' ls_message-line1
      ls_message-line2 ls_message-line3 INTO lv_text.
      MESSAGE lv_text TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    GENERATE SUBROUTINE POOL lt_code NAME lv_prog.

    IF sy-subrc NE 0.
      MESSAGE 'Code Generating Error' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    PERFORM get_itab_fcat IN PROGRAM (lv_prog) IF FOUND TABLES lt_fcat.
    CLEAR lv_tabix.
    LOOP AT lt_fcat ASSIGNING <ls_fcat>.
      ADD 1 TO lv_tabix.
      <ls_fcat>-col_pos = lv_tabix.
      IF <ls_fcat>-fieldname EQ 'MANDT'.
        <ls_fcat>-tech = abap_true.
        <ls_fcat>-col_pos = 999.
      ENDIF.
      IF <ls_fcat>-datatype = 'CURR'.
        CLEAR : <ls_fcat>-ref_table, <ls_fcat>-ref_field.
      ENDIF.
      IF <ls_fcat>-datatype IS INITIAL.
        CASE <ls_fcat>-inttype.
          WHEN 'N'. <ls_fcat>-datatype = 'NUMC'.
          WHEN 'C'. <ls_fcat>-datatype = 'CHAR'.
          WHEN 'P'. <ls_fcat>-datatype = 'DEC'.
          WHEN 'D'. <ls_fcat>-datatype = 'DATS'.
          WHEN 'F'. <ls_fcat>-datatype = 'FLTP'.
          WHEN 'X'.
            CASE <ls_fcat>-intlen.
              WHEN 1. <ls_fcat>-datatype = 'INT1'.
              WHEN 2. <ls_fcat>-datatype = 'INT2'.
              WHEN 4. <ls_fcat>-datatype = 'INT4'.
              WHEN OTHERS. <ls_fcat>-datatype = 'RAW'.
            ENDCASE.
          WHEN 'T'.  <ls_fcat>-datatype = 'TIMS'.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    DATA : lt_str_tab   TYPE TABLE OF string,
           lv_str       TYPE string,
           lv_ref_table TYPE string,
           lv_ref_field TYPE string,
           lv_variable  TYPE string.

    LOOP AT lt_text INTO lv_text.
      CONDENSE lv_text.
      CHECK lv_text <> ''.
      TRANSLATE lv_text TO UPPER CASE.
      REPLACE ALL OCCURRENCES OF ',' IN lv_text WITH ''.
      CLEAR lt_str_tab.
      SPLIT lv_text AT space INTO TABLE lt_str_tab.
      DELETE lt_str_tab WHERE table_line = ''.
      LOOP AT lt_str_tab INTO lv_str.
        IF sy-tabix = 1.
          CLEAR : lv_variable.
          lv_variable = lv_str.
        ENDIF.
        CLEAR : lv_ref_table, lv_ref_field.
        SPLIT lv_str AT '-' INTO lv_ref_table lv_ref_field.
        IF sy-subrc EQ 0 AND lv_ref_field IS NOT INITIAL.
          CLEAR ls_fcat.
          READ TABLE lt_fcat INTO ls_fcat WITH KEY fieldname =
lv_variable.
          lv_tabix = sy-tabix.
          IF sy-subrc EQ 0 AND ls_fcat-datatype NE 'CURR'.
            ls_fcat-ref_table = lv_ref_table.
            ls_fcat-ref_field = lv_ref_field.
            MODIFY lt_fcat FROM ls_fcat INDEX lv_tabix TRANSPORTING
ref_table ref_field.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    rt_fcat[] = lt_fcat[].
*    DATA : lt_text   TYPE TABLE OF char255,
*           lv_text   TYPE char255,
*           lv_result TYPE i,
*           lv_tabix  TYPE sy-tabix.
*    DATA : lt_fcat TYPE lvc_t_fcat,
*           ls_fcat TYPE lvc_s_fcat.
*    DATA: lt_code    TYPE TABLE OF char72,
*          lv_prog(8).
*    DATA : lv_line     TYPE sy-index,
*           lv_off      TYPE sy-tabix,
*           lv_word(30).
*    DATA: BEGIN OF ls_message,
*            line1(72),
*            line2(72),
*            line3(72),
*          END OF ls_message.
*
*    mo_abapedit->get_text( IMPORTING table = lt_text[]
*                                     is_modified = lv_result
*       ).
*    CHECK lt_text[] IS NOT INITIAL.
*
*    APPEND 'PROGRAM YTMP_SUBPOOL.' TO lt_code.
*    APPEND 'FORM get_itab_fcat TABLES pt_tab STRUCTURE lvc_s_fcat.' TO lt_code.
*    LOOP AT lt_text INTO lv_text.
*      APPEND lv_text TO lt_code.
*      CLEAR lv_text.
*    ENDLOOP.
*    APPEND '  DATA:' TO lt_code.
*    APPEND '    lo_columns      TYPE REF TO cl_salv_columns_table,' TO lt_code.
*    APPEND '    lo_aggregations TYPE REF TO cl_salv_aggregations,' TO lt_code.
*    APPEND '    lo_salv_table   TYPE REF TO cl_salv_table,' TO lt_code.
*    APPEND '    lr_table        TYPE REF TO data.' TO lt_code.
*    APPEND '  FIELD-SYMBOLS:' TO lt_code.
*    APPEND '    <lt_table>         TYPE STANDARD TABLE.' TO lt_code.
*    APPEND '  DATA lt_fcat         TYPE lvc_t_fcat.' TO lt_code.
*    APPEND '  CREATE DATA lr_table like lt_tab[].' TO lt_code.
*    APPEND '  ASSIGN lr_table->* TO <lt_table>.' TO lt_code.
*    APPEND '  TRY.' TO lt_code.
*    APPEND '      cl_salv_table=>factory(' TO lt_code.
*    APPEND '        EXPORTING' TO lt_code.
*    APPEND '          list_display = abap_false' TO lt_code.
*    APPEND '        IMPORTING' TO lt_code.
*    APPEND '          r_salv_table = lo_salv_table' TO lt_code.
*    APPEND '        CHANGING' TO lt_code.
*    APPEND '          t_table      = <lt_table> ).' TO lt_code.
*    APPEND '    CATCH cx_salv_msg.' TO lt_code.
*    APPEND '  ENDTRY.' TO lt_code.
*    APPEND '  lo_columns  = lo_salv_table->get_columns( ).' TO lt_code.
*    APPEND '  lo_aggregations = lo_salv_table->get_aggregations( ).' TO lt_code.
*    APPEND '  lt_fcat =' TO lt_code.
*    APPEND '    cl_salv_controller_metadata=>get_lvc_fieldcatalog(' TO lt_code.
*    APPEND '      r_columns             = lo_columns' TO lt_code.
*    APPEND '      r_aggregations        = lo_aggregations ).' TO lt_code.
*    APPEND '  APPEND LINES OF lt_fcat TO pt_tab.' TO lt_code.
*    APPEND 'ENDFORM.' TO lt_code.
*
*
*    CLEAR ls_message.
*    SYNTAX-CHECK FOR lt_code
*    MESSAGE     ls_message
*    LINE         lv_line
*    OFFSET       lv_off
*    WORD         lv_word.
*
*    IF ls_message IS NOT INITIAL.
*      CLEAR lv_text.
*      lv_line = lv_line - 3.
*      lv_text = lv_line && '째 라인 오류=>' && ls_message-line1 && ls_message-line2 && ls_message-line3.
*      MESSAGE lv_text TYPE 'I' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*
*    GENERATE SUBROUTINE POOL lt_code NAME lv_prog.
*
*    IF sy-subrc NE 0.
*      MESSAGE 'Code Generating Error' TYPE 'I' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*
*    PERFORM get_itab_fcat IN PROGRAM (lv_prog) IF FOUND TABLES lt_fcat.
*    CLEAR lv_tabix.
*    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
*      ADD 1 TO lv_tabix.
*      <ls_fcat>-col_pos = lv_tabix.
*      IF <ls_fcat>-fieldname EQ 'MANDT'.
*        <ls_fcat>-tech = abap_true.
*        <ls_fcat>-col_pos = 999.
*      ENDIF.
*      IF <ls_fcat>-datatype IS INITIAL.
*        CASE <ls_fcat>-inttype.
*          WHEN 'N'. <ls_fcat>-datatype = 'NUMC'.
*          WHEN 'C'. <ls_fcat>-datatype = 'CHAR'.
*          WHEN 'P'. <ls_fcat>-datatype = 'DEC'.
*          WHEN 'D'. <ls_fcat>-datatype = 'DATS'.
*          WHEN 'F'. <ls_fcat>-datatype = 'FLTP'.
*          WHEN 'X'.
*            CASE <ls_fcat>-intlen.
*              WHEN 1. <ls_fcat>-datatype = 'INT1'.
*              WHEN 2. <ls_fcat>-datatype = 'INT2'.
*              WHEN 4. <ls_fcat>-datatype = 'INT4'.
*              WHEN OTHERS. <ls_fcat>-datatype = 'RAW'.
*            ENDCASE.
*          WHEN 'T'.  <ls_fcat>-datatype = 'TIMS'.
*        ENDCASE.
*      ENDIF.
*    ENDLOOP.
*
*    DATA : lt_str_tab   TYPE TABLE OF string,
*           lv_str       TYPE string,
*           lv_ref_table TYPE string,
*           lv_ref_field TYPE string,
*           lv_variable  TYPE string.
*
*    LOOP AT lt_text INTO lv_text.
*      CONDENSE lv_text.
*      CHECK lv_text <> ''.
*      TRANSLATE lv_text TO UPPER CASE.
*      REPLACE ALL OCCURRENCES OF ',' IN lv_text WITH ''.
*      CLEAR lt_str_tab.
*      SPLIT lv_text AT space INTO TABLE lt_str_tab.
*      DELETE lt_str_tab WHERE table_line = ''.
*      LOOP AT lt_str_tab INTO lv_str.
*        IF sy-tabix = 1.
*          CLEAR : lv_variable.
*          lv_variable = lv_str.
*        ENDIF.
*        CLEAR : lv_ref_table, lv_ref_field.
*        SPLIT lv_str AT '-' INTO lv_ref_table lv_ref_field.
*        IF sy-subrc EQ 0 AND lv_ref_field IS NOT INITIAL.
*          CLEAR ls_fcat.
*          READ TABLE lt_fcat INTO ls_fcat WITH KEY fieldname = lv_variable.
*          lv_tabix = sy-tabix.
*          IF sy-subrc EQ 0.
*            ls_fcat-ref_table = lv_ref_table.
*            ls_fcat-ref_field = lv_ref_field.
*            MODIFY lt_fcat FROM ls_fcat INDEX lv_tabix TRANSPORTING ref_table ref_field.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*    rt_fcat[] = lt_fcat[].
  ENDMETHOD.


  METHOD generate_source_fcat.
  ENDMETHOD.


  METHOD get_header_data.

  ENDMETHOD.


  METHOD get_item_data.

  ENDMETHOD.


  METHOD header_addr_click.
  ENDMETHOD.


  METHOD header_canc_click.
  ENDMETHOD.


  METHOD header_changed.
  ENDMETHOD.


  METHOD header_chan_click.
  ENDMETHOD.


  METHOD header_crea_click.
  ENDMETHOD.


  METHOD header_dele_click.
  ENDMETHOD.


  METHOD header_delr_click.
  ENDMETHOD.


  METHOD header_double_click.
  ENDMETHOD.


  METHOD header_save_click.
  ENDMETHOD.


  METHOD header_upld_click.
  ENDMETHOD.


  METHOD item_ad10_click.
  ENDMETHOD.


  METHOD item_addr_click.
  ENDMETHOD.


  METHOD item_canc_click.
  ENDMETHOD.


  METHOD item_changed.
  ENDMETHOD.


  METHOD item_chan_click.
  ENDMETHOD.


  METHOD item_crea_click.
  ENDMETHOD.


  METHOD item_dele_click.
  ENDMETHOD.


  METHOD item_delr_click.
  ENDMETHOD.


  METHOD item_double_click.
  ENDMETHOD.


  METHOD item_prt_click.
  ENDMETHOD.


  METHOD item_save_click.
  ENDMETHOD.


  METHOD item_set_toolbar.
    DATA lv_flag TYPE flag.
    lv_flag = mo_alv_item->is_create_mode( ).
    IF lv_flag = abap_true.
      mo_alv_item->insert_toolbar_button( EXPORTING
                                      iv_function = 'AD10'
                                      iv_icon = icon_insert_row
                                      iv_quickinfo = 'Add 10Rows'
                                      iv_text = 'Add 10Rows'
                                      iv_index = 2
                                      ).
    ENDIF.
    lv_flag = mo_alv_item->is_display_mode( ).
    IF lv_flag = abap_false.
      IF sy-sysid EQ mc_sysid_dev.
        mo_alv_item->append_toolbar_button( EXPORTING
                              iv_function = 'PRT'
*                              iv_icon = icon_table_settings
                              iv_quickinfo = 'Pretty Printer'
                              iv_text = 'Pretty Printer'
                                            ).

        mo_alv_item->append_toolbar_button(
        EXPORTING iv_butn_type  = '3' ).
        mo_alv_item->append_toolbar_button( EXPORTING
                              iv_function = 'REF'
                              iv_icon = icon_table_settings
                              iv_quickinfo = 'Structure Fieldcatalog'
                              iv_text = 'Structure Fieldcat'
                                            ).
        mo_alv_item->append_toolbar_button( EXPORTING
                              iv_function = 'COPY'
                              iv_icon = icon_system_copy
                              iv_quickinfo = 'Copy Fieldcatalog'
                              iv_text = 'Copy Fieldcat'
                                            ).
        mo_alv_item->append_toolbar_button( EXPORTING
                              iv_function = 'SRC'
                              iv_icon = icon_create_text
                              iv_quickinfo = 'ABAP Code Fieldcatalog'
                              iv_text = 'Code Fieldcat'
                                            ).
        mo_alv_item->append_toolbar_button(
        EXPORTING iv_butn_type  = '3' ).

        mo_alv_item->append_toolbar_button( EXPORTING
                              iv_function = 'QA'
                              iv_icon = icon_system_copy
                              iv_quickinfo = 'Get QA Fieldcatalog'
                              iv_text = 'Get QA Fieldcat'
                                            ).
        mo_alv_item->append_toolbar_button( EXPORTING
                              iv_function = 'PRD'
                              iv_icon = icon_system_copy
                              iv_quickinfo = 'Get PRD Fieldcatalog'
                              iv_text = 'Get PRD Fieldcat'
                                            ).
      ELSE.
        mo_alv_item->append_toolbar_button(
        EXPORTING iv_butn_type  = '3' ).
        mo_alv_item->append_toolbar_button( EXPORTING
                              iv_function = 'DEV'
                              iv_icon = icon_system_copy
                              iv_quickinfo = 'Get DEV Fieldcatalog'
                              iv_text = 'Get DEV Fieldcat'
                                            ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD item_upld_click.
  ENDMETHOD.


  METHOD search_copy_source.

  ENDMETHOD.


  METHOD search_structure.
  ENDMETHOD.


  METHOD set_header_data.
  ENDMETHOD.


  METHOD set_item_color.
  ENDMETHOD.


  METHOD set_item_data.

  ENDMETHOD.


  METHOD set_pretty_printer.
    DEFINE _set_value.
      ASSIGN COMPONENT &1 OF STRUCTURE cs_fields TO <lv_value>.
      <lv_value> = &2.
    END-OF-DEFINITION.

    FIELD-SYMBOLS : <lv_value>    TYPE any,
                    <lv_datatype> TYPE any,
                    <lv_inttype>  TYPE any,
                    <lv_intlen>   TYPE any.
    CASE iv_fieldname.
      WHEN 'ERDAT'.
        _set_value : 'SCRTEXT_S' 'Create Date',
                     'SCRTEXT_M' 'Create Date',
                     'SCRTEXT_L' 'Create Date',
                     'REPTEXT' 'Create Date',
                     'JUST' 'C',
                     'CREA_EDIT' '',
                     'CHAN_EDIT' '',
                     'UPLD_EDIT' ''.
      WHEN 'ERZET'.
        _set_value : 'SCRTEXT_S' 'Create Time',
                     'SCRTEXT_M' 'Create Time',
                     'SCRTEXT_L' 'Create Time',
                     'REPTEXT' 'Create Time',
                     'JUST' 'C',
                     'CREA_EDIT' '',
                     'CHAN_EDIT' '',
                     'UPLD_EDIT' ''.
      WHEN 'ERNAM'.
        _set_value : 'SCRTEXT_S' 'Create User',
                     'SCRTEXT_M' 'Create User',
                     'SCRTEXT_L' 'Create User',
                     'REPTEXT' 'Create User',
                     'JUST' 'C',
                     'CREA_EDIT' '',
                     'CHAN_EDIT' '',
                     'UPLD_EDIT' ''.
      WHEN 'AEDAT'.
        _set_value : 'SCRTEXT_S' 'Change Date',
                     'SCRTEXT_M' 'Change Date',
                     'SCRTEXT_L' 'Change Date',
                     'REPTEXT' 'Change Date',
                     'JUST' 'C',
                     'CREA_EDIT' '',
                     'CHAN_EDIT' '',
                     'UPLD_EDIT' ''.
      WHEN 'AEZET'.
        _set_value : 'SCRTEXT_S' 'Change Time',
                     'SCRTEXT_M' 'Change Time',
                     'SCRTEXT_L' 'Change Time',
                     'REPTEXT' 'Change Time',
                     'JUST' 'C',
                     'CREA_EDIT' '',
                     'CHAN_EDIT' '',
                     'UPLD_EDIT' ''.
      WHEN 'AENAM'.
        _set_value : 'SCRTEXT_S' 'Change User',
                     'SCRTEXT_M' 'Change User',
                     'SCRTEXT_L' 'Change User',
                     'REPTEXT' 'Change User',
                     'JUST' 'C',
                     'CREA_EDIT' '',
                     'CHAN_EDIT' '',
                     'UPLD_EDIT' ''.
      WHEN 'ZDEL'.
        _set_value : 'TECH' 'X'.
      WHEN OTHERS.
        ASSIGN COMPONENT 'JUST' OF STRUCTURE cs_fields TO <lv_value>.
        IF <lv_value> IS INITIAL.
          ASSIGN COMPONENT 'DATATYPE' OF STRUCTURE cs_fields TO <lv_datatype>.
          ASSIGN COMPONENT 'INTTYPE' OF STRUCTURE cs_fields TO <lv_inttype>.
          ASSIGN COMPONENT 'INTLEN' OF STRUCTURE cs_fields TO <lv_intlen>.
          IF <lv_datatype> EQ 'CHAR' AND <lv_inttype> EQ 'C' AND <lv_intlen> <= 20.
            _set_value : 'JUST' 'C'.
          ENDIF.
        ENDIF.
    ENDCASE.
    ASSIGN COMPONENT 'FLAG' OF STRUCTURE cs_fields TO <lv_value>.
    IF <lv_value> EQ 'FLAG'.
      _set_value : 'JUST' 'C',
                   'OUTPUTLEN' 10,
                   'CHECKBOX' abap_true.
    ENDIF.

    ASSIGN COMPONENT 'TECH' OF STRUCTURE cs_fields TO <lv_value>.
    IF <lv_value> EQ 'X'.
      _set_value 'COL_POS' 999.
    ENDIF.

  ENDMETHOD.


  METHOD set_selected_item_row.
    CLEAR ms_selected_item.
    ms_selected_item = is_row_no.
  ENDMETHOD.


  METHOD source_save_click.
  ENDMETHOD.


  METHOD template_save_click.
  ENDMETHOD.


  METHOD unique_field_changed.
  ENDMETHOD.
ENDCLASS.
