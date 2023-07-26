*----------------------------------------------------------------------*
***INCLUDE ZLBRD00020F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_DOUBLE_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ERR  text
*----------------------------------------------------------------------*
FORM check_double_input  USING    pv_err.
  CLEAR pv_err.
  go_alv_h->check_changed_data( ).
  go_alv_i->check_changed_data( ).

  LOOP AT gt_double_alv.
    CHECK sy-tabix > 1.
    PERFORM check_tabnm USING gt_double_alv-tabnm pv_err.
    IF pv_err EQ abap_true.
      EXIT.
    ENDIF.

    PERFORM check_alv_key USING gt_double_alv-alv_key pv_err.
    IF pv_err EQ abap_true.
      EXIT.
    ENDIF.

    PERFORM check_fcat_key USING gt_double_alv-alv_key gt_double_alv-fcat_key pv_err.
    IF pv_err EQ abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9110 OUTPUT.
  SET PF-STATUS '9110'.
  SET TITLEBAR '9110'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen_9110 OUTPUT.
  CHECK go_splitter_container_db IS INITIAL.
  CREATE OBJECT go_splitter_container_db
    EXPORTING
      parent  = cl_gui_container=>default_screen
      rows    = 2
      columns = 1.
  go_container_h = go_splitter_container_db->get_container( row = 1 column = 1 ) .
  go_container_i = go_splitter_container_db->get_container( row = 2 column = 1 ) .
  go_splitter_container_db->set_row_height( id = 1 height = 30 ).

  CREATE OBJECT go_alv_h
    EXPORTING
      io_parent   = go_container_h
      iv_progid   = sy-cprog
      iv_alv_key  = 'HEADER'
      iv_fcat_key = 'DEFAULT'.
  gt_toolbar_all_excluding = go_alv_h->get_all_exclude_toolbar( ).
  go_alv_h->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                               it_toolbar_excluding = gt_toolbar_all_excluding
                      CHANGING ct_data = gt_double_alv[] ).

  CASE abap_true.
    WHEN pa_s3.
      CREATE OBJECT go_alv_i
        EXPORTING
          io_parent   = go_container_i
          iv_progid   = sy-cprog
          iv_alv_key  = 'ITEM'
          iv_fcat_key = 'SIMPLE'.
    WHEN pa_s4.
      CREATE OBJECT go_alv_i
        EXPORTING
          io_parent   = go_container_i
          iv_progid   = sy-cprog
          iv_alv_key  = 'ITEM'
          iv_fcat_key = 'DEFAULT'.
  ENDCASE.
  go_alv_i->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                               it_toolbar_excluding = gt_toolbar_all_excluding
                     CHANGING ct_data = gt_key_map[] ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen_9200 OUTPUT.

  PERFORM get_source_code.

  CHECK go_splitter_container IS INITIAL.
  CREATE OBJECT go_splitter_container
    EXPORTING
      parent  = cl_gui_container=>default_screen
      rows    = 2
      columns = 1.
  go_container_title = go_splitter_container->get_container( row = 1 column = 1 ) .
  go_container_body = go_splitter_container->get_container( row = 2 column = 1 ) .

  go_splitter_container->set_row_height( id = 1 height = 25 ).

  CREATE OBJECT go_splitter_container_editor
    EXPORTING
      parent  = go_container_body
      rows    = 2
      columns = 2.

  go_container1 = go_splitter_container_editor->get_container( row = 1 column = 2 ).
  CREATE OBJECT go_textedit_top
    EXPORTING
      parent = go_container1.
  go_container2 = go_splitter_container_editor->get_container( row = 1 column = 1 ).
  CREATE OBJECT go_textedit_main
    EXPORTING
      parent = go_container2.
  go_container3 = go_splitter_container_editor->get_container( row = 2 column = 1 ).
  CREATE OBJECT go_textedit_f01
    EXPORTING
      parent = go_container3.
  go_container4 = go_splitter_container_editor->get_container( row = 2 column = 2 ).
  CREATE OBJECT go_textedit_100
    EXPORTING
      parent = go_container4.


  go_textedit_top->set_text( table = gt_top ).
  go_textedit_main->set_text( table = gt_main ).
  go_textedit_f01->set_text( table = gt_f01 ).
  go_textedit_100->set_text( table = gt_100 ).

  CREATE OBJECT go_document.
  PERFORM set_title.
  go_document->display_document( parent = go_container_title ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  header_alv_key_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header_alv_key_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           alv_key  LIKE ztbrd00020-alv_key,
           fcat_key LIKE ztbrd00020-fcat_key,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.

*  go_alv_h->check_changed_data( ).

  READ TABLE gt_double_alv INDEX es_row_no-row_id.

  SELECT DISTINCT alv_key fcat_key
  INTO CORRESPONDING FIELDS OF TABLE lt_search_field
  FROM ztbrd00020
  WHERE progid = pa_prog.
  go_alv_h->get_f4_value( EXPORTING iv_fieldname = 'ALV_KEY'
                                    it_data = lt_search_field[]
                          RECEIVING rv_value = lv_value ).

  CHECK lv_value IS NOT INITIAL.
  gv_flag = go_alv_h->is_display_mode( ).
  IF gv_flag = abap_false.
    CLEAR lt_search_field.
    READ TABLE lt_search_field WITH KEY alv_key = lv_value.
    gt_double_alv-alv_key = lt_search_field-alv_key.
    gt_double_alv-fcat_key = lt_search_field-fcat_key.

    MODIFY gt_double_alv INDEX es_row_no-row_id TRANSPORTING alv_key fcat_key.
    go_alv_h->refresh( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  header_fcat_key_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header_fcat_key_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           alv_key  LIKE ztbrd00020-alv_key,
           fcat_key LIKE ztbrd00020-fcat_key,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.

*  go_alv_h->check_changed_data( ).

  READ TABLE gt_double_alv INDEX es_row_no-row_id.

  SELECT DISTINCT alv_key fcat_key
  INTO CORRESPONDING FIELDS OF TABLE lt_search_field
  FROM ztbrd00020
  WHERE progid = pa_prog.

  go_alv_h->get_f4_value( EXPORTING iv_fieldname = 'FCAT_KEY'
                                    it_data = lt_search_field[]
                          RECEIVING rv_value = lv_value ).

  CHECK lv_value IS NOT INITIAL.
  gv_flag = go_alv_h->is_display_mode( ).
  IF gv_flag = abap_false.
    gt_double_alv-fcat_key = lv_value.
    MODIFY gt_double_alv INDEX es_row_no-row_id TRANSPORTING fcat_key.
    go_alv_h->refresh( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_h_field_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM item_h_field_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           fieldname LIKE dd03l-fieldname,
           position  LIKE dd03l-position,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.

  go_alv_h->check_changed_data( ).

  READ TABLE gt_key_map INDEX es_row_no-row_id.
  CLEAR gt_double_alv.
  READ TABLE gt_double_alv INDEX 1.

  SELECT DISTINCT fieldname position
  INTO CORRESPONDING FIELDS OF TABLE lt_search_field
  FROM dd03l
  WHERE tabname = gt_double_alv-tabnm
    AND as4local = 'A'
    AND comptype <> 'S'
    AND fieldname NE 'MANDT'
    ORDER BY position.
  CHECK lt_search_field[] IS NOT INITIAL.



  go_alv_i->get_f4_value( EXPORTING iv_fieldname = 'FIELDNAME'
                                    it_data = lt_search_field[]
                          RECEIVING rv_value = lv_value ).

  CHECK lv_value IS NOT INITIAL.
  gv_flag = go_alv_i->is_display_mode( ).
  IF gv_flag = abap_false.
    gt_key_map-h_field = lv_value.
    MODIFY gt_key_map INDEX es_row_no-row_id TRANSPORTING h_field.
    go_alv_i->refresh( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_i_field_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM item_i_field_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           fieldname LIKE dd03l-fieldname,
           position  LIKE dd03l-position,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.

  go_alv_h->check_changed_data( ).

  READ TABLE gt_key_map INDEX es_row_no-row_id.
  CLEAR gt_double_alv.
  READ TABLE gt_double_alv INDEX 2.

  SELECT DISTINCT fieldname position
  INTO CORRESPONDING FIELDS OF TABLE lt_search_field
  FROM dd03l
  WHERE tabname = gt_double_alv-tabnm
    AND as4local = 'A'
    AND comptype <> 'S'
    AND fieldname NE 'MANDT'
  ORDER BY position.
  CHECK lt_search_field[] IS NOT INITIAL.


  go_alv_i->get_f4_value( EXPORTING iv_fieldname = 'FIELDNAME'
                                    it_data = lt_search_field[]
                          RECEIVING rv_value = lv_value ).

  CHECK lv_value IS NOT INITIAL.
  gv_flag = go_alv_i->is_display_mode( ).
  IF gv_flag = abap_false.
    gt_key_map-i_field = lv_value.
    MODIFY gt_key_map INDEX es_row_no-row_id TRANSPORTING i_field.
    go_alv_i->refresh( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_TABNM_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_tabnm_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                                pv_row_id
                                pv_value.
  READ TABLE gt_double_alv INDEX pv_row_id.
  IF sy-subrc EQ 0.
    gt_double_alv-tabnm = pv_value.
    MODIFY gt_double_alv INDEX pv_row_id.
  ENDIF.
ENDFORM.                    "` ls_fcat-fieldname `_CHANGED.
*&---------------------------------------------------------------------*
*&      Form  GET_DOUBLE_SRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_double_src .
*  DATA : lo_acg_double TYPE REF TO zcl_BRD_acg_double,
*         lo_acg_simple TYPE REF TO zcl_BRD_acg_double_simple.
  DATA : ls_header LIKE LINE OF gt_double_alv,
         ls_item   LIKE LINE OF gt_double_alv.
  DATA : lt_top  TYPE zybrd00100,
         lt_main TYPE zybrd00100,
         lt_f01  TYPE zybrd00100,
         lt_100  TYPE zybrd00100.
  READ TABLE gt_double_alv INTO ls_header INDEX 1.
  READ TABLE gt_double_alv INTO ls_item INDEX 2.


  SELECT SINGLE * INTO gs_alv_h_config
  FROM ztbrd00010
  WHERE progid = pa_prog
    AND alv_key = ls_header-alv_key.

  SELECT SINGLE * INTO gs_alv_i_config
    FROM ztbrd00010
    WHERE progid = pa_prog
      AND alv_key = ls_item-alv_key.


  SELECT * INTO TABLE gt_alv_h_fcat
    FROM ztbrd00020
    WHERE progid = pa_prog
      AND alv_key = ls_header-alv_key
      AND fcat_key = ls_header-fcat_key
    AND fieldname NE 'BALV_STATUS_ICON'
*      AND fieldname NE 'MANDT'
    .

  SELECT * INTO TABLE gt_alv_i_fcat
    FROM ztbrd00020
    WHERE progid = pa_prog
      AND alv_key = ls_item-alv_key
      AND fcat_key = ls_item-fcat_key
    AND fieldname NE 'BALV_STATUS_ICON'
*      AND fieldname NE 'MANDT'
    .

  CASE abap_true.
    WHEN pa_s3.
      CALL FUNCTION 'ZBRD_GET_SRC_DOUBLE_SIMPLE'
        DESTINATION 'BRD'
        EXPORTING
          iv_progid             = pa_prog
          iv_l_alv_key          = ls_header-tabnm
          iv_l_fcat_key         = ls_header-alv_key
          iv_l_tabname          = ls_header-fcat_key
          iv_r_alv_key          = ls_item-tabnm
          iv_r_fcat_key         = ls_item-alv_key
          iv_r_tabname          = ls_item-fcat_key
          iv_alv_class          = gv_alv_class
          is_alv_l_config       = gs_alv_h_config
          is_alv_r_config       = gs_alv_i_config
          iv_new                = pa_new
        TABLES
          t_top                 = lt_top
          t_main                = lt_main
          t_f01                 = lt_f01
          t_100                 = lt_100
          t_key_map             = gt_key_map[]
          t_alv_l_fcat          = gt_alv_h_fcat[]
          t_alv_r_fcat          = gt_alv_i_fcat[]
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          resource_failure      = 3
          license_expired       = 4.
      CASE sy-subrc.
        WHEN 0.
        WHEN 4.
          MESSAGE 'ACG License Expired' TYPE c_i DISPLAY LIKE c_e.
        WHEN OTHERS.
          MESSAGE 'RFC Call Error' TYPE c_i DISPLAY LIKE c_e.
      ENDCASE.


      MOVE lt_top[] TO gt_top[].
      MOVE lt_main[] TO gt_main[].
      MOVE lt_f01[] TO gt_f01[].
      MOVE lt_100[] TO gt_100[].

    WHEN pa_s4.
      CALL FUNCTION 'ZBRD_GET_SRC_DOUBLE'
        DESTINATION 'BRD'
        EXPORTING
          iv_progid             = pa_prog
          iv_h_alv_key          = ls_header-tabnm
          iv_h_fcat_key         = ls_header-alv_key
          iv_h_tabname          = ls_header-fcat_key
          iv_i_alv_key          = ls_item-tabnm
          iv_i_fcat_key         = ls_item-alv_key
          iv_i_tabname          = ls_item-fcat_key
          iv_alv_class          = gv_alv_class
          is_alv_h_config       = gs_alv_h_config
          is_alv_i_config       = gs_alv_i_config
          iv_new                = pa_new
        TABLES
          t_top                 = lt_top
          t_main                = lt_main
          t_f01                 = lt_f01
          t_100                 = lt_100
          t_key_map             = gt_key_map[]
          t_alv_h_fcat          = gt_alv_h_fcat[]
          t_alv_i_fcat          = gt_alv_i_fcat[]
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          resource_failure      = 3
          license_expired       = 4.
      CASE sy-subrc.
        WHEN 0.
        WHEN 4.
          MESSAGE 'ACG License Expired' TYPE c_i DISPLAY LIKE c_e.
        WHEN OTHERS.
          MESSAGE 'RFC Call Error' TYPE c_i DISPLAY LIKE c_e.
      ENDCASE.


      MOVE lt_top[] TO gt_top[].
      MOVE lt_main[] TO gt_main[].
      MOVE lt_f01[] TO gt_f01[].
      MOVE lt_100[] TO gt_100[].

  ENDCASE.
ENDFORM.
