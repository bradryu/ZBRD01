*----------------------------------------------------------------------*
***INCLUDE ZLBRD00020F04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9130 OUTPUT.
  SET PF-STATUS '9110'.
  SET TITLEBAR '9130'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN_0140  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen_9140 OUTPUT.
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
      io_parent  = go_container_h
      iv_progid  = sy-cprog
      iv_alv_key = 'TGHD'.
  gt_toolbar_all_excluding = go_alv_h->get_all_exclude_toolbar( ).
  go_alv_h->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                               it_toolbar_excluding = gt_toolbar_all_excluding
                      CHANGING ct_data = gt_double_alv[] ).


  CREATE OBJECT go_alv_i
    EXPORTING
      io_parent  = go_container_i
      iv_progid  = sy-cprog
      iv_alv_key = 'ITEM'.

  go_alv_i->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                               it_toolbar_excluding = gt_toolbar_all_excluding
                     CHANGING ct_data = gt_key_map[] ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  TGTD_alv_key_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tgtd_alv_key_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           alv_key  LIKE ztbrd00020-alv_key,
           fcat_key LIKE ztbrd00020-fcat_key,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.
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
    READ TABLE lt_search_field WITH KEY alv_key = lv_value.
    gt_double_alv-alv_key = lt_search_field-alv_key.
    gt_double_alv-fcat_key = lt_search_field-fcat_key.

    MODIFY gt_double_alv INDEX es_row_no-row_id TRANSPORTING alv_key fcat_key.
    go_alv_h->refresh( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TGTD_fcat_key_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tgtd_fcat_key_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           alv_key  LIKE ztbrd00020-alv_key,
           fcat_key LIKE ztbrd00020-fcat_key,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.
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
*&      Form  TGTD_TABNM_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tgtd_tabnm_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                                pv_row_id
                                pv_value.
  READ TABLE gt_double_alv INDEX pv_row_id.
  IF sy-subrc EQ 0.
    gt_double_alv-tabnm = pv_value.
    MODIFY gt_double_alv INDEX pv_row_id.
  ENDIF.
ENDFORM.                    "` ls_fcat-fieldname `_CHANGED.
*&---------------------------------------------------------------------*
*&      Form  GET_TREE_SRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tree_src .

  DATA : lt_top  TYPE zybrd00100,
         lt_main TYPE zybrd00100,
         lt_f01  TYPE zybrd00100,
         lt_100  TYPE zybrd00100.


  SELECT SINGLE * INTO gs_talv_config
    FROM ztbrd00011
    WHERE progid = pa_prog
      AND alv_key = pa_alv.

  SELECT * INTO TABLE gt_talv_fcat
    FROM ztbrd00021
    WHERE progid = pa_prog
      AND alv_key = pa_alv
      AND fcat_key = pa_fcat
*        AND fieldname NE 'MANDT'
    .


  CALL FUNCTION 'ZBRD_GET_SRC_TREE'
    DESTINATION 'BRD'
    EXPORTING
      iv_progid     = pa_prog
      iv_alv_key    = pa_alv
      iv_fcat_key   = pa_fcat
      iv_tabname    = pa_tabnm
      iv_alv_class  = gv_tree_alv_class
      is_alv_config = gs_talv_config
    TABLES
      t_top         = lt_top
      t_main        = lt_main
      t_f01         = lt_f01
      t_100         = lt_100
      t_alv_fcat    = gt_talv_fcat[].

  MOVE lt_top[] TO gt_top[].
  MOVE lt_main[] TO gt_main[].
  MOVE lt_f01[] TO gt_f01[].
  MOVE lt_100[] TO gt_100[].

*  DATA : lo_acg        TYPE REF TO zcl_BRD_acg_tree.
*  CREATE OBJECT lo_acg
*    EXPORTING
*      iv_tabname   = pa_tabnm
*      iv_progid    = pa_prog
*      iv_alv_key   = pa_alv
*      iv_fcat_key  = pa_fcat
*      iv_alv_class = gv_tree_alv_class.
*  lo_acg->get_top_src( IMPORTING et_src = gt_top ).
*  lo_acg->get_main_src( IMPORTING et_src = gt_main ).
*  lo_acg->get_f01_src( IMPORTING et_src = gt_f01 ).
*  lo_acg->get_100_src( IMPORTING et_src = gt_100 ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_TREE_GRID_SRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tree_grid_src .

  DATA : ls_header LIKE LINE OF gt_double_alv,
         ls_item   LIKE LINE OF gt_double_alv.

  DATA : lt_top  TYPE zybrd00100,
         lt_main TYPE zybrd00100,
         lt_f01  TYPE zybrd00100,
         lt_100  TYPE zybrd00100.


  CASE abap_true.
    WHEN pa_s5.


      SELECT SINGLE * INTO gs_alv_config
        FROM ztbrd00010
        WHERE progid = pa_prog
          AND alv_key = gs_tree_grid-g_alv_key.

      SELECT * INTO TABLE gt_alv_fcat
        FROM ztbrd00020
        WHERE progid = pa_prog
          AND alv_key = gs_tree_grid-g_alv_key
          AND fcat_key = gs_tree_grid-g_fcat_key
          AND fieldname NE 'BALV_STATUS_ICON'
*        AND fieldname NE 'MANDT'
        .

      SELECT SINGLE * INTO gs_talv_config
        FROM ztbrd00011
        WHERE progid = pa_prog
          AND alv_key = gs_tree_grid-t_alv_key.

      SELECT * INTO TABLE gt_talv_fcat
        FROM ztbrd00021
        WHERE progid = pa_prog
          AND alv_key = gs_tree_grid-t_alv_key
          AND fcat_key = gs_tree_grid-t_fcat_key
          AND fieldname NE 'BALV_STATUS_ICON'
*        AND fieldname NE 'MANDT'
        .

      CALL FUNCTION 'ZBRD_GET_SRC_TREE_GRID'
        DESTINATION 'BRD'
        EXPORTING
          iv_progid             = pa_tabnm
          iv_alv_key            = pa_prog
          iv_fcat_key           = gs_tree_grid-g_alv_key
          iv_tabname            = gs_tree_grid-g_fcat_key
          iv_alv_class          = gs_tree_grid-g_cls
          iv_alv_key_tree       = gs_tree_grid-t_alv_key
          iv_fcat_key_tree      = gs_tree_grid-t_fcat_key
          iv_alv_class_tree     = gs_tree_grid-t_cls
          is_alv_config         = gs_alv_config
          is_talv_config        = gs_talv_config
          iv_new                = pa_new
        TABLES
          t_top                 = lt_top
          t_main                = lt_main
          t_f01                 = lt_f01
          t_100                 = lt_100
          t_alv_fcat            = gt_alv_fcat[]
          t_talv_fcat           = gt_talv_fcat[]
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
    WHEN pa_s6.
      READ TABLE gt_double_alv INTO ls_header INDEX 1.
      READ TABLE gt_double_alv INTO ls_item INDEX 2.


      SELECT SINGLE * INTO gs_talv_config
        FROM ztbrd00011
        WHERE progid = pa_prog
          AND alv_key = ls_header-alv_key.

      SELECT SINGLE * INTO gs_alv_config
        FROM ztbrd00010
        WHERE progid = pa_prog
          AND alv_key = ls_item-alv_key.


      SELECT * INTO TABLE gt_talv_fcat
        FROM ztbrd00021
        WHERE progid = pa_prog
          AND alv_key = ls_header-alv_key
          AND fcat_key = ls_header-fcat_key
        AND fieldname NE 'BALV_STATUS_ICON'
*      AND fieldname NE 'MANDT'
        .
      SELECT * INTO TABLE gt_alv_fcat
        FROM ztbrd00020
        WHERE progid = pa_prog
          AND alv_key = ls_item-alv_key
          AND fcat_key = ls_item-fcat_key
        AND fieldname NE 'BALV_STATUS_ICON'
*      AND fieldname NE 'MANDT'
        .



      CALL FUNCTION 'ZBRD_GET_SRC_TREE_DOUBLE'
        DESTINATION 'BRD'
        EXPORTING
          iv_progid             = pa_prog
          iv_h_alv_key          = ls_header-alv_key
          iv_h_fcat_key         = ls_header-fcat_key
          iv_h_alv_class        = ls_header-clsname
          iv_h_tabname          = ls_header-tabnm
          is_h_config           = gs_talv_config
          iv_i_alv_key          = ls_item-alv_key
          iv_i_fcat_key         = ls_item-fcat_key
          iv_i_tabname          = ls_item-tabnm
          iv_i_alv_class        = ls_item-clsname
          is_i_config           = gs_alv_config
          iv_new                = pa_new
        TABLES
          t_top                 = lt_top
          t_main                = lt_main
          t_f01                 = lt_f01
          t_100                 = lt_100
          t_key_map             = gt_key_map[]
          t_i_fcat              = gt_alv_fcat[]
          t_h_fcat              = gt_talv_fcat[]
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
