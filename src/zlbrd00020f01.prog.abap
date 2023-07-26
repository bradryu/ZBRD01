*&---------------------------------------------------------------------*
*&  Include           ZLBRD00020F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen_9101 OUTPUT.
  CHECK go_custom_container IS INITIAL.
  CREATE OBJECT go_custom_container
    EXPORTING
      container_name = 'CON1'.

  CREATE OBJECT go_splitter_container_db
    EXPORTING
      parent  = go_custom_container
      rows    = 2
      columns = 1.
  go_container_h = go_splitter_container_db->get_container( row = 1 column = 1 ) .
  go_container_i = go_splitter_container_db->get_container( row = 2 column = 1 ) .
  go_splitter_container_db->set_row_height( id = 1 height = 30 ).


  CREATE OBJECT go_alv_h
    EXPORTING
      io_parent   = go_container_h
      iv_progid   = sy-cprog
      iv_alv_key  = 'TAB'
      iv_fcat_key = 'DEFAULT'.
  gt_toolbar_all_excluding = go_alv_h->get_all_exclude_toolbar( ).
  go_alv_h->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                               it_toolbar_excluding = gt_toolbar_all_excluding
                      CHANGING ct_data = gt_double_alv[] ).

  CREATE OBJECT go_alv_i
    EXPORTING
      io_parent   = go_container_i
      iv_progid   = sy-cprog
      iv_alv_key  = 'ITEM'
      iv_fcat_key = 'DEFAULT'.
  go_alv_i->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                               it_toolbar_excluding = gt_toolbar_all_excluding
                     CHANGING ct_data = gt_key_map[] ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_SINGLE_SRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_single_src .
*  DATA : lo_acg      TYPE REF TO zcl_BRD_acg,
*         lo_acg_join TYPE REF TO zcl_BRD_acg_join.
  DATA : lv_join_type,
         lv_tabname_left  TYPE tabname,
         lv_tabname_right TYPE tabname.
  DATA : lt_top  TYPE zybrd00100,
         lt_main TYPE zybrd00100,
         lt_f01  TYPE zybrd00100,
         lt_100  TYPE zybrd00100.
  CASE abap_true.
    WHEN pa_s1.


      SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_alv_config
        FROM ztbrd00010
        WHERE progid = pa_prog
          AND alv_key = pa_alv.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_alv_fcat
        FROM ztbrd00020
        WHERE progid = pa_prog
          AND alv_key = pa_alv
          AND fcat_key = pa_fcat
        AND fieldname NE 'BALV_STATUS_ICON'
*      AND fieldname NE 'MANDT'
        .

      CALL FUNCTION 'ZBRD_GET_SRC_SINGLE'
        DESTINATION 'BRD'
        EXPORTING
          iv_progid             = pa_prog
          iv_alv_key            = pa_alv
          iv_fcat_key           = pa_fcat
          iv_tabname            = pa_tabnm
          iv_alv_class          = gv_alv_class
          is_alv_config         = gs_alv_config
          iv_new                = pa_new
        TABLES
          t_top                 = lt_top
          t_main                = lt_main
          t_f01                 = lt_f01
          t_100                 = lt_100
          t_alv_fcat            = gt_alv_fcat
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


    WHEN pa_s2.
      IF gv_inner_join EQ abap_true.
        lv_join_type = 'I'.
      ELSE.
        lv_join_type = 'O'.
      ENDIF.
      READ TABLE gt_double_alv INDEX 1.
      lv_tabname_left = gt_double_alv-tabnm.
      READ TABLE gt_double_alv INDEX 2.
      lv_tabname_right = gt_double_alv-tabnm.


      CALL FUNCTION 'ZBRD_GET_SRC_JOIN'
        DESTINATION 'BRD'
        EXPORTING
          iv_progid             = pa_prog
          iv_alv_key            = pa_alv
          iv_fcat_key           = pa_fcat
          iv_alv_class          = gv_alv_class
          iv_tabname_left       = lv_tabname_left
          iv_tabname_right      = lv_tabname_right
          iv_join_type          = lv_join_type
          iv_new                = pa_new
        TABLES
          t_top                 = lt_top
          t_main                = lt_main
          t_f01                 = lt_f01
          t_100                 = lt_100
          t_key_map             = gt_key_map[]
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


  ENDCASE.
  MOVE lt_top[] TO gt_top[].
  MOVE lt_main[] TO gt_main[].
  MOVE lt_f01[] TO gt_f01[].
  MOVE lt_100[] TO gt_100[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_JOIN_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ERR  text
*----------------------------------------------------------------------*
FORM check_join_input  USING    pv_err.
  CLEAR pv_err.
  CHECK pa_s2 EQ abap_true.

  go_alv_h->check_changed_data( ).
  go_alv_i->check_changed_data( ).

  LOOP AT gt_double_alv.
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
