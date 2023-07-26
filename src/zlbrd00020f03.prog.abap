*&---------------------------------------------------------------------*
*&  Include           ZLBRD00020F03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9120 OUTPUT.
  SET PF-STATUS '9120'.
  SET TITLEBAR '9120'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen_9120 OUTPUT.
  CHECK go_alv IS INITIAL.
  CHECK go_custom_container IS INITIAL.
  CREATE OBJECT go_custom_container
    EXPORTING
      container_name = 'CON2'.


  CREATE OBJECT go_alv
    EXPORTING
      io_parent   = go_custom_container
      iv_progid   = sy-cprog
      iv_alv_key  = 'MULTI'
      iv_fcat_key = 'DEFAULT'.
  gt_toolbar_all_excluding = go_alv->get_all_exclude_toolbar( ).
  go_alv->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                             it_toolbar_excluding = gt_toolbar_all_excluding
                   CHANGING ct_data = gt_multi_alv[] ).


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_MULTI_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ERR  text
*----------------------------------------------------------------------*
FORM check_multi_input  USING    pv_err.
  go_alv->check_changed_data( ).
  CLEAR pv_err.
  DATA lv_success.
  CLEAR lv_success.
  LOOP AT gt_multi_alv.
    IF gt_multi_alv-tabnm IS INITIAL
      AND gt_multi_alv-alv_key IS INITIAL
      AND gt_multi_alv-fcat_key IS INITIAL.
      DELETE gt_multi_alv.
      CONTINUE.
    ENDIF.

    PERFORM check_tabnm USING gt_multi_alv-tabnm pv_err.
    IF pv_err EQ abap_true.
      EXIT.
    ENDIF.

    PERFORM check_alv_key USING gt_multi_alv-alv_key pv_err.
    IF pv_err EQ abap_true.
      EXIT.
    ENDIF.

    PERFORM check_fcat_key USING gt_multi_alv-alv_key gt_multi_alv-fcat_key pv_err.
    IF pv_err EQ abap_true.
      EXIT.
    ENDIF.
    lv_success = abap_true.
  ENDLOOP.
  IF lv_success NE abap_true.
    MESSAGE 'No data inputed'  TYPE 'I' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  multi_alv_key_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM multi_alv_key_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           alv_key  LIKE ztbrd00020-alv_key,
           fcat_key LIKE ztbrd00020-fcat_key,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.
  READ TABLE gt_multi_alv INDEX es_row_no-row_id.

  SELECT DISTINCT alv_key fcat_key
  INTO CORRESPONDING FIELDS OF TABLE lt_search_field
  FROM ztbrd00020
  WHERE progid = pa_prog.
  go_alv->get_f4_value( EXPORTING iv_fieldname = 'ALV_KEY'
                                  it_data = lt_search_field[]
                        RECEIVING  rv_value = lv_value ).

  CHECK lv_value IS NOT INITIAL.
  gv_flag = go_alv->is_display_mode( ).
  IF gv_flag = abap_false.
    READ TABLE lt_search_field  WITH KEY alv_key = lv_value.
    gt_multi_alv-alv_key = lt_search_field-alv_key.
    gt_multi_alv-fcat_key = lt_search_field-fcat_key.

    MODIFY gt_multi_alv INDEX es_row_no-row_id TRANSPORTING alv_key fcat_key.
    go_alv->refresh( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  multi_fcat_key_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM multi_fcat_key_f4 USING es_row_no TYPE lvc_s_roid  .
  DATA : BEGIN OF lt_search_field OCCURS 0,
           alv_key  LIKE ztbrd00020-alv_key,
           fcat_key LIKE ztbrd00020-fcat_key,
         END OF lt_search_field.
  DATA lv_value TYPE shvalue_d.
  READ TABLE gt_multi_alv INDEX es_row_no-row_id.

  SELECT DISTINCT alv_key fcat_key
  INTO CORRESPONDING FIELDS OF TABLE lt_search_field
  FROM ztbrd00020
  WHERE progid = pa_prog.

  go_alv->get_f4_value( EXPORTING iv_fieldname = 'FCAT_KEY'
                                  it_data = lt_search_field[]
                        RECEIVING rv_value = lv_value ).

  CHECK lv_value IS NOT INITIAL.
  gv_flag = go_alv->is_display_mode( ).
  IF gv_flag = abap_false.
    gt_multi_alv-fcat_key = lv_value.
    MODIFY gt_multi_alv INDEX es_row_no-row_id TRANSPORTING fcat_key.
    go_alv->refresh( ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PO_R_DATA     text
*      -->&QUOT;CHANGE  text
*----------------------------------------------------------------------*
FORM  multi_data_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                        pv_onf4
                        pv_onf4_before
                        pv_onf4_after
                        pv_ucomm.
  DEFINE _set_assign.
    CLEAR : lv_str.

    CONCATENATE '<gs_stab>-' &1 INTO lv_str.
    ASSIGN (lv_str) TO <ls_stab>.
    IF <ls_stab> IS ASSIGNED.
      <ls_stab> = &2.
    ENDIF.
  END-OF-DEFINITION.

  FIELD-SYMBOLS: <ls_stab> LIKE LINE OF gt_multi_alv.
  DATA : lv_str TYPE string.
  DATA lt_mod_cells TYPE lvc_t_modi WITH HEADER LINE.

  lt_mod_cells[] = po_object->mt_good_cells.
  LOOP AT lt_mod_cells.
    AT NEW row_id.
      READ TABLE gt_multi_alv ASSIGNING <ls_stab> INDEX lt_mod_cells-row_id.
    ENDAT.
    ASSIGN COMPONENT lt_mod_cells-fieldname OF STRUCTURE <ls_stab> TO <gv_field>.
    <gv_field> = lt_mod_cells-value.
  ENDLOOP.

ENDFORM.                    "DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  GET_MULTI_SRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_multi_src .

  DATA : lt_top  TYPE zybrd00100,
         lt_main TYPE zybrd00100,
         lt_f01  TYPE zybrd00100,
         lt_100  TYPE zybrd00100.
  DATA : ls_multi_alv  LIKE LINE OF gt_multi_alv,
         lt_alv_config TYPE zybrd00010.


  SELECT * INTO TABLE gt_alv_fcat
  FROM ztbrd00020
    FOR ALL ENTRIES IN gt_multi_alv
  WHERE progid = pa_prog
    AND alv_key = gt_multi_alv-alv_key
    AND fcat_key = gt_multi_alv-fcat_key
*          AND fieldname NE 'MANDT'
    AND fieldname NE 'BALV_STATUS_ICON'.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_alv_config
  FROM ztbrd00010
  WHERE progid = pa_prog.

  CALL FUNCTION 'ZBRD_GET_SRC_MULTI'
    DESTINATION 'BRD'
    EXPORTING
      iv_progid             = pa_prog
      iv_alv_class          = gv_alv_class
      iv_new                = pa_new
    TABLES
      t_top                 = lt_top
      t_main                = lt_main
      t_f01                 = lt_f01
      t_100                 = lt_100
      t_tab_list            = gt_multi_alv[]
      t_alv_fcat            = gt_alv_fcat[]
      t_alv_config          = lt_alv_config[]
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

ENDFORM.
