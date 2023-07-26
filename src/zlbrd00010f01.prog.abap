*&---------------------------------------------------------------------*
*&  Include           ZLBRD00010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      CLEAR gv_okcode.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_100 INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR gv_okcode.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_okcode.
    WHEN 'DOWN'.
      go_alv_config->excel_download( ).
  ENDCASE.
  CLEAR gv_okcode.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv OUTPUT.
  CHECK go_splitter_container IS INITIAL.
  CREATE OBJECT go_splitter_container
    EXPORTING
      parent  = cl_gui_container=>default_screen
      rows    = 2
      columns = 1.
*  go_splitter_container = NEW cl_gui_splitter_container(
*                              parent  = cl_gui_container=>default_screen
*                              rows    = 2
*                              columns = 1 ).
  go_container_header = go_splitter_container->get_container( row = 1 column = 1 ).
  go_container_item = go_splitter_container->get_container( row = 2 column = 1 ).
  go_splitter_container->set_row_height( id = 1 height = 30 ).

  go_alv_config->display_item( go_container_item ).
  go_alv_config->display_header( go_container_header ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_ALV_TEMPLATE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv_template OUTPUT.
  go_alv_config->display_template( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  IF gv_okcode EQ 'SAVE'.
    go_alv_config->template_save_click( ).
    CLEAR gv_okcode.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_STRUC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_struc INPUT.
  IF gv_okcode IS INITIAL.
    CHECK gv_strucnm IS NOT INITIAL.
    go_alv_config->search_structure( gv_strucnm ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS '0300'.
  SET TITLEBAR '0300'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS '0500'.
  SET TITLEBAR '0500'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_ALV_COPY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv_copy OUTPUT.
  go_alv_config->display_copy( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_ALV_SOURCE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv_source OUTPUT.
  go_alv_config->display_source( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  IF gv_okcode EQ 'SAVE'.
    go_alv_config->copy_save_click( ).
    CLEAR gv_okcode.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  CASE gv_okcode.
    WHEN 'SAVE'.
      go_alv_config->source_save_click( ).
      CLEAR gv_okcode.
      LEAVE TO SCREEN 0.
    WHEN 'GEN_FC'.
      go_alv_config->generate_source_fcat( ).
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_SOURCE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_source INPUT.
  IF gv_okcode IS INITIAL.
    go_alv_config->search_copy_source( EXPORTING iv_progid = gv_progid
                                                   iv_alv_key = gv_alv_key
                                                   iv_fcat_key = gv_fcat_key ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_KEY_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_key_f4 INPUT.

  DATA : lr_progid   TYPE RANGE OF cprog WITH HEADER LINE,
         lr_alv_key  TYPE RANGE OF char10 WITH HEADER LINE,
         lr_fcat_key TYPE RANGE OF char10 WITH HEADER LINE,
         lv_progid   TYPE string,
         lv_alv_key  TYPE string,
         lv_fcat_key TYPE string.
  DATA : BEGIN OF lt_tab OCCURS 0,
           progid   TYPE ztbrd00020-progid,
           alv_key  TYPE ztbrd00020-alv_key,
           fcat_key TYPE ztbrd00020-fcat_key,
         END OF lt_tab.
  DATA : lt_f4_field     TYPE zcl_brd_common=>tt_help_value,
         ls_f4_field     TYPE help_value,
         lt_update_field TYPE dynpread_tabtype,
         ls_update_field TYPE dynpread.

  zcl_brd_common=>get_screen_value( EXPORTING iv_field = 'GV_PROGID' RECEIVING rv_value = lv_progid ).
  zcl_brd_common=>get_screen_value( EXPORTING iv_field = 'GV_ALV_KEY' RECEIVING rv_value = lv_alv_key ).
  zcl_brd_common=>get_screen_value( EXPORTING iv_field = 'GV_FCAT_KEY' RECEIVING rv_value = lv_fcat_key ).

  FIND '*' IN lv_progid.
  IF sy-subrc NE 0.
    CONCATENATE '*' lv_progid '*' INTO lv_progid.
  ENDIF.
  _set_cp_range lr_progid lv_progid ''.
  FIND '*' IN lv_alv_key.
  IF sy-subrc NE 0.
    CONCATENATE '*' lv_alv_key '*' INTO  lv_alv_key.
  ENDIF.
  _set_cp_range lr_alv_key lv_alv_key ''.
  FIND '*' IN lv_fcat_key.
  IF sy-subrc NE 0.
    CONCATENATE '*' lv_fcat_key '*' INTO lv_fcat_key.
  ENDIF.
  _set_cp_range lr_fcat_key lv_fcat_key ''.



  SELECT DISTINCT progid alv_key fcat_key
  INTO TABLE lt_tab
  FROM ztbrd00020 UP TO 200 ROWS
  WHERE progid IN lr_progid
    AND alv_key IN lr_alv_key
    AND fcat_key IN lr_fcat_key.

  CLEAR : lt_f4_field, ls_f4_field.
  ls_f4_field-tabname = 'ZTBRD00020'.
  ls_f4_field-fieldname = 'PROGID'.
  APPEND ls_f4_field TO lt_f4_field.
  CLEAR ls_f4_field.
  ls_f4_field-tabname = 'ZTBRD00020'.
  ls_f4_field-fieldname = 'ALV_KEY'.
  APPEND ls_f4_field TO lt_f4_field.
  CLEAR ls_f4_field.
  ls_f4_field-tabname = 'ZTBRD00020'.
  ls_f4_field-fieldname = 'FCAT_KEY' .
  APPEND ls_f4_field TO lt_f4_field.


  CLEAR : lt_update_field, ls_update_field.
  ls_update_field-fieldname = 'GV_PROGID'.
  ls_update_field-fieldvalue = 'PROGID'.
  APPEND ls_update_field TO lt_update_field.
  CLEAR ls_update_field.
  ls_update_field-fieldname = 'GV_ALV_KEY'.
  ls_update_field-fieldvalue = 'ALV_KEY'.
  APPEND ls_update_field TO lt_update_field.
  CLEAR ls_update_field.
  ls_update_field-fieldname = 'GV_FCAT_KEY'.
  ls_update_field-fieldvalue = 'FCAT_KEY'.
  APPEND ls_update_field TO lt_update_field.

  zcl_brd_common=>get_f4_value( EXPORTING it_tab = lt_tab[]
                                          it_f4_field =  lt_f4_field
                                          it_update_field = lt_update_field
                                 IMPORTING ev_value = gv_progid ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  HEADER_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header_double_click USING e_row TYPE lvc_s_row
                               e_column TYPE lvc_s_col
                               es_row_no TYPE lvc_s_roid.

  go_alv_config->header_double_click( EXPORTING is_row = e_row
                                              is_column = e_column
                                              is_row_no = es_row_no ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_CREA_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_crea_click.
  go_alv_config->header_crea_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_CANC_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_canc_click.
  go_alv_config->header_canc_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_DELE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_dele_click.
  go_alv_config->header_dele_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHAN_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_chan_click.
  go_alv_config->header_chan_click( ).
ENDFORM.                    "CHAN_CLICK.
*&---------------------------------------------------------------------*
*&      Form  DELR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_delr_click.
  go_alv_config->header_delr_click( ).
ENDFORM.                    "DELR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  ADDR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_addr_click.
  go_alv_config->header_addr_click( ).
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  HEADER_SAVE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_save_click.
  go_alv_config->header_save_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_UPLD_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_upld_click.
  go_alv_config->header_upld_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_SET_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_INTERACTIVE     text
*      -->PO_OBJECT  text
*----------------------------------------------------------------------*
FORM item_set_toolbar USING pv_interactive
                       po_object TYPE REF TO cl_alv_event_toolbar_set.
  go_alv_config->item_set_toolbar( ).
ENDFORM.                    "SET_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  item_CREA_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_crea_click.
  go_alv_config->item_crea_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_CANC_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_canc_click.
  go_alv_config->item_canc_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_DELE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_dele_click.
  go_alv_config->item_dele_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHAN_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_chan_click.
  go_alv_config->item_chan_click( ).
ENDFORM.                    "CHAN_CLICK.
*&---------------------------------------------------------------------*
*&      Form  DELR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_delr_click.
  go_alv_config->item_delr_click( ).
ENDFORM.                    "DELR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  ADDR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_addr_click.
  go_alv_config->item_addr_click( ).
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  ADDR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_ad10_click.
  go_alv_config->item_ad10_click( ).
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  item_SAVE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_save_click.
  go_alv_config->item_save_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ITEM_DOUBLE_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_double_click USING e_row TYPE lvc_s_row
                             e_column TYPE lvc_s_col
                             es_row_no TYPE lvc_s_roid.
  go_alv_config->item_double_click( EXPORTING is_row = e_row
                                            is_column = e_column
                                            is_row_no = es_row_no ).
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  REF_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_ref_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key, gv_strucnm.
  go_alv_config->clear_itab( ).
  CALL SCREEN 200 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  COPY_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_copy_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key, gv_strucnm.
  go_alv_config->clear_itab( ).
  go_alv_config->mv_copy_type = 'LOCAL'.
  CALL SCREEN 300 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  COPY_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_dev_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key, gv_strucnm.
  go_alv_config->clear_itab( ).
  go_alv_config->mv_copy_type = go_alv_config->mc_sysid_dev.
  CALL SCREEN 300 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  COPY_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_prd_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key, gv_strucnm.
  go_alv_config->clear_itab( ).
  go_alv_config->mv_copy_type = go_alv_config->mc_sysid_prd.

  CALL SCREEN 300 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  COPY_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_qa_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key, gv_strucnm.
  go_alv_config->clear_itab( ).
  go_alv_config->mv_copy_type = go_alv_config->mc_sysid_qa.
  CALL SCREEN 300 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  SRC_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_src_click.
  go_alv_config->clear_itab( ).
  CALL SCREEN 500 STARTING AT 2 2 ENDING AT 120 20.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SRC_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_prt_click.
  go_alv_config->item_prt_click( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_emphasize_f4 USING es_row_no TYPE lvc_s_roid.
  go_alv_config->set_selected_item_row( es_row_no ).
  CALL SCREEN 400 STARTING AT 80 5 ENDING AT 117 18.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS '0400'.
  SET TITLEBAR '0400'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_ALV_COLOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv_color OUTPUT.
  go_alv_config->display_color( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  HEADER_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM color_double_click USING e_row TYPE lvc_s_row
                              e_column TYPE lvc_s_col
                              es_row_no TYPE lvc_s_roid.

  go_alv_config->set_item_color( EXPORTING is_row = e_row
                                         is_column = e_column
                                         is_row_no = es_row_no ).
  LEAVE TO SCREEN 0.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ITEM_UPLD_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_upld_click.
  go_alv_config->item_upld_click( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  item_col_pos_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM item_col_pos_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                                pv_row_id
                                pv_value TYPE any.
  go_alv_config->item_changed( EXPORTING iv_column = 'COL_POS'
                                         iv_value = pv_value
                                         iv_row_id = pv_row_id  ).
ENDFORM.                    "CARRID_CHANGED.
