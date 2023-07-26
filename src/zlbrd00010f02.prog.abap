*&---------------------------------------------------------------------*
*&  Include           ZLBRD00010F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  HEADER_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM theader_double_click USING e_row TYPE lvc_s_row
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
FORM theader_crea_click.
  go_alv_config->header_crea_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_CANC_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_canc_click.
  go_alv_config->header_canc_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_DELE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_dele_click.
  go_alv_config->header_dele_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHAN_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_chan_click.
  go_alv_config->header_chan_click( ).
ENDFORM.                    "CHAN_CLICK.
*&---------------------------------------------------------------------*
*&      Form  DELR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_delr_click.
  go_alv_config->header_delr_click( ).
ENDFORM.                    "DELR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  ADDR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_addr_click.
  go_alv_config->header_addr_click( ).
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  HEADER_SAVE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_save_click.
  go_alv_config->header_save_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_UPLD_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_upld_click.
  go_alv_config->header_upld_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_col_pos_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM theader_db_click_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                                pv_row_id
                                pv_value TYPE any.
  go_alv_config->header_changed( EXPORTING iv_column = 'DB_CLICK'
                                         iv_value = pv_value
                                         iv_row_id = pv_row_id  ).
ENDFORM.                    "CARRID_CHANGED.
*&---------------------------------------------------------------------*
*&      Form  item_SET_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_INTERACTIVE     text
*      -->PO_OBJECT  text
*----------------------------------------------------------------------*
FORM titem_set_toolbar USING pv_interactive
                       po_object TYPE REF TO cl_alv_event_toolbar_set.
  go_alv_config->item_set_toolbar( ).
ENDFORM.                    "SET_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  item_CREA_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_crea_click.
  go_alv_config->item_crea_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_CANC_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_canc_click.
  go_alv_config->item_canc_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_DELE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_dele_click.
  go_alv_config->item_dele_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHAN_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_chan_click.
  go_alv_config->item_chan_click( ).
ENDFORM.                    "CHAN_CLICK.
*&---------------------------------------------------------------------*
*&      Form  DELR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_delr_click.
  go_alv_config->item_delr_click( ).
ENDFORM.                    "DELR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  ADDR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_addr_click.
  go_alv_config->item_addr_click( ).
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  ADDR_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_ad10_click.
  go_alv_config->item_ad10_click( ).
ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form  item_SAVE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_save_click.
  go_alv_config->item_save_click( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ITEM_DOUBLE_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_double_click USING e_row TYPE lvc_s_row
                             e_column TYPE lvc_s_col
                             es_row_no TYPE lvc_s_roid.
  go_alv_config->item_double_click( EXPORTING is_row = e_row
                                            is_column = e_column
                                            is_row_no = es_row_no ).
ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form  REF_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_ref_click.
  CLEAR gt_template.
  CALL SCREEN 200 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form  COPY_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_copy_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key.
  go_alv_config->mv_copy_type = 'LOCAL'.
  CALL SCREEN 300 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SRC_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_src_click.
  go_alv_config->clear_itab( ).
  CALL SCREEN 500 STARTING AT 2 2 ENDING AT 120 20.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RFC_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_dev_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key, gv_strucnm.
  go_alv_config->clear_itab( ).
  go_alv_config->mv_copy_type = go_alv_config->mc_sysid_dev.
  CALL SCREEN 300 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COPY_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_prd_click.
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
FORM titem_qa_click.
  CLEAR : gv_progid, gv_alv_key, gv_fcat_key, gv_strucnm.
  go_alv_config->clear_itab( ).
  go_alv_config->mv_copy_type = go_alv_config->mc_sysid_qa.
  CALL SCREEN 300 STARTING AT 2 2 ENDING AT 120 15.
ENDFORM.                    "ADDR_CLICK.
*&---------------------------------------------------------------------*
*&      Form  F4.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_emphasize_f4 USING es_row_no TYPE lvc_s_roid.
  go_alv_config->set_selected_item_row( es_row_no ).
  CALL SCREEN 400 STARTING AT 80 5 ENDING AT 117 18.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tcolor_double_click USING e_row TYPE lvc_s_row
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
FORM titem_upld_click.
  go_alv_config->item_upld_click( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TITEM_NODE_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_node_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                              pv_row_id TYPE i
                              pv_value TYPE lvc_value
                              .
  go_alv_config->unique_field_changed( EXPORTING iv_fieldname = 'NODE'
                                                      iv_value = pv_value
                                                      iv_row_id = pv_row_id ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TITEM_PARENTS_NODE_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_parents_node_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                              pv_row_id TYPE i
                              pv_value TYPE lvc_value
                              .
  go_alv_config->unique_field_changed( EXPORTING iv_fieldname = 'PARENTS_NODE'
                                                      iv_value = pv_value
                                                      iv_row_id = pv_row_id ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TITEM_SORTIDX_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_sortidx_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                              pv_row_id TYPE i
                              pv_value TYPE lvc_value
                              .
  go_alv_config->unique_field_changed( EXPORTING iv_fieldname = 'SORTIDX'
                                                      iv_value = pv_value
                                                      iv_row_id = pv_row_id ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TITEM_NODE_TXT_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_node_txt_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                              pv_row_id TYPE i
                              pv_value TYPE lvc_value
                              .
  go_alv_config->unique_field_changed( EXPORTING iv_fieldname = 'NODE_TXT'
                                                      iv_value = pv_value
                                                      iv_row_id = pv_row_id ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SRC_CLICK.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_prt_click.
  go_alv_config->item_prt_click( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  item_col_pos_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM titem_col_pos_changed USING po_object TYPE REF TO cl_alv_changed_data_protocol
                                pv_row_id
                                pv_value TYPE any.
  go_alv_config->item_changed( EXPORTING iv_column = 'COL_POS'
                                         iv_value = pv_value
                                         iv_row_id = pv_row_id  ).
ENDFORM.                    "CARRID_CHANGED.
