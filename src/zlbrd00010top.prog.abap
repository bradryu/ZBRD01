*&---------------------------------------------------------------------*
*&  Include           ZLBRD00010TOP
*&---------------------------------------------------------------------*
DEFINE _set_timestamp_create.
  &1-ernam = sy-uname.
  &1-erdat = sy-datum.
  &1-erzet = sy-uzeit.
  &1-aenam = sy-uname.
  &1-aedat = sy-datum.
  &1-aezet = sy-uzeit.
END-OF-DEFINITION.
DEFINE _set_timestamp_change.
  &1-aenam = sy-uname.
  &1-aedat = sy-datum.
  &1-aezet = sy-uzeit.
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


*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*
DATA : go_alv_config    TYPE REF TO zcl_brd_alv_config,
       go_alv_balv      TYPE REF TO zcl_brd_alv_config_balv,
       go_alv_balv_tree TYPE REF TO zcl_brd_alv_config_tree.


DATA : gv_okcode   TYPE sy-ucomm,
       gv_strucnm  TYPE tabname,
       gv_progid   TYPE ztbrd00020-progid,
       gv_alv_key  TYPE ztbrd00020-alv_key,
       gv_fcat_key TYPE ztbrd00020-fcat_key,
       gt_template TYPE lvc_t_fcat.

DATA : go_splitter_container TYPE REF TO cl_gui_splitter_container,
       go_container_header   TYPE REF TO cl_gui_container,
       go_container_item     TYPE REF TO cl_gui_container.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF LINE .
PARAMETERS pa_alv RADIOBUTTON GROUP rd1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(20) text001 FOR FIELD pa_alv.
PARAMETERS pa_tree RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT (20) text002 FOR FIELD pa_tree.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS : so_prog FOR ztbrd00010-progid,
                 so_alv FOR ztbrd00010-alv_key,
                 so_fcat FOR ztbrd00020-fcat_key NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.
