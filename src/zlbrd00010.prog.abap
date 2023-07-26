*************************************************************************
* Modules / Sub Module : CM / Z
*&---------------------------------------------------------------------
* T_CODE             : ZLBRD00010
* WRITER             : BRAD.RYU
* DATE               : 2015.12.10
* TYPE               : Online Report
* Description        : BALV Configuration Management
*
*************************************************************************
*              Changed History
*&---------------------------------------------------------------------**
*Changed Number     Changed Date   Writer    Changed      Description
*&---------------------------------------------------------------------**
*      N            2015.12.10    BRAD.RYU   CREATE       CREATE       *
*************************************************************************
REPORT zlbrd00010 NO STANDARD PAGE HEADING.
TABLES : ztbrd00010, ztbrd00020, ztbrd00011, ztbrd00021.

*INCLUDE znbrd00030. "cbo log
INCLUDE zlbrd00010top.
INCLUDE zlbrd00010f01.
INCLUDE zlbrd00010f02.
INCLUDE zlbrd00010f03."install



*&---------------------------------------------------------------------*
*& INITIALIZATION.
*&---------------------------------------------------------------------*
INITIALIZATION.
  text001 = 'ALV'.
  text002 = 'Tree ALV'.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
  IF sy-ucomm = 'INSTALL'.
    PERFORM install_program.
  ENDIF.
*&---------------------------------------------------------------------*
*& START-OF-SELECTION.
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  CASE abap_true.
    WHEN pa_alv.
      CREATE OBJECT go_alv_balv.
      go_alv_config ?= go_alv_balv.
*      go_alv_config = NEW zcl_cmz_alv_config_balv( ).
    WHEN pa_tree.
      CREATE OBJECT go_alv_balv_tree.
      go_alv_config ?= go_alv_balv_tree.
*      go_alv_config = NEW zcl_cmz_alv_config_tree( ).
  ENDCASE.
  go_alv_config->mr_prog = so_prog[].
  go_alv_config->mr_alv_key = so_alv[].
  go_alv_config->mr_fcat_key = so_fcat[].
  go_alv_config->get_header_data( ).



*&---------------------------------------------------------------------*
*& END-OF-SELECTION.
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  CALL SCREEN 100.
