*&---------------------------------------------------------------------*
*&  Include           ZLBRD00020F00
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  set_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen OUTPUT.
  SET PF-STATUS '9100'.
  SET TITLEBAR '9100' WITH TEXT-t00.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9101 OUTPUT.
  SET PF-STATUS '9101'.
  SET TITLEBAR '9101'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9210 OUTPUT.
  SET PF-STATUS '9210'.
  SET TITLEBAR '9210'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  set_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv OUTPUT.

  PERFORM get_source_code.

  CHECK go_splitter_container IS INITIAL.
  CREATE OBJECT go_splitter_container
    EXPORTING
      parent  = cl_gui_container=>default_screen
      rows    = 2
      columns = 1.
  go_container_title = go_splitter_container->get_container( row = 1 column = 1 ) .
  go_container_body = go_splitter_container->get_container( row = 2 column = 1 ) .

  go_splitter_container->set_row_height( id = 1 height = 30 ).
  go_splitter_container->set_row_height( id = 2 height = 70 ).


  CREATE OBJECT go_splitter_container_editor
    EXPORTING
      parent  = go_container_body
      rows    = 2
      columns = 2.

  go_container_top = go_splitter_container_editor->get_container( row = 1 column = 2 ).
  go_container_main = go_splitter_container_editor->get_container( row = 1 column = 1 ).
  go_container_f01 = go_splitter_container_editor->get_container( row = 2 column = 1 ).
  go_container_100 = go_splitter_container_editor->get_container( row = 2 column = 2 ).

  CREATE OBJECT go_textedit_top
    EXPORTING
      parent = go_container_top.
  CREATE OBJECT go_textedit_main
    EXPORTING
      parent = go_container_main.
  CREATE OBJECT go_textedit_f01
    EXPORTING
      parent = go_container_f01.
  CREATE OBJECT go_textedit_100
    EXPORTING
      parent = go_container_100.


  go_textedit_top->set_text( table = gt_top ).
  go_textedit_main->set_text( table = gt_main ).
  go_textedit_f01->set_text( table = gt_f01 ).
  go_textedit_100->set_text( table = gt_100 ).

  CREATE OBJECT go_document .
  PERFORM set_title.
  go_document->display_document( parent = go_container_title ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_screen INPUT.
  CASE gv_okcode.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANC'.
      SUBMIT (sy-cprog) VIA SELECTION-SCREEN
                        WITH pa_r1 = pa_r1
                        WITH pa_r2 = pa_r2
                        WITH pa_r3 = pa_r3
                        WITH pa_r4 = pa_r4
                        WITH pa_r5 = pa_r5
                        WITH pa_s1 = pa_s1
                        WITH pa_s2 = pa_s2
                        WITH pa_s3 = pa_s3
                        WITH pa_s4 = pa_s4
                        WITH pa_s5 = pa_s5
                        WITH pa_s6 = pa_s6
                        WITH pa_modu = pa_modu
                        WITH pa_prog = pa_prog
                        WITH pa_tabnm = pa_tabnm
                        WITH pa_alv = pa_alv
                        WITH pa_fcat = pa_fcat
                        WITH pa_prog = pa_prog
                        .
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'COPY'.
      PERFORM copy_screen.
    WHEN 'GENP'.
      CLEAR : gv_repti, gv_tcode_gen.
      CALL SCREEN 9210 STARTING AT 10 5 ENDING AT 100 7.

    WHEN 'EXEC'.
      CASE abap_true.
        WHEN pa_r1.
          PERFORM check_join_input USING gv_err.
          IF gv_err EQ abap_true.
            EXIT.
          ENDIF.
          CALL SCREEN 9100.
        WHEN pa_r2.
          PERFORM check_double_input USING gv_err.
          IF gv_err EQ abap_true.
            EXIT.
          ENDIF.

          CALL SCREEN 9200.
        WHEN pa_r3.
          PERFORM check_multi_input USING gv_err.
          IF gv_err EQ abap_true.
            EXIT.
          ENDIF.
          CALL SCREEN 9200.
        WHEN pa_r5.
          IF pa_s6 EQ abap_true.
            PERFORM check_double_input USING gv_err.
            IF gv_err EQ abap_true.
              EXIT.
            ENDIF.
          ENDIF.
          CALL SCREEN 9100.
      ENDCASE.
  ENDCASE.
  CLEAR gv_okcode.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9210 INPUT.
  CASE gv_okcode.
    WHEN 'EXEC'.
      PERFORM generate_program.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input USING pv_err.
  CASE abap_true.
    WHEN pa_r1.
      CASE abap_true.
        WHEN pa_s1.
          PERFORM check_tabnm USING pa_tabnm pv_err.
          PERFORM check_alv_key USING pa_alv pv_err.
          PERFORM check_fcat_key USING pa_alv pa_fcat pv_err.
        WHEN pa_s2.
          PERFORM check_alv_key USING pa_alv pv_err.
          PERFORM check_fcat_key USING pa_alv pa_fcat pv_err.
      ENDCASE.
    WHEN pa_r4.
      PERFORM check_tabnm USING pa_tabnm pv_err.
      PERFORM check_talv_key USING pa_alv pv_err.
      PERFORM check_tfcat_key USING pa_alv pa_fcat pv_err.
    WHEN pa_r5.
      CASE abap_true.
        WHEN pa_s5.
          PERFORM check_tabnm USING pa_tabnm pv_err.
        WHEN pa_s6.

      ENDCASE.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_TABNM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_tabnm  USING pv_tabnm pv_err.
  IF pv_tabnm IS INITIAL.
    MESSAGE TEXT-m03 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.
  SELECT COUNT( * ) INTO sy-dbcnt
  FROM dd02l
  WHERE tabname = pv_tabnm
  AND as4local EQ 'A'.

  IF sy-dbcnt EQ 0.
    MESSAGE TEXT-m07 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_ALV_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_alv_key  USING pv_alv pv_err.
  IF pv_alv IS INITIAL.
    MESSAGE TEXT-m04 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.

  SELECT COUNT( * ) INTO sy-dbcnt
   FROM ztbrd00020
   WHERE progid = pa_prog
   AND alv_key = pv_alv.

  CONCATENATE `ALV Key ` pv_alv ` is not exists in ZALV` INTO gv_msg.
  IF sy-dbcnt EQ 0.
    MESSAGE gv_msg TYPE 'I' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_FCAT_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_fcat_key  USING pv_alv pv_fcat pv_err.
  IF pv_fcat IS INITIAL.
    MESSAGE TEXT-m05 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.

  SELECT COUNT( * ) INTO sy-dbcnt
   FROM ztbrd00020
   WHERE progid = pa_prog
   AND alv_key = pv_alv
   AND fcat_key = pv_fcat.

  CONCATENATE `ALV Key ` pv_alv `, Fcat Key ` pv_fcat ` is not exists in ZALV` INTO gv_msg.
  IF sy-dbcnt EQ 0.
    MESSAGE gv_msg TYPE 'I' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_TALV_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_talv_key  USING pv_alv pv_err.
  IF pv_alv IS INITIAL.
    MESSAGE TEXT-m04 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.

  SELECT COUNT( * ) INTO sy-dbcnt
   FROM ztbrd00021
   WHERE progid = pa_prog
   AND alv_key = pv_alv.

  IF sy-dbcnt EQ 0.
    MESSAGE TEXT-m06 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_TFCAT_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_tfcat_key  USING pv_alv pv_fcat pv_err.
  IF pv_fcat IS INITIAL.
    MESSAGE TEXT-m05 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.

  SELECT COUNT( * ) INTO sy-dbcnt
   FROM ztbrd00021
   WHERE progid = pa_prog
   AND alv_key = pv_alv
   AND fcat_key = pv_fcat.

  IF sy-dbcnt EQ 0.
    MESSAGE TEXT-m06 TYPE 'S' DISPLAY LIKE 'E'.
    pv_err = abap_true.
    EXIT.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_title .
  DATA lv_str TYPE string.
  DATA lv_title_char(255).
  DEFINE _set_title.
    lv_title_char = &1.
    go_document->add_text( text = lv_title_char ).
    go_document->new_line( ).
  END-OF-DEFINITION .

  CASE abap_true.
    WHEN pa_r1.
      CASE abap_true.
        WHEN pa_s1.
          CONCATENATE text010 ` : ` pa_prog INTO lv_str.
          _set_title lv_str .
          CONCATENATE text011 ` : ` pa_tabnm INTO lv_str.
          _set_title lv_str .
          CONCATENATE text012 ` : ` pa_alv INTO lv_str.
          _set_title lv_str .
          CONCATENATE text013 ` : ` pa_fcat INTO lv_str.
          _set_title lv_str .
        WHEN pa_s2.
          CONCATENATE text010 ` : ` pa_prog INTO lv_str.
          _set_title lv_str .
          LOOP AT gt_double_alv.
            CONCATENATE gt_double_alv-type ` ` text011 ` : ` gt_double_alv-tabnm INTO lv_str.
            _set_title lv_str .
          ENDLOOP.
          CONCATENATE text012 ` : ` pa_alv INTO lv_str.
          _set_title lv_str .
          CONCATENATE text013 ` : ` pa_fcat INTO lv_str.
          _set_title lv_str .
      ENDCASE.
    WHEN pa_r2.
      CONCATENATE text010 ` : ` pa_prog INTO lv_str.
      _set_title lv_str .
      LOOP AT gt_double_alv.
        CONCATENATE gt_double_alv-type ` ` text011 ` : ` gt_double_alv-tabnm INTO lv_str.
        _set_title lv_str .
        CONCATENATE gt_double_alv-type ` ` text012 ` : ` gt_double_alv-alv_key INTO lv_str.
        _set_title lv_str .
        CONCATENATE gt_double_alv-type ` ` text013 ` : ` gt_double_alv-fcat_key INTO lv_str.
        _set_title lv_str .
      ENDLOOP.
    WHEN pa_r3.
      CONCATENATE text010 ` : ` pa_prog INTO lv_str.
      _set_title lv_str .
      LOOP AT gt_multi_alv.
        lv_str = gt_multi_alv-no.
        CONCATENATE lv_str `. ` text011 ` : ` gt_multi_alv-tabnm INTO lv_str.
        CONCATENATE lv_str `, ` text012 ` : ` gt_multi_alv-alv_key INTO lv_str.
        CONCATENATE lv_str `, ` text013 ` : ` gt_multi_alv-fcat_key INTO lv_str.
        _set_title lv_str .
      ENDLOOP.
    WHEN pa_r4.
      CONCATENATE text010 ` : ` pa_prog INTO lv_str.
      _set_title lv_str .
      CONCATENATE text011 ` : ` pa_tabnm INTO lv_str.
      _set_title lv_str .
      CONCATENATE text012 ` : ` pa_alv INTO lv_str.
      _set_title lv_str .
      CONCATENATE text013 ` : ` pa_fcat INTO lv_str.
      _set_title lv_str .
    WHEN pa_r5.
      CASE abap_true.
        WHEN pa_s5.
          CONCATENATE text010 ` : ` pa_prog INTO lv_str.
          _set_title lv_str .
          CONCATENATE text011 ` : ` pa_tabnm INTO lv_str.
          _set_title lv_str .
          CONCATENATE text012 ` : ` gs_tree_grid-g_alv_key INTO lv_str.
          _set_title lv_str .
          CONCATENATE text013 ` : ` gs_tree_grid-g_fcat_key INTO lv_str.
          _set_title lv_str .
          CONCATENATE `Tree ` text012 ` : `  gs_tree_grid-t_alv_key INTO lv_str.
          _set_title lv_str .
          CONCATENATE `Tree ` text013 ` : ` gs_tree_grid-t_fcat_key INTO lv_str.
          _set_title lv_str .
        WHEN pa_s6.
          CONCATENATE text010 ` : ` pa_prog INTO lv_str.
          _set_title lv_str .
          LOOP AT gt_double_alv.
            CONCATENATE gt_double_alv-type ` ` text011 ` : ` gt_double_alv-tabnm INTO lv_str.
            _set_title lv_str .
            CONCATENATE gt_double_alv-type ` ` text012 ` : ` gt_double_alv-alv_key INTO lv_str.
            _set_title lv_str .
            CONCATENATE gt_double_alv-type ` ` text013 ` : ` gt_double_alv-fcat_key INTO lv_str.
            _set_title lv_str .
          ENDLOOP.
      ENDCASE.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA lv_tabix TYPE sy-tabix.
  CASE abap_true.
    WHEN pa_r1.
      CASE abap_true.
        WHEN pa_s1.
        WHEN pa_s2.
          CLEAR : gt_double_alv[], gt_key_map[].

          gt_double_alv-type = 'Left Table'.
          gt_double_alv-alv_key = pa_alv.
          gt_double_alv-fcat_key = pa_fcat.
          APPEND gt_double_alv.

          gt_double_alv-type = 'Right Table'.
          gt_double_alv-alv_key = pa_alv.
          gt_double_alv-fcat_key = pa_fcat.
          APPEND gt_double_alv.

          DO 10 TIMES.
            gt_key_map-icon = icon_equal.
            APPEND gt_key_map.
          ENDDO.
      ENDCASE.
    WHEN pa_r2.
      CLEAR : gt_double_alv[], gt_key_map[].
      CASE abap_true.
        WHEN pa_s3.
          gt_double_alv-type = 'Left Table'.
          gt_double_alv-alv_key = 'LEFT'.
          gt_double_alv-fcat_key = 'DEFAULT'.
          gt_double_alv-progid = pa_prog.
          APPEND gt_double_alv.

          gt_double_alv-type = 'Right Table'.
          gt_double_alv-progid = pa_prog.
          gt_double_alv-alv_key = 'RIGH'.
          gt_double_alv-fcat_key = 'DEFAULT'.
          APPEND gt_double_alv.
        WHEN pa_s4.
          gt_double_alv-type = 'Header'.
          gt_double_alv-progid = pa_prog.
          gt_double_alv-alv_key = 'HEAD'.
          gt_double_alv-fcat_key = 'DEFAULT'.
          APPEND gt_double_alv.

          gt_double_alv-type = 'Item'.
          gt_double_alv-progid = pa_prog.
          gt_double_alv-alv_key = 'ITEM'.
          gt_double_alv-fcat_key = 'DEFAULT'.
          APPEND gt_double_alv.
      ENDCASE.
      DO 10 TIMES.
        gt_key_map-icon = icon_equal.
        APPEND gt_key_map.
      ENDDO.
    WHEN pa_r3.
      DO 20 TIMES.
        ADD 1 TO lv_tabix.
        gt_multi_alv-no = lv_tabix.
        APPEND gt_multi_alv.
      ENDDO.
    WHEN pa_r5.
      CASE abap_true.
        WHEN pa_s5.
          gs_tree_grid-t_cls = gv_tree_alv_class.
          gs_tree_grid-t_alv_key = 'TALV'.
          gs_tree_grid-t_fcat_key = 'DEFAULT'.
          gs_tree_grid-g_cls = gv_alv_class.
          gs_tree_grid-g_alv_key = 'BALV'.
          gs_tree_grid-g_fcat_key = 'DEFAULT'.

        WHEN pa_s6.
          CLEAR : gt_double_alv[], gt_key_map[].
          gt_double_alv-progid = pa_prog.
          gt_double_alv-type = 'Tree Table'.
          gt_double_alv-alv_key = 'TALV'.
          gt_double_alv-clsname = gv_tree_alv_class.
          gt_double_alv-fcat_key = 'DEFAULT'.
          APPEND gt_double_alv.

          gt_double_alv-progid = pa_prog.
          gt_double_alv-type = 'List Table'.
          gt_double_alv-alv_key = 'BALV'.
          gt_double_alv-clsname = gv_alv_class.
          gt_double_alv-fcat_key = 'DEFAULT'.
          APPEND gt_double_alv.

          DO 10 TIMES.
            gt_key_map-icon = icon_equal.
            APPEND gt_key_map.
          ENDDO.
      ENDCASE.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GENERATE_PROGRAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM generate_program .
  DATA : ls_ko200            LIKE  ko200,
         ls_tadir            TYPE tadir,
         lv_program_name     LIKE  trdir-name,
         lv_mainprogram_name LIKE  trdir-name,
         lv_program_corrnum  LIKE  e070-trkorr,
         lv_program_ordernum LIKE  e070-trkorr,
         lv_program_devclass LIKE  tdevc-devclass,
         ls_program_progdir  LIKE  progdir,
         ls_program_trdir    LIKE  trdir.

  IF gv_repti IS INITIAL.
    gv_repti = 'Auto generated by BALV ACG'.
  ENDIF.

  lv_program_name = pa_prog.

  "1. get CTS No, Package ID
  ls_ko200-pgmid = 'R3TR'.
  ls_ko200-object = 'PROG'.
  ls_ko200-obj_name = lv_program_name.
  ls_ko200-masterlang = sy-langu.
  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
    EXPORTING
      input  = sy-langu
    IMPORTING
      output = ls_ko200-masterlang.
  ls_ko200-operation = '1'.

  CALL FUNCTION 'TRINT_CORR_INSERT'
    EXPORTING
      is_ko200 = ls_ko200
    IMPORTING
      we_order = lv_program_ordernum "request
      we_task  = lv_program_corrnum "task
      es_tadir = ls_tadir.

  lv_program_devclass = ls_tadir-devclass. "package name

  "2. Empty Main Report Create
  ls_program_trdir-name = lv_program_name.
  ls_program_trdir-varcl = 'X'.
  ls_program_trdir-subc = '1'. "Excutable
  ls_program_trdir-cnam = sy-uname.
  ls_program_trdir-cdat = sy-datum.
  ls_program_trdir-unam = sy-uname.
  ls_program_trdir-udat = sy-datum.
  ls_program_trdir-vern = '1'.
  ls_program_trdir-levl = sy-saprl.
  ls_program_trdir-rmand = sy-mandt.
  ls_program_trdir-rload = 'E'.
  ls_program_trdir-uccheck = 'X'.

  MOVE-CORRESPONDING ls_program_trdir TO ls_program_progdir.

  CALL FUNCTION 'RS_EDTR_ATTR_ADD'
    EXPORTING
      program_name       = lv_program_name
*     activate_immediately = ' '
      with_trdir_entry   = 'X'
      with_progdir_entry = 'X'
      suppress_dialog    = 'X'
      mainprogram_name   = lv_mainprogram_name
    CHANGING
      program_progdir    = ls_program_progdir
      program_trdir      = ls_program_trdir
      program_ordernum   = lv_program_ordernum
      program_corrnum    = lv_program_corrnum
      program_devclass   = lv_program_devclass
    EXCEPTIONS
      program_exists     = 1
      OTHERS             = 2.

  CASE sy-subrc.
    WHEN 0.
      "title
      DATA ls_trdirti TYPE trdirti.
      ls_trdirti-name = lv_program_name.
      ls_trdirti-sprsl = sy-langu.
      ls_trdirti-text  = gv_repti.
      MODIFY trdirti FROM ls_trdirti.
    WHEN 1."already exits program
      "Ask Overwrite?
      IF zcl_brd_alv=>confirm_msg( EXPORTING iv_title = 'Confirm'
                                          iv_question = 'Program Already Exists.. Overwrite??' ) EQ abap_true.
      ELSE.
        MESSAGE 'Operation Canceled' TYPE 'I'.
        EXIT.
      ENDIF.
    WHEN OTHERS.
      MESSAGE 'Generation Error' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
  ENDCASE.

  "3. Insert Gernated Main Program Source Code
  INSERT REPORT lv_program_name FROM gt_main.
  COMMIT WORK.

  "4. Empty TOP Include Report Create
  lv_mainprogram_name = lv_program_name.
  lv_program_name = lv_mainprogram_name && 'TOP'.
  ls_program_trdir-name = lv_program_name.
  ls_program_trdir-subc = 'I'. "Include
  MOVE-CORRESPONDING ls_program_trdir TO ls_program_progdir.

  CALL FUNCTION 'RS_EDTR_ATTR_ADD'
    EXPORTING
      program_name       = lv_program_name
*     activate_immediately = ' '
      with_trdir_entry   = 'X'
      with_progdir_entry = 'X'
      suppress_dialog    = 'X'
      mainprogram_name   = lv_mainprogram_name
    CHANGING
      program_progdir    = ls_program_progdir
      program_trdir      = ls_program_trdir
      program_ordernum   = lv_program_ordernum
      program_corrnum    = lv_program_corrnum
      program_devclass   = lv_program_devclass
    EXCEPTIONS
      program_exists     = 1
      OTHERS             = 2.

  "5. Insert Gernated TOP Program Source Code
  INSERT REPORT lv_program_name FROM gt_top.
  COMMIT WORK.

  "6. Empty F01 Include Report Create
  lv_program_name = lv_mainprogram_name && 'F01'.
  ls_program_trdir-name = lv_program_name.
  ls_program_trdir-subc = 'I'. "Include
  MOVE-CORRESPONDING ls_program_trdir TO ls_program_progdir.

  CALL FUNCTION 'RS_EDTR_ATTR_ADD'
    EXPORTING
      program_name       = lv_program_name
*     activate_immediately = ' '
      with_trdir_entry   = 'X'
      with_progdir_entry = 'X'
      suppress_dialog    = 'X'
      mainprogram_name   = lv_mainprogram_name
    CHANGING
      program_progdir    = ls_program_progdir
      program_trdir      = ls_program_trdir
      program_ordernum   = lv_program_ordernum
      program_corrnum    = lv_program_corrnum
      program_devclass   = lv_program_devclass
    EXCEPTIONS
      program_exists     = 1
      OTHERS             = 2.
  "7. Insert Gernated F01 Program Source Code
  INSERT REPORT lv_program_name FROM gt_f01.
  COMMIT WORK.

  "8. Screen Copy
  DATA : lv_from_dynnr TYPE dynname,
         lv_to_dynnr   TYPE dynname,
         lv_numc       TYPE n LENGTH 2.
  IF pa_r3 EQ 'X'."multi
    CALL FUNCTION 'RS_SCRP_COPY'
      EXPORTING
        source_dynnr         = '0200'
        source_progname      = sy-cprog
        target_dynnr         = '0100'
        target_progname      = lv_mainprogram_name
      EXCEPTIONS
        illegal_value        = 01
        not_executed         = 02
        no_modify_permission = 03
        source_not_exists    = 04
        target_exists        = 05.

    LOOP AT gt_multi_alv.
      CHECK gt_multi_alv-alv_key IS NOT INITIAL.

      ADD 1 TO lv_numc.
      lv_from_dynnr = '02' && lv_numc.
      lv_to_dynnr = '01' && lv_numc.
      CALL FUNCTION 'RS_SCRP_COPY'
        EXPORTING
          source_dynnr         = lv_from_dynnr
          source_progname      = sy-cprog
          target_dynnr         = lv_to_dynnr
          target_progname      = lv_mainprogram_name
        EXCEPTIONS
          illegal_value        = 01
          not_executed         = 02
          no_modify_permission = 03
          source_not_exists    = 04
          target_exists        = 05.
    ENDLOOP.


  ELSE.
    CALL FUNCTION 'RS_SCRP_COPY'
      EXPORTING
        source_dynnr         = '0100'
        source_progname      = sy-cprog
        target_dynnr         = '0100'
        target_progname      = lv_mainprogram_name
      EXCEPTIONS
        illegal_value        = 01
        not_executed         = 02
        no_modify_permission = 03
        source_not_exists    = 04
        target_exists        = 05.
  ENDIF.
  COMMIT WORK.
  "9. Titlebar Copy
  DATA lv_rcode(5).
  PERFORM copy_title_diff_progs_new(saplsmpe) USING abap_true
                                            sy-cprog   lv_mainprogram_name
                                            '0100' '0100'
                                            CHANGING lv_rcode.
  COMMIT WORK.
  "10. Statusbar Copy
  PERFORM copy_status_diff_progs(saplsmpe) USING sy-cprog   lv_mainprogram_name
                                                '0100' '0100'
                                          CHANGING lv_rcode.
  COMMIT WORK.
  CALL FUNCTION 'RS_ACCESS_PERMISSION'
    EXPORTING
      mode         = 'FREE'
      object       = lv_mainprogram_name
      object_class = 'SCUA'.


  "11. Object List Rebuild
  DATA lv_tname        TYPE dirtree-tname.
  lv_tname = 'PG_' && lv_mainprogram_name.
  CALL FUNCTION 'WB_TREE_ACTUALIZE'
    EXPORTING
      tree_name              = lv_tname
      without_crossreference = abap_true
      with_tcode_index       = abap_true.

  "12. Text Pool
  DATA  BEGIN OF hirst2 OCCURS 200.
  INCLUDE STRUCTURE textpool.
  DATA  END OF hirst2.

  CLEAR hirst2.
  hirst2-id = 'R'.
  hirst2-key = ''.
  hirst2-entry = gv_repti.
  hirst2-length = '26'.
  APPEND hirst2.

  CLEAR hirst2.
  hirst2-id = 'I'.
  hirst2-key = 'T00'.
  hirst2-entry = gv_repti.
  hirst2-length = '26'.
  APPEND hirst2.

  CLEAR hirst2.
  hirst2-id = 'I'.
  hirst2-key = 'B01'.
  hirst2-entry = 'Download'.
  hirst2-length = '20'.
  APPEND hirst2.

  CLEAR hirst2.
  hirst2-id = 'I'.
  hirst2-key = 'T01'.
  hirst2-entry = 'Display'.
  hirst2-length = '20'.
  APPEND hirst2.

  CLEAR hirst2.
  hirst2-id = 'I'.
  hirst2-key = 'T02'.
  hirst2-entry = 'Upload'.
  hirst2-length = '20'.
  APPEND hirst2.

  CLEAR hirst2.
  hirst2-id = 'I'.
  hirst2-key = 'T03'.
  hirst2-entry = 'File Path'.
  hirst2-length = '20'.
  APPEND hirst2.

  CLEAR hirst2.
  hirst2-id = 'I'.
  hirst2-key = 'T04'.
  hirst2-entry = 'Sample Excel'.
  hirst2-length = '20'.
  APPEND hirst2.

  CLEAR hirst2.
  hirst2-id = 'I'.
  hirst2-key = 'T09'.
  hirst2-entry = 'Select Options'.
  hirst2-length = '20'.
  APPEND hirst2.

  INSERT TEXTPOOL lv_mainprogram_name FROM hirst2 LANGUAGE sy-langu.

  "13. Tcode create
  IF gv_tcode_gen EQ abap_true.
    DATA : ls_tstc   TYPE tstc,
           ls_tstct  TYPE tstct,
           lv_dynpro TYPE d020s-dnum.
    ls_tstc-tcode = lv_mainprogram_name.
    ls_tstc-pgmna = lv_mainprogram_name.
    ls_tstct-ttext = gv_repti.
    ls_tstct-tcode = lv_mainprogram_name.
    lv_dynpro = '1000'.
    CALL FUNCTION 'RPY_TRANSACTION_INSERT'
      EXPORTING
        transaction             = ls_tstc-tcode
        program                 = ls_tstc-pgmna
        dynpro                  = lv_dynpro
        language                = sy-langu
        development_class       = lv_program_devclass
        transport_number        = lv_program_ordernum
        transaction_type        = 'R' "report
        shorttext               = ls_tstct-ttext
        called_transaction      = ls_tstc-tcode
        called_transaction_skip = abap_true
*       variant                 = ls_rsstcd-variant
*       cl_independend          = ls_rsstcd-s_ind_vari
        html_enabled            = abap_true
        java_enabled            = abap_true
        wingui_enabled          = abap_true
        suppress_corr_insert    = abap_true
*  TABLES
*       param_values            = lt_param_values
      EXCEPTIONS
        cancelled               = 1
        already_exist           = 2
        permission_error        = 3
        name_not_allowed        = 4
        name_conflict           = 5
        illegal_type            = 6
        object_inconsistent     = 7
        db_access_error         = 8
        OTHERS                  = 9.
  ENDIF.

  MESSAGE 'Program Generated!' TYPE 'I' DISPLAY LIKE 'S'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COPY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_screen .
  DATA : lv_program           TYPE  program,
         lv_obj_type          TYPE  seu_stype,
         lv_obj_key           TYPE  rsmpe_obj,
         lv_operation         TYPE  seu_action,
         lo_wb_program_state  TYPE REF TO if_wb_program_state,
         lo_wb_object_state   TYPE REF TO if_wb_object_state,
         lo_wb_data_container TYPE REF TO cl_wb_data_container,
         lo_men_instance      TYPE REF TO cl_menu_painter.

  zcl_brd_alv=>confirm_msg( EXPORTING iv_title = 'Confirm'
                                      iv_question = 'Copy Screen?'
                            RECEIVING rv_return = gv_flag ).
  IF gv_flag NE abap_true.
    EXIT.
  ENDIF.
  IF pa_r3 EQ 'X'."multi
    "screen
    CALL FUNCTION 'RS_SCREEN_COPY'
      EXPORTING
        source_dynnr_import    = '0200'
        source_progname_import = sy-cprog
        target_dynnr_import    = '0200'
        target_progname_import = pa_prog
      EXCEPTIONS
        not_executed           = 1
        OTHERS                 = 2.
    CALL FUNCTION 'RS_SCREEN_COPY'
      EXPORTING
        source_dynnr_import    = '0201'
        source_progname_import = sy-cprog
        target_dynnr_import    = '0201'
        target_progname_import = pa_prog
      EXCEPTIONS
        not_executed           = 1
        OTHERS                 = 2.
    CALL FUNCTION 'RS_SCREEN_COPY'
      EXPORTING
        source_dynnr_import    = '0202'
        source_progname_import = sy-cprog
        target_dynnr_import    = '0202'
        target_progname_import = pa_prog
      EXCEPTIONS
        not_executed           = 1
        OTHERS                 = 2.
  ELSE.
    "screen
    CALL FUNCTION 'RS_SCREEN_COPY'
      EXPORTING
        source_dynnr_import    = '0100'
        source_progname_import = sy-cprog
        target_dynnr_import    = '0100'
        target_progname_import = pa_prog
      EXCEPTIONS
        not_executed           = 1
        OTHERS                 = 2.
  ENDIF.
  "title bar
  lv_program = sy-cprog.
  lv_obj_type = 'PZ'.
  lv_obj_key = '0100'.
  lv_operation = 'COPY'.
  CALL FUNCTION 'RS_CUA_WM_EDITOR'
    EXPORTING
      p_program           = lv_program
      p_obj_type          = lv_obj_type
      p_obj_key           = lv_obj_key
      p_operation         = lv_operation
      p_wb_program_state  = lo_wb_program_state
      p_wb_object_state   = lo_wb_object_state
      p_wb_data_container = lo_wb_data_container
      p_men_instance      = lo_men_instance
    EXCEPTIONS
      error_occured       = 1
      action_cancelled    = 2
      OTHERS              = 3.

  "staus bar
  lv_obj_type = 'PC'.
  CALL FUNCTION 'RS_CUA_WM_EDITOR'
    EXPORTING
      p_program           = lv_program
      p_obj_type          = lv_obj_type
      p_obj_key           = lv_obj_key
      p_operation         = lv_operation
      p_wb_program_state  = lo_wb_program_state
      p_wb_object_state   = lo_wb_object_state
      p_wb_data_container = lo_wb_data_container
      p_men_instance      = lo_men_instance
    EXCEPTIONS
      error_occured       = 1
      action_cancelled    = 2
      OTHERS              = 3.

  IF sy-subrc EQ 0.
    MESSAGE 'Screen, Status bar, Tilte bar area Copied..' TYPE 'I'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SOURCE_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_source_code .
  CASE abap_true.
    WHEN pa_r1. PERFORM get_single_src.
    WHEN pa_r2. PERFORM get_double_src.
    WHEN pa_r3. PERFORM get_multi_src.
    WHEN pa_r4. PERFORM get_tree_src.
    WHEN pa_r5. PERFORM get_tree_grid_src.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INSTALL_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM install_program .
  DATA : lt_header TYPE TABLE OF ztbrd00010 WITH HEADER LINE,
         lt_item   TYPE TABLE OF ztbrd00020 WITH HEADER LINE.
  DATA lv_progid TYPE sy-cprog VALUE 'ZLBRD00020'.

  PERFORM get_header_install_data TABLES lt_header.
  PERFORM get_item_install_data TABLES lt_item.

  LOOP AT lt_header.
    lt_header-progid = lv_progid.
    lt_header-erdat = sy-datum.
    lt_header-erzet = sy-uzeit.
    lt_header-ernam = sy-uname.
    lt_header-aedat = sy-datum.
    lt_header-aezet = sy-uzeit.
    lt_header-aenam = sy-uname.
    MODIFY lt_header.
  ENDLOOP.

  LOOP AT lt_item.
    lt_item-progid = lv_progid.
    lt_item-erdat = sy-datum.
    lt_item-erzet = sy-uzeit.
    lt_item-ernam = sy-uname.
    lt_item-aedat = sy-datum.
    lt_item-aezet = sy-uzeit.
    lt_item-aenam = sy-uname.
    MODIFY lt_item.
  ENDLOOP.

  MODIFY ztbrd00010 FROM TABLE lt_header.
  MODIFY ztbrd00020 FROM TABLE lt_item.

  MESSAGE 'Installed!!' TYPE 'I' DISPLAY LIKE 'S'.
ENDFORM.
FORM get_header_install_data TABLES pt_header STRUCTURE ztbrd00010.

  "header
  CLEAR pt_header.
  pt_header-alv_key = 'HEADER'.
  pt_header-sel_mode = 'B'.
  APPEND pt_header.
  CLEAR pt_header.
  pt_header-alv_key = 'ITEM'.
  pt_header-sel_mode = 'B'.
  APPEND pt_header.
  CLEAR pt_header.
  pt_header-alv_key = 'MULTI'.
  pt_header-sel_mode = 'B'.
  pt_header-change_evt = 'X'.
  APPEND pt_header.
  CLEAR pt_header.
  pt_header-alv_key = 'TAB'.
  pt_header-sel_mode = 'B'.
  APPEND pt_header.
  CLEAR pt_header.
  pt_header-alv_key = 'TGHD'.
  pt_header-sel_mode = 'B'.
  APPEND pt_header.
ENDFORM.
FORM get_item_install_data TABLES pt_item STRUCTURE ztbrd00020.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'HEADER'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'ALV_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '4'.
  pt_item-emphasize = 'K41'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-scrtext_l = 'ALV Key'.
  pt_item-scrtext_m = 'ALV Key'.
  pt_item-scrtext_s = 'ALV Key'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'HEADER'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'CLSNAME'.
  pt_item-col_pos = '999'.
  pt_item-emphasize = 'K41'.
  pt_item-tech = 'X'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '30'.
  pt_item-reptext = 'Object Type Name'.
  pt_item-domname = 'SEOCLSNAME'.
  pt_item-ref_table = 'SEOCLASS'.
  pt_item-dd_outlen = '30'.
  pt_item-scrtext_l = 'Class/Interface'.
  pt_item-scrtext_m = 'ObjectTypeName'.
  pt_item-scrtext_s = 'ObjectType'.
  pt_item-crea_edit = 'X'.
  pt_item-upld_edit = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'HEADER'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'FCAT_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '5'.
  pt_item-emphasize = 'K41'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-scrtext_l = 'Fieldcat Key'.
  pt_item-scrtext_m = 'Fieldcat Key'.
  pt_item-scrtext_s = 'Fieldcat K'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'HEADER'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'PROGID'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '2'.
  pt_item-zkey = 'X'.
  pt_item-emphasize = 'K41'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '40'.
  pt_item-domname = 'PROGRAM_ID'.
  pt_item-col_opt = 'X'.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Program ID'.
  pt_item-scrtext_m = 'Program ID'.
  pt_item-scrtext_s = 'Program ID'.
  pt_item-crea_edit = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'HEADER'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'TABNM'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '3'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '0'.
  pt_item-ref_field = 'TABNAME'.
  pt_item-ref_table = 'DD02L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Table ID'.
  pt_item-scrtext_m = 'Table ID'.
  pt_item-scrtext_s = 'Table ID'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'HEADER'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'TYPE'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '1'.
  pt_item-zkey = 'X'.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = ''.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Type'.
  pt_item-scrtext_m = 'Type'.
  pt_item-scrtext_s = 'Type'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'ITEM'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'H_FIELD'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '1'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '20'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '30'.
  pt_item-reptext = 'Header Field'.
  pt_item-domname = ''.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = 'FIELDNAME'.
  pt_item-ref_table = 'DD03L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Header Field'.
  pt_item-scrtext_m = 'Header Field'.
  pt_item-scrtext_s = 'Header Fie'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'ITEM'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'ICON'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '2'.
  pt_item-zkey = ''.
  pt_item-icon = 'X'.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '4'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = 'X'.
  pt_item-ref_field = ''.
  pt_item-ref_table = ''.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = ''.
  pt_item-scrtext_m = ''.
  pt_item-scrtext_s = ''.
  pt_item-crea_edit = ''.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'ITEM'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'I_FIELD'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '3'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '20'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '30'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = 'FIELDNAME'.
  pt_item-ref_table = 'DD03L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Item Field'.
  pt_item-scrtext_m = 'Item Field'.
  pt_item-scrtext_s = 'Item Field'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'ITEM'.
  pt_item-fcat_key = 'SIMPLE'.
  pt_item-fieldname = 'H_FIELD'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '1'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '20'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '30'.
  pt_item-reptext = 'Select Field'.
  pt_item-domname = ''.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = 'FIELDNAME'.
  pt_item-ref_table = 'DD03L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Select Field'.
  pt_item-scrtext_m = 'Select Field'.
  pt_item-scrtext_s = 'Select Fie'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'ITEM'.
  pt_item-fcat_key = 'SIMPLE'.
  pt_item-fieldname = 'ICON'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '2'.
  pt_item-zkey = ''.
  pt_item-icon = 'X'.
  pt_item-emphasize = ''.
  pt_item-tech = 'X'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '4'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = 'X'.
  pt_item-ref_field = ''.
  pt_item-ref_table = ''.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = ''.
  pt_item-scrtext_m = ''.
  pt_item-scrtext_s = ''.
  pt_item-crea_edit = ''.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'ITEM'.
  pt_item-fcat_key = 'SIMPLE'.
  pt_item-fieldname = 'I_FIELD'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '3'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = 'X'.
  pt_item-outputlen = '20'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '30'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = 'FIELDNAME'.
  pt_item-ref_table = 'DD03L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Item Field'.
  pt_item-scrtext_m = 'Item Field'.
  pt_item-scrtext_s = 'Item Field'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'MULTI'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'ALV_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '4'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'YTBRBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'ALV Key'.
  pt_item-scrtext_m = 'ALV Key'.
  pt_item-scrtext_s = 'ALV Key'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'MULTI'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'FCAT_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '5'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'YTBRBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Fieldcat Key'.
  pt_item-scrtext_m = 'Fieldcat Key'.
  pt_item-scrtext_s = 'Fieldcat K'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'MULTI'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'NO'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '1'.
  pt_item-zkey = 'X'.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'INT1'.
  pt_item-inttype = 'I'.
  pt_item-intlen = '4'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = ''.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = ''.
  pt_item-scrtext_m = ''.
  pt_item-scrtext_s = ''.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'MULTI'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'TABNM'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '3'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '0'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = 'TABNAME'.
  pt_item-ref_table = 'DD02L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Table ID'.
  pt_item-scrtext_m = 'Table ID'.
  pt_item-scrtext_s = 'Table ID'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TAB'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'ALV_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '4'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = 'X'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'ALV Key'.
  pt_item-scrtext_m = 'ALV Key'.
  pt_item-scrtext_s = 'ALV Key'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TAB'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'CLSNAME'.
  pt_item-row_pos = '0'.
  pt_item-col_pos = '999'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = 'X'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '30'.
  pt_item-reptext = 'Object Type Name'.
  pt_item-domname = 'SEOCLSNAME'.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'SEOCLASS'.
  pt_item-dd_outlen = '30'.
  pt_item-scrtext_l = 'Class/Interface'.
  pt_item-scrtext_m = 'ObjectTypeName'.
  pt_item-scrtext_s = 'ObjectType'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = 'X'.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TAB'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'FCAT_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '5'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = 'X'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Fieldcat Key'.
  pt_item-scrtext_m = 'Fieldcat Key'.
  pt_item-scrtext_s = 'Fieldcat K'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TAB'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'PROGID'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '2'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = 'X'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '40'.
  pt_item-reptext = ''.
  pt_item-domname = 'PROGRAM_ID'.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Program ID'.
  pt_item-scrtext_m = 'Program ID'.
  pt_item-scrtext_s = 'Program ID'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TAB'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'TABNM'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '3'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '0'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = 'TABNAME'.
  pt_item-ref_table = 'DD02L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Table ID'.
  pt_item-scrtext_m = 'Table ID'.
  pt_item-scrtext_s = 'Table ID'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TAB'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'TYPE'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '1'.
  pt_item-zkey = 'X'.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = ''.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Type'.
  pt_item-scrtext_m = 'Type'.
  pt_item-scrtext_s = 'Type'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TGHD'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'ALV_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '5'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'ALV Key'.
  pt_item-scrtext_m = 'ALV Key'.
  pt_item-scrtext_s = 'ALV Key'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TGHD'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'CLSNAME'.
  pt_item-row_pos = '0'.
  pt_item-col_pos = '3'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = ''.
  pt_item-outputlen = '20'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '30'.
  pt_item-reptext = ''.
  pt_item-domname = 'SEOCLSNAME'.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'SEOCLASS'.
  pt_item-dd_outlen = '30'.
  pt_item-scrtext_l = 'Class/Interface'.
  pt_item-scrtext_m = 'ObjectTypeName'.
  pt_item-scrtext_s = 'ObjectType'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TGHD'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'FCAT_KEY'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '6'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = 'CHAR10'.
  pt_item-f4availabl = 'X'.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Fieldcat Key'.
  pt_item-scrtext_m = 'Fieldcat Key'.
  pt_item-scrtext_s = 'Fieldcat K'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TGHD'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'PROGID'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '999'.
  pt_item-zkey = 'X'.
  pt_item-icon = ''.
  pt_item-emphasize = 'K41'.
  pt_item-tech = 'X'.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '40'.
  pt_item-reptext = ''.
  pt_item-domname = 'PROGRAM_ID'.
  pt_item-f4availabl = ''.
  pt_item-col_opt = 'X'.
  pt_item-ref_field = ''.
  pt_item-ref_table = 'ZTBRD00020'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Program ID'.
  pt_item-scrtext_m = 'Program ID'.
  pt_item-scrtext_s = 'Program ID'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TGHD'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'TABNM'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '4'.
  pt_item-zkey = ''.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '15'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '0'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = 'TABNAME'.
  pt_item-ref_table = 'DD02L'.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Table ID'.
  pt_item-scrtext_m = 'Table ID'.
  pt_item-scrtext_s = 'Table ID'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = 'X'.
  pt_item-upld_edit = ''.
  pt_item-change_evt = 'X'.
  APPEND pt_item.
  CLEAR pt_item.
  pt_item-progid = 'ZLBRD00020'.
  pt_item-alv_key = 'TGHD'.
  pt_item-fcat_key = 'DEFAULT'.
  pt_item-fieldname = 'TYPE'.
  pt_item-row_pos = '1'.
  pt_item-col_pos = '1'.
  pt_item-zkey = 'X'.
  pt_item-icon = ''.
  pt_item-emphasize = ''.
  pt_item-tech = ''.
  pt_item-outputlen = '0'.
  pt_item-datatype = 'CHAR'.
  pt_item-inttype = 'C'.
  pt_item-intlen = '10'.
  pt_item-reptext = ''.
  pt_item-domname = ''.
  pt_item-f4availabl = ''.
  pt_item-col_opt = ''.
  pt_item-ref_field = ''.
  pt_item-ref_table = ''.
  pt_item-dd_outlen = '0'.
  pt_item-scrtext_l = 'Type'.
  pt_item-scrtext_m = 'Type'.
  pt_item-scrtext_s = 'Type'.
  pt_item-crea_edit = 'X'.
  pt_item-chan_edit = ''.
  pt_item-upld_edit = ''.
  pt_item-change_evt = ''.
  APPEND pt_item.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_alv_class
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_alv_class .
  IF pa_new EQ abap_true.
    CONCATENATE 'ZCL_' pa_modu 'Z_ALV_N' INTO gv_alv_class.
    CONCATENATE 'ZCL_' pa_modu 'Z_TREE_ALV' INTO gv_tree_alv_class.
  ELSE.
    CONCATENATE 'ZCL_' pa_modu 'Z_ALV' INTO gv_alv_class.
    CONCATENATE 'ZCL_' pa_modu 'Z_TREE_ALV' INTO gv_tree_alv_class.
  ENDIF.

  SELECT SINGLE COUNT(*) INTO sy-dbcnt
  FROM tadir
  WHERE pgmid = 'R3TR'
    AND object = 'CLAS'
    AND obj_name  = gv_alv_class.

  IF sy-dbcnt EQ 0.
    IF pa_new EQ abap_true.
      gv_alv_class = 'ZCL_BRD_ALV_N'.
      gv_tree_alv_class = 'ZCL_BRD_TREE_ALV'.
    ELSE.
      gv_alv_class = 'ZCL_BRD_ALV'.
      gv_tree_alv_class = 'ZCL_BRD_TREE_ALV'.
    ENDIF.
  ENDIF.
ENDFORM.
