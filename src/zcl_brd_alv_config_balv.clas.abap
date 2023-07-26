CLASS zcl_brd_alv_config_balv DEFINITION
  PUBLIC
  INHERITING FROM zcl_brd_alv_config
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS clear_itab
        REDEFINITION .
    METHODS copy_save_click
        REDEFINITION .
    METHODS display_copy
        REDEFINITION .
    METHODS display_header
        REDEFINITION .
    METHODS display_item
        REDEFINITION .
    METHODS display_source
        REDEFINITION .
    METHODS display_template
        REDEFINITION .
    METHODS excel_download
        REDEFINITION .
    METHODS generate_source_fcat
        REDEFINITION .
    METHODS get_header_data
        REDEFINITION .
    METHODS get_item_data
        REDEFINITION .
    METHODS header_addr_click
        REDEFINITION .
    METHODS header_canc_click
        REDEFINITION .
    METHODS header_chan_click
        REDEFINITION .
    METHODS header_crea_click
        REDEFINITION .
    METHODS header_dele_click
        REDEFINITION .
    METHODS header_delr_click
        REDEFINITION .
    METHODS header_double_click
        REDEFINITION .
    METHODS header_save_click
        REDEFINITION .
    METHODS header_upld_click
        REDEFINITION .
    METHODS item_ad10_click
        REDEFINITION .
    METHODS item_addr_click
        REDEFINITION .
    METHODS item_canc_click
        REDEFINITION .
    METHODS item_changed
        REDEFINITION .
    METHODS item_chan_click
        REDEFINITION .
    METHODS item_crea_click
        REDEFINITION .
    METHODS item_dele_click
        REDEFINITION .
    METHODS item_delr_click
        REDEFINITION .
    METHODS item_double_click
        REDEFINITION .
    METHODS item_prt_click
        REDEFINITION .
    METHODS item_save_click
        REDEFINITION .
    METHODS item_upld_click
        REDEFINITION .
    METHODS search_copy_source
        REDEFINITION .
    METHODS search_structure
        REDEFINITION .
    METHODS set_item_color
        REDEFINITION .
    METHODS source_save_click
        REDEFINITION .
    METHODS template_save_click
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF gty_header.
        .INCLUDE TYPE ztbrd00010.
        TYPES : celltab TYPE lvc_t_styl,
        coltab  TYPE lvc_t_scol.
    TYPES END OF gty_header .
    TYPES:
      BEGIN OF gty_item.
        .INCLUDE TYPE ztbrd00020.
        TYPES : celltab TYPE lvc_t_styl,
        coltab  TYPE lvc_t_scol.
    TYPES END OF gty_item .

    DATA:
      mt_header      TYPE TABLE OF gty_header .
    DATA:
      ms_header      LIKE LINE OF mt_header .
    DATA:
      mt_header_buff TYPE TABLE OF gty_header .
    DATA:
      mt_item        TYPE TABLE OF gty_item .
    DATA ms_item TYPE gty_item .
    DATA:
      mt_item_buff   TYPE TABLE OF gty_item .
    DATA:
      mt_template TYPE TABLE OF gty_item .
    DATA ms_template TYPE gty_item .
    DATA:
      mt_copy     TYPE TABLE OF gty_item .
    DATA ms_copy TYPE gty_item .
    DATA:
      mt_source   TYPE TABLE OF gty_item .
    DATA ms_source TYPE gty_item .

    METHODS set_header_data
        REDEFINITION .
    METHODS set_item_data
        REDEFINITION .
ENDCLASS.



CLASS ZCL_BRD_ALV_CONFIG_BALV IMPLEMENTATION.


  METHOD clear_itab.
    CLEAR : mt_copy, mt_template.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mv_header_alv_key = 'HEADER'.
    mv_item_alv_key = 'ITEM'.
    mv_template_alv_key = 'TEMPLATE'.
    mv_copy_alv_key = 'COPY'.
    mv_source_alv_key = 'SOURCE'.
  ENDMETHOD.


  METHOD copy_save_click.
    CLEAR : mt_rows[], ms_rows.
    mo_alv_copy->get_selected_rows(
     IMPORTING
       et_index_rows = mt_rows
       et_row_no     = mt_row_ids ).

    IF mt_rows[] IS INITIAL.
      MESSAGE 'No Row Selected.' TYPE 'I'.
      EXIT.
    ENDIF.

    mo_alv_copy->check_changed_data( ).
    CASE mv_copy_type.
      WHEN 'LOCAL'.
        LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
          CLEAR ms_copy.
          READ TABLE mt_copy INTO ms_copy INDEX ms_rows-index.
          IF sy-subrc EQ 0.
            ms_copy-progid = ms_header-progid.
            ms_copy-alv_key = ms_header-alv_key.
            CHECK ms_copy-fcat_key IS NOT INITIAL.
*            ms_copy-fcat_key = mc_default_key.
            APPEND ms_copy TO mt_item.
          ENDIF.
        ENDLOOP.
      WHEN mc_sysid_dev OR mc_sysid_qa OR mc_sysid_prd.
        LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
          CLEAR ms_copy.
          READ TABLE mt_copy INTO ms_copy INDEX ms_rows-index.
          IF sy-subrc EQ 0.

            READ TABLE mt_item TRANSPORTING NO FIELDS
                               WITH KEY progid = ms_header-progid
                                        alv_key = ms_header-alv_key
                                        fcat_key = ms_copy-fcat_key
                                        fieldname =  ms_copy-fieldname.
            IF sy-subrc EQ 0.
              DELETE mt_item INDEX sy-tabix.
            ENDIF.
            ms_copy-progid = ms_header-progid.
            ms_copy-alv_key = ms_header-alv_key.
            APPEND ms_copy TO mt_item.
          ENDIF.
        ENDLOOP.
    ENDCASE.
    CLEAR mt_copy[].
    mo_alv_item->refresh( ).

  ENDMETHOD.


  METHOD display_copy.
    super->display_copy( ).
    mo_alv_copy->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                           CHANGING ct_data = mt_copy ).
  ENDMETHOD.


  METHOD display_header.
    DATA lt_toolbar TYPE ui_functions.
    IF mo_alv_header IS INITIAL.
      CREATE OBJECT mo_alv_header
        EXPORTING
          io_parent  = io_container_header
          iv_progid  = sy-cprog
          iv_alv_key = mv_header_alv_key.
    ENDIF.

    lt_toolbar = mo_alv_header->get_all_exclude_toolbar( ).

    mo_alv_header->display(  EXPORTING it_drop_down  = mt_ddl
                                       it_toolbar_excluding = lt_toolbar
                             CHANGING ct_data = mt_header ).
*    IF mo_alv_header IS INITIAL.
*      mo_alv_header = NEW ZCL_BRD_alv( io_parent = io_container_header
*                                       iv_progid = sy-cprog
*                                       iv_alv_key = mv_header_alv_key
*                                        ).
*    ENDIF.
*    mo_alv_header->display(  EXPORTING it_drop_down  = mt_ddl
*                                       it_toolbar_excluding = mo_alv_header->get_all_exclude_toolbar( )
*                             CHANGING ct_data = mt_header ).

  ENDMETHOD.


  METHOD display_item.
    DATA lt_toolbar TYPE ui_functions.
    IF mo_alv_item IS INITIAL.
      CREATE OBJECT mo_alv_item
        EXPORTING
          io_parent  = io_container_item
          iv_progid  = sy-cprog
          iv_alv_key = mv_item_alv_key.
    ENDIF.
    lt_toolbar = mo_alv_item->get_all_exclude_toolbar( ).
    mo_alv_item->display( EXPORTING it_drop_down  = mt_ddl
                                    it_toolbar_excluding = lt_toolbar
                           CHANGING ct_data = mt_item ).
*    IF mo_alv_item IS INITIAL.
*      mo_alv_item = NEW ZCL_BRD_alv( io_parent = io_container_item
*                                         iv_progid = sy-cprog
*                                         iv_alv_key = mv_item_alv_key
*                                         ).
*    ENDIF.
*    mo_alv_item->display( EXPORTING it_drop_down  = mt_ddl
*                                    it_toolbar_excluding = mo_alv_item->get_all_exclude_toolbar( )
*                           CHANGING ct_data = mt_item ).
  ENDMETHOD.


  METHOD display_source.
    super->display_source( ).
    mo_alv_source->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                            CHANGING ct_data = mt_source ).
  ENDMETHOD.


  METHOD display_template.
    super->display_template( ).
    mo_alv_template->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode
                              CHANGING ct_data = mt_template ).

  ENDMETHOD.


  METHOD excel_download.
    zcl_brd_alv=>add_excel_download_sheet( it_data = mt_header ).
    zcl_brd_alv=>add_excel_download_sheet( it_data = mt_item ).

    zcl_brd_alv=>excel_download( ).
  ENDMETHOD.


  METHOD generate_source_fcat.
    DATA lt_fcat TYPE lvc_t_fcat.
    FIELD-SYMBOLS <ls_fcat> TYPE lvc_s_fcat.
    lt_fcat = generate_source( ).

    CLEAR mt_source.
    LOOP AT lt_fcat ASSIGNING <ls_fcat>.
      CLEAR ms_source.
      MOVE-CORRESPONDING <ls_fcat> TO ms_source.
      ms_source-progid = mv_selected_prog_id.
      ms_source-alv_key = mv_selected_alv_key.
      ms_source-fcat_key = mc_default_key.
      APPEND ms_source TO mt_source.
    ENDLOOP.

    display_source( ).
*    DATA lt_fcat TYPE lvc_t_fcat.
*    lt_fcat = generate_source( ).
*
*    CLEAR mt_source.
*    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
*      CLEAR ms_source.
*      MOVE-CORRESPONDING <ls_fcat> TO ms_source.
*      ms_source-progid = mv_selected_prog_id.
*      ms_source-alv_key = mv_selected_alv_key.
*      ms_source-fcat_key = mc_default_key.
*      APPEND ms_source TO mt_source.
*    ENDLOOP.
*
*    display_source( ).
  ENDMETHOD.


  METHOD get_header_data.
    CLEAR : mt_header[], mt_header_buff[].
    TYPES : BEGIN OF lty_fcat,
              progid   TYPE ztbrd00020-progid,
              alv_key  TYPE ztbrd00020-alv_key,
              fcat_key TYPE ztbrd00020-fcat_key,
            END OF lty_fcat.
    DATA lt_fcat TYPE TABLE OF lty_fcat.

    IF mr_fcat_key[] IS NOT INITIAL.
      SELECT progid alv_key fcat_key
      INTO TABLE lt_fcat
      FROM ztbrd00020
      WHERE progid IN mr_prog
        AND alv_key IN mr_alv_key
        AND fcat_key IN mr_fcat_key.
      IF lt_fcat[] IS NOT INITIAL.
        SELECT *
        INTO CORRESPONDING FIELDS OF TABLE mt_header
        FROM ztbrd00010
          FOR ALL ENTRIES IN lt_fcat
        WHERE progid = lt_fcat-progid
          AND alv_key = lt_fcat-alv_key.
      ENDIF.
    ELSE.
      SELECT *
      INTO CORRESPONDING FIELDS OF TABLE mt_header
      FROM ztbrd00010
      WHERE progid IN mr_prog
      AND alv_key IN mr_alv_key.
    ENDIF.

    IF lines( mt_header ) EQ 1.
      READ TABLE mt_header INTO ms_header INDEX 1.
      mv_selected_alv_key = ms_header-alv_key.
      mv_selected_prog_id = ms_header-progid.
      get_item_data( EXPORTING iv_prog = mv_selected_prog_id
                               iv_alv_key = mv_selected_alv_key ).
    ENDIF.
  ENDMETHOD.


  METHOD get_item_data.
    CLEAR : mt_item[], mt_item_buff[], ms_edit_flag.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE mt_item
      FROM ztbrd00020
      WHERE progid = iv_prog
      AND alv_key = iv_alv_key
      AND fcat_key IN mr_fcat_key.


  ENDMETHOD.


  METHOD header_addr_click.
    CLEAR : ms_header.

    ms_header-info_fname = 'RCOLOR'.
    ms_header-ctab_fname = 'COLTAB'.
    ms_header-stylefname = 'CELLTAB'.
    ms_header-zebra = 'X'.
    ms_header-sel_mode = 'D'.

    INSERT ms_header INTO mt_header INDEX 1.

    mo_alv_header->refresh( ).
  ENDMETHOD.


  METHOD header_canc_click.
    IF zcl_brd_alv=>confirm_cancel( ) NE abap_true.
      EXIT.
    ENDIF.

    CLEAR mt_header[].
    mt_header[] = mt_header_buff[].
    CLEAR : mt_header_buff[].

    mo_alv_header->set_all_field_unchangeable( ).
    mo_alv_header->display( EXPORTING iv_mode = zcl_brd_alv=>mc_display_mode
                             CHANGING ct_data  = mt_header ).
  ENDMETHOD.


  METHOD header_chan_click.
    CLEAR mt_header_buff.
    mt_header_buff[] = mt_header[].

    mo_alv_header->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode ).
  ENDMETHOD.


  METHOD header_crea_click.
    CLEAR mt_header_buff[].
    mt_header_buff[] = mt_header[].
    CLEAR : mt_header[], ms_header .

    ms_header-info_fname = 'RCOLOR'.
    ms_header-ctab_fname = 'COLTAB'.
    ms_header-stylefname = 'CELLTAB'.
    ms_header-zebra = 'X'.
    ms_header-sel_mode = 'D'.



    APPEND ms_header TO mt_header.

    mo_alv_header->set_all_field_changeable( ).
    mo_alv_header->display( EXPORTING iv_mode = zcl_brd_alv=>mc_create_mode
                             CHANGING ct_data  = mt_header ).
  ENDMETHOD.


  METHOD header_dele_click.
    DATA : lt_header    TYPE TABLE OF ztbrd00010,
           lt_item      TYPE TABLE OF ztbrd00020,
           lt_item_text TYPE TABLE OF ztbrd00070,
           ls_header    TYPE ztbrd00010.
    CLEAR : mt_rows[], ms_rows, lt_header[], lt_item[].
    mo_alv_header->get_selected_rows(
     IMPORTING
       et_index_rows = mt_rows
       et_row_no     = mt_row_ids ).

    IF mt_rows[] IS INITIAL.
      MESSAGE 'No Row Selected.' TYPE 'I'.
      EXIT.
    ENDIF.

    IF zcl_brd_alv=>confirm_delete( ) NE abap_true.
      EXIT.
    ENDIF.

    TRY.
        IF mo_alv_header->is_display_mode( ) = abap_true.
          CLEAR mt_header_buff.
          LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
            CLEAR : ms_header.
            READ TABLE mt_header INTO ms_header INDEX ms_rows-index.
            IF sy-subrc EQ 0.
              CLEAR ls_header.
              MOVE-CORRESPONDING ms_header TO ls_header.
              APPEND ls_header TO lt_header.
            ENDIF.
          ENDLOOP.
          DELETE  ztbrd00010 FROM TABLE lt_header.
          IF sy-subrc NE 8.
            zcl_brd_alv=>commit_delete( sy-dbcnt ).


            "delete item
            SELECT * INTO TABLE lt_item
            FROM ztbrd00020
            FOR ALL ENTRIES IN lt_header
            WHERE progid = lt_header-progid
              AND alv_key = lt_header-alv_key.
            DELETE ztbrd00020 FROM TABLE lt_item.

            "delete item text
            SELECT progid alv_key fcat_key fieldname spras
            INTO CORRESPONDING FIELDS OF TABLE lt_item_text
            FROM ztbrd00070
            FOR ALL ENTRIES IN lt_item
            WHERE progid = lt_item-progid
              AND alv_key = lt_item-alv_key
              AND fcat_key = lt_item-fcat_key
              AND fieldname = lt_item-fieldname.
            DELETE ztbrd00070 FROM TABLE lt_item_text.

            SORT mt_rows BY index DESCENDING.
            LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
              DELETE mt_header INDEX ms_rows-index.
            ENDLOOP.
            mo_alv_header->refresh(  ).

          ELSE.
            zcl_brd_alv=>rollback_fail( ).
          ENDIF.
        ENDIF.
        IF mo_alv_header->is_upload_mode( ) = abap_true.
          SORT mt_rows BY index DESCENDING.
          LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
            DELETE mt_header INDEX ms_rows-index.
          ENDLOOP.
          mo_alv_header->refresh(  ).
        ENDIF.

      CATCH cx_root INTO mo_exception.
        zcl_brd_alv=>catch_exception( io_root = mo_exception ).
    ENDTRY.
  ENDMETHOD.


  METHOD header_delr_click.
    mo_alv_header->delete_selected_rows( ).
  ENDMETHOD.


  METHOD header_double_click.
    DATA : lt_scol  TYPE lvc_t_scol,
           ls_scol  TYPE lvc_s_scol,
           lv_tabix TYPE sy-tabix.
    READ TABLE mt_header INTO ms_header INDEX is_row_no-row_id.
    IF sy-subrc EQ 0.
      CLEAR mt_rows.
      APPEND is_row TO mt_rows. "double click한 header의 row를 선택 표시 함
      mo_alv_header->set_selected_rows( EXPORTING it_index_rows = mt_rows ).

      mv_selected_prog_id = ms_header-progid.
      mv_selected_alv_key = ms_header-alv_key.
      get_item_data( EXPORTING iv_prog = ms_header-progid iv_alv_key = ms_header-alv_key ).

      LOOP AT mt_item INTO ms_item.
        IF strlen( ms_item-emphasize ) EQ 4.
          mo_alv_item->set_cell_color( EXPORTING iv_fieldname = 'EMPHASIZE'
                                                iv_rowid = sy-tabix
                                                iv_color =  ms_item-emphasize+1 ).
        ENDIF.
      ENDLOOP.

      mo_alv_item->display( EXPORTING iv_mode = zcl_brd_alv=>mc_display_mode
                            CHANGING ct_data  = mt_item ).
    ENDIF.
  ENDMETHOD.


  METHOD header_save_click.
    DATA :lt_header TYPE TABLE OF ztbrd00010,
          ls_header TYPE ztbrd00010.
    CLEAR : mt_rows[], ms_rows.
    mo_alv_header->get_selected_rows(
     IMPORTING
       et_index_rows = mt_rows
       et_row_no     = mt_row_ids ).

    IF mt_rows[] IS INITIAL.
      MESSAGE 'No Row Selected.' TYPE 'I'.
      EXIT.
    ENDIF.

    IF zcl_brd_alv=>confirm_save( ) NE abap_true.
      EXIT.
    ENDIF.
    CLEAR mt_header_buff[].
    LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
      CLEAR ms_header.
      READ TABLE mt_header INTO ms_header INDEX ms_rows-index.
      IF sy-subrc EQ 0.
        CLEAR ls_header.
        MOVE-CORRESPONDING ms_header TO ls_header.
        IF mo_alv_header->is_create_mode( ) = abap_true
          OR mo_alv_header->is_upload_mode( ) = abap_true
          OR ls_header-erdat IS INITIAL.
          _set_timestamp_create ls_header.
        ELSEIF mo_alv_header->is_change_mode( ) = abap_true.
          _set_timestamp_change ls_header.
        ENDIF.
        APPEND ls_header TO lt_header.
      ENDIF.
    ENDLOOP.
    TRY.
        IF mo_alv_header->is_create_mode( ) = abap_true.
          MODIFY ztbrd00010 FROM TABLE lt_header.
        ENDIF.
        IF mo_alv_header->is_change_mode( ) = abap_true.
          MODIFY ztbrd00010 FROM TABLE lt_header.
        ENDIF.
        IF mo_alv_header->is_upload_mode( ) = abap_true.
          MODIFY ztbrd00010 FROM TABLE lt_header.
        ENDIF.

        IF sy-subrc NE 8.
          zcl_brd_alv=>commit_save( sy-dbcnt ).

          get_header_data( ).
          mo_alv_header->set_all_field_unchangeable( ).
          mo_alv_header->display( EXPORTING iv_mode = zcl_brd_alv=>mc_display_mode
                                   CHANGING ct_data  = mt_header ).
        ELSE.
          zcl_brd_alv=>rollback_fail( ).
        ENDIF.
      CATCH cx_root INTO mo_exception.
        zcl_brd_alv=>catch_exception( io_root = mo_exception ).
    ENDTRY.
  ENDMETHOD.


  METHOD header_upld_click.
*    TYPES : BEGIN OF lty_header,
*              progid            TYPE ZTBRD00010-progid,
*              alv_key           TYPE ZTBRD00010-alv_key,
*              zcreate           TYPE ZTBRD00010-zcreate,
*              zupdate           TYPE ZTBRD00010-zupdate,
*              zdelete           TYPE ZTBRD00010-zdelete,
*              zexcel            TYPE ZTBRD00010-zexcel,
*              zrefresh          TYPE ZTBRD00010-zrefresh,
*              db_click          TYPE ZTBRD00010-db_click,
*              zebra             TYPE ZTBRD00010-zebra,
*              smalltitle        TYPE ZTBRD00010-smalltitle,
*              sel_mode          TYPE ZTBRD00010-sel_mode,
*              edit              TYPE ZTBRD00010-edit,
*              optimize_all_cols TYPE ZTBRD00010-optimize_all_cols,
*              change_evt        TYPE ZTBRD00010-change_evt,
*              ctab_fname        TYPE ZTBRD00010-ctab_fname,
*              stylefname        TYPE ZTBRD00010-stylefname,
*            END OF lty_header.
*    DATA : lt_header TYPE TABLE OF lty_header,
*           ls_header TYPE lty_header.
*    DATA lv_subrc TYPE sy-subrc .
*    ZCL_BRD_alv=>excel_upload( CHANGING ct_data = lt_header[]
*                               RECEIVING ev_subrc = lv_subrc ).
*    IF lv_subrc  EQ 0.
*      CLEAR mt_header_buff[].
*      mt_header_buff[] = mt_header[].
*      CLEAR : mt_header[], ms_header .
*      LOOP AT lt_header INTO ls_header.
*        MOVE-CORRESPONDING ls_header TO ms_header.
*        APPEND ms_header TO mt_header.
*      ENDLOOP.
*
*      mo_alv_header->set_all_field_changeable( ).
*      mo_alv_header->display( EXPORTING iv_mode = ZCL_BRD_alv=>mc_upload_mode
*                            CHANGING ct_data  = mt_header ).
*
*    ENDIF.
  ENDMETHOD.


  METHOD item_ad10_click.
    CLEAR : ms_item.
    ms_item-progid = ms_header-progid.
    ms_item-alv_key = ms_header-alv_key.
    DO 10 TIMES.
      INSERT ms_item INTO mt_item INDEX 1.
    ENDDO.
    mo_alv_item->refresh( ).
  ENDMETHOD.


  METHOD item_addr_click.
    CLEAR : ms_item.
    ms_item-progid = ms_header-progid.
    ms_item-alv_key = ms_header-alv_key.
    INSERT ms_item INTO mt_item INDEX 1.

    mo_alv_item->refresh( ).
  ENDMETHOD.


  METHOD item_canc_click.
    IF zcl_brd_alv=>confirm_cancel( ) NE abap_true.
      EXIT.
    ENDIF.

    CLEAR mt_item[].
    mt_item[] = mt_item_buff[].
    CLEAR : mt_item_buff[].

    mo_alv_item->display( EXPORTING iv_mode = zcl_brd_alv=>mc_display_mode
                           CHANGING ct_data  = mt_item ).

  ENDMETHOD.


  METHOD item_changed.
    FIELD-SYMBOLS : <ls_item> TYPE gty_item.
    CASE iv_column.
      WHEN 'COL_POS'.
        IF iv_value EQ 999.
          READ TABLE mt_item ASSIGNING <ls_item> INDEX iv_row_id.
          IF sy-subrc EQ 0.
            <ls_item>-tech = abap_true.
          ENDIF.
        ENDIF.
      WHEN 'TECH'.
        IF iv_value EQ abap_true.
          READ TABLE mt_item ASSIGNING <ls_item> INDEX iv_row_id.
          IF sy-subrc EQ 0.
            <ls_item>-col_pos = 999.
          ENDIF.
        ENDIF.
    ENDCASE.
    mo_alv_item->refresh( ).
  ENDMETHOD.


  METHOD item_chan_click.
    DATA lv_tabix TYPE sy-tabix.
    IF mv_selected_alv_key IS INITIAL.
      MESSAGE 'Header Line을 선택하세요.' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    IF mv_selected_prog_id IS INITIAL.
      MESSAGE 'Header Line을 선택하세요.' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    CLEAR mt_item_buff.
    mt_item_buff[] = mt_item[].
*  mo_alv_item->set_all_field_changeable( ).
    LOOP AT mt_item INTO ms_item.
      lv_tabix = sy-tabix.
      IF ms_header-alv_key IS NOT INITIAL.
        mo_alv_item->set_cell_unchangeable( EXPORTING iv_fieldname = 'ALV_KEY'
                                                      iv_rowid = lv_tabix  ).
      ENDIF.
      IF ms_header-progid IS NOT INITIAL.
        mo_alv_item->set_cell_unchangeable( EXPORTING iv_fieldname = 'PROGID'
                                                      iv_rowid = lv_tabix  ).
      ENDIF.

    ENDLOOP.

    mo_alv_item->display( EXPORTING iv_mode = zcl_brd_alv=>mc_change_mode ).
  ENDMETHOD.


  METHOD item_crea_click.
    IF mv_selected_alv_key IS INITIAL.
      MESSAGE 'Header Line을 선택하세요.' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    IF mv_selected_prog_id IS INITIAL.
      MESSAGE 'Header Line을 선택하세요.' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    CLEAR mt_item_buff[].
    mt_item_buff[] = mt_item[].
    CLEAR : mt_item[], ms_item .
    ms_item-progid = mv_selected_prog_id.
    ms_item-alv_key = mv_selected_alv_key.

    APPEND ms_item TO mt_item.

    mo_alv_item->display( EXPORTING iv_mode = zcl_brd_alv=>mc_create_mode
                           CHANGING ct_data  = mt_item ).
  ENDMETHOD.


  METHOD item_dele_click.
    DATA : lt_item TYPE TABLE OF ztbrd00020,
           ls_item TYPE ztbrd00020.
    DATA : lt_item_text TYPE TABLE OF ztbrd00070.
    CLEAR : mt_rows[], ms_rows.
    mo_alv_item->get_selected_rows(
     IMPORTING
       et_index_rows = mt_rows
       et_row_no     = mt_row_ids ).

    IF mt_rows[] IS INITIAL.
      MESSAGE 'No Row Selected.' TYPE 'I'.
      EXIT.
    ENDIF.

    IF zcl_brd_alv=>confirm_delete( ) NE abap_true.
      EXIT.
    ENDIF.

    TRY.
        IF mo_alv_item->is_display_mode( ) = abap_true.
          CLEAR mt_item_buff.
          LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
            CLEAR : ms_item.
            READ TABLE mt_item INTO ms_item INDEX ms_rows-index.
            IF sy-subrc EQ 0.
              CLEAR ls_item.
              MOVE-CORRESPONDING ms_item TO ls_item.
              APPEND ls_item TO lt_item.
            ENDIF.
          ENDLOOP.
          DELETE ztbrd00020 FROM TABLE lt_item.
          IF sy-subrc NE 8.
            zcl_brd_alv=>commit_delete( sy-dbcnt ).
            SORT mt_rows BY index DESCENDING.
            LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
              DELETE mt_item INDEX ms_rows-index.
            ENDLOOP.

            "delete item text
            SELECT progid alv_key fcat_key fieldname spras
            INTO CORRESPONDING FIELDS OF TABLE lt_item_text
            FROM ztbrd00070
            FOR ALL ENTRIES IN lt_item
            WHERE progid = lt_item-progid
              AND alv_key = lt_item-alv_key
              AND fcat_key = lt_item-fcat_key
              AND fieldname = lt_item-fieldname.
            DELETE ztbrd00070 FROM TABLE lt_item_text.

            mo_alv_item->refresh(  ).

          ELSE.
            zcl_brd_alv=>rollback_fail( ).
          ENDIF.
        ENDIF.

      CATCH cx_root INTO mo_exception.
        zcl_brd_alv=>catch_exception( io_root = mo_exception ).
    ENDTRY.
  ENDMETHOD.


  METHOD item_delr_click.
    mo_alv_item->delete_selected_rows( ).
  ENDMETHOD.


  METHOD item_double_click.
    DEFINE _set_edit.
      IF ms_edit_flag-&1 EQ abap_true.
        ms_edit_flag-&1 = abap_false.
      ELSE.
        ms_edit_flag-&1 = abap_true.
      ENDIF.
      LOOP AT mt_item INTO ms_item.
        lv_tabix = sy-tabix.
        ms_item-&1 = ms_edit_flag-&1.
        MODIFY mt_item FROM ms_item INDEX lv_tabix TRANSPORTING &1.
      ENDLOOP.
      mo_alv_item->refresh( ).
    END-OF-DEFINITION.
    DEFINE _set_text.
      LOOP AT mt_item INTO ms_item.
        lv_tabix = sy-tabix.
        ms_item-&1 = ms_item-reptext.
        MODIFY mt_item FROM ms_item INDEX lv_tabix TRANSPORTING &1.
      ENDLOOP.
      mo_alv_item->refresh( ).
    END-OF-DEFINITION.
    DATA lv_tabix TYPE sy-tabix.
    DATA: lt_pram TYPE TABLE OF rsparams,
          ls_pram LIKE LINE OF lt_pram.
    CHECK mo_alv_item->is_display_mode( ) = abap_false.
    IF is_row_no-row_id EQ 0.
      CASE is_column-fieldname.
        WHEN 'CREA_EDIT'. _set_edit crea_edit.
        WHEN 'CHAN_EDIT'. _set_edit chan_edit.
        WHEN 'UPLD_EDIT'. _set_edit upld_edit.
        WHEN 'REPTEXT'.
          READ TABLE mt_item INTO ms_item INDEX 1.
          ls_pram-selname = 'SO_PROGI'.
          ls_pram-kind    = 'S'.
          ls_pram-sign    = 'I'.
          ls_pram-option  = 'EQ'.
          ls_pram-low     = ms_item-progid.
          APPEND ls_pram TO lt_pram.
          ls_pram-selname = 'SO_ALVK'.
          ls_pram-low     = ms_item-alv_key.
          APPEND ls_pram TO lt_pram.
          ls_pram-selname = 'SO_FCATK'.
          ls_pram-low     = ms_item-fcat_key.
          APPEND ls_pram TO lt_pram.

          SUBMIT zlcmz0050 USING SELECTION-SCREEN '1000'
               WITH SELECTION-TABLE lt_pram
               WITH pa_balv EQ abap_true
               WITH pa_talv EQ abap_false
           AND RETURN.

        WHEN 'SCRTEXT_S'. _set_text scrtext_s.
        WHEN 'SCRTEXT_M'. _set_text scrtext_m.
        WHEN 'SCRTEXT_L'. _set_text scrtext_l.
        WHEN 'COL_POS'.
          CLEAR lv_tabix.
          LOOP AT mt_item INTO ms_item.

            ADD 1 TO lv_tabix.
            ms_item-col_pos = lv_tabix.
            MODIFY mt_item FROM ms_item INDEX sy-tabix TRANSPORTING col_pos.

          ENDLOOP.
          mo_alv_item->refresh( ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD item_prt_click.
    FIELD-SYMBOLS <ls_item> TYPE gty_item.
    LOOP AT mt_item ASSIGNING <ls_item>.
      set_pretty_printer( EXPORTING iv_fieldname = <ls_item>-fieldname
                          CHANGING cs_fields = <ls_item> ).

    ENDLOOP.
    mo_alv_item->refresh( ).
  ENDMETHOD.


  METHOD item_save_click.
    DATA : lt_item TYPE TABLE OF ztbrd00020,
           ls_item TYPE ztbrd00020.
    CLEAR : mt_rows[], ms_rows.
    mo_alv_item->get_selected_rows(
     IMPORTING
       et_index_rows = mt_rows
       et_row_no     = mt_row_ids ).

    IF zcl_brd_alv=>confirm_save( ) NE abap_true.
      EXIT.
    ENDIF.

    IF mt_rows[] IS INITIAL.
      LOOP AT mt_item INTO ms_item.
        ms_rows-index = sy-tabix.
        APPEND ms_rows TO mt_rows.
      ENDLOOP.
    ENDIF.

    CLEAR mt_item_buff[].
    LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
      CLEAR ms_item.
      READ TABLE mt_item INTO ms_item INDEX ms_rows-index.
      IF sy-subrc EQ 0.
        CLEAR ls_item.
        MOVE-CORRESPONDING ms_item TO ls_item.
        IF ls_item-progid IS INITIAL.
          ls_item-progid = mv_selected_prog_id.
        ENDIF.
        IF ls_item-alv_key IS INITIAL.
          ls_item-alv_key = mv_selected_alv_key.
        ENDIF.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = ls_item-decimals_o
          IMPORTING
            output = ls_item-decimals_o.
        IF ls_item-decimals_o = '0'.
          CLEAR ls_item-decimals_o.
        ENDIF.

        IF mo_alv_item->is_create_mode( ) = abap_true
          OR mo_alv_item->is_upload_mode( ) = abap_true
          OR ls_item-erdat IS INITIAL.
          _set_timestamp_create ls_item.
        ELSEIF mo_alv_item->is_change_mode( ) = abap_true.
          _set_timestamp_change ls_item.
        ENDIF.
        APPEND ls_item TO lt_item.
      ENDIF.
    ENDLOOP.
    TRY.
        IF mo_alv_item->is_create_mode( ) = abap_true.
          MODIFY ztbrd00020 FROM TABLE lt_item.
        ENDIF.
        IF mo_alv_item->is_change_mode( ) = abap_true.
          MODIFY ztbrd00020 FROM TABLE lt_item.
        ENDIF.
        IF mo_alv_item->is_upload_mode( ) = abap_true.
          MODIFY ztbrd00020 FROM TABLE lt_item.
        ENDIF.

        IF sy-subrc NE 8.
          zcl_brd_alv=>commit_save( sy-dbcnt ).
          get_item_data( EXPORTING iv_prog = ms_header-progid iv_alv_key = ms_header-alv_key ).

          mo_alv_item->display( EXPORTING iv_mode = zcl_brd_alv=>mc_display_mode
                                   CHANGING ct_data  = mt_item ).
        ELSE.
          zcl_brd_alv=>rollback_fail( ).
        ENDIF.
      CATCH cx_root INTO mo_exception.
        zcl_brd_alv=>catch_exception( io_root = mo_exception ).
    ENDTRY.
  ENDMETHOD.


  METHOD item_upld_click.
*    TYPES : BEGIN OF lty_item,
*              fcat_key   TYPE ZTBRD00020-fcat_key,
*              col_pos    TYPE ZTBRD00020-col_pos,
*              fieldname  TYPE ZTBRD00020-fieldname,
*              zkey       TYPE ZTBRD00020-zkey,
*              fix_column TYPE ZTBRD00020-fix_column,
*              reptext    TYPE ZTBRD00020-reptext,
*              scrtext_s  TYPE ZTBRD00020-scrtext_s,
*              scrtext_m  TYPE ZTBRD00020-scrtext_m,
*              scrtext_l  TYPE ZTBRD00020-scrtext_l,
*              icon       TYPE ZTBRD00020-icon,
*              col_opt    TYPE ZTBRD00020-col_opt,
*              outputlen  TYPE ZTBRD00020-outputlen,
*              checkbox   TYPE ZTBRD00020-checkbox,
*              no_out     TYPE ZTBRD00020-no_out,
*              tech       TYPE ZTBRD00020-tech,
*              just       TYPE ZTBRD00020-just,
*              no_sign    TYPE ZTBRD00020-no_sign,
*              no_zero    TYPE ZTBRD00020-no_zero,
*              edit_mask  TYPE ZTBRD00020-edit_mask,
*              sr_spos    TYPE ZTBRD00020-sr_spos,
*              sr_up      TYPE ZTBRD00020-sr_up,
*              emphasize  TYPE ZTBRD00020-emphasize,
*              sr_down    TYPE ZTBRD00020-sr_down,
*              crea_edit  TYPE ZTBRD00020-crea_edit,
*              chan_edit  TYPE ZTBRD00020-chan_edit,
*              upld_edit  TYPE ZTBRD00020-upld_edit,
*              do_sum     TYPE ZTBRD00020-do_sum,
*              no_sum     TYPE ZTBRD00020-no_sum,
*              button     TYPE ZTBRD00020-button,
*              lowercase  TYPE ZTBRD00020-lowercase,
*              change_evt TYPE ZTBRD00020-change_evt,
*              no_convext TYPE ZTBRD00020-no_convext,
*              convexit   TYPE ZTBRD00020-convexit,
*              hotspot    TYPE ZTBRD00020-hotspot,
*              f4availabl TYPE ZTBRD00020-f4availabl,
*              no_merging TYPE ZTBRD00020-no_merging,
*              drdn_hndl  TYPE ZTBRD00020-drdn_hndl,
*              drdn_alias TYPE ZTBRD00020-drdn_alias,
*              drdn_field TYPE ZTBRD00020-drdn_field,
*              ref_table  TYPE ZTBRD00020-ref_table,
*              ref_field  TYPE ZTBRD00020-ref_field,
*              checktable TYPE ZTBRD00020-checktable,
*              datatype   TYPE ZTBRD00020-datatype,
*              inttype    TYPE ZTBRD00020-inttype,
*              intlen     TYPE ZTBRD00020-intlen,
*              currency   TYPE ZTBRD00020-currency,
*              style      TYPE ZTBRD00020-style,
*            END OF lty_item.
*    DATA : lt_item TYPE TABLE OF lty_item,
*           ls_item TYPE lty_item.
*    DATA lv_subrc TYPE sy-subrc.
*
*    IF ms_header-progid IS INITIAL OR ms_header-alv_key IS INITIAL.
*      MESSAGE 'Header를 먼저 선택해야 합니다. Upload하고자 하는 alv key를 먼저 더블클릭 하세요.' TYPE 'I' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*
*    ZCL_BRD_alv=>excel_upload( CHANGING ct_data = lt_item[]
*                               RECEIVING ev_subrc = lv_subrc ).
*    IF lv_subrc EQ 0.
*      CLEAR mt_item_buff[].
*      mt_item_buff[] = mt_item[].
*      CLEAR : mt_item[], ms_item .
*      LOOP AT lt_item INTO ls_item.
*        MOVE-CORRESPONDING ls_item TO ms_item.
*        ms_item-progid = ms_header-progid.
*        ms_item-alv_key = ms_header-alv_key.
*        APPEND ms_item TO mt_item.
*      ENDLOOP.
*
*      mo_alv_item->set_all_field_changeable( ).
*      mo_alv_item->display( EXPORTING iv_mode = ZCL_BRD_alv=>mc_upload_mode
*                            CHANGING ct_data  = mt_item ).
*
*    ENDIF.
  ENDMETHOD.


  METHOD search_copy_source.
    DATA : lt_detail TYPE TABLE OF ztbrd00020,
           ls_detail TYPE ztbrd00020,
           ls_copy   TYPE gty_item.
    CLEAR mt_copy.

    CASE mv_copy_type.
      WHEN 'LOCAL'.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_copy
          FROM ztbrd00020
          WHERE progid = iv_progid
          AND alv_key = iv_alv_key
          AND fcat_key = iv_fcat_key.
      WHEN mc_sysid_dev.
        CLEAR lt_detail.
        CALL FUNCTION 'ZBRD_GET_ALV_CONFIG'
          DESTINATION mc_rfc_dest_dev
          EXPORTING
            iv_progid   = iv_progid
            iv_alv_key  = iv_alv_key
            iv_fcat_key = iv_fcat_key
          TABLES
            t_data      = lt_detail.
        LOOP AT lt_detail INTO ls_detail.
          MOVE-CORRESPONDING ls_detail TO ls_copy.
          APPEND ls_copy TO mt_copy.
          CLEAR : ls_detail, ls_copy.
        ENDLOOP.
      WHEN mc_sysid_prd.
        CLEAR lt_detail.
        CALL FUNCTION 'ZBRD_GET_ALV_CONFIG'
          DESTINATION mc_rfc_dest_prd
          EXPORTING
            iv_progid   = iv_progid
            iv_alv_key  = iv_alv_key
            iv_fcat_key = iv_fcat_key
          TABLES
            t_data      = lt_detail.
        LOOP AT lt_detail INTO ls_detail.
          MOVE-CORRESPONDING ls_detail TO ls_copy.
          APPEND ls_copy TO mt_copy.
          CLEAR : ls_detail, ls_copy.
        ENDLOOP.
      WHEN mc_sysid_qa.
        CLEAR lt_detail.
        CALL FUNCTION 'ZBRD_GET_ALV_CONFIG'
          DESTINATION mc_rfc_dest_qa
          EXPORTING
            iv_progid   = iv_progid
            iv_alv_key  = iv_alv_key
            iv_fcat_key = iv_fcat_key
          TABLES
            t_data      = lt_detail.
        LOOP AT lt_detail INTO ls_detail.
          MOVE-CORRESPONDING ls_detail TO ls_copy.
          APPEND ls_copy TO mt_copy.
          CLEAR : ls_detail, ls_copy.
        ENDLOOP.
    ENDCASE.
    IF mt_copy IS INITIAL.
      MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD search_structure.
    CLEAR mt_template.

    DATA : lt_field TYPE lvc_t_fcat,
           ls_field TYPE lvc_s_fcat.

    CLEAR lt_field.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = iv_strucnm
        "i_buffer_active        = abap_true      " Put this as X
        i_client_never_display = abap_true
        i_bypassing_buffer     = abap_true      " Put this as X
      CHANGING
        ct_fieldcat            = lt_field
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    LOOP AT lt_field INTO ls_field.
      CLEAR ms_template.
      MOVE-CORRESPONDING ls_field TO ms_template.
      ms_template-progid = mv_selected_prog_id.
      ms_template-alv_key = mv_selected_alv_key.
      ms_template-fcat_key = mc_default_key.
      ms_template-zkey = ls_field-key.
      ms_template-decimals_o = ms_template-decimals.
      IF ms_template-tabname EQ '1'.
        CLEAR ms_template-tabname.
      ENDIF.
      IF ms_template-ref_table EQ 'ICON'.
        CLEAR ms_template-ref_table.
      ENDIF.
      IF ms_template-fieldname EQ 'MANDT'.
        ms_template-col_pos = 999.
      ENDIF.
      " 이로직이 포함되면 모든 currency는 p타입이 되어 자리 수 오류 발생
*      IF ms_template-datatype = 'CURR'.
*        CLEAR : ms_template-ref_table, ms_template-ref_field.
*      ENDIF.
      APPEND ms_template TO mt_template.
      CLEAR ls_field.
    ENDLOOP.

    CLEAR ms_item.
    READ TABLE mt_item INTO ms_item WITH KEY fieldname = 'BALV_STATUS_ICON'.
    IF sy-subrc NE 0.
      CLEAR ms_template.
      ms_template-progid = mv_selected_prog_id.
      ms_template-alv_key = mv_selected_alv_key.
      ms_template-fcat_key = mc_default_key.
      ms_template-fieldname = 'BALV_STATUS_ICON'.
      ms_template-icon = abap_true.
      ms_template-datatype = 'CHAR'.
      ms_template-intlen = 4.
      ms_template-col_pos = 1.
      ms_template-zkey = abap_true.
      ms_template-inttype = 'C'.
      ms_template-emphasize = 'K41'.
      ms_template-just = 'C'.
      INSERT ms_template INTO mt_template INDEX 1.
    ENDIF.

  ENDMETHOD.


  METHOD set_header_data.
    CLEAR : mt_header[], mt_header_buff[].
    DATA lo_data TYPE REF TO data.
    FIELD-SYMBOLS : <lt_data> TYPE STANDARD TABLE,
                    <ls_data> TYPE any.
    ASSIGN it_tab->* TO <lt_data>.

    CREATE DATA lo_data LIKE LINE OF <lt_data>.
    ASSIGN lo_data->* TO <ls_data>.

    LOOP AT <lt_data> ASSIGNING <ls_data>.
      MOVE-CORRESPONDING <ls_data> TO ms_header.
      APPEND ms_header TO mt_header.
      CLEAR ms_header.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_item_color.
    DATA : ls_item TYPE gty_item,
           lv_str  TYPE string.

    CHECK NOT mo_alv_item->is_display_mode( ) = abap_true.

    READ TABLE mt_color INTO ms_color INDEX is_row_no-row_id.
    IF sy-subrc EQ 0.
      CLEAR ls_item.
      READ TABLE mt_item INTO ls_item INDEX ms_selected_item-row_id.

      IF sy-subrc EQ 0.
        lv_str = ms_color-col.
        CONCATENATE 'C' lv_str INTO ls_item-emphasize.
        lv_str = ms_color-int.
        CONCATENATE ls_item-emphasize lv_str INTO ls_item-emphasize.
        lv_str = ms_color-inv.
        CONCATENATE ls_item-emphasize lv_str INTO ls_item-emphasize.

        MODIFY mt_item FROM ls_item INDEX ms_selected_item-row_id.
        mo_alv_item->soft_refresh( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_item_data.
    CLEAR : mt_item[], mt_item_buff[].
    DATA lo_data TYPE REF TO data.
    FIELD-SYMBOLS : <lt_data> TYPE STANDARD TABLE,
                    <ls_data> TYPE any.
    ASSIGN it_tab->* TO <lt_data>.

    CREATE DATA lo_data LIKE LINE OF <lt_data>.
    ASSIGN lo_data->* TO <ls_data>.

    LOOP AT <lt_data> ASSIGNING <ls_data>.
      MOVE-CORRESPONDING <ls_data> TO ms_item.
      APPEND ms_item TO mt_item.
      CLEAR ms_item.
    ENDLOOP.
  ENDMETHOD.


  METHOD source_save_click.

    CLEAR : mt_rows[], ms_rows.
    mo_alv_source->get_selected_rows(
     IMPORTING
       et_index_rows = mt_rows
       et_row_no     = mt_row_ids ).

    IF mt_rows[] IS INITIAL.
      MESSAGE 'No Row Selected.' TYPE 'I'.
      EXIT.
    ENDIF.

    mo_alv_source->check_changed_data( ).

    LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
      READ TABLE mt_source INTO ms_source INDEX ms_rows-index.
      IF sy-subrc EQ 0.
        ms_source-progid = ms_header-progid.
        ms_source-alv_key = ms_header-alv_key.
        CHECK ms_source-fcat_key IS NOT INITIAL.
*        ms_source-fcat_key = mc_default_key.
        APPEND ms_source TO mt_item.
      ENDIF.
    ENDLOOP.

    CLEAR mt_source[].
    mo_alv_item->refresh( ).

  ENDMETHOD.


  METHOD template_save_click.
    DATA ls_item TYPE gty_item.
    CLEAR : mt_rows[], ms_rows.
    mo_alv_template->get_selected_rows(
     IMPORTING
       et_index_rows = mt_rows
       et_row_no     = mt_row_ids ).

    IF mt_rows[] IS INITIAL.
      MESSAGE 'No Row Selected.' TYPE 'I'.
      EXIT.
    ENDIF.

    mo_alv_template->check_changed_data( ).

    LOOP AT mt_rows INTO ms_rows WHERE rowtype EQ space.
      CLEAR ms_template.
      READ TABLE mt_template INTO ms_template INDEX ms_rows-index.
      IF sy-subrc EQ 0.
        IF ms_template-zkey EQ abap_true.
          IF ms_template-fieldname NE 'BALV_STATUS_ICON'.
            ms_template-crea_edit = abap_true.
            ms_template-chan_edit = abap_false.
            ms_template-upld_edit = abap_true.
          ENDIF.
        ELSE.
          ms_template-crea_edit = abap_true.
          ms_template-chan_edit = abap_true.
          ms_template-upld_edit = abap_true.
        ENDIF.

        APPEND ms_template TO mt_item.
      ENDIF.
    ENDLOOP.

    CLEAR mt_template[].
    mo_alv_item->refresh( ).
  ENDMETHOD.
ENDCLASS.
