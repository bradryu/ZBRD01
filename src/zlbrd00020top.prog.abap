*&---------------------------------------------------------------------*
*&  Include           ZLBRD00020TOP
*&---------------------------------------------------------------------*
TABLES : ztbrd00020.
*&---------------------------------------------------------------------*
*&  DEFINE
*&---------------------------------------------------------------------*
DEFINE _set_screen_enable.
  screen-input = 1 .
  screen-invisible = 0 .
END-OF-DEFINITION.
DEFINE _set_screen_disable.
  screen-input = 0 .
  screen-invisible = 1 .
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&  DATA
*&---------------------------------------------------------------------*
DATA : gv_alv_class      TYPE seoclsname,
       gv_tree_alv_class TYPE seoclsname.
DATA : gt_top  TYPE stringtab,
       gt_main TYPE stringtab,
       gt_f01  TYPE stringtab,
       gt_100  TYPE stringtab.

DATA : go_splitter_container        TYPE REF TO cl_gui_splitter_container,
       go_splitter_container_editor TYPE REF TO cl_gui_splitter_container,
       go_splitter_container_db     TYPE REF TO cl_gui_splitter_container,
       go_container_body            TYPE REF TO cl_gui_container,
       go_container_alv             TYPE REF TO cl_gui_container,
       go_container_h               TYPE REF TO cl_gui_container,
       go_container_i               TYPE REF TO cl_gui_container,
       go_container1                TYPE REF TO cl_gui_container,
       go_container2                TYPE REF TO cl_gui_container,
       go_container3                TYPE REF TO cl_gui_container,
       go_container4                TYPE REF TO cl_gui_container,
       go_textedit_top              TYPE REF TO cl_gui_abapedit,
       go_textedit_main             TYPE REF TO cl_gui_abapedit,
       go_textedit_f01              TYPE REF TO cl_gui_abapedit,
       go_textedit_100              TYPE REF TO cl_gui_abapedit.
DATA : gt_toolbar_all_excluding   TYPE ui_functions.
DATA : go_container_top	 TYPE REF TO cl_gui_container,
       go_container_main TYPE REF TO cl_gui_container,
       go_container_f01  TYPE REF TO cl_gui_container,
       go_container_100  TYPE REF TO cl_gui_container.
DATA : gv_mode              TYPE char04,   "screen change display mode
       gv_on_chainging_flag,
       gv_inner_join,
       gv_outer_join.
DATA : go_tab   TYPE REF TO data,
       go_struc TYPE REF TO data.

DATA : go_alv   TYPE REF TO zcl_brd_alv,
       go_alv_h TYPE REF TO zcl_brd_alv,
       go_alv_i TYPE REF TO zcl_brd_alv.
DATA : BEGIN OF gt_f4 OCCURS 0,
         zno      TYPE int4,
         progid   LIKE ztbrd00020-progid,
         alv_key  LIKE ztbrd00020-alv_key,
         fcat_key LIKE ztbrd00020-fcat_key,
       END OF gt_f4.
DATA : BEGIN OF gt_double_alv OCCURS 0,
         progid   LIKE ztbrd00020-progid,
         type(12),
         tabnm    TYPE tabname,
         clsname  TYPE seoclsname,
         alv_key  LIKE ztbrd00020-alv_key,
         fcat_key LIKE ztbrd00020-fcat_key,
       END OF gt_double_alv.
DATA : BEGIN OF gt_key_map OCCURS 0,
         h_field TYPE fieldname,
         icon    TYPE icon-id,
         i_field TYPE fieldname,
       END OF gt_key_map.


DATA : BEGIN OF gt_multi_alv OCCURS 0,
         no       TYPE sy-tabix,
         tabnm    TYPE tabname,
         alv_key  TYPE char10,
         fcat_key TYPE char10,
       END OF gt_multi_alv.

DATA : BEGIN OF gs_tree_grid,
         t_cls      TYPE seoclsname,
         t_alv_key  TYPE ztbrd00020-alv_key VALUE 'TALV',
         t_fcat_key TYPE ztbrd00020-fcat_key VALUE 'DEFAULT',
         g_cls      TYPE seoclsname,
         g_alv_key  TYPE ztbrd00020-alv_key VALUE 'BALV',
         g_fcat_key TYPE ztbrd00020-fcat_key VALUE 'DEFAULT',
       END OF gs_tree_grid.

DATA : gs_alv_config   TYPE   ztbrd00010,
       gt_alv_fcat     TYPE   zybrd00020,
       gs_alv_h_config TYPE   ztbrd00010,
       gt_alv_h_fcat   TYPE   zybrd00020,
       gs_alv_i_config TYPE   ztbrd00010,
       gt_alv_i_fcat   TYPE   zybrd00020,
       gs_talv_config  TYPE   ztbrd00011,
       gt_talv_fcat    TYPE   zybrd00021.
DATA : gv_repti     TYPE repti,
       gv_tcode_gen TYPE flag.
*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text017.
PARAMETER : pa_modu TYPE zesg_module OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text010.
PARAMETER : pa_prog TYPE ztbrd00020-progid OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text004 .
PARAMETER : pa_new AS CHECKBOX  DEFAULT 'X' .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : pa_r1 RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND set .
SELECTION-SCREEN COMMENT (20) text014 FOR FIELD pa_r1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETER : pa_s1 RADIOBUTTON GROUP g2  USER-COMMAND set DEFAULT 'X' MODIF ID s1 .
SELECTION-SCREEN COMMENT (20) text018 FOR FIELD pa_s1 MODIF ID s1 .
PARAMETER : pa_s2 RADIOBUTTON GROUP g2 MODIF ID s1 .
SELECTION-SCREEN COMMENT (21) text019 FOR FIELD pa_s2 MODIF ID s1 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : pa_r2 RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (20) text015 FOR FIELD pa_r2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.

PARAMETER : pa_s4 RADIOBUTTON GROUP g3 USER-COMMAND set DEFAULT 'X' MODIF ID s2 .
SELECTION-SCREEN COMMENT (15) text021 FOR FIELD pa_s4 MODIF ID s2 .
PARAMETER : pa_s3 RADIOBUTTON GROUP g3 MODIF ID s2 .
SELECTION-SCREEN COMMENT (15) text020 FOR FIELD pa_s3 MODIF ID s2 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : pa_r3 RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (20) text016 FOR FIELD pa_r3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : pa_r4 RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (20) text022 FOR FIELD pa_r4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : pa_r5 RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (20) text025 FOR FIELD pa_r5.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETER : pa_s6 RADIOBUTTON GROUP g4  USER-COMMAND set DEFAULT 'X' MODIF ID s3 .
SELECTION-SCREEN COMMENT (22) text024 FOR FIELD pa_s6 MODIF ID s3 .
PARAMETER : pa_s5 RADIOBUTTON GROUP g4  MODIF ID s3 .
SELECTION-SCREEN COMMENT (25) text023 FOR FIELD pa_s5 MODIF ID s3 .

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text011 MODIF ID tb.
PARAMETER : pa_tabnm LIKE dd02l-tabname MODIF ID tb .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text012 MODIF ID sg.
PARAMETER : pa_alv TYPE ztbrd00020-alv_key MODIF ID sg.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text013 MODIF ID sg.
PARAMETER : pa_fcat TYPE ztbrd00020-fcat_key MODIF ID sg.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b2.
