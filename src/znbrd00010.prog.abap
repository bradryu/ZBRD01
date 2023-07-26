*************************************************************************
* Modules / Sub Module : CM /
*&---------------------------------------------------------------------
* T_CODE             : ZNBRD00010
* WRITER             : BRAD.RYU
* DATE               : 2015.12.10
* TYPE               : Include Report
* Description        : 공통 변수 include프로그램
*
*************************************************************************
*              Changed History
*&---------------------------------------------------------------------**
*Changed Number     Changed Date   Writer    Changed      Description
*&---------------------------------------------------------------------**
*      N            2015.12.10    BRAD.RYU   CREATE       CREATE       *
*************************************************************************
************************************************************************
* Types                                                                *
************************************************************************

TYPE-POOLS:sgui, "Typen und Konstanten f#r Funktionsgruppe SGUI
           stree,"Hierarchiebausteine
           slis. "Globale Typen f#r generische Listbausteine
TYPE-POOLS:slis.

************************************************************************
* Tables                                                               *
************************************************************************

TABLES : sscrfields.
TABLES : icon.

************************************************************************
* Include                                                              *
************************************************************************
*OLE
INCLUDE <icon>.

************************************************************************
* DEFINITION
************************************************************************
DEFINE _remove_1st_chr.
  IF &1 IS NOT INITIAL AND &1(1) EQ &2.
    &1 = &1+1.
  ENDIF.
END-OF-DEFINITION.
DEFINE _set_active_screen.
  IF &1 EQ c_x AND screen-group1 EQ &2.
    screen-input = 1 .
    screen-invisible = 0 .
  ENDIF.
END-OF-DEFINITION.
DEFINE _init.
  CLEAR &1. REFRESH &1.
END-OF-DEFINITION.
************************************************************************
* Field Symbols
************************************************************************

FIELD-SYMBOLS:<gv_field>.
FIELD-SYMBOLS:<gv_f4tab> TYPE lvc_t_modi.
FIELD-SYMBOLS:<gv_row1> TYPE lvc_s_roid,
              <gv_row2> TYPE lvc_s_roid.
FIELD-SYMBOLS <gt_download_tab> TYPE STANDARD TABLE.




************************************************************************
* Ranges                                                               *
************************************************************************

RANGES:gr_datum FOR sy-datum.


************************************************************************
* Internal Table                                                       *
************************************************************************
*BDC

DATA : gt_bdctab LIKE bdcdata OCCURS 0 WITH HEADER LINE,
       gt_bdcmsg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.


************************************************************************
* Global Data                                                          *
************************************************************************
DATA : gt_excel TYPE soi_generic_table,
       gs_excel TYPE soi_generic_item.

*F4
DATA : gt_dynpread   LIKE dynpread OCCURS 0 WITH HEADER LINE,
       gt_ddshretval LIKE ddshretval OCCURS 0 WITH HEADER LINE,
       gt_help_value LIKE help_value OCCURS 0 WITH HEADER LINE,
       gt_help_vtab  LIKE help_vtab  OCCURS 0  WITH HEADER LINE.

*Global Data
DATA : gv_count        TYPE i,
       gv_first        TYPE c,
       gv_flag         TYPE c,
       gv_err          TYPE c, "Error flag
       gv_sign         TYPE c,
       gv_exit         TYPE c,
       gv_bdcmode      TYPE ctu_mode, "BDC MODE,
       gv_valdate      LIKE sy-datum,
       gv_sdate        LIKE sy-datum,               "##
       gv_edate        LIKE sy-datum,               "#####
       gv_day          TYPE i,                      "##
       gv_cal_day      TYPE i,                      "####
       gv_month        TYPE n LENGTH 2,                      "##
       gv_year         TYPE n LENGTH 4,                      "##
       gv_opt          LIKE ctu_params,
       gv_answer(10)   TYPE c, "Answer
       gv_okcode       LIKE sy-ucomm, "ok code
       gv_save_okcode  LIKE sy-ucomm,
       gv_filename     TYPE rlgrap-filename,
       gv_msgty        LIKE sy-msgty,
       gv_filename01   TYPE string,
       gv_filetype(10) TYPE c,
       gv_message(100),
       gv_fval(15),
       gv_save,
       gv_repid        TYPE sy-repid,
       gv_program_name LIKE sy-repid,
       gv_inclname     LIKE trdir-name,
       gv_extension    TYPE i,
       gv_checkfld(10),
       gv_markfld(10),
       gv_repmode(10),   "REPORT EXECUTE MODE(UPLOAD, DISPLAY)
       gv_status,     "######(X = #### ##)
       gv_lock,       "Lock = 'L', Unlock = 'U', Error = 'E'.
       gv_subrc        LIKE sy-subrc,
       gv_datum        LIKE sy-datum,
       gv_sdatum       LIKE sy-datum,
       gv_edatum       LIKE sy-datum,
       gv_week         TYPE scal-week,
       gv_dynnr        LIKE sy-dynnr,
       gv_cprog        LIKE sy-cprog,
       gv_fldvalue     LIKE help_info-fldvalue,
       gv_fieldname    LIKE help_info-dynprofld,
       gv_tabix        LIKE sy-tabix,
       gv_line         TYPE i,
       gv_width        TYPE i,
       gv_height       TYPE i,
       gv_index        TYPE lvc_index,
       gv_msg          TYPE string,
       gv_str          TYPE string,
       gv_tmp_str      TYPE string.

DATA : gv_timestamp TYPE timestamp.

DATA : gs_layout      TYPE lvc_s_layo,
       gs_alv_variant LIKE disvariant.
DATA : gs_fieldcat TYPE lvc_s_fcat.

*. Function Code 관리
DATA: BEGIN OF gt_exclude OCCURS 10,
        fcode LIKE rsmpe-func,                    " (GUI CODE)
      END   OF gt_exclude.
************************************************************************
* F4 Possible Entry
************************************************************************
*Structures
DATA : gs_f4_value TYPE seahlpres,
       gs_f4_field TYPE dfies,
       gs_f4       TYPE ddshretval,
       gs_lvc_modi TYPE lvc_s_modi,
       gs_lvc_f4   TYPE lvc_s_f4,
       gs_lvc_f4_2 TYPE lvc_s_f4.


*Internal tables
DATA : gt_f4_values TYPE TABLE OF seahlpres,
       gt_f4_fields TYPE TABLE OF dfies,
       gt_lvc_f4    TYPE lvc_t_f4,
       gt_lvc_f4_2  TYPE lvc_t_f4.

************************************************************************
* Object
************************************************************************
DATA : go_splitter_container_out TYPE REF TO cl_gui_splitter_container,
       go_splitter_container_in  TYPE REF TO cl_gui_splitter_container,
       go_container              TYPE REF TO cl_gui_container,
       go_custom_container       TYPE REF TO cl_gui_custom_container,
       go_container_desc         TYPE REF TO cl_gui_container,
       go_container_for_inner_sp TYPE REF TO cl_gui_container,
       go_container_title        TYPE REF TO cl_gui_container,
       go_document               TYPE REF TO cl_dd_document,
       go_html_viewr             TYPE REF TO cl_gui_html_viewer,
       go_docking_container      TYPE REF TO cl_gui_docking_container.





DATA : gt_rows    TYPE lvc_t_row,
       gs_rows    TYPE lvc_s_row,
       gt_row_ids TYPE lvc_t_roid,
       gs_row_id  TYPE lvc_s_roid,
       gt_celltab TYPE lvc_t_styl.
DATA : gs_mod_cells  TYPE lvc_s_modi.
DATA : go_cl_menu_xls TYPE REF TO cl_ctmenu,
       gs_stbl        TYPE lvc_s_stbl.
DATA : go_exception  TYPE REF TO cx_root.
DATA : gt_excel_data TYPE soi_generic_table,
       gs_excel_data TYPE soi_generic_item.


DATA : BEGIN OF gt_log OCCURS 0,
         icon   TYPE icon-id,
         row_no TYPE sy-tabix,
         msg    TYPE rstxtlg,
       END OF gt_log.
DATA : BEGIN OF gs_create_timestamp,
         ernam TYPE sy-uname,
         erdat TYPE sy-datum,
         erzet TYPE sy-uzeit,
         aenam TYPE sy-uname,
         aedat TYPE sy-datum,
         aezet TYPE sy-uzeit,
       END OF gs_create_timestamp.
DATA : BEGIN OF gs_change_timestamp,
         aenam TYPE sy-uname,
         aedat TYPE sy-datum,
         aezet TYPE sy-uzeit,
       END OF gs_change_timestamp.
DATA : go_download_data   TYPE REF TO data,
       gt_excel_header TYPE string_table.
************************************************************************
* Constants                                                            *
************************************************************************

CONSTANTS:c_a(1)    TYPE c VALUE 'A',
          c_b(1)    TYPE c VALUE 'B',
          c_c(1)    TYPE c VALUE 'C',
          c_d(1)    TYPE c VALUE 'D',
          c_e(1)    TYPE c VALUE 'E',
          c_f(1)    TYPE c VALUE 'F',
          c_g(1)    TYPE c VALUE 'G',
          c_h(1)    TYPE c VALUE 'H',
          c_i(1)    TYPE c VALUE 'I',
          c_j(1)    TYPE c VALUE 'J',
          c_k(1)    TYPE c VALUE 'K',
          c_l(1)    TYPE c VALUE 'L',
          c_m(1)    TYPE c VALUE 'M',
          c_n(1)    TYPE c VALUE 'N',
          c_o(1)    TYPE c VALUE 'O',
          c_p(1)    TYPE c VALUE 'P',
          c_q(1)    TYPE c VALUE 'Q',
          c_r(1)    TYPE c VALUE 'R',
          c_s(1)    TYPE c VALUE 'S',
          c_t(1)    TYPE c VALUE 'T',
          c_u(1)    TYPE c VALUE 'U',
          c_v(1)    TYPE c VALUE 'V',
          c_w(1)    TYPE c VALUE 'W',
          c_x(1)    TYPE c VALUE 'X',
          c_y(1)    TYPE c VALUE 'Y',
          c_z(1)    TYPE c VALUE 'Z',
          c_0(1)    TYPE c VALUE '0',
          c_1(1)    TYPE c VALUE '1',
          c_2(1)    TYPE c VALUE '2',
          c_3(1)    TYPE c VALUE '3',
          c_4(1)    TYPE c VALUE '4',
          c_5(1)    TYPE c VALUE '5',
          c_6(1)    TYPE c VALUE '6',
          c_7(1)    TYPE c VALUE '7',
          c_8(1)    TYPE c VALUE '8',
          c_9(1)    TYPE c VALUE '9',
          c_zero    TYPE c LENGTH 1 VALUE '0',
          c_zero2   TYPE c LENGTH 2 VALUE '00',
          c_zero3   TYPE c LENGTH 3 VALUE '000',
          c_zero4   TYPE c LENGTH 4 VALUE '0000',
          c_zero5   TYPE c LENGTH 5 VALUE '00000',
          c_space   TYPE c LENGTH 1 VALUE ' ',
          c_percent TYPE c LENGTH 1 VALUE '%',
          c_minus   TYPE c LENGTH 1 VALUE '-',
          c_separ   TYPE c LENGTH 1 VALUE '|',
          c_ast     TYPE c LENGTH 1 VALUE '*',
          c_eq      TYPE c LENGTH 2 VALUE 'EQ',
          c_bt      TYPE c LENGTH 2 VALUE 'BT',
          c_cp      TYPE c LENGTH 2 VALUE 'CP',
          c_ieq     TYPE c LENGTH 3 VALUE 'IEQ',
          c_eeq     TYPE c LENGTH 3 VALUE 'EEQ',
          c_ile     TYPE c LENGTH 3 VALUE 'ILE',
          c_ele     TYPE c LENGTH 3 VALUE 'ELE',
          c_ige     TYPE c LENGTH 3 VALUE 'IGE',
          c_ege     TYPE c LENGTH 3 VALUE 'EGE',
          c_igt     TYPE c LENGTH 3 VALUE 'IGT',
          c_egt     TYPE c LENGTH 3 VALUE 'EGT',
          c_ilt     TYPE c LENGTH 3 VALUE 'ILT',
          c_elt     TYPE c LENGTH 3 VALUE 'ELT',
          c_ibt     TYPE c LENGTH 3 VALUE 'IBT',
          c_ebt     TYPE c LENGTH 3 VALUE 'EBT',
          c_icp     TYPE c LENGTH 3 VALUE 'ICP',
          c_ecp     TYPE c LENGTH 3 VALUE 'ECP',
          c_ip0     TYPE c LENGTH 3 VALUE 'IP0', "screen group
          c_ip1     TYPE c LENGTH 3 VALUE 'IP1', "screen group
          c_kr      LIKE tfacd-ident VALUE 'KR', "##
          c_yes     TYPE c    VALUE '1', "yes
          c_no      TYPE c    VALUE '2'. "no

CONSTANTS : c_langu_en TYPE sy-langu VALUE 'E'.

*BDC Message ID
CONSTANTS:c_msgid_f5 LIKE bdcmsgcoll-msgid VALUE 'F5',
          c_msgid_06 LIKE bdcmsgcoll-msgid VALUE '06',
          c_msgid_bp LIKE bdcmsgcoll-msgid VALUE 'BP',
          c_msgid_fp LIKE bdcmsgcoll-msgid VALUE 'FP',
          c_msgid_m1 LIKE bdcmsgcoll-msgid VALUE 'M1',
          c_msgid_m7 LIKE bdcmsgcoll-msgid VALUE 'M7'.

CONSTANTS:c_rfsh    TYPE char04 VALUE 'RFSH', "Refresh
          c_crea    TYPE char04 VALUE 'CREA', "Create
          c_chan    TYPE char04 VALUE 'CHAN', "Change
          c_disp    TYPE char04 VALUE 'DISP', "Display
          c_dele    TYPE char04 VALUE 'DELE', "Delete
          c_upld    TYPE char04 VALUE 'UPLD', "Upload
          c_canc    TYPE char04 VALUE 'CANC', "Cancle
          c_exec    TYPE char04 VALUE 'EXEC', "Excute
          c_open    TYPE char04 VALUE 'OPEN', "Excute
          c_back    TYPE sy-ucomm VALUE 'BACK', "Back
          c_exit    TYPE sy-ucomm VALUE 'EXIT', "Exit
          c_save    TYPE sy-ucomm VALUE 'SAVE', "Save
          c_post    TYPE sy-ucomm VALUE 'POST', "Posting
          c_reve    TYPE sy-ucomm VALUE 'REVE', "Reversed
          c_simu    TYPE sy-ucomm VALUE 'SIMU', "Simulate
          c_prnt    TYPE sy-ucomm VALUE 'PRNT', "Print
          c_prev    TYPE sy-ucomm VALUE 'PREV', "Preview
          c_prin    TYPE sy-ucomm VALUE 'PRIN', "Print
          c_hist    TYPE sy-ucomm VALUE 'HIST', "History
          c_dbclick TYPE sy-ucomm VALUE 'DBCLK',
          c_cancel  TYPE sy-ucomm VALUE 'CANCEL'. "cancel
CONSTANTS : c_newline TYPE c VALUE cl_abap_char_utilities=>newline,
            c_htab    TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
            c_vtab    TYPE c VALUE cl_abap_char_utilities=>vertical_tab.

CONSTANTS : c_root    TYPE c LENGTH 12 VALUE '000000000000',
            c_node_id TYPE tv_nodekey VALUE '1'.
CONSTANTS : c_alv_blue(4)      VALUE 'C100',
            c_alv_gray(4)      VALUE 'C200',
            c_alv_yellow(4)    VALUE 'C300',
            c_alv_blue_gray(4) VALUE 'C400',
            c_alv_green(4)     VALUE 'C500',
            c_alv_red(4)       VALUE 'C600'.

CONSTANTS : c_htc_company  TYPE bukrs VALUE 'G100',
            c_dhtc_company TYPE bukrs VALUE 'G610',
            c_shtc_company TYPE bukrs VALUE 'G620',
            c_krw          TYPE waers VALUE 'KRW'.

CONSTANTS : c_tz_system TYPE ttzz-tzone VALUE 'UTC+9',
            c_tz_htc    TYPE ttzz-tzone VALUE 'UTC+9',
            c_tz_dhtc   TYPE ttzz-tzone VALUE 'UTC+8'.


CONSTANTS : BEGIN OF c_ucomm,
              refresh  TYPE ui_func VALUE 'RFSH', "Refresh
              create   TYPE ui_func VALUE 'CREA', "Create
              insert   TYPE ui_func VALUE 'INST', "Insert
              change   TYPE ui_func VALUE 'CHAN', "Change
              display  TYPE ui_func VALUE 'DISP', "Display
              delete   TYPE ui_func VALUE 'DELE', "Delete
              xls      TYPE ui_func VALUE '$XLS', "Excel
              upload   TYPE ui_func VALUE 'UPLD', "Upload
              download TYPE ui_func VALUE '&XXL', "Download
              cancel   TYPE ui_func VALUE 'CANC', "Cancle
              excute   TYPE ui_func VALUE 'EXEC', "Excute
              open     TYPE ui_func VALUE 'OPEN', "Excute
              back     TYPE sy-ucomm VALUE 'BACK', "Back
              exit     TYPE sy-ucomm VALUE 'EXIT', "Exit
              save     TYPE sy-ucomm VALUE 'SAVE', "Save
              simulate TYPE sy-ucomm VALUE 'SIMU', "Simulate
              print    TYPE sy-ucomm VALUE 'PRNT', "Print
              dbclick  TYPE sy-ucomm VALUE 'DBCLK',
            END OF c_ucomm.
CONSTANTS : BEGIN OF c_field,
              mandt            TYPE fieldname VALUE 'MANDT',
              balv_status_icon TYPE fieldname VALUE 'BALV_STATUS_ICON',
              celltab          TYPE fieldname VALUE 'CELLTAB',
              coltab           TYPE fieldname VALUE 'COLTAB',
              rcolor           TYPE fieldname VALUE 'RCOLOR',
              erdat            TYPE fieldname VALUE 'ERDAT',
              erzet            TYPE fieldname VALUE 'ERZET',
              ernam            TYPE fieldname VALUE 'ERNAM',
              aedat            TYPE fieldname VALUE 'AEDAT',
              aezet            TYPE fieldname VALUE 'AEZET',
              aenam            TYPE fieldname VALUE 'AENAM',
            END OF c_field.
