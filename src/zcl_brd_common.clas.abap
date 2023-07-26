class ZCL_BRD_COMMON definition
  public
  inheriting from ZCL_BRD_UTIL
  final
  create public .

public section.

  types:
    TT_HELP_VALUE type TABLE OF HELP_VALUE .

  class-data MC_IT_ADMIN_EMAIL type CHAR40 value 'noreply.sg82@sg82.com' ##NO_TEXT.
  constants MC_IT_ADMIN_PHONE type CHAR30 value '0100000000' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_AMOUNT_OUTPUT
    importing
      !IV_AMT type WRBTR optional
      !IV_WAERS type WAERS
      !IV_NUMBER_FORMAT type CHAR01 default 'X'
      !IV_NO_DECIMAL type CHAR01
      !IV_AMT_15 type BAPICURR_D optional
    returning
      value(RV_AMT_STRING) type STRING .
  class-methods GET_DOMAIN_TEXT
    importing
      !IV_DOMNAME type DOMNAME
      !IV_LANGU type SY-LANGU default SY-LANGU
      !IV_VALUE type ANY
    returning
      value(RV_TEXT) type VAL_TEXT .
  class-methods GET_F4_VALUE
    importing
      !IT_TAB type TABLE
      !IV_CPROG type SY-CPROG default SY-CPROG
      !IV_DYNNR type SY-DYNNR default SY-DYNNR
      !IT_F4_FIELD type TT_HELP_VALUE
      !IT_UPDATE_FIELD type DYNPREAD_TABTYPE optional
    exporting
      !EV_VALUE type DATA .
  class-methods GET_SCREEN_VALUE
    importing
      !IV_REPID type SYREPID default SY-CPROG
      !IV_DYNNR type SYDYNNR default SY-DYNNR
      !IV_FIELD type DYNFNAM
      !IV_STEPL type SYSTEPL optional
      !IV_UPPER_CASE type FLAG default 'X'
      !IV_INPUT_CONV type FLAG default 'X'
    returning
      value(RV_VALUE) type DYNFIELDVALUE .
  class-methods GET_SCREEN_VALUES
    importing
      !IV_REPID type SYREPID default SY-REPID
      !IV_DYNNR type SYDYNNR default SY-DYNNR
      !IV_FIELD type DYNFNAM
      !IV_STEPL type SYSTEPL optional
      !IV_UPPER_CASE type FLAG default 'X'
      !IV_INPUT_CONV type FLAG default 'X'
    changing
      !CT_DYNPFIELDS type DYNPREAD_TABTYPE .
  class-methods CALL_BROWSER
    importing
      !IV_URL type STRING .
  class-methods CONFIRM_MSG
    importing
      !IV_TITLE type CHAR255 default 'Confirm'
      !IV_QUESTION type CHAR255 default 'Confirm?'
      !IV_BTN1 type CHAR10 default 'YES'
      !IV_BTN2 type CHAR10 default 'NO'
    returning
      value(RV_RETURN) type CHAR1 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BRD_COMMON IMPLEMENTATION.


  METHOD CALL_BROWSER.
    DATA lv_url TYPE char1792.
    lv_url = iv_url.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = lv_url
        new_window             = 'X'
      EXCEPTIONS
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        OTHERS                 = 6.

  ENDMETHOD.


  METHOD CLASS_CONSTRUCTOR.

  ENDMETHOD.


  METHOD CONFIRM_MSG.
    DATA:lt_spar TYPE TABLE OF spar.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        diagnose_object       = space
        text_question         = iv_question
        text_button_1         = iv_btn1
        icon_button_1         = space
        text_button_2         = iv_btn2
        icon_button_2         = space
        default_button        = '1'
        display_cancel_button = 'X'
        userdefined_f1_help   = space
        start_column          = 25
        start_row             = 6
        popup_type            = space
        iv_quickinfo_button_1 = space
        iv_quickinfo_button_2 = space
      IMPORTING
        answer                = rv_return
      TABLES
        parameter             = lt_spar
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    CASE rv_return.
      WHEN '1'. rv_return = abap_true.
      WHEN OTHERS. rv_return = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD GET_AMOUNT_OUTPUT.

    DATA : lv_str      TYPE string,
           lv_str2     TYPE string,
           lv_char(15),
           lv_amt      TYPE p LENGTH 16 DECIMALS 2,
           lv_return   TYPE bapicurr-bapicurr.
    IF iv_amt IS NOT INITIAL.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = iv_waers
          amount_internal = iv_amt
        IMPORTING
          amount_external = lv_return.
    ELSEIF iv_amt_15 IS NOT INITIAL.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = iv_waers
          amount_internal = iv_amt_15
        IMPORTING
          amount_external = lv_return.
    ENDIF.

    lv_amt = lv_return.

    IF iv_number_format EQ abap_true.
      CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
        EXPORTING
          betrg                   = lv_amt
          new_decimal_separator   = '.'
          new_thousands_separator = ','
        IMPORTING
          string                  = lv_char.
      lv_str = lv_char.
      CONDENSE lv_str NO-GAPS.
    ELSE.
      lv_str = lv_amt.
    ENDIF.

    IF iv_no_decimal EQ abap_true.
      SPLIT lv_str AT '.' INTO lv_str lv_str2.
    ENDIF.
    rv_amt_string = lv_str.
  ENDMETHOD.


  METHOD GET_DOMAIN_TEXT.

    SELECT SINGLE ddtext
    INTO rv_text
    FROM dd07v
    WHERE domname = iv_domname
      AND ddlanguage = iv_langu
      AND domvalue_l = iv_value.

  ENDMETHOD.


  METHOD GET_F4_VALUE.
    "$. Region Sample Code
*SELECT-OPTIONS so_bukrs FOR t001-bukrs.
*SELECT-OPTIONS so_butxt FOR t001-butxt.
*SELECT-OPTIONS so_land1 FOR t001-land1.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_bukrs-low.
*  DATA lt_value TYPE TABLE OF t001.
*  DATA : lt_f4_field     TYPE wlf_tt_help_value,
*         ls_f4_field     TYPE help_value,
*         lt_update_field TYPE dynpread_t,
*         ls_update_field TYPE dynpread.
*
*  SELECT * FROM t001
*  INTO CORRESPONDING FIELDS OF TABLE lt_value.
*
*  CLEAR : lt_f4_field, ls_f4_field.
*  ls_f4_field-tabname = 'T001'.
*  ls_f4_field-fieldname = 'BUKRS'.
*  APPEND ls_f4_field TO lt_f4_field.
*  CLEAR : lt_update_field, ls_update_field.
*  ls_update_field-fieldname = 'PA_BUKRS'.
*  ls_update_field-fieldvalue = 'BUKRS'.
*  APPEND ls_update_field TO lt_update_field.
*
*  ZCL_BRD_common=>get_f4_value( EXPORTING it_tab = lt_value
*                                          it_f4_field =  lt_f4_field
*                                          it_update_field = lt_update_field
*                                 IMPORTING ev_value = so_bukrs-low ).
    "$. Endregion Sample Code
    DATA : lv_selectfield  TYPE fieldname,
           lv_select_value TYPE dynfieldvalue,
           lv_tabix        TYPE sy-tabix.
    DATA : lt_f4_field     TYPE tt_help_value,
           lt_update_field TYPE dynpread_tabtype.
    DATA lv_itab_nm TYPE string.
    FIELD-SYMBOLS <lv_value> TYPE any.
    FIELD-SYMBOLS <ls_f4_field> TYPE help_value.
    FIELD-SYMBOLS <ls_tab> TYPE any.
    FIELD-SYMBOLS <ls_update_field> TYPE dynpread.

    CHECK it_tab[] IS NOT INITIAL.
    CHECK it_f4_field[] IS NOT INITIAL.

    lt_f4_field[] = it_f4_field[].
    "선택 필드 세팅
    READ TABLE lt_f4_field ASSIGNING <ls_f4_field> WITH KEY selectflag = abap_true.
    IF sy-subrc NE 0.
      READ TABLE lt_f4_field ASSIGNING <ls_f4_field> INDEX 1.
      <ls_f4_field>-selectflag = abap_true.
    ENDIF.
    lv_selectfield = <ls_f4_field>-fieldname.

    " F4 호출
    CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
      EXPORTING
        selectfield                  = lv_selectfield
      IMPORTING
        ind                          = lv_tabix
        select_value                 = lv_select_value
      TABLES
        fields                       = lt_f4_field
        full_table                   = it_tab
      EXCEPTIONS
        full_table_empty             = 1
        no_tablestructure_given      = 2
        no_tablefields_in_dictionary = 3
        more_then_one_selectfield    = 4
        no_selectfield               = 5
        OTHERS                       = 6.

    "선택값을 화면에 DISPLAY
    CHECK sy-subrc EQ 0.
    CHECK lv_tabix IS NOT INITIAL.
    ev_value = lv_select_value.           "표시될 화면 필드

    "다른 필드 업데이트
    CHECK it_update_field[] IS NOT INITIAL.
    lt_update_field[] = it_update_field[].
    READ TABLE it_tab ASSIGNING <ls_tab> INDEX lv_tabix.

    LOOP AT lt_update_field ASSIGNING <ls_update_field>.
      ASSIGN COMPONENT <ls_update_field>-fieldvalue OF STRUCTURE <ls_tab> TO <lv_value>.
      IF sy-subrc EQ 0.
        CLEAR <ls_update_field>-fieldvalue.
        <ls_update_field>-fieldvalue = <lv_value>.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = iv_cprog
        dynumb               = iv_dynnr
      TABLES
        dynpfields           = lt_update_field
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
  ENDMETHOD.


  METHOD GET_SCREEN_VALUE.

    DATA: lt_dynpread TYPE dynpread_tabtype,
          ls_dynpread TYPE dynpread.

    ls_dynpread-fieldname = iv_field.
    ls_dynpread-stepl     = iv_stepl.
    APPEND ls_dynpread TO lt_dynpread.

    "화면 필드값 얻기
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname                   = iv_repid
        dynumb                   = iv_dynnr
        translate_to_upper       = iv_upper_case
        perform_input_conversion = iv_input_conv
      TABLES
        dynpfields               = lt_dynpread
      EXCEPTIONS
        invalid_abapworkarea     = 1
        invalid_dynprofield      = 2
        invalid_dynproname       = 3
        invalid_dynpronummer     = 4
        invalid_request          = 5
        no_fielddescription      = 6
        invalid_parameter        = 7
        undefind_error           = 8
        double_conversion        = 9
        stepl_not_found          = 10
        OTHERS                   = 11.

    READ TABLE lt_dynpread INTO ls_dynpread INDEX 1.
    CHECK sy-subrc EQ 0.
    rv_value = ls_dynpread-fieldvalue.


  ENDMETHOD.


  METHOD GET_SCREEN_VALUES.

    CHECK ct_dynpfields[] IS NOT INITIAL.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname                   = iv_repid
        dynumb                   = iv_dynnr
        translate_to_upper       = iv_upper_case
        perform_input_conversion = iv_input_conv
      TABLES
        dynpfields               = ct_dynpfields
      EXCEPTIONS
        invalid_abapworkarea     = 1
        invalid_dynprofield      = 2
        invalid_dynproname       = 3
        invalid_dynpronummer     = 4
        invalid_request          = 5
        no_fielddescription      = 6
        invalid_parameter        = 7
        undefind_error           = 8
        double_conversion        = 9
        stepl_not_found          = 10
        OTHERS                   = 11.

  ENDMETHOD.
ENDCLASS.
