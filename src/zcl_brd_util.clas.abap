class ZCL_BRD_UTIL definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_module,
        code(3),
        text(20),
      END OF ts_module .
  types:
    tt_module TYPE TABLE OF ts_module .

  class-methods GET_DOMAIN_VALUE
    importing
      value(IV_DOMNAME) type DOMNAME
      value(IV_DDLAGUAGE) type DDLANGUAGE default SY-LANGU
      value(IV_DOMVALUE_L) type DOMVALUE_L
    returning
      value(RV_VALUE) type VAL_TEXT .
  class-methods GET_MODULE_MASTER
    exporting
      value(ET_DATA) type TT_MODULE .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_BRD_UTIL IMPLEMENTATION.


  METHOD get_domain_value.
  ENDMETHOD.


  METHOD get_module_master.

    SELECT domvalue_l  AS code ddtext AS text
    INTO CORRESPONDING FIELDS OF TABLE et_data
    FROM dd07v
    WHERE domname = 'ZDSG_MODULE'.

  ENDMETHOD.
ENDCLASS.
