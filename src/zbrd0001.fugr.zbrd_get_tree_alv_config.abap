FUNCTION zbrd_get_tree_alv_config.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PROGID) TYPE  CPROG
*"     VALUE(IV_ALV_KEY) TYPE  CHAR10
*"     VALUE(IV_FCAT_KEY) TYPE  CHAR10
*"  TABLES
*"      T_DATA STRUCTURE  ZTBRD00021
*"----------------------------------------------------------------------
**************************************************************************
* Modules / Sub Module : SG / Z
*&---------------------------------------------------------------------
* WRITER             : BRAD.RYU
* DATE               : 2020.07.30
* TYPE               : RFC
* Description        : Get ALV Configuration From Other System
*                      DEV의 BALV Config를 QA, PRD에서 copy하게 한다.
*************************************************************************
*              Changed History
*&---------------------------------------------------------------------**
*Changed Number     Changed Date   Writer    Changed      Description
*&---------------------------------------------------------------------**
*      N            2020.07.30    BRAD.RYU   CREATE       CREATE       *
*************************************************************************


  SELECT * INTO CORRESPONDING FIELDS OF TABLE t_data
    FROM ztbrd00021
    WHERE progid = iv_progid
    AND alv_key = iv_alv_key
    AND fcat_key = iv_fcat_key.



ENDFUNCTION.
