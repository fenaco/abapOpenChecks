"! <p class="shorttext synchronized" lang="en">fenaco BW: Prefix Naming Conventions</p>
CLASS zcl_fen_aoc_check_bw DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_regex,
             pos   TYPE i,
             len   TYPE i,
             regex TYPE string,
             subrc TYPE sy-subrc,
           END OF ty_regex.

    TYPES: tt_regex TYPE TABLE OF ty_regex.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.

    METHODS check_bw_object .
    METHODS compare
      IMPORTING
                !iv_name        TYPE string
                !iv_regex       TYPE string
                !iv_relative    TYPE i
                !iv_inform      TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_subrc) TYPE sy-subrc.
    METHODS compare_multi
      IMPORTING
                !iv_inform      TYPE abap_bool DEFAULT abap_true
      CHANGING
                !ct_regex       TYPE tt_regex
      RETURNING VALUE(rv_subrc) TYPE sy-subrc.
    METHODS set_defaults .
    "! Fügt die BW/4HANA Objekttypen zur Liste mit den zu verarbeitenden Objekttypen hinzu
    METHODS get_obj_types.
    METHODS check_area.
    "! Ermittelt die oberste InfoArea in der Hierarchiestufe
    METHODS get_top_area
      IMPORTING
                !iv_area           TYPE rsinfoarea
      RETURNING VALUE(rv_top_area) TYPE rsinfoarea.
    METHODS check_area_aap.
    METHODS check_area_dwh.
    METHODS check_area_ods.
    METHODS check_adso.
    METHODS check_adso_aap.
    METHODS check_adso_dwh.
    METHODS check_fbpa.
    METHODS check_iobj.
    METHODS check_hcpr.
    METHODS check_ctrt.
    METHODS check_uomt.
    METHODS check_alvl.
    METHODS check_plse.
    METHODS check_plst.
    METHODS check_plsq.
    METHODS check_rsds.
    METHODS check_rspc.
    METHODS check_rspc_aap.
    METHODS check_rspc_dwh.
    METHODS check_rspc_ods.
    "! Gibt die Subdomäne zur Domäne zurück
    METHODS get_subdomain
      IMPORTING
        iv_dom_pos    TYPE i
      RETURNING
        VALUE(rv_sub) TYPE string.
  PRIVATE SECTION.

    DATA mo_scan TYPE REF TO zcl_aoc_scan .
    DATA ms_naming TYPE zfen_aoc_naming_bw .
    DATA mo_compiler TYPE REF TO cl_abap_compiler .
    DATA mo_stack TYPE REF TO lcl_stack .
    DATA mv_begin TYPE i .
    DATA mv_at TYPE string .
    DATA mv_position TYPE i .
    DATA mt_obj_types TYPE TABLE OF rsobjs_obj_types.
    DATA mt_area TYPE TABLE OF rsdarea.
    DATA mt_adso TYPE TABLE OF rsoadso.
    DATA mt_iobj TYPE TABLE OF rsdiobj.
    DATA mt_hcpr TYPE TABLE OF rsohcpr.
    DATA mt_regex TYPE tt_regex.
    DATA mt_plse TYPE TABLE OF rsplf_srv.
    DATA mt_sub TYPE TABLE OF string.
ENDCLASS.



CLASS zcl_fen_aoc_check_bw IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    me->check_bw_object( ).

  ENDMETHOD.


  METHOD check_bw_object.

    CASE object_type.
      WHEN 'AREA'.
        me->check_area( ).
      WHEN 'ADSO'.
        me->check_adso( ).
      WHEN 'FBPA'.
        me->check_fbpa( ).
      WHEN 'IOBJ'.
        me->check_iobj( ).
      WHEN 'HCPR'.
        me->check_hcpr( ).
      WHEN 'CTRT'.
        me->check_ctrt( ).
      WHEN 'UOMT'.
        me->check_uomt( ).
      WHEN 'ALVL'.
        me->check_alvl( ).
      WHEN 'PLSE'.
        me->check_plse( ).
      WHEN 'PLST'.
        me->check_plst( ).
      WHEN 'PLSQ'.
        me->check_plsq( ).
      WHEN 'RSDS'.
        me->check_rsds( ).
      WHEN 'RSPC'.
        me->check_rspc( ).
    ENDCASE.

  ENDMETHOD.


  METHOD compare.

    DATA: lv_regex   TYPE string,
          lv_include TYPE sobj_name.


    lv_regex = |^{ iv_regex }|.

    FIND REGEX lv_regex IN iv_name IGNORING CASE.
    rv_subrc = sy-subrc.

    IF sy-subrc <> 0 AND iv_inform = abap_true.
      lv_include = object_name.
      inform( p_sub_obj_name = lv_include
              p_line         = get_line_rel( iv_relative )
              p_column       = get_column_rel( iv_relative )
              p_position     = mv_position
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = iv_regex
              p_param_2      = iv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version     = '002'.
    position    = '900'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    set_defaults( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Bad naming, expected &1, got &2'(m01)
        iv_pcom = '"#EC CI_NAMING' ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Unable to resolve &1'(m05) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Error qualifying tokens'(m02) ).

    insert_scimessage(
        iv_code = '004'
        iv_text = 'Unable to resolve &1'(m05) ).

    insert_scimessage(
        iv_code = '005'
        iv_text = 'Syntax error'(m03) ).

    insert_scimessage(
        iv_code = '006'
        iv_text = 'Error reading FM parameters'(m04) ).

    me->get_obj_types( ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    ms_naming-set_errty = mv_errty.

    CALL FUNCTION 'ZFEN_AOC_NAMING_BW'
      EXPORTING
        iv_read_only = p_display
      CHANGING
        cs_data      = ms_naming.

    mv_errty = ms_naming-set_errty.
    attributes_ok = abap_true.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD set_defaults.

  ENDMETHOD.


  METHOD run.

    check( NEW zcl_aoc_scan( it_tokens = VALUE stokesx_tab( )
                                      it_statements = VALUE sstmnt_tab( )
                                      it_levels = VALUE slevel_tab( )
                                      it_structures = VALUE ty_structures_tt( ) ) ).

  ENDMETHOD.


  METHOD get_obj_types.

    SELECT
      FROM rsobjs_obj_types
      FIELDS obj_type
      INTO TABLE @DATA(lt_obj_types).

    LOOP AT lt_obj_types ASSIGNING FIELD-SYMBOL(<ls_obj_types>).
      me->add_obj_type( p_obj_type = CONV #( <ls_obj_types> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check_area.

    IF mt_area IS INITIAL.
      SELECT
        FROM rsdarea
        FIELDS *
        WHERE objvers = 'A'
        INTO TABLE @mt_area.
    ENDIF.

    DATA(lv_top_area) = me->get_top_area( iv_area = CONV #( object_name ) ).

    CASE lv_top_area.
      WHEN 'AAP'.
        me->check_area_aap( ).
      WHEN 'DWH'.
        me->check_area_dwh( ).
      WHEN 'ODS'.
        me->check_area_ods( ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_top_area.

    TRY.
        DATA(ls_area) = mt_area[ infoarea = iv_area ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF ls_area-infoarea_p IS INITIAL.
      rv_top_area = ls_area-infoarea.
    ELSE.
      rv_top_area = me->get_top_area( iv_area = ls_area-infoarea_p ).
    ENDIF.

  ENDMETHOD.


  METHOD check_area_aap.

    DATA(lv_strlen) = strlen( object_name ).

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'AAP' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 2 regex = ms_naming-bw_app )
        ( pos = 9 len = 1 regex = '_' )
        ( pos = 10 len = COND #( WHEN lv_strlen >= 10 THEN lv_strlen - 10 ELSE 0 ) regex = ms_naming-bw_applsa ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD check_area_dwh.

    DATA(lv_sub) = me->get_subdomain( 8 ).
    DATA(lv_strlen) = strlen( object_name ).

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'DWH' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = ms_naming-bw_dom )
        ( pos = 8 len = 2 regex = lv_sub )
        ( pos = 10 len = 1 regex = '_' )
        ( pos = 11 len = COND #( WHEN lv_strlen >= 10 THEN lv_strlen - 10 ELSE 0 ) regex = ms_naming-bw_dwhlsa ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD check_area_ods.

    DATA(lv_sub) = me->get_subdomain( 8 ).

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'ODS' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = ms_naming-bw_dom )
        ( pos = 8 len = 2 regex = lv_sub ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD check_adso.

    IF mt_adso IS INITIAL.
      SELECT
        FROM rsoadso "#EC CI_GENBUFF
        FIELDS *
        INTO TABLE @mt_adso.
    ENDIF.

    DATA(lv_area) = mt_adso[ adsonm = object_name ]-infoarea.
    IF sy-subrc = 0.
      DATA(lv_top_area) = me->get_top_area( iv_area = lv_area ).
    ENDIF.

    CASE lv_top_area.
      WHEN 'AAP'.
        me->check_adso_aap( ).
      WHEN 'DWH'.
        me->check_adso_dwh( ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_adso_aap.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 3 len = 2 regex = ms_naming-bw_app )
        ( pos = 5 len = 1 regex = ms_naming-bw_applsa ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD check_adso_dwh.

    DATA(lv_sub) = me->get_subdomain( 4 ).

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 3 len = 1 regex = ms_naming-bw_dom )
        ( pos = 4 len = 2 regex = lv_sub )
        ( pos = 6 len = 1 regex = ms_naming-bw_dwhlsa ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD check_fbpa.

    DATA(lv_regex) = |[:cmp:][:dom:][:sub:][:dwh_lsa:]|.

    REPLACE FIRST OCCURRENCE OF '[:cmp:]' IN lv_regex WITH ms_naming-bw_cmp.
    REPLACE FIRST OCCURRENCE OF '[:dom:]' IN lv_regex WITH ms_naming-bw_dom.
    REPLACE FIRST OCCURRENCE OF '[:sub:]' IN lv_regex WITH ms_naming-bw_sub.
    REPLACE FIRST OCCURRENCE OF '[:dwh_lsa:]' IN lv_regex WITH ms_naming-bw_applsa.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_iobj.

    DATA(lv_regex) = |F|.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_hcpr.

    DATA(lv_regex) = |[:cmp:][:app:]V|.

    REPLACE FIRST OCCURRENCE OF '[:cmp:]' IN lv_regex WITH ms_naming-bw_cmp.
    REPLACE FIRST OCCURRENCE OF '[:app:]' IN lv_regex WITH ms_naming-bw_app.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_ctrt.

    DATA(lv_regex) = |FCT_|.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_uomt.

    DATA(lv_regex) = |FUT_|.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_alvl.

    DATA(lv_regex) = |[:cmp:][:app:]L|.

    REPLACE FIRST OCCURRENCE OF '[:cmp:]' IN lv_regex WITH ms_naming-bw_cmp.
    REPLACE FIRST OCCURRENCE OF '[:app:]' IN lv_regex WITH ms_naming-bw_app.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_plse.

    IF mt_plse IS INITIAL.
      SELECT
        FROM rsplf_srv "#EC CI_GENBUFF
        FIELDS *
        INTO TABLE @mt_plse.
    ENDIF.

    DATA(lv_alvl) = mt_plse[ srvnm = CONV #( object_name ) ]-infoprov.

    DATA(lv_regex) = |{ lv_alvl }_[:func_type:]_[0-9]\{3\}$|.

    REPLACE FIRST OCCURRENCE OF '[:func_type:]' IN lv_regex WITH ms_naming-bw_cmp.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_plst.

    DATA(lv_regex) = |[:cmp:][:app:]_FT|.

    REPLACE FIRST OCCURRENCE OF '[:cmp:]' IN lv_regex WITH ms_naming-bw_cmp.
    REPLACE FIRST OCCURRENCE OF '[:app:]' IN lv_regex WITH ms_naming-bw_app.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_plsq.

    DATA(lv_regex) = |[:cmp:][:app:]_SQ[0-9]\{3\}$|.

    REPLACE FIRST OCCURRENCE OF '[:cmp:]' IN lv_regex WITH ms_naming-bw_cmp.
    REPLACE FIRST OCCURRENCE OF '[:app:]' IN lv_regex WITH ms_naming-bw_app.

    me->compare(
      iv_name     = CONV #( object_name )
      iv_regex    = lv_regex
      iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_rsds.

    DATA(lv_regex) = |Z[:dom:]_[:sem:]_|.

    REPLACE FIRST OCCURRENCE OF '[:cmp:]' IN lv_regex WITH ms_naming-bw_cmp.
    REPLACE FIRST OCCURRENCE OF '[:app:]' IN lv_regex WITH ms_naming-bw_app.

    SPLIT object_name AT ' ' INTO TABLE DATA(lt_split).
    IF sy-subrc = 0.
      DATA(lv_object_name) = lt_split[ 1 ].

      me->compare(
        iv_name     = lv_object_name
        iv_regex    = lv_regex
        iv_relative = 2 ).
    ENDIF.

  ENDMETHOD.


  METHOD check_rspc.

    CASE object_name(3).
      WHEN 'AAP'.
        me->check_rspc_aap( ).
      WHEN 'DWH'.
        me->check_rspc_dwh( ).
      WHEN 'ODS'.
        me->check_rspc_ods( ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_rspc_aap.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'AAP' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_per ) ).

    DATA(lv_subrc) = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'AAP' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = '_' )
        ( pos = 8 len = 3 regex = ms_naming-bw_per ) ).

    lv_subrc = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'AAP' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 2 regex = ms_naming-bw_app )
        ( pos = 9 len = 1 regex = '_' )
        ( pos = 10 len = 3 regex = ms_naming-bw_per ) ).

    lv_subrc = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'AAP' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 2 regex = ms_naming-bw_app )
        ( pos = 9 len = 1 regex = '_' )
        ( pos = 10 len = 1 regex = ms_naming-bw_applsa )
        ( pos = 11 len = 1 regex = '_' )
        ( pos = 12 len = 3 regex = '{3}$' ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD check_rspc_dwh.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'DWH' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_per ) ).

    DATA(lv_subrc) = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'DWH' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = '_' )
        ( pos = 8 len = 3 regex = ms_naming-bw_per ) ).

    lv_subrc = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    DATA(lv_sub) = me->get_subdomain( 7 ).

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'DWH' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = ms_naming-bw_dom )
        ( pos = 8 len = 1 regex = lv_sub )
        ( pos = 9 len = 1 regex = '_' )
        ( pos = 10 len = 3 regex = ms_naming-bw_per ) ).

    lv_subrc = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'DWH' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = ms_naming-bw_dom )
        ( pos = 8 len = 1 regex = lv_sub )
        ( pos = 9 len = 1 regex = '_' )
        ( pos = 10 len = 1 regex = ms_naming-bw_dwhlsa )
        ( pos = 11 len = 1 regex = '_' )
        ( pos = 12 len = 3 regex = '{3}$' ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD check_rspc_ods.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'ODS' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_per ) ).

    DATA(lv_subrc) = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'ODS' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = '_' )
        ( pos = 8 len = 3 regex = ms_naming-bw_per ) ).

    lv_subrc = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    DATA(lv_sub) = me->get_subdomain( 7 ).

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'ODS' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = ms_naming-bw_dom )
        ( pos = 8 len = 1 regex = lv_sub )
        ( pos = 9 len = 1 regex = '_' )
        ( pos = 10 len = 3 regex = ms_naming-bw_per ) ).

    lv_subrc = me->compare_multi(
      EXPORTING
        iv_inform = abap_false
      CHANGING
        ct_regex  = mt_regex ).

    IF lv_subrc = 0.
      RETURN.
    ENDIF.

    mt_regex = VALUE #(
        ( pos = 0 len = 3 regex = 'ODS' )
        ( pos = 3 len = 1 regex = '_' )
        ( pos = 4 len = 3 regex = ms_naming-bw_cmp )
        ( pos = 7 len = 1 regex = ms_naming-bw_dom )
        ( pos = 8 len = 1 regex = lv_sub )
        ( pos = 9 len = 1 regex = '_' )
        ( pos = 10 len = 1 regex = 'O' )
        ( pos = 11 len = 1 regex = '_' )
        ( pos = 12 len = 3 regex = '{3}$' ) ).

    me->compare_multi(
      CHANGING
        ct_regex  = mt_regex ).

  ENDMETHOD.


  METHOD compare_multi.

    DATA(lv_strlen) = strlen( object_name ).

    LOOP AT mt_regex ASSIGNING FIELD-SYMBOL(<ls_regex>) WHERE pos < lv_strlen.
      DATA(lv_subrc) = me->compare(
        iv_name     = CONV #( object_name+<ls_regex>-pos(<ls_regex>-len) )
        iv_regex    = <ls_regex>-regex
        iv_relative = 2
        iv_inform = iv_inform ).

      <ls_regex>-subrc = lv_subrc.

      IF lv_subrc <> 0.
        EXIT.
      ENDIF.
    ENDLOOP.

    rv_subrc = lv_subrc.

  ENDMETHOD.

  METHOD get_subdomain.

    DATA(lv_dom) = object_name+iv_dom_pos(1).

    IF mt_sub IS INITIAL.
      SPLIT ms_naming-bw_sub AT ';' INTO TABLE mt_sub.
    ENDIF.

    LOOP AT mt_sub ASSIGNING FIELD-SYMBOL(<lv_sub>).
      IF <lv_sub>(1) = lv_dom.
        rv_sub = shift_left( val = <lv_sub> places = 1 ).
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
