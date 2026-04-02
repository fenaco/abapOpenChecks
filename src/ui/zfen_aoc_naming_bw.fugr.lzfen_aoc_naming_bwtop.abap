FUNCTION-POOL zfen_aoc_naming_bw.                  "MESSAGE-ID ..

* INCLUDE LZAOC_NAMINGD...                   " Local class definition

SELECTION-SCREEN BEGIN OF SCREEN 2000 TITLE TEXT-001 AS WINDOW.
  SELECTION-SCREEN:
  BEGIN OF TABBED BLOCK main_tab FOR 28 LINES,
  TAB (30) button_1 USER-COMMAND to_3000 DEFAULT SCREEN 3000,
  TAB (30) button_2 USER-COMMAND to_4000 DEFAULT SCREEN 4000,
  END OF BLOCK main_tab.
SELECTION-SCREEN END OF SCREEN 2000.

* BW Objects
SELECTION-SCREEN BEGIN OF SCREEN 3000 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK sge WITH FRAME TITLE TEXT-sge.
    PARAMETERS:
      p_cmp  TYPE text40 MODIF ID ro,
      p_cmps TYPE text40 MODIF ID ro.
  SELECTION-SCREEN END OF BLOCK sge.

  SELECTION-SCREEN BEGIN OF BLOCK app WITH FRAME TITLE TEXT-app.
    PARAMETERS:
      p_app    TYPE text40 MODIF ID ro,
      p_applsa TYPE text40 MODIF ID ro.
  SELECTION-SCREEN END OF BLOCK app.

  SELECTION-SCREEN BEGIN OF BLOCK dwh WITH FRAME TITLE TEXT-dwh.
    PARAMETERS:
      p_dom    TYPE text40 MODIF ID ro,
      p_sub    TYPE text255 MODIF ID ro,
      p_dwhlsa TYPE text40 MODIF ID ro.
  SELECTION-SCREEN END OF BLOCK dwh.

  SELECTION-SCREEN BEGIN OF BLOCK oth WITH FRAME TITLE TEXT-oth.
    PARAMETERS:
      p_sem   TYPE text40 MODIF ID ro,
      p_per   TYPE text40 MODIF ID ro,
      p_ftype TYPE text255 MODIF ID ro.
  SELECTION-SCREEN END OF BLOCK oth.
SELECTION-SCREEN END OF SCREEN 3000.

* Other Settings
SELECTION-SCREEN BEGIN OF SCREEN 4000 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK other WITH FRAME TITLE TEXT-oth.
    PARAMETERS:
      p_errty  TYPE sci_errty MODIF ID ro OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK other.
SELECTION-SCREEN END OF SCREEN 4000.

************************

CLASS lcl_screen2000 DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-DATA:
      gv_cancel TYPE abap_bool.

    CLASS-METHODS:
      initialize
        IMPORTING
          iv_read_only TYPE abap_bool
          is_data      TYPE zfen_aoc_naming_bw,
      get_data
        RETURNING
          VALUE(rs_data) TYPE zfen_aoc_naming_bw,
      at_output,
      handle_command.

  PRIVATE SECTION.
    CLASS-DATA:
      gv_read_only TYPE abap_bool.

    CLASS-METHODS:
      modify_screen,
      set_read_only
        IMPORTING
          iv_only TYPE abap_bool,
      set_data
        IMPORTING
          is_data TYPE zfen_aoc_naming_bw,
      read_structure
        RETURNING
          VALUE(rt_data) TYPE dd03ptab,
      set_texts
        IMPORTING
          iv_prefix TYPE clike.

ENDCLASS.
