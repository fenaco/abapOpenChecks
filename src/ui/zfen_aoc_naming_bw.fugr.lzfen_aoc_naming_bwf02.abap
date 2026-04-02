*----------------------------------------------------------------------*
***INCLUDE LZAOC_NAMINGF02.
*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  lcl_screen2000=>handle_command( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_screen2000=>at_output( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  lcl_screen2000=>handle_command( ).

LOAD-OF-PROGRAM.
  button_1 = 'BW Objects'(002).
  button_2 = 'Other Settings'(003).
