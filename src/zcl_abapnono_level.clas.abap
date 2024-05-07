CLASS zcl_abapnono_level DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_description
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS get_difficulty
      RETURNING
        VALUE(r_result) TYPE zcl_abapnono=>_difficulty.

    METHODS get_board
      RETURNING VALUE(r_result) TYPE zcl_abapnono=>_input_table.

    METHODS set_description
      IMPORTING
        i_value TYPE string.

    METHODS set_difficulty
      IMPORTING
        i_value TYPE zcl_abapnono=>_difficulty.

    METHODS set_board
      IMPORTING
        i_value TYPE zcl_abapnono=>_input_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA board TYPE zcl_abapnono=>_board.
    DATA description TYPE string.
    DATA difficulty TYPE zcl_abapnono=>_difficulty.
ENDCLASS.


CLASS zcl_abapnono_level IMPLEMENTATION.
  METHOD get_description.
    r_result = description.
  ENDMETHOD.

  METHOD get_difficulty.
    r_result = difficulty.
  ENDMETHOD.

  METHOD get_board.
    r_result = board.
  ENDMETHOD.

  METHOD set_board.
    board = i_value.
  ENDMETHOD.

  METHOD set_description.
    description = i_value.
  ENDMETHOD.

  METHOD set_difficulty.
    difficulty = i_value.
  ENDMETHOD.
ENDCLASS.
