CLASS zcl_abapnono DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES _difficulty TYPE c LENGTH 1.

    CONSTANTS difficulty_beginner TYPE _difficulty VALUE '1'.
    CONSTANTS difficulty_easy     TYPE _difficulty VALUE '2'.
    CONSTANTS difficulty_normal   TYPE _difficulty VALUE '3'.
    CONSTANTS difficulty_hard     TYPE _difficulty VALUE '4'.
    CONSTANTS difficulty_insane   TYPE _difficulty VALUE '5'.

    TYPES _option TYPE c LENGTH 1.

    CONSTANTS option_set   TYPE _option VALUE 'S'.
    CONSTANTS option_clr   TYPE _option VALUE 'X'.
    CONSTANTS option_empty TYPE _option VALUE ' '.

    TYPES _helper_type TYPE c LENGTH 1.

    CONSTANTS helper_type_col TYPE _helper_type VALUE 'C'.
    CONSTANTS helper_type_row TYPE _helper_type VALUE 'R'.

    " helper type for simple data input
    TYPES: BEGIN OF _input,
             line TYPE c LENGTH 15,
           END OF _input,
           _input_table TYPE TABLE OF _input WITH EMPTY KEY.
    " table for storing the required values
    TYPES: BEGIN OF _helper_value,
             type TYPE c LENGTH 1,
             cell TYPE i,
             idx  TYPE i,
             val  TYPE i,
           END OF _helper_value,
           _helper_values TYPE SORTED TABLE OF _helper_value WITH UNIQUE KEY type cell idx.

    " type for storing the board values
    TYPES: BEGIN OF _line,
             c01 TYPE _option,
             c02 TYPE _option,
             c03 TYPE _option,
             c04 TYPE _option,
             c05 TYPE _option,
             c06 TYPE _option,
             c07 TYPE _option,
             c08 TYPE _option,
             c09 TYPE _option,
             c10 TYPE _option,
           END OF _line,
           _board TYPE STANDARD TABLE OF _line WITH EMPTY KEY.

    METHODS constructor.
    METHODS set_data IMPORTING input_data TYPE _input_table.

    METHODS get_helper_numbers
      RETURNING
        VALUE(r_result) TYPE _helper_values.

    METHODS get_helper_values_by_index
      IMPORTING
        type          TYPE _helper_type
        index         TYPE i
      RETURNING
        VALUE(values) TYPE _helper_values.

    METHODS set_level
      IMPORTING
        i_level TYPE REF TO zcl_abapnono_level.

    METHODS is_set
      IMPORTING
                i_row           TYPE i
                i_column        TYPE clike
      RETURNING VALUE(r_result) TYPE abap_bool.
    METHODS is_empty
      IMPORTING
                i_row           TYPE i
                i_column        TYPE clike
      RETURNING VALUE(r_result) TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA board TYPE _board.
    DATA helper_values TYPE _helper_values.
    DATA level TYPE REF TO zcl_abapnono_level.

    METHODS init.

    METHODS transform_input
      IMPORTING
                input_data        TYPE _input_table
      RETURNING VALUE(board_data) TYPE _board.

    METHODS build_helper_numbers.

    METHODS get_value
      IMPORTING
                i_row           TYPE i
                i_column        TYPE clike
      RETURNING VALUE(r_result) TYPE _option.

    METHODS build_helper_numbers_row.
    METHODS build_helper_numbers_col.


ENDCLASS.


CLASS zcl_abapnono IMPLEMENTATION.
  METHOD build_helper_numbers.
    build_helper_numbers_row( ).
    build_helper_numbers_col( ).
  ENDMETHOD.

  METHOD constructor.
    init( ).
  ENDMETHOD.

  METHOD get_helper_numbers.
  ENDMETHOD.

  METHOD get_helper_values_by_index.
    values = FILTER #( helper_values WHERE type = type AND cell = index ).
  ENDMETHOD.

  METHOD get_value.
    ASSIGN board[ i_row ] TO FIELD-SYMBOL(<line>).
    ASSIGN COMPONENT i_column OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
    r_result = <value>.
  ENDMETHOD.

  METHOD init.
    DO 10 TIMES.
      INSERT VALUE #( ) INTO TABLE board.
    ENDDO.
  ENDMETHOD.

  METHOD is_empty.
    IF get_value( i_row = i_row i_column = i_column ) = option_clr.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_set.
    IF get_value( i_row = i_row i_column = i_column ) = option_set.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD set_data.
    transform_input( input_data ).
    build_helper_numbers( ).
  ENDMETHOD.

  METHOD set_level.
    level = i_level.
    set_data( level->get_board( ) ).
  ENDMETHOD.

  METHOD transform_input.
    LOOP AT input_data INTO DATA(line).
      ASSIGN board[ sy-tabix ] TO FIELD-SYMBOL(<line>).
      DATA(offset) = 0.
      DATA(field_index) = 1.
      DO strlen( line ) TIMES.
        ASSIGN COMPONENT field_index OF STRUCTURE <line> TO FIELD-SYMBOL(<val>).
        IF sy-subrc = 0.
          <val> = SWITCH #( line+offset(1) WHEN '.' THEN option_clr ELSE option_set ).
        ENDIF.
        offset = offset + 1.
        field_index = field_index + 1.
      ENDDO.

    ENDLOOP.
  ENDMETHOD.

  METHOD build_helper_numbers_row.
    DATA helper_value TYPE _helper_value.

    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      DATA(line_index) = sy-tabix.
      helper_value-type = helper_type_row.
      helper_value-cell = line_index.
      helper_value-idx  = 0.

      DO 10 TIMES.
        DATA(field) = |C{ sy-index ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
        ASSIGN COMPONENT field OF STRUCTURE <line> TO FIELD-SYMBOL(<opt>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <opt> = option_set.
          helper_value-val = helper_value-val + 1.
          CONTINUE.
        ENDIF.

        IF <opt> = option_clr AND helper_value-val > 0.
          helper_value-idx = helper_value-idx + 1.
          INSERT helper_value INTO TABLE helper_values.
          helper_value-val = 0.
          CONTINUE.
        ENDIF.

      ENDDO.

      IF helper_value-val > 0.
        helper_value-idx = helper_value-idx + 1.
        INSERT helper_value INTO TABLE helper_values.
        helper_value-val = 0.
        CONTINUE.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD build_helper_numbers_col.
    "todo
  ENDMETHOD.
ENDCLASS.
