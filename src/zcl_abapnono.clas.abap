CLASS zcl_abapnono DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: _option TYPE c LENGTH 1.
    CONSTANTS option_set   TYPE _option VALUE 'S'.
    CONSTANTS option_clr   TYPE _option VALUE 'X'.
    CONSTANTS option_empty TYPE _option VALUE ' '.

    TYPES _helper_type TYPE c LENGTH 1.

    CONSTANTS helper_type_col TYPE _helper_type VALUE 'C'.
    CONSTANTS helper_type_row TYPE _helper_type VALUE 'R'.

    "helper type for simple data input
    TYPES: BEGIN OF _input,
             line TYPE c LENGTH 15,
           END OF _input,
           _input_table TYPE TABLE OF _input.
    "table for storing the required values
    TYPES: BEGIN OF _helper_value,
             type TYPE c LENGTH 1,
             cell TYPE i,
             idx  TYPE i,
             val  TYPE i,
           END OF _helper_value,
           _helper_values TYPE SORTED TABLE OF _helper_value WITH UNIQUE KEY type cell idx.

    "type for storing the board values
    TYPES: BEGIN OF _line,
             idx TYPE i,
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
           _board TYPE SORTED TABLE OF _line WITH UNIQUE KEY idx.
    METHODS constructor.
  PROTECTED SECTION.
    DATA board TYPE _board.
    DATA helper_values TYPE _helper_values.
    METHODS init.
    METHODS set_data IMPORTING input_data TYPE _input_table.
  PRIVATE SECTION.
    METHODS transform_input
      IMPORTING
                input_data        TYPE _input_table
      RETURNING VALUE(board_data) TYPE _board.
    METHODS build_helping_numbers.
    METHODS get_helper_values_by_index
      IMPORTING
                type          TYPE _helper_type
                index         TYPE i
      RETURNING VALUE(values) TYPE _helper_values.
ENDCLASS.



CLASS zcl_abapnono IMPLEMENTATION.

  METHOD init.
    DO 10 TIMES.
      INSERT VALUE #(  idx = sy-index ) INTO TABLE board.
    ENDDO.
  ENDMETHOD.

  METHOD constructor.
    init(  ).
  ENDMETHOD.

  METHOD set_data.

    transform_input( input_data ).

  ENDMETHOD.


  METHOD transform_input.

    LOOP AT input_data INTO DATA(line).
      ASSIGN board[ idx = sy-tabix ] TO FIELD-SYMBOL(<line>).
      DATA(offset) = 0.
      DATA(field_index) = 2.
      DO strlen( line ) TIMES.
        ASSIGN COMPONENT field_index OF STRUCTURE <line> TO FIELD-SYMBOL(<val>).
        IF sy-subrc = 0.
          <val> = SWITCH #( line+offset(1) WHEN '.' THEN option_clr ELSE option_set ).
        ENDIF.
        ADD 1 TO offset.
        ADD 1 TO field_index.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.

  METHOD build_helping_numbers.
    DATA switches  TYPE i.
    DATA last_option TYPE _option.
    DATA last_switch TYPE _option.
    DATA helper_value TYPE _helper_value.

    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      DATA(line_index) = sy-tabix.
      helper_value-type = helper_type_row.
      helper_value-cell = line_index.
      helper_value-idx  = 0.
      last_option = space.


      DO 10 TIMES.
        DATA(field) = |C{ sy-index ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
        ASSIGN COMPONENT field OF STRUCTURE <line> TO FIELD-SYMBOL(<opt>).
        IF sy-subrc = 0.

          IF <opt> = option_set.
            ADD 1 TO helper_value-val.
            last_switch = <opt>.
            last_option = <opt>.
            CONTINUE.
          ENDIF.

          IF <opt> = option_clr AND helper_value-val > 0.
            ADD 1 TO helper_value-idx.
            INSERT helper_value INTO TABLE helper_values.
            helper_value-val = 0.
            last_switch = <opt>.
            last_option = <opt>.
            CONTINUE.
          ENDIF.

          last_option = <opt>.
        ENDIF.
      ENDDO.

      IF helper_value-val > 0.
        ADD 1 TO helper_value-idx.
        INSERT helper_value INTO TABLE helper_values.
        helper_value-val = 0.
        last_switch = <opt>.
        last_option = <opt>.
        CONTINUE.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_helper_values_by_index.

    values = FILTER #( helper_values WHERE type = type AND cell = index ).

  ENDMETHOD.

ENDCLASS.
