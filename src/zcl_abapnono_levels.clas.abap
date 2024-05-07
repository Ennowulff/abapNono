CLASS zcl_abapnono_levels DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES _level_id TYPE i.

    CLASS-METHODS get
      IMPORTING
                i_level         TYPE i
      RETURNING VALUE(r_result) TYPE REF TO zcl_abapnono_level
      RAISING   zcx_abapnono.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_1 RETURNING VALUE(r_result) TYPE REF TO zcl_abapnono_level.
    CLASS-METHODS get_2 RETURNING VALUE(r_result) TYPE REF TO zcl_abapnono_level.
    CLASS-METHODS get_3 RETURNING VALUE(r_result) TYPE REF TO zcl_abapnono_level.
ENDCLASS.


CLASS zcl_abapnono_levels IMPLEMENTATION.
  METHOD get.
    r_result = SWITCH #( i_level
                         WHEN 1 THEN get_1( )
                         WHEN 2 THEN get_2( )
                         WHEN 3 THEN get_3( )
                         ELSE        THROW zcx_abapnono( ) ).
  ENDMETHOD.

  METHOD get_1.
    r_result = NEW #( ).
    r_result->set_description( 'House' ).
    r_result->set_difficulty( zcl_abapnono=>difficulty_easy ).
    r_result->set_board( VALUE #( ( line = '....OO....' )
                                  ( line = '...OOOO...' )
                                  ( line = '..OO..OO..' )
                                  ( line = '.OO....OO.' )
                                  ( line = 'OOOOOOOOOO' )
                                  ( line = 'OO.....OOO' )
                                  ( line = 'OOO.....OO' )
                                  ( line = 'OO.....OOO' )
                                  ( line = 'OO..OO..OO' )
                                  ( line = 'OOOOOOOOOO' ) ) ).
  ENDMETHOD.

  METHOD get_2.
    r_result = NEW #( ).
    r_result->set_description( 'Church' ).
    r_result->set_difficulty( zcl_abapnono=>difficulty_easy ).
    r_result->set_board( VALUE #( ( line = '....OO....' )
                                  ( line = '..OOOOOO..' )
                                  ( line = '....OO....' )
                                  ( line = '....OO....' )
                                  ( line = '...OOOO...' )
                                  ( line = '..OO..OO..' )
                                  ( line = '.OO....OO.' )
                                  ( line = 'OO..OO..OO' )
                                  ( line = 'OO..OO..OO' )
                                  ( line = 'OOOOOOOOOO' ) ) ).
  ENDMETHOD.

  METHOD get_3.
    r_result = NEW #( ).
    r_result->set_description( 'Bird' ).
    r_result->set_difficulty( zcl_abapnono=>difficulty_normal ).
    r_result->set_board( VALUE #( ( line = '..OOOOOO..' )
                                  ( line = '.O......O.' )
                                  ( line = 'O..O..O..O' )
                                  ( line = 'O...O....O' )
                                  ( line = 'O........O' )
                                  ( line = 'O..O..O..O' )
                                  ( line = 'O..OOOO...' )
                                  ( line = '.OOOOOOOO.' )
                                  ( line = '...OOOO.OO' )
                                  ( line = '......OOOO' ) ) ).
  ENDMETHOD.
ENDCLASS.
