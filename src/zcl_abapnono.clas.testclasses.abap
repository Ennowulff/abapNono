CLASS ltcl_test_board_setup DEFINITION DEFERRED.
CLASS zcl_abapnono DEFINITION LOCAL FRIENDS ltcl_test_board_setup.

CLASS ltcl_test_board_setup DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO zcl_abapnono.
    METHODS setup.
    METHODS init FOR TESTING.
    METHODS value_set_01 FOR TESTING.

ENDCLASS.


CLASS ltcl_test_board_setup IMPLEMENTATION.

  METHOD init.
    cl_abap_unit_assert=>assert_not_initial(  f_cut->board ).

  ENDMETHOD.

  METHOD value_set_01.
    f_cut->set_data(  VALUE #(
     ( line = '..........' )
     ( line = '..OOOOOO..' )
     ( line = '.OOOOOOOO.' )
     ( line = '.OOO..OOO.' )
     ( line = '.OOO..OOO.' )
     ( line = '.OOO..OOO.' )
     ( line = '.OOO..OOO.' )
     ( line = '.OOOOOOOO.' )
     ( line = '..OOOOOO..' )
     ( line = '..........' ) ) ).

    cl_abap_unit_assert=>assert_table_contains(
      table = f_cut->board
      line  = VALUE zcl_abapnono=>_line(
         idx = 1
         c01 = zcl_abapnono=>option_clr
         c02 = zcl_abapnono=>option_clr
         c03 = zcl_abapnono=>option_clr
         c04 = zcl_abapnono=>option_clr
         c05 = zcl_abapnono=>option_clr
         c06 = zcl_abapnono=>option_clr
         c07 = zcl_abapnono=>option_clr
         c08 = zcl_abapnono=>option_clr
         c09 = zcl_abapnono=>option_clr
         c10 = zcl_abapnono=>option_clr ) ).

  ENDMETHOD.

  METHOD setup.
    f_cut = NEW #(  ).
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test_helper_values DEFINITION DEFERRED.
CLASS zcl_abapnono DEFINITION LOCAL FRIENDS ltcl_test_helper_values.

CLASS ltcl_test_helper_values DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO zcl_abapnono.
    METHODS setup.
    METHODS helper_values_col_01 FOR TESTING.
    METHODS helper_values_col_02 FOR TESTING.
    METHODS helper_values_col_03 FOR TESTING.
    METHODS helper_values_col_04 FOR TESTING.
    METHODS helper_values_col_05 FOR TESTING.
    METHODS helper_values_col_06 FOR TESTING.
    METHODS helper_values_col_07 FOR TESTING.
    METHODS helper_values_col_08 FOR TESTING.
    METHODS helper_values_col_09 FOR TESTING.
    METHODS helper_values_col_10 FOR TESTING.

ENDCLASS.


CLASS ltcl_test_helper_values IMPLEMENTATION.

  METHOD setup.

    f_cut = NEW #(  ).
    f_cut->set_data(  VALUE #(
     ( line = '..........' ) "01 => test none
     ( line = '..OOOOOO..' ) "02 => test middle
     ( line = '.OOOOOOOO.' ) "03 => test middle
     ( line = '.OOO..OOO.' ) "04 => test 2 sections
     ( line = 'OOO..OOO.O' ) "05 => test 3 sections
     ( line = '.O.O..O.O.' ) "06 => test 4 sections
     ( line = 'OO.O.O.O.O' ) "07 => test 5 sections
     ( line = 'OOOOO.....' ) "08 => test beginning
     ( line = '.........O' ) "09 => test ending
     ( line = 'OOOOOOOOOO' ) "10 => test completely filled
       ) ).


    f_cut->build_helping_numbers(  ).

  ENDMETHOD.


  METHOD helper_values_col_01.

    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values( )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 1 ) ).

  ENDMETHOD.

  METHOD helper_values_col_02.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values( ( type = 'R' cell = 2 idx = 1 val = 6 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 2 ) ).

  ENDMETHOD.

  METHOD helper_values_col_03.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values( ( type = 'R' cell = 3 idx = 1 val = 8 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 3 ) ).
  ENDMETHOD.

  METHOD helper_values_col_04.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values(
        (  type = 'R' cell = 4 idx = 1 val = 3 )
        (  type = 'R' cell = 4 idx = 2 val = 3 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 4 ) ).
  ENDMETHOD.

  METHOD helper_values_col_05.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values(
                type = 'R' cell = 5
        ( idx = 1 val = 3 )
        ( idx = 2 val = 3 )
        ( idx = 3 val = 1 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 5 ) ).
  ENDMETHOD.

  METHOD helper_values_col_06.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values(
          type = 'R' cell = 6
        ( idx = 1 val = 1 )
        ( idx = 2 val = 1 )
        ( idx = 3 val = 1 )
        ( idx = 4 val = 1 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 6 ) ).
  ENDMETHOD.

  METHOD helper_values_col_07.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values(
          type = 'R' cell = 7
        ( idx = 1 val = 2 )
        ( idx = 2 val = 1 )
        ( idx = 3 val = 1 )
        ( idx = 4 val = 1 )
        ( idx = 5 val = 1 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 7 ) ).
  ENDMETHOD.

  METHOD helper_values_col_08.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values( ( type = 'R' cell = 8 idx = 1 val = 5 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 8 ) ).
  ENDMETHOD.

  METHOD helper_values_col_09.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values( ( type = 'R' cell = 9 idx = 1 val = 1 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 9 ) ).
  ENDMETHOD.

  METHOD helper_values_col_10.
    cl_abap_unit_assert=>assert_equals(
      exp = VALUE zcl_abapnono=>_helper_values( ( type = 'R' cell = 10 idx = 1 val = 10 ) )
      act = f_cut->get_helper_values_by_index(  type = 'R' index = 10 ) ).
  ENDMETHOD.

ENDCLASS.
