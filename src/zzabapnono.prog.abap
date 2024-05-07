REPORT zzabapnono.


CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS init.
    METHODS show.
    METHODS pai IMPORTING i_ucomm TYPE clike.
    METHODS pbo.
  PRIVATE SECTION.
    TYPES: BEGIN OF _row_counter,
             i1 TYPE c LENGTH 2,
             i2 TYPE c LENGTH 2,
             i3 TYPE c LENGTH 2,
             i4 TYPE c LENGTH 2,
             i5 TYPE c LENGTH 2,
           END OF _row_counter.
    TYPES: BEGIN OF _matrix_line,
             row_counter TYPE _row_counter,
             line        TYPE zcl_abapnono=>_line,
             style       TYPE lvc_t_scol,
           END OF _matrix_line.
    TYPES: _playing_matrix TYPE STANDARD TABLE OF _matrix_line WITH EMPTY KEY.
    DATA playing_matrix TYPE _playing_matrix.
    DATA container TYPE REF TO cl_gui_custom_container.
    DATA nono TYPE REF TO zcl_abapnono.
    DATA helping_numbers TYPE zcl_abapnono=>_helper_values.
    DATA salv_table TYPE REF TO cl_salv_table.
    METHODS init_playing_matrix.
    METHODS on_hotspot FOR EVENT link_click OF cl_salv_events_table IMPORTING column row.
ENDCLASS.

CLASS main IMPLEMENTATION.
  METHOD constructor.
    nono = NEW #( ).
  ENDMETHOD.

  METHOD init.
    init_playing_matrix( ).
  ENDMETHOD.


  METHOD init_playing_matrix.

    nono->set_data( VALUE #(
     ( line = '....OO....' )
     ( line = '...OOOO...' )
     ( line = '..OO..OO..' )
     ( line = '.OO....OO.' )
     ( line = 'OOOOOOOOOO' )
     ( line = 'OO.....OOO' )
     ( line = 'OOO.....OO' )
     ( line = 'OO.....OOO' )
     ( line = 'OO..OO..OO' )
     ( line = 'OOOOOOOOOO' )
       ) ).

    DATA line_idx TYPE i.
    DATA col_idx TYPE i.
    DATA helper_idx TYPE i.

    DO 5 TIMES.
      APPEND INITIAL LINE TO playing_matrix ASSIGNING FIELD-SYMBOL(<line>).
      <line>-style = VALUE #(
       ( fname = 'LINE-C01' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C02' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C03' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C04' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C05' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C06' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C07' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C08' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C09' color = VALUE #( col = 5 ) )
       ( fname = 'LINE-C10' color = VALUE #( col = 5 ) )
       ).
    ENDDO.


    DO 10 TIMES.
      line_idx = sy-index.
      APPEND INITIAL LINE TO playing_matrix ASSIGNING <line>.
      <line>-style = VALUE #(
       ( fname = 'ROW_COUNTER-I1' color = VALUE #( col = 5 ) )
       ( fname = 'ROW_COUNTER-I2' color = VALUE #( col = 5 ) )
       ( fname = 'ROW_COUNTER-I3' color = VALUE #( col = 5 ) )
       ( fname = 'ROW_COUNTER-I4' color = VALUE #( col = 5 ) )
       ( fname = 'ROW_COUNTER-I5' color = VALUE #( col = 5 ) )
       ).

      DATA(row_helper_values) = nono->get_helper_values_by_index( type = zcl_abapnono=>helper_type_row index = line_idx ).
      col_idx = 5.
      helper_idx = lines( row_helper_values ).

      DO 5 TIMES.

        TRY.
            DATA(row_helper_value) = row_helper_values[ idx = helper_idx ] .
            DATA(fieldname) = |ROW_COUNTER-I{ col_idx WIDTH = 1 ALIGN = RIGHT }|.
            ASSIGN COMPONENT fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
            <value> = row_helper_value-val.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        col_idx = col_idx - 1.
        helper_idx = helper_idx - 1.
        IF helper_idx = 0.
          EXIT. "from do
        ENDIF.
      ENDDO.
    ENDDO.


  ENDMETHOD.

  METHOD show.
    IF salv_table IS INITIAL.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container = container
            IMPORTING
              r_salv_table   = salv_table
            CHANGING
              t_table        = playing_matrix  ).

          DATA(o_columns) = salv_table->get_columns( ).
          o_columns->set_color_column( 'STYLE' ).
          DATA(columns) = o_columns->get( ).
          LOOP AT columns INTO DATA(column).
            IF column-columnname(4) = 'LINE'.
              column-r_column->set_output_length( 2 ).
              CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
            ENDIF.
            column-r_column->set_alignment( if_salv_c_alignment=>centered ).
          ENDLOOP.

          SET HANDLER on_hotspot FOR salv_table->get_event( ).

          salv_table->display( ).
        CATCH cx_salv_msg cx_salv_data_error.
      ENDTRY.
    ELSE.
      salv_table->refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_hotspot.

    DATA(selection) = salv_table->get_selections( ).
    selection->set_selected_cells( VALUE #( ) ).

  ENDMETHOD.

  METHOD pbo.
    IF container IS INITIAL.
      container = NEW #( container_name = 'CC_MATRIX' ).
    ENDIF.

    show( ).
  ENDMETHOD.

  METHOD pai.
    CASE i_ucomm.
      WHEN 'CANCEL'.
        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN 'SET_MODE_CHECKED'.
      WHEN 'SET_MODE_EMPTY'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  DATA(app) = NEW main( ).
  app->init( ).

  CALL SCREEN 100 STARTING AT 30 1.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'NONO'.
  SET TITLEBAR 'NONO'.
  CLEAR sy-ucomm.
  app->pbo( ).
ENDMODULE.

MODULE user_command_0100 INPUT.

  app->pai( sy-ucomm ).

ENDMODULE.
