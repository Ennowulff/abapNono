REPORT zzabapnono.


PARAMETERS p_level TYPE zcl_abapnono_levels=>_level_id DEFAULT 1.
PARAMETERS p_descr TYPE string MODIF ID dsp.

CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_level TYPE zcl_abapnono_levels=>_level_id.
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
    TYPES: BEGIN OF _matrix_line.
             INCLUDE TYPE _row_counter.
             INCLUDE TYPE zcl_abapnono=>_line.
             TYPES: style TYPE lvc_t_scol,
           END OF _matrix_line.
    TYPES: _playing_matrix TYPE STANDARD TABLE OF _matrix_line WITH EMPTY KEY.
    DATA level TYPE zcl_abapnono_levels=>_level_id.
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
    TRY.
        nono->set_level( zcl_abapnono_levels=>get( i_level ) ).
      CATCH zcx_abapnono.
    ENDTRY.

  ENDMETHOD.

  METHOD init.
    init_playing_matrix( ).
  ENDMETHOD.


  METHOD init_playing_matrix.

    DATA line_idx TYPE i.
    DATA col_idx TYPE i.
    DATA helper_idx TYPE i.

    DO 5 TIMES.
      APPEND INITIAL LINE TO playing_matrix ASSIGNING FIELD-SYMBOL(<line>).
      <line>-style = VALUE #(
       ( fname = 'C01' color = VALUE #( col = 5 ) )
       ( fname = 'C02' color = VALUE #( col = 5 ) )
       ( fname = 'C03' color = VALUE #( col = 5 ) )
       ( fname = 'C04' color = VALUE #( col = 5 ) )
       ( fname = 'C05' color = VALUE #( col = 5 ) )
       ( fname = 'C06' color = VALUE #( col = 5 ) )
       ( fname = 'C07' color = VALUE #( col = 5 ) )
       ( fname = 'C08' color = VALUE #( col = 5 ) )
       ( fname = 'C09' color = VALUE #( col = 5 ) )
       ( fname = 'C10' color = VALUE #( col = 5 ) )
       ).
    ENDDO.


    DO 10 TIMES.
      line_idx = sy-index.
      APPEND INITIAL LINE TO playing_matrix ASSIGNING <line>.
      <line>-style = VALUE #(
       ( fname = 'I1' color = VALUE #( col = 5 ) )
       ( fname = 'I2' color = VALUE #( col = 5 ) )
       ( fname = 'I3' color = VALUE #( col = 5 ) )
       ( fname = 'I4' color = VALUE #( col = 5 ) )
       ( fname = 'I5' color = VALUE #( col = 5 ) )
       ).

      DATA(row_helper_values) = nono->get_helper_values_by_index( type = zcl_abapnono=>helper_type_row index = line_idx ).
      col_idx = 5.
      helper_idx = lines( row_helper_values ).

      DO 5 TIMES.

        TRY.
            DATA(row_helper_value) = row_helper_values[ idx = helper_idx ] .
            DATA(fieldname) = |I{ col_idx WIDTH = 1 ALIGN = RIGHT }|.
            ASSIGN COMPONENT fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
            IF sy-subrc = 0.
              <value> = row_helper_value-val.
            ENDIF.
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
            IF column-columnname(1) = 'C'.
              column-r_column->set_output_length( 2 ).
              CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
              CAST cl_salv_column_table( column-r_column )->set_icon( abap_true ).

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

    FIELD-SYMBOLS <style> TYPE lvc_t_scol.

    DATA(selection) = salv_table->get_selections( ).
    selection->set_selected_cells( VALUE #( ) ).

    DATA(new_row) = row - 5.

    ASSIGN playing_matrix[ row ] TO FIELD-SYMBOL(<line>).
    ASSIGN COMPONENT column OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).

    ASSIGN COMPONENT 'STYLE' OF STRUCTURE <line> TO <style>.
    IF nono->is_set( i_row = new_row i_column = column ).
      TRY.
          <style>[ fname = column ]-color = VALUE #( col = 7 ).
        CATCH cx_sy_itab_line_not_found.
          INSERT VALUE #( fname = column color = VALUE #( col = 7 ) ) INTO TABLE <style>.
      ENDTRY.
    ENDIF.
    salv_table->refresh( ).

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

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'DSP'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  TRY.
      DATA(level) = zcl_abapnono_levels=>get( p_level ).
      p_descr = level->get_description( ).
    CATCH zcx_abapnono.
      MESSAGE 'Level does not exist' TYPE 'E'.
  ENDTRY.

START-OF-SELECTION.
  DATA(app) = NEW main( p_level ).
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
