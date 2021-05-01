*&---------------------------------------------------------------------*
*& Report ZRGNPUSERS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers.
* Samples: BDSFIND_1, SALV_DEMO_METADATA

include <color>.
include <icon>.

tables:
  zgnpuserscrstat.

types:
  begin of g_type_s_outtab.
    include type zgnpusers.
  types:
    uar_age(4)       type c,
    uar_status       type icon_d,
    pwd_status       type icon_d,
    pwd_age(4)       type c,
    docu             type icon_d,
    history          type icon_d,
    tlo_fullname     type text40,
    crstate_text(20) type c,
    cr_duration(4)   type c,
    t_color          type lvc_t_scol,
  end of g_type_s_outtab.

types:
  begin of g_type_s_outtab2.
types:
  timestamp               type tzntstmps,
  timestamp_formatted(19) type c,
  crstate                 type zgnpusrcrstate,
  crstate_text(20)        type c,
  crid                    type zgnpusrcrid,
  crsysid                 type zgnpusrcrsysid,
  end of g_type_s_outtab2.

class lcl_handle_events definition deferred.

data:
  gt_outtab      type standard table of g_type_s_outtab,
  gt_outtab2     type standard table of g_type_s_outtab2,
  gr_table       type ref to cl_salv_table,
  gr_table2      type ref to cl_salv_table,
  gr_events      type ref to lcl_handle_events,
  g_okcode       type syucomm,
  gc_true        type sap_bool value 'X',
  gc_max_uarage  type i value 365,
  gc_max_pwdage  type i value 365,
  gv_destination type char1024,
  lv_title       type string,
  lv_usrcount    type i.
gv_destination = 'SERVICE_NOW'.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
class lcl_handle_events definition.
  public section.
    methods:
      on_user_command for event added_function of cl_salv_events
        importing e_salv_function,
      on_link_click for event link_click of cl_salv_events_table
        importing row column.
endclass.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_handle_events implementation.
  method on_user_command.
    perform handle_user_command using e_salv_function.
  endmethod.

  method on_link_click.
    if column ='DOCU'.          " Display documentation
      perform display_account_docu using row.
    elseif column ='HISTORY'.   " Display history
      perform display_history using row.
    elseif column ='CRID'.      " Display CR in SN
      perform display_cr using row.
    endif.
  endmethod.

endclass.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block acc with frame title text-001.
parameters:
  p_usrid type zgnpusrid matchcode object zgnpusers_usruuid.
selection-screen end of block acc.
selection-screen begin of block pwd with frame title text-002.
parameters:
  p_pwdall radiobutton group dsp default 'X',
  p_pwdcp  radiobutton group dsp,
  p_pwdncp radiobutton group dsp.
selection-screen end of block pwd.
selection-screen begin of block cr with frame title text-003.
select-options: s_state for zgnpuserscrstat-state matchcode object zgnpusers_crstat.
parameters:
  p_crid type zgnpusrcrid.
selection-screen end of block cr.
selection-screen begin of block uar with frame title text-004.
parameters:
  p_uarall radiobutton group uar default 'X',
  p_uarcp  radiobutton group uar,
  p_uarncp radiobutton group uar.
selection-screen end of block uar.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.
  set pf-status 'SEL_SCREEN'.
  select count( * ) into lv_usrcount from zgnpusers.
  lv_title = lv_usrcount.
  concatenate 'Generic and Privileged (GnP) Accounts (' lv_title 'accounts)' into lv_title.
  sy-title = lv_title.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.
  perform select_data.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.
  perform display_fullscreen.

*&---------------------------------------------------------------------*
*& Form SELECT_DATA
*&---------------------------------------------------------------------*
form select_data .
  field-symbols:
    <ls_outtab> type g_type_s_outtab.
  data:
    ls_zgnpuserscrhist type zgnpuserscrhist,
    lv_date            type dats,
    lv_uar_age         type i,
    lv_pwd_age         type i,
    lv_cr_duration     type i,
    lt_color           type lvc_t_scol,
    ls_color           type lvc_s_scol,
    ls_usr03           type usr03.

  if p_crid is initial.
    if p_pwdall eq 'X'.
      select * from zgnpusers into corresponding fields of table gt_outtab
        where crstate in s_state.
    elseif p_pwdncp eq 'X'.
      lv_date = sy-datum - gc_max_pwdage.
      select * from zgnpusers into corresponding fields of table gt_outtab
        where pwdchange lt lv_date and
        crstate in s_state.
    elseif p_pwdcp eq 'X'.
      lv_date = sy-datum - gc_max_pwdage.
      select * from zgnpusers into corresponding fields of table gt_outtab
        where pwdchange ge lv_date and
        crstate in s_state.
    endif.
    if p_uarall ne 'X'.
      lv_date = sy-datum - 1.
      if p_uarcp eq 'X'.
        " delete gt_outtab where ( uar is initial ) or ( uar lt lv_date ).
        delete gt_outtab where ( uar is initial ).
      endif.
      if p_uarncp eq 'X'.
        " delete gt_outtab where uar is not initial and uar gt lv_date.
        delete gt_outtab where ( uar is not initial ).
      endif.
    endif.
  else.
    select * from zgnpusers into corresponding fields of table gt_outtab
      where crid eq p_crid.
  endif.

  describe table gt_outtab lines lv_usrcount.
  lv_title = lv_usrcount.
  concatenate 'Generic and Privileged (GnP) Accounts (' lv_title 'accounts selected)' into lv_title.
  sy-title = lv_title.

  loop at gt_outtab assigning <ls_outtab>.
    " Key columns
    clear lt_color.
    clear ls_color.
    ls_color-fname     = 'USRID'.
    ls_color-color-col = col_key.
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    append ls_color to lt_color.
    ls_color-fname     = 'EXTSID'.
    ls_color-color-col = col_key.
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    append ls_color to lt_color.
    ls_color-fname     = 'COMP'.
    ls_color-color-col = col_key.
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    append ls_color to lt_color.
    <ls_outtab>-t_color = lt_color.
    clear lt_color.
    clear ls_color.
    " UAR age
    if <ls_outtab>-uar is not initial.
      lv_uar_age = sy-datum - <ls_outtab>-uar.
      <ls_outtab>-uar_age = lv_uar_age.
    else.
      <ls_outtab>-uar_age = 999.
    endif.
    if <ls_outtab>-uar_age > gc_max_uarage.
      <ls_outtab>-uar_status = icon_status_critical.
    else.
      <ls_outtab>-uar_status = icon_okay.
    endif.
    " PWD age
    if <ls_outtab>-pwdchange is not initial.
      lv_pwd_age = sy-datum - <ls_outtab>-pwdchange.
      <ls_outtab>-pwd_age = lv_pwd_age.
    else.
      <ls_outtab>-pwd_age = 999.
    endif.
    if <ls_outtab>-pwd_age > gc_max_pwdage.
      <ls_outtab>-pwd_status = icon_status_critical.
    else.
      <ls_outtab>-pwd_status = icon_okay.
    endif.
    " TLO name
    clear <ls_outtab>-tlo_fullname.
    if <ls_outtab>-tlo is not initial.
      call function 'SUSR_SHOW_USER_DETAILS'
        exporting
          bname      = <ls_outtab>-tlo
          mandt      = sy-mandt
          no_display = 'X'
        changing
          user_usr03 = ls_usr03.
      concatenate ls_usr03-name1 ls_usr03-name2 into <ls_outtab>-tlo_fullname separated by space.
    endif.
    if <ls_outtab>-tlo_fullname eq ''.
      <ls_outtab>-tlo_fullname = <ls_outtab>-tlo.
    endif.
    if <ls_outtab>-docid is not initial.
      <ls_outtab>-docu = icon_content_object.
    endif.
    " History
    select single * from zgnpuserscrhist into ls_zgnpuserscrhist where usruuid = <ls_outtab>-usruuid.
    if sy-subrc eq 0.
      <ls_outtab>-history = icon_history.
    endif.
    " CR state text, CR duration
    if <ls_outtab>-crid is not initial.
      select single state_text from zgnpuserscrstat
        into <ls_outtab>-crstate_text
        where state eq <ls_outtab>-crstate.
      lv_cr_duration = sy-datum - <ls_outtab>-crdate + 1.
      <ls_outtab>-cr_duration = lv_cr_duration.
    endif.
  endloop.
  sort gt_outtab by pwd_age descending.
endform.

*&---------------------------------------------------------------------*
*&      Form  display_fullscreen
*&---------------------------------------------------------------------*
form display_fullscreen .
  field-symbols:
    <ls_outtab> type g_type_s_outtab.
  data:
*   lr_functions  type ref to cl_salv_functions_list,
    lr_selections          type ref to cl_salv_selections,
    lr_columns             type ref to cl_salv_columns_table,
    lr_column              type ref to cl_salv_column_table,
    lr_events              type ref to cl_salv_events_table,
    lr_functional_settings type ref to cl_salv_functional_settings,
    lr_tooltips            type ref to cl_salv_tooltips,
    ls_color               type lvc_s_colo,
    l_value                type lvc_value.

  try.
      cl_salv_table=>factory(
        importing
          r_salv_table = gr_table
        changing
          t_table      = gt_outtab ).
    catch cx_salv_msg.
  endtry.
  gr_table->set_screen_status(
        pfstatus      =  'SALV_STANDARD'
        report        =  sy-repid
        set_functions = gr_table->c_functions_all ).
  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( gc_true ).
  " Hide columns
  try.
      lr_column ?= lr_columns->get_column( 'MANDT' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'USRUUID' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'USRGRP' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'DOCID' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'TLO' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'BO' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'UAR' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'PWDCHANGE' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'CRDATE' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'CRSTATE' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'CRSYSID' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.
  endtry.
  " Set tooltips
  lr_functional_settings = gr_table->get_functional_settings( ).
  lr_tooltips = lr_functional_settings->get_tooltips( ).
  try.
      l_value = icon_content_object.
      lr_tooltips->add_tooltip(
        type    = cl_salv_tooltip=>c_type_icon
        value   = l_value
        tooltip = 'Documentation' ).
    catch cx_salv_existing.
  endtry.
  try.
      l_value = icon_history.
      lr_tooltips->add_tooltip(
        type    = cl_salv_tooltip=>c_type_icon
        value   = l_value
        tooltip = 'History' ).
    catch cx_salv_existing.
  endtry.
  " Set events
  lr_events = gr_table->get_event( ).
  create object gr_events.
  set handler gr_events->on_user_command for lr_events.
  set handler gr_events->on_link_click for lr_events.
  lr_selections = gr_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  " Column position
  gr_table->get_columns( )->set_column_position( columnname = 'USRID' position = 1 ).
  gr_table->get_columns( )->set_column_position( columnname = 'EXTSID' position = 2 ).
  gr_table->get_columns( )->set_column_position( columnname = 'COMP' position = 3 ).
  gr_table->get_columns( )->set_column_position( columnname = 'ENV' position = 4 ).
  gr_table->get_columns( )->set_column_position( columnname = 'DESCRIPTION' position = 5 ).
  gr_table->get_columns( )->set_column_position( columnname = 'HEC' position = 6 ).
  gr_table->get_columns( )->set_column_position( columnname = 'UAR_STATUS' position = 7 ).
  gr_table->get_columns( )->set_column_position( columnname = 'UAR_AGE' position = 8 ).
  gr_table->get_columns( )->set_column_position( columnname = 'PWD_STATUS' position = 9 ).
  gr_table->get_columns( )->set_column_position( columnname = 'PWD_AGE' position = 10 ).
  gr_table->get_columns( )->set_column_position( columnname = 'DOCU' position = 11 ).
  gr_table->get_columns( )->set_column_position( columnname = 'HISTORY' position = 12 ).
  gr_table->get_columns( )->set_column_position( columnname = 'TLO_FULLNAME' position = 13 ).
  gr_table->get_columns( )->set_column_position( columnname = 'CRID' position = 14 ).
  gr_table->get_columns( )->set_column_position( columnname = 'CRSTATE_TEXT' position = 15 ).
  gr_table->get_columns( )->set_column_position( columnname = 'CR_DURATION' position = 16 ).
  " Column color
  try.
      lr_columns->set_color_column( 't_color' ).
    catch cx_salv_data_error.
  endtry.
  " Column headers
  try.
      lr_column ?= lr_columns->get_column( 'USRID' ).
      lr_column->set_short_text( 'User ID' ).
      lr_column->set_medium_text( 'User ID' ).
      lr_column->set_long_text( 'User ID' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'EXTSID' ).
      lr_column->set_short_text( 'Ext SID' ).
      lr_column->set_medium_text( 'External SID' ).
      lr_column->set_long_text( 'External SID' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'COMP' ).
      lr_column->set_short_text( 'Component' ).
      lr_column->set_medium_text( 'Component' ).
      lr_column->set_long_text( 'Component' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'ENV' ).
      lr_column->set_short_text( 'Env' ).
      lr_column->set_medium_text( 'Environment' ).
      lr_column->set_long_text( 'Environment' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'DESCRIPTION' ).
      lr_column->set_short_text( 'Descript.' ).
      lr_column->set_medium_text( 'Description' ).
      lr_column->set_long_text( 'Description' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'HEC' ).
      lr_column->set_short_text( 'HEC' ).
      lr_column->set_medium_text( 'HEC managed' ).
      lr_column->set_long_text( 'HEC managed' ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'UAR_STATUS' ).
      lr_column->set_short_text( 'UAR' ).
      lr_column->set_medium_text( 'UAR status' ).
      lr_column->set_long_text( 'UAR status' ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'UAR_AGE' ).
      lr_column->set_short_text( 'age' ).
      lr_column->set_medium_text( 'UAR age' ).
      lr_column->set_long_text( 'UAR age' ).
      lr_column->set_alignment( if_salv_c_alignment=>right ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'PWD_STATUS' ).
      lr_column->set_short_text( 'PWD' ).
      lr_column->set_medium_text( 'Password status' ).
      lr_column->set_long_text( 'Password status' ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'PWD_AGE' ).
      lr_column->set_short_text( 'age' ).
      lr_column->set_medium_text( 'Password age' ).
      lr_column->set_long_text( 'Password age' ).
    catch cx_salv_not_found.
  endtry.

  try.
      lr_column ?= lr_columns->get_column( 'DOCU' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lr_column->set_short_text( 'Docu' ).
      lr_column->set_medium_text( 'Accnt documentation' ).
      lr_column->set_long_text( 'Account documentation' ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'HISTORY' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lr_column->set_short_text( 'History' ).
      lr_column->set_medium_text( 'Change history' ).
      lr_column->set_long_text( 'Change history' ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'TLO_FULLNAME' ).
      lr_column->set_short_text( 'TLO' ).
      lr_column->set_medium_text( 'TLO' ).
      lr_column->set_long_text( 'TLO' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'CRID' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lr_column->set_short_text( 'CR ID' ).
      lr_column->set_medium_text( 'CR ID' ).
      lr_column->set_long_text( 'CR ID' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'CRSTATE_TEXT' ).
      lr_column->set_short_text( 'CR state' ).
      lr_column->set_medium_text( 'CR state' ).
      lr_column->set_long_text( 'CR state' ).
    catch cx_salv_not_found.
  endtry.
  try.
      lr_column ?= lr_columns->get_column( 'CR_DURATION' ).
      lr_column->set_short_text( 'duration' ).
      lr_column->set_medium_text( 'CR duration' ).
      lr_column->set_long_text( 'CR duration' ).
      lr_column->set_alignment( if_salv_c_alignment=>right ).
    catch cx_salv_not_found.
  endtry.
  gr_table->display( ).
endform.

*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
form handle_user_command using i_ucomm type salv_de_function.
  case i_ucomm.
    when 'CRT_REQ'.
      perform get_selections.
      perform select_data.
      gr_table->refresh( refresh_mode = if_salv_c_refresh=>soft ).
    when 'READ_SN'.
      perform sn_get_change_requests.
      perform select_data.
      gr_table->refresh( refresh_mode = if_salv_c_refresh=>soft ).
    when 'INFO'.
      perform display_program_docu.
    when others.
  endcase.
endform.

*&---------------------------------------------------------------------*
*&      Form  get_selections
*&---------------------------------------------------------------------*
form get_selections .
  data:
    lr_selections type ref to cl_salv_selections,
    lt_rows       type salv_t_row,
    ls_outtab     type g_type_s_outtab,
    l_row         type i,
    lv_crid       type zgnpusrcrid,
    lv_message    type string.

  lr_selections = gr_table->get_selections( ).
  lt_rows = lr_selections->get_selected_rows( ).
  loop at lt_rows into l_row.
    read table gt_outtab index l_row into ls_outtab.
    perform sn_post_change_request using
      ls_outtab-usruuid
      ls_outtab-usrid
      ls_outtab-extsid
      ls_outtab-comp
      lv_crid.
    if lv_crid is not initial.
      concatenate 'Change Request' lv_crid 'has been created' into lv_message separated by space.
      message i000(0k) with lv_message.
    else.
      lv_message = 'Communication error'.
      message i000(0k) with lv_message.
    endif.
  endloop.
endform.
*&---------------------------------------------------------------------*
*& Form DISPLAY_PROGRAM_DOCU
*&---------------------------------------------------------------------*
form display_program_docu .
  data:
    lt_signature like bapisignat occurs 0 with header line,
    lt_component like bapicompon occurs 0 with header line.

  call function 'BDS_BUSINESSDOCUMENT_GET_URL'
    exporting
      classname       = 'GNPUSERS'
      classtype       = 'OT'
      client          = sy-mandt
      object_key      = 'ZRGNPUSERS'
      url_lifetime    = 'T'
    tables
      signature       = lt_signature
      components      = lt_component
    exceptions
      nothing_found   = 1
      parameter_error = 2
      not_allowed     = 3
      error_kpro      = 4
      internal_error  = 5
      not_authorized  = 6
      others          = 7.
  if sy-subrc = 0.
    read table lt_signature index 1.
    cl_progress_indicator=>progress_indicate( i_text = |Opening program documentation ...| i_output_immediately = abap_true ).
    call function 'BDS_DOCUMENT_DISPLAY'
      exporting
        client          = sy-mandt
        doc_id          = lt_signature-doc_id
      exceptions
        nothing_found   = 1
        parameter_error = 2
        not_allowed     = 3
        error_kpro      = 4
        internal_error  = 5
        not_authorized  = 6
        others          = 7.
    if sy-subrc ne 0.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ACCOUNT_DOCU
*&---------------------------------------------------------------------*
form display_account_docu using p_row.
  data:
    ls_outtab     type g_type_s_outtab,
    lv_object_key type swo_typeid,
    lt_signature  like bapisignat occurs 0 with header line,
    lt_component  like bapicompon occurs 0 with header line.

  read table gt_outtab index p_row into ls_outtab.
  lv_object_key = ls_outtab-docid.
  if  lv_object_key is not initial.
    call function 'BDS_BUSINESSDOCUMENT_GET_URL'
      exporting
        classname       = 'GNPUSERS'
        classtype       = 'OT'
        client          = sy-mandt
        object_key      = lv_object_key
        url_lifetime    = 'T'
      tables
        signature       = lt_signature
        components      = lt_component
      exceptions
        nothing_found   = 1
        parameter_error = 2
        not_allowed     = 3
        error_kpro      = 4
        internal_error  = 5
        not_authorized  = 6
        others          = 7.
    if sy-subrc = 0.
      read table lt_signature index 1.
      cl_progress_indicator=>progress_indicate( i_text = |Opening documentation ...| i_output_immediately = abap_true ).
      call function 'BDS_DOCUMENT_DISPLAY'
        exporting
          client          = sy-mandt
          doc_id          = lt_signature-doc_id
        exceptions
          nothing_found   = 1
          parameter_error = 2
          not_allowed     = 3
          error_kpro      = 4
          internal_error  = 5
          not_authorized  = 6
          others          = 7.
      if sy-subrc ne 0.
      endif.
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form SN_POST_CHANGE_REQUEST
*&---------------------------------------------------------------------*
form sn_post_change_request
  using
    p_usruuid type zgnpusruuid
    p_usrid type zgnpusrid
    p_extsid type zgnpusrextsid
    p_comp type zgnpusrcomp
    p_crid type zgnpusrcrid.
  types:
    begin of ty_result,
      number         type string,
      state          type string,
      sys_id         type string,
      sys_created_on type string,
    end of ty_result.

  data:
    begin of ls_response,
      result type ty_result,
    end of ls_response.

  data:
    lo_http_client     type ref to if_http_client,
    lo_rest_client     type ref to if_rest_client,
    lo_rest_entity     type ref to if_rest_entity,
    ls_zgnpusers       type zgnpusers,
    ls_zgnpuserscrhist type zgnpuserscrhist,
    ls_result          type ty_result,
    lv_uri_path        type string,
    lv_return_code     type i,
    lv_json_data       type string,
    lv_text            type string.

  clear p_crid.
  call method cl_http_client=>create_by_destination
    exporting
      destination              = gv_destination
    importing
      client                   = lo_http_client
    exceptions
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      others                   = 6.
  if sy-subrc is not initial.
*   Implement suitable error handling here
  endif.
  create object lo_rest_client type cl_rest_http_client
    exporting
      io_http_client = lo_http_client.
  lo_rest_entity = lo_rest_client->create_request_entity( ).
  lv_uri_path = '/api/now/table/change_request'.
  lo_rest_client->set_request_header( exporting
    iv_name  = if_http_header_fields_sap=>request_uri
    iv_value = lv_uri_path ).
  lo_rest_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
  cl_progress_indicator=>progress_indicate( i_text = |Calling ServiceNow API ...| i_output_immediately = abap_true ).
* lo_rest_entity->set_string_data( '{"description":"test","short_description":"test","category":"Software"}' ).
  concatenate 'Nestle Compliance - Password change for GnP accounts -'
  p_usrid '/' p_extsid '/' p_comp into lv_text separated by space.
  concatenate '{"short_description":"'
  lv_text
  '","description":"'
  'Nestle Compliance - Password change for GnP accounts'
  '\nUser ID:\t' p_usrid
  '\nExt.system ID:\t' p_extsid
  '\nComponent:\t' p_comp
  '","category":"'
  'Software' '"}'
  into lv_json_data.
  lo_rest_entity->set_string_data( lv_json_data ).
  lo_rest_client->post( lo_rest_entity ).
  lv_return_code = lo_rest_client->get_status( ).
  if lv_return_code eq 201.
    lo_rest_entity = lo_rest_client->get_response_entity( ).
    lv_json_data = lo_rest_entity->get_string_data( ).
    /ui2/cl_json=>deserialize( exporting json = lv_json_data
        changing data = ls_response ).
    p_crid = ls_response-result-number.
    select single * from zgnpusers into ls_zgnpusers  where usruuid eq p_usruuid.
    ls_zgnpusers-crid = p_crid.
    concatenate ls_response-result-sys_created_on(4) ls_response-result-sys_created_on+5(2) ls_response-result-sys_created_on+8(2)
    into ls_zgnpusers-crdate.
    ls_zgnpusers-crstate = ls_response-result-state.
    ls_zgnpusers-crsysid = ls_response-result-sys_id.
    update zgnpusers from ls_zgnpusers.
    " Update history
    ls_zgnpuserscrhist-usruuid = p_usruuid.
    get time stamp field ls_zgnpuserscrhist-timestamp.
    ls_zgnpuserscrhist-crid = p_crid.
    ls_zgnpuserscrhist-crsysid = ls_response-result-sys_id.
    ls_zgnpuserscrhist-crstate = ls_response-result-state.
    insert zgnpuserscrhist from ls_zgnpuserscrhist.
    gr_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
*   write:/ ls_response-result-number, ls_response-result-state.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form SN_GET_CHANGE_REQUESTS
*&---------------------------------------------------------------------*
*& text
form sn_get_change_requests .
  types:
    begin of ty_result,
      number         type string,
      state          type string,
      sys_id         type string,
      sys_created_on type string,
    end of ty_result.

  data:
    begin of ls_response,
      result type standard table of ty_result,
    end of ls_response.

  data:
    lo_http_client type ref to if_http_client,
    lo_rest_client type ref to if_rest_client,
    lo_rest_entity type ref to if_rest_entity,
    lt_result      type table of ty_result,
    ls_result      type ty_result,
    ls_zgnpusers   type zgnpusers,
    lv_uri_path    type string,
    lv_return_code type i,
    lv_json_data   type string,
    lv_message     type string.

  call method cl_http_client=>create_by_destination
    exporting
      destination              = gv_destination
    importing
      client                   = lo_http_client
    exceptions
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      others                   = 6.
  if sy-subrc is not initial.
    lv_message = 'ServiceNow API: Communication error'.
    message i000(0k) with lv_message.
  endif.
*  lo_http_client->authenticate( username = 'MY_SAP_USER' password = 'secret' ).
*  lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
*  lo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).
  create object lo_rest_client type cl_rest_http_client
    exporting
      io_http_client = lo_http_client.
* lv_uri_path = '/api/now/table/change_request?sysparm_limit=1'.
  lv_uri_path = '/api/now/table/change_request'.
  lo_rest_client->set_request_header( exporting
    iv_name  = if_http_header_fields_sap=>request_uri
    iv_value = lv_uri_path ).
  cl_progress_indicator=>progress_indicate( i_text = |Calling ServiceNow API ...| i_output_immediately = abap_true ).
  lo_rest_client->get( ).
  lv_return_code = lo_rest_client->get_status( ).
  if lv_return_code eq 200.
    cl_progress_indicator=>progress_indicate( i_text = |Processing ServiceNow data ...| i_output_immediately = abap_true ).
    lo_rest_entity = lo_rest_client->get_response_entity( ).
    lv_json_data = lo_rest_entity->get_string_data( ).
    /ui2/cl_json=>deserialize( exporting json = lv_json_data
      changing data = ls_response  ).
    loop at ls_response-result into ls_result.
      insert ls_result into table lt_result.
    endloop.
    select * from zgnpusers into ls_zgnpusers.
      loop at lt_result into ls_result.
        if ls_result-number = ls_zgnpusers-crid.
          ls_zgnpusers-crstate = ls_result-state.
          concatenate ls_result-sys_created_on(4) ls_result-sys_created_on+5(2) ls_result-sys_created_on+8(2)
          into ls_zgnpusers-crdate.
          update zgnpusers from ls_zgnpusers.
          exit.
        endif.
      endloop.
    endselect.
  else.
    lv_message = 'ServiceNow API: Communication error'.
    message i000(0k) with lv_message.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form DISPLAY_CR
*&---------------------------------------------------------------------*
form display_cr using p_row.
  data:
    ls_outtab    type g_type_s_outtab,
    lv_crsysid   type zgnpusrcrsysid,
    lv_url(1000).

  read table gt_outtab index p_row into ls_outtab.
  lv_crsysid = ls_outtab-crsysid.
  if lv_crsysid is not initial.
    concatenate 'https://dev91014.service-now.com/nav_to.do?uri=change_request.do?sys_id=' lv_crsysid into lv_url.
    call function 'CALL_BROWSER'
      exporting
        url         = lv_url
        window_name = 'ServiceNow'
        new_window  = 'X'
*       BROWSER_TYPE                 =
*       CONTEXTSTRING                =
* EXCEPTIONS
*       FRONTEND_NOT_SUPPORTED       = 1
*       FRONTEND_ERROR               = 2
*       PROG_NOT_FOUND               = 3
*       NO_BATCH    = 4
*       UNSPECIFIED_ERROR            = 5
*       OTHERS      = 6
      .
    if sy-subrc ne 0.
* Implement suitable error handling here
    endif.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form DISPLAY_HISTORY
*&---------------------------------------------------------------------*
form display_history using p_row.
  field-symbols:
    <ls_outtab> type g_type_s_outtab2.
  data:
    ls_outtab    type g_type_s_outtab,
    lr_columns   type ref to cl_salv_columns_table,
    lr_column    type ref to cl_salv_column_table,
    lr_display   type ref to cl_salv_display_settings,
    lv_usruuid   type zgnpusruuid,
    lv_timestamp type string.

  read table gt_outtab index p_row into ls_outtab.
  lv_usruuid = ls_outtab-usruuid.
  if lv_usruuid is not initial.
    select * from zgnpuserscrhist into corresponding fields of table gt_outtab2.
    loop at gt_outtab2 assigning <ls_outtab>.
      "Timestamp
      call function 'RRBA_CONVERT_TIMESTAMP_TO_STR'
        exporting
          i_timestamp = <ls_outtab>-timestamp
        importing
          e_output    = lv_timestamp.
      <ls_outtab>-timestamp_formatted = lv_timestamp.
      " State
      select single state_text from zgnpuserscrstat
        into <ls_outtab>-crstate_text
        where state eq <ls_outtab>-crstate.
    endloop.
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = gr_table2
          changing
            t_table      = gt_outtab2 ).
      catch cx_salv_msg.
    endtry.
    lr_columns = gr_table2->get_columns( ).
    lr_columns->set_optimize( gc_true ).
    " Hide columns
    try.
        lr_column ?= lr_columns->get_column( 'TIMESTAMP' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      catch cx_salv_not_found.
    endtry.
    try.
        lr_column ?= lr_columns->get_column( 'CRSTATE' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      catch cx_salv_not_found.
    endtry.
    try.
        lr_column ?= lr_columns->get_column( 'CRSYSID' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      catch cx_salv_not_found.
    endtry.
    " Column position
    gr_table2->get_columns( )->set_column_position( columnname = 'TIMESTAMP_FORMATTED' position = 1 ).
    gr_table2->get_columns( )->set_column_position( columnname = 'CRID' position = 2 ).
    gr_table2->get_columns( )->set_column_position( columnname = 'CRSTATE_TEXT' position = 3 ).
    " Column headers
    try.
        lr_column ?= lr_columns->get_column( 'TIMESTAMP_FORMATTED' ).
        lr_column->set_short_text( 'Timestamp' ).
        lr_column->set_medium_text( 'Timestamp' ).
        lr_column->set_long_text( 'Timestamp' ).
      catch cx_salv_not_found.
    endtry.
    try.
        lr_column ?= lr_columns->get_column( 'CRSTATE_TEXT' ).
        lr_column->set_short_text( 'State' ).
        lr_column->set_medium_text( 'State' ).
        lr_column->set_long_text( 'State' ).
      catch cx_salv_not_found.
    endtry.
    try.
        lr_column ?= lr_columns->get_column( 'CRID' ).
        lr_column->set_short_text( 'CR ID' ).
        lr_column->set_medium_text( 'CR ID' ).
        lr_column->set_long_text( 'CR ID' ).
      catch cx_salv_not_found.
    endtry.
    " Popup
    gr_table2->set_screen_popup(
    start_column = 5
    end_column   = 47
    start_line   = 3
    end_line     = 20 ).
    " Set title
    lr_display = gr_table2->get_display_settings( ).
    lr_display->set_list_header( 'Change History' ).
    " Display table
    gr_table2->display( ).
  endif.
endform.
