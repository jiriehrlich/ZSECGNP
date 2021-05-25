*&---------------------------------------------------------------------*
*& Report ZRGNPUSERS_CONF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers_conf.
data: begin of gs_global_conf,
        destination   type rfcdest,
        ar_age        type numc3,
        ar_threshold  type numc2,
        pwd_age       type numc3,
        pwd_threshold type numc2,
      end of gs_global_conf.
data:
  gs_conf_table type zgnpusersconf,
  gv_xml_string type string.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block global with frame title text-001.
parameters:
  p_dest  type rfcdest default '1',
  p_arage type numc3 default '2',
  p_arthd type numc2 default '3',
  p_pwage type numc3 default '4',
  p_pwthd type numc2 default '5'.
selection-screen end of block global.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.
  perform read_configuration.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.
  perform save_configuration.
*&---------------------------------------------------------------------*
*& Form READ_CONFIGURATION
*&---------------------------------------------------------------------*
form read_configuration.
  " Global
  select single * from zgnpusersconf into corresponding fields of gs_conf_table
    where type eq 'GLOBAL'.
  if sy-subrc eq 0.
    gv_xml_string = gs_conf_table-data_str.
    call transformation id source xml gv_xml_string
    result struct = gs_global_conf.
    p_dest  = gs_global_conf-destination.
    p_arage = gs_global_conf-ar_age.
    p_arthd = gs_global_conf-ar_threshold.
    p_pwage = gs_global_conf-pwd_age.
    p_pwthd = gs_global_conf-pwd_threshold.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form SAVE_CONFIGURATION
*&---------------------------------------------------------------------*
form save_configuration.
  " Global
  gs_global_conf-destination = p_dest.
  gs_global_conf-ar_age = p_arage.
  gs_global_conf-ar_threshold = p_arthd.
  gs_global_conf-pwd_age = p_pwage.
  gs_global_conf-pwd_threshold = p_pwthd.
  call transformation id source struct = gs_global_conf
                       result xml gv_xml_string.
  gs_conf_table-type = 'GLOBAL'.
  gs_conf_table-data_str = gv_xml_string.
  modify zgnpusersconf from gs_conf_table.
endform.
