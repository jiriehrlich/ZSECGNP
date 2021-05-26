*&---------------------------------------------------------------------*
*& Report ZRGNPUSERS_CONF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers_conf.
data:
  begin of gs_conf_global,
    destination type rfcdest,
    ar_maxage   type numc3,
    ar_wngage   type numc3,
    pwd_maxage  type numc3,
    pwd_wngage  type numc3,
  end of gs_conf_global.
data:
  gs_conf_db    type zgnpusersconf,
  gv_xml_string type string.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block global with frame title text-001.
parameters:
  p_dest  type rfcdest,
  p_armax type numc3,
  p_arwng type numc3,
  p_pwmax type numc3,
  p_pwwng type numc3.
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
  " Global configuration
  select single * from zgnpusersconf into corresponding fields of gs_conf_db
    where type eq 'GLOBAL'.
  if sy-subrc eq 0.
    gv_xml_string = gs_conf_db-data_str.
    call transformation id source xml gv_xml_string
    result struct = gs_conf_global.
    p_dest  = gs_conf_global-destination.
    p_armax = gs_conf_global-ar_maxage.
    p_arwng = gs_conf_global-ar_wngage.
    p_pwmax = gs_conf_global-pwd_maxage.
    p_pwwng = gs_conf_global-pwd_wngage.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form SAVE_CONFIGURATION
*&---------------------------------------------------------------------*
form save_configuration.
  " Global configuration
  gs_conf_global-destination = p_dest.
  gs_conf_global-ar_maxage = p_armax.
  gs_conf_global-ar_wngage = p_arwng.
  gs_conf_global-pwd_maxage = p_pwmax.
  gs_conf_global-pwd_wngage = p_pwwng.
  call transformation id source struct = gs_conf_global
                       result xml gv_xml_string.
  gs_conf_db-type = 'GLOBAL'.
  gs_conf_db-data_str = gv_xml_string.
  modify zgnpusersconf from gs_conf_db.
  if sy-subrc ne 0.
    message e000(0k) with 'Configuration not updated'.
    exit.
  endif.
  message s000(0k) with 'Configuration successfully updated'.
endform.
