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
  begin of gs_conf_personal,
    conf_nw   type text1,
    compl_nw  type text1,
    sn_update type text1,
  end of gs_conf_personal.
data:
  gs_conf_db    type zgnpusersconf,
  gv_xml_string type string.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block global with frame title text-001.
parameters p_dest  type rfcdest.
selection-screen begin of line.
selection-screen comment 1(33) text-002 for field p_conf.
selection-screen end of line.
parameters:
  p_armax type numc3,
  p_arwng type numc3.
selection-screen begin of line.
selection-screen comment 1(33) text-003 for field p_conf.
selection-screen end of line.
parameters:
  p_pwmax type numc3,
  p_pwwng type numc3.
selection-screen end of block global.
selection-screen begin of block personal with frame title text-004.
selection-screen begin of line.
selection-screen comment 1(33) text-005 for field p_conf.
selection-screen position 35.
parameters p_conf as checkbox.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment 1(33) text-006 for field p_compl.
selection-screen position 35.
parameters p_compl as checkbox.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment 1(33) text-007 for field p_sn.
selection-screen position 35.
parameters p_sn as checkbox.
selection-screen end of line.
selection-screen end of block personal.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.
  set pf-status 'SEL_SCREEN'.
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
  " Personal configuration
  select single * from zgnpusersconf into corresponding fields of gs_conf_db
    where type eq 'PERSONAL' and value eq sy-uname.
  if sy-subrc eq 0.
    gv_xml_string = gs_conf_db-data_str.
    call transformation id source xml gv_xml_string
    result struct = gs_conf_personal.
    p_conf  = gs_conf_personal-conf_nw.
    p_compl = gs_conf_personal-compl_nw.
    p_sn = gs_conf_personal-sn_update.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form SAVE_CONFIGURATION
*&---------------------------------------------------------------------*
form save_configuration.
  " Global configuration
  clear gs_conf_db.
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
    message e000(0k) with 'Global configuration not updated'.
    exit.
  endif.
  " Personal configuration
  clear gs_conf_db.
  gs_conf_personal-conf_nw = p_conf.
  gs_conf_personal-compl_nw =  p_compl.
  gs_conf_personal-sn_update = p_sn.
  call transformation id source struct = gs_conf_personal
                       result xml gv_xml_string.
  gs_conf_db-type = 'PERSONAL'.
  gs_conf_db-value = sy-uname.
  gs_conf_db-data_str = gv_xml_string.
  modify zgnpusersconf from gs_conf_db.
  if sy-subrc ne 0.
    message e000(0k) with 'Personal configuration not updated'.
    exit.
  endif.
  message s000(0k) with 'Configuration successfully updated'.
endform.
