*&---------------------------------------------------------------------*
*& Report ZRGNPUSERS_API_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zrgnpusers_api_test.
* RSICFCLTST01

data:
  go_http_client type ref to cl_http_client,
  gv_destination type char1024.

gv_destination = 'SERVICE_NOW'.

* perform post_change_request.
perform get_change_request.

*&---------------------------------------------------------------------*
*& Form POST_CHANGE_REQUEST
*&---------------------------------------------------------------------*
form post_change_request .
  types:
    begin of ty_result,
      number type string,
      state  type string,
    end of ty_result.

  data:
    begin of ls_response,
      result type ty_result,
    end of ls_response.

  data:
    lo_http_client type ref to if_http_client,
    lo_rest_client type ref to if_rest_client,
    lo_rest_entity type ref to if_rest_entity,
    ls_result      type ty_result,
    lv_uri_path    type string,
    lv_return_code type i,
    lv_json_data   type string.

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
  lo_rest_client->post( lo_rest_entity ).
  lv_return_code = lo_rest_client->get_status( ).
  if lv_return_code eq 201.
    lo_rest_entity = lo_rest_client->get_response_entity( ).
    lv_json_data = lo_rest_entity->get_string_data( ).
    /ui2/cl_json=>deserialize( exporting json = lv_json_data
        changing data = ls_response ).
    write:/ ls_response-result-number, ls_response-result-state.
  endif.
endform.

*&---------------------------------------------------------------------*
*& Form GET CHANGE_REGUEST
*&---------------------------------------------------------------------*
form get_change_request.
  types:
    begin of ty_result,
      number type string,
      state  type string,
    end of ty_result.

  data:
    begin of ls_response,
      result type standard table of ty_result,
    end of ls_response.

* types: lt_data type standard table of string with empty key.

  data:
    lo_http_client type ref to if_http_client,
    lo_rest_client type ref to if_rest_client,
    lo_rest_entity type ref to if_rest_entity,
    ls_result      type ty_result,
    lv_uri_path    type string,
    lv_return_code type i,
    lv_json_data   type string,
    lv_state(2)    type c.

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
  lo_rest_client->get( ).
  lv_return_code = lo_rest_client->get_status( ).
  if lv_return_code eq 200.
    lo_rest_entity = lo_rest_client->get_response_entity( ).
    lv_json_data = lo_rest_entity->get_string_data( ).
    /ui2/cl_json=>deserialize( exporting json = lv_json_data
      changing data = ls_response  ).
    loop at ls_response-result into ls_result.
      lv_state = ls_result-state.
      case lv_state.
        when '-5'. "New"
          ls_result-state = 'In process TO'.
        when '-4'.  "Access"
          ls_result-state = 'In process BO'.
        when others.
      endcase.
      write:/ ls_result-number, ls_result-state.
    endloop.
  endif.

endform.
