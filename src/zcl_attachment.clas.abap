CLASS zcl_attachment DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .
    INTERFACES if_abap_parallel.
    METHODS: set_new_doc_type
      IMPORTING
        iv_doc_typ TYPE zc_attachments-documentinforecorddoctype,
      get_results
        EXPORTING ev_doc_number TYPE zc_documents-documentinforecorddocnumber
                  es_log        TYPE bapiret2,
      add_attachment
        IMPORTING is_attachment TYPE zc_attachments,
      set_header
        IMPORTING is_document TYPE zc_documents.
    CLASS-METHODS get_file_content
      IMPORTING
        ls_att_item      TYPE zc_attachments
      RETURNING
        VALUE(rv_result) TYPE zc_attachments-filedata.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_buffer,
             document TYPE zc_documents-documentinforecorddocnumber,
             filename TYPE zc_attachments-filename,
             filedata TYPE zc_attachments-filedata,
           END OF ty_buffer.

    DATA: s_document    TYPE zc_documents,
          t_attachments TYPE TABLE OF zc_attachments.
    DATA: v_doc_typ    TYPE dokar,
          v_doc_number TYPE zc_documents-documentinforecorddocnumber,
          s_log        TYPE bapiret2.
    CLASS-DATA: t_buffer     TYPE TABLE OF ty_buffer WITH EMPTY KEY.
    METHODS upload_and_get_filename
      IMPORTING
        is_attach        TYPE zc_attachments
      RETURNING
        VALUE(rv_result) TYPE string.
ENDCLASS.



CLASS zcl_attachment IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA: lt_content       TYPE TABLE OF drao,
          lt_original_data TYPE TABLE OF zc_attachments.
    DATA server TYPE REF TO cl_http_server_net.
    lt_original_data = CORRESPONDING #( it_original_data ).

    DATA(lv_url_with_placeholders) = VALUE string( ).

    SELECT low, name FROM tvarvc WHERE name LIKE 'ZDMSAPPURL%'
            ORDER BY name ASCENDING
            INTO TABLE @DATA(lt_url).

    lv_url_with_placeholders = REDUCE #( INIT url = VALUE #( )
                                         FOR ls_url IN lt_url
                                         NEXT url = url && ls_url-low ).


    LOOP AT lt_original_data REFERENCE INTO DATA(ls_line).

      DATA(lv_url_helper) = lv_url_with_placeholders.

      ls_line->filedata = get_file_content( ls_line->* ).

      DO 7 TIMES. "because of seven keys

        ASSIGN COMPONENT sy-index OF STRUCTURE ls_line->* TO FIELD-SYMBOL(<fs_key>).

        IF sy-subrc = 0 AND <fs_key> IS ASSIGNED.

          DATA(lv_variable_place_holder) = CONV string(  '&' && sy-index ).

          DATA(lv_key_of_url) = CONV string( <fs_key> ).

          CONDENSE lv_key_of_url.

          REPLACE ALL OCCURRENCES OF lv_variable_place_holder IN lv_url_helper WITH lv_key_of_url.
        ENDIF.
      ENDDO.

      ls_line->downloadurl = lv_url_helper.

    ENDLOOP.
    ct_calculated_data   = CORRESPONDING #( lt_original_data ).
  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
  ENDMETHOD.


  METHOD get_file_content.

    DATA: lt_content TYPE TABLE OF drao.

    DATA(ls_phio) = VALUE sdokobject( class = ls_att_item-physicaldocumentclass
                                      objid = ls_att_item-physicaldocument ).

    CALL FUNCTION 'CV120_KPRO_CHECKOUT_TO_TABLE'
      EXPORTING
        ps_phio_id  = ls_phio
        pf_comp_get = space
      TABLES
        ptx_content = lt_content
      EXCEPTIONS
        error       = 1
        no_content  = 2
        OTHERS      = 3.

    rv_result = REDUCE #( INIT file = VALUE xstring( )
                           FOR ls_content IN lt_content
                           NEXT file = file && ls_content-orblk ).
  ENDMETHOD.


  METHOD if_abap_parallel~do.

    DATA: lt_objectfiles TYPE TABLE OF bapi_doc_files2.
    DATA: return TYPE bapiret2.

    lt_objectfiles = VALUE #( FOR ls_attach IN t_attachments
                              ( docfile         = upload_and_get_filename( ls_attach )
                                wsapplication   = ls_attach-workstationapplication
                                checkedin       = 'X'
                                storagecategory = 'DMS_CS'
                                created_by      = sy-uname ) ).

    WAIT UP TO 1 SECONDS.

    DATA(ls_docdata)    = VALUE bapi_doc_draw2( documenttype    = v_doc_typ
                                                documentversion = s_document-documentinforecorddocversion
                                                documentpart    = s_document-documentinforecorddocpart ).


    CALL FUNCTION 'BAPI_DOCUMENT_CREATE2'
      EXPORTING
        documentdata   = ls_docdata
        pf_ftp_dest    = 'SAPFTPA'
        pf_http_dest   = 'SAPHTTPA'
      IMPORTING
        documentnumber = v_doc_number
        return         = s_log
      TABLES
        documentfiles  = lt_objectfiles.

    IF v_doc_number IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
  ENDMETHOD.


  METHOD upload_and_get_filename.

    DATA(lv_msg) = VALUE string( ).

    DATA(lv_filename) = '/tmp/' && is_attach-filename.

    OPEN DATASET lv_filename FOR OUTPUT IN BINARY MODE MESSAGE lv_msg.

    IF sy-subrc = 0.

      DATA(lv_content) = get_file_content( is_attach ).
      TRANSFER lv_content TO lv_filename.
      CLOSE DATASET lv_filename.

    ENDIF.

    RETURN  lv_filename.
  ENDMETHOD.


  METHOD set_new_doc_type.
    v_doc_typ = iv_doc_typ.
  ENDMETHOD.


  METHOD get_results.
    ev_doc_number = v_doc_number.
    es_log = s_log.
  ENDMETHOD.


  METHOD add_attachment.
    APPEND is_attachment TO t_attachments.
  ENDMETHOD.


  METHOD set_header.
    s_document = is_document.
  ENDMETHOD.
ENDCLASS.
