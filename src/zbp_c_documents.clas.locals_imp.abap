CLASS lhc__document DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR _document RESULT result.
    METHODS copy FOR MODIFY
      IMPORTING keys FOR ACTION _document~copy.
    TYPES: tt_document   TYPE HASHED TABLE OF zc_documents WITH UNIQUE KEY documentinforecorddocnumber documentinforecorddocpart
             documentinforecorddoctype documentinforecorddocversion,
           tt_attachment TYPE HASHED TABLE OF zc_attachments WITH UNIQUE KEY
             documentinforecorddocnumber documentinforecorddocpart documentinforecorddoctype documentinforecorddocversion documentindex fileuuid
                           physicaldocument.

    TYPES: BEGIN OF MESH ty_docs,
             t_document   TYPE tt_document ASSOCIATION _attach TO t_attachment ON
               documentinforecorddocnumber   =  documentinforecorddocnumber AND
               documentinforecorddocpart     =  documentinforecorddocpart   AND
               documentinforecorddoctype     =  documentinforecorddoctype   AND
               documentinforecorddocversion  =  documentinforecorddocversion,
             t_attachment TYPE tt_attachment ASSOCIATION _document TO t_document ON
                            documentinforecorddocnumber   =  documentinforecorddocnumber AND
                            documentinforecorddocpart     =  documentinforecorddocpart   AND
                            documentinforecorddoctype     =  documentinforecorddoctype   AND
                            documentinforecorddocversion  =  documentinforecorddocversion,
           END OF MESH ty_docs.
    DATA: s_document_mesh TYPE ty_docs.
ENDCLASS.

CLASS lhc__document IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD copy.
    READ ENTITIES OF zc_documents IN LOCAL MODE
   ENTITY _document ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(lt_document)

  BY \_attachments ALL FIELDS WITH
  CORRESPONDING #( keys )
  RESULT DATA(lt_attachments).
    s_document_mesh-t_document = CORRESPONDING #( lt_document DISCARDING DUPLICATES ).
    s_document_mesh-t_attachment = CORRESPONDING #( lt_attachments DISCARDING DUPLICATES ).


    LOOP AT s_document_mesh-t_document INTO DATA(ls_document).
      DATA(lo_thread) = NEW zcl_attachment(  ).
      lo_thread->set_header( ls_document ).
      lo_thread->set_new_doc_type( ls_document-documentinforecorddoctype ).
      LOOP AT s_document_mesh-t_document\_attach[ ls_document ] REFERENCE INTO DATA(ls_attachment).
        ls_attachment->filedata = zcl_attachment=>get_file_content( ls_attachment->* ).
        lo_thread->add_attachment( ls_attachment->* ).
      ENDLOOP.

      NEW cl_abap_parallel( )->run_inst( EXPORTING p_in_tab  = VALUE #( ( lo_thread ) )
                                         IMPORTING p_out_tab = DATA(lt_output) ).

      WAIT UP TO 2 SECONDS.
      LOOP AT lt_output INTO DATA(lo_output).
        CAST zcl_attachment( lo_output-inst )->get_results( IMPORTING ev_doc_number = DATA(lv_doc)
                                                                      es_log        = DATA(ls_log) ).

        IF lv_doc IS NOT INITIAL.
          READ ENTITIES OF zc_documents IN LOCAL MODE
         ENTITY _document ALL FIELDS WITH
            VALUE #(                ( documentinforecorddocnumber  = lv_doc
                                      documentinforecorddoctype    = ls_document-documentinforecorddoctype "keys[ 1 ]-%paramls_document--documentinforecorddoctype
                                      documentinforecorddocpart    = ls_document-documentinforecorddocpart
                                      documentinforecorddocversion = ls_document-documentinforecorddocversion ) )
            RESULT DATA(lt_document_copy)

             BY \_attachments ALL FIELDS WITH
                           VALUE #( ( documentinforecorddocnumber  = lv_doc
                                      documentinforecorddoctype    = ls_document-documentinforecorddoctype
                                      documentinforecorddocpart    = ls_document-documentinforecorddocpart
                                      documentinforecorddocversion = ls_document-documentinforecorddocversion ) )
                RESULT DATA(lt_attachments_copy).
          mapped-_document = CORRESPONDING #(  lt_document_copy ).
          LOOP AT mapped-_document ASSIGNING FIELD-SYMBOL(<fs_mapped>).
            <fs_mapped>-%cid = keys[ sy-tabix ]-%cid.
          ENDLOOP.
        ELSE.
          failed-_document = CORRESPONDING #(  lt_document ).
          LOOP AT failed-_document ASSIGNING FIELD-SYMBOL(<fs_failed>).
            <fs_failed>-%cid = keys[ sy-tabix ]-%cid.
          ENDLOOP.
        ENDIF.

        reported-_document = VALUE #( FOR ls_key IN keys
                                      ( %key = CORRESPONDING #( ls_key )
                           " %cid = ls_key-%cid
                                        %msg = COND #( WHEN ls_log IS NOT INITIAL THEN new_message( id       = ls_log-id
                                                                                                    number   = ls_log-number
                                                                                                    severity = CONV #( ls_log-type )
                                                                                                    v1       = ls_log-message_v1
                                                                                                    v2       = ls_log-message_v2 )
                        ELSE new_message_with_text(                                                 severity = SWITCH #( lv_doc WHEN space THEN if_abap_behv_message=>severity-error
                                                                                                                                ELSE if_abap_behv_message=>severity-success )
                                                                                                    text     = SWITCH #( lv_doc WHEN space THEN 'Failed to copy this document'
                                                                                                                                ELSE lv_doc && ' created'
                        ) )
                        ) ) ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.

CLASS lsc_zc_documents DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zc_documents IMPLEMENTATION.

  METHOD adjust_numbers.
  ENDMETHOD.

  METHOD save_modified.

  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
