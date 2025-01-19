@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Documents'
@Metadata.ignorePropagatedAnnotations: true

@UI.headerInfo:{typeName: 'DMS Documents',
typeNamePlural: 'DMS Documents',
    title: {
        type: #STANDARD,
        label: 'DMS Documents',
        value: 'DocumentInfoRecordDocNumber'
    }}
define root view entity ZC_DOCUMENTS
  as select from C_DocumentInfoRecordTP as _document

  composition [*] of zc_attachments as _attachments
{
      @UI.facet: [{  id: 'Documents',
                   purpose: #STANDARD,
                   position: 10,
                   label: 'Documents',
                   type: #IDENTIFICATION_REFERENCE },

                   { id: 'Attachments',
                   purpose: #STANDARD,
                   position: 20,
                   label: 'Attachments',
                   type: #LINEITEM_REFERENCE,
                   targetElement: '_attachments' }]

      @Consumption.valueHelpDefinition: [
        { entity:  { name:    'I_DocumentInfoRecordDocPrtVH',
                     element: 'DocumentInfoRecordDocNumber' },
          additionalBinding: [{ localElement: 'DocumentInfoRecordDocType',
                                element: 'DocumentInfoRecordDocType' },
                              { localElement: 'DocumentInfoRecordDocVersion',
                                element: 'DocumentInfoRecordDocVersion' },
                              { localElement: 'DocumentInfoRecordDocPart',
                                element: 'DocumentInfoRecordDocPart' }]
        }]
      @UI:{ lineItem: [{ position: 10 }],
      identification: [{ position: 10 }],
      selectionField: [{ position: 10 }]}
      @EndUserText.label:'Document Number'
  key DocumentInfoRecordDocNumber,
      @UI:{ lineItem: [{ position: 20 }],
      identification: [{ position: 20 }],
      selectionField: [{ position: 20 }]}
      @EndUserText.label:'Document Type'
  key DocumentInfoRecordDocType,
      @UI:{ lineItem: [{ position: 30 }],
      identification: [{ position: 30 }],
      selectionField: [{ position: 30 }]}
      @EndUserText.label:'Document Version'
  key DocumentInfoRecordDocVersion,
      @UI:{ lineItem: [{ position: 40 }],
      identification: [{ position: 40 }],
      selectionField: [{ position: 40 }]}
      @EndUserText.label:'Document Record'
  key DocumentInfoRecordDocPart,
      DocInfoRecdDocNumberForEdit,
      DocInfoRecdDocTypeForEdit,
      DocInfoRecdDocPartForEdit,
      DocInfoRecdDocVersionForEdit,
      DocumentDescriptionForEdit,
      ExternalDocumentStatusForEdit,
      DocumentInfoRecord,
      ChangedDateTime,
      LastChangedByUser,
      LastChangedByUserFullName,
      CreatedByUser,
      @UI:{ lineItem: [{ position: 50 }],
      identification: [{ position: 50 }],
      selectionField: [{ position: 50 }]}
      @EndUserText.label:'Created on'
      CreationDateTime,

      @UI:{lineItem: [{ position: 10, type: #FOR_ACTION, dataAction: 'copy', label: 'Copy document to another types' }],
      identification: [{ position: 10, type: #FOR_ACTION, dataAction: 'copy', label: 'Copy document to another types' }] }
      _attachments


}
