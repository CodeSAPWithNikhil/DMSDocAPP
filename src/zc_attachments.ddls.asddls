@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'document attachments'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@UI.headerInfo:{typeName: 'Attachments',
typeNamePlural: 'Attachments',
    title: {
        type: #STANDARD,
        label: 'Attachments',
        value: 'DocumentInfoRecordDocNumber'
    }}
define view entity zc_attachments
  as select from I_DocumentInfoRecordAttachment
  association to parent ZC_DOCUMENTS as _document on  _document.DocumentInfoRecordDocType    = $projection.DocumentInfoRecordDocType
                                                  and _document.DocumentInfoRecordDocNumber  = $projection.DocumentInfoRecordDocNumber
                                                  and _document.DocumentInfoRecordDocVersion = $projection.DocumentInfoRecordDocVersion
                                                  and _document.DocumentInfoRecordDocPart    = $projection.DocumentInfoRecordDocPart


{
      @UI.facet: [{  id: 'Attachments',
                        purpose: #STANDARD,
                        position: 10,
                        label: 'Attachment Item',
                        type: #IDENTIFICATION_REFERENCE },

      //**----  Body facets for attachment
      { label: 'Attached Files',
        id: 'Attachment',
         type: #FIELDGROUP_REFERENCE,
         targetQualifier: 'Fileinfo',
         position: 90    } ]

      @UI:{ lineItem: [{ position: 10 }],
      identification: [{ position: 10 }]}
      @EndUserText.label:'Document Number'
  key DocumentInfoRecordDocNumber,
      @UI:{ lineItem: [{ position: 20 }],
      identification: [{ position: 20 }]}
      @EndUserText.label:'Document Type'
  key DocumentInfoRecordDocType,
      @UI:{ lineItem: [{ position: 30 }],
      identification: [{ position: 30 }]}
      @EndUserText.label:'Document Version'
  key DocumentInfoRecordDocVersion,
      @UI:{ lineItem: [{ position: 40 }],
      identification: [{ position: 40 }]}
      @EndUserText.label:'Document Record'
  key DocumentInfoRecordDocPart,
  key DocumentIndex,
  key FileUUID,
  key PhysicalDocument,

      FileName,
      FileSize,
      WorkstationApplication,
      @Semantics.mimeType: true
      MimeType,


      StorageCategory,
      IsOriginalVersionActive,
      OriginalIsProtected,
      DocumentTitle,
      DocumentFormat,
      CreationDateTime,
      @UI:{ lineItem: [{ position: 50 }],
       identification: [{ position: 50 }]}
      @EndUserText.label:'Created by'

      CreatedByUser,
      ChangedDateTime,
      LastChangedByUser,
      OriginalCheckedOutTime,
      OriginalCheckedOutUser,
      LogicalDocument,
      LogicalDocumentClass,
      PhysicalDocumentClass,
      LogicalDocumentIsReference,
      OriginalType,
      PhysicalDocTechnicalStatus,
      InternalComment,


      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_ATTACHMENT'
      abap.string''    as downloadUrl,

      @UI:{ lineItem: [{position: 70,type:#WITH_URL, url:'downloadUrl' }],
      identification: [{position: 70,type:#WITH_URL, url:'downloadUrl' }],
      fieldGroup: [{ position: 90,qualifier: 'Fileinfo', label:'File download(in V2)',type:#WITH_URL, url:'downloadUrl' }]}
      @EndUserText.label:'Download File(in V2 service)'
      FileName         as downloadFile, // THIS IS FOR V2 as contentDispositionPreference may not work in V4

      @UI:{ lineItem: [{ position: 80 }],
       identification: [{ position: 80 }],
       fieldGroup: [{ position: 90,qualifier: 'Fileinfo', label:'File download(in V4)' }]}

      @Semantics.largeObject: { mimeType: 'MimeType',
                                fileName: 'FileName',
                                cacheControl:{maxAge: #LONG},
                                contentDispositionPreference: #ATTACHMENT  }

      @EndUserText.label:'Download File(in V4 service)'
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_ATTACHMENT' // this should be read only
      abap.rawstring'' as fileData,
      /* Associations */
      _ChangedUser,
      _CheckedOutUser,
      _CreatedUser,
      _document
}
