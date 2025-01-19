# DMSDocAPP

### Simple Demo RAP application to copy dms documents

This is a basic demo app built using RAP (RESTful ABAP Programming Model) to copy DMS (Document Management System) documents.  

![image](https://github.com/user-attachments/assets/56ee21a6-62a4-46af-8ba8-28df488c6636)

#### What Does It Do? 
- **Copies DMS Documents**:  
   The app uses the **custom factory action `COPY`** to duplicate documents. The actual copying happens during the transactional phase using **`cl_abap_parallel`** that calls BAPI_DOCUMENT_CREATE2, keeping everything quick and seamless.  

- **Displays the Files Dynamically**:  
   A **dynamic URI** is created in a **virtual element** to display the copied file after clicking it.  

- **Fetches File Content**:  
   The app fetches the file content dynamically within virtual elements, making it simple to access the document data directly.  

#### Why Build It?  
This app is a small and straightforward tool to demonstrate how RAP can handle DMS documents efficiently. It’s not fancy, but it gets the job done in a clean and simple way. Perfect for basic use cases or as a starting point for something bigger!  
