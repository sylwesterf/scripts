Attribute VB_Name = "Module2"
Sub Macro1()
Attribute Macro1.VB_ProcData.VB_Invoke_Func = "e\n14"
'
' Macro1 Macro
'
' Keyboard Shortcut: Ctrl+e
'

   Dim MyFolder As String 'Path collected from the folder picker dialog

   Dim MyFile As String 'Filename obtained by DIR function

   Dim wbk As Workbook 'Used to loop through each workbook
   
   Dim csv As String 'csv name
   

On Error Resume Next

Application.ScreenUpdating = False

'Opens the folder picker dialog to allow user selection

With Application.FileDialog(msoFileDialogFolderPicker)

.Title = "Please select a folder"

.Show

.AllowMultiSelect = False

   If .SelectedItems.Count = 0 Then 'If no folder is selected, abort

MsgBox "You did not select a folder"

      Exit Sub

   End If

MyFolder = .SelectedItems(1) & "\" 'Assign selected folder to MyFolder

End With

MyFile = Dir(MyFolder) 'DIR gets the first file of the folder

'Loop through all files in a folder until DIR cannot find anymore

Do While MyFile <> ""

   'Opens the file and assigns to the wbk variable for future use

   Set wbk = Workbooks.Open(Filename:=MyFolder & MyFile)
   
   csv = Left(MyFile, Len(MyFile) - 5)

   'Replace the line below with the statements you would want your macro to perform

Range("A2").Select
Range(Selection, Selection.End(xlDown)).Select
Selection.NumberFormat = "m/d/yyyy"

wbk.SaveAs Filename:= _
        "C:\Users\xxx\" & csv & ".csv", FileFormat:= _
        xlCSV, CreateBackup:=False
        
wbk.Close savechanges:=True

MyFile = Dir 'DIR gets the next file in the folder

Loop

Application.ScreenUpdating = True

End Sub


