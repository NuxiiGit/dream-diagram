'' import pre-made class libraries
Imports System.Math
Imports System.Drawing

'' import custom classes
Imports Graph_Program.Graph_Struct
Imports Graph_Program.Custom_Buttons

'' this is the main form
Public Class DevInterface

    '' initialise private space variables
    Private Dim workingfile As String = ""
    Private Dim view_offset As Point = New Point(0,0)
    Private Dim button_clicked As Boolean
    Private Dim window_beingDragged As Boolean = False
    Private Dim window_dragOffset As Point
    Private Dim window_resizeLeft As Boolean = False
    Private Dim window_resizeRight As Boolean = False
    Private Dim window_resizeUp As Boolean = False
    Private Dim window_resizeDown As Boolean = False

    '' initialise "global" space variables
    Public Dim graph As Graph_Struct = New Graph_Struct
    Public Dim selectedNode As Integer = -1
    Public Dim selectedEdge As Integer = -1
    
    '' initialise application toolbars
    Private Dim toolbar_tabs() as button     ' the 4 tabs (and x) located at the top of the screen
    Private Dim toolbar_left() As button     ' the small toolbar located left of the screen
    Private Dim toolbar_file() As button     ' the file tab of the main toolbar
    Private Dim toolbar_insert() As button   ' the insert tab of the main toolbar
    Private Dim toolbar_graph() As button    ' the graph tab
    Private Dim toolbar_analysis() As button ' the analysis tab (algorithms)

    '' initialise enumerated constants for toolbars
    Private Enum toolbars
        file = 0
        graph = 1
        insert = 2
        analysis = 3
    End Enum
    Private Dim toolbar_selection As Byte = toolbars.file ' default

    '' initialise colours
    Private Const BRIGHTYELLOW As Integer = &hCBF3FF
    Private Const GREY As Integer = &h808080
    Private Const LIGHTBLUE As Integer = &hEED6BC

    Private Const ORANGE As Integer = &h82B0F4
    Private Const ORANGE2 As Integer = &hBDD5F9
    Private Const DIRTYWHITE As Integer = &hEDEDED
    Private Const DIRTYWHITE2 As Integer = &hF5F5F5
    Private Const LIGHTGREY As Integer = &hD8D8D8
    Private Const LIGHTGREY2 As Integer = &hEAEAEA
    Private Const LIGHTGREEN As Integer = &hD4FDCF
    Private Const LIGHTGREEN2 As Integer = &hE8FEE6
    Private Const CYAN As Integer = &hFEF9CE
    Private Const CYAN2 As Integer = &hFFFFE7
    Private Const RED As Integer = &h5F83FF
    Private Const RED2 As Integer = &hAABDFF

    '' initialise minimum width and height of the window
    Public Const MINWIDTH As Integer = 450
    Public Const MINHEIGHT As Integer = 205

    '' initialise mouse button constants
    Private Const LEFTMB As Integer = &h1
    Private Const RIGHTMB As Integer = &h2
    Private Const MIDDLEMB As Integer = &h4

    '' initialise mouse variables
    Private Dim previous_mouse_position As Point
    Private Dim local_mouse_position As Point ' mouse pos - view offset
    Public Dim mouse_position As Point = New Point(0,0)
    Public Dim mouse_pressed As Boolean
    Public Dim mouse_held As Boolean = False
    Public Dim previous_mouse_held As boolean

    '' right mouse button flags
    Private Dim mouse_right_pressed As Boolean
    Private Dim mouse_right_held As Boolean = False
    Private Dim previous_mouse_right_held As Boolean
    Private Dim ignore_click As Boolean = False

    '' middle mouse button flag
    Private Dim mouse_mid_held As Boolean = False

    '' initialise enumerated constants for better readability of code
    Private Enum mouseState
        none = 0
        addNode = 1
        addEdge = 2
        removeObject = 3
        dijkstras = 4
        depthFirst = 5
        breadthFirst = 6
    End Enum
    Private Dim mouse_state As Byte = mouseState.none ' no tool selected

    '' import external library which allows me to obtain the user's keystrokes
    Public Declare Function GetAsyncMouseButtonClick Lib "User32" Alias "GetAsyncKeyState" (ByVal IO_VirtualKey As Integer) As Short

    '' create procedures which are to be called by the delegate function pointers
    #Region "Delegate mouse state procedures"

        '' the following procedures set the user's mouse state
        Private Sub mouseState_set_none()
            mouse_state = mouseState.none
        End Sub
        Private Sub mouseState_set_addNode()
            mouse_state = mouseState.addNode
        End Sub
        Private Sub mouseState_set_addEdge()
            mouse_state = mouseState.addEdge
        End Sub
        Private Sub mouseState_set_removeObject()
            mouse_state = mouseState.removeObject
        End Sub
        Private Sub mouseState_set_Dijkstras()
            mouse_state = mouseState.dijkstras
        End Sub
        Private Sub mouseState_set_depthFirst()
            mouse_state = mouseState.depthFirst
        End Sub
        Private Sub mouseState_set_breadthFirst()
            mouse_state = mouseState.breadthFirst
        End Sub

    #End Region
    #Region "Delegate toolbar state procedures"
        
        '' function to easily change the colours of the tabs without repeating code
        Private Sub setToolbar(byVal toolbar As Byte)
            For index As Integer = 0 To 3
                '' set all four butons to Orange
                toolbar_tabs(index).primaryColour = ORANGE
                toolbar_tabs(index).secondaryColour = ORANGE2
            Next
            '' set the desired toolbar to grey (selected toolbar)
            toolbar_tabs(toolbar).primaryColour = DIRTYWHITE
            toolbar_tabs(toolbar).secondaryColour = DIRTYWHITE2
        End Sub

        '' the following procedures set the toolbar state
        Private Sub toolbar_set_file()
            toolbar_selection = toolbars.file
            '' update colours of toolbars
            setToolbar(toolbars.file)
        End Sub
        Private Sub toolbar_set_graph()
            toolbar_selection = toolbars.graph
            setToolbar(toolbars.graph)
        End Sub
        Private Sub toolbar_set_insert()
            toolbar_selection = toolbars.insert
            setToolbar(toolbars.insert)
        End Sub
        Private Sub toolbar_set_analysis()
            toolbar_selection = toolbars.analysis
            setToolbar(toolbars.analysis)
        End Sub

    #End Region
    #Region "Delegate file handling procedures"
        
        '' sub procedure for creating a new graph file
        Private Sub file_new()
            '' declare a lambda function so that it may be called at various conditions
            '' (without having to copy+paste the code again)
            Dim purgeGraph = Sub()
                                 '' reset the working file (currently open graph)
                                 workingfile = ""
                                 '' reset the user's view offset
                                 view_offset = New Point(0,0)
                                 '' initialise a new graph state
                                 graph.initialise_new_graph()
                                 '' reset the application title
                                 Me.Text = "New Graph - 日記 Dream 図表 Diagram"
                             End Sub
            '' check if the current graph contains data
            If (Graph.NodeNumb <> 0) Then
                '' warn the user that this action may overwrite their data, and if they wish to save
                select Case MsgBox("Performing this action may overwrite your current graph" & vbCrLf & _
                                    "Do you wish to save your graph before creating a new one?", _
                                    MsgBoxStyle.YesNoCancel + MsgBoxStyle.Information,"Warning!")
                    '' the user has chosen to save their file
                    Case MsgBoxResult.Yes:
                        '' if the user has a file open already, automatically save it, otherwise prompt them to save to a new location
                        If (workingfile = "") Then file_saveAs() else file_save()
                        purgeGraph()
                        Exit Select ' break
                    '' the user has chosen to continue without saving
                    Case MsgBoxResult.No:
                        purgeGraph()
                        Exit Select
                    '' the user has chosen "cancel" or exited in another way
                    Case Else:
                        Exit Sub
                End Select
            Else
                '' if the graph contains no data, then there is no need to warn the user data will be removed
                purgeGraph()
            End If
        End Sub

        '' sub procedure for saving an existing graph to an existing file
        Private Sub file_save()
            '' check if the user currently has a graph open. if so, save it to this locaiton
            '' otherwise, pass this function onto the file_saveAs procedure, so the user
            '' manually decides a save location
            If (workingfile <> "") Then graph.file_save(workingfile) Else file_saveAs()
        End Sub

        '' sub procedure for saving a graph to a new location
        Private Sub file_saveAs()
            '' use the built-in explorer
            Using saveFile As New SaveFileDialog
                '' set-up design
                saveFile.Filter = "Graph Data File Format|*.GDF;*.NFF|Text Document|*.txt"
                saveFile.Title = "Save your graph file!"
                saveFile.DefaultExt = "GDF"
                '' once the user makes their decision
                If saveFile.ShowDialog = Windows.Forms.DialogResult.OK Then
                    graph.file_save(saveFile.FileName)
                    '' update the working File and title
                    workingfile = saveFile.FileName
                    me.Text = workingfile & " - 日記 Dream 図表 Diagram"
                End If
            End Using
        End Sub

        '' sub procedure for loading an existing graph file
        Private Sub file_load()
            '' declare lambda function
            Dim loadGraph = Sub()
                                '' using the built-in explorer
                                Using loadFile As New OpenFileDialog
                                    loadFile.Filter = "Graph Data File Format|*.GDF;*.NFF|Text Document|*.txt"
                                    loadFile.Title = "Load your graph file!"
                                    loadFile.DefaultExt = "GDF"
                                    '' when the user makes their choice
                                    If loadFile.ShowDialog = Windows.Forms.DialogResult.OK Then
                                        if loadFile.CheckFileExists Then
                                            '' load the file only if it exists
                                            Try
                                                '' try if there are any errors notify the user that their file may
                                                '' be corrupted
                                                graph.file_load(loadFile.FileName)
                                                workingfile = loadFile.FileName
                                            Catch e As Exception
                                                '' open the most recent graph
                                                If (workingfile = "") Then file_new() else graph.file_load(workingfile)
                                                '' output to the user
                                                MsgBox("The desired file (" & loadFile.FileName & ") is invalid or corrupted!", _
                                                    MsgBoxStyle.OkOnly+MsgBoxStyle.Critical, "Warning: Invalid File!")
                                            End Try
                                            Me.Text = workingfile & "- 日記 Dream 図表 Diagram"
                                        Else
                                            '' file does not exist
                                            MsgBox("It appears as though the file you have chosen does not exist!", _
                                                   MsgBoxStyle.OkOnly + MsgBoxStyle.Information, "Oops!")
                                        End If
                                    End If
                                End Using
                            End Sub
            '' check if the current graph contains data
            If (Graph.NodeNumb <> 0) Then
                '' warn the user that this action may overwrite their data, and if they wish to save
                select Case MsgBox("Performing this action may overwrite your current graph" & vbCrLf & _
                                    "Do you wish to save your graph before loading an existing file?", _
                                    MsgBoxStyle.YesNoCancel + MsgBoxStyle.Information,"Warning!")
                    '' the user has chosen to save their file
                    Case MsgBoxResult.Yes:
                        '' if the user has a file open already, automatically save it, otherwise prompt them to save to a new location
                        If (workingfile = "") Then file_saveAs else file_save()
                        loadGraph()
                        Exit Select ' break
                    '' the user has chosen to continue without saving
                    Case MsgBoxResult.No:
                        loadGraph() ' load without saving
                        Exit Select
                    '' the user has chosen "cancel" or exited in another way
                    Case Else:
                        Exit Sub
                End Select
            Else
                '' else, the graph already contains no data, so overwriting it should not matter!
                loadGraph()
            End If
        End Sub

        '' sub procedure for exporting graph data as an image file or .CSV
        Private Sub file_export()
            Const BITMAP As Byte = 1
            Const CSV As Byte = 2
            '' using the explorer
            Using exportFile As New SaveFileDialog
                exportFile.Filter = "Portable Network Graphic|*.png|Comma Separated Format|*.csv"
                exportFile.Title = "Export your graph file!"
                '' user has made a choice
                If exportFile.ShowDialog = Windows.Forms.DialogResult.OK Then
                    '' check whether the user has chosen to export as a bitmap or .CSV
                    If exportFile.FilterIndex = BITMAP Then
                        graph.exportAs_bitmap(exportFile.FileName)
                    Else If exportFile.FilterIndex = CSV Then
                        graph.exportAs_CSV(exportFile.FileName)
                    End If
                End If
            End Using
        End Sub

    #End Region
    #Region "Delegate window procedures"
        
        '' sub procedure to show the adjacency matrix form
        Private Sub window_open_adjacencyMatrix()
            ignore_click = True
            AdjacencyMatrix.Show()
        End Sub

        '' sub procedure to show the edit instance window
        Private Sub window_open_editInstance()
            ignore_click = True
            EditInstance.Show()
        End Sub

        '' sub procedure for closing the window
        Private Sub window_close()
            '' check if the current graph contains data
            If (Graph.NodeNumb <> 0) Then
                '' warn the user that this action may overwrite their data, and if they wish to save
                select Case MsgBox("Any unsaved data will be lost!" & vbCrLf & _
                                    "Do you wish to save your graph before quitting?", _
                                    MsgBoxStyle.YesNoCancel + MsgBoxStyle.Information,"Warning!")
                    '' the user has chosen to save their file
                    Case MsgBoxResult.Yes:
                        If (workingfile = "") Then file_saveAs else file_save()
                        End
                        Exit Select ' break
                    '' the user has chosen to continue without saving
                    Case MsgBoxResult.No:
                        End
                        Exit Select
                    '' the user has chosen "cancel" or exited in another way
                    Case Else:
                        Exit Sub
                End Select
            Else
                '' else, the graph already contains no data, so overwriting it should not matter!
                End
            End If
        End Sub

        '' sub procedure for minimising the window
        Private Sub window_minimise()
            Me.WindowState = FormWindowState.Minimized
        End Sub

        '' sub procedure for maximising the window
        Private Sub window_maximise()
            '' if the window is maximised already, then de-maximise it
            If (Me.WindowState = FormWindowState.Maximized) Then
                Me.WindowState = FormWindowState.Normal
            Else
                '' maximise the window
                Me.WindowState = FormWindowState.Maximized
            End If
        End Sub

    #End Region

    '' load the application
    Private Sub Application_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        '' set the form options
        Me.Text = "Dream Diagram"
        Me.FormBorderStyle = windows.forms.FormBorderStyle.None
        Me.ControlBox = False
        Me.Icon = My.Resources.Application_Icon
        '' double buffer the application (prevents flickering upon using draw functions)
        DoubleBuffered = True
        '' add pointers to delegate procedure to the toolbar buttons' "functPtr" variables
        '' toolbar tabs ("FILE|GRAPH|INSERT|ANALYSIS MINIMISE|MAXIMISE|CLOSE")
        Dim i As Integer = 0
        Dim toolbarPosition As Point = New Point(0,0)
        ReDim toolbar_tabs(6)
            '' select FILE
            toolbar_tabs(0) = New button With _
            { _
                .functPtr = AddressOf toolbar_set_file, _
                .text = "FILE", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 30 _
            }
            i += 1
            '' select GRAPH
            toolbar_tabs(1) = New button With _
            { _
                .functPtr = AddressOf toolbar_set_graph, _
                .text = "GRAPH", _
                .primaryColour = ORANGE, _
                .secondaryColour = ORANGE2, _
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 30 _
            }
            i += 1
            '' select INSERT
            toolbar_tabs(2) = New button With _
            { _
                .functPtr = AddressOf toolbar_set_insert, _
                .text = "INSERT", _
                .primaryColour = ORANGE, _
                .secondaryColour = ORANGE2, _
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 30 _
            }
            i += 1
            '' select ANALYSIS
            toolbar_tabs(3) = New button With _
            { _
                .functPtr = AddressOf toolbar_set_analysis, _
                .text = "ANALYSIS", _
                .primaryColour = ORANGE, _
                .secondaryColour = ORANGE2, _
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 60, _
                    .height = 30 _
            }
            '' window buttons (maximise/minimise/close)
            '' close window
            toolbar_tabs(4) = New button With _
            { _
                .functPtr = AddressOf window_close, _
                .text = "X", _
                .primaryColour = RED, _
                .secondaryColour = RED2, _ 
                    .x = 0, _
                    .y = toolbarPosition.Y, _
                    .width = 20, _
                    .height = 30 _
            }
            '' maximise window
            toolbar_tabs(5) = New button With _
            { _
                .functPtr = AddressOf window_maximise, _
                .text = "[_]", _
                .primaryColour = CYAN, _
                .secondaryColour = CYAN2, _ 
                    .x = 0, _
                    .y = toolbarPosition.Y, _
                    .width = 20, _
                    .height = 30 _
            }
            '' minimise window
            toolbar_tabs(6) = New button With _
            { _
                .functPtr = AddressOf window_minimise, _
                .text = "_", _
                .primaryColour = LIGHTGREEN, _
                .secondaryColour = LIGHTGREEN2, _ 
                    .x = 0, _
                    .y = toolbarPosition.Y, _
                    .width = 20, _
                    .height = 30 _
            }
        '' left toolbar
        i = 0
        toolbarPosition = New Point(0,75)
        ReDim toolbar_left(3)
            '' no tool
            toolbar_left(0) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_none, _
                .text = "Select", _
                .primaryColour = LIGHTGREY, _
                .secondaryColour = LIGHTGREY2, _ 
                    .x = toolbarPosition.X, _
                    .y = toolbarPosition.Y +25*i, _
                    .width = 100, _
                    .height = 25 _
            }
            i += 1
            '' add Node
            toolbar_left(1) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_addNode, _
                .text = "Add Node", _
                .primaryColour = LIGHTGREY, _
                .secondaryColour = LIGHTGREY2, _ 
                    .x = toolbarPosition.X, _
                    .y = toolbarPosition.Y +25*i, _
                    .width = 100, _
                    .height = 25 _
            }
            i += 1
            '' add Edge
            toolbar_left(2) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_addEdge, _
                .text = "Add Edge", _
                .primaryColour = LIGHTGREY, _
                .secondaryColour = LIGHTGREY2, _ 
                    .x = toolbarPosition.X, _
                    .y = toolbarPosition.Y +25*i, _
                    .width = 100, _
                    .height = 25 _
            }
            i += 1
            '' remove Instance
            toolbar_left(3) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_removeObject, _
                .text = "Remove Item", _
                .primaryColour = LIGHTGREY, _
                .secondaryColour = LIGHTGREY2, _ 
                    .x = toolbarPosition.X, _
                    .y = toolbarPosition.Y +25*i, _
                    .width = 100, _
                    .height = 25 _
            }
        '' file toolbar
        i = 0
        toolbarPosition = New Point(0,30)
        ReDim toolbar_file(4)
            '' create a new graph
            toolbar_file(0) = New button With _
            { _
                .functPtr = AddressOf file_new, _
                .text = "New", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 25 _
            }
            i += 1
            '' save an existing graph
            toolbar_file(1) = New button With _
            { _
                .functPtr = AddressOf file_save, _
                .text = "Save", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 25 _
            }
            i += 1
            '' save graph to a new location
            toolbar_file(2) = New button With _
            { _
                .functPtr = AddressOf file_saveAs, _
                .text = "SaveAs", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 25 _
            }
            i += 1
            '' load an existing graph
            toolbar_file(3) = New button With _
            { _
                .functPtr = AddressOf file_load, _
                .text = "Load", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 25 _
            }
            i += 1
            '' export the graph as a .png or .csv
            toolbar_file(4) = New button With _
            { _
                .functPtr = AddressOf file_export, _
                .text = "Export", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +50*i, _
                    .y = toolbarPosition.Y, _
                    .width = 50, _
                    .height = 25 _
            }
        '' graph toolbar
        REM i = 0
        REM toolbarPosition = New Point(0,30)
        ReDim toolbar_graph(2)
            '' view Adjacency Matrix
            toolbar_graph(0) = New button With _
            { _
                .functPtr = AddressOf window_open_adjacencyMatrix, _
                .text = "View Adjacency Matrix", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X, _
                    .y = toolbarPosition.Y, _
                    .width = 150, _
                    .height = 25 _
            }
            '' select a Node
            toolbar_graph(1) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_none, _
                .text = "Move a Node", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +150, _
                    .y = toolbarPosition.Y, _
                    .width = 100, _
                    .height = 25 _
            }
            '' delete an Instance
            toolbar_graph(2) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_removeObject, _
                .text = "Remove an Object", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +250, _
                    .y = toolbarPosition.Y, _
                    .width = 150, _
                    .height = 25 _
            }
        '' insert toolbar
        i = 0
        REM toolbarPosition = New Point(0,30)
        ReDim toolbar_insert(1)
            '' add Node
            toolbar_insert(0) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_addNode, _
                .text = "Create a New Node", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +120*i, _
                    .y = toolbarPosition.Y, _
                    .width = 120, _
                    .height = 25 _
            }
            i += 1
            '' add Edge
            toolbar_insert(1) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_addEdge, _
                .text = "Create a New Edge", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +120*i, _
                    .y = toolbarPosition.Y, _
                    .width = 120, _
                    .height = 25 _
            }
        '' analysis toolbar
        i = 0
        REM toolbarPosition = New Point(0,30)
        ReDim toolbar_analysis(2)
            '' Dijkstra's Algorithm
            toolbar_analysis(0) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_Dijkstras, _
                .text = "Dijkstra's Algorithm", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +150*i, _
                    .y = toolbarPosition.Y, _
                    .width = 150, _
                    .height = 25 _
            }
            i += 1
            '' Depth First Traversal
            toolbar_analysis(1) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_depthFirst, _
                .text = "Depth First Traversal", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +150*i, _
                    .y = toolbarPosition.Y, _
                    .width = 150, _
                    .height = 25 _
            }
            i += 1
            '' Breadth First Traversal
            toolbar_analysis(2) = New button With _
            { _
                .functPtr = AddressOf mouseState_set_breadthFirst, _
                .text = "Breadth First Traversal", _
                .primaryColour = DIRTYWHITE, _
                .secondaryColour = DIRTYWHITE2, _ 
                    .x = toolbarPosition.X +150*i, _
                    .y = toolbarPosition.Y, _
                    .width = 150, _
                    .height = 25 _
            }
        '' start the main loop
        mainLoop.Interval = 15
        mainLoop.Start()
    End Sub

    '' private function used to detect whether the user has pressed a button
    Private Function collision_boundingBox(ByVal r As Rectangle, ByVal mouse_pos As Point) As Boolean
        '' use bounding box collisions to detect whether a given rectangle and point intersect
        If (r.Left < mouse_pos.x) And (r.Right > mouse_pos.x) And _
           (r.Top < mouse_pos.y) And (r.Bottom > mouse_pos.y) Then Return True else Return False
    End Function

    '' private function for use with toolbars to determine whether the user has pressed a button
    Private Sub detectButtonPressIn(ByRef toolbar As button())
        '' check if a button has already been clicked
        If (Not button_clicked) then
            '' if no button was clicked, 
            '' try locate a button within this toolbar
            For Each btn As button In toolbar
                With btn
                    '' use bounding box collision checks to 
                    '' detect whether the user has clicked
                    '' this button
                    If collision_boundingBox(New Rectangle(.x,.y,.width,.height), _
                                             mouse_position) Then
                        '' button has been pressed. Thus,
                        '' call it's delegate function
                        '' pointer
                        .functPtr()
                        '' update the button clicked flag,
                        '' so multiple buttons are not
                        '' activated, and exit the rocedure
                        button_clicked = True
                        Exit Sub
                    End If
                end With
            Next
        End If
    End Sub

    '' main application loop
    Private Sub mainLoop_Tick( sender As Object,  e As EventArgs) Handles mainLoop.Tick
        '' update the Maximise, minimise, and close window buttons in the main toolbar
        toolbar_tabs(4).x = Me.Width -20
        toolbar_tabs(5).x = Me.Width -40
        toolbar_tabs(6).x = Me.Width -60
        '' update the mouse position variables
        previous_mouse_position = mouse_position
        mouse_position = Me.PointToClient(Cursor.Position)
        '' check mouse buttons for keystrokes
        previous_mouse_held = mouse_held
        previous_mouse_right_held = mouse_right_held
        If (Me.Focused) then
            '' left mouse button
            If GetAsyncMouseButtonClick(LEFTMB) then mouse_held = True else mouse_held = False
            '' right mouse button
            If GetAsyncMouseButtonClick(RIGHTMB) then mouse_right_held = True else mouse_right_held = False
            '' middle mouse button
            If GetAsyncMouseButtonClick(MIDDLEMB) then mouse_mid_held = True else mouse_mid_held = False
        Else
            mouse_held = False
            mouse_right_held = False
            mouse_mid_held = False
        End If
        '' check for a change in keystate, where the left mouse button was not held previously
        '' the following is a shorter version of the pseudo code described in my documentation!
        mouse_pressed = (Not previous_mouse_held) and (mouse_held)
        mouse_right_pressed = (Not previous_mouse_right_held) and (mouse_right_held)
        If ignore_click And (mouse_pressed Or mouse_right_pressed)
            ignore_click = False
            mouse_pressed = False
            mouse_right_pressed = False
        End If
        ''
        '' if the user is holding down their middle mouse button
        ''
        If (mouse_mid_held) Then
            '' move view
            view_offset.X += (mouse_position.X-previous_mouse_position.x)
            view_offset.y += (mouse_position.y-previous_mouse_position.y)
        End If
        '' update the alternate mouse position variables
        local_mouse_position = New Point(mouse_position.X - view_offset.X, _
                                         mouse_position.Y - view_offset.Y)
        ''
        '' if the user presses their right mouse button
        ''
        If Not mouse_held and mouse_right_pressed then
            '' edit instance
            '' obtain the pointer of the node at the relative mouse position
            selectedNode = graph.get_NodePtr_at(local_mouse_position)
            '' if no Node exists, then try obtain an Edge
            selectedEdge = -1
            If selectedNode = -1 then _
                selectedEdge = graph.get_EdgePtr_at(local_mouse_position)
            '' if an Edge or a Node was found, open the window from which the user
            '' may edit the attributes of the selected Node/Edge
            If (selectedNode <> -1) or (selectedEdge <> -1) then _
                window_open_editInstance()
        end If
        ''
        '' if the user presses their left mouse button
        ''
        If mouse_pressed then
            button_clicked = False
            '' check if the user is clicking at the Edge of the window to resize it
            '' resizing up
            If collision_boundingBox(New Rectangle(0,0,Me.Width,5), mouse_position) Then
                window_resizeUp = True : button_clicked = True : End if
            '' resizing down
            If collision_boundingBox(New Rectangle(0,Me.Height-5,Me.Width,5), mouse_position) Then
                window_resizeDown = True : button_clicked = True : End if
            '' resizing left
            If collision_boundingBox(New Rectangle(0,0,5,Me.Height), mouse_position) Then
                window_resizeLeft = True : button_clicked = True : End if
            '' resizing right
            If collision_boundingBox(New Rectangle(Me.width-5,0,5,Me.Height), mouse_position) Then
                window_resizeRight = True : button_clicked = True : End if
            '' the resizing of the window overides the buttons because if the user were to click
            '' the edge of the window where a button is located, the user would click the button
            '' instead of resize the window
            '' locate the button the user could have pressed
            detectButtonPressIn(toolbar_left) ' for the left toolbar
            detectButtonPressIn(toolbar_tabs) ' for the tabs toolbar
            Select Case toolbar_selection
                Case toolbars.file: detectButtonPressIn(toolbar_file) ' for the file toolbar
                Case toolbars.graph: detectButtonPressIn(toolbar_graph) ' for the graph toolbar
                Case toolbars.insert: detectButtonPressIn(toolbar_insert) ' for the insert toolbar
                Case toolbars.analysis: detectButtonPressIn(toolbar_analysis) ' for the analysis toolbar
            End Select
            '' if no button was found, try and detect a collision with the window bar, so the user may move the window
            If (Not button_clicked) and collision_boundingBox(New Rectangle(0,0,Me.Width,30), mouse_position) Then
                window_beingDragged = True
                window_dragOffset = mouse_position
                button_clicked = True
            Else
                window_beingDragged = False
            End If
            '' if no button was located, then use the user's mouse state and
            '' perform its corresponding function on the graph structure
            ''  (E.g. Adding an Edge, Deleting a Node, etc...)
            If Not button_clicked Then
                Select Case mouse_state
                    '' no special tool selected
                    Case mouseState.none:
                        '' get the ID of the Node at the user's mouse position
                        '' so that it can be moved by the user in the mouse_held
                        '' block
                        selectedNode = graph.get_NodePtr_at(local_mouse_position)
                    Exit Select
                    '' add Node tool
                    Case mouseState.addNode:
                        '' place a Node at the current cursor position if No Node
                        '' exists at the position yet
                        If (graph.get_NodePtr_at(local_mouse_position) = -1) then _
                            Graph.create_Node(local_mouse_position)
                    Exit Select
                    '' add Edge tool
                    case mouseState.addEdge:
                        '' select two Nodes to branch an Edge between
                        Dim desiredNode As Integer = graph.get_NodePtr_at(local_mouse_position)
                        '' if the Node exists at the position the user clicked
                        If graph.Node_exists(desiredNode) Then
                            '' check whether the user has previously selected a Node, or the
                            '' previous Node that have selected is NOT the same Node the user
                            '' has selected a second time
                            If not({-1,desiredNode}.Contains(selectedNode)) Then ' whitelist
                                graph.Create_Edge(desiredNode,selectedNode)
                                selectedNode = -1 ' reset first Node
                            Else
                                '' if the user hadn't previously selected a Node, then store the
                                '' Node pointer they had just obtained into the "selectedNode"
                                '' variable, so that the user may click another Node to create an
                                '' Edge between
                                selectedNode = desiredNode
                            End If
                        else
                            '' if the user clicks off the graph, then ignore the first Node
                            '' the user had selected
                            selectedNode = -1
                        End If
                    Exit Select
                    '' remove instances
                    Case mouseState.removeObject:
                        '' check if the user wishes to delete a Node
                        Dim desiredNode As Integer = graph.get_NodePtr_at(local_mouse_position)
                        If graph.Node_exists(desiredNode) then
                            '' delete Node
                            graph.Remove_Node(desiredNode)
                        Else
                            '' the user didn't delete a Node, so see if they wanted to delete
                            '' an Edge
                            Dim desiredEdge As Integer = graph.get_EdgePtr_at(local_mouse_position)
                            If graph.Edge_exists(desiredEdge) then
                                '' delete the Edge
                                graph.Remove_Edge(desiredEdge)
                            End If
                        End If
                    Exit Select
                    '' select two Nodes for use with the path finding algorithm
                    Case mouseState.dijkstras:
                        dim desiredNode As Integer = graph.get_NodePtr_at(local_mouse_position)
                        If graph.Node_exists(desiredNode) then ' if Node exists at the position
                            If graph.Node_exists(selectedNode) then ' second Node selected
                                '' retreive the output of a traversal algorithm between the two selected
                                '' Nodes
                                Dim out() As Integer = graph.algorithm_Dijkstras(selectedNode,desiredNode)
                                If out(0) = -1 then
                                    '' if the first Node pointer is "-1" it implies the path was
                                    '' unreachable, thus output this to the user
                                    MsgBox("Path was Unreachable!", _
                                           MsgBoxStyle.Information + MsgBoxStyle.OkOnly, _
                                           "Shortest Path Result: Infinite Cost Between Nodes")
                                else
                                    Dim str As String = ""
                                    For i As Integer = 0 to out.Length -1
                                        str += "'" & graph.Node_Table(Node.Label,out(i)) & "'"
                                        If i < out.Length-1 then str+= "->"
                                    Next
                                    MsgBox("The shortest path from Node '" & _
                                           graph.Node_Table(Node.Label,selectedNode) & "' to Node '" & _ 
                                           graph.Node_Table(Node.Label,desiredNode) & _
                                           "' was through the following Nodes:" & vbCrLf & str, _
                                           MsgBoxStyle.OkOnly, "Shortest Path Result")
                                End If
                                selectedNode = -1 ' reset the first selected Node
                            Else
                                '' set the previous Node to the current Node, so the user may
                                '' select another and perform the algorithm between them
                                selectedNode = desiredNode
                            End If
                        End If
                    Exit Select
                    '' select Node for depth first traversal
                    Case mouseState.depthFirst:
                        dim desiredNode As Integer = graph.get_NodePtr_at(local_mouse_position)
                        If graph.Node_exists(desiredNode) then
                            Dim out() As Integer= graph.algorithm_depthFirst(desiredNode)
                            Dim str As String = ""
                            For i As Integer = 0 to out.Length -1
                                str += "'" & graph.Node_Table(Node.Label,out(i)) & "'"
                            Next
                            MsgBox("Depth First Traversal of the given graph starting at Node '" & _
                                    graph.Node_Table(Node.Label,desiredNode) & "':" & vbCrLf & str, _
                                    MsgBoxStyle.OkOnly, "Depth First Traversal Result")
                        End If
                    Exit Select
                    '' select Node for depth first traversal
                    Case mouseState.breadthFirst:
                        dim desiredNode As Integer = graph.get_NodePtr_at(local_mouse_position)
                        If graph.Node_exists(desiredNode) then
                            Dim out() As Integer= graph.algorithm_breadthFirst(desiredNode)
                            Dim str As String = ""
                            For i As Integer = 0 to out.Length -1
                                str += "'" & graph.Node_Table(Node.Label,out(i)) & "'"
                            Next
                            MsgBox("Breadth First Traversal of the given graph starting at Node '" & _
                                    graph.Node_Table(Node.Label,desiredNode) & "':" & vbCrLf & str, _
                                    MsgBoxStyle.OkOnly, "Breadth First Traversal Result")
                        End If
                    Exit Select
                End Select
            End If
        End If
        ''
        '' the user is holding down their left mouse button
        ''
        If (mouse_held) Then
            '' check if the window is being resized
            If (window_resizeUp Or window_resizeDown Or window_resizeLeft oR window_resizeRight) Then
                If (window_resizeUp) Then
                    '' move position and resize height
                    Dim tempHeight As Integer = Me.Height
                    Dim tempY As Integer = Cursor.Position.Y
                    tempHeight += Me.Location.Y-Cursor.Position.Y ' old location minus new location
                    '' check if window width is smaller than the minimum
                    If tempHeight < MINHEIGHT Then
                        tempY = Me.Bottom-MINHEIGHT
                        tempHeight = MINHEIGHT
                    End If
                    Me.Height = tempHeight
                    Me.Top = tempY
                End If
                If (window_resizeDown) Then
                    '' resize height
                    Dim tempHeight As Integer = Me.Height
                    tempHeight = mouse_position.y
                    '' make sure the window stays greater than the minimum height
                    If tempHeight < MINHEIGHT Then tempHeight = MINHEIGHT
                    Me.Height = tempHeight
                End If
                If (window_resizeLeft) Then
                    '' move position and resize width
                    Dim tempWidth As Integer = Me.Width
                    Dim tempX As Integer = Cursor.Position.X
                    tempWidth += Me.Location.X-Cursor.Position.X ' old location minus new location
                    '' check if window width is smaller than the minimum
                    If tempWidth < MINWIDTH Then
                        tempX = Me.Right-MINWIDTH
                        tempWidth = MINWIDTH
                    End If
                    Me.Width = tempWidth
                    Me.Left = tempX
                End If
                If (window_resizeRight) Then
                    '' resize width
                    Dim tempWidth As Integer = Me.Width
                    tempWidth = mouse_position.x
                    '' make sure the window stays greater than the minimum width
                    If tempWidth < MINWIDTH Then tempWidth = MINWIDTH
                    Me.Width = tempWidth
                End If
            '' if not being resized, then check if it's being moved by the user
            ElseIf (window_beingDragged) Then
                '' if the window is maximised, check if the user is dragging the window down
                If (Me.WindowState = FormWindowState.Maximized) And (Cursor.Position.Y > 50) Then
                    Me.WindowState = FormWindowState.Normal
                    window_dragOffset = New Point(Me.Width*0.5,10) ' centre of form
                End If
                '' snap the window to the user's mouse
                Me.location = New Point(Cursor.Position.x-window_dragOffset.x, _
                                        Cursor.Position.Y-window_dragOffset.Y)
            '' finally, if the user is doing neither, check if they are dragging a Node
            ElseIf (mouse_state = mouseState.none) then
                '' drag Node
                If graph.Node_exists(selectedNode) then _
                    graph.Node_Table(Node.Position,selectedNode) = local_mouse_position
            End If
        Else If (Not mouse_held) Then
            '' if the user drags the window to the top of the screen, maximise it
            If (Me.WindowState = FormWindowState.Normal) And _
               (window_beingDragged) And (Cursor.Position.Y < 30) Then _
                Me.WindowState = FormWindowState.Maximized
            '' stop dragging the window
            window_beingDragged = False
            '' stop resizing the window
            window_resizeUp = False
            window_resizeDown = False
            window_resizeLeft = False
            window_resizeRight = False
            If (mouse_state = mouseState.none) then
                '' once the user stops holding their left mouse button, remove the pointer
                selectedNode = -1
            End If
        End If
        '' set the cursor image when resizing - the following bounding box collisions are the same as what was
        '' used to obtain the window_resize* flags. However, I require them to be repeated here because the
        '' cursor must display the re-size arrows EVEN IF THE USER IS NOT CLICKING THEIR MOUSE BUTTON!
        Dim bitWise As Byte = if(window_resizeUp Or collision_boundingBox(New Rectangle(0,0,Me.Width,5), mouse_position),1,0) Or _
                              if(window_resizeDown Or collision_boundingBox(New Rectangle(0,Me.Height-5,Me.Width,5), mouse_position),2,0) Or _
                              if(window_resizeLeft Or collision_boundingBox(New Rectangle(0,0,5,Me.Height), mouse_position),4,0) Or _
                              if(window_resizeRight Or collision_boundingBox(New Rectangle(Me.width-5,0,5,Me.Height), mouse_position),8,0)
        '' use the bitwise number to dtermine which cursor to use
        ''
        '' up = 1: down = 2: left = 4: right = 8
        '' NORTH = up         = 1
        '' SOUTH = down       = 2
        '' WEST  = left       = 4
        '' EAST  = right      = 8
        '' NE    = up+right   = 1+8 = 9
        '' SE    = down+right = 2+8 = 10
        '' SW    = down+left  = 2+4 = 12
        '' NW    = up+left    = 1+4 = 5
        ''
        '' thus these can be plugged into the switch statement to
        '' easily switch colliding states, instead of using a large
        '' nested if conditional tree
        Select Case bitWise
            Case 1,2: Me.Cursor = Cursors.SizeNS ' North or South
            Case 4,8: Me.Cursor = Cursors.SizeWE ' East or West
            Case 9,12: Me.Cursor = Cursors.SizeNESW ' NorthEast or SouthWest
            Case 5,10: Me.Cursor = Cursors.SizeNWSE ' NorthWest or SouthEast
            Case Else: Me.Cursor = Cursors.Default ' no valid case
        End Select
        '' update draw event
        me.Invalidate()
    End Sub

    '' set up the string alignment options, and the font to be used
    Dim FormatAlign2Centre As StringFormat = New StringFormat() with _
    { _
        .Alignment = StringAlignment.Center, _
        .LineAlignment = StringAlignment.Center _
    }
    Dim formatFont As Font = SystemFonts.DefaultFont

    '' private procedure for quickly drawing all the buttons within a toolbar
    Private Sub drawToolbar(ByRef toolbar As button(), ByRef e As PaintEventArgs)
        '' iterate through every button within the toolbar and draw it
        Dim col As Integer
        Dim r,g,b As Byte
        Dim drawColour As Color
        Dim drawBrush As SolidBrush
        For each btn As button In toolbar
            With btn
                '' if the user is hovering over the button, use the secondary colour
                '' otherwise use the primary colour
                If collision_boundingBox(New Rectangle(.x,.y,.width,.height), mouse_position) _
                   And (Not mouse_pressed) Then col = .secondaryColour Else col = .primaryColour
                '' split the button colour into separate channels
                r = (col And &H0000FF)
                g = (col And &H00FF00) >> 8
                b = (col And &HFF0000) >> 16
                '' create the colour and add it to a brush
                drawColour = Color.FromArgb(r,g,b)
                drawBrush = new SolidBrush(drawColour)
                e.Graphics.FillRectangle(drawBrush,.x,.y,.width,.height)
                e.Graphics.DrawString(.text, formatFont, Brushes.Black, _
                                      (.x+.x+.width)*0.5,(.y+.y+.height)*0.5,formatAlign2Centre)
            End With
            '' dispose brush
            drawBrush.Dispose()
        Next
    end Sub

    '' draw the user interface
    Private Sub Application_Paint(sender As Object, e As PaintEventArgs) Handles Me.Paint
        '' clear the draw buffers
        e.Graphics.Clear(Color.White)
        '' initialise drawing variables
        Dim col As Integer
        Dim r, g, b As Byte
        Dim drawBrush As SolidBrush ' used for drawing solid polygons
        Dim drawPen As Pen          ' used for drawing lines
        Dim drawColour As Color
        With graph
            '' draw Edges
            Dim nodePosition1, nodePosition2, centre As Point
            Const ARROW_SHARPNESS As Single = Math.PI*0.75
            Const RIGHTANGLE As Single = Math.PI*0.5
            Dim o, a As Integer ' opposite and adjacent
            Dim bearingBetweenNodes, arrowTipX, arrowTipY As Single
            Dim arrowVerticies() as PointF
            For edgePtr As Integer = 0 to .Edge_Table.GetLength(1) -1
                If .Edge_exists(edgePtr) Then
                    '' split the colour of the Edge into 3 separate channels
                    '' using the bitwise "AND" operator
                    col = .Edge_Table(Edge.Colour, edgePtr)
                    r = (col And &H0000FF)
                    g = (col And &H00FF00) >> 8
                    b = (col And &HFF0000) >> 16
                    '' generate the colour and add it to the drawing pen
                    '' and brush
                    drawColour = Color.FromArgb(r,g,b)
                    drawPen    = New Pen(drawColour)
                    drawBrush  = New SolidBrush(drawColour)
                    '' add weight to the pen
                    drawPen.Width = 2
                    '' obtain the positions of the head and tail Nodes
                    nodePosition1 = .Node_Table(Node.Position, .Edge_Table(Edge.Tail_Node, edgePtr))
                    nodePosition2 = .Node_Table(Node.Position, .Edge_Table(Edge.Head_Node, edgePtr))
                    '' apply the view_offset to the obtained Node positions
                    nodePosition1 = New Point(nodePosition1.X +view_offset.X, nodePosition1.Y +view_offset.Y)
                    nodePosition2 = New Point(nodePosition2.X +view_offset.X, nodePosition2.Y +view_offset.Y)
                    '' obtain the centre of the Edge (this is where the labels will be displayed)
                    centre = New Point( (nodePosition1.X + nodePosition2.X)*0.5, _
                                        (nodePosition1.Y + nodePosition2.Y)*0.5)
                    '' draw the Edge
                    e.Graphics.DrawLine(drawPen,nodePosition1,nodePosition2)
                    '' draw the label and weight of said Edge
                    e.Graphics.DrawString(.Edge_Table(Edge.Label, edgePtr), formatFont, Brushes.Black, centre.X, centre.Y-10, formatAlign2Centre)
                    e.Graphics.DrawString(.Edge_Table(Edge.Weight, edgePtr), formatFont, Brushes.Black, centre.X, centre.Y+10, formatAlign2Centre)
                    '' calculate the verticies of the arrow appended to the tip of the Edge
                    o = nodePosition2.Y - nodePosition1.Y ' opposite length
                    a = nodePosition2.X - nodePosition1.X ' adjacent length
                    bearingBetweenNodes = RIGHTANGLE - Math.Atan2(o,a) ' RIGHTANGLE used to offset the result of tan^-1(o/a)
                    arrowTipX = nodePosition2.X - Math.Sin(bearingBetweenNodes) * .Node_Table(Node.Size, .Edge_Table(Edge.Head_Node, edgePtr)) * 0.5
                    arrowTipY = nodePosition2.Y - Math.Cos(bearingBetweenNodes) * .Node_Table(Node.Size, .Edge_Table(Edge.Head_Node, edgePtr)) * 0.5
                    arrowVerticies = New PointF() _
                    { _
                        New PointF(arrowTipX,arrowTipY), _
                        New PointF(arrowTipX + Math.Sin(bearingBetweenNodes+ARROW_SHARPNESS)*10, arrowTipY + Math.Cos(bearingBetweenNodes+ARROW_SHARPNESS)*10 ), _
                        New PointF(arrowTipX + Math.Sin(bearingBetweenNodes-ARROW_SHARPNESS)*10, arrowTipY + Math.Cos(bearingBetweenNodes-ARROW_SHARPNESS)*10 ) _
                    }
                    '' draw the arrow
                    e.Graphics.FillPolygon(drawBrush,arrowVerticies)
                    '' dispose of pen and brush
                    drawBrush.Dispose()
                    drawPen.Dispose()
                End If
            Next
            '' draw Nodes
            Dim NodeSize As UShort
            Dim position As Point
            For nodePtr As Integer = 0 to .Node_Table.GetLength(1) -1
                If .Node_exists(nodePtr) Then
                    '' split the Node colour into separate channels
                    col = .Node_Table(Node.Colour, nodePtr)
                    r = (col And &H0000FF)
                    g = (col And &H00FF00) >> 8
                    b = (col And &HFF0000) >> 16
                    '' create the colour and add it to a brush
                    drawColour = Color.FromArgb(r,g,b)
                    drawBrush = new SolidBrush(drawColour)
                    '' obtain the Node's radius (Size)
                    NodeSize = .Node_Table(Node.Size, nodePtr)*0.5
                    '' obtain the Node position (and apply the view_offset)
                    position = New Point(.Node_Table(Node.Position, nodePtr).X +view_offset.X, _
                                         .Node_Table(Node.Position, nodePtr).Y +view_offset.Y)
                    '' draw the Node
                    e.Graphics.FillEllipse(drawBrush, position.x-NodeSize,position.Y-NodeSize, NodeSize*2, NodeSize*2)
                    '' draw the Node's label
                    e.Graphics.DrawString(.Node_Table(Node.Label, nodePtr), formatFont, Brushes.Black, _
                                   position.X, position.Y-NodeSize-10, formatAlign2Centre)
                    '' dispose of the brush
                    drawBrush.Dispose()
                End If
            Next
        End With
        ''
        '' draw the user interface
        ''
        '' draw the toolbar boxes
        Dim drawBoxUsingHex = Sub(byval colour As Integer, ByVal rect As Rectangle, _
                                  ByRef _e As PaintEventArgs)
                                  r = (colour And &H0000FF)
                                  g = (colour And &H00FF00) >> 8
                                  b = (colour And &HFF0000) >> 16
                                  Dim br As SolidBrush = new SolidBrush(Color.FromArgb(r,g,b))
                                  _e.Graphics.FillRectangle(br,rect)
                                  br.Dispose()
                              End Sub
        '' yellow top bar
        drawBoxUsingHex(BRIGHTYELLOW,new Rectangle(0,0,Me.Width,30),e)
        '' main toolbar light-grey area
        drawBoxUsingHex(DIRTYWHITE,new Rectangle(0,30,Me.Width,25),e)
        '' separating dark-grey area
        drawBoxUsingHex(GREY,New Rectangle(0,55,Me.Width,20),e)
        '' left toolbar box
        drawBoxUsingHex(LIGHTGREY,New Rectangle(0,55,100,Me.Height-55),e)
        '' bottom, blue, toolbar box
        drawBoxUsingHex(LIGHTBLUE,New Rectangle(0,Me.Height-30,Me.Width,30),e)
        '' draw the number of Nodes/ number of Edges
        e.Graphics.DrawString("Node Number: " & graph.NodeNumb & vbTab & "Edge Number: " & graph.EdgeNumb & vbTab & vbTab & _
                              "X: " & local_mouse_position.x & vbTab & "Y: " & local_mouse_position.y, formatFont, Brushes.Black,5,Me.Height-20)
        '' draw the title
        e.Graphics.DrawString(if (workingfile = "", "New Graph", workingfile) & " - Dream Diagram", formatFont, Brushes.Black,(210 + Me.Width-75)*0.5,15,FormatAlign2Centre)
        '' draw toolbars
        drawToolbar(toolbar_left,e)
        Select Case toolbar_selection
            Case toolbars.file: drawToolbar(toolbar_file,e)
            Case toolbars.graph: drawToolbar(toolbar_graph,e)
            Case toolbars.insert: drawToolbar(toolbar_insert,e)
            Case toolbars.analysis: drawToolbar(toolbar_analysis,e)
        End Select
        drawToolbar(toolbar_tabs,e)
        '' draw a box around the whole application
        e.Graphics.DrawRectangle(Pens.HotPink,0,0,Me.Width-1,Me.height-1)
    End Sub

End Class