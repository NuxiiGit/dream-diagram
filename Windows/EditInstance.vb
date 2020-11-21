Public Class EditInstance
    
    Dim nn As Integer
    Dim ee As Integer

    Private Sub EditInstance_Load( sender As Object,  e As EventArgs) Handles MyBase.Load
        '' set form style
        Me.FormBorderStyle = FormBorderStyle.FixedToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.ShowInTaskbar = False
        Me.ShowIcon = False
        Me.Location = Cursor.Position
        '' use the selected Node or selected Edge to format the form
        nn = DevInterface.selectedNode
        ee = DevInterface.selectedEdge
        If (DevInterface.selectedNode <> -1) then
            '' the user is editing a Node
            With DevInterface
                out_ID.Text = .selectedNode
                out_Type.Text = "Node"
                out_Label.Text = .graph.Node_Table(Node.Label,.selectedNode)
                out_Size.Text  = .graph.Node_Table(Node.Size,.selectedNode)
                '' get colour
                Dim NodeCol As Integer = .graph.Node_Table(Node.Colour,.selectedNode)
                out_R.Text = (NodeCol And &H0000FF)
                out_G.Text = (NodeCol And &H00FF00) >> 8
                out_B.Text = (NodeCol And &HFF0000) >> 16
                '' get position
                Dim NodePos As Point = .graph.Node_Table(Node.Position,.selectedNode)
                out_x.Text = NodePos.x
                out_y.Text = NodePos.y
            End With
        Else if (DevInterface.selectedEdge <> -1) then
            '' the user is editing an Edge
            With DevInterface
                out_ID.Text = .selectedEdge
                out_Type.Text = "Edge"
                out_Label.Text = .graph.Edge_Table(Edge.Label,.selectedEdge)
                out_Size.Text  = .graph.Edge_Table(Edge.Weight,.selectedEdge)
                '' get colour
                Dim EdgeCol As Integer = .graph.Edge_Table(Edge.Colour,.selectedEdge)
                out_R.Text = (EdgeCol And &H0000FF)
                out_G.Text = (EdgeCol And &H00FF00) >> 8
                out_B.Text = (EdgeCol And &HFF0000) >> 16
                '' hide the positions
                txt_x.Hide
                txt_y.Hide
                out_x.Hide
                out_y.Hide
            End With
        Else
            Me.Close() ' unknown
        End If

    End Sub

    Private Sub btn_cancel_Click( sender As Object,  e As EventArgs) Handles btn_cancel.Click
        Me.Close() ' close window
    End Sub

    Private Sub btn_Apply_Click( sender As Object,  e As EventArgs) Handles btn_Apply.Click
        '' create lambda for checking if the data is a string
        Dim dataIsString = Function(ByVal [string] As String) As Boolean
                               Try
                                   Dim out As Integer = Int([string])
                                   Return False
                               Catch ex As InvalidCastException
                                   Return True
                               End Try
                           End Function
        '' apply the changes
        If (nn <> -1) then
            '' the user is editing a Node
            With DevInterface
                '' output label
                .graph.Node_Table(Node.Label,nn) = out_Label.Text
                '' output size
                If Not dataIsString(out_Size.Text) then 
                    If (out_Size.Text > 0) then _
                        .graph.Node_Table(Node.Size,nn) = out_Size.Text
                End If
                '' output colour
                If Not (dataIsString(out_R.Text) or dataIsString(out_G.Text) or dataIsString(out_B.Text)) then _
                    .graph.Node_Table(Node.Colour,nn) = Int(out_R.Text) or Int(out_G.Text) << 8 or Int(out_B.Text) << 16
                '' output position
                If Not (dataIsString(out_x.Text) or dataIsString(out_y.Text)) then _
                    .graph.Node_Table(Node.Position,nn) = New Point(out_x.Text,out_y.Text)
            End With
        Else if (ee <> -1) then
            '' the user is editing an Edge
            With DevInterface
                '' output label
                .graph.Edge_Table(Edge.Label,ee) = out_Label.Text
                '' output size
                If Not dataIsString(out_Size.Text) then _
                    .graph.Edge_Table(Edge.Weight,ee) = out_Size.Text
                '' output colour
                If Not (dataIsString(out_R.Text) or dataIsString(out_G.Text) or dataIsString(out_B.Text)) then _
                    .graph.Edge_Table(Edge.Colour,ee) = Int(out_R.Text) or Int(out_G.Text) << 8 or Int(out_B.Text) << 16
            End With
        End If
        Me.Close()
    End Sub

End Class