Public Class AdjacencyMatrix
    
    Private Sub AdjacencyMatrix_Load(sender As Object,  e As EventArgs) Handles MyBase.Load
        '' set from style
        Me.FormBorderStyle = FormBorderStyle.FixedToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.ShowInTaskbar = False
        Me.ShowIcon = False
        Me.Location = New Point(DevInterface.Location.X+100,DevInterface.Location.Y+55)
        '' dynamically set the dimensions of the form
        Me.Width = 20+(DevInterface.graph.NodeNumb+1) * 50
        Me.Height = 10+(DevInterface.graph.NodeNumb+3) * 20
    End Sub

    Dim alignToCentre As StringFormat = New StringFormat() with _
        {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center}

    Private Sub AdjacencyMatrix_Paint(sender As Object, e As PaintEventArgs) Handles Me.Paint
        
        '' dynamically update the dimensions of the form
        Me.Width = 20+(DevInterface.graph.NodeNumb+1) * 50
        Me.Height = 10+(DevInterface.graph.NodeNumb+3) * 20
        '' draw the background
        e.Graphics.FillRectangle(Brushes.LightGray,0,0,50,Me.Height)
        e.Graphics.FillRectangle(Brushes.LightGray,0,0,Me.Width,30)
        '' add the Adjacency Matrix in text format
        Dim outstring As String
        Dim count As Integer = 1
        Dim count2 As Integer = 1
        With DevInterface

            For i As Integer = 0 to .graph.Node_Table.GetLength(1) -1
                If .graph.Node_exists(i) then   
                    outstring = .graph.Node_Table(Node.Label,i)
                    outstring = If(outstring.Length >= 7, outstring.Substring(0,5) & "...", outstring)
                    e.Graphics.DrawString(outstring,SystemFonts.DefaultFont, Brushes.Black,50*count,10)
                    e.Graphics.DrawString(outstring,SystemFonts.DefaultFont, Brushes.Black,0,10+20*count)
                    e.Graphics.DrawLine(Pens.DarkGray,50*count,0,50*count,Me.Height)
                    e.Graphics.DrawLine(Pens.DarkGray,0,10+20*count,Me.Width,10+20*count)

                    '' get relationship
                    For k As Integer = 0 To .graph.Node_Table.GetLength(1) -1
                        If (k = i) Then
                            '' draw a cross
                            e.Graphics.DrawLine(Pens.DarkGray,50*count,10+20*count2,50*(count+1),10+20*(count2+1))
                            count2 +=1
                        Else
                            If .graph.Node_exists(k) Then
                                If .graph.Edge_exists(i,k) Then outstring = 1 Else outstring = 0
                                e.Graphics.DrawString(outstring,SystemFonts.DefaultFont, Brushes.Black,50*count,10+20*count2)
                                count2 +=1
                            End If
                        End If
                    Next
                    count2 = 1
                    count+=1
                End If
            Next
        End With
    End Sub
End Class