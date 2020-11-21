<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class DevInterface
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.mainLoop = New System.Windows.Forms.Timer(Me.components)
        Me.SuspendLayout
        '
        'mainLoop
        '
        Me.mainLoop.Interval = 10
        '
        'DevInterface
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6!, 13!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(738, 436)
        Me.Name = "DevInterface"
        Me.Text = "Graphing Program"
        Me.ResumeLayout(false)

End Sub
    Friend WithEvents mainLoop As System.Windows.Forms.Timer
End Class
