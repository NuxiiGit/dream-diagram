Public Class Custom_Buttons

    Public class button
        Public dim text As String
        Public dim x As Integer
        Public dim y As Integer
        Public dim width As Integer
        Public dim height As Integer
        Public dim primaryColour As Integer
        Public dim secondaryColour As Integer
        Public Delegate Sub FunctionPtr()
        Public dim functPtr As FunctionPtr
    End Class

End Class
