Imports System.IO.StreamWriter ' for use with file load procedures
Imports System.IO.StreamReader

'' create Enumerated constants which correspond the each attributes'
'' position within its Table
Public Enum Node
    Label    = 0 : Size   = 1
    Position = 2 : Colour = 3
    Branches = 4 : Active = 5
End Enum
Public Enum Edge
    Label     = 0 : Weight = 1
    Colour    = 2 : Head_Node = 3
    Tail_Node = 4 : Active = 5
End Enum

'' Graph Structure
Public Class Graph_Struct
    
    '' create constant for use with the Node's Branches array, to check if
    '' an Edge pointer is valid
    Public Const NOEDGE = -1

    '' create variables which store the number of Active Nodes and Edges
    Public dim NodeNumb As Integer = 0
    Public dim EdgeNumb As Integer = 0

    '' create Node and Edge arrays with 6 attributes
    '' The variant type allows me to store the Labels, Integer types, and
    '' boolean flags of each instance simultaneously within the same array
    Public Dim Node_Table(5,0) ' as variant
    Public Dim Edge_Table(5,0) ' as variant
    
    '' create the class constructor to initialise the  Node and Edge tables
    sub New()
        '' format the new graph
        initialise_new_graph()
    End sub

    '' procedure which resets the graph to it's original state
    Public Sub initialise_new_graph()
        '' redefine the size of the tables
        ReDim Preserve Node_Table(5,0)
        ReDim Preserve Edge_Table(5,0)
        '' reset the Node/Edge counters
        NodeNumb = 0
        EdgeNumb = 0
        '' fill initial Node data with Null data
        Node_Table(Node.Label,0) = ""
        Node_Table(Node.Size,0) = 0
        Node_Table(Node.Position,0) = new Point(0,0)
        Node_Table(Node.Colour,0) = 0
        Node_Table(Node.Branches,0) = New Integer() {NOEDGE}
        Node_Table(Node.Active,0) = False
        '' same for Edge
        Edge_Table(Edge.Label,0) = ""
        Edge_Table(Edge.Weight,0) = 0
        Edge_Table(Edge.Colour,0) = 0
        Edge_Table(Edge.Head_Node,0) = 0
        Edge_Table(Edge.Tail_Node,0) = 0
        Edge_Table(Edge.Active,0) = False
    End Sub
    
    '' NOTE:
    '' Most non-private functions have been replaced by sub-procedures to avoid any return errors, and
    '' such, the returnal of any IDs of Nodes and Edges created has become obsolite
    ''

    '' Function to locate a memory location within a supplied Table
    Private Function findFreeSpaceWithin(ByRef _Table(,) as Object)
        '' the "Active" attribute of both the Node table and Edge table are located at the index 5.
        '' thus _Table(5,i) will be used to check if a position of the supplied table contains a valid
        '' instance
        Dim Length As Integer = _Table.GetLength(1)
        Dim ID As Integer = Length
        Dim freeSpace As Boolean = False
        '' check table for a free position
        For i As Integer = 0 To Length -1
            '' check if the instance is inactive
            If (_Table(5,i) = False) Then
                freeSpace = True
                ID = i
                Exit For
            End If
        Next
        '' if no free space was found, increase the size of the table to account for the new data!
        If (Not freeSpace) Then ' the array is full
            '' increase the size by 1
            ReDim Preserve _Table(5,Length)
        End If
        '' output the location the data may be stored in
        Return ID
    End Function

    '' Procedure for easily adding Edge references to Nodes
    Private Sub addReference(byval NodePtr As Integer, byval EdgeID As Integer)
        '' find a free location within the Node's Branches Array
        Dim Length As Integer = Node_Table(Node.Branches,NodePtr).length
        Dim EdgeLocation As Integer = Length
        Dim freeSpace As Boolean = False
        '' check Array for a free position (NOEDGE)
        For i As Integer = 0 To Length -1
            '' loop through all the Edge pointers contained within the Node's
            '' Branches Array
            If (Node_Table(Node.Branches,NodePtr)(i) = NOEDGE) Then ' free space
                freeSpace = True
                EdgeLocation = i
                Exit For
            End If
        Next
        '' If no free space was found, increase the size of the Node's Branches array
        If (Not freeSpace) Then ReDim Preserve Node_Table(Node.Branches,NodePtr)(Length)
        '' Add the supplied EdgeID to the free location within the corresponding Node's
        '' Branches Array
        Node_Table(Node.Branches,NodePtr)(EdgeLocation) = EdgeID
    End Sub

    '' NOTE:
    '' The following procedure is the solution to the problem where Edges are remaining
    '' after the Node it connected was removed. The issue was because I was checking if
    '' the value "i" == "EdgeID", when it was required to check whether the value
    '' LOCATED AT "i" == "EdgeID"
    ''

    '' Procedure for easily removing Edge references from Nodes
    Private SUb removeReference(byval NodePtr as Integer, byval EdgeID as Integer)
        '' iterate through all indexes of the supplied Node's Branches array
        Dim Length as Integer = Node_Table(Node.Branches,NodePtr).length
        for i as integer = 0 to Length -1
            '' check if the pointer located at the current index 
            '' (i) of the Branches array corresponds the the Edge ID
            '' supplied. If it does, then the Edge would have been found!
            '' Thus, set it's value to "NOEDGE" to purge it from existance
            if (Node_Table(Node.Branches,NodePtr)(i) = EdgeID) then
                '' Edge found. remove the Edge.
                Node_Table(Node.Branches,NodePtr)(i) = NOEDGE
                exit Sub
            End If
        Next
    End SUb

    '' Function for quickly checking a Node exists
    Public Function Node_exists(byval NodePtr As Integer) As Boolean
        '' check if the pointer falls within the bounds of it's array, and that
        '' it's Active flag is set to true
        If (NodePtr < 0) Or (NodePtr >= Node_Table.GetLength(1)) Then
            '' outside Bounds of array (negative or larger than length)
            Return False
        Else If (Node_Table(Node.Active,NodePtr) = False) then
            '' the Node is set to inactive
            Return False
        End If
        '' if the pointer passes the previous two tests, then it must point to an
        '' Active, valid Node. Thus return True
        Return True
    End Function

    '' Two Functions for quickly checking if an Edge exists
    '' #1
    Public Overloads Function Edge_exists(byval EdgePtr As Integer) As Boolean
        '' Similar technique to the Node_exists function
        If (EdgePtr < 0) Or (EdgePtr >= Edge_Table.GetLength(1)) Then
            '' outside Bounds of array (negative or larger than length)
            Return False
        Else If (Edge_Table(Edge.Active,EdgePtr) = False) then
            '' the Edge is set to inactive
            Return False
        End If
        '' Edge exists
        Return True
    End Function
    '' #2
    Public Overloads Function Edge_exists(
                                         byval NodePtrHead As Integer, _
                                         byval NodePtrTail As Integer
                                         ) As Boolean
        '' Use two Node pointers to check if an Edge branches between them
        Dim Length As Integer = Edge_Table.GetLength(1)
        '' iterate through all Edges
        For i As Integer = 0 To Length -1
            '' check the current Edge is active
            If (Edge_Table(Edge.Active,i) = True) Then
                '' Compare head and tail Nodes, if they are equal to the supplied
                '' Node pointers, then it is confirmed that there is an Edge connecting
                '' these Two Nodes
                If (Edge_Table(Edge.Head_Node,i) = NodePtrHead) And _
                   (Edge_Table(Edge.Tail_Node,i) = NodePtrTail) Then
                    '' Edge branches between the two supplied Nodes
                    Return True
                    Exit Function
                End If
            End If
        Next
        '' if the function continued without exiting, that means no Edge was located
        '' between the two supplied Nodes!
        Return False
    End Function

    '' Create Node Procedure
    Public Sub create_Node(
                          ByVal Position As Point, _
                          Optional ByVal Label As String = "New Node", _
                          Optional ByVal Size As UInt16 = 50, _
                          Optional ByVal Colour As Integer = &HFF8F73
                          )
        '' get a valid ID
        Dim ID As Integer = findFreeSpaceWithin(Node_Table)
        '' add the attributes to this location
        Node_Table(Node.Label,ID) = Label
        Node_Table(Node.Size,ID) = Size
        Node_Table(Node.Position,ID) = Position
        Node_Table(Node.Colour,ID) = Colour
        '' initialise the new Node's branches array
        Node_Table(Node.Branches,ID) = New Integer() {NOEDGE}
        '' activate the Node!
        Node_Table(Node.Active,ID) = True
        '' increment the Node counter
        NodeNumb += 1
    End Sub

    '' Create Edge Procedure
    Public Sub create_Edge(
                          ByVal Node_Head As Integer, _
                          ByVal Node_Tail As Integer, _
                          Optional ByVal Label As String = "New Edge", _
                          Optional ByVal Weight As UInt16 = 10, _
                          Optional ByVal Colour As Integer = 0
                          )
        '' check the Head and Tail Nodes are valid
        If (Node_exists(Node_Head) And Node_exists(Node_Tail)) Then ' both Nodes are active
            '' check whether an Edge already branches between these two Nodes
            if (Edge_exists(Node_Head,Node_Tail)) Then
                '' an Edge is already at this location, so do NOT create an overlapping
                '' Edge. Just exit the Sub-Procedure
                exit Sub
            End If
            '' get a valid ID
            Dim ID as Integer = findFreeSpaceWithin(Edge_Table)
            '' add references to self within the Node tables
            addReference(Node_Head,ID)
            addReference(Node_Tail,ID)
            '' add attributes to the newly created Edge
            Edge_Table(Edge.Label,ID) = Label
            Edge_Table(Edge.Weight,ID) = Weight
            Edge_Table(Edge.Colour,ID) = Colour
            Edge_Table(Edge.Head_Node,ID) = Node_Head
            Edge_Table(Edge.Tail_Node,ID) = Node_Tail
            '' activate the Edge!
            Edge_Table(Edge.Active,ID) = True
            '' increment the Edge counter
            EdgeNumb += 1
        Else
            '' one of the supplied Nodes does not exist!
            exit Sub
        End If
    End Sub

    '' Remove Node Procedure
    Public Sub Remove_Node(ByVal NodePtr as Integer)
        '' check the Node exists
        if (Node_exists(NodePtr)) then
            '' the supplied Node exists
            '' delete all the connected Edges
            for Each EdgeID in Node_Table(Node.Branches,NodePtr)
                Remove_Edge(EdgeID)
            Next
            '' delete self (by setting oneself to inactive)
            Node_Table(Node.Active,NodePtr) = False
            '' decrement the Node counter
            NodeNumb -= 1
        End If
        '' if the Node doesn't exist, it need not be deleted
    End Sub

    '' Remove Edge Procedure
    public Sub Remove_Edge(byval EdgePtr as Integer)
        '' check the Edge exists
        if (Edge_exists(EdgePtr)) then
            '' the supplied Edge exists
            '' remove the reference from the Head and Tail Nodes
            removeReference(Edge_Table(Edge.Head_Node,EdgePtr),EdgePtr)
            removeReference(Edge_Table(Edge.Tail_Node,EdgePtr),EdgePtr)
            '' delete self (by setting oneself to inactive)
            Edge_Table(Edge.Active,EdgePtr) = False
            '' decrement the Edge counter
            EdgeNumb -= 1
        End If
        '' if the Edge doesn't exist, it need not be deleted
    End Sub

    ''
    '' Various procedures to check whether an Edge or Node is located at a given position
    ''

    '' circular collision for Nodes
    Public Function get_NodePtr_at(ByVal position as point) as Integer
        '' find the Node which is colliding at the given position
        dim distanceFromNode as Integer
        '' iterate through all Nodes to locate the Node at the position
        dim Length as integer = Node_Table.GetLength(1)
        for NodeID as integer = 0 to Length -1
            '' if the current Node is active
            if (Node_Table(Node.Active,NodeID)) then
                with Node_Table(Node.Position,NodeID)
                    '' use trigonometry to calculate the distance to the Node
                    distanceFromNode = ( (position.x - .x)^2 + (position.y - .y)^2 )^0.5
                End With
                '' if the distance from the Node is smaller than said Node's radius,
                '' then it is within the bounds of the Node. Thus , a collision has occured
                if (distanceFromNode < Node_Table(Node.Size,NodeID)*0.5) then return NodeID
            End If
        Next
        '' if no Node was located, return a flag
        return -1
    End Function

    '' line-based collision for Edges
    Public Function get_EdgePtr_at(ByVal position as Point) as Integer
        '' constants (Infinite Gradient, 0 Gradient)
        Const ZERO as Integer = 0
        const UNDEFINED = 1/0
        '' variables
        dim lineGradient as Single
        dim x1 as Integer = position.x - 5
        dim y1 as Integer = position.y - 5
        dim x2 as Integer = position.x + 5
        dim y2 as Integer = position.y + 5
        dim NodePosition1, NodePosition2 as Point ' positions of the Two Nodes
        dim lineX, lineY as Integer               ' calculated position of the Line
        '' check whether the cursor is within the area of the Line
        dim Length as integer = Edge_Table.GetLength(1)
        for EdgeID as Integer = 0 to Length -1
            '' check the Edge is active
            If Edge_Table(Edge.Active,EdgeID) then
                '' calculate the gradient of the Edge (dy/dx)
                NodePosition1 = Node_Table(Node.Position, Edge_Table(Edge.Tail_Node,EdgeID))
                NodePosition2 = Node_Table(Node.Position, Edge_Table(Edge.Head_Node,EdgeID))
                lineGradient = (NodePosition2.Y-NodePosition1.Y)/(NodePosition2.X-NodePosition1.X)
                '' avoid an infinite gradient
                if (lineGradient <> UNDEFINED) then
                    '' check the top of the supplied position
                    lineY = lineGradient * (x1 - NodePosition1.x) + NodePosition1.y
                    if (lineY > y1) and (liney < y2) then return EdgeID
                    '' check the bottom of the supplied position
                    lineY = lineGradient * (x2 - NodePosition1.x) + NodePosition1.y
                    if (lineY > y1) and (liney < y2) then return EdgeID
                End If
                '' avoid divide by zero error
                if (lineGradient <> ZERO) then
                    '' check the left of the supplied position
                    lineX = (y1 - NodePosition1.y) / lineGradient + NodePosition1.x
                    if (lineY > x1) and (liney < x2) then return EdgeID
                    '' check the right of the supplied position
                    lineX = (y2 - NodePosition1.y) / lineGradient + NodePosition1.x
                    if (lineY > x1) and (liney < x2) then return EdgeID
                End If
            End If
        Next
        '' if no Edge was located, return a flag
        return -1
    End Function

    ''
    '' Various tarversal algorithms
    ''

    '' shortest path algorithm
    Public Function algorithm_Dijkstras(ByVal startingNodePtr As Integer, _
                                        ByVal targetNodePtr As Integer) As Integer()
        '' check if the pointers are equal
        If (startingNodePtr = targetNodePtr) then Return new Integer() {startingNodePtr}
        '' initialise variables
        Dim currentNodePtr As Integer = startingNodePtr
        Dim nextNodePtr As Integer
        Dim highestPriorityNode As Integer
        Dim smallestEdgeWeight As Integer
        Dim newDistance As Integer
        '' declare extra attributes
        Dim visited                 (Node_Table.GetLength(1) -1) As Boolean
        Dim rootNode                (Node_Table.GetLength(1) -1) As Integer
        Dim distanceFromStartingNode(Node_Table.GetLength(1) -1) As Integer
        '' initialise the arrays
        Const INFINITY As Integer = 2147483647 ' largest 32-bit integer value
        For i As Integer = 0 To Node_Table.GetLength(1) -1
            rootNode(i) = -1 ' flag which says the root Node is undefined
            distanceFromStartingNode(i) = INFINITY
        Next
        distanceFromStartingNode(currentNodePtr) = 0 ' starting Node is 0 units from itself
        ''1 iterate through all the connecting Nodes of the current Node
        ''2 compare their distance to their current
        ''3 if smaller than current, update rootNOde(i) with current Node pointer
        ''4 otherwise, do nothing
        ''5 continue until all have been checked
        ''6 move onto the NOde with the smallest relative distance
        ''7 repeat until dead end
        ''8 backtrack, checking if there are any unvisited Nodes along the way, until
        ''  the root node is reached (startingNodePtr)
        ''8.5 if there are any unvisited Nodes which branch off the current node whilst
        ''    back tracking, then traverse them as normal
        Do
            highestPriorityNode = -1
            smallestEdgeWeight = INFINITY
            '' visit the current Node
            visited(currentNodePtr) = True
            '' loop through all the current Node's connecting Edges
            For each edgePtr As Integer In Node_Table(Node.Branches,currentNodePtr)
                '' check if the pointer is valid
                If Edge_exists(edgePtr) Then
                    '' locate the connecting Node
                    nextNodePtr = Edge_Table(Edge.Head_Node,edgePtr)
                    '' check the connecting Node has is directed away from the
                    '' current Node
                    If (nextNodePtr <> currentNodePtr) Then
                        '' check if the Node has been visited
                        If Not visited(nextNodePtr) Then
                            '' calculate its distance from the starting Node
                            newDistance = Edge_Table(Edge.Weight,edgePtr) _
                                + distanceFromStartingNode(currentNodePtr)
                            '' compare the new distance with the current shortest path
                            If (newDistance < distanceFromStartingNode(nextNodePtr)) Then
                                '' a shorter path was found! thus, update its root Node
                                '' pointer, as its distance from the starting Node
                                rootNode(nextNodePtr) = currentNodePtr
                                distanceFromStartingNode(nextNodePtr) = newDistance
                            End If
                            '' obtain the Node with the smallest relative distance
                            If (Edge_Table(Edge.Weight,EdgePtr) < smallestEdgeWeight) Then
                                '' a movement with a smaller cost exists
                                highestPriorityNode = nextNodePtr
                                smallestEdgeWeight = Edge_Table(Edge.Weight,edgePtr)
                            End If
                        End If
                    End If
                End If
                '' continue until all Edges have been checked
            Next
            '' move to the Node with the smallest relative weight
            If (highestPriorityNode <> -1) Then
                currentNodePtr = highestPriorityNode
            Else
                '' a dead end has been reached! thus, step back a Node
                currentNodePtr = rootNode(currentNodePtr)
            End If
        Loop Until (currentNodePtr = -1) ' the starting Node has been reached
        '' compile the output
        Dim outputNode As Integer = targetNodePtr
        Dim flipRoute As Stack = New Stack()
        flipRoute.Push(outputNode)
        Do
            outputNode = rootNode(outputNode)
            If (outputNode = -1) Then Return New Integer() {-1} ' unreachable
            flipRoute.Push(outputNode)
        Loop Until outputNode = startingNodePtr
        '' add data to the output in reverse
        Dim output(flipRoute.Count -1) as Integer
        For i As Integer = 0 To flipRoute.Count -1
            output(i) = flipRoute.Pop()
        Next
        Return output
    End Function

    '' depth first traversal algorithm
    Public Function algorithm_depthFirst(ByVal startingNodePtr) As Integer()
        '' initialise variables
        Dim nodeStack As Stack = New Stack()
        Dim currentNodePtr As Integer = startingNodePtr
        Dim nextNodePtr As Integer
        Dim highestPriorityNode As Integer = 0
        Dim outputStr As String = startingNodePtr.ToString
        Dim visited(Node_Table.GetLength(1) -1) As Boolean
        '' start by adding the first Node pointer to the stack
        nodeStack.Push(currentNodePtr)
        '' mark the first Node as visited
        visited(currentNodePtr) = True 
        Do
            '' obtain the top item in the stack
            currentNodePtr = nodeStack.Peek()
            highestPriorityNode = -1
            '' iterate through all the current Node's connecting Edges
            For Each edgePtr As Integer In Node_Table(Node.Branches,currentNodePtr)
                '' make sure the current Edge is valid
                If Edge_exists(edgePtr) then
                    '' locate the connecting Node (next Node pointer)
                    nextNodePtr = Edge_Table(Edge.Head_Node,edgePtr)
                    '' if the next Node pointer is equal to the current Node pointer,
                    '' then that means the Edge is directed towards the current Node.
                    '' Thus, the Node which is required by the traversal algorithm is 
                    '' the Tail Node
                    If (nextNodePtr = currentNodePtr) then ' head_Node = currentNode
                        '' swap head_Node with tail_Node; this prevents loop-backs
                        nextNodePtr = Edge_Table(Edge.Tail_Node,edgePtr)
                    End If
                    '' check the next Node has not been visited
                    If (Not visited(nextNodePtr)) then
                        '' use this new Node and check if it's priority is greater than the
                        '' highest priority Node with -1 being an exception where the next
                        '' node simply overwrites this value
                        If (highestPriorityNode = -1) then
                            '' no Node has been previously set
                            highestPriorityNode = nextNodePtr
                        '' priority is determined by which label is alphabetically first
                        Else If (Node_Table(Node.Label,nextNodePtr) < _
                                 Node_Table(Node.Label,highestPriorityNode)) then
                            '' the next Node has a higher priority. Thus, overwrite
                            highestPriorityNode = nextNodePtr
                        End If
                    End If ' else the Node has already been visited, so ignore it!
                End If
            Next
            '' once finished iterating through all the Edges, add the Node ID with
            '' the highest priority to the stack (highestPriorityNode). If no Edges 
            '' were located, or all Nodes have already been visited (this is when
            '' highestPriorityNode does not change it's value - it stays at -1!),
            '' then pop the top item
            If (highestPriorityNode <> -1) then
                nodeStack.Push(highestPriorityNode) ' push the Node into the stack
                visited(highestPriorityNode) = True ' mark the Node as visited
                '' add the Node to the output. The output is stored as a string
                '' until the end, from which the string will be split into an array
                '' at the comma character ","c
                outputStr += "," & highestPriorityNode.ToString
            Else
                nodeStack.Pop() ' remove the Node from the stack
            End If
        Loop Until nodeStack.Count = 0 ' stack is empty
        '' obtain the output
        Dim stringToInt = Function(ByRef [string]() As String)
                              '' use lambda function to transform a string array
                              '' into an integer array
                              Dim out([string].Length-1) As Integer
                              For i As Integer = 0 to [string].Length -1
                                  out(i) = Int([string](i)) : Next
                              Return out
                          End Function
        Return stringToInt(outputStr.Split(","c))
    End Function

    '' breadth first traversal algorithm
    Public Function algorithm_breadthFirst(ByVal startingNodePtr As Integer) As Integer()
        '' initialise variables
        Dim nodeQueue As Queue = New Queue()
        Dim currentNodePtr As Integer = startingNodePtr
        Dim nextNodePtr As Integer
        Dim highestPriorityNode As Integer = 0
        Dim outputStr As String = startingNodePtr.ToString
        Dim visited(Node_Table.GetLength(1) -1) As Boolean
        '' start by adding the first Node pointer to the queue
        nodeQueue.Enqueue(currentNodePtr)
        '' mark the first Node as visited
        visited(currentNodePtr) = True
        Do
            '' obtain the last item of the queue
            currentNodePtr = nodeQueue.Dequeue()
            Do
                highestPriorityNode = -1
                '' itterate through all the current Node's connecting Edges
                For Each edgePtr As Integer In Node_Table(Node.Branches,currentNodePtr)
                    '' make sure the current Edge is valid
                    If Edge_exists(edgePtr) then
                        '' the following code is a copy of what was used in the depth first
                        '' traversal algorithm, but thus iterates until the current Node
                        '' (currentNodePtr) has no remaining, unvisited Nodes, fromwhich the
                        '' next Node is dequeued from the queue, and the process is repeated
                        '' until the queue is emptied
                        nextNodePtr = Edge_Table(Edge.Head_Node,edgePtr)
                        If (nextNodePtr = currentNodePtr) then _
                            nextNodePtr = Edge_Table(Edge.Tail_Node,edgePtr)
                        '' check if unvisited
                        If (Not visited(nextNodePtr)) then
                            '' obtain the highest priority Node
                            If (highestPriorityNode = -1) then
                                highestPriorityNode = nextNodePtr
                            Else If (Node_Table(Node.Label,nextNodePtr) < _
                                     Node_Table(Node.Label,highestPriorityNode)) then
                                '' the next Node has a higher priority
                                highestPriorityNode = nextNodePtr
                            End If
                        End If ' else the Node has already been visited, so ignore it!
                    End If
                Next
                '' once finished looping through all the active Edges, add the valid Node
                '' pointers to the nodeQueue item
                If (highestPriorityNode <> -1) then 
                    '' Node has been found
                    nodeQueue.Enqueue(highestPriorityNode) ' add the Node to the queue
                    visited(highestPriorityNode) = True    ' visit the Node
                    '' add the Node pointer to the output string. This will be split
                    '' at the end of the procedure, like the depth first traversal algorithm
                    outputStr += "," & highestPriorityNode.ToString
                End If
            loop While (highestPriorityNode <> -1) ' keep repeating until all Nodes are visited
        Loop Until nodeQueue.Count = 0 ' queue is empty
        '' obtain the output
        Dim stringToInt = Function(ByRef [string]() As String)
                              '' use lambda function to transform a string array
                              '' into an integer array
                              Dim out([string].Length-1) As Integer
                              For i As Integer = 0 to [string].Length -1
                                  out(i) = Int([string](i)) : Next
                              Return out
                          End Function
        Return stringToInt(outputStr.Split(","c))
    End Function

    ''
    '' file handling procedures
    ''

    '' declare arbitary encryption key
    public Const ENCRYPTION_KEY As Integer = 0 ' 96

    '' function used for encrypting and decrypting the graph data within the text file
    Public Function string_encrypt(ByVal dataString As String, _
                                    ByVal KEY As Integer) As String
        '' initialise variables
        Dim returnString As String = ""
        Dim unicodeVal As Integer
        For ch As Integer = 0 to dataString.Length -1
            '' convert the character located at ch to its unicode value
            unicodeVal = AscW(dataString(ch))
            '' shift value by the given key
            unicodeVal += KEY
            '' convert unicodeVal back into a character, and add it to the
            '' return string
            returnString += ChrW(unicodeVal)
        Next
        Return returnString
    End Function

    '' procedure used to save the data to a desired file
    Public Sub file_save(ByVal filepath As String)
        '' open the file
        Dim file As System.IO.StreamWriter
        file = My.Computer.FileSystem.OpenTextFileWriter(filepath,False)
        '' encryption key = ENCRYPTION_KEY
        '' write the encoded header to the file
        Dim encryptionResult As String = string_encrypt("---!Graph Data File!---",ENCRYPTION_KEY)
        file.WriteLine(encryptionResult)
        '' encrypt and write the number of Nodes and Edges to the second line of the file
        Dim numberOfNodes As Integer = Node_Table.GetLength(1)
        Dim numberOfEdges As Integer = Edge_Table.GetLength(1)
        encryptionResult = string_encrypt(numberOfNodes.ToString & "," & numberOfEdges.ToString, _
                                          ENCRYPTION_KEY)
        file.WriteLine(encryptionResult)
        '' add Node records
        Dim record As String
        For i As Integer = 0 To numberOfNodes -1
            '' add attributes
            record = Node_Table(Node.Label,i)       ' add label
            record += "," & Node_Table(Node.Size,i) ' add size
            record += "," & Node_Table(Node.Colour,i)     ' add colour
            record += "," & Node_Table(Node.Position,i).x ' add x
            record += "," & Node_Table(Node.Position,i).y ' add y
            record += "," & Node_Table(Node.Active,i)     ' add active state
            '' append all Edge pointers connected to the current Node
            For each edgePtr As Integer In Node_Table(Node.Branches,i)
                record += "," & edgePtr
            Next
            '' write the encoded record to the next line of the file
            encryptionResult = string_encrypt(record,ENCRYPTION_KEY)
            file.WriteLine(encryptionResult)
        Next
        '' add Node records
        REM record as string
        For i As Integer = 0 To numberOfEdges -1
            '' add attributes
            record = Edge_Table(Edge.Label,i)       ' add label
            record += "," & Edge_Table(Edge.Weight,i) ' add size
            record += "," & Edge_Table(Edge.Colour,i)     ' add colour
            record += "," & Edge_Table(Edge.Head_Node,i)  ' add head Node
            record += "," & Edge_Table(Edge.Tail_Node,i)  ' add tail Node
            record += "," & Edge_Table(Edge.Active,i)     ' add active state
            '' write the encoded record to the next line of the file
            encryptionResult = string_encrypt(record,ENCRYPTION_KEY)
            file.WriteLine(encryptionResult)
        Next
        '' close the file and dispose of the resource
        file.Close()
        file.Dispose()
    End Sub

    '' procedure used to load the data from a desired file
    Public Sub file_load(ByVal filepath As String)
        '' open the file
        Dim file As System.IO.StreamReader
        file = My.Computer.FileSystem.OpenTextFileReader(filepath)
        '' decryption key = -ENCRYPTION_KEY (NEGATIVE)
        '' check the header of the file is valid
        Dim decryptionResult As String = string_encrypt(file.ReadLine(),-ENCRYPTION_KEY)
        If (decryptionResult <> "---!Graph Data File!---") then _
            Exit Sub ' if the header does not match: exit the procedure
        '' get the number of Nodes and Edges
        decryptionResult = string_encrypt(file.ReadLine(),-ENCRYPTION_KEY)
        Dim temp() As string = decryptionResult.Split(","c)
        Dim numberOfNodes As Integer = temp(0)
        Dim numberOfEdges As Integer = temp(1)
        '' resize the tables
        ReDim Node_Table(5,numberOfNodes -1)
        ReDim Edge_Table(5,numberOfEdges -1)
        '' add Node records
        Dim record() As String
        Dim branches() As Integer
        For i As Integer = 0 To numberOfNodes -1
            '' get Node records
            decryptionResult = string_encrypt(file.ReadLine(),-ENCRYPTION_KEY)
            record = decryptionResult.Split(","c)
            Node_Table(Node.Label,i) = record(0)    ' load Label
            Node_Table(Node.Size,i) = record(1)     ' load Size
            Node_Table(Node.Colour,i) = record(2)   ' load Colour
            Node_Table(Node.Position,i) = New Point(record(3),record(4)) ' load Position
            Node_Table(Node.Active,i) = record(5)   ' load active state
            '' add all the conecting Edges
            ReDim branches(record.Length-1 -6)
            For recordIndex As Integer = 6 to record.Length -1
                '' start at record index 6; that is where the Edge Pointers begin in the file
                branches(recordIndex-6) = record(recordIndex)
            Next
            Node_Table(Node.Branches,i) = branches  ' load branches
            '' increase the number of active Nodes
            If (record(5) = True) Then NodeNumb += 1
        Next
        '' add Edge records
        REM Record() as string
        For i As Integer = 0 To numberOfEdges -1
            '' get Node records
            decryptionResult = string_encrypt(file.ReadLine(),-ENCRYPTION_KEY)
            record = decryptionResult.Split(","c)
            Edge_Table(Edge.Label,i) = record(0)    ' load Label
            Edge_Table(Edge.Weight,i) = record(1)   ' load weight
            Edge_Table(Edge.Colour,i) = record(2)   ' load Colour
            Edge_Table(Edge.Head_Node,i) = record(3)' load head Node
            Edge_Table(Edge.Tail_Node,i) = record(4)' load tail Node
            Edge_Table(Edge.Active,i) = record(5)   ' load active state
            '' increase the number of active Nodes
            If (record(5) = True) Then EdgeNumb += 1
        Next
        '' close the file and dispose the resource
        file.Close()
        file.Dispose()
    End Sub

    ''
    '' exporting procedures
    ''

    '' export as an image
    Public sub exportAs_bitmap(byval filepath As String)
        '' initialise variables
        Const INFINITY = 2147483647
        Dim leftMost As Integer = INFINITY ' x1,y1,x2,y2 positions of the drawing canvas
        Dim topMost As Integer = INFINITY
        Dim bottomMost As Integer = 0
        Dim farMost As Integer = 0
        Dim newMostPosition As Integer ' used to store the location of the current Node to
                                       ' be comapred against the current [item]Most variables
        '' iterate through all the valid Nodes to determine the bounds of the image
        Dim NodeRadius As UShort
        For NodePtr As Integer = 0 to Node_Table.GetLength(1) -1
            If Node_exists(NodePtr) then
                '' obtain the radius of the current Node
                NodeRadius = Node_Table(Node.Size,NodePtr)*0.5
                '' use the node's position to compare it with the [current]Most positions
                With Node_Table(Node.Position,NodePtr)
                    '' check leftMost - get the tangent left of the Node's circumference
                    newMostPosition = .x -NodeRadius -10 ' the "10" is padding around the Node
                    If (newMostPosition < leftMost) then leftMost = newMostPosition
                    '' check farMost  - get the tangent right of the Node's circumference
                    newMostPosition = .x +NodeRadius +10
                    If (newMostPosition > farMost) then farMost = newMostPosition
                    '' check topMost
                    newMostPosition = .y -NodeRadius -20 ' add more padding to the top for the Label
                    If (newMostPosition < topMost) then topMost = newMostPosition
                    '' check bottomMost
                    newMostPosition = .y +NodeRadius +10
                    If (newMostPosition > bottomMost) then bottomMost = newMostPosition
                end With
            End If
        Next
        '' after bounding box of the canvas is obtained, calculate the canvas offset, and said
        '' canvas' width/height
        Dim xOff As Integer = -leftMost ' xOffset
        Dim yOff As Integer = -topMost  ' yOffset
        Dim width As Integer = farMost-leftMost
        Dim height As Integer = bottomMost-topMost
        '' if the width or height is negative, then no graph exists
        If (width <= 0) Or (height <= 0) Then
            MsgBox("Please add atleast one Node to the structure" & vbCrLf & _
                   "before exporting as an image file!", MsgBoxStyle.OkOnly + _
                   MsgBoxStyle.Information, "No graph error!")
            Exit Sub
        End If
        ''
        '' build the bitmap
        ''
        Dim graphMap As Bitmap = New Drawing.Bitmap(width,height)
        '' set the resolution to 300x300 dpi
        graphMap.SetResolution(300,300)
        '' create a graphics handle for the bitmap
        Dim GFX As Graphics = Drawing.Graphics.FromImage(graphMap)
        GFX.Clear(Color.Transparent) ' clear all noise data
        '' set up the string alignment options, and the font to be used
        Dim formatAlign2Centre As StringFormat = New StringFormat() with _
        { _
            .Alignment = StringAlignment.Center, _
            .LineAlignment = StringAlignment.Center _
        }
        Dim formatFont As Font = New Font("Calibri",3)
        '' initialise drawing variables
        Dim col As Integer
        Dim r, g, b As Byte
        Dim drawBrush As SolidBrush ' used for drawing solid polygons
        Dim drawPen As Pen          ' used for drawing lines
        Dim drawColour As Color
        '' draw Edges
        Dim nodePosition1, nodePosition2, centre As Point
        Const ARROW_SHARPNESS As Single = Math.PI*0.75
        Const RIGHTANGLE As Single = Math.PI*0.5
        Dim o, a As Integer
        Dim bearingBetweenNodes, arrowTipX, arrowTipY As Single
        Dim arrowVerticies() as PointF
        For i As Integer = 0 To Edge_Table.GetLength(1) -1
            If (Edge_Table(Edge.Active, i) = true) Then
                '' colour is split into three channels using bitwise "And"
                col = Edge_Table(Edge.Colour, i)
                r = (col And &H0000FF)
                g = (col And &H00FF00) >> 8
                b = (col And &HFF0000) >> 16
                '' create the colour and add it to the pen and bush
                drawColour = Color.FromArgb(r,g,b)
                drawPen    = New Pen(drawColour) : drawPen.Width = 2 ' add weight to Edge
                drawBrush  = New SolidBrush(drawColour)
                '' obtain the positions of the head and tail Nodes
                nodePosition1 = Node_Table(Node.Position, Edge_Table(Edge.Tail_Node, i))
                nodePosition2 = Node_Table(Node.Position, Edge_Table(Edge.Head_Node, i))
                '' apply the canvas offset to these positions
                nodePosition1 = New Point(nodePosition1.X + xOff, nodePosition1.Y + yOff)
                nodePosition2 = New Point(nodePosition2.X + xOff, nodePosition2.Y + yOff)
                '' obtain the centre of the Edge (this is where the labels will be displayed)
                centre = New Point( (nodePosition1.X + nodePosition2.X)*0.5, _
                                    (nodePosition1.Y + nodePosition2.Y)*0.5)
                '' draw the Edge
                GFX.DrawLine(drawPen,nodePosition1,nodePosition2)
                '' draw the Label and weight of said Edge
                GFX.DrawString(Edge_Table(Edge.Label, i), formatFont, Brushes.Black, centre.X, centre.Y-10, formatAlign2Centre)
                GFX.DrawString(Edge_Table(Edge.Weight, i), formatFont, Brushes.Black, centre.X, centre.Y+10, formatAlign2Centre)
                '' calculate the verticies of the arrow of said Edge (which displays the direction of the Edge)
                o = nodePosition2.Y - nodePosition1.Y ' opposite length
                a = nodePosition2.X - nodePosition1.X ' adjacent length
                bearingBetweenNodes = RIGHTANGLE - Math.Atan2(o,a) ' RIGHTANGLE used to offset the result of tan^-1(o/a)
                arrowTipX = nodePosition2.X - Math.Sin(bearingBetweenNodes) * Node_Table(Node.Size, Edge_Table(Edge.Head_Node, i)) * 0.5
                arrowTipY = nodePosition2.Y - Math.Cos(bearingBetweenNodes) * Node_Table(Node.Size, Edge_Table(Edge.Head_Node, i)) * 0.5
                arrowVerticies = New PointF() _
                { _
                    New PointF(arrowTipX,arrowTipY), _
                    New PointF(arrowTipX + Math.Sin(bearingBetweenNodes+ARROW_SHARPNESS)*10, arrowTipY + Math.Cos(bearingBetweenNodes+ARROW_SHARPNESS)*10 ), _
                    New PointF(arrowTipX + Math.Sin(bearingBetweenNodes-ARROW_SHARPNESS)*10, arrowTipY + Math.Cos(bearingBetweenNodes-ARROW_SHARPNESS)*10 ) _
                }
                '' draw the arrow
                GFX.FillPolygon(drawBrush, arrowVerticies)
                '' dispose resources
                drawBrush.Dispose()
                drawPen.Dispose()
            End If
        Next
        '' draw Nodes
        REM NodeRadius As UShort
        Dim position As Point
        For i As Integer = 0 To Node_Table.GetLength(1) -1
            If (Node_Table(Node.Active, i) = true) Then
                '' colour is again, split into three channels
                col = Node_Table(Node.Colour, i)
                r = (col And &H0000FF)
                g = (col And &H00FF00) >> 8
                b = (col And &HFF0000) >> 16
                '' create the colour and add it to a brush
                drawColour = Color.FromArgb(r,g,b)
                drawBrush = new SolidBrush(drawColour)
                '' obtain the Node's radius (Size)
                NodeRadius = Node_Table(Node.Size, i)*0.5
                '' obtain the Node position (and apply the offset to said position)
                position = New Point(Node_Table(Node.Position, i).X +xOff, _
                                     Node_Table(Node.Position, i).Y +yOff)
                '' draw the Node
                GFX.FillEllipse(drawBrush, position.x-NodeRadius,position.Y-NodeRadius, NodeRadius*2, NodeRadius*2)
                '' draw the Node's label
                GFX.DrawString(Node_Table(Node.Label, i), formatFont, Brushes.Black, _
                               position.X, position.Y-NodeRadius-10, formatAlign2Centre)
                '' dispose of the brush
                drawBrush.Dispose()
            End If
        Next
        '' save the bitmap to the given file
        graphMap.Save(filepath, System.Drawing.Imaging.ImageFormat.Png)
        '' dispose resources
        graphMap.Dispose()
        GFX.Dispose()
    End sub

    '' export as a .csv
    Public Sub exportAs_CSV(byval filepath As String)
        '' initialise variables
        Dim numberOfNodes As Integer = Node_Table.GetLength(1)
        Dim outputString As String
        Dim file As System.IO.StreamWriter
        '' open the file
        file = My.Computer.FileSystem.OpenTextFileWriter(filepath,False)
        '' write all labels to the first line of the file
        outputString = ""
        For i As Integer = 0 To numberOfNodes -1
            If Node_exists(i) Then _
                outputString += "," & Node_Table(Node.Label,i)
        Next
        file.WriteLine(outputString)
        '' write the label of all the Nodes, followed by a 0 if the corresponding
        '' Node branches from the current Node
        For i As Integer = 0 To numberOfNodes -1
            '' check the current Node exists
            If Node_exists(i) Then
                '' write the label of said Node to the begining of the line
                outputString = Node_Table(Node.Label,i)
                '' for every other Node, compare whether there is an Edge between them
                For k As Integer = 0 To numberOfNodes -1
                    '' check the corresponding Node exists
                    If Node_exists(k) Then
                        '' check whether there is an Edge between these two, valid, Nodes!
                        If Edge_exists(k,i) Then
                            '' an Edge exists between these two Nodes, thus add a ",1"
                            outputString += ",1"
                        Else
                            '' no Edge
                            outputString += ",0"
                        End If
                    End If
                Next
                '' after all corresponding Nodes have been checked, write this line
                file.WriteLine(outputString)
            End If
        Next
        '' close the file, and dispose the resource
        file.Close
        file.Dispose()
    End Sub


End Class
