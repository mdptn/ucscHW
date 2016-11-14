#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

" hzip.st "
" A file compression utility "
" Assignment 3"
" CMPS 112 Fall 2016 "
" Megan Nguyen "
" mednguye@ucsc.edu "


"line below referenced from cmdline.st. not sure if needed yet"
execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.

"initiallize flag options"
dopt := false.
copt := false.
topt := false.
uopt := false.

"initiallize input file read variables. two since the file is read TWICE.
 numargs counts the number of arguments after opt"
|inputfiler1 inputfiler2 numargs outputfile dTree|
numargs := 0.


" referenced from catfile.st provided by Mackey-----------------------------------
  opens the file"

FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]


" referenced from charfreq.st provided by Mackey----------------------------------
  counts the number of times each char appears in the input"

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ self asString]
                   ifFalse: [^ self asInteger printString]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]


" referenced from tree.st provided by Mackey-------------------------------------"

Object subclass: Leaf [
   |char count leaf|
   char [ ^ char ]
   count [ ^ count ]
   leaf [^ leaf]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      leaf := true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor value: char value: leaf.
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      leaf := false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor[      
      left postorder: visitor.
      right postorder: visitor.  
      visitor value: char value: leaf.
   ]

]


" referenced from outbits.st provided by Mackey-----------------------------------
  writes out bits"

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

" referenced from bitstream.st provided by Mackey----------------------------------"

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]


freqtable := ZeroArray new: 257.
"at entry 256 put 1 to indicate EOF"
freqtable at:256 put:1.

encodingStrings := ZeroArray new: 257.

pqueue := SortedCollection new.


" referenced from cmdline.st------------------------------------------------------
  parses command line args"

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

   "check for the options and turn them true if detected"
   (opt = $d) ifTrue: [
      dopt := true.
   ].

   (opt = $t) ifTrue: [
      topt := true.
   ].
   
   (opt = $c) ifTrue: [
      copt := true.
   ].
   
   (opt = $u) ifTrue: [
      uopt := true.
   ].
   

   "check if arg is null. arg should be the input filename"
   (arg ~= nil) ifTrue: [

      "if 0, this is the first arg, aka the inputfile."
      numargs = 0 ifTrue: [
         "first read is to construct the decoding tree"
         inputfiler1 := FileStream open: arg
                                   mode: FileStream read.
         "second read is for compression"
         inputfiler2 := FileStream open: arg
                                   mode: FileStream read.
         dopt ifTrue: [stdout << 'input file opened' << nl.].

      ]

      "if not the first arg, then assume its the output file.
       open the file in write mode"
      ifFalse: [
         outputfile := FileStream open: arg
                                  mode: FileStream write.
         dopt ifTrue: [stdout << 'output file opened' << nl.].
      ].

      numargs := numargs + 1.

   ].
].


"print debug statements for which options are on"
dopt ifTrue: [
   stdout <<'-d option is on'<<nl.

   topt ifTrue: [
      stdout <<'-t option is on'<<nl.].

   copt ifTrue: [
      stdout <<'-c option is on'<<nl.].

   uopt ifTrue: [
      stdout <<'-u option is on'<<nl.].
].


"make sure that u is not turned on with c or t. print usage and quit"
uopt ifTrue: [
   copt ifTrue: [
      stdout <<'error: cannot have -u with -c or -t.'<<nl.
      ObjectMemory quit.
   ].
   topt ifTrue: [
      stdout <<'error: cannot have -u with -c or -t.'<<nl.
      ObjectMemory quit.
   ].
].

"make sure that u, c and/or t have been turned on"
uopt ifFalse: [
   copt ifFalse: [
      topt ifFalse: [
         stdout <<'error: must include options -u, -c, or -t'<<nl.
         ObjectMemory quit.
      ].
   ].
].


"if the u option is on, perform decompression.-----------------------------------"
uopt ifTrue: [
   |stack atend outBytes|

   "perform decompression process- Reconstruct the Huffman decoding tree."
   dopt ifTrue: [
         stdout << 'Reconstructing the decoding tree...' << nl.].

   atend := false. "end of file indicator"
   reconstructed := false.
   dTree := ''.
   pointer := ''.
   outBytes := OutBits new: outputfile.
   inputBits := BitStream new: inputfiler1.
   stack := OrderedCollection new.

   [(inputBits atEnd not) & (atend not)] whileTrue: [

      charvalue := 0.
      "read one bit at a time"
      nodet := inputBits nextBit.

      reconstructed ifFalse: [

         "if the bit is 0, read the next 8 bits and reconstruct the byte,
          giving a value from 0 to 255"
         nodet = 0 ifTrue: [
            8 to:1 by:-1 do: [:i |
               bit := inputBits nextBit.
               charvalue := charvalue bitAt: i put: bit.].

            "if value is 0, read one more bit"
            charvalue = 0 ifTrue: [
               eofBit := inputBits nextBit.
               "if the next bit is 1, give the encoding for EOF"
               eofBit = 1 ifTrue: [
                  charvalue := charvalue bitAt:9 put:1.].
            ].
            "push the byte to the stack"
            stack add: (Leaf new: charvalue count: '').
         ].

         "if the bit is 1, create a new tree using the last two leaf in the stack"
         nodet = 1 ifTrue: [
            
            "if the stack has more than one item in it, the tree isn't rebuilt yet"
            stack size > 1 ifTrue:[
               "pop the 1-subtree and 0-subtree from the stack"
               onesubtree := stack removeLast.
               zerosubtree := stack removeLast.

               "create a new tree with both those children and push it to the stack"
               subtree := (Tree new: -1 count: '' left: zerosubtree right: onesubtree).
               stack add: subtree.

            ] ifFalse: [
               "if the size is not greater than 1, there is only 1 tree left."
               dTree := stack removeLast.

               "initiallize a pointer to the root of the decoding tree"
               pointer := dTree.

               dopt ifTrue: [
                  stdout << dTree << nl.
                  stdout << 'Decoding tree has been reconstructed.' << nl.
                  stdout << 'Starting uncompressing...' << nl.].

               reconstructed := true.
            ].
         ]. 
      ].

      "once the decoding tree has been reconstructed, start uncompressing"
      "loop over the rest of the input file to reconstruct the original file"
      reconstructed ifTrue: [

         "Read a single bit and use it to move down the 0-link or the 1-link
         to the child of the current node - line 378, nodet reads the bit"

         "If this node is a leaf, write out the corresponding byte"
         pointer leaf ifTrue: [

            "if the leaf is 256, thats the end of the file"
            pointer char = 256 ifTrue: [
               atend := true.
            ].
            pointer char = 256 ifFalse: [
               stdout nextPut: (Character value: (pointer char)).

               "reset the pointer back to the root of the tree"
               pointer := dTree.
            ].
         ].

         "If not a leaf, continue reading bits until you find a leaf."
         pointer leaf ifFalse: [

            "if the bit is 0, move left"
            nodet = 0 ifTrue: [
               pointer := pointer left.

               pointer leaf ifTrue: [

                  pointer char = 256 ifTrue: [
                     atend := true.
                  ].

                  pointer char = 256 ifFalse: [

                     "if theres an output file, write to it"
                     outputfile ~= nil ifTrue: [
                        8 to: 1 by: -1 do: [:index |
                           bit:= (pointer char) bitAt: index.
                           outBytes writeBit: bit.].
                     ].

                     "if there is no output file, write to stdout"
                     outputfile ~= nil ifFalse: [
                        stdout nextPut: (Character value: (pointer char)).
                     ].

                     "reset the pointer back to the root of the tree"
                     pointer := dTree.
                  ].

               ].

            ].

            "if the node is not 0, it is 1. move right"
            nodet = 0 ifFalse:[
               pointer := pointer right.

               "code below is the same as the code above"
               pointer leaf ifTrue: [

                  pointer char = 256 ifTrue: [
                     atend := true.
                  ].

                  pointer char = 256 ifFalse: [

                     "if theres an output file, write to it"
                     outputfile ~= nil ifTrue: [
                        8 to: 1 by: -1 do: [:index |
                           bit:= (pointer char) bitAt: index.
                           outBytes writeBit: bit.].
                     ].

                     "if there is no output file, write to stdout"
                     outputfile ~= nil ifFalse: [
                        stdout nextPut: (Character value: (pointer char)).
                     ].

                     "reset the pointer back to the root of the tree"
                     pointer := dTree.
                  ].
               ].
            ].
         ].
      ].
   ].
].


"if the u option is not on, use the compression algorithm-------------------------
 first construct the decoding tree, and if c option is on then perform compression"
uopt ifFalse: [

   "fill in the frequency table - referenced from charfreq.st"
   [inputfiler1 atEnd not] whileTrue: [
      |ordChar|
      ordChar := inputfiler1 next asInteger.
      freqtable incr: ordChar.
   ].

   freqtable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [

         "create a leaf node and insert into a priority queue, with the character and count"
         pqueue add: (Leaf new: key count: value).

         "if debug option is on, show freq counts"
         dopt ifTrue: [
            stdout << 'freq[' << key asCharacter visible << ']=' << value
             << nl.].
      ]
   ].

   "Repeatedly remove the two smallest elements from the priority queue, creating
    a new tree which is then entered into the priority queue"
   dopt ifTrue: [stdout << 'building decoding tree...' << nl.].
   [pqueue size >= 2] whileTrue: [

      |leftChild rightChild|
      leftChild := pqueue removeFirst.
      rightChild := pqueue removeFirst.

      nextTree := (Tree new: (leftChild char) count: (leftChild count) + (rightChild count) left: leftChild right: rightChild).
      pqueue add: nextTree.

   ].

   "the remaining tree in the priority queue is the decoding tree."
   dTree := pqueue removeFirst.
   dopt ifTrue: [
      stdout << 'decoding tree built' << nl.
      dTree inspect.].

   "Construct an encoding string, using a depth-first traversal. The encoding
    string is a sequence of bits indicating the path from the root to a leaf
    used tree.st as reference."

   dopt ifTrue: [ stdout << 'constructing encoding strings...' << nl.].

   dTree inorder: [:char :string |
      encodingStrings at: char put: string.
   ] prefix: ''.

   dopt ifTrue: [ stdout << 'encoding strings constructed' << nl.].


   "write out the encoding table sorted by character if t is on"
   topt ifTrue: [

      stdout << '-t: Encoding Table:-----------------' << nl.

      encodingStrings keysAndValuesDo: [:key :count |
         (count notNil and: [count > 0]) ifTrue: [
            stdout <<  key asCharacter visible << ' , '
               << (freqtable at: key) << ' , ' <<count
               << nl.
         ]. 
      ].

      stdout << '------------------------------------' << nl.
   ].

   "for when c is on, write out the encoding table using post order traversal"
   copt ifTrue: [
      "beginning of encoding string, empty"
      postTree := ''.
      binNumber := ''. "binary number helper"
      dTree postorder: [:char :leaf |

         "if leaf, write out a 0 bit, + the 8 bits of the corresponding byte"
         leaf ifTrue: [
            binNumber := ''.

            "if char is 0, write out 000000000"
            char = 0 ifTrue: [binNumber := '000000000'.].

            char = 0 ifFalse: [

               "if char is 256/EOF, write out 000000001"
               char = 256 ifTrue: [binNumber := '000000001'.].

               char = 256 ifFalse: [
                  8 to:1 by: -1 do:[:i |
                     binNumber := binNumber, ((char bitAt:i) printString).].
               ].
            ].
            "add 0 bit + 8 bits"
            postTree := postTree, '0', binNumber.
         ].

         "if not a leaf, then it's an interior node. write out 1 bit "
         leaf ifFalse: [
            postTree := postTree, '1'.].

      ].
      stdout << '-t: postorder encoding string:------' << nl.
      stdout << postTree << nl.
      stdout << '------------------------------------' << nl.


      dopt ifTrue: [
         stdout << 'starting file compression...' << nl.].

      "if there's an outputfile, write to it. if not then write to stdout"
      outputfile ~= nil ifTrue: [

         "write out the encoded version of each byte"
         outBytes := OutBits new: outputfile.
         1 to: (postTree size) by: 1 do: [:index |
            bit:= ((postTree at: index) asInteger) - 48.
         outBytes writeBit: bit.].

         
         [inputfiler2 atEnd not] whileTrue: [
            |ordChar2|
            ordChar2 := inputfiler2 next asInteger.
            code:= (encodingStrings at: ordChar2). 
            1 to: (code size) by: 1 do: [:index |
               bit:= ((code at: index) asInteger) - 48.
            outBytes writeBit: bit.].
         ].

         "at the end of the buffer, write out the string for EOF"
         eof:= (encodingStrings at: 256).
            1 to: (eof size) by: 1 do: [:index |
               bit:= ((eof at: index) asInteger) - 48.
            outBytes writeBit: bit.]. 

         stdout << bit << nl.

         outBytes writeBit: 1.    
         outBytes flushByte.

         dopt ifTrue: [
         stdout << 'output sent to output file.' << nl.].
      ]

      "if no output file, print to stdout"
      ifFalse: [

         stdout << 'Compressed Contents:----------------' << nl.

         "write out the encoded version of each byte"
         outBytes := OutBits new: stdout.
         1 to: (postTree size) by: 1 do: [:index |
            bit:= ((postTree at: index) asInteger) - 48.
         outBytes writeBit: bit.].

         
         [inputfiler2 atEnd not] whileTrue: [
            |ordChar2|
            ordChar2 := inputfiler2 next asInteger.
            code:= (encodingStrings at: ordChar2). 
            1 to: (code size) by: 1 do: [:index |
               bit:= ((code at: index) asInteger) - 48.
            outBytes writeBit: bit.].
         ].

         "at the end of the buffer, write out the string for EOF"
         eof:= (encodingStrings at: 256).
            1 to: (eof size) by: 1 do: [:index |
               bit:= ((eof at: index) asInteger) - 48.
            outBytes writeBit: bit.]. 

         outBytes writeBit: 1.    
         outBytes flushByte.

         Transcript cr.
         stdout << '------------------------------------' << nl.
      ].
   ].
].



inputfiler1 close.
inputfiler2 close.

"if there was an output file, close it."
outputfile~=nil ifTrue:[
   outputfile close.
].