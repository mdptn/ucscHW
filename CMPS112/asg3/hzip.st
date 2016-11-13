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
|inputfiler1 inputfiler2 numargs outputfile decodingTree|
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
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
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

freqtable := ZeroArray new: 257.
"at entry 256 put 1 to indicate EOF"
freqtable at:256 put:1.


" referenced from tree.st provided by Mackey----------------------------------
  counts the number of times each char appears in the input"

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

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
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]

pqueue := SortedCollection new.


" referenced from cmdline.st------------------------------------------------------
  parses command line args"

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

   "check for the options and turn them true if detected"
   (opt = $d) ifTrue: [
      dopt := true.

      dopt ifTrue: [
      stdout <<'-d option is on'<<nl.].
   ].

   (opt = $t) ifTrue: [
      topt := true.

      dopt ifTrue: [
      stdout <<'-t option is on'<<nl.].
   ].
   
   (opt = $c) ifTrue: [
      copt := true.

      dopt ifTrue: [
      stdout <<'-c option is on'<<nl.].
   ].
   
   (opt = $u) ifTrue: [
      uopt := true.
      
      dopt ifTrue: [
      stdout <<'-u option is on'<<nl.].
   ].
   

   "check if arg is null. arg should be the input filename"
   (arg ~= nil) ifTrue: [

      "if 0, this is the first arg, aka the inputfile."
      numargs = 0 ifTrue: [
         "open the file twice since it will be read twice"
         inputfiler1 := FileStream open: arg
                                   mode: FileStream read.
         inputfiler2 := FileStream open: arg
                                   mode: FileStream read.
         stdout <<'inputfile pathed'<<nl.

      ]

      "if not the first arg, then assume its the output file.
       open the file in write mode"
      ifFalse: [
         outputfile := FileStream open: arg
                                  mode: FileStream write.
         stdout <<'outputfile pathed'<<nl.
      ].

      numargs := numargs + 1.

   ].
].


"if the u option is on, perform decompression."
uopt ifTrue: [

   "perform decompression process"

].


"if the u option is not on, use the compression algorithm.
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
         pqueue add: (Leaf new: key count: value); inspect.

         "if debug option is on, show freq counts"
         dopt ifTrue: [
         stdout <<  key asCharacter visible << ': ' << value << nl.].
      ]
   ].

].



inputfiler1 close.
inputfiler2 close.

"if there was an output file, close it."
outputfile~=nil ifTrue:[
   outputfile close.
].