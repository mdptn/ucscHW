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

" initiallize input file read variables. two since the file is read TWICE. numargs counts the number of arguments after opt"
|inputfiler1 inputfiler2 numargs outputfile|
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
freqtable at:256 put:1.


" referenced from cmdline.st------------------------------------------------------
  parses command line args"

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

   "check for the options and turn them true if detected"
   (opt = $d) ifTrue: [
      dopt := true.
      stdout <<'-d option is set'<<nl.
   ].

   (opt = $t) ifTrue: [
      topt := true.
      stdout <<'-t option is set'<<nl.
   ].
   
   (opt = $c) ifTrue: [
      copt := true.
      stdout <<'-c option is set'<<nl.
   ].
   
   (opt = $u) ifTrue: [
      uopt := true.
      stdout <<'-u option is set'<<nl.
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

         [inputfiler1 atEnd not] whileTrue: [
            |ordChar|
            ordChar := inputfiler1 next asInteger.
            freqtable incr: ordChar.
         ].

         freqtable keysAndValuesDo: [:key :value |
            (value notNil and: [value > 0]) ifTrue: [
               stdout <<  key asCharacter visible << '     ' << value
             << nl.
            ]
         ].
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

inputfiler1 close.
inputfiler2 close.

"if there was an output file, close it."
outputfile~=nil ifTrue:[
   outputfile close.
].