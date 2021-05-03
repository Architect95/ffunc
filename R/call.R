# Function calls

"fsubstr"   = function(x, start, stop)        .Call(CfsubstrR, x, start, stop)
"fsubstr<-" = function(x, start, stop, value) .Call(CfsubstrassignR, x, start, stop, value)
"fstr_sub"  = function(x, start, stop)        .Call(Cfstr_subR, x, start, stop)

#.onAttach
#.onLoad
.onUnload   = function(libpath) library.dynam.unload("ffunc", libpath)
