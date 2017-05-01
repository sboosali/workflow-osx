# workflow-osx

a (free) monad, with Objective-C bindings, for "Workflow" actions. 

for detailed examples, see the documentation on hackage <https://hackage.haskell.org/package/workflow-osx> or github (<http://sboosali.github.io/documentation/workflow-osx/index.html).  

# Issues

## the build

foreign dependencies always complicate the build process. it's known to work with the following:

* OS 

        $ sw_vers
        ProductName:	Mac OS X
        ProductVersion:	10.9.5
        BuildVersion:	13F34

* C compiler 

        $ gcc --version
        Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
        Apple LLVM version 6.0 (clang-600.0.51) (based on LLVM 3.5svn)
        Target: x86_64-apple-darwin13.4.0
        Thread model: posix

* Haskell compiler 

        $ ghc --version 
        The Glorious Glasgow Haskell Compilation System, version 7.10.1

# TODO

## platform agnosticism

exploit the free monad\'s flexibility to define platform-agnostic workflows 

problem: windows (Linux/Windows) versus processes (OS X) 

problem: keyboards. Apple keyboards don't have the Windows key, Windows keyboards don't have the Apple key. some keyboards have a dozen random extra unbound keys.  

## automatic `delay` insertion 

problem: currently, delays must be inserted manually. keyboard shortcuts in Emacs succeed with no delay. keyboard shortcuts in Chrome, like closing a tab with `M-w`, may drop without a long delay (like 250ms). furthermore, different actions need different delays between them (e.g. inserting text into Chrome can be done without delay). 

## parameterize `Workflow` on a keyboard type

