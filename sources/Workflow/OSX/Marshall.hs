module Workflow.OSX.Marshall where
import Workflow.OSX.Constants
import Workflow.OSX.Types

import Data.Bits


{- | Marshall the bitmask.

relates Haskell types with Objective-C types:

* Haskell list-of-enum ~ Objective-C bit-mask
* Haskell @['Modifier']@ ~ Objective-C @CGEventFlags@
* Haskell 'CULLong' ~ Objective-C @uint64_t@
* Haskell can marshal 'CULLong'


= Implementation

"Bit-vectors are interpreted as unsigned integers (i.e. natural numbers)"

the folded bitvector size, is the initial bitvector size. because:

>>> toBits (zeros 5 .|. ones 2)
[False, False, False, True, True]

and:

>>> foldl (+) 0 [1,2,3]
((0 + 1) + 2) + 3

since each bit'mask' is disjoint, and we logical-OR the bits
together, we don't need to remove duplicates.


-}
encodeModifiers :: [Modifier] -> CGEventFlags
encodeModifiers
 = foldl (.|.) zeroBits
 . fmap marshallMask

{- | marshall the 'keycode'

relates Haskell types with Objective-C types:

* Haskell 'VirtualKey' ~ Objective-C @CGKeyCode@
* Haskell 'CUShort' ~ Objective-C @uint16_t@
* Haskell can marshal 'CUShort'

-}
encodeKey :: Key -> CGKeyCode
encodeKey
 = marshallKeycode
