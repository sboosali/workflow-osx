{- |


Example 1:

@
import qualified Data.ByteString.Char8  as BS
import qualified Network.HTTP.Types.URI as WAI

-- | google a query, in the default browser. properly encodes the url.
google :: (MonadWorkflow m) => String -> m ()
google query = do
 'openURL' (BS.unpack (\"https:\/\/www.google.com\/search\" <> WAI.renderQuery True [(\"q\", Just (BS.pack query))]))
@


Example 2:

@
-- | access the currently selected region from Haskell, via the clipboard
copy :: (MonadWorkflow m) => m String
copy = do
 'sendKeyChord' ['CommandModifier'] 'CKey'
 'delay' 250
 'getClipboard'

@


Example 3:

@
import qualified Data.Char

-- uppercase the contents of the clipboard, and paste the result
uppercase_clipboard = do
 oldContents <- 'getClipboard'
 let newContents = fmap Data.Char.toUpper oldContents
 'setClipboard' newContents
 'sendKeyChord' ['CommandModifier'] 'VKey'
@


Example 4:

@
-- pause/play the first YouTube tab open in the Chrome browser, by pressing a key after full-screening it
--
-- (this script is super-duper-robust)
youtube_toggle_sound = do
 app <- 'currentApplication'                     -- save the currently open application
 reach_youtube
 'sendKeyChord' ['CommandModifier'] 'UpKey'          -- move to the top of the screen
 'delay' chromeDelay
 youtube_toggle_fullscreen
 'delay' chromeDelay
 'sendKeyChord' [] 'KKey'                          -- pauses/plays the video
 'delay' chromeDelay
 youtube_toggle_fullscreen
 'delay' 2000
 'openApplication' app                           -- restore the previously open application

youtube_toggle_fullscreen = do
 'sendKeyChord' ['ShiftModifier'] 'FKey'

reach_youtube = do
 'openApplication' "Google Chrome"
 'switch_tab' "YouTube.com"

switch_tab s = do
 'sendKeyChord' ['OptionModifier'] 'TKey'            -- needs the <https://chrome.google.com/webstore/detail/tab-ahead/naoajjeoiblmpegfelhkapanmmaaghmi?hl=en Tab Ahead> chrome extension
 'delay' chromeDelay
 'sendText' s
 'sendKeyChord' [] 'ReturnKey'

chromeDelay = 250                              -- milliseconds
@


Example 5:

@
-- | cut the currently selected region
cut :: (MonadWorkflow m) => m String
cut = do
 'sendKeyChord' ['CommandModifier'] 'XKey'
 'delay' 250
 'getClipboard'

-- | transform the currently selected region from Haskell, via the clipboard
transformClipboard :: (MonadWorkflow m) => (String -> String) -> m ()
transformClipboard f = do
 contents <- 'cut'
 'setClipboard' (f contents)
 'sendKeyChord' ['CommandModifier'] 'VKey'
@


-}
module Workflow.OSX
 ( module Workflow.OSX.Types     
 , module Workflow.OSX.Foreign   
-- , module Workflow.OSX.Bindings   -- after mving out WorkflowF
 , module Workflow.OSX.Constants 
 , module Workflow.OSX.Marshall  
 , module Workflow.OSX.Execute   
 , module Workflow.OSX.Screenshot
 ) where
   --TODO non-aliased for docs
import Workflow.OSX.Types     
import Workflow.OSX.Foreign   
-- import Workflow.OSX.Bindings   -- after mving out WorkflowF
import Workflow.OSX.Constants 
import Workflow.OSX.Marshall  
import Workflow.OSX.Execute   
import Workflow.OSX.Screenshot
