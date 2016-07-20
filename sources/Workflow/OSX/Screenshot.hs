{-# LANGUAGE OverloadedStrings #-}

{-| @shell@s out.

-}
module Workflow.OSX.Screenshot where
import Workflow.OSX.Extra

import Turtle
--import Control.Monad.Managed
import Codec.Picture

{-| Temporary file template.

@= "workflow-osx.XXXXXX"@

-}
__temporary__ :: Text
__temporary__ = "workflow-osx.XXXXXX.png"

{-| Takes screen shot of the whole screen (as PNG, no delay, no sound).

@shell@s out.

@.png@

-}
takeScreenshot :: IO (Either String DynamicImage)
takeScreenshot = with (mktempfile "/tmp" __temporary__) $ \path -> do
  shells (format ("screencapture -x -tpng -T0 -o "%fp) path) empty -- NOTE throws ProcFailed
  i <- liftIO $ readImage (fp2s path)
  return i

{-

<https://hackage.haskell.org/package/JuicyPixels-3.2.7.1/docs/Codec-Picture.html#t:DynamicImage>

-}

