{-# LANGUAGE ScopedTypeVariables #-}
module Utils.Visual where

import Import
import Data.List.Split
import Data.List

--I want a different layout for my forms than what yesod's renderBootstrap3
--will produce. This makes all the labels and fields line up with bootstrap's
--columns, and makes all the fields have the same width.
--It's recommended to have the following line in whatever julius template goes
--with the page you're putting a form on:
-- $("input").addClass("form-control");
renderLambdollarsForm :: Monad m => FormRender m a
renderLambdollarsForm aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
        widget = [whamlet|
            $newline never
            #{fragment}
            $forall view <- views
                <div :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.has-error>
                    <div .row>
                        $if fvId view /= bootstrapSubmitId
                            <div .col-sm-3 .text-right .control-label>
                                <strong>
                                    <label for=#{fvId view}>#{fvLabel view}
                        $else
                            <div .col-sm-3>
                        <div .col-sm-9>
                            ^{fvInput view}
                            ^{helpWidget view}
                <br>
                  |]
    return (res, widget)

--Used internally in Yesod.Form.Bootstrap3, since I'm copy/paste/modifying
--their renderBootstrap3, this is useful here.
helpWidget :: FieldView site -> WidgetT site IO ()
helpWidget view = [whamlet|
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block>#{err}
|]

--Idk wtf this is, see the bottom of:
--https://hackage.haskell.org/package/yesod-form-1.3.8/docs/src/Yesod-Form-Bootstrap3.html
bootstrapSubmitId :: Text
bootstrapSubmitId = "b:ootstrap___unique__:::::::::::::::::submit-id"

--Returns nicely formatted money
prettyMoney :: Double -> String
prettyMoney n = "$" ++ si ++ "." ++ sd
    where i :: Int64 = truncate n
          d :: Int64 = truncate $ 100 * (n - (fromIntegral i))
          si = reverse (intercalate "," $ chunksOf 3 $ reverse $ show $ i)
          sd = if d < 10
                then show d
                else (show d) ++ "0"
