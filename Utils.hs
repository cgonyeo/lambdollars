module Utils where

import Import
import Network.Wai (Request,requestHeaders)
import Data.Text.Encoding (decodeUtf8)

getUser :: Request -> Text
getUser r = case muser of
                Just user -> decodeUtf8 user
                Nothing   -> "bimbotron"
    where headers = requestHeaders r
          muser = lookup "X-WEBAUTH-USER" headers

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
