{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.Static
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 hiding (map, style)
import Text.Blaze.Html5.Attributes hiding (span, title)
import Web.Scotty qualified as S
import Prelude hiding (div, head, span)

main :: IO ()
main =
  S.scotty 1339 $ do
    S.middleware static
    S.get "/" $ do
      S.html $ renderHtml $ html $ do
        head $ do
          title "ХАСКЕЛЛПОБЕДА"
          meta ! name "darkreader-lock"
          link ! rel "stylesheet" ! type_ "text/css" ! href "/static/style.css"
        body $ do
          div ! class_ "logo" $ img ! src "/static/haskell.png"
          h1 "ХАСКЕЛЬ >>= ПОБЕДА!"
          h2 "Почему 2026 -- очередной $ год хаскельпобеды?"
          (p . ol . mconcat . map li)
            [ "Declarative . (statically typed) . code"
            , "Enjoy long-term maintainable software you can rely on"
            , "Пиши чистый код <$> или принимай <|> грязевую ванну"
            , "Используй __Nix__, потому что я так сказал"
            , "ОЧЕРЕДНАЯ ХАСКЕЛЬПОБЕДА! $$$"
            ]
          mconcat $
            map
              ((img ! class_ "meme" !) . src)
              [ "/static/meme1.jpeg"
              , "/static/meme2.webp"
              , "/static/meme3.jpg"
              , "/static/meme4.png"
              ]
          br
          br
          code $
            span
              "\
              \mconcat $ map ((img ! class_ \"meme\" !) . src)\n\
              \[ \"/static/meme1.jpeg\"\n\
              \, \"/static/meme2.webp\"\n\
              \, \"/static/meme3.jpg\"\n\
              \, \"/static/meme4.png\"\n\
              \]"
              ! style "font-family: arial"
          h1 "HASKELL IS SUPREME $ & ~="
          audio "" ! autoplay "" ! controls "" ! src "/static/order.mp3"
          br
          br
          footer $ do
            "sources: "
            a "GitHub" ! href "https://github.com/ProggerX/haskellpobeda"
