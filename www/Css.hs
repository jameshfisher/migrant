{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Prelude hiding (div, (**))
import Clay
  ( renderWith
  , (?)
  , margin
  , (-:)
  )
import Clay.Background (background, url, backgroundColor, placed, sideCenter, sideBottom, repeatX)
import Clay.Border (borderStyle, solid, borderColor, borderWidth)
import Clay.Color (rgba, hsl, transparent)
import Clay.Common (auto)
import Clay.Display (display, block, inlineBlock, table, float, floatLeft, floatRight, clear, both)
import Clay.Elements (body, code, header, footer, h1, h2, h3, h4, h5, h6, div, q, ul, li, section)
import Clay.Font (fontFamily, fontSize, fontColor, fontStyle, italic, serif)
import Clay.FontFace (fontFaceSrc, FontFaceSrc (FontFaceSrcUrl))
import Clay.Geometry (height, width, maxWidth, padding, paddingTop, marginTop)
import Clay.Pseudo (before, after, firstChild, nthChild)
import Clay.Render (pretty)
import Clay.Selector ((#), (**), (|>), star, with, byClass)
import Clay.Size (px, em, pct, sym2)
import Clay.Stylesheet (fontFace, (&), (<?))
import Clay.Text (textAlign, alignSide, textTransform, uppercase, content, stringContent, letterSpacing)
import Clay.Transform (transform, scaleX)
import Data.Monoid (mappend, (<>))
import qualified Data.Text.Lazy as Text (Text)
import qualified Data.Text.Lazy.IO as TextIO (writeFile)


main :: IO ()
main = TextIO.writeFile "index.css" $ css

run = renderWith pretty []

css = run $ do

  fontFace $ do
    fontFamily   ["IM Fell Great Primer"] []
    fontFaceSrc  [FontFaceSrcUrl "fonts/IM_Fell_Great_Primer/IMFeGPrm28P.ttf" Nothing]

  fontFace $ do
    fontFamily   ["IM Fell Great Primer"] []
    fontStyle    italic
    fontFaceSrc  [FontFaceSrcUrl "fonts/IM_Fell_Great_Primer/IMFeGPit28P.ttf" Nothing]

  body ? do
    margin       0 0 0 0
    fontFamily   ["IM Fell Great Primer"] [serif]
    fontSize $   px 21
    fontColor $  rgba 0 0 0 231

    background   [url "images/paper.png", url "images/water_stain.png"]
    "background-color" -: "hsl(30, 70%, 98%)" -- TODO fix Clay

  code ? do
    fontSize $  em 0.8

  header ? do
    textAlign $   alignSide sideCenter

    paddingTop $  px 100

    background
      (  url "images/travel_without_sky-nq8.png"
      ,( placed sideBottom sideCenter
      ,  repeatX
      ))

    height $ px 800

    "color" -: "hsl(0, 70%, 40%)"

  (".content" <> footer) # before ? do
    display     block
    content $   stringContent ""
    height $    px 5
    background  [url "images/horizontal.png"]
    width $     pct 100

  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? ("font-weight" -: "normal")

  header ? do
    h1 ? do
      background       [url "images/decor-nq8.png"]
      width $          px 387
      height $         px 103
      paddingTop $     px 76
      sym2 margin      0 auto
      textTransform    uppercase
      fontSize $       px 37
      letterSpacing $  px 2
    h2 ? do
      fontStyle        italic

  (div # ".content") ** h2 ? do
    textAlign $    alignSide sideCenter
    sym2 padding   (em 1.5) 0
    margin         0 0 0 0
    textTransform  uppercase
    fontSize $     em 1.4
    "color" -: "hsl(0, 70%, 40%)" -- TODO fix Clay

    let
      leaf = do
        content $    stringContent ""
        background   [url "images/leaf.png"]
        width $      px 39
        height $     px 24
        display      inlineBlock
        sym2 margin  0 (px 20)

    before & leaf

    after & do
      leaf
      transform $ scaleX (-1)

  ".container" ? do
    maxWidth $   px 960
    sym2 margin  0 auto

  ".jumbotron" ? do
    fontSize $   em 1.8
    marginTop $  em 1

  q ? do
    before & (content $ stringContent "‘")
    after  & (content $ stringContent "’")

  footer ? (fontStyle italic)

  ul |> li ? do
    "list-style-image" -: "url(\"images/oak.png\")" -- TODO add to Clay
    marginTop $ em 1

  "#wrapper" ? (display table)

  "#info_blocks" ? do
    margin (em 2) 0 (em 2) 0 -- TODO fix Clay

    section ? do
      textAlign $        alignSide sideCenter
      borderStyle        solid
      borderColor        transparent
      "border-width" -:  "40px 70px" -- TODO fix Clay
      "border-image" -:  "url(\"images/border_small.png\") 40 70 repeat" -- TODO add to Clay
      margin             (px (-10)) (px (-20)) (px (-15)) (px (-27))
      "text-shadow" -:   "2px 2px 1px white, 2px -2px 1px white, -2px 2px 1px white, -2px -2px 1px white" -- TODO fix Clay

      div <? do
        background  [url"images/shade.png"]
        margin      (px (-15)) (px (-35)) (px (-10)) (px (-28))
        padding     (em 1) 0 (em 2) 0
        width $     px 400

      firstChild   & float floatLeft
      nthChild "2" & float floatRight

  ".clearfix" ? (clear both)