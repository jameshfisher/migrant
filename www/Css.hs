{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Clay
  ( renderWith
  , (?)
  , margin
  , (-:)
  )
import Data.Monoid (mappend)
import Clay.Render (Config (Config))
import Clay.Elements (body, code, header)
import Clay.Geometry (height, paddingTop)
import Clay.Size (px, em)
import Clay.Font (fontFamily, fontSize, fontColor)
import Clay.Text (textAlign, alignSide)
import Clay.Color (rgba, hsl)
import Clay.Background (background, url, backgroundColor, placed, sideCenter, sideBottom, repeatX)
import Text.Here (here)
import qualified Data.Text.Lazy as Text (Text)
import qualified Data.Text.Lazy.IO as TextIO (putStr)


fontFaces :: Text.Text
fontFaces = [here|
@font-face {
  font-family "IM Fell Great Primer";
  src url("fonts/IM_Fell_Great_Primer/IMFeGPrm28P.ttf");
}

@font-face {
  font-family "IM Fell Great Primer";
  font-style italic;
  src url("fonts/IM_Fell_Great_Primer/IMFeGPit28P.ttf");
}
|]

main :: IO ()
main = TextIO.putStr $ fontFaces `mappend` css


run = renderWith (Config "" "" "" False False False) []

css = run $ do
  body ? do
    margin 0 0 0 0

    fontFamily ["IM Fell Great Primer"] ["serif"]
    fontSize $ px 21
    fontColor $ rgba 0 0 0 231

    background [url "images/paper.png", url "images/water_stain.png"]
    "background-color" -: "hsl(30, 70%, 98%)"

  code ? do
    fontSize $ em 0.8

  header ? do
    textAlign $ alignSide sideCenter

    paddingTop (px 100)

    background
      (  url "images/travel_without_sky-nq8.png"
      ,( placed sideBottom sideCenter
      ,  repeatX
      ))

    height $ px 800

    fontColor $ hsl 0 200 50
    "color" -: "hsl(0, 70%, 40%)"

  -- .content:before, footer:before
  --  display block
  --  content ""
  --  height 5px
  --  background-image url("images/horizontal.png")
  --  width 100%


    -- h1, h2, h3, h4, h5, h6  font-weight normal

    -- header h1
    --   background-image url("images/decor-nq8.png")
    --   width 387px
    --   height 103px
    --   padding-top 76px

    --   margin 0 auto

    --   text-transform uppercase
    --   font-size 37px
    --   letter-spacing 2px
    --

    -- header h2
    --   font-style italic
    --

    -- div.content h2
    --   text-align center
    --   padding 1.5em 0
    --   margin 0
    --   text-transform uppercase
    --   font-size 1.4em
    --   color hsl(0, 70%, 40%)
    --

    -- div.content h2:before, div.content h2:after
    --   content ""

    --   background-image url("images/leaf.png")
    --   width 39px
    --   height 24px

    --   display inline-block

    --   margin 0 20px
    --

    -- div.content h2:after
    --   -moz-transform scaleX(-1)
    --   -o-transform scaleX(-1)
    --   -webkit-transform scaleX(-1)
    --   transform scaleX(-1)
    --   filter FlipH
    --   -ms-filter "FlipH"
    --

    -- .container
    --   max-width 960px
    --   margin 0 auto
    --

    -- .jumbotron
    --   font-size 1.8em
    --   margin-top 1em
    --

    -- q:before  content "‘"
    -- q:after  content "’"

    -- footer
    --   font-style italic
    --

    -- ul li
    --   list-style-image url("images/oak.png")
    --   margin-top 1em
    --

    -- #wrapper  display table
    -- #info_blocks
    --   margin 2em 0
    --

    -- #info_blocks section
    --   text-align center

    --   border solid transparent
    --   border-width 40px 70px
    --   border-image url("images/border_small.png") 40 70 repeat

    --   margin -10px -20px -15px -27px

    --   text-shadow 2px 2px 1px white, 2px -2px 1px white, -2px 2px 1px white, -2px -2px 1px white
    --

    -- #info_blocks section > div
    --   background-image url("images/shade.png")
    --   margin -15px -35px -10px -28px
    --   padding 1em 0 2em 0
    --   width 400px
    --

    -- #info_blocks section:first-child
    --   float left
    --

    -- #info_blocks section:nth-child(2)
    --   float right
    --


    -- section#body_text section
    --

    -- .clearfix  clear both
