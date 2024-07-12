{-# LANGUAGE OverloadedStrings #-}

module Calculator.Css where

import Clay

getCss :: Css
getCss = do
  h1 ? fontSize (px 24)
  body ? textAlign (alignSide sideCenter)
  input # "type=\"input\"" ? do
    width (px 600)
    height (px 50)
    fontSize (px 18)

postCss :: Css
postCss = do
  h1 ? do
    fontSize (px 24)
    fontFamily ["Tahoma"] [sansSerif]
  body ? textAlign (alignSide sideCenter)
  input # "type=\"input\"" ? do
    width (px 600)
    height (px 50)
    fontSize (px 18)
  input # "type=\"submit\"" ? do
    fontFamily ["Tahoma"] [sansSerif]
    color white
    background red
    borderStyle none
  ul ? do
    listStyleType none
    fontFamily ["Tahoma"] [sansSerif]
  table ? do
    width (px 600)
    "table-layout" -: "fixed"
    textAlign (alignSide sideCenter)
    marginRight auto
    marginLeft auto
    fontFamily ["Tahoma"] [sansSerif]
  td ? ("word-wrap" -: "break-word")
  tr # ":nth-child(even)" ? backgroundColor "#c0c0c0"
  tr # ":nth-child(odd)" ? backgroundColor "#e0e0e0"
