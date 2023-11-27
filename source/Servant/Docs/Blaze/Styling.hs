{-# LANGUAGE QuasiQuotes #-}

module Servant.Docs.Blaze.Styling (cssContent) where

import Text.Blaze.Html5 qualified as H
import Text.RawString.QQ (r)
import Data.String (String)

cssContent :: String
cssContent =
  [r|* {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
      border: 0;
    }
    body {
      font-family: sans-serif;
      font-size: 1.2rem;
      line-height: 1.5;
      margin: 2rem;
    }
    main {
      max-width: 50rem;
      margin: auto;
    }
    h1, h2, h3, h4, h5, h6 {
      font-weight: bold;
      margin-top: 1em;
      margin-bottom: 0.5em;
    }
    h1 {
      font-size: 2rem;
    }
    h2 {
      font-size: 1.5rem;
    }
    h3 {
      font-size: 1.2rem;
    }
    h4 {
      font-size: 1rem;
    }
    p {
      margin-bottom: 1rem;
    }
    code {
      background-color: hsl(0, 0%, 95%);
      border: 1px solid hsl(0, 0%, 80%);
      border-radius: 0.2rem;
      padding: 0.2rem;
      font-size: 0.9rem;
    }
    pre {
      background-color: hsl(0, 0%, 95%);
      border: 1px solid hsl(0, 0%, 80%);
      border-radius: 0.5rem;
      padding: 1rem;
      font-size: 0.8rem;
      margin-bottom: 1rem;
    }
    hr {
      height: 2px;
      width: 100%;
      background-color: hsl(0, 0%, 80%);
      margin: 2rem 0;
    }

    .endpoint {
      background-color: hsl(0, 0%, 95%);
      background-color: hsl(0, 0%, 60%);
      border-radius: 0.4rem;
      padding: 0.2rem;
      font-size: 1.2rem;
      font-weight: 500;
    }
    .method {
      display: inline-block;
      border-radius: 0.3rem;
      padding: 0.4rem 0.8rem;
      color: white;
      font-size: 0.9rem;
      margin-right: 0.5rem;
    }
    .path {
      font-family: monospace;
      font-weight: 600;
    }

    .callout {
      border-radius: 0.4rem;
      padding: 1rem;
      margin-bottom: 1rem;
      border-width: 1px;
      border-style: solid;
      white-space: pre-wrap;
    }
    .callout strong {
      display: block;
      margin-bottom: 1rem;
    }

    #toc a {
      display: inline-block;
      color: hsl(0, 0%, 20%);
      text-decoration: none;
      font-size: 1rem !important;
      width: 100%;
    }
    #toc ul { margin-left: 2rem; }
    #toc li { margin-bottom: 0.3rem; }
    #toc .method-wrapper {
      display: inline-block;
      width: 4.5rem;
    }
    #toc .method { padding: 0.2rem 0.4rem; }
    #toc .path { font-size: 0.9rem; }

    /***** GET *****/
    [data-method="GET"] {
      background-color: hsl(216, 90%, 95%);
      border: 1px solid hsl(216, 95%, 60%);
    }
    [data-method="GET"] .method { background-color: hsl(216, 80%, 50%); }

    h2.endpoint {
      margin-top: 3rem;
    }

    /***** POST *****/
    [data-method="POST"] {
      background-color: hsl(100, 90%, 95%);
      border: 1px solid hsl(100, 95%, 60%);
    }
    [data-method="POST"] .method { background-color: hsl(100, 80%, 40%); }

    /***** PUT *****/
    [data-method="PUT"] {
      background-color: hsl(30, 90%, 95%);
      border: 1px solid hsl(30, 95%, 60%);
    }
    [data-method="PUT"] .method { background-color: hsl(30, 80%, 45%); }

    /***** PATCH *****/
    [data-method="PATCH"] {
      background-color: hsl(180, 90%, 95%);
      border: 1px solid hsl(180, 95%, 60%);
    }
    [data-method="PATCH"] .method { background-color: hsl(180, 80%, 35%); }

    /***** DELETE *****/
    [data-method="DELETE"] {
      background-color: hsl(0, 90%, 95%);
      border: 1px solid hsl(0, 95%, 60%);
    }
    [data-method="DELETE"] .method { background-color: hsl(0, 80%, 45%); }|]
