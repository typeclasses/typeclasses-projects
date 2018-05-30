#! /usr/bin/env stack
-- stack --resolver lts-11.8 script --no-nix-pure

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main :: IO ()
main = sh $ do
    path <- build  -- Build NixOS for our server
    echo path      -- Print the location that we just built.

build :: Shell Line
build =
    inproc command args empty
  where
    command = "nix-build"
    args = ["server.nix", "--no-out-link"]
