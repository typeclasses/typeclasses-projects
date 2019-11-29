{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Environment
import Prelude hiding (head, tail)

body = circle 10 # fillColor brown # lineWidth none
head = circle 5 # fillColor brown # lineWidth none

eyes = hsep 1.4 (stimes 2 [circle 0.8 # fillColor black # lineWidth none]) # center

beak = triangle 2 # fillColor orange # lineWidth none # rotateBy (1 / 2)

teardrop = fromSegments [bezier3 c1 c2 0]
  where a = 1/16; c1 = angleV (a @@ turn); c2 = angleV (-a @@ turn)

wattle = teardrop # glueTrail # toPath # strokePath
    # fillColor red # lineWidth none # scale 5 # rotate (-1/4 @@ turn)

tail = foldMap feather featherNumbers
  where featherCount = 9; colors = cycle [darkolivegreen, darkslateblue, sandybrown]
        featherNumbers = filter odd (enumFromTo 1 featherCount) ++
                         filter even (enumFromTo 1 featherCount)
        feather i = teardrop # glueTrail # toPath # strokePath
                      # fillColor (colors !! i) # lineWidth none # scaleY (3/4) # scale 40
                      # rotate ((fromIntegral i - 1) / (featherCount - 1) / 2 @@ turn)

turkeys :: [Diagram B]
turkeys =
  [ body
  , vsep (-0.5) [head, body]
  , vsep (-0.5) [atop eyes head, body]
  , let face = vsep (0.5) [eyes, beak] # center in vsep (-0.5) [atop face head, body]
  , let face = atop (vsep (0.5) [eyes, beak] # center) (wattle # translateX (-1))
    in vsep (-0.5) [atop face head, body]
  , let face = atop (vsep (0.5) [eyes, beak] # center) (wattle # translateX (-1))
    in atop (vsep (-0.5) [atop face head, body] # center) (tail # translateY (-5))
  ]

main = sequence_ $ zipWith drawTurkey (enumFrom 1) turkeys
  where drawTurkey n dia = withArgs [ "--width", "400"
                                    , "--output", "turkey" ++ show n ++ ".svg" ] $
                               mainWith (dia # center # pad 1.2)
