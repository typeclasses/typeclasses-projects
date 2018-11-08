module Demo where

-- In the Foundation library, subtraction of Naturals produces Maybe Natural.
xs :: [Maybe Natural]
xs =
    [ (8 :: Natural) - 6  -- Just 2
    , (3 :: Natural) - 5  -- Nothing
    ]
