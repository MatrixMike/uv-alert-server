module Fetcher.Arpansa.Characters (characters) where

import Fetcher.Arpansa.Base

-- Parse a stored character into a matrix of pixels
parseCharacter :: [String] -> CharacterMatrix
parseCharacter = take charHeight . map (map (/= ' ') . take charWidth . (++ (repeat ' '))) . (++ (repeat []))

digit0 :: CharacterMatrix
digit0 = parseCharacter [ "  xxxx  "
                        , " xx  xx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xx  xx "
                        , "  xxxx  "
                        ]

digit1 :: CharacterMatrix
digit1 = parseCharacter [ "  xx  "
                        , " xxx  "
                        , "xxxx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "  xx  "
                        , "xxxxxx"
                        ]

digit2 :: CharacterMatrix
digit2 = parseCharacter [ " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "      xx"
                        , "     xx "
                        , "    xx  "
                        , "  xxx   "
                        , " xx     "
                        , "xx      "
                        , "xxxxxxxx"
                        ]

digit3 :: CharacterMatrix
digit3 = parseCharacter [ "xxxxxxxx"
                        , "      xx"
                        , "     xx "
                        , "    xx  "
                        , "   xxxx "
                        , "      xx"
                        , "      xx"
                        , "      xx"
                        , "xx    xx"
                        , " xxxxxx "
                        ]

digit4 :: CharacterMatrix
digit4 = parseCharacter [ "     xx "
                        , "    xxx "
                        , "   xxxx "
                        , "  xx xx "
                        , " xx  xx "
                        , "xx   xx "
                        , "xxxxxxxx"
                        , "     xx "
                        , "     xx "
                        , "     xx "
                        ]

digit5 :: CharacterMatrix
digit5 = parseCharacter [ "xxxxxxxx"
                        , "xx      "
                        , "xx      "
                        , "xxxxxxx "
                        , "xxx   xx"
                        , "      xx"
                        , "      xx"
                        , "      xx"
                        , "xx    xx"
                        , " xxxxxx "
                        ]

digit6 :: CharacterMatrix
digit6 = parseCharacter [ "  xxxxx "
                        , " xx     "
                        , "xx      "
                        , "xx      "
                        , "xxxxxxx "
                        , "xxx   xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xxxxxx "
                        ]

digit7 :: CharacterMatrix
digit7 = parseCharacter [ "xxxxxxxx"
                        , "      xx"
                        , "      xx"
                        , "     xx "
                        , "    xx  "
                        , "   xx   "
                        , "  xx    "
                        , "  xx    "
                        , " xx     "
                        , " xx     "
                        ]

digit8 :: CharacterMatrix
digit8 = parseCharacter [ " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , " xxxxxx "
                        ]

digit9 :: CharacterMatrix
digit9 = parseCharacter [ " xxxxxx "
                        , "xx    xx"
                        , "xx    xx"
                        , "xx    xx"
                        , "xx   xxx"
                        , " xxxxxxx"
                        , "      xx"
                        , "      xx"
                        , "     xx "
                        , " xxxxx  "
                        ]

letterBigA :: CharacterMatrix
letterBigA = parseCharacter [ "   xx   "
                            , "  xxxx  "
                            , " xx  xx "
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xxxxxxxx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            ]

letterBigD :: CharacterMatrix
letterBigD = parseCharacter [ "xxxxxxx "
                            , " xx   xx"
                            , " xx   xx"
                            , " xx   xx"
                            , " xx   xx"
                            , " xx   xx"
                            , " xx   xx"
                            , " xx   xx"
                            , " xx   xx"
                            , "xxxxxxx "
                            ]

letterBigF :: CharacterMatrix
letterBigF = parseCharacter [ "xxxxxxxx"
                            , " xx     "
                            , " xx     "
                            , " xx     "
                            , " xxxxx  "
                            , " xx     "
                            , " xx     "
                            , " xx     "
                            , " xx     "
                            , " xx     "
                            ]


letterBigJ :: CharacterMatrix
letterBigJ = parseCharacter [ "   xxxxx"
                            , "     xx "
                            , "     xx "
                            , "     xx "
                            , "     xx "
                            , "     xx "
                            , "     xx "
                            , "     xx "
                            , "xx   xx "
                            , " xxxxx  "
                            ]

letterBigM :: CharacterMatrix
letterBigM = parseCharacter [ "xx    xx"
                            , "xx    xx"
                            , "xxx  xxx"
                            , "xxxxxxxx"
                            , "xxxxxxxx"
                            , "xx xx xx"
                            , "xx xx xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            ]

letterBigN :: CharacterMatrix
letterBigN = parseCharacter [ "xx    xx"
                            , "xx    xx"
                            , "xxx   xx"
                            , "xxxx  xx"
                            , "xx xx xx"
                            , "xx  xxxx"
                            , "xx   xxx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            ]

letterBigO :: CharacterMatrix
letterBigO = parseCharacter [ " xxxxxx "
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , " xxxxxx "
                            ]

letterBigS :: CharacterMatrix
letterBigS = parseCharacter [ " xxxxxx "
                            , "xx    xx"
                            , "xx      "
                            , "xx      "
                            , " xxxxxx "
                            , "      xx"
                            , "      xx"
                            , "      xx"
                            , "xx    xx"
                            , " xxxxxx "
                            ]

letterBigT :: CharacterMatrix
letterBigT = parseCharacter [ "xxxxxxxx"
                            , "   xx   "
                            , "   xx   "
                            , "   xx   "
                            , "   xx   "
                            , "   xx   "
                            , "   xx   "
                            , "   xx   "
                            , "   xx   "
                            , "   xx   "
                            ]

letterBigW :: CharacterMatrix
letterBigW = parseCharacter [ "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx    xx"
                            , "xx xx xx"
                            , "xx xx xx"
                            , "xx xx xx"
                            , "xx xx xx"
                            , "xxxxxxxx"
                            , " xx  xx "
                            ]

letterA :: CharacterMatrix
letterA = parseCharacter [ ""
                         , ""
                         , ""
                         , " xxxxxx "
                         , "      xx"
                         , "      xx"
                         , " xxxxxxx"
                         , "xx    xx"
                         , "xx   xxx"
                         , " xxxxxxx"
                         ]

letterB :: CharacterMatrix
letterB = parseCharacter [ "xx      "
                         , "xx      "
                         , "xx      "
                         , "xxxxxxx "
                         , "xxx   xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xxx   xx"
                         , "xxxxxxx "
                         ]

letterC :: CharacterMatrix
letterC = parseCharacter [ ""
                         , ""
                         , ""
                         , " xxxxxx "
                         , "xx    xx"
                         , "xx      "
                         , "xx      "
                         , "xx      "
                         , "xx    xx"
                         , " xxxxxx "
                         ]

letterD :: CharacterMatrix
letterD = parseCharacter [ "      xx"
                         , "      xx"
                         , "      xx"
                         , " xxxxxxx"
                         , "xx   xxx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx   xxx"
                         , " xxxxxxx"
                         ]

letterE :: CharacterMatrix
letterE = parseCharacter [ ""
                         , ""
                         , ""
                         , " xxxxxx "
                         , "xx    xx"
                         , "xx    xx"
                         , "xxxxxxxx"
                         , "xx      "
                         , "xx      "
                         , " xxxxxx "
                         ]

letterG :: CharacterMatrix
letterG = parseCharacter [ "        "
                         , "        "
                         , "        "
                         , " xxxxxxx"
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , " xxxxx  "
                         , "xx      "
                         , " xxxxxx "
                         , "xx    xx"
                         , "xx    xx"
                         , " xxxxxx "
                         ]

letterH :: CharacterMatrix
letterH = parseCharacter [ "xx      "
                         , "xx      "
                         , "xx      "
                         , "xxxxxxx "
                         , "xxx   xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         ]

letterI :: CharacterMatrix
letterI = parseCharacter [ ""
                         , "  xx    "
                         , "        "
                         , " xxx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "xxxxxx  "
                         ]

letterL :: CharacterMatrix
letterL = parseCharacter [ " xxx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "xxxxxx  "
                         ]

letterM :: CharacterMatrix
letterM = parseCharacter [ "        "
                         , "        "
                         , "        "
                         , "xxxxxxx "
                         , "xx xx xx"
                         , "xx xx xx"
                         , "xx xx xx"
                         , "xx xx xx"
                         , "xx xx xx"
                         , "xx    xx"
                         ]

letterN :: CharacterMatrix
letterN = parseCharacter [ ""
                         , ""
                         , ""
                         , "xxxxxxx "
                         , "xxx   xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         ]

letterO :: CharacterMatrix
letterO = parseCharacter [ "        "
                         , "        "
                         , "        "
                         , " xxxxxx "
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , " xxxxxx "
                         ]

letterP :: CharacterMatrix
letterP = parseCharacter [ "        "
                         , "        "
                         , "        "
                         , "xxxxxxx "
                         , "xxx   xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xx    xx"
                         , "xxx   xx"
                         , "xxxxxxx "
                         , "xx      "
                         , "xx      "
                         , "xx      "
                         ]

letterR :: CharacterMatrix
letterR = parseCharacter [ ""
                         , ""
                         , ""
                         , "xx xxxx "
                         , " xxx  xx"
                         , " xx   xx"
                         , " xx     "
                         , " xx     "
                         , " xx     "
                         , " xx     "
                         ]

letterS :: CharacterMatrix
letterS = parseCharacter [ ""
                         , ""
                         , ""
                         , " xxxxxx "
                         , "xx    xx"
                         , "xx      "
                         , " xxxxxx "
                         , "      xx"
                         , "xx    xx"
                         , " xxxxxx "
                         ]

letterT :: CharacterMatrix
letterT = parseCharacter [ ""
                         , "  xx    "
                         , "  xx    "
                         , "xxxxxxx "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx    "
                         , "  xx  xx"
                         , "   xxxx "
                         ]

letterU :: CharacterMatrix
letterU = parseCharacter [ ""
                         , ""
                         , ""
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , " xxxxxxx"
                         ]

letterV :: CharacterMatrix
letterV = parseCharacter [ "        "
                         , "        "
                         , "        "
                         , "xx    xx"
                         , "xx    xx"
                         , " xx  xx "
                         , " xx  xx "
                         , "  xxxx  "
                         , "  xxxx  "
                         , "   xx   "
                         ]

letterY :: CharacterMatrix
letterY = parseCharacter [ ""
                         , ""
                         , ""
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , "xx   xx "
                         , "xx  xxx "
                         , " xxxxxx "
                         , "     xx "
                         , "xx   xx "
                         , " xxxxx  "
                         ]

-- All known characters
characters :: [(CharacterMatrix, Char)]
characters = [ (digit0, '0')
             , (digit1, '1')
             , (digit2, '2')
             , (digit3, '3')
             , (digit4, '4')
             , (digit5, '5')
             , (digit6, '6')
             , (digit7, '7')
             , (digit8, '8')
             , (digit9, '9')
             , (letterBigA, 'A')
             , (letterBigD, 'D')
             , (letterBigF, 'F')
             , (letterBigJ, 'J')
             , (letterBigM, 'M')
             , (letterBigN, 'N')
             , (letterBigO, 'O')
             , (letterBigS, 'S')
             , (letterBigT, 'T')
             , (letterBigW, 'W')
             , (letterA, 'a')
             , (letterB, 'b')
             , (letterC, 'c')
             , (letterD, 'd')
             , (letterE, 'e')
             , (letterG, 'g')
             , (letterH, 'h')
             , (letterI, 'i')
             , (letterL, 'l')
             , (letterM, 'm')
             , (letterN, 'n')
             , (letterO, 'o')
             , (letterP, 'p')
             , (letterR, 'r')
             , (letterS, 's')
             , (letterT, 't')
             , (letterU, 'u')
             , (letterV, 'v')
             , (letterY, 'y')
             ]
