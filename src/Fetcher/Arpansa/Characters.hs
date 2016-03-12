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

letterBigF =parseCharacter [ "xxxxxxxx"
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
             , (digit6, '6')
             , (digit8, '8')
             , (digit9, '9')
             , (letterBigF, 'F')
             , (letterBigJ, 'J')
             , (letterBigM, 'M')
             , (letterBigT, 'T')
             , (letterBigW, 'W')
             , (letterA, 'a')
             , (letterC, 'c')
             , (letterD, 'd')
             , (letterE, 'e')
             , (letterH, 'h')
             , (letterI, 'i')
             , (letterN, 'n')
             , (letterR, 'r')
             , (letterS, 's')
             , (letterT, 't')
             , (letterU, 'u')
             , (letterY, 'y')
             ]
