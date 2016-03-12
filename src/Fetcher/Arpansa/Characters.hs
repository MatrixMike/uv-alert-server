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
             , (letterBigA, 'A')
             , (letterBigD, 'D')
             , (letterBigF, 'F')
             , (letterBigJ, 'J')
             , (letterBigM, 'M')
             , (letterBigN, 'N')
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
