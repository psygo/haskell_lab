module PrettyJSON
  ( renderJValue,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Numeric (showHex)
import Prettify (Doc, char, compact, double, fsep, hcat, pretty, punctuate, text, (<>))
import SimpleJSON

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray array) = series '[' ']' renderJValue array
renderJValue (JObject obj) = series '{' '}' field obj
  where
    field (name, value) =
      string name
        <> text ": "
        <> renderJValue val

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item =
  enclose open close
    . fsep
    . punctuate (char ',')
    . map item

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds
