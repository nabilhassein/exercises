-- see https://stackoverflow.com/questions/17905537/dependently-typed-printf-in-idris
-- partly copied, partly redone

data Format = FormatEnd | FormatInt Format | FormatStr Format | FormatChar Char Format

toFormat : String -> Format
toFormat = go . unpack
  where
    go : List Char -> Format
    go Nil                = FormatEnd
    go ('%' :: 'd' :: cs) = FormatInt (go cs)
    go ('%' :: 's' :: cs) = FormatStr (go cs)
    go (c :: cs)          = FormatChar c (go cs)

printfType : Format -> Type
printfType FormatEnd           = String
printfType (FormatInt rest)    = Int -> printfType rest
printfType (FormatStr rest)    = String -> printfType rest
printfType (FormatChar _ rest) = printfType rest

printf : (s : String) -> printfType (toFormat s)
printf s = printFormat (toFormat s)
  where
    printFormat : (format : Format) -> printfType format
    printFormat format = go format ""
      where
        go : (fmt : Format) -> String -> printfType fmt
        go FormatEnd           acc = acc
        go (FormatInt rest)    acc = \i : Int    => go rest (acc ++ show i)
        go (FormatStr rest)    acc = \s : String => go rest (acc ++ s)
        go (FormatChar c rest) acc = go rest (acc ++ pack [c])