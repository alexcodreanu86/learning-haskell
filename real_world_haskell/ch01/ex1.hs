-- file: ch01/WC.hs
-- -- lines beginning with "--" are comments.
main :: IO()
main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
