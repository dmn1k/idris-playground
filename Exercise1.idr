palindrome : Nat -> String -> Bool
palindrome minLength str = let lengthCheck = (length str) >= minLength 
                               palindromeCheck = (toLower str) == (reverse (toLower str)) in
                                    lengthCheck && palindromeCheck

counts : String -> (Nat, Nat)
counts str = (wordCount str, length str)
             where
                wordCount : String -> Nat
                wordCount str = length (words str)

top_ten : Ord a => List a -> List a
top_ten list = take 10 (reverse (sort list))

over_length : Nat -> List String -> Nat
over_length minLength list = length (filter (> minLength) (map length list))