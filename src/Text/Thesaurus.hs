-- | This module provides synonyms, antonyms, and other kinds of related words from https://words.bighugelabs.com/api.php.
--   See 'getWords'.

module Text.Thesaurus (POS(..), Relation(..), API(..), getWords)
    where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Network.HTTP (getRequest, simpleHTTP, getResponseBody)

-- | 'POS' is the part of speech a word.
data POS = Noun | Verb | Adj | Adv deriving (Show, Read, Eq, Ord, Enum, Bounded)
-- | 'Relation' is how a word is related to another.
data Relation = Synonym | Antonym | Related | Similar | Suggestion deriving (Show, Read, Eq, Ord, Enum, Bounded)
-- | 'API' is the api key. You can get one at https://words.bighugelabs.com/api.php.
newtype API = API String

parse word = do
    [pos',relation',related] <- return $ splitOn "|" word
    pos <- case pos' of
        "noun" -> Just Noun
        "verb" -> Just Verb
        "adjective" -> Just Adj
        "adverb" -> Just Adv
        _ -> Nothing
    relation <- case relation' of
        "syn" -> Just Synonym
        "ant" -> Just Antonym
        "rel" -> Just Related
        "sim" -> Just Similar
        "usr" -> Just Suggestion
        _ -> Nothing
    return (related,pos,relation)

-- | 'getWords' @key@ @word@ returns a list of words related @word@
getWords :: API -> String -> IO [ (String, POS, Relation) ]
getWords (API key) word = simpleHTTP (getRequest $ "http://words.bighugelabs.com/api/2/"++key++"/"++word++"/") >>= getResponseBody >>= (return . fmap (fromJust . parse) . lines)