{- LANGUAGE Der -}

module AlcoEval
where

import qualified Data.Map as Map
import           Data.Map ((!))
import           Parser.Parser
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Pos
import           Control.Arrow (first, second)

type AlcoContainer a = Map.Map a (Alco a)


data Alco a = Alco
    { parent :: a
    , unparsed :: Maybe UnparsedAlco
    , parsed :: Maybe (AlcoHolder a)
    }
    deriving (Eq)

data AlcoHolder a = AlcoBinding [a] [a] a -- | Constructed lazily
                  | AlcoPattern [a]
                  | AlcoBindingPattern [a]
                  | BuiltInComputation [UnparsedAlco] ([Alco a] -> Alco a)

instance Ord a => Eq (AlcoHolder a) where
    (AlcoBinding x y z) == (AlcoBinding x' y' z') = x == x' && y == y' && z == z'
    (AlcoPattern x) == (AlcoPattern x') = x == x'
    (AlcoBindingPattern x) == (AlcoBindingPattern x') = x == x'
    (BuiltInComputation x _) == (BuiltInComputation x' _) = x == x'
    _ == _ = False

alcoAny :: Ord a => a -> Alco a -- a pattern that can match anything
alcoAny p = Alco p Nothing $ Just (BuiltInComputation [UnparsedAlco (initialPos "") "any"] (\_ -> undefined))

alcoStop :: Ord a => a -> Alco a
alcoStop p = Alco p Nothing $ Just (BuiltInComputation [UnparsedAlco (initialPos "") "stop"] (\_ -> undefined))

anyContainer :: Ord a => a -> (a -> a) -> AlcoContainer a
anyContainer i sf = Map.insert (sf i) (alcoStop i) $ Map.insert i (alcoAny i) Map.empty

fetchAlcoAny :: Ord a => AlcoContainer a -> a
fetchAlcoAny = fst . Map.findMin

fetchAlcoStop :: Ord a => (a -> a) -> AlcoContainer a -> a
fetchAlcoStop sf = sf . fst . Map.findMin -- special pattern termination marker, can be matched to val 'where'

-- | Used on a source file (or a REPL str in the future)
constructFromString :: Ord a => (a -> a) -> a -> SourcePos -> String -> AlcoContainer a -> (a, AlcoContainer a)
constructFromString sf paf p s = construct sf paf (UnparsedAlco p s)

constructUnparsed :: Ord a => (a -> a) -> a -> UnparsedAlco -> AlcoContainer a -> (a, AlcoContainer a)
constructUnparsed sf paf ua = insert sf (Alco paf (Just ua) Nothing)

constructFrom :: Ord a => (a -> a) -> a -> Maybe UnparsedAlco -> ParsedAlco -> AlcoContainer a -> (a, AlcoContainer a)
constructFrom sf paf ua p c = let (i, c') = insert sf (Alco paf ua Nothing) c
                                  (afh, c'') = case p of
                                    Pattern [ua']       -> first (AlcoBinding [] []) (constructUnparsed sf i ua' c')
                                    Pattern fs           -> first AlcoPattern $ foldr (\fx (is, cx) -> first (:is) $ construct sf i fx cx) ([], c') fs
                                    BindToLiteral [p']   -> first (AlcoBinding [] []) (constructFrom sf i ua p' c')
                                    BindToLiteral fs     -> first AlcoBindingPattern $ foldr (\fx (is, cx) -> first (:is) $ constructFrom sf i Nothing (BindToLiteral [fx]) cx) ([], c') fs
                                in (i, Map.insert i (Alco paf ua (Just afh)) c'')

construct :: Ord a => (a -> a) -> a -> UnparsedAlco -> AlcoContainer a -> (a, AlcoContainer a)
construct sf paf ua@(UnparsedAlco uasp uas) = constructFrom sf paf (Just ua) (parseString uas)

insert :: Ord a => (a -> a) -> Alco a -> AlcoContainer a -> (a, AlcoContainer a)
insert sf fh c = let pos = sf $ fst $ Map.findMax c in (pos, Map.insert pos fh c)



match :: Ord a => (a -> a) -> a -> a -> AlcoContainer a -> (Bool, AlcoContainer a)
match sf vali pati c = let val = c ! vali
                           pat = c ! pati
                        in if checkUnparsedMatch (unparsed val) (unparsed pat)
                            then (True, c)
                            else undefined
    where checkMatch (AlcoBinding bfr bto b) (AlcoBinding bfr' bto' b') = undefined
          checkMatch v (AlcoBinding bfr' [] b') = undefined
          checkMatch (AlcoPattern vs) (AlcoPattern ps) | length vs >= length ps = foldr (\(p, p') (s, c') -> first (&& s) (match sf p p' c'))
                                                                                        (True, c)
                                                                                        (zip (vs ++ ((fetchAlcoStop sf c) : repeat (fetchAlcoAny c))) ps)
          checkMatch (AlcoBindingPattern vs) (AlcoBindingPattern fs') | length vs <= length fs' = undefined
          checkMatch (BuiltInComputation vs _) (BuiltInComputation fs' _) = (vs == fs', c)
          checkMatch _ _ = (False, c)

          checkUnparsedMatch (Just (UnparsedAlco _ sv)) (Just (UnparsedAlco _ sp)) = sv == sp
          checkUnparsedMatch _ _ = False

-- | After a successful match, we evaluate the matched computation, if any
propagate :: Ord a => [a] -> [a] -> AlcoContainer a -> AlcoContainer a
propagate [val] [pat] = undefined
propagate vals pats = undefined