module Main where

import           Data.IORef
import           Data.List       (find, intercalate)
import           Data.Map.Strict as Map
import           Data.Set        as Set
import           Expression
import           Lexer           (alexScanTokens)
import           Parser          (parseLambda)
import           System.IO       (isEOF)
import           Type

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
    r <- newIORef 0
    return (\i -> do modifyIORef r (+i)
                     readIORef r)

getString :: IO String
getString = do
    endMark <- isEOF
    if endMark then return ""
    else do
         current <- getLine
         tail <- getString
         return $! current ++ " " ++ tail


collectSystem :: Expression -> Counter -> Map Expression Type -> IO ([TypeEquation], Type, Map Int Type)
collectSystem expr counter map = do
    number <- counter 1
    let curType = Single ("t" ++ (show number))
    case expr of
        Var s -> case Map.lookup expr map of
                   Just t -> return ([], t, Map.singleton number t)
                   Nothing -> return ([], curType, Map.singleton number curType)
        Apply a b -> do
            (leftSystem, leftType, leftMap) <- collectSystem a counter map
            (rightSystem, rightType, rightMap) <- collectSystem b counter map
            return (leftSystem ++ rightSystem ++ [Equals leftType (Arrow rightType curType)], curType, Map.insert number curType (Map.union leftMap rightMap))
        Lambda s e -> do
            (subSystem, subType, subMap) <- collectSystem e counter (Map.insert (Var s) curType map)
            let actualCurType = Arrow curType subType
            return (subSystem, actualCurType, Map.insert number actualCurType subMap)

unify :: [TypeEquation] -> Maybe [TypeEquation]
unify system =
    let (rounded, changed) = unifyRound system
    in if isSolvable rounded
       then if not changed
            then Just rounded
            else unify rounded
       else Nothing
    where
        isSolvable :: [TypeEquation] -> Bool
        isSolvable system  = not $ any hasSelfInside system where
            hasSelfInside :: TypeEquation -> Bool
            hasSelfInside (Equals a@(Single _) (Arrow b c)) = (hasInside a b) || (hasInside a c) where
                hasInside :: Type -> Type -> Bool
                hasInside a b =
                    case b of
                        Single _  -> a == b
                        Arrow c d -> (hasInside a c) || (hasInside a d)

            hasSelfInside _ = False

        unifyRound :: [TypeEquation] -> ([TypeEquation], Bool)
        unifyRound system = unifyRoundOther $ unifyRoundReplacement system Set.empty

        unifyRoundReplacement :: [TypeEquation] -> Set String -> [TypeEquation]
        unifyRoundReplacement system was = helper system system where
            helper :: [TypeEquation] -> [TypeEquation] -> [TypeEquation]
            helper [] all = all
            helper (equation : rest) all =
                let replaced = replaceAll equation all
                in case equation of
                        Equals (Single a) (Single b) | (a == b) -> helper rest all
                        Equals (Single a) d | (Set.notMember a was) -> unifyRoundReplacement replaced (Set.insert a was)
                        _ -> helper rest all


            replaceAll :: TypeEquation -> [TypeEquation] -> [TypeEquation]
            replaceAll repl@(Equals replaceable replacement) system  =
                Prelude.map (replaceInsideEquation replaceable replacement) system where
                    replaceInsideEquation :: Type -> Type -> TypeEquation -> TypeEquation
                    replaceInsideEquation a b equation =
                        case equation of
                            -- NOTE equation to replace inside system will be leaved
                            c | (c == repl) -> c
                            (Equals c d) -> Equals (replaceInside a b c) (replaceInside a b d)

                    replaceInside :: Type -> Type -> Type -> Type
                    replaceInside replaceable replacement expr =
                        let goInside = replaceInside replaceable replacement
                         in case expr of
                                a | (a == replaceable) -> replacement
                                Arrow a b -> Arrow (goInside a) (goInside b)
                                _ -> expr



        unifyRoundOther :: [TypeEquation] -> ([TypeEquation], Bool)
        unifyRoundOther [] = ([], False)
        unifyRoundOther (equation : system) =
            let (rest, changed) = unifyRoundOther system
            in case equation of
                    Equals (Single a) (Single b) | (a == b) -> (rest, True)
                                                 | True -> (equation : rest, changed)
                    Equals b (Single a) -> ((Equals (Single a) b) : rest, True)
                    Equals (Arrow a b) (Arrow c d) -> ((Equals a c) : (Equals b d) : rest, True)
                    _ -> (equation : rest, changed)


lookupType :: Type -> [TypeEquation] -> Maybe Type
lookupType t ((Equals name@(Single _) value) : xs) | name == t = Just value
                                                   | otherwise = lookupType t xs
lookupType t [] = Nothing

replaceAllInside :: [TypeEquation] -> Type -> Type
replaceAllInside solution t =
    case t of
        Single _ -> case lookupType t solution of
                        Nothing -> t
                        Just r  -> r
        Arrow a b -> Arrow (replaceAllInside solution a) (replaceAllInside solution b)

collectFreeVariables :: Expression -> Counter -> Map Int Type -> Set String -> IO (Map String [Type])
collectFreeVariables expr counter map bounded = do
    number <- counter 1
    let curType = case Map.lookup number map of
                    Just t  -> t
                    Nothing -> error "Couldn't be"
    case expr of
            Var s -> if (Set.member s bounded) then return Map.empty else return $ Map.singleton s [curType]
            Apply a b -> do
                leftVars <- collectFreeVariables a counter map bounded
                rightVars <- collectFreeVariables b counter map bounded
                return $ Map.unionWith (++) leftVars rightVars
            Lambda s e -> collectFreeVariables e counter map (Set.insert s bounded)


collectAnswer :: Map Int Type -> Expression -> [TypeEquation] -> String -> [String] -> Counter -> IO ()
collectAnswer map expr solution prefix hyps counter = do
    number <- counter 1
    let common = prefix ++ (intercalate ", " hyps) ++ (if hyps == [] then "" else " ") ++ "|- " ++ (show expr) ++ " : "
    let curType = case Map.lookup number map of
                    Just t  -> replaceAllInside solution t
                    Nothing -> error "Can't be"
    case expr of
        Var s -> putStrLn $ common ++ (show curType) ++ " [rule #1]"
        Apply a b -> do
            -- apply has t type
            putStrLn $ common ++ (show curType) ++ " [rule #2]"
            collectAnswer map a solution (prefix ++ "*   ") hyps counter
            collectAnswer map b solution (prefix ++ "*   ") hyps counter
        Lambda s e -> do
            let sType = case curType of
                          Arrow s _ -> s
            putStrLn $ common ++ (show curType) ++ " [rule #3]"
            collectAnswer map e solution (prefix ++ "*   ") (hyps ++ [s ++ " : " ++ (show sType)]) counter

appendRepeation :: [(String, [Type])] -> [TypeEquation] -> [TypeEquation]
appendRepeation [] system = system
appendRepeation ((_, (f : l)) : xs) system = (appendHelper l f) ++  appendRepeation xs system where
    appendHelper :: [Type] -> Type -> [TypeEquation]
    appendHelper [] _       = []
    appendHelper (x : xs) f = (Equals x f) : appendHelper xs f

main :: IO ()
main = do
    line <- getString
    case parseLambda $ alexScanTokens line of
        Left error -> putStrLn error
        Right expr -> do
            -- putStrLn $ "Expression: " ++ (show expr)
            counter <- makeCounter
            (systemx, needed, map) <- collectSystem expr counter Map.empty
            counter <- makeCounter
            free <- collectFreeVariables expr counter map Set.empty
            -- putStrLn $ "Type needed: " ++ (show needed)
            --putStrLn $ "Free variables: " ++ (show free)
            --putStrLn "System got: "
            --putStrLn $ unlines $ Prelude.map show systemx
            let system = appendRepeation (Map.toList free) systemx
            --putStrLn $ unlines $ Prelude.map show system
            let solution = unify system
            --putStrLn $ show map
            case solution of
                Nothing       -> putStrLn "Expression has no type"
                Just solution -> do
                    let hyps = Prelude.map (\(var, t) -> var ++ " : " ++ (show $ replaceAllInside solution $ head t)) $ Map.toList free
                    --putStrLn $ "Hyps as string: " ++ (show hyps)
                    --putStrLn "Solution:"
                    --putStrLn $ unlines $ Prelude.map show solution
                    --putStrLn "Result type:"
                    -- let resultType = replaceAllInside solution needed
                    --putStrLn $ show resultType
                    ncounter <- makeCounter
                    collectAnswer map expr solution "" hyps ncounter
                    --putStrLn ""
